/* llvm_runtime/clos.c
   ネイティブ C ランタイムの断片。bin/islc / bin/islc-aot は
   src/isl/compiler/llvm_runtime.c を単一翻訳単位として clang に渡し、
   本ファイルはそこから #include される（単独ではコンパイル不可）。 */

/* ---- CLOS: classes / instances / generic functions ---- */
typedef struct IslClass {
  const char *name;
  IslValue **supers; int nsupers;
  const char **slots; const char **initargs; int nslots;   /* effective */
  const char **own_slots; const char **own_initargs; int nown;
  int finalized;
} IslClass;

static IslValue **g_classes = NULL;
static int g_nclasses = 0, g_classcap = 0;

static IslValue *isl_find_class(const char *name) {
  for (int i = 0; i < g_nclasses; i++)
    if (strcmp(g_classes[i]->as.klass->name, name) == 0) return g_classes[i];
  return NULL;
}
static void isl_register_class(IslValue *c) {
  if (g_nclasses == g_classcap) {
    g_classcap = g_classcap ? g_classcap * 2 : 8;
    g_classes = (IslValue **)realloc(g_classes, (size_t)g_classcap * sizeof(IslValue *));
  }
  g_classes[g_nclasses++] = c;
}

void *isl_rt_class_new(void *envp, void *namep) {
  (void)envp;
  IslValue *nm = (IslValue *)namep;
  IslValue *ex = isl_find_class(nm->as.str);
  if (ex) { IslClass *k = ex->as.klass; k->nsupers = 0; k->nown = 0; k->finalized = 0; return namep; }
  IslClass *k = (IslClass *)calloc(1, sizeof(IslClass));
  k->name = isl_strdup(nm->as.str);
  IslValue *cv = isl_alloc_value(ISL_V_CLASS);
  cv->as.klass = k;
  isl_register_class(cv);
  return namep;
}
void *isl_rt_class_add_super(void *envp, void *namep, void *superp) {
  (void)envp;
  IslValue *cv = isl_find_class(((IslValue *)namep)->as.str);
  IslValue *sv = isl_find_class(((IslValue *)superp)->as.str);
  if (cv && sv) {
    IslClass *k = cv->as.klass;
    k->supers = (IslValue **)realloc(k->supers, (size_t)(k->nsupers + 1) * sizeof(IslValue *));
    k->supers[k->nsupers++] = sv;
  }
  return namep;
}
void *isl_rt_class_add_slot(void *envp, void *namep, void *slotp, void *initargp) {
  (void)envp;
  IslValue *cv = isl_find_class(((IslValue *)namep)->as.str);
  if (cv) {
    IslClass *k = cv->as.klass;
    k->own_slots = (const char **)realloc(k->own_slots, (size_t)(k->nown + 1) * sizeof(char *));
    k->own_initargs = (const char **)realloc(k->own_initargs, (size_t)(k->nown + 1) * sizeof(char *));
    k->own_slots[k->nown] = isl_strdup(((IslValue *)slotp)->as.str);
    IslValue *ia = (IslValue *)initargp;
    k->own_initargs[k->nown] = (ia && ia->tag == ISL_V_SYMBOL) ? isl_strdup(ia->as.str) : NULL;
    k->nown++;
  }
  return namep;
}
void *isl_rt_class_finalize(void *envp, void *namep) {
  (void)envp;
  IslValue *cv = isl_find_class(((IslValue *)namep)->as.str);
  if (!cv) return namep;
  IslClass *k = cv->as.klass;
  int total = k->nown;
  for (int i = 0; i < k->nsupers; i++) total += k->supers[i]->as.klass->nslots;
  k->slots = (const char **)calloc((size_t)(total ? total : 1), sizeof(char *));
  k->initargs = (const char **)calloc((size_t)(total ? total : 1), sizeof(char *));
  int idx = 0;
  for (int i = 0; i < k->nsupers; i++) {
    IslClass *s = k->supers[i]->as.klass;
    for (int j = 0; j < s->nslots; j++) { k->slots[idx] = s->slots[j]; k->initargs[idx] = s->initargs[j]; idx++; }
  }
  for (int j = 0; j < k->nown; j++) { k->slots[idx] = k->own_slots[j]; k->initargs[idx] = k->own_initargs[j]; idx++; }
  k->nslots = idx;
  k->finalized = 1;
  return namep;
}
/* distance from cls up to target through superclasses, or -1 if unrelated */
static int isl_class_distance(IslValue *cls, IslValue *target) {
  if (cls == target) return 0;
  if (!cls || cls->tag != ISL_V_CLASS) return -1;
  int best = -1;
  IslClass *k = cls->as.klass;
  for (int i = 0; i < k->nsupers; i++) {
    int d = isl_class_distance(k->supers[i], target);
    if (d >= 0 && (best < 0 || d + 1 < best)) best = d + 1;
  }
  return best;
}

void *isl_rt_defgeneric(void *envp, void *namep) {
  IslValue *g = (IslValue *)isl_rt_lookup(envp, namep);
  if (g && g->tag == ISL_V_GENERIC) return namep; /* keep existing methods */
  IslValue *nm = (IslValue *)namep;
  g = isl_alloc_value(ISL_V_GENERIC);
  g->as.gen.name = isl_strdup(nm->as.str);
  g->as.gen.specs = NULL; g->as.gen.fns = NULL; g->as.gen.quals = NULL; g->as.gen.n = 0;
  isl_rt_define(envp, namep, g);
  return namep;
}
/* map a qualifier symbol string to a method-combination code */
static int isl_qualifier_code(IslValue *q) {
  if (!q || q->tag != ISL_V_SYMBOL || !q->as.str || !q->as.str[0]) return 0;
  const char *s = q->as.str;
  if (s[0] == ':') s++;            /* tolerate :before / before */
  if (strcmp(s, "before") == 0) return 1;
  if (strcmp(s, "after") == 0) return 2;
  if (strcmp(s, "around") == 0) return 3;
  return 0;
}
void *isl_rt_defmethod(void *envp, void *namep, void *specp, void *closurep, void *qualp) {
  IslValue *g = (IslValue *)isl_rt_lookup(envp, namep);
  if (!g || g->tag != ISL_V_GENERIC) {
    IslValue *nm = (IslValue *)namep;
    g = isl_alloc_value(ISL_V_GENERIC);
    g->as.gen.name = isl_strdup(nm->as.str);
    g->as.gen.specs = NULL; g->as.gen.fns = NULL; g->as.gen.quals = NULL; g->as.gen.n = 0;
    isl_rt_define(envp, namep, g);
  }
  IslValue *spec = (IslValue *)specp;
  IslValue *speccls = NULL;
  if (spec && spec->tag == ISL_V_SYMBOL && spec->as.str[0]) speccls = isl_find_class(spec->as.str);
  int n = g->as.gen.n;
  g->as.gen.specs = (IslValue **)realloc(g->as.gen.specs, (size_t)(n + 1) * sizeof(IslValue *));
  g->as.gen.fns = (IslValue **)realloc(g->as.gen.fns, (size_t)(n + 1) * sizeof(IslValue *));
  g->as.gen.quals = (int *)realloc(g->as.gen.quals, (size_t)(n + 1) * sizeof(int));
  g->as.gen.specs[n] = speccls;
  g->as.gen.fns[n] = (IslValue *)closurep;
  g->as.gen.quals[n] = isl_qualifier_code((IslValue *)qualp);
  g->as.gen.n = n + 1;
  return namep;
}

/* (make-instance class [:initarg val]...) / (create class ...) */
static IslValue *prim_make_instance(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  if (argc < 1) return isl_make_error("make-instance: arity");
  IslValue *cls = argv[0];
  if (cls && cls->tag == ISL_V_SYMBOL) cls = isl_find_class(cls->as.str);
  if (!cls || cls->tag != ISL_V_CLASS) return isl_make_error("make-instance: unknown class");
  IslClass *k = cls->as.klass;
  IslValue *obj = isl_alloc_value(ISL_V_INSTANCE);
  obj->as.inst.cls = cls;
  obj->as.inst.vals = k->nslots ? (IslValue **)calloc((size_t)k->nslots, sizeof(IslValue *)) : NULL;
  for (int i = 0; i < k->nslots; i++) obj->as.inst.vals[i] = (IslValue *)isl_rt_nil();
  for (int32_t a = 1; a + 1 < argc; a += 2) {
    IslValue *ia = argv[a];
    if (ia && ia->tag == ISL_V_SYMBOL)
      for (int i = 0; i < k->nslots; i++)
        if (k->initargs[i] && strcmp(k->initargs[i], ia->as.str) == 0) { obj->as.inst.vals[i] = argv[a + 1]; break; }
  }
  return obj;
}
/* (%slot-read instance slot-name) */
static IslValue *prim_slot_read(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  if (argc != 2) return isl_make_error("slot accessor: arity");
  IslValue *obj = argv[0], *slot = argv[1];
  if (!obj || obj->tag != ISL_V_INSTANCE) return isl_make_error("slot accessor: not an instance");
  if (!slot || slot->tag != ISL_V_SYMBOL) return isl_make_error("slot accessor: bad slot");
  IslClass *k = obj->as.inst.cls->as.klass;
  for (int i = 0; i < k->nslots; i++)
    if (strcmp(k->slots[i], slot->as.str) == 0) return obj->as.inst.vals[i];
  return isl_make_error("slot accessor: no such slot");
}
/* (%slot-write value instance slot-name) */
static IslValue *prim_slot_write(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  if (argc != 3) return isl_make_error("slot writer: arity");
  IslValue *val = argv[0], *obj = argv[1], *slot = argv[2];
  if (!obj || obj->tag != ISL_V_INSTANCE) return isl_make_error("slot writer: not an instance");
  if (!slot || slot->tag != ISL_V_SYMBOL) return isl_make_error("slot writer: bad slot");
  IslClass *k = obj->as.inst.cls->as.klass;
  for (int i = 0; i < k->nslots; i++)
    if (strcmp(k->slots[i], slot->as.str) == 0) { obj->as.inst.vals[i] = val; return val; }
  return isl_make_error("slot writer: no such slot");
}
/* (class-of x) — only instances carry a class here; others return nil. */
static IslValue *prim_class_of(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  if (argc != 1) return isl_make_error("class-of: arity");
  if (argv[0] && argv[0]->tag == ISL_V_INSTANCE) return argv[0]->as.inst.cls;
  return (IslValue *)isl_rt_nil();
}
/* (instancep x class) */
static IslValue *prim_instancep(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  if (argc != 2) return isl_make_error("instancep: arity");
  IslValue *obj = argv[0], *cls = argv[1];
  if (cls && cls->tag == ISL_V_SYMBOL) cls = isl_find_class(cls->as.str);
  if (!obj || obj->tag != ISL_V_INSTANCE || !cls || cls->tag != ISL_V_CLASS) return g_false;
  return (isl_class_distance(obj->as.inst.cls, cls) >= 0) ? g_true : g_false;
}
/* Active generic-call context, so call-next-method / next-method-p can walk the
   effective method.  An :around chain (kind=1) delegates to the qualified core
   (before* + primary chain + after*) when its arounds run out; a primary chain
   (kind=0) advances through less-specific primaries. */
typedef struct MethodCtx {
  int kind;                  /* 0 = primary chain, 1 = around chain */
  IslValue **fns; int n; int idx;     /* the chain being walked */
  IslValue **argv; int32_t argc;
  IslEnv *env;
  /* qualified core (only meaningful for an around chain) */
  IslValue **befores; int nbefore;
  IslValue **primaries; int nprimary;
  IslValue **afters; int nafter;
  struct MethodCtx *next;
} MethodCtx;
static MethodCtx *g_method_ctx = NULL;

/* Collect applicable methods of qualifier code `qual`, sorted most-specific
   first.  Returns count; *out receives a malloc'd array (caller frees). */
static int isl_collect_methods(IslValue *gen, IslValue *argcls, int qual,
                               IslValue ***out) {
  int total = gen->as.gen.n;
  IslValue **fns = (IslValue **)malloc((size_t)(total ? total : 1) * sizeof(IslValue *));
  int *dist = (int *)malloc((size_t)(total ? total : 1) * sizeof(int));
  int m = 0;
  for (int i = 0; i < total; i++) {
    if (gen->as.gen.quals[i] != qual) continue;
    IslValue *spec = gen->as.gen.specs[i];
    int d;
    if (!spec) { d = 1000000; }
    else if (argcls) { d = isl_class_distance(argcls, spec); if (d < 0) continue; }
    else { continue; }
    fns[m] = gen->as.gen.fns[i]; dist[m] = d; m++;
  }
  for (int i = 1; i < m; i++) { /* insertion sort by distance asc (most specific first) */
    IslValue *f = fns[i]; int d = dist[i]; int j = i - 1;
    while (j >= 0 && dist[j] > d) { fns[j+1] = fns[j]; dist[j+1] = dist[j]; j--; }
    fns[j+1] = f; dist[j+1] = d;
  }
  free(dist);
  *out = fns;
  return m;
}

/* Run the qualified core: all before methods (most specific first), then the
   primary chain, then all after methods (least specific first).  Returns the
   primary value. */
static IslValue *isl_run_core(IslEnv *env, int32_t argc, IslValue **argv,
                              IslValue **befores, int nbefore,
                              IslValue **primaries, int nprimary,
                              IslValue **afters, int nafter) {
  if (nprimary == 0) return isl_make_error("no applicable primary method");
  for (int i = 0; i < nbefore; i++) (void)isl_rt_call(env, befores[i], argc, argv);
  MethodCtx ctx;
  ctx.kind = 0; ctx.fns = primaries; ctx.n = nprimary; ctx.idx = 0;
  ctx.argv = argv; ctx.argc = argc; ctx.env = env;
  ctx.befores = NULL; ctx.nbefore = 0; ctx.primaries = NULL; ctx.nprimary = 0;
  ctx.afters = NULL; ctx.nafter = 0;
  ctx.next = g_method_ctx;
  g_method_ctx = &ctx;
  IslValue *r = (IslValue *)isl_rt_call(env, primaries[0], argc, argv);
  g_method_ctx = ctx.next;
  for (int i = nafter - 1; i >= 0; i--) (void)isl_rt_call(env, afters[i], argc, argv);
  return r;
}

/* Partition methods by qualifier, then run the effective method. */
static IslValue *isl_generic_call(IslEnv *env, IslValue *gen, int32_t argc, IslValue **argv) {
  IslValue *argcls = (argc > 0 && argv[0] && argv[0]->tag == ISL_V_INSTANCE)
                         ? argv[0]->as.inst.cls : NULL;
  IslValue **arounds, **befores, **primaries, **afters;
  int na = isl_collect_methods(gen, argcls, 3, &arounds);
  int nb = isl_collect_methods(gen, argcls, 1, &befores);
  int np = isl_collect_methods(gen, argcls, 0, &primaries);
  int nf = isl_collect_methods(gen, argcls, 2, &afters);
  IslValue *r;
  if (na == 0 && np == 0) {
    r = isl_make_error("no applicable method");
  } else if (na > 0) {
    MethodCtx ctx;
    ctx.kind = 1; ctx.fns = arounds; ctx.n = na; ctx.idx = 0;
    ctx.argv = argv; ctx.argc = argc; ctx.env = env;
    ctx.befores = befores; ctx.nbefore = nb;
    ctx.primaries = primaries; ctx.nprimary = np;
    ctx.afters = afters; ctx.nafter = nf;
    ctx.next = g_method_ctx;
    g_method_ctx = &ctx;
    r = (IslValue *)isl_rt_call(env, arounds[0], argc, argv);
    g_method_ctx = ctx.next;
  } else {
    r = isl_run_core(env, argc, argv, befores, nb, primaries, np, afters, nf);
  }
  free(arounds); free(befores); free(primaries); free(afters);
  return r;
}
static IslValue *prim_call_next_method(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)argc; (void)argv;
  MethodCtx *ctx = g_method_ctx;
  if (!ctx) return isl_make_error("call-next-method: no method context");
  if (ctx->idx + 1 < ctx->n) {
    int saved = ctx->idx;
    ctx->idx = saved + 1;
    IslValue *r = (IslValue *)isl_rt_call(ctx->env, ctx->fns[ctx->idx], ctx->argc, ctx->argv);
    ctx->idx = saved;
    return r;
  }
  if (ctx->kind == 1) {
    /* arounds exhausted: delegate to before* + primary chain + after* */
    return isl_run_core(ctx->env, ctx->argc, ctx->argv,
                        ctx->befores, ctx->nbefore,
                        ctx->primaries, ctx->nprimary,
                        ctx->afters, ctx->nafter);
  }
  return isl_make_error("call-next-method: no next method");
}
static IslValue *prim_next_method_p(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env; (void)argc; (void)argv;
  MethodCtx *ctx = g_method_ctx;
  if (!ctx) return g_false;
  if (ctx->idx + 1 < ctx->n) return g_true;
  if (ctx->kind == 1 && ctx->nprimary > 0) return g_true;
  return g_false;
}

