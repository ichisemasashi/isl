/* llvm_runtime/dynamic_convert.c
   ネイティブ C ランタイムの断片。bin/islc / bin/islc-aot は
   src/isl/compiler/llvm_runtime.c を単一翻訳単位として clang に渡し、
   本ファイルはそこから #include される（単独ではコンパイル不可）。 */

/* ---- dynamic (special) variables ---- */
typedef struct DynVar {
  const char *name;
  IslValue *value;
  struct DynVar *next;
} DynVar;
static DynVar *g_dynvars = NULL;

static DynVar *isl_dyn_find(const char *name) {
  for (DynVar *d = g_dynvars; d; d = d->next)
    if (strcmp(d->name, name) == 0) return d;
  return NULL;
}
void *isl_rt_dyn_get(void *symp) {
  IslValue *s = (IslValue *)symp;
  if (!s || s->tag != ISL_V_SYMBOL) return isl_make_error("dynamic: not a symbol");
  DynVar *d = isl_dyn_find(s->as.str);
  if (!d) return isl_make_error("dynamic variable not bound");
  return d->value;
}
void *isl_rt_dyn_set(void *symp, void *valp) {
  IslValue *s = (IslValue *)symp;
  IslValue *v = (IslValue *)valp;
  if (!s || s->tag != ISL_V_SYMBOL) return isl_make_error("dynamic: not a symbol");
  DynVar *d = isl_dyn_find(s->as.str);
  if (d) {
    d->value = v;
  } else {
    d = (DynVar *)calloc(1, sizeof(DynVar));
    if (!d) { fprintf(stderr, "isl_rt: out of memory\n"); exit(2); }
    d->name = isl_strdup(s->as.str);
    d->value = v;
    d->next = g_dynvars;
    g_dynvars = d;
  }
  return v;
}
static IslValue *prim_dynamic_get(IslEnv *e, int32_t c, IslValue **a) {
  (void)e; if (c != 1) return isl_make_error("dynamic: arity");
  return (IslValue *)isl_rt_dyn_get(a[0]);
}
static IslValue *prim_dynamic_set(IslEnv *e, int32_t c, IslValue **a) {
  (void)e; if (c != 2) return isl_make_error("dynamic-set: arity");
  return (IslValue *)isl_rt_dyn_set(a[0], a[1]);
}

/* ---- convert (type conversion); target class is passed as a class-name symbol ---- */
static IslValue *isl_str_value(const char *s) {
  IslValue *v = isl_alloc_value(ISL_V_STRING);
  v->as.str = isl_strdup(s);
  return v;
}
static IslValue *prim_convert(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  if (argc != 2) return isl_make_error("convert: arity");
  IslValue *obj = argv[0];
  IslValue *cls = argv[1];
  if (!obj || !cls || cls->tag != ISL_V_SYMBOL)
    return isl_make_error("convert: second argument must be a class name");
  const char *c = cls->as.str;
  if (strcmp(c, "<character>") == 0) {
    if (obj->tag == ISL_V_CHAR) return obj;
    if (obj->tag == ISL_V_INT) return isl_make_char(obj->as.i64);
    return isl_make_error("convert: cannot convert to <character>");
  }
  if (strcmp(c, "<integer>") == 0) {
    switch (obj->tag) {
      case ISL_V_INT:   return obj;
      case ISL_V_CHAR:  return (IslValue *)isl_rt_make_int(obj->as.ch);
      case ISL_V_FLOAT: return (IslValue *)isl_rt_make_int((int64_t)obj->as.f64);
      case ISL_V_RATIO: return (IslValue *)isl_rt_make_int(obj->as.ratio.num / obj->as.ratio.den);
      default: return isl_make_error("convert: cannot convert to <integer>");
    }
  }
  if (strcmp(c, "<float>") == 0) {
    if (obj->tag == ISL_V_INT || obj->tag == ISL_V_RATIO || obj->tag == ISL_V_FLOAT)
      return isl_make_float(isl_as_double(obj));
    return isl_make_error("convert: cannot convert to <float>");
  }
  if (strcmp(c, "<string>") == 0) {
    if (obj->tag == ISL_V_STRING) return obj;
    if (obj->tag == ISL_V_SYMBOL) return isl_str_value(obj->as.str);
    if (obj->tag == ISL_V_INT) {
      char b[32]; snprintf(b, sizeof b, "%lld", (long long)obj->as.i64); return isl_str_value(b);
    }
    if (obj->tag == ISL_V_FLOAT) {
      char b[64]; isl_format_double(b, sizeof b, obj->as.f64); return isl_str_value(b);
    }
    return isl_make_error("convert: cannot convert to <string>");
  }
  if (strcmp(c, "<symbol>") == 0) {
    if (obj->tag == ISL_V_SYMBOL) return obj;
    if (obj->tag == ISL_V_STRING) return (IslValue *)isl_rt_make_symbol((void *)obj->as.str);
    return isl_make_error("convert: cannot convert to <symbol>");
  }
  if (strcmp(c, "<list>") == 0) {
    if (obj->tag == ISL_V_NIL || obj->tag == ISL_V_CONS) return obj;
    if (obj->tag == ISL_V_STRING) {
      const char *s = obj->as.str;
      int64_t n = (int64_t)strlen(s);
      IslValue *acc = (IslValue *)isl_rt_nil();
      for (int64_t i = n - 1; i >= 0; i--)
        acc = isl_cons(isl_make_char((int64_t)(unsigned char)s[i]), acc);
      return acc;
    }
    if (obj->tag == ISL_V_VECTOR) {
      IslValue *acc = (IslValue *)isl_rt_nil();
      for (int64_t i = obj->as.vec.len - 1; i >= 0; i--)
        acc = isl_cons(obj->as.vec.items[i], acc);
      return acc;
    }
    return isl_make_error("convert: cannot convert to <list>");
  }
  return isl_make_error("convert: unsupported target class");
}

/* (assure class obj) — type assertion; returns obj (the class arg is checked
   by the interpreter; the native backend trusts it and passes the value through). */
static IslValue *prim_assure(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  if (argc != 2) return isl_make_error("assure: arity");
  return argv[1];
}

/* (%the class-name value) — assert value's class.  Built-in classes are
   checked; unknown class names pass through.  Returns value on success. */
static IslValue *prim_the(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  if (argc != 2) return isl_make_error("the: arity");
  IslValue *cls = argv[0], *val = argv[1];
  if (!cls || cls->tag != ISL_V_SYMBOL) return val;
  const char *c = cls->as.str;
  int ok = 1; /* unknown class name => accept */
  if      (!strcmp(c, "<integer>"))   ok = val && val->tag == ISL_V_INT;
  else if (!strcmp(c, "<float>"))     ok = val && val->tag == ISL_V_FLOAT;
  else if (!strcmp(c, "<number>"))    ok = val && (val->tag == ISL_V_INT || val->tag == ISL_V_RATIO || val->tag == ISL_V_FLOAT);
  else if (!strcmp(c, "<character>")) ok = val && val->tag == ISL_V_CHAR;
  else if (!strcmp(c, "<string>"))    ok = val && val->tag == ISL_V_STRING;
  else if (!strcmp(c, "<symbol>"))    ok = val && val->tag == ISL_V_SYMBOL;
  else if (!strcmp(c, "<list>"))      ok = val && (val->tag == ISL_V_NIL || val->tag == ISL_V_CONS);
  else if (!strcmp(c, "<cons>"))      ok = val && val->tag == ISL_V_CONS;
  else if (!strcmp(c, "<null>"))      ok = val && val->tag == ISL_V_NIL;
  else if (!strcmp(c, "<general-vector>") || !strcmp(c, "<basic-vector>") ||
           !strcmp(c, "<vector>"))     ok = val && val->tag == ISL_V_VECTOR;
  if (!ok) return isl_make_error("the: value is not of the asserted class");
  return val;
}

static IslValue *prim_standard_output(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env; (void)argc; (void)argv;
  return g_stdout;
}

static IslValue *prim_error_output(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env; (void)argc; (void)argv;
  return g_stderr;
}

