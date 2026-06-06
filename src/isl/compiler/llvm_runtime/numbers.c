/* llvm_runtime/numbers.c
   ネイティブ C ランタイムの断片。bin/islc / bin/islc-aot は
   src/isl/compiler/llvm_runtime.c を単一翻訳単位として clang に渡し、
   本ファイルはそこから #include される（単独ではコンパイル不可）。 */

/* ---- exact rational arithmetic (int64 numerator/denominator) ---- */

static int64_t isl_gcd(int64_t a, int64_t b) {
  if (a < 0) a = -a;
  if (b < 0) b = -b;
  while (b) { int64_t t = a % b; a = b; b = t; }
  return a ? a : 1;
}

/* Decompose any number value into a fraction num/den (den > 0). */
static void isl_as_fraction(IslValue *v, int64_t *num, int64_t *den) {
  if (v->tag == ISL_V_RATIO) { *num = v->as.ratio.num; *den = v->as.ratio.den; }
  else { *num = v->as.i64; *den = 1; }
}

/* Build a normalized number: reduced, den > 0, collapsing den==1 to an int. */
static IslValue *isl_make_rational(int64_t num, int64_t den) {
  if (den == 0) return isl_make_error("/: division by zero");
  if (den < 0) { num = -num; den = -den; }
  int64_t g = isl_gcd(num, den);
  num /= g;
  den /= g;
  if (den == 1) {
    IslValue *v = isl_alloc_value(ISL_V_INT);
    v->as.i64 = num;
    return v;
  }
  IslValue *v = isl_alloc_value(ISL_V_RATIO);
  v->as.ratio.num = num;
  v->as.ratio.den = den;
  return v;
}

/* ---- inexact (double) support, with float contagion ---- */

static IslValue *isl_make_float(double d) {
  IslValue *v = isl_alloc_value(ISL_V_FLOAT);
  v->as.f64 = d;
  return v;
}

static IslValue *isl_make_char(int64_t cp) {
  IslValue *v = isl_alloc_value(ISL_V_CHAR);
  v->as.ch = cp;
  return v;
}

static IslValue *isl_make_vector_n(int64_t len, IslValue *fill) {
  IslValue *v = isl_alloc_value(ISL_V_VECTOR);
  v->as.vec.len = len;
  v->as.vec.items =
      (len > 0) ? (IslValue **)calloc((size_t)len, sizeof(IslValue *)) : NULL;
  if (len > 0 && !v->as.vec.items) {
    fprintf(stderr, "isl_rt: out of memory\n");
    exit(2);
  }
  for (int64_t i = 0; i < len; i++) v->as.vec.items[i] = fill;
  return v;
}

/* Numeric value as a double (int / ratio / float). */
static double isl_as_double(IslValue *v) {
  switch (v->tag) {
    case ISL_V_INT:   return (double)v->as.i64;
    case ISL_V_RATIO: return (double)v->as.ratio.num / (double)v->as.ratio.den;
    case ISL_V_FLOAT: return v->as.f64;
    default:          return 0.0;
  }
}

/* True if any of the args is a float (drives float contagion). */
static int isl_any_float(int32_t argc, IslValue **argv) {
  for (int32_t i = 0; i < argc; i++)
    if (argv[i] && argv[i]->tag == ISL_V_FLOAT) return 1;
  return 0;
}

/* A signaled condition object (class + message + irritants). */
static IslValue *isl_make_condition(const char *cls, const char *msg,
                                    IslValue *irritants) {
  IslValue *v = isl_alloc_value(ISL_V_CONDITION);
  v->as.cond.cls = isl_strdup(cls);
  v->as.cond.msg = isl_strdup(msg ? msg : "");
  v->as.cond.irritants = irritants;
  return v;
}
/* Defined after the non-local-exit machinery (they use continue_unwind). */
static IslValue *isl_signal(IslValue *cond);
static IslValue *isl_signal_div0(const char *op);

static IslBinding *isl_find_binding(IslEnv *env, const char *name) {
  for (IslEnv *e = env; e != NULL; e = e->parent) {
    for (IslBinding *b = e->bindings; b != NULL; b = b->next) {
      if (strcmp(b->name, name) == 0) {
        return b;
      }
    }
  }
  return NULL;
}

static void isl_define_raw(IslEnv *env, const char *name, IslValue *value) {
  IslBinding *b = (IslBinding *)calloc(1, sizeof(IslBinding));
  if (!b) {
    fprintf(stderr, "isl_rt: out of memory\n");
    exit(2);
  }
  b->name = isl_strdup(name);
  b->value = value;
  b->next = env->bindings;
  env->bindings = b;
}

static IslValue *isl_make_primitive(const char *name, IslPrimitiveFn fn) {
  IslValue *v = isl_alloc_value(ISL_V_FUNCTION);
  IslFunction *f = (IslFunction *)calloc(1, sizeof(IslFunction));
  if (!f) {
    fprintf(stderr, "isl_rt: out of memory\n");
    exit(2);
  }
  f->kind = ISL_FN_PRIMITIVE;
  f->arity = -1;
  f->name = name;
  f->impl.primitive = fn;
  v->as.fn = f;
  return v;
}

static IslValue *prim_plus(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  for (int32_t i = 0; i < argc; i++) {
    IslValue *n = isl_expect_number(argv[i], "+");
    if (n->tag == ISL_V_ERROR) return n;
  }
  if (isl_any_float(argc, argv)) {
    double acc = 0.0;
    for (int32_t i = 0; i < argc; i++) acc += isl_as_double(argv[i]);
    return isl_make_float(acc);
  }
  int64_t an = 0, ad = 1;
  for (int32_t i = 0; i < argc; i++) {
    IslValue *n = argv[i];
    int64_t bn, bd;
    isl_as_fraction(n, &bn, &bd);
    an = an * bd + bn * ad;
    ad = ad * bd;
    int64_t g = isl_gcd(an, ad);
    an /= g; ad /= g;
  }
  return isl_make_rational(an, ad);
}

static IslValue *prim_mul(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  for (int32_t i = 0; i < argc; i++) {
    IslValue *n = isl_expect_number(argv[i], "*");
    if (n->tag == ISL_V_ERROR) return n;
  }
  if (isl_any_float(argc, argv)) {
    double acc = 1.0;
    for (int32_t i = 0; i < argc; i++) acc *= isl_as_double(argv[i]);
    return isl_make_float(acc);
  }
  int64_t an = 1, ad = 1;
  for (int32_t i = 0; i < argc; i++) {
    IslValue *n = argv[i];
    int64_t bn, bd;
    isl_as_fraction(n, &bn, &bd);
    an = an * bn;
    ad = ad * bd;
    int64_t g = isl_gcd(an, ad);
    an /= g; ad /= g;
  }
  return isl_make_rational(an, ad);
}

static IslValue *prim_minus(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  if (argc <= 0) return isl_make_error("-: arity");
  for (int32_t i = 0; i < argc; i++) {
    IslValue *n = isl_expect_number(argv[i], "-");
    if (n->tag == ISL_V_ERROR) return n;
  }
  if (isl_any_float(argc, argv)) {
    double acc = isl_as_double(argv[0]);
    if (argc == 1) return isl_make_float(-acc);
    for (int32_t i = 1; i < argc; i++) acc -= isl_as_double(argv[i]);
    return isl_make_float(acc);
  }
  IslValue *n0 = argv[0];
  int64_t an, ad;
  isl_as_fraction(n0, &an, &ad);
  if (argc == 1) return isl_make_rational(-an, ad);
  for (int32_t i = 1; i < argc; i++) {
    IslValue *n = argv[i];
    int64_t bn, bd;
    isl_as_fraction(n, &bn, &bd);
    an = an * bd - bn * ad;
    ad = ad * bd;
    int64_t g = isl_gcd(an, ad);
    an /= g; ad /= g;
  }
  return isl_make_rational(an, ad);
}

static IslValue *prim_div(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  if (argc <= 0) return isl_make_error("/: arity");
  for (int32_t i = 0; i < argc; i++) {
    IslValue *n = isl_expect_number(argv[i], "/");
    if (n->tag == ISL_V_ERROR) return n;
  }
  if (isl_any_float(argc, argv)) {
    double acc = isl_as_double(argv[0]);
    if (argc == 1) {
      if (acc == 0.0) return isl_signal_div0("/");
      return isl_make_float(1.0 / acc);
    }
    for (int32_t i = 1; i < argc; i++) {
      double d = isl_as_double(argv[i]);
      if (d == 0.0) return isl_signal_div0("/");
      acc /= d;
    }
    return isl_make_float(acc);
  }
  IslValue *n0 = argv[0];
  int64_t an, ad;
  isl_as_fraction(n0, &an, &ad);
  if (argc == 1) {
    /* (/ x) == 1/x */
    if (an == 0) return isl_signal_div0("/");
    return isl_make_rational(ad, an);
  }
  for (int32_t i = 1; i < argc; i++) {
    IslValue *n = argv[i];
    int64_t bn, bd;
    isl_as_fraction(n, &bn, &bd);
    if (bn == 0) return isl_signal_div0("/");
    an = an * bd;
    ad = ad * bn;
    int64_t g = isl_gcd(an, ad);
    an /= g; ad /= g;
  }
  return isl_make_rational(an, ad);
}

static IslValue *prim_cmp(const char *name, int32_t argc, IslValue **argv, int mode) {
  if (argc != 2) return isl_make_error("comparison: arity");
  IslValue *a = isl_expect_number(argv[0], name);
  if (a->tag == ISL_V_ERROR) return a;
  IslValue *b = isl_expect_number(argv[1], name);
  if (b->tag == ISL_V_ERROR) return b;
  int ok = 0;
  if (a->tag == ISL_V_FLOAT || b->tag == ISL_V_FLOAT) {
    double x = isl_as_double(a), y = isl_as_double(b);
    switch (mode) {
      case 0: ok = (x == y); break;
      case 1: ok = (x < y);  break;
      case 2: ok = (x > y);  break;
      case 3: ok = (x <= y); break;
      case 4: ok = (x >= y); break;
      case 5: ok = (x != y); break;
      default: ok = 0; break;
    }
    return ok ? g_true : g_false;
  }
  int64_t an, ad, bn, bd;
  isl_as_fraction(a, &an, &ad);
  isl_as_fraction(b, &bn, &bd);
  /* denominators are positive, so compare an/ad ? bn/bd via cross-multiply */
  int64_t lhs = an * bd;
  int64_t rhs = bn * ad;
  switch (mode) {
    case 0: ok = (lhs == rhs); break;
    case 1: ok = (lhs < rhs); break;
    case 2: ok = (lhs > rhs); break;
    case 3: ok = (lhs <= rhs); break;
    case 4: ok = (lhs >= rhs); break;
    case 5: ok = (lhs != rhs); break;
    default: ok = 0; break;
  }
  return ok ? g_true : g_false;
}

static IslValue *prim_eq(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  return prim_cmp("=", argc, argv, 0);
}

static IslValue *prim_lt(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  return prim_cmp("<", argc, argv, 1);
}

static IslValue *prim_gt(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  return prim_cmp(">", argc, argv, 2);
}

static IslValue *prim_le(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  return prim_cmp("<=", argc, argv, 3);
}

static IslValue *prim_ge(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  return prim_cmp(">=", argc, argv, 4);
}

static IslValue *prim_ne(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  return prim_cmp("/=", argc, argv, 5);
}

/* (abs x) — absolute value, preserving int/ratio. */
static IslValue *prim_abs(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  if (argc != 1) return isl_make_error("abs: arity");
  IslValue *n = isl_expect_number(argv[0], "abs");
  if (n->tag == ISL_V_ERROR) return n;
  int64_t num, den;
  isl_as_fraction(n, &num, &den);
  return isl_make_rational(num < 0 ? -num : num, den);
}

/* max/min over >=1 numeric args, returning the original (winning) value so
   int/ratio identity is preserved. mode: 0=max, 1=min. */
static IslValue *isl_minmax(const char *name, int32_t argc, IslValue **argv, int mode) {
  if (argc < 1) return isl_make_error("max/min: arity");
  IslValue *best = isl_expect_number(argv[0], name);
  if (best->tag == ISL_V_ERROR) return best;
  for (int32_t i = 1; i < argc; i++) {
    IslValue *n = isl_expect_number(argv[i], name);
    if (n->tag == ISL_V_ERROR) return n;
    int64_t an, ad, bn, bd;
    isl_as_fraction(best, &an, &ad);
    isl_as_fraction(n, &bn, &bd);
    int64_t lhs = an * bd, rhs = bn * ad; /* den > 0 */
    if ((mode == 0 && rhs > lhs) || (mode == 1 && rhs < lhs)) best = n;
  }
  return best;
}

static IslValue *prim_max(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  return isl_minmax("max", argc, argv, 0);
}

static IslValue *prim_min(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  return isl_minmax("min", argc, argv, 1);
}

/* Require an integer argument; returns 0 and sets *err on failure. */
static int64_t isl_expect_int(IslValue *v, const char *name, IslValue **err) {
  IslValue *n = isl_expect_number(v, name);
  if (n->tag == ISL_V_ERROR) { *err = n; return 0; }
  int64_t num, den;
  isl_as_fraction(n, &num, &den);
  if (den != 1) { *err = isl_make_error("expects an integer"); return 0; }
  return num;
}

/* (mod z1 z2) — ISLISP modulo; result takes the sign of the divisor. */
static IslValue *prim_mod(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  if (argc != 2) return isl_make_error("mod: arity");
  IslValue *err = NULL;
  int64_t a = isl_expect_int(argv[0], "mod", &err); if (err) return err;
  int64_t b = isl_expect_int(argv[1], "mod", &err); if (err) return err;
  if (b == 0) return isl_signal_div0("mod");
  int64_t r = a % b;
  if (r != 0 && ((r < 0) != (b < 0))) r += b; /* floor-style: sign of divisor */
  IslValue *v = isl_alloc_value(ISL_V_INT);
  v->as.i64 = r;
  return v;
}

/* (expt base exp) — exp must be an integer; base may be int or ratio.
   Negative exp yields the reciprocal (an exact rational). */
static IslValue *prim_expt(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  if (argc != 2) return isl_make_error("expt: arity");
  IslValue *base = isl_expect_number(argv[0], "expt");
  if (base->tag == ISL_V_ERROR) return base;
  IslValue *err = NULL;
  int64_t e = isl_expect_int(argv[1], "expt", &err); if (err) return err;
  if (base->tag == ISL_V_FLOAT) {
    double b = base->as.f64, acc = 1.0;
    int64_t k2 = e < 0 ? -e : e;
    for (int64_t i = 0; i < k2; i++) acc *= b;
    if (e < 0) {
      if (acc == 0.0) return isl_make_error("expt: division by zero");
      return isl_make_float(1.0 / acc);
    }
    return isl_make_float(acc);
  }
  int64_t bn, bd;
  isl_as_fraction(base, &bn, &bd);
  int64_t k = e < 0 ? -e : e;
  int64_t rn = 1, rd = 1;
  for (int64_t i = 0; i < k; i++) { rn *= bn; rd *= bd; }
  if (e < 0) {
    if (rn == 0) return isl_make_error("expt: division by zero");
    return isl_make_rational(rd, rn); /* reciprocal */
  }
  return isl_make_rational(rn, rd);
}

static IslValue *prim_floatp(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  if (argc != 1) return isl_make_error("floatp: arity");
  return (argv[0] && argv[0]->tag == ISL_V_FLOAT) ? g_true : g_false;
}

static IslValue *prim_float(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  if (argc != 1) return isl_make_error("float: arity");
  IslValue *n = isl_expect_number(argv[0], "float");
  if (n->tag == ISL_V_ERROR) return n;
  return isl_make_float(isl_as_double(n));
}

static void isl_print_value(IslValue *v);
static void isl_write_value(IslValue *v, int readable);
static void isl_format_radix(IslValue *v, int radix);
static void isl_format_double(char *out, size_t outsz, double d);
static IslValue *isl_str_value(const char *s);
static IslValue *isl_generic_call(IslEnv *env, IslValue *gen, int32_t argc, IslValue **argv);
static int64_t isl_array_index(IslValue *a, int32_t argc, IslValue **argv, int32_t base);

static IslValue *prim_print(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  if (argc != 1) return isl_make_error("print: arity");
  isl_print_value(argv[0]);
  fputc('\n', stdout);
  return argv[0];
}

static IslValue *prim_not(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  if (argc != 1) return isl_make_error("not: arity");
  return isl_truthy(argv[0]) ? g_false : g_true;
}

/* (funcall fn arg ...) — call a function value with the remaining arguments. */
static IslValue *prim_funcall(IslEnv *env, int32_t argc, IslValue **argv) {
  if (argc < 1) return isl_make_error("funcall: expects at least 1 argument");
  return (IslValue *)isl_rt_call(env, argv[0], argc - 1, (void *)(argv + 1));
}

static IslValue *isl_cons(IslValue *a, IslValue *d) {
  IslValue *v = isl_alloc_value(ISL_V_CONS);
  v->as.cons.car = a;
  v->as.cons.cdr = d;
  return v;
}

static int isl_is_list(IslValue *v) {
  for (; v && v->tag == ISL_V_CONS; v = v->as.cons.cdr) {}
  return (v && v->tag == ISL_V_NIL);
}

static int isl_list_length(IslValue *v) {
  int n = 0;
  for (; v && v->tag == ISL_V_CONS; v = v->as.cons.cdr) n++;
  return n;
}

static IslValue *prim_cons(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  if (argc != 2) return isl_make_error("cons: arity");
  return isl_cons(argv[0], argv[1]);
}

static IslValue *prim_car(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  if (argc != 1) return isl_make_error("car: arity");
  if (argv[0] && argv[0]->tag == ISL_V_NIL) return argv[0]; /* (car nil) = nil */
  if (!argv[0] || argv[0]->tag != ISL_V_CONS) return isl_make_error("car: not a list");
  return argv[0]->as.cons.car;
}

static IslValue *prim_cdr(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  if (argc != 1) return isl_make_error("cdr: arity");
  if (argv[0] && argv[0]->tag == ISL_V_NIL) return argv[0]; /* (cdr nil) = nil */
  if (!argv[0] || argv[0]->tag != ISL_V_CONS) return isl_make_error("cdr: not a list");
  return argv[0]->as.cons.cdr;
}

static IslValue *prim_list(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  IslValue *acc = (IslValue *)isl_rt_nil();
  for (int32_t i = argc - 1; i >= 0; i--) acc = isl_cons(argv[i], acc);
  return acc;
}

static IslValue *prim_consp(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  if (argc != 1) return isl_make_error("consp: arity");
  return (argv[0] && argv[0]->tag == ISL_V_CONS) ? g_true : g_false;
}

static IslValue *prim_null(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  if (argc != 1) return isl_make_error("null: arity");
  return (argv[0] && argv[0]->tag == ISL_V_NIL) ? g_true : g_false;
}

/* (apply fn a b ... last-list) — spread the final list argument. */
static IslValue *prim_apply(IslEnv *env, int32_t argc, IslValue **argv) {
  if (argc < 1) return isl_make_error("apply: expects at least 1 argument");
  IslValue *fn = argv[0];
  if (argc == 1) {
    return (IslValue *)isl_rt_call(env, fn, 0, NULL);
  }
  IslValue *last = argv[argc - 1];
  if (!(last && (last->tag == ISL_V_CONS || last->tag == ISL_V_NIL)) || !isl_is_list(last)) {
    return isl_make_error("apply: last argument must be a proper list");
  }
  int32_t lead = argc - 2;               /* args between fn and the list */
  int32_t tail = isl_list_length(last);
  int32_t total = lead + tail;
  IslValue **callv = (IslValue **)calloc((size_t)(total > 0 ? total : 1), sizeof(IslValue *));
  if (!callv) {
    fprintf(stderr, "isl_rt: out of memory\n");
    exit(2);
  }
  int32_t k = 0;
  for (int32_t i = 0; i < lead; i++) callv[k++] = argv[1 + i];
  for (IslValue *p = last; p && p->tag == ISL_V_CONS; p = p->as.cons.cdr)
    callv[k++] = p->as.cons.car;
  IslValue *r = (IslValue *)isl_rt_call(env, fn, total, (void *)callv);
  free(callv);
  return r;
}

/* Runtime cons constructor used by codegen to build quoted list literals. */
void *isl_rt_cons(void *a, void *d) {
  return isl_cons((IslValue *)a, (IslValue *)d);
}

/* Append a single element to a (head,tail) list being built in order. */
static void isl_list_append1(IslValue **head, IslValue **tail, IslValue *x) {
  IslValue *cell = isl_cons(x, (IslValue *)isl_rt_nil());
  if (*tail == NULL) {
    *head = cell;
    *tail = cell;
  } else {
    (*tail)->as.cons.cdr = cell;
    *tail = cell;
  }
}

/* (mapcar fn list ...) — apply fn across N lists, collecting results.
   (mapc fn list ...)   — same, but for effect; returns the first list.
   collect != 0 selects mapcar behaviour. */
static IslValue *isl_map_impl(IslEnv *env, int32_t argc, IslValue **argv, int collect) {
  if (argc < 2) return isl_make_error("mapcar/mapc: expects function and at least one list");
  IslValue *fn = argv[0];
  int32_t nlists = argc - 1;
  IslValue **cur = (IslValue **)calloc((size_t)nlists, sizeof(IslValue *));
  IslValue **callv = (IslValue **)calloc((size_t)nlists, sizeof(IslValue *));
  if (!cur || !callv) { fprintf(stderr, "isl_rt: out of memory\n"); exit(2); }
  for (int32_t i = 0; i < nlists; i++) cur[i] = argv[1 + i];
  IslValue *head = (IslValue *)isl_rt_nil();
  IslValue *tail = NULL;
  for (;;) {
    int done = 0;
    for (int32_t i = 0; i < nlists; i++) {
      if (!(cur[i] && cur[i]->tag == ISL_V_CONS)) { done = 1; break; }
    }
    if (done) break;
    for (int32_t i = 0; i < nlists; i++) {
      callv[i] = cur[i]->as.cons.car;
      cur[i] = cur[i]->as.cons.cdr;
    }
    IslValue *r = (IslValue *)isl_rt_call(env, fn, nlists, (void *)callv);
    if (r && r->tag == ISL_V_ERROR) { free(cur); free(callv); return r; }
    if (collect) isl_list_append1(&head, &tail, r);
  }
  free(cur);
  free(callv);
  return collect ? head : argv[1];
}

static IslValue *prim_mapcar(IslEnv *env, int32_t argc, IslValue **argv) {
  return isl_map_impl(env, argc, argv, 1);
}

static IslValue *prim_mapc(IslEnv *env, int32_t argc, IslValue **argv) {
  return isl_map_impl(env, argc, argv, 0);
}

static IslValue *prim_length(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  if (argc != 1) return isl_make_error("length: arity");
  IslValue *v = argv[0];
  if (!v) return isl_make_error("length: bad argument");
  switch (v->tag) {
    case ISL_V_NIL:    return (IslValue *)isl_rt_make_int(0);
    case ISL_V_STRING: return (IslValue *)isl_rt_make_int((int64_t)strlen(v->as.str));
    case ISL_V_VECTOR: return (IslValue *)isl_rt_make_int(v->as.vec.len);
    case ISL_V_CONS:
      if (!isl_is_list(v)) return isl_make_error("length: not a proper list");
      return (IslValue *)isl_rt_make_int(isl_list_length(v));
    default: return isl_make_error("length: not a sequence");
  }
}

