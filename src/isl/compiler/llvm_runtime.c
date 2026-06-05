#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <setjmp.h>

typedef struct IslValue IslValue;
typedef struct IslEnv IslEnv;

typedef IslValue *(*IslCompiledFn)(IslEnv *env);
typedef IslValue *(*IslPrimitiveFn)(IslEnv *env, int32_t argc, IslValue **argv);

/* Forward declarations: defined later, used by primitives above their defs. */
void *isl_rt_call(void *envp, void *fnp, int32_t argc, void *argvp);
void *isl_rt_nil(void);
void *isl_rt_true(void);
void *isl_rt_false(void);
void *isl_rt_make_int(int64_t x);
void *isl_rt_make_symbol(void *p);

typedef enum {
  ISL_V_INT,
  ISL_V_RATIO,
  ISL_V_FLOAT,
  ISL_V_CHAR,
  ISL_V_VECTOR,
  ISL_V_BOOL,
  ISL_V_NIL,
  ISL_V_SYMBOL,
  ISL_V_STRING,
  ISL_V_FUNCTION,
  ISL_V_CONS,
  ISL_V_STREAM,
  ISL_V_ERROR
} IslTag;

typedef enum {
  ISL_FN_COMPILED,
  ISL_FN_PRIMITIVE
} IslFnKind;

typedef struct {
  IslFnKind kind;
  int32_t arity;
  const char *name;
  IslEnv *env;            /* captured lexical environment (NULL for primitives) */
  union {
    IslCompiledFn compiled;
    IslPrimitiveFn primitive;
  } impl;
} IslFunction;

struct IslValue {
  IslTag tag;
  union {
    int64_t i64;
    double f64;
    int64_t ch;   /* ISL_V_CHAR: Unicode code point */
    int b;
    const char *str;
    struct {
      struct IslValue **items;
      int64_t len;
    } vec;        /* ISL_V_VECTOR: general vector */
    IslFunction *fn;
    struct {
      IslValue *car;
      IslValue *cdr;
    } cons;
    struct {
      int64_t num;
      int64_t den;  /* always > 0 and reduced; never 1 (those are ISL_V_INT) */
    } ratio;
  } as;
};

typedef struct IslBinding {
  const char *name;
  IslValue *value;
  struct IslBinding *next;
} IslBinding;

struct IslEnv {
  IslEnv *parent;
  IslBinding *bindings;
  int32_t argc;
  IslValue **argv;
};

static IslValue *g_nil = NULL;
static IslValue *g_true = NULL;
static IslValue *g_false = NULL;
static IslValue *g_stdout = NULL;
static IslValue *g_stderr = NULL;

static IslValue *isl_alloc_value(IslTag tag) {
  IslValue *v = (IslValue *)calloc(1, sizeof(IslValue));
  if (!v) {
    fprintf(stderr, "isl_rt: out of memory\n");
    exit(2);
  }
  v->tag = tag;
  return v;
}

static const char *isl_strdup(const char *s) {
  size_t n = strlen(s);
  char *d = (char *)malloc(n + 1);
  if (!d) {
    fprintf(stderr, "isl_rt: out of memory\n");
    exit(2);
  }
  memcpy(d, s, n + 1);
  return d;
}

static IslValue *isl_make_error(const char *msg) {
  IslValue *v = isl_alloc_value(ISL_V_ERROR);
  v->as.str = isl_strdup(msg);
  return v;
}

static int isl_truthy(IslValue *v) {
  if (!v) return 0;
  if (v->tag == ISL_V_NIL) return 0;
  if (v->tag == ISL_V_BOOL && v->as.b == 0) return 0;
  return 1;
}

static IslValue *isl_expect_number(IslValue *v, const char *who) {
  if (!v || (v->tag != ISL_V_INT && v->tag != ISL_V_RATIO &&
             v->tag != ISL_V_FLOAT)) {
    char buf[128];
    snprintf(buf, sizeof(buf), "%s: expected number", who);
    return isl_make_error(buf);
  }
  return v;
}

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
      if (acc == 0.0) return isl_make_error("/: division by zero");
      return isl_make_float(1.0 / acc);
    }
    for (int32_t i = 1; i < argc; i++) {
      double d = isl_as_double(argv[i]);
      if (d == 0.0) return isl_make_error("/: division by zero");
      acc /= d;
    }
    return isl_make_float(acc);
  }
  IslValue *n0 = argv[0];
  int64_t an, ad;
  isl_as_fraction(n0, &an, &ad);
  if (argc == 1) {
    /* (/ x) == 1/x */
    if (an == 0) return isl_make_error("/: division by zero");
    return isl_make_rational(ad, an);
  }
  for (int32_t i = 1; i < argc; i++) {
    IslValue *n = argv[i];
    int64_t bn, bd;
    isl_as_fraction(n, &bn, &bd);
    if (bn == 0) return isl_make_error("/: division by zero");
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
  if (b == 0) return isl_make_error("mod: division by zero");
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

/* ---- characters ---- */
static IslValue *prim_char_cmp(const char *name, int32_t argc, IslValue **argv, int mode) {
  if (argc != 2) return isl_make_error("char comparison: arity");
  if (!argv[0] || argv[0]->tag != ISL_V_CHAR ||
      !argv[1] || argv[1]->tag != ISL_V_CHAR) {
    return isl_make_error(name);
  }
  int64_t a = argv[0]->as.ch, b = argv[1]->as.ch;
  int ok = 0;
  switch (mode) {
    case 0: ok = (a == b); break;
    case 1: ok = (a < b);  break;
    case 2: ok = (a > b);  break;
    case 3: ok = (a <= b); break;
    case 4: ok = (a >= b); break;
    case 5: ok = (a != b); break;
  }
  return ok ? g_true : g_false;
}
static IslValue *prim_char_eq (IslEnv *e, int32_t c, IslValue **a){(void)e; return prim_char_cmp("char=", c,a,0);}
static IslValue *prim_char_ne (IslEnv *e, int32_t c, IslValue **a){(void)e; return prim_char_cmp("char/=",c,a,5);}
static IslValue *prim_char_lt (IslEnv *e, int32_t c, IslValue **a){(void)e; return prim_char_cmp("char<", c,a,1);}
static IslValue *prim_char_gt (IslEnv *e, int32_t c, IslValue **a){(void)e; return prim_char_cmp("char>", c,a,2);}
static IslValue *prim_char_le (IslEnv *e, int32_t c, IslValue **a){(void)e; return prim_char_cmp("char<=",c,a,3);}
static IslValue *prim_char_ge (IslEnv *e, int32_t c, IslValue **a){(void)e; return prim_char_cmp("char>=",c,a,4);}
static IslValue *prim_char_to_int(IslEnv *e, int32_t c, IslValue **a) {
  (void)e;
  if (c != 1 || !a[0] || a[0]->tag != ISL_V_CHAR) return isl_make_error("char->integer: expected character");
  return (IslValue *)isl_rt_make_int(a[0]->as.ch);
}
static IslValue *prim_int_to_char(IslEnv *e, int32_t c, IslValue **a) {
  (void)e;
  if (c != 1 || !a[0] || a[0]->tag != ISL_V_INT) return isl_make_error("integer->char: expected integer");
  return isl_make_char(a[0]->as.i64);
}

/* ---- strings ---- */
static IslValue *prim_string_append(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  size_t total = 0;
  for (int32_t i = 0; i < argc; i++) {
    if (!argv[i] || argv[i]->tag != ISL_V_STRING)
      return isl_make_error("string-append: expected strings");
    total += strlen(argv[i]->as.str);
  }
  char *buf = (char *)malloc(total + 1);
  if (!buf) { fprintf(stderr, "isl_rt: out of memory\n"); exit(2); }
  buf[0] = '\0';
  for (int32_t i = 0; i < argc; i++) strcat(buf, argv[i]->as.str);
  IslValue *v = isl_alloc_value(ISL_V_STRING);
  v->as.str = buf;
  return v;
}
static IslValue *prim_string_eq(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  if (argc != 2 || argv[0]->tag != ISL_V_STRING || argv[1]->tag != ISL_V_STRING)
    return isl_make_error("string=: expected strings");
  return (strcmp(argv[0]->as.str, argv[1]->as.str) == 0) ? g_true : g_false;
}
static IslValue *prim_create_string(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  if (argc < 1 || argv[0]->tag != ISL_V_INT)
    return isl_make_error("create-string: expected length");
  int64_t n = argv[0]->as.i64;
  if (n < 0) return isl_make_error("create-string: negative length");
  char fill = (argc >= 2 && argv[1]->tag == ISL_V_CHAR) ? (char)argv[1]->as.ch : ' ';
  char *buf = (char *)malloc((size_t)n + 1);
  if (!buf) { fprintf(stderr, "isl_rt: out of memory\n"); exit(2); }
  for (int64_t i = 0; i < n; i++) buf[i] = fill;
  buf[n] = '\0';
  IslValue *v = isl_alloc_value(ISL_V_STRING);
  v->as.str = buf;
  return v;
}
/* (char-index char string) -> index or nil */
static IslValue *prim_char_index(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  if (argc != 2 || argv[0]->tag != ISL_V_CHAR || argv[1]->tag != ISL_V_STRING)
    return isl_make_error("char-index: expected (character string)");
  const char *s = argv[1]->as.str;
  for (int64_t i = 0; s[i]; i++)
    if ((int64_t)(unsigned char)s[i] == argv[0]->as.ch)
      return (IslValue *)isl_rt_make_int(i);
  return (IslValue *)isl_rt_nil();
}

/* ---- vectors / general 1-D arrays ---- */
static IslValue *prim_vector(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  IslValue *v = isl_make_vector_n(argc, (IslValue *)isl_rt_nil());
  for (int32_t i = 0; i < argc; i++) v->as.vec.items[i] = argv[i];
  return v;
}
static IslValue *prim_create_vector(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  if (argc < 1 || argv[0]->tag != ISL_V_INT)
    return isl_make_error("create-vector: expected length");
  int64_t n = argv[0]->as.i64;
  if (n < 0) return isl_make_error("create-vector: negative length");
  IslValue *fill = (argc >= 2) ? argv[1] : (IslValue *)isl_rt_nil();
  return isl_make_vector_n(n, fill);
}
/* (create-array dims [fill]) — only rank-1 (a one-element dimension list). */
static IslValue *prim_create_array(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  if (argc < 1) return isl_make_error("create-array: expected dimensions");
  IslValue *dims = argv[0];
  if (!dims || dims->tag != ISL_V_CONS || isl_list_length(dims) != 1 ||
      dims->as.cons.car->tag != ISL_V_INT) {
    return isl_make_error("create-array: only rank-1 dimensions supported");
  }
  int64_t n = dims->as.cons.car->as.i64;
  IslValue *fill = (argc >= 2) ? argv[1] : (IslValue *)isl_rt_nil();
  return isl_make_vector_n(n, fill);
}
/* element access shared by elt / aref / vector-ref (1-D) */
static IslValue *isl_seq_ref(IslValue *seq, IslValue *idxv, const char *who) {
  if (!idxv || idxv->tag != ISL_V_INT) return isl_make_error(who);
  int64_t i = idxv->as.i64;
  if (!seq) return isl_make_error(who);
  switch (seq->tag) {
    case ISL_V_VECTOR:
      if (i < 0 || i >= seq->as.vec.len) return isl_make_error("index out of range");
      return seq->as.vec.items[i];
    case ISL_V_STRING: {
      int64_t len = (int64_t)strlen(seq->as.str);
      if (i < 0 || i >= len) return isl_make_error("index out of range");
      return isl_make_char((int64_t)(unsigned char)seq->as.str[i]);
    }
    case ISL_V_CONS: {
      IslValue *c = seq;
      while (i-- > 0 && c->tag == ISL_V_CONS) c = c->as.cons.cdr;
      if (!c || c->tag != ISL_V_CONS) return isl_make_error("index out of range");
      return c->as.cons.car;
    }
    default: return isl_make_error(who);
  }
}
static IslValue *prim_elt(IslEnv *e, int32_t c, IslValue **a) {
  (void)e; if (c != 2) return isl_make_error("elt: arity");
  return isl_seq_ref(a[0], a[1], "elt: bad sequence");
}
static IslValue *prim_aref(IslEnv *e, int32_t c, IslValue **a) {
  (void)e; if (c != 2) return isl_make_error("aref: arity (only rank-1)");
  return isl_seq_ref(a[0], a[1], "aref: bad array");
}
/* (set-elt value seq index) / (set-aref value array index) */
static IslValue *isl_seq_set(IslValue *val, IslValue *seq, IslValue *idxv, const char *who) {
  if (!idxv || idxv->tag != ISL_V_INT || !seq) return isl_make_error(who);
  int64_t i = idxv->as.i64;
  if (seq->tag == ISL_V_VECTOR) {
    if (i < 0 || i >= seq->as.vec.len) return isl_make_error("index out of range");
    seq->as.vec.items[i] = val;
    return val;
  }
  return isl_make_error(who);
}
static IslValue *prim_set_elt(IslEnv *e, int32_t c, IslValue **a) {
  (void)e; if (c != 3) return isl_make_error("set-elt: arity");
  return isl_seq_set(a[0], a[1], a[2], "set-elt: bad sequence");
}
static IslValue *prim_set_aref(IslEnv *e, int32_t c, IslValue **a) {
  (void)e; if (c != 3) return isl_make_error("set-aref: arity");
  return isl_seq_set(a[0], a[1], a[2], "set-aref: bad array");
}

static IslValue *prim_reverse(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  if (argc != 1) return isl_make_error("reverse: arity");
  IslValue *v = argv[0];
  if (!(v && (v->tag == ISL_V_CONS || v->tag == ISL_V_NIL)) || !isl_is_list(v))
    return isl_make_error("reverse: not a proper list");
  IslValue *acc = (IslValue *)isl_rt_nil();
  for (; v && v->tag == ISL_V_CONS; v = v->as.cons.cdr)
    acc = isl_cons(v->as.cons.car, acc);
  return acc;
}

/* (append list ...) — concatenate proper lists; the last argument may be any
   object and becomes the tail. */
static IslValue *prim_append(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  if (argc == 0) return (IslValue *)isl_rt_nil();
  IslValue *head = (IslValue *)isl_rt_nil();
  IslValue *tail = NULL;
  for (int32_t i = 0; i < argc - 1; i++) {
    IslValue *lst = argv[i];
    if (!(lst && (lst->tag == ISL_V_CONS || lst->tag == ISL_V_NIL)) || !isl_is_list(lst))
      return isl_make_error("append: non-final arguments must be proper lists");
    for (IslValue *p = lst; p && p->tag == ISL_V_CONS; p = p->as.cons.cdr)
      isl_list_append1(&head, &tail, p->as.cons.car);
  }
  IslValue *last = argv[argc - 1];
  if (tail == NULL) return last;
  tail->as.cons.cdr = last;
  return head;
}

/* Object identity for eq/eql.  Symbols compare by name (the runtime does not
   intern), integers and booleans by value; everything else by pointer. */
static int isl_eq(IslValue *a, IslValue *b) {
  if (a == b) return 1;
  if (!a || !b || a->tag != b->tag) return 0;
  switch (a->tag) {
    case ISL_V_INT:    return a->as.i64 == b->as.i64;
    case ISL_V_RATIO:  return a->as.ratio.num == b->as.ratio.num &&
                              a->as.ratio.den == b->as.ratio.den;
    case ISL_V_FLOAT:  return a->as.f64 == b->as.f64;
    case ISL_V_CHAR:   return a->as.ch == b->as.ch;
    case ISL_V_BOOL:   return a->as.b == b->as.b;
    case ISL_V_NIL:    return 1;
    case ISL_V_SYMBOL: return strcmp(a->as.str, b->as.str) == 0;
    default:           return 0;
  }
}

static IslValue *prim_eq_id(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  if (argc != 2) return isl_make_error("eq: arity");
  return isl_eq(argv[0], argv[1]) ? g_true : g_false;
}

/* ---- non-local exit: block/return-from and catch/throw (setjmp/longjmp) ----
   A single unified stack of exit points is used for both constructs so that a
   throw/return-from that jumps past intervening frames of the *other* kind
   still discards them correctly (the target frame's next pointer is the
   restored stack top).  kind 0 = catch (match tag by eql), 1 = block (match
   block name symbol by eql). */
typedef struct ExitFrame {
  jmp_buf buf;
  int kind;
  IslValue *key;
  IslValue *result;
  struct ExitFrame *next;
} ExitFrame;

/* kind: 0 = catch (eql tag), 1 = block (eql name), 2 = unwind-protect. */
static ExitFrame *g_exit_stack = NULL;
/* A non-local exit in flight: the target frame to reach and the value to
   deliver.  Unwinding proceeds one unwind-protect frame at a time so each
   cleanup runs before control reaches the target. */
static ExitFrame *g_unwind_target = NULL;
static IslValue *g_unwind_value = NULL;

/* longjmp toward the pending target, stopping first at the nearest
   unwind-protect (kind 2) frame strictly above it so its cleanup can run. */
static void continue_unwind(void) {
  ExitFrame *jmp_to = g_unwind_target;
  for (ExitFrame *f = g_exit_stack; f && f != g_unwind_target; f = f->next) {
    if (f->kind == 2) { jmp_to = f; break; }
  }
  longjmp(jmp_to->buf, 1);
}

/* Establish a catch/block exit point, run the thunk, return its value; a
   matching throw/return-from delivers the non-local value here. */
static IslValue *exit_enter(IslEnv *env, int kind, IslValue *key, IslValue *thunk) {
  ExitFrame frame;
  frame.kind = kind;
  frame.key = key;
  frame.result = NULL;
  frame.next = g_exit_stack;
  if (setjmp(frame.buf) == 0) {
    g_exit_stack = &frame;
    IslValue *r = (IslValue *)isl_rt_call(env, thunk, 0, NULL);
    g_exit_stack = frame.next; /* normal exit: pop */
    return r;
  }
  /* We are the unwind target (catch/block frames are only landed on as the
     final destination). */
  g_exit_stack = frame.next;
  IslValue *v = g_unwind_value;
  g_unwind_target = NULL;
  g_unwind_value = NULL;
  return v;
}

static IslValue *exit_unwind(int kind, IslValue *key, IslValue *value,
                             const char *err) {
  ExitFrame *target = NULL;
  for (ExitFrame *f = g_exit_stack; f; f = f->next) {
    if (f->kind == kind && isl_eq(f->key, key)) { target = f; break; }
  }
  if (!target) return isl_make_error(err);
  g_unwind_target = target;
  g_unwind_value = value;
  continue_unwind();
  return NULL; /* unreachable */
}

static IslValue *prim_catch(IslEnv *env, int32_t argc, IslValue **argv) {
  if (argc != 2) return isl_make_error("catch: arity");
  return exit_enter(env, 0, argv[0], argv[1]);
}
static IslValue *prim_throw(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  if (argc != 2) return isl_make_error("throw: arity");
  return exit_unwind(0, argv[0], argv[1], "throw: no matching catch tag");
}
static IslValue *prim_block(IslEnv *env, int32_t argc, IslValue **argv) {
  if (argc != 2) return isl_make_error("block: arity");
  return exit_enter(env, 1, argv[0], argv[1]);
}
static IslValue *prim_return_from(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  if (argc != 2) return isl_make_error("return-from: arity");
  return exit_unwind(1, argv[0], argv[1], "return-from: no matching block");
}

/* (%unwind-protect protected-thunk cleanup-thunk): the cleanup always runs,
   whether protected returns normally or a non-local exit unwinds through it. */
static IslValue *prim_unwind_protect(IslEnv *env, int32_t argc, IslValue **argv) {
  if (argc != 2) return isl_make_error("unwind-protect: arity");
  ExitFrame frame;
  frame.kind = 2;
  frame.key = NULL;
  frame.result = NULL;
  frame.next = g_exit_stack;
  if (setjmp(frame.buf) == 0) {
    g_exit_stack = &frame;
    IslValue *r = (IslValue *)isl_rt_call(env, argv[0], 0, NULL);
    g_exit_stack = frame.next;
    (void)isl_rt_call(env, argv[1], 0, NULL); /* normal cleanup */
    return r;
  }
  /* An exit is unwinding through us: pop, run cleanup, then continue. */
  g_exit_stack = frame.next;
  (void)isl_rt_call(env, argv[1], 0, NULL);
  continue_unwind();
  return NULL; /* unreachable */
}

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

/* (format stream control arg...) — minimal directive support: ~A ~S ~D ~% ~~.
   Output always goes to stdout (the native runtime models only one stream). */
static IslValue *prim_format(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  if (argc < 2) return isl_make_error("format: expects a stream and a control string");
  IslValue *ctrl = argv[1];
  if (!ctrl || ctrl->tag != ISL_V_STRING)
    return isl_make_error("format: control argument must be a string");
  const char *s = ctrl->as.str;
  int32_t argi = 2;
  for (const char *p = s; *p; p++) {
    if (*p != '~') { putchar(*p); continue; }
    p++;
    if (!*p) break;
    switch (*p) {
      case '%': putchar('\n'); break;
      case '&': putchar('\n'); break;  /* ~& fresh-line (single stream: approximate) */
      case '~': putchar('~'); break;
      case 'A': case 'a':
        if (argi < argc) isl_write_value(argv[argi++], 0);
        break;
      case 'S': case 's':
        if (argi < argc) isl_write_value(argv[argi++], 1);
        break;
      case 'D': case 'd':
        if (argi < argc) isl_write_value(argv[argi++], 0);
        break;
      case 'X': case 'x':
        if (argi < argc) isl_format_radix(argv[argi++], 16);
        break;
      case 'O': case 'o':
        if (argi < argc) isl_format_radix(argv[argi++], 8);
        break;
      case 'B': case 'b':
        if (argi < argc) isl_format_radix(argv[argi++], 2);
        break;
      default:
        /* Unknown directive: emitting it literally produces silently-wrong
           output, so fail loudly instead. */
        fflush(stdout);
        fprintf(stderr, "isl_rt: format: unsupported directive ~%c\n", *p);
        exit(70);
    }
  }
  return (IslValue *)isl_rt_nil();
}

/* Print an integer value in the given radix (2/8/16) for ~B/~O/~X.
   Non-integers fail loudly rather than print a wrong representation. */
static void isl_format_radix(IslValue *v, int radix) {
  if (!v || v->tag != ISL_V_INT) {
    fflush(stdout);
    fprintf(stderr, "isl_rt: format: ~B/~O/~X expects an integer\n");
    exit(70);
  }
  int64_t n = v->as.i64;
  if (n == 0) { putchar('0'); return; }
  uint64_t mag = (n < 0) ? (uint64_t)(-(n + 1)) + 1u : (uint64_t)n;
  char buf[65];
  int i = 0;
  const char *digits = "0123456789abcdef";
  while (mag > 0) {
    buf[i++] = digits[mag % (uint64_t)radix];
    mag /= (uint64_t)radix;
  }
  if (n < 0) putchar('-');
  while (i > 0) putchar(buf[--i]);
}

/* Format a double as the shortest decimal that round-trips, always carrying a
   decimal point (or exponent) so it reads back as a float — matching the
   interpreter's flonum printing (e.g. 4.0, 0.5, 0.1). */
static void isl_format_double(char *out, size_t outsz, double d) {
  for (int p = 1; p <= 17; p++) {
    snprintf(out, outsz, "%.*g", p, d);
    if (strtod(out, NULL) == d) break;
  }
  if (!strpbrk(out, ".eEnN")) {
    size_t L = strlen(out);
    if (L + 2 < outsz) { out[L] = '.'; out[L + 1] = '0'; out[L + 2] = '\0'; }
  }
}

/* Write a value to stdout.  readable!=0 selects S-style (strings quoted),
   readable==0 selects A-style (strings unquoted). */
static void isl_write_value(IslValue *v, int readable) {
  if (!v) {
    printf("#<null>");
    return;
  }
  switch (v->tag) {
    case ISL_V_INT:
      printf("%lld", (long long)v->as.i64);
      break;
    case ISL_V_RATIO:
      printf("%lld/%lld", (long long)v->as.ratio.num, (long long)v->as.ratio.den);
      break;
    case ISL_V_FLOAT: {
      char buf[64];
      isl_format_double(buf, sizeof(buf), v->as.f64);
      printf("%s", buf);
      break;
    }
    case ISL_V_CHAR:
      if (readable) {
        /* #\<char> readable syntax, with names for a few non-graphic chars */
        switch (v->as.ch) {
          case ' ':  printf("#\\space");   break;
          case '\n': printf("#\\newline"); break;
          case '\t': printf("#\\tab");      break;
          default:   printf("#\\%c", (int)v->as.ch); break;
        }
      } else {
        printf("%c", (int)v->as.ch);
      }
      break;
    case ISL_V_VECTOR:
      printf("#(");
      for (int64_t i = 0; i < v->as.vec.len; i++) {
        if (i > 0) printf(" ");
        isl_write_value(v->as.vec.items[i], readable);
      }
      printf(")");
      break;
    case ISL_V_BOOL:
      printf(v->as.b ? "#t" : "#f");
      break;
    case ISL_V_NIL:
      printf("nil");
      break;
    case ISL_V_SYMBOL:
      printf("%s", v->as.str);
      break;
    case ISL_V_STRING:
      if (readable) printf("\"%s\"", v->as.str);
      else printf("%s", v->as.str);
      break;
    case ISL_V_FUNCTION:
      printf("#<function>");
      break;
    case ISL_V_STREAM:
      printf("#<stream>");
      break;
    case ISL_V_CONS: {
      IslValue *cur = v;
      printf("(");
      for (;;) {
        isl_write_value(cur->as.cons.car, readable);
        IslValue *rest = cur->as.cons.cdr;
        if (rest && rest->tag == ISL_V_CONS) {
          printf(" ");
          cur = rest;
          continue;
        }
        if (rest && rest->tag != ISL_V_NIL) {
          printf(" . ");
          isl_write_value(rest, readable);
        }
        break;
      }
      printf(")");
      break;
    }
    case ISL_V_ERROR:
      printf("#<error %s>", v->as.str);
      break;
    default:
      printf("#<unknown>");
      break;
  }
}

static void isl_print_value(IslValue *v) {
  isl_write_value(v, 1);
}

void *isl_rt_make_int(int64_t x) {
  IslValue *v = isl_alloc_value(ISL_V_INT);
  v->as.i64 = x;
  return v;
}

/* Build a float literal from its decimal text (codegen passes a string to avoid
   LLVM's hex-float literal requirement; strtod round-trips the reader's value). */
void *isl_rt_make_float(void *p) {
  return isl_make_float(strtod((const char *)p, NULL));
}

/* Build a character literal from its code point. */
void *isl_rt_make_char(int64_t cp) {
  return isl_make_char(cp);
}

void *isl_rt_make_symbol(void *p) {
  const char *s = (const char *)p;
  IslValue *v = isl_alloc_value(ISL_V_SYMBOL);
  v->as.str = isl_strdup(s);
  return v;
}

void *isl_rt_make_string(void *p) {
  const char *s = (const char *)p;
  IslValue *v = isl_alloc_value(ISL_V_STRING);
  v->as.str = isl_strdup(s);
  return v;
}

void *isl_rt_nil(void) {
  if (!g_nil) {
    g_nil = isl_alloc_value(ISL_V_NIL);
  }
  return g_nil;
}

void *isl_rt_true(void) {
  if (!g_true) {
    g_true = isl_alloc_value(ISL_V_BOOL);
    g_true->as.b = 1;
  }
  return g_true;
}

void *isl_rt_false(void) {
  if (!g_false) {
    g_false = isl_alloc_value(ISL_V_BOOL);
    g_false->as.b = 0;
  }
  return g_false;
}

void *isl_rt_lookup(void *envp, void *keyp) {
  IslEnv *env = (IslEnv *)envp;
  IslValue *key = (IslValue *)keyp;
  if (!key || key->tag != ISL_V_SYMBOL) {
    return isl_make_error("lookup: key must be symbol");
  }
  IslBinding *b = isl_find_binding(env, key->as.str);
  if (!b) {
    return isl_make_error("unbound variable");
  }
  return b->value;
}

/* setq: update the nearest existing binding of `sym`, or define it in `env`
   when none exists.  Returns the assigned value. */
void *isl_rt_set(void *envp, void *symp, void *valp) {
  IslEnv *env = (IslEnv *)envp;
  IslValue *sym = (IslValue *)symp;
  IslValue *val = (IslValue *)valp;
  if (!sym || sym->tag != ISL_V_SYMBOL) {
    return isl_make_error("set: key must be symbol");
  }
  IslBinding *b = isl_find_binding(env, sym->as.str);
  if (b) {
    b->value = val;
  } else {
    isl_define_raw(env, sym->as.str, val);
  }
  return val;
}

void *isl_rt_lookup_param(void *envp, int32_t index) {
  IslEnv *env = (IslEnv *)envp;
  if (!env || index < 0 || index >= env->argc || !env->argv) {
    return isl_make_error("arity mismatch");
  }
  return env->argv[index];
}

void *isl_rt_call(void *envp, void *fnp, int32_t argc, void *argvp) {
  IslEnv *env = (IslEnv *)envp;
  IslValue *fnv = (IslValue *)fnp;
  IslValue **argv = (IslValue **)argvp;
  if (!fnv || fnv->tag == ISL_V_ERROR) return fnv;
  if (fnv->tag != ISL_V_FUNCTION || !fnv->as.fn) {
    return isl_make_error("attempt to call non-function");
  }
  IslFunction *fn = fnv->as.fn;
  if (fn->arity >= 0 && fn->arity != argc) {
    return isl_make_error("arity mismatch");
  }
  if (fn->kind == ISL_FN_PRIMITIVE) {
    return fn->impl.primitive(env, argc, argv);
  }

  IslEnv *child = (IslEnv *)calloc(1, sizeof(IslEnv));
  if (!child) {
    fprintf(stderr, "isl_rt: out of memory\n");
    exit(2);
  }
  /* Lexical scoping: a compiled function runs in the environment where it was
     defined (captured at closure-creation time), not where it is called. */
  child->parent = (fn->env != NULL) ? fn->env : env;
  child->bindings = NULL;
  child->argc = argc;
  child->argv = argv;
  return fn->impl.compiled(child);
}

_Bool isl_rt_truthy(void *v) {
  return isl_truthy((IslValue *)v) ? 1 : 0;
}

void *isl_rt_unsupported(void *msgp) {
  /* An unsupported construct cannot be honored correctly.  Returning an error
     sentinel here is dangerous: when its result is discarded (e.g. a `while`
     in a `seq` whose value is unused) the program silently keeps running with
     stale state and prints a *wrong* answer.  To never return a wrong value we
     fail loudly and stop the process instead.

     codegen passes a raw C string pointer here (the same `str-ptr` ABI used by
     isl_rt_make_string / isl_rt_make_symbol), not an IslValue*. */
  const char *text = (const char *)msgp;
  fflush(stdout);
  fprintf(stderr, "isl_rt: %s\n", text ? text : "unsupported operation");
  exit(70); /* EX_SOFTWARE: this program uses a feature the native backend lacks */
}

void *isl_rt_create_env(void) {
  IslEnv *env = (IslEnv *)calloc(1, sizeof(IslEnv));
  if (!env) {
    fprintf(stderr, "isl_rt: out of memory\n");
    exit(2);
  }
  env->parent = NULL;
  env->bindings = NULL;
  env->argc = 0;
  env->argv = NULL;

  (void)isl_rt_nil();
  (void)isl_rt_true();
  (void)isl_rt_false();
  if (!g_stdout) g_stdout = isl_alloc_value(ISL_V_STREAM);
  if (!g_stderr) g_stderr = isl_alloc_value(ISL_V_STREAM);

  isl_define_raw(env, "nil", g_nil);
  isl_define_raw(env, "t", g_true);
  return env;
}

void isl_rt_install_primitives(void *envp) {
  IslEnv *env = (IslEnv *)envp;
  isl_define_raw(env, "+", isl_make_primitive("+", prim_plus));
  isl_define_raw(env, "*", isl_make_primitive("*", prim_mul));
  isl_define_raw(env, "-", isl_make_primitive("-", prim_minus));
  isl_define_raw(env, "/", isl_make_primitive("/", prim_div));
  isl_define_raw(env, "=", isl_make_primitive("=", prim_eq));
  isl_define_raw(env, "<", isl_make_primitive("<", prim_lt));
  isl_define_raw(env, ">", isl_make_primitive(">", prim_gt));
  isl_define_raw(env, "<=", isl_make_primitive("<=", prim_le));
  isl_define_raw(env, ">=", isl_make_primitive(">=", prim_ge));
  isl_define_raw(env, "/=", isl_make_primitive("/=", prim_ne));
  isl_define_raw(env, "abs", isl_make_primitive("abs", prim_abs));
  isl_define_raw(env, "max", isl_make_primitive("max", prim_max));
  isl_define_raw(env, "min", isl_make_primitive("min", prim_min));
  isl_define_raw(env, "mod", isl_make_primitive("mod", prim_mod));
  isl_define_raw(env, "expt", isl_make_primitive("expt", prim_expt));
  isl_define_raw(env, "floatp", isl_make_primitive("floatp", prim_floatp));
  isl_define_raw(env, "float", isl_make_primitive("float", prim_float));
  /* characters */
  isl_define_raw(env, "char=",  isl_make_primitive("char=",  prim_char_eq));
  isl_define_raw(env, "char/=", isl_make_primitive("char/=", prim_char_ne));
  isl_define_raw(env, "char<",  isl_make_primitive("char<",  prim_char_lt));
  isl_define_raw(env, "char>",  isl_make_primitive("char>",  prim_char_gt));
  isl_define_raw(env, "char<=", isl_make_primitive("char<=", prim_char_le));
  isl_define_raw(env, "char>=", isl_make_primitive("char>=", prim_char_ge));
  isl_define_raw(env, "char->integer", isl_make_primitive("char->integer", prim_char_to_int));
  isl_define_raw(env, "integer->char", isl_make_primitive("integer->char", prim_int_to_char));
  /* strings */
  isl_define_raw(env, "string-append", isl_make_primitive("string-append", prim_string_append));
  isl_define_raw(env, "string=", isl_make_primitive("string=", prim_string_eq));
  isl_define_raw(env, "create-string", isl_make_primitive("create-string", prim_create_string));
  isl_define_raw(env, "char-index", isl_make_primitive("char-index", prim_char_index));
  /* vectors / 1-D arrays */
  isl_define_raw(env, "vector", isl_make_primitive("vector", prim_vector));
  isl_define_raw(env, "create-vector", isl_make_primitive("create-vector", prim_create_vector));
  isl_define_raw(env, "create-array", isl_make_primitive("create-array", prim_create_array));
  isl_define_raw(env, "elt", isl_make_primitive("elt", prim_elt));
  isl_define_raw(env, "aref", isl_make_primitive("aref", prim_aref));
  isl_define_raw(env, "vector-ref", isl_make_primitive("vector-ref", prim_aref));
  isl_define_raw(env, "set-elt", isl_make_primitive("set-elt", prim_set_elt));
  isl_define_raw(env, "set-aref", isl_make_primitive("set-aref", prim_set_aref));
  isl_define_raw(env, "print", isl_make_primitive("print", prim_print));
  isl_define_raw(env, "not", isl_make_primitive("not", prim_not));
  isl_define_raw(env, "funcall", isl_make_primitive("funcall", prim_funcall));
  isl_define_raw(env, "apply", isl_make_primitive("apply", prim_apply));
  isl_define_raw(env, "cons", isl_make_primitive("cons", prim_cons));
  isl_define_raw(env, "car", isl_make_primitive("car", prim_car));
  isl_define_raw(env, "cdr", isl_make_primitive("cdr", prim_cdr));
  isl_define_raw(env, "list", isl_make_primitive("list", prim_list));
  isl_define_raw(env, "consp", isl_make_primitive("consp", prim_consp));
  isl_define_raw(env, "null", isl_make_primitive("null", prim_null));
  isl_define_raw(env, "mapcar", isl_make_primitive("mapcar", prim_mapcar));
  isl_define_raw(env, "mapc", isl_make_primitive("mapc", prim_mapc));
  isl_define_raw(env, "length", isl_make_primitive("length", prim_length));
  isl_define_raw(env, "reverse", isl_make_primitive("reverse", prim_reverse));
  isl_define_raw(env, "append", isl_make_primitive("append", prim_append));
  isl_define_raw(env, "eq", isl_make_primitive("eq", prim_eq_id));
  isl_define_raw(env, "eql", isl_make_primitive("eql", prim_eq_id));
  /* non-local exit primitives that lowering rewrites block/catch/etc. into */
  isl_define_raw(env, "%catch", isl_make_primitive("%catch", prim_catch));
  isl_define_raw(env, "%throw", isl_make_primitive("%throw", prim_throw));
  isl_define_raw(env, "%block", isl_make_primitive("%block", prim_block));
  isl_define_raw(env, "%return-from", isl_make_primitive("%return-from", prim_return_from));
  isl_define_raw(env, "%unwind-protect",
                 isl_make_primitive("%unwind-protect", prim_unwind_protect));
  /* dynamic variables + convert */
  isl_define_raw(env, "%dynamic-get", isl_make_primitive("%dynamic-get", prim_dynamic_get));
  isl_define_raw(env, "%dynamic-set", isl_make_primitive("%dynamic-set", prim_dynamic_set));
  isl_define_raw(env, "convert", isl_make_primitive("convert", prim_convert));
  isl_define_raw(env, "assure", isl_make_primitive("assure", prim_assure));
  isl_define_raw(env, "%the", isl_make_primitive("%the", prim_the));
  /* class-name designators usable as the second argument to convert */
  isl_define_raw(env, "<integer>",   (IslValue *)isl_rt_make_symbol((void *)"<integer>"));
  isl_define_raw(env, "<float>",     (IslValue *)isl_rt_make_symbol((void *)"<float>"));
  isl_define_raw(env, "<number>",    (IslValue *)isl_rt_make_symbol((void *)"<number>"));
  isl_define_raw(env, "<character>", (IslValue *)isl_rt_make_symbol((void *)"<character>"));
  isl_define_raw(env, "<string>",    (IslValue *)isl_rt_make_symbol((void *)"<string>"));
  isl_define_raw(env, "<symbol>",    (IslValue *)isl_rt_make_symbol((void *)"<symbol>"));
  isl_define_raw(env, "<list>",      (IslValue *)isl_rt_make_symbol((void *)"<list>"));
  isl_define_raw(env, "format", isl_make_primitive("format", prim_format));
  isl_define_raw(env, "standard-output", isl_make_primitive("standard-output", prim_standard_output));
  isl_define_raw(env, "error-output", isl_make_primitive("error-output", prim_error_output));
}

/* Create a compiled-function value capturing its defining environment.
   envp is the lexical environment in effect where the closure is created. */
void *isl_rt_make_closure(void *entryp, int32_t arity, void *envp) {
  IslValue *v = isl_alloc_value(ISL_V_FUNCTION);
  IslFunction *f = (IslFunction *)calloc(1, sizeof(IslFunction));
  if (!f) {
    fprintf(stderr, "isl_rt: out of memory\n");
    exit(2);
  }
  f->kind = ISL_FN_COMPILED;
  f->arity = arity;
  f->name = "closure";
  f->env = (IslEnv *)envp;
  f->impl.compiled = (IslCompiledFn)entryp;
  v->as.fn = f;
  return v;
}

void isl_rt_define(void *envp, void *symp, void *valuep) {
  IslEnv *env = (IslEnv *)envp;
  IslValue *sym = (IslValue *)symp;
  IslValue *value = (IslValue *)valuep;
  if (!sym || sym->tag != ISL_V_SYMBOL) {
    return;
  }
  isl_define_raw(env, sym->as.str, value);
}

void isl_rt_println(void *vp) {
  IslValue *v = (IslValue *)vp;
  if (!v) {
    return;
  }
  isl_print_value(v);
  fputc('\n', stdout);
  if (v->tag == ISL_V_ERROR) {
    fflush(stdout);
    exit(1);
  }
}
