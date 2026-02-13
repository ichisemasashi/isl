#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

typedef struct IslValue IslValue;
typedef struct IslEnv IslEnv;

typedef IslValue *(*IslCompiledFn)(IslEnv *env);
typedef IslValue *(*IslPrimitiveFn)(IslEnv *env, int32_t argc, IslValue **argv);

typedef enum {
  ISL_V_INT,
  ISL_V_BOOL,
  ISL_V_NIL,
  ISL_V_SYMBOL,
  ISL_V_STRING,
  ISL_V_FUNCTION,
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
  union {
    IslCompiledFn compiled;
    IslPrimitiveFn primitive;
  } impl;
} IslFunction;

struct IslValue {
  IslTag tag;
  union {
    int64_t i64;
    int b;
    const char *str;
    IslFunction *fn;
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
  if (!v || v->tag != ISL_V_INT) {
    char buf[128];
    snprintf(buf, sizeof(buf), "%s: expected number", who);
    return isl_make_error(buf);
  }
  return v;
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
  int64_t acc = 0;
  for (int32_t i = 0; i < argc; i++) {
    IslValue *n = isl_expect_number(argv[i], "+");
    if (n->tag == ISL_V_ERROR) return n;
    acc += n->as.i64;
  }
  IslValue *out = isl_alloc_value(ISL_V_INT);
  out->as.i64 = acc;
  return out;
}

static IslValue *prim_mul(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  int64_t acc = 1;
  if (argc == 0) {
    IslValue *out = isl_alloc_value(ISL_V_INT);
    out->as.i64 = 1;
    return out;
  }
  for (int32_t i = 0; i < argc; i++) {
    IslValue *n = isl_expect_number(argv[i], "*");
    if (n->tag == ISL_V_ERROR) return n;
    acc *= n->as.i64;
  }
  IslValue *out = isl_alloc_value(ISL_V_INT);
  out->as.i64 = acc;
  return out;
}

static IslValue *prim_minus(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  if (argc <= 0) return isl_make_error("-: arity");
  IslValue *n0 = isl_expect_number(argv[0], "-");
  if (n0->tag == ISL_V_ERROR) return n0;
  int64_t acc = n0->as.i64;
  if (argc == 1) acc = -acc;
  for (int32_t i = 1; i < argc; i++) {
    IslValue *n = isl_expect_number(argv[i], "-");
    if (n->tag == ISL_V_ERROR) return n;
    acc -= n->as.i64;
  }
  IslValue *out = isl_alloc_value(ISL_V_INT);
  out->as.i64 = acc;
  return out;
}

static IslValue *prim_div(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  if (argc <= 0) return isl_make_error("/: arity");
  IslValue *n0 = isl_expect_number(argv[0], "/");
  if (n0->tag == ISL_V_ERROR) return n0;
  int64_t acc = n0->as.i64;
  for (int32_t i = 1; i < argc; i++) {
    IslValue *n = isl_expect_number(argv[i], "/");
    if (n->tag == ISL_V_ERROR) return n;
    if (n->as.i64 == 0) return isl_make_error("/: division by zero");
    acc /= n->as.i64;
  }
  IslValue *out = isl_alloc_value(ISL_V_INT);
  out->as.i64 = acc;
  return out;
}

static IslValue *prim_cmp(const char *name, int32_t argc, IslValue **argv, int mode) {
  if (argc != 2) return isl_make_error("comparison: arity");
  IslValue *a = isl_expect_number(argv[0], name);
  if (a->tag == ISL_V_ERROR) return a;
  IslValue *b = isl_expect_number(argv[1], name);
  if (b->tag == ISL_V_ERROR) return b;
  int ok = 0;
  switch (mode) {
    case 0: ok = (a->as.i64 == b->as.i64); break;
    case 1: ok = (a->as.i64 < b->as.i64); break;
    case 2: ok = (a->as.i64 > b->as.i64); break;
    case 3: ok = (a->as.i64 <= b->as.i64); break;
    case 4: ok = (a->as.i64 >= b->as.i64); break;
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

static void isl_print_value(IslValue *v);

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

static void isl_print_value(IslValue *v) {
  if (!v) {
    printf("#<null>");
    return;
  }
  switch (v->tag) {
    case ISL_V_INT:
      printf("%lld", (long long)v->as.i64);
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
      printf("\"%s\"", v->as.str);
      break;
    case ISL_V_FUNCTION:
      printf("#<function>");
      break;
    case ISL_V_ERROR:
      printf("#<error %s>", v->as.str);
      break;
    default:
      printf("#<unknown>");
      break;
  }
}

void *isl_rt_make_int(int64_t x) {
  IslValue *v = isl_alloc_value(ISL_V_INT);
  v->as.i64 = x;
  return v;
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
  child->parent = env;
  child->bindings = NULL;
  child->argc = argc;
  child->argv = argv;
  return fn->impl.compiled(child);
}

_Bool isl_rt_truthy(void *v) {
  return isl_truthy((IslValue *)v) ? 1 : 0;
}

void *isl_rt_unsupported(void *msgp) {
  IslValue *msg = (IslValue *)msgp;
  if (msg && msg->tag == ISL_V_STRING) {
    return isl_make_error(msg->as.str);
  }
  return isl_make_error("unsupported operation");
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
  isl_define_raw(env, "print", isl_make_primitive("print", prim_print));
  isl_define_raw(env, "not", isl_make_primitive("not", prim_not));
}

void *isl_rt_make_compiled_fun(void *entryp, int32_t arity) {
  IslValue *v = isl_alloc_value(ISL_V_FUNCTION);
  IslFunction *f = (IslFunction *)calloc(1, sizeof(IslFunction));
  if (!f) {
    fprintf(stderr, "isl_rt: out of memory\n");
    exit(2);
  }
  f->kind = ISL_FN_COMPILED;
  f->arity = arity;
  f->name = "compiled";
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
