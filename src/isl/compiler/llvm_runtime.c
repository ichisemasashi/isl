#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

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

typedef enum {
  ISL_V_INT,
  ISL_V_BOOL,
  ISL_V_NIL,
  ISL_V_SYMBOL,
  ISL_V_STRING,
  ISL_V_FUNCTION,
  ISL_V_CONS,
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
    int b;
    const char *str;
    IslFunction *fn;
    struct {
      IslValue *car;
      IslValue *cdr;
    } cons;
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
  if (v && v->tag == ISL_V_NIL) return (IslValue *)isl_rt_make_int(0);
  if (!isl_is_list(v)) return isl_make_error("length: not a proper list");
  return (IslValue *)isl_rt_make_int(isl_list_length(v));
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
    case ISL_V_CONS: {
      IslValue *cur = v;
      printf("(");
      for (;;) {
        isl_print_value(cur->as.cons.car);
        IslValue *rest = cur->as.cons.cdr;
        if (rest && rest->tag == ISL_V_CONS) {
          printf(" ");
          cur = rest;
          continue;
        }
        if (rest && rest->tag != ISL_V_NIL) {
          printf(" . ");
          isl_print_value(rest);
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
