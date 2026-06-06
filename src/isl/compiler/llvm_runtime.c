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
void *isl_rt_lookup(void *envp, void *keyp);
void isl_rt_define(void *envp, void *symp, void *valuep);

typedef enum {
  ISL_V_INT,
  ISL_V_RATIO,
  ISL_V_FLOAT,
  ISL_V_CHAR,
  ISL_V_VECTOR,
  ISL_V_CONDITION,
  ISL_V_CLASS,
  ISL_V_INSTANCE,
  ISL_V_GENERIC,
  ISL_V_ARRAY,
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
    struct {
      const char *cls;            /* class name, e.g. "<simple-error>" */
      const char *msg;
      struct IslValue *irritants;
    } cond;       /* ISL_V_CONDITION: a signaled condition */
    struct IslClass *klass;       /* ISL_V_CLASS */
    struct {
      struct IslValue *cls;       /* the instance's class value */
      struct IslValue **vals;     /* slot values, parallel to class effective slots */
    } inst;       /* ISL_V_INSTANCE */
    struct {
      const char *name;
      struct IslValue **specs;    /* per-method specializer class value (or NULL) */
      struct IslValue **fns;      /* per-method handler closure */
      int *quals;                 /* per-method qualifier: 0=primary 1=before 2=after 3=around */
      int n;
    } gen;        /* ISL_V_GENERIC */
    struct {
      int64_t *dims; int ndim;
      struct IslValue **items; int64_t len;   /* row-major flat storage */
    } arr;        /* ISL_V_ARRAY: rank >= 2 general array */
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


/* ---- 実装本体（保守性のため論理単位へ分割し、定義順を保って取り込む） ----
   clang には本ファイルが単一翻訳単位として渡されるため、各断片は
   #include で連結され前処理結果は分割前と同一になる。 */
#include "llvm_runtime/numbers.c"
#include "llvm_runtime/sequences.c"
#include "llvm_runtime/control.c"
#include "llvm_runtime/conditions.c"
#include "llvm_runtime/clos.c"
#include "llvm_runtime/dynamic_convert.c"
#include "llvm_runtime/format.c"
#include "llvm_runtime/api.c"
