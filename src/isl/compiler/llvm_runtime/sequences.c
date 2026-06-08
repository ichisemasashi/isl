/* llvm_runtime/sequences.c
   ネイティブ C ランタイムの断片。bin/islc / bin/islc-aot は
   src/isl/compiler/llvm_runtime.c を単一翻訳単位として clang に渡し、
   本ファイルはそこから #include される（単独ではコンパイル不可）。 */

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
/* (create-array dims [fill]) — rank-1 yields a vector, rank>=2 an N-D array. */
static IslValue *prim_create_array(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  if (argc < 1) return isl_make_error("create-array: expected dimensions");
  IslValue *dims = argv[0];
  if (!dims || (dims->tag != ISL_V_CONS && dims->tag != ISL_V_NIL) || !isl_is_list(dims))
    return isl_make_error("create-array: dimensions must be a list");
  int ndim = isl_list_length(dims);
  IslValue *fill = (argc >= 2) ? argv[1] : (IslValue *)isl_rt_nil();
  if (ndim == 1) return isl_make_vector_n(dims->as.cons.car->as.i64, fill);
  if (ndim == 0) return isl_make_error("create-array: empty dimensions");
  int64_t *ds = (int64_t *)calloc((size_t)ndim, sizeof(int64_t));
  int64_t total = 1; int i = 0;
  for (IslValue *p = dims; p && p->tag == ISL_V_CONS; p = p->as.cons.cdr) {
    if (p->as.cons.car->tag != ISL_V_INT) { free(ds); return isl_make_error("create-array: dimension must be an integer"); }
    ds[i] = p->as.cons.car->as.i64; total *= ds[i]; i++;
  }
  IslValue *v = isl_alloc_value(ISL_V_ARRAY);
  v->as.arr.dims = ds; v->as.arr.ndim = ndim; v->as.arr.len = total;
  v->as.arr.items = total ? (IslValue **)calloc((size_t)total, sizeof(IslValue *)) : NULL;
  for (int64_t j = 0; j < total; j++) v->as.arr.items[j] = fill;
  return v;
}
/* Row-major flat index for an N-D array given indices in argv[base..base+ndim). */
static int64_t isl_array_index(IslValue *a, int32_t argc, IslValue **argv, int32_t base) {
  if (argc - base != a->as.arr.ndim) return -1;
  int64_t flat = 0;
  for (int d = 0; d < a->as.arr.ndim; d++) {
    IslValue *iv = argv[base + d];
    if (!iv || iv->tag != ISL_V_INT) return -1;
    int64_t i = iv->as.i64;
    if (i < 0 || i >= a->as.arr.dims[d]) return -1;
    flat = flat * a->as.arr.dims[d] + i;
  }
  return flat;
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
  (void)e;
  if (c >= 1 && a[0] && a[0]->tag == ISL_V_ARRAY) {
    int64_t flat = isl_array_index(a[0], c, a, 1);
    if (flat < 0) return isl_make_error("aref: index out of range or rank mismatch");
    return a[0]->as.arr.items[flat];
  }
  if (c != 2) return isl_make_error("aref: arity");
  return isl_seq_ref(a[0], a[1], "aref: bad array");
}
static IslValue *prim_array_dimensions(IslEnv *e, int32_t c, IslValue **a) {
  (void)e; if (c != 1) return isl_make_error("array-dimensions: arity");
  IslValue *v = a[0];
  IslValue *acc = (IslValue *)isl_rt_nil();
  if (v && v->tag == ISL_V_ARRAY) {
    for (int d = v->as.arr.ndim - 1; d >= 0; d--)
      acc = isl_cons((IslValue *)isl_rt_make_int(v->as.arr.dims[d]), acc);
  } else if (v && v->tag == ISL_V_VECTOR) {
    acc = isl_cons((IslValue *)isl_rt_make_int(v->as.vec.len), acc);
  } else if (v && v->tag == ISL_V_STRING) {
    acc = isl_cons((IslValue *)isl_rt_make_int((int64_t)strlen(v->as.str)), acc);
  } else {
    return isl_make_error("array-dimensions: not an array");
  }
  return acc;
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
  (void)e;
  if (c >= 2 && a[1] && a[1]->tag == ISL_V_ARRAY) {
    int64_t flat = isl_array_index(a[1], c, a, 2);
    if (flat < 0) return isl_make_error("set-aref: index out of range or rank mismatch");
    a[1]->as.arr.items[flat] = a[0];
    return a[0];
  }
  if (c != 3) return isl_make_error("set-aref: arity");
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

/* (member obj list) — tail of list starting at the first eql element, or nil. */
static int isl_eq(IslValue *a, IslValue *b);
static IslValue *prim_member(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  if (argc != 2) return isl_make_error("member: arity");
  for (IslValue *p = argv[1]; p && p->tag == ISL_V_CONS; p = p->as.cons.cdr)
    if (isl_eq(argv[0], p->as.cons.car)) return p;
  return (IslValue *)isl_rt_nil();
}
/* (assoc key alist) — first (key . v) pair whose car is eql key, or nil. */
static IslValue *prim_assoc(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  if (argc != 2) return isl_make_error("assoc: arity");
  for (IslValue *p = argv[1]; p && p->tag == ISL_V_CONS; p = p->as.cons.cdr) {
    IslValue *e = p->as.cons.car;
    if (e && e->tag == ISL_V_CONS && isl_eq(argv[0], e->as.cons.car)) return e;
  }
  return (IslValue *)isl_rt_nil();
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

