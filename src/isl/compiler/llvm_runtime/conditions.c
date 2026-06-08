/* llvm_runtime/conditions.c
   ネイティブ C ランタイムの断片。bin/islc / bin/islc-aot は
   src/isl/compiler/llvm_runtime.c を単一翻訳単位として clang に渡し、
   本ファイルはそこから #include される（単独ではコンパイル不可）。 */

/* ---- conditions: error / signal / handler-case ---- */
/* The condition class hierarchy (parent of each class, NULL at the root). */
static const char *cond_parent(const char *c) {
  if (!strcmp(c, "<simple-error>"))        return "<error>";
  if (!strcmp(c, "<arithmetic-error>"))    return "<error>";
  if (!strcmp(c, "<division-by-zero>"))    return "<arithmetic-error>";
  if (!strcmp(c, "<domain-error>"))        return "<program-error>";
  if (!strcmp(c, "<undefined-function>"))  return "<undefined-entity>";
  if (!strcmp(c, "<undefined-entity>"))    return "<program-error>";
  if (!strcmp(c, "<program-error>"))       return "<error>";
  if (!strcmp(c, "<error>"))               return "<serious-condition>";
  if (!strcmp(c, "<serious-condition>"))   return "<condition>";
  return NULL;
}
static int isl_cond_subclass(const char *sub, const char *super) {
  for (const char *c = sub; c; c = cond_parent(c))
    if (strcmp(c, super) == 0) return 1;
  return 0;
}
static const char *isl_cond_class(IslValue *c) {
  if (c && c->tag == ISL_V_CONDITION) return c->as.cond.cls;
  return "<error>"; /* plain ISL_V_ERROR values are generic errors */
}
static const char *isl_cond_message(IslValue *c) {
  if (c && c->tag == ISL_V_CONDITION) return c->as.cond.msg;
  if (c && c->tag == ISL_V_ERROR)     return c->as.str;
  return "condition";
}

/* Signal a condition: unwind to the nearest handler-case frame (running any
   intervening unwind-protect cleanups via continue_unwind).  Uncaught => abort. */
static IslValue *isl_signal(IslValue *cond) {
  ExitFrame *target = NULL;
  for (ExitFrame *f = g_exit_stack; f; f = f->next)
    if (f->kind == 3) { target = f; break; }
  if (!target) {
    fflush(stdout);
    fprintf(stderr, "isl_rt: uncaught condition (%s): %s\n",
            isl_cond_class(cond), isl_cond_message(cond));
    exit(70);
  }
  g_unwind_target = target;
  g_unwind_value = cond;
  continue_unwind();
  return NULL; /* unreachable */
}

static IslValue *isl_signal_div0(const char *op) {
  (void)op;
  return isl_signal(isl_make_condition("<division-by-zero>", "division by zero",
                                       (IslValue *)isl_rt_nil()));
}

/* (error format-string irritant...) — signal a <simple-error>. */
static IslValue *prim_error(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  if (argc < 1 || !argv[0] || argv[0]->tag != ISL_V_STRING)
    return isl_make_error("error: first argument must be a string");
  IslValue *irr = (IslValue *)isl_rt_nil();
  for (int32_t i = argc - 1; i >= 1; i--) irr = isl_cons(argv[i], irr);
  return isl_signal(isl_make_condition("<simple-error>", argv[0]->as.str, irr));
}

/* Dispatch a condition against handler-case clauses passed as flat alternating
   (class-symbol, one-argument-handler-closure) arguments in argv[1..].  */
static IslValue *handler_dispatch(IslEnv *env, IslValue *cond,
                                  int32_t argc, IslValue **argv, int *matched) {
  const char *ccls = isl_cond_class(cond);
  for (int32_t i = 1; i + 1 < argc; i += 2) {
    IslValue *csym = argv[i];
    IslValue *handler = argv[i + 1];
    const char *cs = (csym && csym->tag == ISL_V_SYMBOL) ? csym->as.str : "";
    if (strcmp(cs, "t") == 0 || strcmp(cs, "otherwise") == 0 ||
        isl_cond_subclass(ccls, cs)) {
      *matched = 1;
      IslValue *a[1] = { cond };
      return (IslValue *)isl_rt_call(env, handler, 1, a);
    }
  }
  *matched = 0;
  return NULL;
}

/* (%handler-case body-thunk class1 handler1 class2 handler2 ...) */
static IslValue *prim_handler_case(IslEnv *env, int32_t argc, IslValue **argv) {
  if (argc < 1 || (argc % 2) != 1) return isl_make_error("handler-case: arity");
  IslValue *body = argv[0];
  ExitFrame frame;
  frame.kind = 3;
  frame.key = NULL;
  frame.result = NULL;
  frame.next = g_exit_stack;
  if (setjmp(frame.buf) == 0) {
    g_exit_stack = &frame;
    IslValue *r = (IslValue *)isl_rt_call(env, body, 0, NULL);
    g_exit_stack = frame.next;
    /* Also catch errors that propagated as return values (e.g. type errors). */
    if (r && (r->tag == ISL_V_CONDITION || r->tag == ISL_V_ERROR)) {
      int matched = 0;
      IslValue *h = handler_dispatch(env, r, argc, argv, &matched);
      if (matched) return h;
      return r; /* no clause matched: propagate as a value */
    }
    return r;
  }
  /* a signaled condition unwound to us */
  g_exit_stack = frame.next;
  IslValue *cond = g_unwind_value;
  g_unwind_target = NULL;
  g_unwind_value = NULL;
  int matched = 0;
  IslValue *h = handler_dispatch(env, cond, argc, argv, &matched);
  if (matched) return h;
  return (IslValue *)isl_signal(cond); /* re-signal to an outer handler */
}

/* (condition-message c) — accessor used by some handlers. */
static IslValue *prim_condition_message(IslEnv *env, int32_t argc, IslValue **argv) {
  (void)env;
  if (argc != 1) return isl_make_error("condition-message: arity");
  return isl_str_value(isl_cond_message(argv[0]));
}

