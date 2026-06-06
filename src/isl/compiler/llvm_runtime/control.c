/* llvm_runtime/control.c
   ネイティブ C ランタイムの断片。bin/islc / bin/islc-aot は
   src/isl/compiler/llvm_runtime.c を単一翻訳単位として clang に渡し、
   本ファイルはそこから #include される（単独ではコンパイル不可）。 */

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

