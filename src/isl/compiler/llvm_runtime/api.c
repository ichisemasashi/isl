/* llvm_runtime/api.c
   ネイティブ C ランタイムの断片。bin/islc / bin/islc-aot は
   src/isl/compiler/llvm_runtime.c を単一翻訳単位として clang に渡し、
   本ファイルはそこから #include される（単独ではコンパイル不可）。 */

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

/* Build a vector literal #(...) from an array of n element values. */
void *isl_rt_vector(int32_t n, void *arrp) {
  IslValue **items = (IslValue **)arrp;
  IslValue *v = isl_make_vector_n(n, (IslValue *)isl_rt_nil());
  for (int32_t i = 0; i < n; i++) v->as.vec.items[i] = items[i];
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
  if (fnv->tag == ISL_V_GENERIC)
    return isl_generic_call(env, fnv, argc, argv);
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
  isl_define_raw(env, "array-dimensions", isl_make_primitive("array-dimensions", prim_array_dimensions));
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
  isl_define_raw(env, "member", isl_make_primitive("member", prim_member));
  isl_define_raw(env, "assoc", isl_make_primitive("assoc", prim_assoc));
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
  /* conditions */
  isl_define_raw(env, "error", isl_make_primitive("error", prim_error));
  isl_define_raw(env, "%handler-case", isl_make_primitive("%handler-case", prim_handler_case));
  isl_define_raw(env, "condition-message", isl_make_primitive("condition-message", prim_condition_message));
  /* CLOS runtime primitives */
  isl_define_raw(env, "make-instance", isl_make_primitive("make-instance", prim_make_instance));
  isl_define_raw(env, "create", isl_make_primitive("create", prim_make_instance));
  isl_define_raw(env, "%slot-read", isl_make_primitive("%slot-read", prim_slot_read));
  isl_define_raw(env, "%slot-write", isl_make_primitive("%slot-write", prim_slot_write));
  isl_define_raw(env, "class-of", isl_make_primitive("class-of", prim_class_of));
  isl_define_raw(env, "instancep", isl_make_primitive("instancep", prim_instancep));
  isl_define_raw(env, "call-next-method", isl_make_primitive("call-next-method", prim_call_next_method));
  isl_define_raw(env, "next-method-p", isl_make_primitive("next-method-p", prim_next_method_p));
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
