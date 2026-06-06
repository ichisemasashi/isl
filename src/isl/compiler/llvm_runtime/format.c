/* llvm_runtime/format.c
   ネイティブ C ランタイムの断片。bin/islc / bin/islc-aot は
   src/isl/compiler/llvm_runtime.c を単一翻訳単位として clang に渡し、
   本ファイルはそこから #include される（単独ではコンパイル不可）。 */

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
    case ISL_V_CONDITION:
      printf("#<condition %s: %s>", v->as.cond.cls, v->as.cond.msg);
      break;
    case ISL_V_CLASS:
      printf("#<class %s>", v->as.klass->name);
      break;
    case ISL_V_INSTANCE:
      printf("#<instance of %s>", v->as.inst.cls->as.klass->name);
      break;
    case ISL_V_GENERIC:
      printf("#<generic-function %s>", v->as.gen.name);
      break;
    case ISL_V_ARRAY:
      /* match the interpreter's internal display: #(isl-array (d...) #(flat...)) */
      printf("#(isl-array (");
      for (int d = 0; d < v->as.arr.ndim; d++) {
        if (d) printf(" ");
        printf("%lld", (long long)v->as.arr.dims[d]);
      }
      printf(") #(");
      for (int64_t j = 0; j < v->as.arr.len; j++) {
        if (j) printf(" ");
        isl_write_value(v->as.arr.items[j], readable);
      }
      printf("))");
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

