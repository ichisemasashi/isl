#include <dlfcn.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define FFI_NULL_TOKEN "__ISLISP_FFI_NULL__"

static int parse_long(const char *s, long *out) {
  char *end = NULL;
  errno = 0;
  long v = strtol(s, &end, 10);
  if (errno != 0 || end == s || *end != '\0') return 0;
  *out = v;
  return 1;
}

static int parse_double(const char *s, double *out) {
  char *end = NULL;
  errno = 0;
  double v = strtod(s, &end);
  if (errno != 0 || end == s || *end != '\0') return 0;
  *out = v;
  return 1;
}

static int all_type(const char *const *types, int argc, const char *want) {
  int i;
  for (i = 0; i < argc; i++) {
    if (strcmp(types[i], want) != 0) return 0;
  }
  return 1;
}

static void usage(void) {
  fprintf(stderr,
          "usage: isl_ffi_bridge <lib> <symbol> <ret> <argc> [<type> <value>]...\n");
}

static void emit_result(const char *s) {
  printf("__ISLISP_FFI_RESULT__:%s", s);
}

static void emit_result_long(long v) {
  char buf[64];
  snprintf(buf, sizeof(buf), "%ld", v);
  emit_result(buf);
}

static void emit_result_double(double v) {
  char buf[128];
  snprintf(buf, sizeof(buf), "%.17g", v);
  emit_result(buf);
}

static void emit_result_cstr(const char *s) {
  if (s == NULL) {
    emit_result(FFI_NULL_TOKEN);
  } else {
    emit_result(s);
  }
}

static long call_long_int(void *sym, int n, const long *a) {
  switch (n) {
    case 0: return ((long (*)(void))sym)();
    case 1: return ((long (*)(long))sym)(a[0]);
    case 2: return ((long (*)(long, long))sym)(a[0], a[1]);
    case 3: return ((long (*)(long, long, long))sym)(a[0], a[1], a[2]);
    case 4: return ((long (*)(long, long, long, long))sym)(a[0], a[1], a[2], a[3]);
    case 5: return ((long (*)(long, long, long, long, long))sym)(a[0], a[1], a[2], a[3], a[4]);
    case 6:
      return ((long (*)(long, long, long, long, long, long))sym)(
          a[0], a[1], a[2], a[3], a[4], a[5]);
    case 7:
      return ((long (*)(long, long, long, long, long, long, long))sym)(
          a[0], a[1], a[2], a[3], a[4], a[5], a[6]);
    case 8:
      return ((long (*)(long, long, long, long, long, long, long, long))sym)(
          a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7]);
    default: return 0;
  }
}

static double call_double_int(void *sym, int n, const long *a) {
  switch (n) {
    case 0: return ((double (*)(void))sym)();
    case 1: return ((double (*)(long))sym)(a[0]);
    case 2: return ((double (*)(long, long))sym)(a[0], a[1]);
    case 3: return ((double (*)(long, long, long))sym)(a[0], a[1], a[2]);
    case 4: return ((double (*)(long, long, long, long))sym)(a[0], a[1], a[2], a[3]);
    case 5:
      return ((double (*)(long, long, long, long, long))sym)(
          a[0], a[1], a[2], a[3], a[4]);
    case 6:
      return ((double (*)(long, long, long, long, long, long))sym)(
          a[0], a[1], a[2], a[3], a[4], a[5]);
    case 7:
      return ((double (*)(long, long, long, long, long, long, long))sym)(
          a[0], a[1], a[2], a[3], a[4], a[5], a[6]);
    case 8:
      return ((double (*)(long, long, long, long, long, long, long, long))sym)(
          a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7]);
    default: return 0.0;
  }
}

static const char *call_cstr_int(void *sym, int n, const long *a) {
  switch (n) {
    case 0: return ((const char *(*)(void))sym)();
    case 1: return ((const char *(*)(long))sym)(a[0]);
    case 2: return ((const char *(*)(long, long))sym)(a[0], a[1]);
    case 3: return ((const char *(*)(long, long, long))sym)(a[0], a[1], a[2]);
    case 4: return ((const char *(*)(long, long, long, long))sym)(a[0], a[1], a[2], a[3]);
    case 5:
      return ((const char *(*)(long, long, long, long, long))sym)(
          a[0], a[1], a[2], a[3], a[4]);
    case 6:
      return ((const char *(*)(long, long, long, long, long, long))sym)(
          a[0], a[1], a[2], a[3], a[4], a[5]);
    case 7:
      return ((const char *(*)(long, long, long, long, long, long, long))sym)(
          a[0], a[1], a[2], a[3], a[4], a[5], a[6]);
    case 8:
      return ((const char *(*)(long, long, long, long, long, long, long, long))sym)(
          a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7]);
    default: return NULL;
  }
}

static void call_void_int(void *sym, int n, const long *a) {
  switch (n) {
    case 0: ((void (*)(void))sym)(); break;
    case 1: ((void (*)(long))sym)(a[0]); break;
    case 2: ((void (*)(long, long))sym)(a[0], a[1]); break;
    case 3: ((void (*)(long, long, long))sym)(a[0], a[1], a[2]); break;
    case 4: ((void (*)(long, long, long, long))sym)(a[0], a[1], a[2], a[3]); break;
    case 5: ((void (*)(long, long, long, long, long))sym)(a[0], a[1], a[2], a[3], a[4]); break;
    case 6:
      ((void (*)(long, long, long, long, long, long))sym)(
          a[0], a[1], a[2], a[3], a[4], a[5]);
      break;
    case 7:
      ((void (*)(long, long, long, long, long, long, long))sym)(
          a[0], a[1], a[2], a[3], a[4], a[5], a[6]);
      break;
    case 8:
      ((void (*)(long, long, long, long, long, long, long, long))sym)(
          a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7]);
      break;
    default: break;
  }
}

static long call_long_double(void *sym, int n, const double *a) {
  switch (n) {
    case 0: return ((long (*)(void))sym)();
    case 1: return ((long (*)(double))sym)(a[0]);
    case 2: return ((long (*)(double, double))sym)(a[0], a[1]);
    case 3: return ((long (*)(double, double, double))sym)(a[0], a[1], a[2]);
    case 4: return ((long (*)(double, double, double, double))sym)(a[0], a[1], a[2], a[3]);
    case 5:
      return ((long (*)(double, double, double, double, double))sym)(
          a[0], a[1], a[2], a[3], a[4]);
    case 6:
      return ((long (*)(double, double, double, double, double, double))sym)(
          a[0], a[1], a[2], a[3], a[4], a[5]);
    case 7:
      return ((long (*)(double, double, double, double, double, double, double))sym)(
          a[0], a[1], a[2], a[3], a[4], a[5], a[6]);
    case 8:
      return ((long (*)(double, double, double, double, double, double, double, double))sym)(
          a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7]);
    default: return 0;
  }
}

static double call_double_double(void *sym, int n, const double *a) {
  switch (n) {
    case 0: return ((double (*)(void))sym)();
    case 1: return ((double (*)(double))sym)(a[0]);
    case 2: return ((double (*)(double, double))sym)(a[0], a[1]);
    case 3: return ((double (*)(double, double, double))sym)(a[0], a[1], a[2]);
    case 4: return ((double (*)(double, double, double, double))sym)(a[0], a[1], a[2], a[3]);
    case 5:
      return ((double (*)(double, double, double, double, double))sym)(
          a[0], a[1], a[2], a[3], a[4]);
    case 6:
      return ((double (*)(double, double, double, double, double, double))sym)(
          a[0], a[1], a[2], a[3], a[4], a[5]);
    case 7:
      return ((double (*)(double, double, double, double, double, double, double))sym)(
          a[0], a[1], a[2], a[3], a[4], a[5], a[6]);
    case 8:
      return ((double (*)(double, double, double, double, double, double, double, double))sym)(
          a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7]);
    default: return 0.0;
  }
}

static const char *call_cstr_double(void *sym, int n, const double *a) {
  switch (n) {
    case 0: return ((const char *(*)(void))sym)();
    case 1: return ((const char *(*)(double))sym)(a[0]);
    case 2: return ((const char *(*)(double, double))sym)(a[0], a[1]);
    case 3: return ((const char *(*)(double, double, double))sym)(a[0], a[1], a[2]);
    case 4:
      return ((const char *(*)(double, double, double, double))sym)(
          a[0], a[1], a[2], a[3]);
    case 5:
      return ((const char *(*)(double, double, double, double, double))sym)(
          a[0], a[1], a[2], a[3], a[4]);
    case 6:
      return ((const char *(*)(double, double, double, double, double, double))sym)(
          a[0], a[1], a[2], a[3], a[4], a[5]);
    case 7:
      return ((const char *(*)(double, double, double, double, double, double, double))sym)(
          a[0], a[1], a[2], a[3], a[4], a[5], a[6]);
    case 8:
      return ((const char *(*)(double, double, double, double, double, double, double, double))sym)(
          a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7]);
    default: return NULL;
  }
}

static void call_void_double(void *sym, int n, const double *a) {
  switch (n) {
    case 0: ((void (*)(void))sym)(); break;
    case 1: ((void (*)(double))sym)(a[0]); break;
    case 2: ((void (*)(double, double))sym)(a[0], a[1]); break;
    case 3: ((void (*)(double, double, double))sym)(a[0], a[1], a[2]); break;
    case 4: ((void (*)(double, double, double, double))sym)(a[0], a[1], a[2], a[3]); break;
    case 5:
      ((void (*)(double, double, double, double, double))sym)(
          a[0], a[1], a[2], a[3], a[4]);
      break;
    case 6:
      ((void (*)(double, double, double, double, double, double))sym)(
          a[0], a[1], a[2], a[3], a[4], a[5]);
      break;
    case 7:
      ((void (*)(double, double, double, double, double, double, double))sym)(
          a[0], a[1], a[2], a[3], a[4], a[5], a[6]);
      break;
    case 8:
      ((void (*)(double, double, double, double, double, double, double, double))sym)(
          a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7]);
      break;
    default: break;
  }
}

static long call_long_string(void *sym, int n, const char *const *a) {
  switch (n) {
    case 0: return ((long (*)(void))sym)();
    case 1: return ((long (*)(const char *))sym)(a[0]);
    case 2: return ((long (*)(const char *, const char *))sym)(a[0], a[1]);
    case 3: return ((long (*)(const char *, const char *, const char *))sym)(a[0], a[1], a[2]);
    case 4:
      return ((long (*)(const char *, const char *, const char *, const char *))sym)(
          a[0], a[1], a[2], a[3]);
    default: return 0;
  }
}

static double call_double_string(void *sym, int n, const char *const *a) {
  switch (n) {
    case 0: return ((double (*)(void))sym)();
    case 1: return ((double (*)(const char *))sym)(a[0]);
    case 2: return ((double (*)(const char *, const char *))sym)(a[0], a[1]);
    case 3:
      return ((double (*)(const char *, const char *, const char *))sym)(
          a[0], a[1], a[2]);
    case 4:
      return ((double (*)(const char *, const char *, const char *, const char *))sym)(
          a[0], a[1], a[2], a[3]);
    default: return 0.0;
  }
}

static const char *call_cstr_string(void *sym, int n, const char *const *a) {
  switch (n) {
    case 0: return ((const char *(*)(void))sym)();
    case 1: return ((const char *(*)(const char *))sym)(a[0]);
    case 2:
      return ((const char *(*)(const char *, const char *))sym)(
          a[0], a[1]);
    case 3:
      return ((const char *(*)(const char *, const char *, const char *))sym)(
          a[0], a[1], a[2]);
    case 4:
      return ((const char *(*)(const char *, const char *, const char *, const char *))sym)(
          a[0], a[1], a[2], a[3]);
    default: return NULL;
  }
}

static void call_void_string(void *sym, int n, const char *const *a) {
  switch (n) {
    case 0: ((void (*)(void))sym)(); break;
    case 1: ((void (*)(const char *))sym)(a[0]); break;
    case 2: ((void (*)(const char *, const char *))sym)(a[0], a[1]); break;
    case 3:
      ((void (*)(const char *, const char *, const char *))sym)(
          a[0], a[1], a[2]);
      break;
    case 4:
      ((void (*)(const char *, const char *, const char *, const char *))sym)(
          a[0], a[1], a[2], a[3]);
      break;
    default: break;
  }
}

int main(int argc, char **argv) {
  const char *libpath;
  const char *symname;
  const char *ret;
  int n;
  int i;
  void *handle;
  void *sym;
  const char **types = NULL;
  const char **values = NULL;

  if (argc < 5) {
    usage();
    return 2;
  }

  libpath = argv[1];
  symname = argv[2];
  ret = argv[3];
  n = atoi(argv[4]);
  if (n < 0 || n > 8) {
    fprintf(stderr, "ffi bridge supports 0..8 arguments\n");
    return 2;
  }
  if (argc != 5 + n * 2) {
    usage();
    return 2;
  }

  types = (const char **)calloc((size_t)n, sizeof(const char *));
  values = (const char **)calloc((size_t)n, sizeof(const char *));
  if ((n > 0) && (!types || !values)) {
    fprintf(stderr, "memory allocation failed\n");
    free(types);
    free(values);
    return 2;
  }

  for (i = 0; i < n; i++) {
    types[i] = argv[5 + i * 2];
    values[i] = argv[6 + i * 2];
  }

  handle = dlopen(libpath, RTLD_LAZY);
  if (!handle) {
    fprintf(stderr, "dlopen failed: %s\n", dlerror());
    free(types);
    free(values);
    return 3;
  }

  dlerror();
  sym = dlsym(handle, symname);
  {
    const char *e = dlerror();
    if (e != NULL) {
      fprintf(stderr, "dlsym failed: %s\n", e);
      dlclose(handle);
      free(types);
      free(values);
      return 3;
    }
  }

  if (all_type(types, n, "int")) {
    long a[8] = {0, 0, 0, 0, 0, 0, 0, 0};
    for (i = 0; i < n; i++) {
      if (!parse_long(values[i], &a[i])) {
        fprintf(stderr, "invalid int argument: %s\n", values[i]);
        dlclose(handle);
        free(types);
        free(values);
        return 4;
      }
    }
    if (strcmp(ret, "int") == 0) {
      emit_result_long(call_long_int(sym, n, a));
    } else if (strcmp(ret, "double") == 0) {
      emit_result_double(call_double_int(sym, n, a));
    } else if (strcmp(ret, "void") == 0) {
      call_void_int(sym, n, a);
      emit_result("VOID");
    } else if (strcmp(ret, "string") == 0) {
      emit_result_cstr(call_cstr_int(sym, n, a));
    } else {
      fprintf(stderr, "unsupported return type: %s\n", ret);
      dlclose(handle);
      free(types);
      free(values);
      return 4;
    }
  } else if (all_type(types, n, "double")) {
    double a[8] = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
    for (i = 0; i < n; i++) {
      if (!parse_double(values[i], &a[i])) {
        fprintf(stderr, "invalid double argument: %s\n", values[i]);
        dlclose(handle);
        free(types);
        free(values);
        return 4;
      }
    }
    if (strcmp(ret, "int") == 0) {
      emit_result_long(call_long_double(sym, n, a));
    } else if (strcmp(ret, "double") == 0) {
      emit_result_double(call_double_double(sym, n, a));
    } else if (strcmp(ret, "void") == 0) {
      call_void_double(sym, n, a);
      emit_result("VOID");
    } else if (strcmp(ret, "string") == 0) {
      emit_result_cstr(call_cstr_double(sym, n, a));
    } else {
      fprintf(stderr, "unsupported return type: %s\n", ret);
      dlclose(handle);
      free(types);
      free(values);
      return 4;
    }
  } else if (all_type(types, n, "string")) {
    const char *a[4] = {NULL, NULL, NULL, NULL};
    if (n > 4) {
      fprintf(stderr, "string signatures support up to 4 arguments\n");
      dlclose(handle);
      free(types);
      free(values);
      return 4;
    }
    for (i = 0; i < n; i++) a[i] = values[i];
    if (strcmp(ret, "int") == 0) {
      emit_result_long(call_long_string(sym, n, a));
    } else if (strcmp(ret, "double") == 0) {
      emit_result_double(call_double_string(sym, n, a));
    } else if (strcmp(ret, "void") == 0) {
      call_void_string(sym, n, a);
      emit_result("VOID");
    } else if (strcmp(ret, "string") == 0) {
      emit_result_cstr(call_cstr_string(sym, n, a));
    } else {
      fprintf(stderr, "unsupported return type: %s\n", ret);
      dlclose(handle);
      free(types);
      free(values);
      return 4;
    }
  } else {
    fprintf(stderr,
            "unsupported signature: ret=%s argc=%d (supports homogeneous int/double up to 8 args, homogeneous string up to 4 args)\n",
            ret, n);
    dlclose(handle);
    free(types);
    free(values);
    return 4;
  }

  dlclose(handle);
  free(types);
  free(values);
  return 0;
}
