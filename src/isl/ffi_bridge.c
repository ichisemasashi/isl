#include <dlfcn.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef long (*fn_i0_t)(void);
typedef long (*fn_i1_t)(long);
typedef long (*fn_i2_t)(long, long);
typedef long (*fn_i3_t)(long, long, long);
typedef long (*fn_i4_t)(long, long, long, long);

typedef double (*fn_d0_t)(void);
typedef double (*fn_d1_t)(double);
typedef double (*fn_d2_t)(double, double);
typedef double (*fn_d3_t)(double, double, double);
typedef double (*fn_d4_t)(double, double, double, double);

typedef void (*fn_v0_t)(void);
typedef void (*fn_v1_t)(long);
typedef void (*fn_v2_t)(long, long);
typedef void (*fn_v3_t)(long, long, long);
typedef void (*fn_v4_t)(long, long, long, long);

typedef long (*fn_is_t)(const char *);
typedef double (*fn_ds_t)(const char *);
typedef void (*fn_vs_t)(const char *);

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
  if (n < 0 || n > 4) {
    fprintf(stderr, "ffi bridge supports 0..4 arguments\n");
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

  if (strcmp(ret, "int") == 0 && all_type(types, n, "int")) {
    long a[4] = {0, 0, 0, 0};
    long r = 0;
    for (i = 0; i < n; i++) {
      if (!parse_long(values[i], &a[i])) {
        fprintf(stderr, "invalid int argument: %s\n", values[i]);
        dlclose(handle);
        free(types);
        free(values);
        return 4;
      }
    }
    switch (n) {
      case 0: r = ((fn_i0_t)sym)(); break;
      case 1: r = ((fn_i1_t)sym)(a[0]); break;
      case 2: r = ((fn_i2_t)sym)(a[0], a[1]); break;
      case 3: r = ((fn_i3_t)sym)(a[0], a[1], a[2]); break;
      case 4: r = ((fn_i4_t)sym)(a[0], a[1], a[2], a[3]); break;
      default: break;
    }
    {
      char buf[64];
      snprintf(buf, sizeof(buf), "%ld", r);
      emit_result(buf);
    }
  } else if (strcmp(ret, "double") == 0 && all_type(types, n, "double")) {
    double a[4] = {0.0, 0.0, 0.0, 0.0};
    double r = 0.0;
    for (i = 0; i < n; i++) {
      if (!parse_double(values[i], &a[i])) {
        fprintf(stderr, "invalid double argument: %s\n", values[i]);
        dlclose(handle);
        free(types);
        free(values);
        return 4;
      }
    }
    switch (n) {
      case 0: r = ((fn_d0_t)sym)(); break;
      case 1: r = ((fn_d1_t)sym)(a[0]); break;
      case 2: r = ((fn_d2_t)sym)(a[0], a[1]); break;
      case 3: r = ((fn_d3_t)sym)(a[0], a[1], a[2]); break;
      case 4: r = ((fn_d4_t)sym)(a[0], a[1], a[2], a[3]); break;
      default: break;
    }
    {
      char buf[128];
      snprintf(buf, sizeof(buf), "%.17g", r);
      emit_result(buf);
    }
  } else if (n == 1 && strcmp(types[0], "string") == 0) {
    const char *s = values[0];
    if (strcmp(ret, "int") == 0) {
      long r = ((fn_is_t)sym)(s);
      {
        char buf[64];
        snprintf(buf, sizeof(buf), "%ld", r);
        emit_result(buf);
      }
    } else if (strcmp(ret, "double") == 0) {
      double r = ((fn_ds_t)sym)(s);
      {
        char buf[128];
        snprintf(buf, sizeof(buf), "%.17g", r);
        emit_result(buf);
      }
    } else if (strcmp(ret, "void") == 0) {
      ((fn_vs_t)sym)(s);
      emit_result("VOID");
    } else {
      fprintf(stderr, "unsupported return type for string arg: %s\n", ret);
      dlclose(handle);
      free(types);
      free(values);
      return 4;
    }
  } else if (strcmp(ret, "void") == 0 && all_type(types, n, "int")) {
    long a[4] = {0, 0, 0, 0};
    for (i = 0; i < n; i++) {
      if (!parse_long(values[i], &a[i])) {
        fprintf(stderr, "invalid int argument: %s\n", values[i]);
        dlclose(handle);
        free(types);
        free(values);
        return 4;
      }
    }
    switch (n) {
      case 0: ((fn_v0_t)sym)(); break;
      case 1: ((fn_v1_t)sym)(a[0]); break;
      case 2: ((fn_v2_t)sym)(a[0], a[1]); break;
      case 3: ((fn_v3_t)sym)(a[0], a[1], a[2]); break;
      case 4: ((fn_v4_t)sym)(a[0], a[1], a[2], a[3]); break;
      default: break;
    }
    emit_result("VOID");
  } else {
    fprintf(stderr,
            "unsupported signature: ret=%s argc=%d (supports int/double homogeneous args, or single string arg)\n",
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
