#!/usr/bin/env sh
set -eu

ROOT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)

if ! command -v clang >/dev/null 2>&1; then
  echo "m10 opt bench skipped: clang not found"
  exit 0
fi

TMP_DIR=$(mktemp -d -t isl_m10_bench_XXXXXX)
trap 'rm -rf "$TMP_DIR"' EXIT

SRC="$TMP_DIR/bench.lsp"
cat >"$SRC" <<'EOF'
(defun m10-fib (n)
  (if (< n 2) n (+ (m10-fib (- n 1)) (m10-fib (- n 2)))))
(print (m10-fib 30))
EOF

BIN_O0="$TMP_DIR/bench_o0"
BIN_O2="$TMP_DIR/bench_o2"

"$ROOT_DIR/bin/islc-aot" --profile strict --opt-preset none -o "$BIN_O0" "$SRC" >/dev/null
"$ROOT_DIR/bin/islc-aot" --profile strict --opt-preset safe -o "$BIN_O2" "$SRC" >/dev/null

python3 - "$BIN_O0" "$BIN_O2" <<'PY'
import statistics
import subprocess
import time
import sys

o0, o2 = sys.argv[1], sys.argv[2]

def measure(path, n=5):
    vals = []
    for _ in range(n):
        t0 = time.perf_counter()
        out = subprocess.check_output([path], text=True).strip().splitlines()[-1]
        t1 = time.perf_counter()
        if out != "832040":
            raise SystemExit(f"unexpected output from {path}: {out}")
        vals.append(t1 - t0)
    return vals

v0 = measure(o0)
v2 = measure(o2)
m0 = statistics.median(v0)
m2 = statistics.median(v2)
speedup = (m0 / m2) if m2 > 0 else float("inf")

print(f"m10 bench O0 median: {m0:.6f}s")
print(f"m10 bench O2 median: {m2:.6f}s")
print(f"m10 bench speedup (O0/O2): {speedup:.2f}x")
PY
