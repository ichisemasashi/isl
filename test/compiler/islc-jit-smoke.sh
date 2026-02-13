#!/usr/bin/env sh
set -eu

ROOT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)

TMP_SRC=$(mktemp -t islc_jit_smoke_XXXXXX.lsp)
trap 'rm -f "$TMP_SRC"' EXIT

cat >"$TMP_SRC" <<'EOF'
(defglobal x 10)
(defun fact (n)
  (if (= n 0) 1 (* n (fact (- n 1)))))
(fact x)
EOF

OUT=$("$ROOT_DIR/bin/islc" --profile strict --jit "$TMP_SRC" | tail -n 1)
case "$OUT" in
  *"3628800"*) ;;
  *)
    echo "islc --jit smoke failed: expected 3628800 got $OUT" >&2
    exit 1
    ;;
esac

REPL_OUT=$(printf "(defun inc (x) (+ x 1))\n(inc 9)\n" | "$ROOT_DIR/bin/islc-jit" --profile strict --repl 2>/dev/null | tail -n 2)
case "$REPL_OUT" in
  *"10"*) ;;
  *)
    echo "islc-jit repl smoke failed: expected result 10" >&2
    exit 1
    ;;
esac

echo "islc-jit smoke passed"
