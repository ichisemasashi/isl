#!/usr/bin/env sh
set -eu

ROOT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)

if ! command -v clang >/dev/null 2>&1; then
  echo "m10 stage regression skipped: clang not found"
  exit 0
fi

TMP_DIR=$(mktemp -d -t isl_m10_stage_XXXXXX)
trap 'rm -rf "$TMP_DIR"' EXIT

SRC="$TMP_DIR/case.lsp"
cat >"$SRC" <<'EOF'
(defun m10-stage (n acc)
  (if (= n 0) acc (m10-stage (- n 1) (+ acc n))))
(print (m10-stage 200 0))
EOF

run_preset() {
  preset="$1"
  bin="$TMP_DIR/${preset}.bin"
  out="$TMP_DIR/${preset}.txt"
  "$ROOT_DIR/bin/islc-aot" --profile strict --opt-preset "$preset" -o "$bin" "$SRC" >/dev/null
  "$bin" | tail -n 1 >"$out"
}

run_preset none
run_preset safe
run_preset aggressive

base=$(cat "$TMP_DIR/none.txt")
for p in safe aggressive; do
  cur=$(cat "$TMP_DIR/${p}.txt")
  if [ "$base" != "$cur" ]; then
    echo "m10 stage regression failed: none vs $p mismatch ($base vs $cur)" >&2
    exit 1
  fi
done

echo "m10 stage regression passed"
