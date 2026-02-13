#!/usr/bin/env sh
set -eu

ROOT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)

if ! command -v clang >/dev/null 2>&1; then
  echo "islc m3 smoke skipped: clang not found"
  exit 0
fi

TMP_LL=$(mktemp -t islc_m3_XXXXXX.ll)
TMP_O=$(mktemp -t islc_m3_XXXXXX.o)
TMP_BIN=$(mktemp -t islc_m3_XXXXXX.bin)
trap 'rm -f "$TMP_LL" "$TMP_O" "$TMP_BIN"' EXIT

"$ROOT_DIR/bin/islc" --profile strict --emit-llvm "$TMP_LL" "$ROOT_DIR/examples/hello.lsp"
test -s "$TMP_LL"

"$ROOT_DIR/bin/islc" --profile strict --emit-obj "$TMP_O" "$ROOT_DIR/examples/hello.lsp"
test -s "$TMP_O"

OUT=$("$ROOT_DIR/bin/islc" --profile strict --run -o "$TMP_BIN" "$ROOT_DIR/examples/hello.lsp" | tail -n 1)
if [ "$OUT" != "3628800" ]; then
  echo "islc run failed: expected 3628800 got $OUT" >&2
  exit 1
fi

echo "islc m3 smoke passed"
