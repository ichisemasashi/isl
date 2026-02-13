#!/usr/bin/env sh
set -eu

ROOT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)

if ! command -v clang >/dev/null 2>&1; then
  echo "aot run smoke skipped: clang not found"
  exit 0
fi

OUT_BIN=$(mktemp -t isl_aot_bin_XXXXXX)
trap 'rm -f "$OUT_BIN"' EXIT

"$ROOT_DIR/bin/islc-aot" -o "$OUT_BIN" "$ROOT_DIR/examples/hello.lsp"
OUT=$($OUT_BIN | tail -n 1)

if [ "$OUT" != "3628800" ]; then
  echo "aot run smoke failed: expected 3628800 got $OUT" >&2
  exit 1
fi

echo "aot run smoke passed"
