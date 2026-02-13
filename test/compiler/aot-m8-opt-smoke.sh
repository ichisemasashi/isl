#!/usr/bin/env sh
set -eu

ROOT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)

if ! command -v clang >/dev/null 2>&1; then
  echo "aot M8 opt smoke skipped: clang not found"
  exit 0
fi

TMP_SRC=$(mktemp -t isl_m8_opt_XXXXXX.lsp)
TMP_O0=$(mktemp -t isl_m8_o0_XXXXXX)
TMP_O2=$(mktemp -t isl_m8_o2_XXXXXX)
trap 'rm -f "$TMP_SRC" "$TMP_O0" "$TMP_O2"' EXIT

cat >"$TMP_SRC" <<'EOF'
(print (+ 9007199254740991 1))
EOF

"$ROOT_DIR/bin/islc-aot" --profile strict --opt-level 0 -o "$TMP_O0" "$TMP_SRC"
"$ROOT_DIR/bin/islc-aot" --profile strict --opt-level 2 -o "$TMP_O2" "$TMP_SRC"

OUT_O0=$("$TMP_O0" | tail -n 1)
OUT_O2=$("$TMP_O2" | tail -n 1)

if [ "$OUT_O0" != "$OUT_O2" ]; then
  echo "aot M8 opt smoke failed: O0/O2 mismatch ($OUT_O0 vs $OUT_O2)" >&2
  exit 1
fi

if [ "$OUT_O0" != "9007199254740992" ]; then
  echo "aot M8 opt smoke failed: unexpected output $OUT_O0" >&2
  exit 1
fi

echo "aot M8 opt smoke passed"
