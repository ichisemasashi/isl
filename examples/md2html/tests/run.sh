#!/bin/sh
set -eu

ROOT_DIR="/Volumes/SSD-PLU3/work/LISP/islisp/isl"
MD2HTML="$ROOT_DIR/examples/md2html/md2html"
CASES_DIR="$ROOT_DIR/examples/md2html/tests/cases"
EXPECTED_DIR="$ROOT_DIR/examples/md2html/tests/expected"

failed=0

for case_path in "$CASES_DIR"/*.md; do
  name=$(basename "$case_path" .md)
  expected="$EXPECTED_DIR/$name.html"
  env_file="$CASES_DIR/$name.env"

  if [ ! -f "$expected" ]; then
    echo "[ERROR] expected file missing: $expected"
    failed=1
    continue
  fi

  actual=$(mktemp)
  if [ -f "$env_file" ]; then
    (
      set -a
      # shellcheck disable=SC1090
      . "$env_file"
      set +a
      "$MD2HTML" "$case_path" > "$actual"
    )
  else
    "$MD2HTML" "$case_path" > "$actual"
  fi

  if diff -u "$expected" "$actual" > /tmp/md2html_diff_$$.txt; then
    echo "[PASS] $name"
  else
    echo "[FAIL] $name"
    cat /tmp/md2html_diff_$$.txt
    failed=1
  fi

  rm -f "$actual" /tmp/md2html_diff_$$.txt
done

if [ "$failed" -ne 0 ]; then
  exit 1
fi

echo "All md2html fixed tests passed."
