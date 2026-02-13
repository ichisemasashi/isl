#!/usr/bin/env sh
set -eu

ROOT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)

if ! command -v clang >/dev/null 2>&1; then
  echo "m10 opt parity skipped: clang not found"
  exit 0
fi

TMP_DIR=$(mktemp -d -t isl_m10_parity_XXXXXX)
trap 'rm -rf "$TMP_DIR"' EXIT

compile_and_run() {
  src="$1"
  opt="$2"
  out="$3"
  bin="$TMP_DIR/prog_${opt}"
  "$ROOT_DIR/bin/islc-aot" --profile strict --opt-level "$opt" -o "$bin" "$src" >/dev/null
  "$bin" | tail -n 1 >"$out"
}

run_case() {
  id="$1"
  body="$2"
  expected="$3"

  src="$TMP_DIR/${id}.lsp"
  out0="$TMP_DIR/${id}.o0.txt"
  out2="$TMP_DIR/${id}.o2.txt"
  printf "%s\n" "$body" >"$src"

  compile_and_run "$src" 0 "$out0"
  compile_and_run "$src" 2 "$out2"

  v0=$(cat "$out0")
  v2=$(cat "$out2")

  if [ "$v0" != "$v2" ]; then
    echo "m10 parity failed: $id O0/O2 mismatch: $v0 vs $v2" >&2
    exit 1
  fi

  if [ "$expected" = "__ERROR__" ]; then
    case "$v0" in
      \#\<error*) ;;
      *)
        echo "m10 parity failed: $id expected error, got $v0" >&2
        exit 1
        ;;
    esac
  else
    if [ "$v0" != "$expected" ]; then
      echo "m10 parity failed: $id expected $expected got $v0" >&2
      exit 1
    fi
  fi
}

run_case "m10_1" \
"(defun m10-sum (n acc) (if (= n 0) acc (m10-sum (- n 1) (+ acc n))))
(print (m10-sum 100 0))" \
"5050"

run_case "m10_3" \
"(defun m10-g (x) (if x 1 (car 1)))
(print (m10-g t))" \
"1"

run_case "m10_4" \
"(defun m10-h (x) (* (+ x 1) (- x 1)))
(print (m10-h 9))" \
"80"

run_case "m10_5" \
"(defun m10-e () (car 1))
(print (m10-e))" \
"__ERROR__"

echo "m10 opt parity smoke passed"
