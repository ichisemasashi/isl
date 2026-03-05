#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"

ISL_BIN="$REPO_ROOT/bin/isl"
ISLC_RUN_BIN="$REPO_ROOT/bin/islc-run"

if [[ ! -x "$ISL_BIN" ]]; then
  echo "ERROR: missing executable: $ISL_BIN" >&2
  exit 2
fi
if [[ ! -x "$ISLC_RUN_BIN" ]]; then
  echo "ERROR: missing executable: $ISLC_RUN_BIN" >&2
  exit 2
fi

TMP_DIR="$(mktemp -d /tmp/dbms-regression.XXXXXX)"
trap 'rm -rf "$TMP_DIR"' EXIT

discover_tests() {
  (
    cd "$REPO_ROOT"
    find examples/dbms/tests -type f -name 't*.lsp' ! -name '._*' | sort
  )
}

extract_assert_label() {
  local log_file="$1"
  sed -n "s/.*assert failed: //p" "$log_file" | tail -n 1
}

extract_error_code() {
  local out_file="$1"
  local err_file="$2"
  local code
  code="$( (cat "$out_file"; cat "$err_file") | grep -Eo "dbms/[a-z0-9-]+" | tail -n 1 || true )"
  if [[ -n "$code" ]]; then
    echo "$code"
  else
    echo "N/A"
  fi
}

extract_actual() {
  local out_file="$1"
  local err_file="$2"
  local actual
  actual="$( (cat "$err_file"; cat "$out_file") | awk 'NF{line=$0} END{print line}' )"
  if [[ -n "$actual" ]]; then
    echo "$actual"
  else
    echo "N/A"
  fi
}

extract_sql_near_label() {
  local test_file="$1"
  local label="$2"
  local sql_line
  if [[ -z "$label" ]]; then
    echo "N/A"
    return
  fi
  sql_line="$(
    awk -v label="$label" '
      {
        lines[NR] = $0
        if (index($0, "\"" label "\"") > 0) {
          hit = NR
        }
      }
      END {
        if (!hit) {
          print "N/A"
          exit
        }
        start = hit - 25
        if (start < 1) start = 1
        for (i = hit; i >= start; i--) {
          if (index(lines[i], "dbms-exec-sql") > 0) {
            line = lines[i]
            while (i < hit && index(line, ");") == 0) {
              i++
              line = line " " lines[i]
            }
            if (match(line, /"[^"]*"/)) {
              sql = substr(line, RSTART + 1, RLENGTH - 2)
              print sql
              exit
            }
          }
        }
        print "N/A"
      }
    ' "$test_file"
  )"
  echo "$sql_line"
}

print_failure_report() {
  local runner="$1"
  local test_file="$2"
  local out_file="$3"
  local err_file="$4"
  local label sql expected actual error_code

  label="$(extract_assert_label "$err_file")"
  sql="$(extract_sql_near_label "$test_file" "$label")"
  expected="${label:-N/A}"
  actual="$(extract_actual "$out_file" "$err_file")"
  error_code="$(extract_error_code "$out_file" "$err_file")"

  echo
  echo "FAILED: $runner $test_file"
  echo "  SQL       : $sql"
  echo "  期待      : $expected"
  echo "  実際      : $actual"
  echo "  エラーコード: $error_code"
  echo "  --- stderr (tail) ---"
  tail -n 40 "$err_file" || true
  echo "  --- stdout (tail) ---"
  tail -n 40 "$out_file" || true
}

run_one() {
  local runner_name="$1"
  local bin="$2"
  local test_file="$3"
  local idx="$4"
  local total="$5"
  local out_file="$TMP_DIR/${runner_name}.${idx}.out"
  local err_file="$TMP_DIR/${runner_name}.${idx}.err"

  printf '[%s] (%d/%d) %s\n' "$runner_name" "$idx" "$total" "$test_file"
  if (cd "$REPO_ROOT" && DBMS_TEST_RUNNER_BIN="$bin" "$bin" "$test_file" >"$out_file" 2>"$err_file"); then
    local summary
    summary="$(tail -n 1 "$out_file" || true)"
    if [[ -n "$summary" ]]; then
      echo "  -> $summary"
    else
      echo "  -> OK"
    fi
    return 0
  fi

  print_failure_report "$runner_name" "$test_file" "$out_file" "$err_file"
  return 1
}

main() {
  TEST_FILES=()
  while IFS= read -r line; do
    TEST_FILES+=("$line")
  done < <(discover_tests)
  if [[ ${#TEST_FILES[@]} -eq 0 ]]; then
    echo "ERROR: no test files found under examples/dbms/tests" >&2
    exit 3
  fi

  echo "DBMS regression baseline"
  echo "  repo   : $REPO_ROOT"
  echo "  tests  : ${#TEST_FILES[@]}"
  echo "  isl    : $ISL_BIN"
  echo "  islc   : $ISLC_RUN_BIN"
  echo

  local total="${#TEST_FILES[@]}"
  local i=0
  local passed_isl=0
  local passed_islc=0

  for test_file in "${TEST_FILES[@]}"; do
    i=$((i + 1))
    run_one "isl" "$ISL_BIN" "$test_file" "$i" "$total"
    passed_isl=$((passed_isl + 1))
  done

  i=0
  for test_file in "${TEST_FILES[@]}"; do
    i=$((i + 1))
    run_one "islc-run" "$ISLC_RUN_BIN" "$test_file" "$i" "$total"
    passed_islc=$((passed_islc + 1))
  done

  echo
  echo "PASS SUMMARY"
  echo "  isl      : $passed_isl / $total"
  echo "  islc-run : $passed_islc / $total"
  echo "  result   : ALL_OK"
}

main "$@"
