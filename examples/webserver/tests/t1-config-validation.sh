#!/bin/sh
set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname "$0")" && pwd)
WS=$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)
BIN="$WS/webserver"
CFG="$WS/conf/webserver.conf.example"

run_ok() {
  "$BIN" --config "$1" --check-config >/dev/null
}

run_ng() {
  set +e
  "$BIN" --config "$1" --check-config >/dev/null 2>&1
  st=$?
  set -e
  if [ "$st" -eq 0 ]; then
    echo "expected failure but succeeded: $1" 1>&2
    exit 1
  fi
}

tmpdir=$(mktemp -d /tmp/webserver-t1.XXXXXX)
trap 'rm -rf "$tmpdir"' EXIT INT TERM

# 1) valid config should pass
run_ok "$CFG"

# 2) listen_port != 8080 should fail
bad_port="$tmpdir/bad-port.conf"
sed 's/^listen_port=.*/listen_port=9090/' "$CFG" > "$bad_port"
run_ng "$bad_port"

# 3) missing required key should fail
missing_key="$tmpdir/missing-key.conf"
grep -v '^cgi_bin_dir=' "$CFG" > "$missing_key"
run_ng "$missing_key"

echo "t1-config-validation: ok"
