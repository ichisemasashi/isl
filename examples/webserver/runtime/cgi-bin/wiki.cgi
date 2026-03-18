#!/bin/sh
set -eu

ROOT_DIR="/Volumes/SD_ONE/work/dev/isl"
export ISL_ROOT="$ROOT_DIR"
export PATH="/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin:${PATH:-}"

cd "$ROOT_DIR"
exec "$ROOT_DIR/bin/isl" "$ROOT_DIR/examples/wiki/app/wiki.lsp"
