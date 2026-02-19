#!/bin/sh
set -eu

ROOT_DIR="/Volumes/SD_ONE/work/dev/isl"

cd "$ROOT_DIR"
exec "$ROOT_DIR/bin/isl" "$ROOT_DIR/examples/wiki/app/wiki.lsp"
