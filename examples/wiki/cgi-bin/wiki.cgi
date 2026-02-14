#!/bin/sh
set -eu

ROOT_DIR="/Volumes/SSD-PLU3/work/LISP/islisp/isl"
exec "$ROOT_DIR/bin/isl" "$ROOT_DIR/examples/wiki/app/wiki.lsp"
