#!/bin/sh
set -eu

ROOT_DIR="/Volumes/SSD-PLU3/work/LISP/islisp/isl"

export ISL_WIKI_MEDIA_DIR="/tmp/isl-wiki-media"
export ISL_WIKI_MEDIA_BASE_URL="/cgi-bin/wiki.cgi/files"
mkdir -p "$ISL_WIKI_MEDIA_DIR"
cd "$ROOT_DIR"
exec "$ROOT_DIR/bin/isl" "$ROOT_DIR/examples/wiki/app/wiki.lsp"
