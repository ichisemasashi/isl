#!/bin/sh
set -eu

ROOT_DIR="/Volumes/SD_ONE/work/dev/isl"

export ISL_WIKI_MEDIA_DIR="$ROOT_DIR/examples/webserver/runtime/docroot/public/wiki-files"
export ISL_WIKI_MEDIA_BASE_URL="/public/wiki-files"
export ISL_WIKI_MD2HTML="$ROOT_DIR/examples/md2html/md2html"
mkdir -p "$ISL_WIKI_MEDIA_DIR"
cd "$ROOT_DIR"
exec "$ROOT_DIR/bin/isl" "$ROOT_DIR/examples/wiki/app/wiki.lsp"
