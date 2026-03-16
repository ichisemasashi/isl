#!/bin/sh
set -eu

DB_URL="${ISL_WIKI_DB_URL:-postgresql://127.0.0.1:5432/isl_wiki}"
MEDIA_DIR="${ISL_WIKI_MEDIA_DIR:-./examples/webserver/runtime/docroot/public/wiki-files}"
BACKUP_DIR="${ISL_WIKI_BACKUP_DIR:-/tmp/isl-wiki-backups}"
KEEP_COUNT="${ISL_WIKI_BACKUP_KEEP_COUNT:-7}"
DRY_RUN="${ISL_WIKI_DRY_RUN:-0}"

mkdir -p "$BACKUP_DIR"

ts="$(date -u +%Y%m%dT%H%M%SZ)"
base="$BACKUP_DIR/wiki-backup-$ts"
sql_path="$base.sql"
media_path="$base-media.tar.gz"

echo "backup_dir=$BACKUP_DIR"
echo "keep_count=$KEEP_COUNT"
echo "sql_path=$sql_path"
echo "media_path=$media_path"

if [ "$DRY_RUN" = "1" ]; then
  echo "dry_run=true"
  exit 0
fi

pg_dump --clean --if-exists "$DB_URL" > "$sql_path"
tar -czf "$media_path" -C "$MEDIA_DIR" .

ls -1t "$BACKUP_DIR"/wiki-backup-* 2>/dev/null | awk "NR>$KEEP_COUNT {print}" | while IFS= read -r old; do
  [ -n "$old" ] || continue
  rm -f "$old"
done

echo "backup_complete=true"
