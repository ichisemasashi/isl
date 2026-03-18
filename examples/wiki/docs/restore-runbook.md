# Wiki Restore Runbook

## Preconditions

- Stop write traffic to the wiki if possible.
- Confirm the target backup files exist under the configured backup directory.
- Keep the current database and media directory backed up before running restore.

## Restore Steps

1. Restore the SQL dump:

```sh
ISL_WIKI_DB_ROOT=./examples/dbms/storage/wiki ./bin/isl examples/wiki/scripts/run-backup.lsp
```

2. Restore media files if required:

```sh
mkdir -p ./examples/webserver/runtime/docroot/public/wiki-files
tar -xzf /tmp/isl-wiki-backups/wiki-backup-XXXX-media.tar.gz -C ./examples/webserver/runtime/docroot/public/wiki-files
```

3. Restart or reload the CGI/httpd process if needed.

## Validation Commands

```sh
ls -lh "$ISL_WIKI_DB_ROOT"
cat "$ISL_WIKI_DB_ROOT/catalog.lspdata"
ls "$ISL_WIKI_DB_ROOT"/table_*.lspdata
ls -lh /tmp/isl-wiki-backups
curl -i http://localhost:8080/wiki/healthz
curl -i http://localhost:8080/wiki/home
```

## App-Level Checks

- Open `/wiki/healthz` and confirm `status: ok`.
- Open a known page and attached media.
- Review `/wiki/admin/audit` and `/wiki/admin/backup` history.

## Rollback Note

- If the restore target is incorrect, restore the previous SQL/media backup pair immediately.
