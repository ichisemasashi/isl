# Wiki Restore Runbook

## Preconditions

- Stop write traffic to the wiki if possible.
- Confirm the target backup files exist under the configured backup directory.
- Keep the current database and media directory backed up before running restore.

## Restore Steps

1. Restore the SQL dump:

```sh
psql "$ISL_WIKI_DB_URL" -f /tmp/isl-wiki-backups/wiki-backup-XXXX.sql
```

2. Restore media files if required:

```sh
mkdir -p ./examples/webserver/runtime/docroot/public/wiki-files
tar -xzf /tmp/isl-wiki-backups/wiki-backup-XXXX-media.tar.gz -C ./examples/webserver/runtime/docroot/public/wiki-files
```

3. Restart or reload the CGI/httpd process if needed.

## Validation Commands

```sh
psql "$ISL_WIKI_DB_URL" -c '\dt'
psql "$ISL_WIKI_DB_URL" -c 'select count(*) as pages from pages;'
psql "$ISL_WIKI_DB_URL" -c 'select count(*) as media_assets from media_assets;'
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
