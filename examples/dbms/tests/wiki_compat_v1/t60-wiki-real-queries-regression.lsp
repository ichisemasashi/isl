;; dbms/tests/wiki_compat_v1/t60-wiki-real-queries-regression.lsp
;; Reproduce major SQL patterns from examples/wiki:
;; index/search/edit/new/media/migration.

(load "./examples/dbms/app/repr.lsp")
(load "./examples/dbms/app/sql_parser.lsp")
(load "./examples/dbms/app/engine.lsp")

(defun assert-true (label cond)
  (if cond
      t
      (error (string-append "assert failed: " label))))

(defun assert-parse-ok (label sql)
  (let ((r (dbms-parse-sql sql)))
    (assert-true label (dbms-ast-p r))))

(defun assert-exec-ok (catalog label sql)
  (let ((r (dbms-exec-sql catalog sql)))
    (assert-true label (and (dbms-result-p r)
                            (or (eq (second r) 'ok)
                                (eq (second r) 'count)
                                (eq (second r) 'rows))))))

(defun run-tests ()
  (let* ((root (string-append "/tmp/dbms-wiki-real-q-" (format nil "~A" (get-universal-time))))
         (catalog (dbms-engine-init)))
    (setenv "DBMS_STORAGE_ROOT" root)
    (setq catalog (dbms-engine-init))

    ;; list/index
    (assert-parse-ok
     "index query parse"
     "select slug, title, to_char(updated_at, 'YYYY-MM-DD HH24:MI:SS') from pages order by slug;")

    ;; search
    (assert-parse-ok
     "search query parse"
     "select slug, title, to_char(updated_at, 'YYYY-MM-DD HH24:MI:SS') from pages where position(lower('home') in lower(title)) > 0 or position(lower('home') in lower(body_md)) > 0 order by updated_at desc, slug;")

    ;; view/details
    (assert-parse-ok
     "fetch page parse"
     "select slug, title, body_md from pages where slug='home' limit 1;")
    (assert-parse-ok
     "latest summary join parse"
     "select coalesce(edit_summary, '') from page_revisions r join pages p on p.id = r.page_id where p.slug='home' order by r.rev_no desc limit 1;")

    ;; media listing and lookup
    (assert-parse-ok
     "media list parse"
     "select m.id, m.media_type, coalesce(m.title, ''), m.stored_filename, m.mime_type, coalesce((select p.slug from pages p where p.id = m.page_id), '') from media_assets m order by m.id desc;")
    (assert-parse-ok
     "media by page parse"
     "select m.media_type, coalesce(m.title, ''), m.stored_filename, m.mime_type from media_assets m join pages p on p.id = m.page_id where p.slug='home' order by m.id desc;")
    (assert-parse-ok
     "media file meta parse"
     "select storage_path, mime_type from media_assets where stored_filename='stored.bin' limit 1;")

    ;; edit flow CTE
    (assert-parse-ok
     "edit save cte parse"
     "with target as ( select p.id as page_id, coalesce((select max(r.rev_no) from page_revisions r where r.page_id = p.id), 0) + 1 as next_rev from pages p where p.slug = 'home' ), updated as ( update pages p set title = 'New title', body_md = 'New body', last_edited_by = 'web' from target t where p.id = t.page_id returning p.id, t.next_rev ) insert into page_revisions (page_id, rev_no, title, body_md, edited_by, edit_summary) select u.id, u.next_rev, 'New title', 'New body', 'web', 'summary' from updated u;")

    ;; new page flow CTE
    (assert-parse-ok
     "new page cte parse"
     "with inserted as ( insert into pages (slug, title, body_md, last_edited_by) values ('new-page', 'New page', 'Body', 'web') returning id, title, body_md ) insert into page_revisions (page_id, rev_no, title, body_md, edited_by, edit_summary) select i.id, 1, i.title, i.body_md, 'web', 'create page' from inserted i;")

    ;; media add path SQL
    (assert-parse-ok
     "media insert parse"
     "insert into media_assets (page_id, media_type, title, original_filename, stored_filename, mime_type, storage_path, public_url, created_by) values (1, 'image', 'hero', 'hero.png', '123-hero.png', 'image/png', '/tmp/hero.png', '/public/wiki-files/123-hero.png', 'web');")
    (assert-parse-ok
     "media-linked revision insert parse"
     "insert into page_revisions (page_id, rev_no, title, body_md, edited_by, edit_summary) select p.id, coalesce((select max(r.rev_no) from page_revisions r where r.page_id=p.id),0)+1, p.title, p.body_md, 'web', 'add media' from pages p where p.slug='home';")

    ;; migration-relevant executable subset (dbms-compatible)
    (assert-exec-ok catalog "BEGIN exec" "BEGIN;")
    (assert-exec-ok
     catalog
     "create pages exec"
     "CREATE TABLE IF NOT EXISTS pages (id BIGSERIAL PRIMARY KEY, slug TEXT NOT NULL, title TEXT NOT NULL, body_md TEXT NOT NULL, created_at TIMESTAMPTZ DEFAULT now(), updated_at TIMESTAMPTZ DEFAULT now(), last_edited_by TEXT);")
    (assert-exec-ok
     catalog
     "create page_revisions exec"
     "CREATE TABLE IF NOT EXISTS page_revisions (id BIGSERIAL PRIMARY KEY, page_id BIGINT NOT NULL, rev_no INTEGER NOT NULL, title TEXT NOT NULL, body_md TEXT NOT NULL, edited_by TEXT, edit_summary TEXT, created_at TIMESTAMPTZ DEFAULT now());")
    (assert-exec-ok
     catalog
     "create media_assets exec"
     "CREATE TABLE IF NOT EXISTS media_assets (id BIGSERIAL PRIMARY KEY, page_id BIGINT, media_type TEXT NOT NULL, title TEXT, original_filename TEXT NOT NULL, stored_filename TEXT NOT NULL, mime_type TEXT NOT NULL, storage_path TEXT NOT NULL, public_url TEXT NOT NULL, created_by TEXT, created_at TIMESTAMPTZ DEFAULT now());")
    (assert-exec-ok
     catalog
     "create media index #1 exec"
     "CREATE INDEX IF NOT EXISTS idx_media_assets_page_id_created ON media_assets(page_id, created_at);")
    (assert-exec-ok
     catalog
     "create media index #2 exec"
     "CREATE INDEX IF NOT EXISTS idx_media_assets_type_created ON media_assets(media_type, created_at);")
    (assert-exec-ok
     catalog
     "alter drop constraint exec"
     "ALTER TABLE media_assets DROP CONSTRAINT IF EXISTS media_assets_type_chk;")
    (assert-exec-ok
     catalog
     "alter add check exec"
     "ALTER TABLE media_assets ADD CONSTRAINT media_assets_type_chk CHECK (media_type IN ('image', 'video', 'audio', 'file'));")
    (assert-exec-ok catalog "COMMIT exec" "COMMIT;")

    (format t "dbms wiki real query regression tests passed.~%")))

(run-tests)
