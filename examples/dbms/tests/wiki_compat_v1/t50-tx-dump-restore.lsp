;; dbms/tests/wiki_compat_v1/t50-tx-dump-restore.lsp
;; BEGIN/COMMIT and admin dump/restore compatibility API

(load "./examples/dbms/app/repr.lsp")
(load "./examples/dbms/app/engine.lsp")

(defun assert-true (label cond)
  (if cond
      t
      (error (string-append "assert failed: " label))))

(defun run-tests ()
  (let* ((root-a (string-append "/tmp/dbms-tx-a-" (format nil "~A" (get-universal-time))))
         (root-b (string-append "/tmp/dbms-tx-b-" (format nil "~A" (get-universal-time))))
         (dump-path (string-append "/tmp/dbms-wiki-backup-" (format nil "~A" (get-universal-time)) ".lspdump"))
         (catalog (dbms-engine-init))
         (r '())
         (rows '())
         (wk '()))
    (setenv "DBMS_STORAGE_ROOT" root-a)
    (setq catalog (dbms-engine-init))

    ;; BEGIN/COMMIT executable
    (setq r (dbms-exec-sql catalog "BEGIN;"))
    (assert-true "BEGIN ok" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "ROLLBACK;"))
    (assert-true "ROLLBACK ok" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "BEGIN;"))
    (assert-true "BEGIN #2 ok" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "COMMIT;"))
    (assert-true "COMMIT ok" (and (dbms-result-p r) (eq (second r) 'ok)))

    ;; wiki target tables
    (setq r (dbms-exec-sql catalog
                           "CREATE TABLE pages (id BIGSERIAL PRIMARY KEY, slug TEXT NOT NULL, title TEXT NOT NULL, updated_at TIMESTAMPTZ DEFAULT now());"))
    (assert-true "create pages" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog
                           "CREATE TABLE page_revisions (id BIGSERIAL PRIMARY KEY, page_id BIGINT NOT NULL, body_md TEXT, updated_at TIMESTAMPTZ DEFAULT now());"))
    (assert-true "create page_revisions" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog
                           "CREATE TABLE media_assets (id BIGSERIAL PRIMARY KEY, page_id BIGINT, filename TEXT NOT NULL, updated_at TIMESTAMPTZ DEFAULT now());"))
    (assert-true "create media_assets" (and (dbms-result-p r) (eq (second r) 'ok)))

    (setq r (dbms-exec-sql catalog "INSERT INTO pages (slug, title) VALUES ('home', 'Home');"))
    (assert-true "insert pages" (and (dbms-result-p r) (eq (second r) 'count)))
    (setq r (dbms-exec-sql catalog "INSERT INTO page_revisions (page_id, body_md) VALUES (1, '# Home');"))
    (assert-true "insert revisions" (and (dbms-result-p r) (eq (second r) 'count)))
    (setq r (dbms-exec-sql catalog "INSERT INTO media_assets (page_id, filename) VALUES (1, 'logo.png');"))
    (assert-true "insert media" (and (dbms-result-p r) (eq (second r) 'count)))

    ;; dump in root-a
    (setq r (dbms-admin-dump dump-path))
    (assert-true "admin dump ok" (and (dbms-result-p r) (eq (second r) 'ok)))
    (assert-true "dump file exists" (not (null (probe-file dump-path))))

    ;; restore in root-b
    (setenv "DBMS_STORAGE_ROOT" root-b)
    (setq catalog (dbms-engine-init))
    (setq r (dbms-admin-restore dump-path))
    (assert-true "admin restore ok" (and (dbms-result-p r) (eq (second r) 'ok)))

    (setq r (dbms-exec-sql catalog "SELECT slug, title FROM pages WHERE id = 1;"))
    (assert-true "pages restored rows" (and (dbms-result-p r) (eq (second r) 'rows)))
    (setq rows (second (third r)))
    (assert-true "pages restored value" (and (= (length rows) 1)
                                              (string= (first (first rows)) "home")
                                              (string= (second (first rows)) "Home")))

    (setq r (dbms-exec-sql catalog "SELECT body_md FROM page_revisions WHERE id = 1;"))
    (setq rows (second (third r)))
    (assert-true "revisions restored value" (and (= (length rows) 1)
                                                  (string= (first (first rows)) "# Home")))

    (setq r (dbms-exec-sql catalog "SELECT filename FROM media_assets WHERE id = 1;"))
    (setq rows (second (third r)))
    (assert-true "media restored value" (and (= (length rows) 1)
                                              (string= (first (first rows)) "logo.png")))

    ;; wiki compatibility wrappers
    (setq wk (dbms-wiki-backup dump-path))
    (assert-true "wiki backup wrapper" (and (listp wk) (string= (first wk) "ok")))
    (setq wk (dbms-wiki-restore dump-path))
    (assert-true "wiki restore wrapper" (and (listp wk) (string= (first wk) "ok")))

    (format t "dbms tx/dump/restore tests passed.~%")))

(run-tests)
