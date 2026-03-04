;; dbms/tests/wiki_compat_v1/t40-pages-updated-at.lsp
;; Spec 14.6: TIMESTAMPTZ DEFAULT now(), pages.updated_at auto update

(load "./examples/dbms/app/repr.lsp")
(load "./examples/dbms/app/engine.lsp")

(defun assert-true (label cond)
  (if cond
      t
      (error (string-append "assert failed: " label))))

(defun rows-of (res)
  (second (third res)))

(defun run-tests ()
  (let* ((root (string-append "/tmp/dbms-updated-at-" (format nil "~A" (get-universal-time))))
         (catalog (dbms-engine-init))
         (r '())
         (rows '())
         (updated-at ""))
    (setenv "DBMS_STORAGE_ROOT" root)
    (setq catalog (dbms-engine-init))

    (setq r (dbms-exec-sql catalog
                           "CREATE TABLE pages (id BIGSERIAL PRIMARY KEY, slug TEXT NOT NULL, title TEXT NOT NULL, updated_at TIMESTAMPTZ DEFAULT now());"))
    (assert-true "create pages" (and (dbms-result-p r) (eq (second r) 'ok)))

    (setq r (dbms-exec-sql catalog
                           "CREATE TABLE notes (id BIGSERIAL PRIMARY KEY, body TEXT, updated_at TIMESTAMPTZ DEFAULT now());"))
    (assert-true "create notes" (and (dbms-result-p r) (eq (second r) 'ok)))

    ;; DEFAULT now()
    (setq r (dbms-exec-sql catalog "INSERT INTO pages (slug, title) VALUES ('p1', 'title-1');"))
    (assert-true "insert pages row" (and (dbms-result-p r) (eq (second r) 'count) (= (third r) 1)))
    (setq r (dbms-exec-sql catalog "SELECT updated_at FROM pages WHERE id = 1;"))
    (assert-true "select updated_at rows" (and (dbms-result-p r) (eq (second r) 'rows)))
    (setq rows (rows-of r))
    (assert-true "pages row exists" (= (length rows) 1))
    (setq updated-at (first (first rows)))
    (assert-true "default now set non-null" (not (eq updated-at 'NULL)))

    ;; explicit assignment wins
    (setq r (dbms-exec-sql catalog "UPDATE pages SET updated_at='manual-ts' WHERE id = 1;"))
    (assert-true "manual updated_at update" (and (dbms-result-p r) (eq (second r) 'count) (= (third r) 1)))
    (setq r (dbms-exec-sql catalog "SELECT updated_at FROM pages WHERE id = 1;"))
    (setq rows (rows-of r))
    (assert-true "manual updated_at persisted" (string= (first (first rows)) "manual-ts"))

    ;; pages auto updated_at when omitted in UPDATE SET
    (setq r (dbms-exec-sql catalog "UPDATE pages SET title='title-2' WHERE id = 1;"))
    (assert-true "pages update count" (and (dbms-result-p r) (eq (second r) 'count) (= (third r) 1)))
    (setq r (dbms-exec-sql catalog "SELECT updated_at FROM pages WHERE id = 1;"))
    (setq rows (rows-of r))
    (assert-true "pages auto touched updated_at" (not (string= (first (first rows)) "manual-ts")))

    ;; non-pages table should not auto touch
    (setq r (dbms-exec-sql catalog "INSERT INTO notes (body) VALUES ('n1');"))
    (assert-true "insert notes row" (and (dbms-result-p r) (eq (second r) 'count) (= (third r) 1)))
    (setq r (dbms-exec-sql catalog "UPDATE notes SET updated_at='notes-manual' WHERE id = 1;"))
    (assert-true "notes manual update" (and (dbms-result-p r) (eq (second r) 'count) (= (third r) 1)))
    (setq r (dbms-exec-sql catalog "UPDATE notes SET body='n2' WHERE id = 1;"))
    (assert-true "notes body update" (and (dbms-result-p r) (eq (second r) 'count) (= (third r) 1)))
    (setq r (dbms-exec-sql catalog "SELECT updated_at FROM notes WHERE id = 1;"))
    (setq rows (rows-of r))
    (assert-true "notes updated_at unchanged" (string= (first (first rows)) "notes-manual"))

    (format t "dbms pages.updated_at auto update tests passed.~%")))

(run-tests)
