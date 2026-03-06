;; dbms/tests/prod/t160-online-add-column.lsp
;; P4-003: metadata-only ALTER TABLE ... ADD COLUMN with continued read/write.

(load "./examples/dbms/app/repr.lsp")
(load "./examples/dbms/app/engine.lsp")
(load "./examples/dbms/app/storage.lsp")

(defun assert-true (label cond)
  (if cond
      t
      (progn
        (format t "assert failed: ~A~%" label)
        (throw 'dbms-assert-failed label))))

(defun kv-get (pairs key)
  (if (null pairs)
      '()
      (if (string= (first (car pairs)) key)
          (second (car pairs))
          (kv-get (cdr pairs) key))))

(defun make-rows-range (start end)
  (if (> start end)
      '()
      (cons (dbms-make-row start (list (list "id" start)
                                       (list "title" (string-append "t" (format nil "~A" start)))))
            (make-rows-range (+ start 1) end))))

(defun rows-result-values (r)
  (second (third r)))

(defun run-tests ()
  (let* ((root-base (string-append "/tmp/dbms-online-add-column-" (format nil "~A" (get-universal-time))))
         (root root-base)
         (suffix 0)
         (catalog '())
         (r '())
         (saved '())
         (rebuild '())
         (before '())
         (after '())
         (rows '()))
    (while (not (null (probe-file root)))
      (setq suffix (+ suffix 1))
      (setq root (string-append root-base "-" (format nil "~A" suffix))))
    (setenv "DBMS_STORAGE_ROOT" root)

    (setq catalog (dbms-engine-init))
    (setq r (dbms-exec-sql catalog "CREATE TABLE pages (id INT PRIMARY KEY, title TEXT);"))
    (assert-true "create table ok" (and (dbms-result-p r) (eq (second r) 'ok)))

    ;; Seed a larger table state so we can check ALTER itself does not rewrite data pages.
    (setq saved (dbms-storage-save-table-rows "pages" (make-rows-range 1 120)))
    (assert-true "seed rows save ok" (dbms-table-state-p saved))
    ;; Keep PK index in sync because we bypassed SQL INSERT path.
    (setq rebuild (dbms-engine-rebuild-table-indexes-from-def
                   "pages"
                   (dbms-catalog-find-table (dbms-engine-current-catalog) "pages")
                   (third saved)))
    (assert-true "seed index rebuild ok" (not (dbms-error-p rebuild)))
    (setq before (dbms-storage-table-pages-stats "pages"))

    (setq r (dbms-exec-sql catalog "ALTER TABLE pages ADD COLUMN body TEXT;"))
    (assert-true "alter add column ok" (and (dbms-result-p r) (eq (second r) 'ok)))

    (setq after (dbms-storage-table-pages-stats "pages"))
    (assert-true "alter does not rewrite page-count"
                 (= (kv-get before "page-count") (kv-get after "page-count")))
    (assert-true "alter does not rewrite row-count"
                 (= (kv-get before "row-count") (kv-get after "row-count")))

    ;; Existing rows are readable and expose NULL for the newly added column.
    (setq r (dbms-exec-sql catalog "SELECT body FROM pages WHERE id = 1;"))
    (assert-true "select body existing row" (and (dbms-result-p r) (eq (second r) 'rows)))
    (setq rows (rows-result-values r))
    (assert-true "select existing row count=1" (= (length rows) 1))
    (assert-true "existing row body is NULL" (eq (first (car rows)) 'NULL))

    ;; Write paths continue after ADD COLUMN.
    (setq r (dbms-exec-sql catalog "UPDATE pages SET body = 'hello' WHERE id = 1;"))
    (assert-true "update new column works" (and (dbms-result-p r) (eq (second r) 'count) (= (third r) 1)))
    (setq r (dbms-exec-sql catalog "SELECT body FROM pages WHERE id = 1;"))
    (assert-true "updated value visible" (string= (first (car (rows-result-values r))) "hello"))

    (setq r (dbms-exec-sql catalog "INSERT INTO pages (id, title) VALUES (999, 'n');"))
    (assert-true "insert without new column works" (and (dbms-result-p r) (eq (second r) 'count) (= (third r) 1)))
    (setq r (dbms-exec-sql catalog "SELECT body FROM pages WHERE id = 999;"))
    (assert-true "inserted row default body NULL" (eq (first (car (rows-result-values r))) 'NULL))

    ;; Online ADD COLUMN currently rejects attrs requiring eager full-table backfill.
    (setq r (dbms-exec-sql catalog "ALTER TABLE pages ADD COLUMN must_fill TEXT NOT NULL;"))
    (assert-true "add column not null rejected kind" (and (dbms-result-p r) (eq (second r) 'error)))
    (assert-true "add column not null rejected code" (eq (dbms-error-code (third r)) 'dbms/not-implemented))

    (format t "dbms online add-column tests passed.~%")))

(catch 'dbms-assert-failed
  (run-tests))
