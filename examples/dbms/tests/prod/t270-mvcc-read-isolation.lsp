;; dbms/tests/prod/t270-mvcc-read-isolation.lsp
;; MT-002: MVCC-based read isolation (statement snapshot + read lock elision).

(load "./examples/dbms/app/repr.lsp")
(load "./examples/dbms/app/engine.lsp")
(load "./examples/dbms/app/storage.lsp")

(defun assert-true (label cond)
  (if cond
      t
      (error (string-append "assert failed: " label))))

(defun select-first-scalar (r)
  (if (and (dbms-result-p r)
           (eq (second r) 'rows)
           (> (length (second (third r))) 0)
           (> (length (first (second (third r)))) 0))
      (first (first (second (third r))))
      '()))

(defun rewrite-title-direct! (new-title)
  (let* ((state (dbms-storage-load-table-rows "pages"))
         (rows (third state))
         (row (car rows))
         (new-row (dbms-make-row (dbms-row-id row)
                                 (dbms-row-values-set (dbms-row-values row) "title" new-title)))
         (saved (dbms-storage-save-table-rows "pages" (list new-row))))
    (if (dbms-error-p saved)
        saved
        t)))

(defun run-tests ()
  (let* ((root-base (string-append "/tmp/dbms-mvcc-" (format nil "~A" (get-universal-time))))
         (root root-base)
         (suffix 0)
         (catalog '())
         (r '())
         (w '()))
    (while (not (null (probe-file root)))
      (setq suffix (+ suffix 1))
      (setq root (string-append root-base "-" (format nil "~A" suffix))))
    (setenv "DBMS_STORAGE_ROOT" root)
    (setenv "DBMS_ENABLE_MVCC" "1")

    (setq catalog (dbms-engine-init))
    (dbms-engine-reset-tx-state)
    (dbms-lock-table-reset!)

    (setq r (dbms-exec-sql catalog "CREATE TABLE pages (id INT PRIMARY KEY, title TEXT NOT NULL);"))
    (assert-true "create pages" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "INSERT INTO pages VALUES (1, 'base');"))
    (assert-true "insert base row" (and (dbms-result-p r) (eq (second r) 'count) (= (third r) 1)))

    ;; MVCC READ COMMITTED: statement snapshot refreshes committed view.
    (setq r (dbms-exec-sql catalog "BEGIN;"))
    (assert-true "begin#1" (and (dbms-result-p r) (eq (second r) 'ok)))

    (setq r (dbms-exec-sql catalog "SELECT title FROM pages WHERE id = 1;"))
    (assert-true "select#1 base" (and (dbms-result-p r)
                                        (eq (second r) 'rows)
                                        (string= (select-first-scalar r) "base")))

    (setq w (rewrite-title-direct! "outside"))
    (assert-true "direct rewrite outside" (not (dbms-error-p w)))

    (setq r (dbms-exec-sql catalog "SELECT title FROM pages WHERE id = 1;"))
    (assert-true "select#2 sees refreshed committed value"
                 (and (dbms-result-p r)
                      (eq (second r) 'rows)
                      (string= (select-first-scalar r) "outside")))

    ;; Own writes remain visible over refreshed snapshots.
    (setq r (dbms-exec-sql catalog "UPDATE pages SET title='self' WHERE id = 1;"))
    (assert-true "update self" (and (dbms-result-p r) (eq (second r) 'count) (= (third r) 1)))

    (setq w (rewrite-title-direct! "outside-2"))
    (assert-true "direct rewrite outside-2" (not (dbms-error-p w)))

    (setq r (dbms-exec-sql catalog "SELECT title FROM pages WHERE id = 1;"))
    (assert-true "select#3 keeps own write"
                 (and (dbms-result-p r)
                      (eq (second r) 'rows)
                      (string= (select-first-scalar r) "self")))

    (setq r (dbms-exec-sql catalog "ROLLBACK;"))
    (assert-true "rollback#1" (and (dbms-result-p r) (eq (second r) 'ok)))

    ;; MVCC read lock elision: SELECT should not conflict with external table X lock.
    (setq w (dbms-lock-acquire 900 "pages" 'X))
    (assert-true "external table X lock" (not (dbms-error-p w)))

    (setq r (dbms-exec-sql catalog "BEGIN;"))
    (assert-true "begin#2" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "SELECT title FROM pages WHERE id = 1;"))
    (assert-true "select under external X lock succeeds with MVCC"
                 (and (dbms-result-p r) (eq (second r) 'rows)))
    (setq r (dbms-exec-sql catalog "ROLLBACK;"))
    (assert-true "rollback#2" (and (dbms-result-p r) (eq (second r) 'ok)))

    (dbms-lock-release-tx 900)

    (setenv "DBMS_ENABLE_MVCC" "0")
    (format t "dbms mvcc read isolation tests passed.~%")))

(run-tests)
