;; dbms/tests/prod/t30-commit-staging.lsp

(load "./examples/dbms/app/repr.lsp")
(load "./examples/dbms/app/engine.lsp")
(load "./examples/dbms/app/storage.lsp")

(defun assert-true (label cond)
  (if cond
      t
      (error (string-append "assert failed: " label))))

(defun run-tests ()
  (let* ((root-base (string-append "/tmp/dbms-commit-staging-" (format nil "~A" (get-universal-time))))
         (root root-base)
         (suffix 0)
         (catalog '())
         (r '())
         (state '())
         (rows '()))
    (while (not (null (probe-file root)))
      (setq suffix (+ suffix 1))
      (setq root (string-append root-base "-" (format nil "~A" suffix))))
    (setenv "DBMS_STORAGE_ROOT" root)
    (setq catalog (dbms-engine-init))
    (dbms-engine-reset-tx-state)

    (setq r (dbms-exec-sql catalog "CREATE TABLE IF NOT EXISTS pages (id INT PRIMARY KEY, title TEXT NOT NULL);"))
    (assert-true "create table ok" (and (dbms-result-p r) (eq (second r) 'ok)))

    (setq r (dbms-exec-sql catalog "BEGIN;"))
    (assert-true "begin ok" (and (dbms-result-p r) (eq (second r) 'ok)))

    (setq r (dbms-exec-sql catalog "INSERT INTO pages VALUES (1, 'staged');"))
    (assert-true "insert staged ok" (and (dbms-result-p r) (eq (second r) 'count)))

    ;; tx 中の SQL 読み取りでは見える（read-your-writes）
    (setq r (dbms-exec-sql catalog "SELECT id FROM pages;"))
    (assert-true "select in tx rows kind" (and (dbms-result-p r) (eq (second r) 'rows)))
    (assert-true "select in tx rows=1" (= (length (second (third r))) 1))

    ;; committed read（storage 直読）ではまだ見えない
    (setq state (dbms-storage-load-table-rows "pages"))
    (setq rows (third state))
    (assert-true "storage still no rows before commit" (= (length rows) 0))

    (setq r (dbms-exec-sql catalog "COMMIT;"))
    (assert-true "commit ok" (and (dbms-result-p r) (eq (second r) 'ok)))

    ;; commit 後に永続化される
    (setq state (dbms-storage-load-table-rows "pages"))
    (setq rows (third state))
    (assert-true "storage has row after commit" (= (length rows) 1))

    ;; rollback では staging が破棄される
    (setq r (dbms-exec-sql catalog "BEGIN;"))
    (assert-true "begin#2 ok" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "INSERT INTO pages VALUES (2, 'drop-me');"))
    (assert-true "insert#2 staged ok" (and (dbms-result-p r) (eq (second r) 'count)))
    (setq r (dbms-exec-sql catalog "ROLLBACK;"))
    (assert-true "rollback ok" (and (dbms-result-p r) (eq (second r) 'ok)))

    (setq state (dbms-storage-load-table-rows "pages"))
    (setq rows (third state))
    (assert-true "storage rows remain 1 after rollback" (= (length rows) 1))

    (format t "dbms commit staging tests passed.~%")))

(run-tests)
