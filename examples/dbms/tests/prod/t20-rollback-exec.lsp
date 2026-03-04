;; dbms/tests/prod/t20-rollback-exec.lsp

(load "./examples/dbms/app/repr.lsp")
(load "./examples/dbms/app/engine.lsp")

(defun assert-true (label cond)
  (if cond
      t
      (progn
        (format t "assert failed: ~A~%" label)
        (throw 'dbms-assert-failed label))))

(defun run-tests ()
  (let* ((root-base (string-append "/tmp/dbms-rollback-" (format nil "~A" (get-universal-time))))
         (root root-base)
         (suffix 0)
         (catalog '())
         (r '())
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

    (setq r (dbms-exec-sql catalog "INSERT INTO pages VALUES (1, 'tx-row');"))
    (assert-true "insert in tx ok" (and (dbms-result-p r) (eq (second r) 'count) (= (third r) 1)))

    (setq r (dbms-exec-sql catalog "ROLLBACK;"))
    (assert-true "rollback ok" (and (dbms-result-p r) (eq (second r) 'ok)))

    (setq r (dbms-exec-sql catalog "SELECT id, title FROM pages;"))
    (assert-true "select rows kind" (and (dbms-result-p r) (eq (second r) 'rows)))
    (setq rows (second (third r)))
    (assert-true "rollback removed inserted row" (= (length rows) 0))

    (format t "dbms rollback execution tests passed.~%")))

(catch 'dbms-assert-failed
  (run-tests))
