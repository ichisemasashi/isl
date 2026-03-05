;; dbms/tests/prod/helpers/durability_commit_worker.lsp

(load "./examples/dbms/app/repr.lsp")
(load "./examples/dbms/app/engine.lsp")

(defun worker-assert-ok (label r expected-kind)
  (if (and (dbms-result-p r) (eq (second r) expected-kind))
      t
      (progn
        (format t "worker assertion failed: ~A => ~A~%" label r)
        (throw 'dbms-worker-failed label))))

(defun run-worker ()
  (let* ((iter (getenv "DBMS_DURABILITY_ID"))
         (tag (if (or (null iter) (string= iter "")) "0" iter))
         (catalog (dbms-engine-init))
         (r '())
         (sql ""))
    (setq r (dbms-exec-sql catalog "CREATE TABLE IF NOT EXISTS pages (id INT PRIMARY KEY, title TEXT NOT NULL);"))
    (worker-assert-ok "create table" r 'ok)
    (setq r (dbms-exec-sql catalog "BEGIN;"))
    (worker-assert-ok "begin" r 'ok)
    (setq sql (string-append "INSERT INTO pages VALUES (1, 'durability-" tag "');"))
    (setq r (dbms-exec-sql catalog sql))
    (worker-assert-ok "insert" r 'count)
    ;; COMMIT may be interrupted by external kill in durability tests.
    (setq r (dbms-exec-sql catalog "COMMIT;"))
    (if (and (dbms-result-p r) (eq (second r) 'ok))
        (format t "worker commit completed.~%")
        (format t "worker commit interrupted or failed: ~A~%" r))))

(catch 'dbms-worker-failed
  (run-worker))
