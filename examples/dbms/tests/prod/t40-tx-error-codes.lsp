;; dbms/tests/prod/t40-tx-error-codes.lsp

(load "./examples/dbms/app/repr.lsp")
(load "./examples/dbms/app/engine.lsp")

(defun assert-true (label cond)
  (if cond
      t
      (error (string-append "assert failed: " label))))

(defun result-error-code (r)
  (if (and (dbms-result-p r)
           (eq (second r) 'error)
           (dbms-error-p (third r)))
      (dbms-error-code (third r))
      '()))

(defun run-tests ()
  (let* ((root-base (string-append "/tmp/dbms-tx-err-" (format nil "~A" (get-universal-time))))
         (root root-base)
         (suffix 0)
         (catalog '())
         (r '()))
    (while (not (null (probe-file root)))
      (setq suffix (+ suffix 1))
      (setq root (string-append root-base "-" (format nil "~A" suffix))))
    (setenv "DBMS_STORAGE_ROOT" root)
    (setq catalog (dbms-engine-init))
    (dbms-engine-reset-tx-state)

    ;; COMMIT without BEGIN
    (setq r (dbms-exec-sql catalog "COMMIT;"))
    (assert-true "commit without begin is error" (and (dbms-result-p r) (eq (second r) 'error)))
    (assert-true "commit without begin code" (eq (result-error-code r) 'dbms/tx-not-active))

    ;; ROLLBACK without BEGIN
    (setq r (dbms-exec-sql catalog "ROLLBACK;"))
    (assert-true "rollback without begin is error" (and (dbms-result-p r) (eq (second r) 'error)))
    (assert-true "rollback without begin code" (eq (result-error-code r) 'dbms/tx-not-active))

    ;; Double BEGIN
    (setq r (dbms-exec-sql catalog "BEGIN;"))
    (assert-true "begin #1 ok" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "BEGIN;"))
    (assert-true "begin #2 is error" (and (dbms-result-p r) (eq (second r) 'error)))
    (assert-true "begin #2 code" (eq (result-error-code r) 'dbms/tx-already-active))

    ;; cleanup
    (setq r (dbms-exec-sql catalog "ROLLBACK;"))
    (assert-true "cleanup rollback ok" (and (dbms-result-p r) (eq (second r) 'ok)))

    (format t "dbms tx error code tests passed.~%")))

(run-tests)
