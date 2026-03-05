;; dbms/tests/prod/t80-lock-manager.lsp

(load "./examples/dbms/app/repr.lsp")
(load "./examples/dbms/app/engine.lsp")

(defun assert-true (label cond)
  (if cond
      t
      (progn
        (format t "assert failed: ~A~%" label)
        (throw 'dbms-assert-failed label))))

(defun result-error-code (r)
  (if (and (dbms-result-p r)
           (eq (second r) 'error)
           (dbms-error-p (third r)))
      (dbms-error-code (third r))
      '()))

(defun run-tests ()
  (let* ((root-base (string-append "/tmp/dbms-lock-mgr-" (format nil "~A" (get-universal-time))))
         (root root-base)
         (suffix 0)
         (catalog '())
         (r '())
         (s '())
         (txid 0))
    (while (not (null (probe-file root)))
      (setq suffix (+ suffix 1))
      (setq root (string-append root-base "-" (format nil "~A" suffix))))
    (setenv "DBMS_STORAGE_ROOT" root)
    (setq catalog (dbms-engine-init))
    (dbms-engine-reset-tx-state)
    (dbms-lock-table-reset!)

    ;; direct lock manager contract (S/S allowed, X conflict, upgrade)
    (setq r (dbms-lock-acquire 101 "pages" 'S))
    (assert-true "tx101 S lock ok" (not (dbms-error-p r)))
    (setq r (dbms-lock-acquire 102 "pages" 'S))
    (assert-true "tx102 S lock ok" (not (dbms-error-p r)))
    (setq r (dbms-lock-acquire 201 "pages" 'X))
    (assert-true "tx201 X lock conflict" (and (dbms-error-p r) (eq (dbms-error-code r) 'dbms/lock-conflict)))
    (setq r (dbms-lock-acquire 101 "pages" 'X))
    (assert-true "tx101 upgrade conflict while tx102 holds S" (and (dbms-error-p r) (eq (dbms-error-code r) 'dbms/lock-conflict)))
    (dbms-lock-release-tx 102)
    (setq r (dbms-lock-acquire 101 "pages" 'X))
    (assert-true "tx101 upgrade to X after release ok" (not (dbms-error-p r)))
    (setq s (dbms-engine-lock-table-debug))
    (assert-true "snapshot has 1 entry" (= (length s) 1))
    (assert-true "snapshot mode X" (eq (second (first s)) 'X))
    (dbms-lock-release-tx 101)
    (assert-true "lock table empty after releases" (null (dbms-engine-lock-table-debug)))

    ;; engine path: write-write conflict returns error.
    (setq r (dbms-exec-sql catalog "CREATE TABLE IF NOT EXISTS pages (id INT PRIMARY KEY, title TEXT NOT NULL);"))
    (assert-true "create pages ok" (and (dbms-result-p r) (eq (second r) 'ok)))

    ;; external holder simulates another tx's write lock.
    (setq r (dbms-lock-acquire 999 "pages" 'X))
    (assert-true "external X holder set" (not (dbms-error-p r)))

    (setq r (dbms-exec-sql catalog "BEGIN;"))
    (assert-true "begin ok" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq txid (dbms-tx-state-tx-id (dbms-current-tx-state)))
    (assert-true "txid positive" (> txid 0))

    (setq r (dbms-exec-sql catalog "INSERT INTO pages VALUES (1, 'x');"))
    (assert-true "insert blocked by lock conflict" (and (dbms-result-p r)
                                                          (eq (second r) 'error)
                                                          (eq (result-error-code r) 'dbms/lock-conflict)))

    (setq r (dbms-exec-sql catalog "ROLLBACK;"))
    (assert-true "rollback ok after conflict" (and (dbms-result-p r) (eq (second r) 'ok)))

    (dbms-lock-release-tx 999)
    (setq r (dbms-exec-sql catalog "BEGIN;"))
    (assert-true "begin #2 ok" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "INSERT INTO pages VALUES (1, 'ok');"))
    (assert-true "insert succeeds without conflict" (and (dbms-result-p r) (eq (second r) 'count)))
    (setq r (dbms-exec-sql catalog "COMMIT;"))
    (assert-true "commit ok" (and (dbms-result-p r) (eq (second r) 'ok)))

    (format t "dbms lock manager tests passed.~%")))

(catch 'dbms-assert-failed
  (run-tests))
