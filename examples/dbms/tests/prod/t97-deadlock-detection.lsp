;; dbms/tests/prod/t97-deadlock-detection.lsp

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
  (let* ((root-base (string-append "/tmp/dbms-deadlock-" (format nil "~A" (get-universal-time))))
         (root root-base)
         (suffix 0)
         (catalog '())
         (r '())
         (txid 0))
    (while (not (null (probe-file root)))
      (setq suffix (+ suffix 1))
      (setq root (string-append root-base "-" (format nil "~A" suffix))))
    (setenv "DBMS_STORAGE_ROOT" root)
    (setq catalog (dbms-engine-init))
    (dbms-engine-reset-tx-state)
    (dbms-lock-table-reset!)
    (dbms-set-session-isolation-level! 'SERIALIZABLE)

    (setq r (dbms-exec-sql catalog "CREATE TABLE IF NOT EXISTS t1 (id INT PRIMARY KEY, v TEXT);"))
    (assert-true "create t1 ok" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "CREATE TABLE IF NOT EXISTS t2 (id INT PRIMARY KEY, v TEXT);"))
    (assert-true "create t2 ok" (and (dbms-result-p r) (eq (second r) 'ok)))

    ;; tx1: hold X on t1
    (setq r (dbms-exec-sql catalog "BEGIN;"))
    (assert-true "begin ok" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq txid (dbms-tx-state-tx-id (dbms-current-tx-state)))
    (assert-true "txid positive" (> txid 0))
    (setq r (dbms-exec-sql catalog "INSERT INTO t1 VALUES (1, 'a');"))
    (assert-true "tx1 lock t1 by write ok" (and (dbms-result-p r) (eq (second r) 'count)))

    ;; tx2 (simulated id=900) holds X on t2 and waits on t1.
    (setq r (dbms-lock-acquire 900 "t2" 'X))
    (assert-true "tx2 lock t2 ok" (not (dbms-error-p r)))
    (setq r (dbms-lock-acquire 900 "t1" 'X))
    (assert-true "tx2 waits on t1 conflict"
                 (and (dbms-error-p r) (eq (dbms-error-code r) 'dbms/lock-conflict)))

    ;; tx1 now requests t2 -> cycle (tx1->tx2 and tx2->tx1) => deadlock detected.
    (setq r (dbms-exec-sql catalog "INSERT INTO t2 VALUES (1, 'b');"))
    (assert-true "deadlock detected on tx1 request"
                 (and (dbms-result-p r)
                      (eq (second r) 'error)
                      (eq (result-error-code r) 'dbms/deadlock-detected)))
    (assert-true "tx1 aborted by deadlock victim policy"
                 (eq (dbms-tx-state-status (dbms-current-tx-state)) 'aborted))

    ;; cleanup and confirm forward progress after victim abort.
    (dbms-lock-release-tx 900)
    (setq r (dbms-exec-sql catalog "BEGIN;"))
    (assert-true "begin after deadlock ok" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "INSERT INTO t2 VALUES (2, 'c');"))
    (assert-true "insert after deadlock ok" (and (dbms-result-p r) (eq (second r) 'count)))
    (setq r (dbms-exec-sql catalog "COMMIT;"))
    (assert-true "commit after deadlock ok" (and (dbms-result-p r) (eq (second r) 'ok)))

    (format t "dbms deadlock detection tests passed.~%")))

(catch 'dbms-assert-failed
  (run-tests))
