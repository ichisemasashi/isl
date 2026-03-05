;; dbms/tests/prod/t95-serializable.lsp

(load "./examples/dbms/app/repr.lsp")
(load "./examples/dbms/app/engine.lsp")

(defun assert-true (label cond)
  (if cond
      t
      (progn
        (format t "assert failed: ~A~%" label)
        (throw 'dbms-assert-failed label))))

(defun has-table-lock-p (table-name snapshot)
  (if (null snapshot)
      nil
      (if (string= (first (car snapshot)) table-name)
          t
          (has-table-lock-p table-name (cdr snapshot)))))

(defun table-holders (table-name snapshot)
  (if (null snapshot)
      '()
      (if (string= (first (car snapshot)) table-name)
          (third (car snapshot))
          (table-holders table-name (cdr snapshot)))))

(defun number-in-list-p (n xs)
  (if (null xs)
      nil
      (if (= n (car xs))
          t
          (number-in-list-p n (cdr xs)))))

(defun result-error-code (r)
  (if (and (dbms-result-p r)
           (eq (second r) 'error)
           (dbms-error-p (third r)))
      (dbms-error-code (third r))
      '()))

(defun run-tests ()
  (let* ((root-base (string-append "/tmp/dbms-serializable-" (format nil "~A" (get-universal-time))))
         (root root-base)
         (suffix 0)
         (catalog '())
         (r '())
         (snap '())
         (txid 0))
    (while (not (null (probe-file root)))
      (setq suffix (+ suffix 1))
      (setq root (string-append root-base "-" (format nil "~A" suffix))))
    (setenv "DBMS_STORAGE_ROOT" root)
    (setq catalog (dbms-engine-init))
    (dbms-engine-reset-tx-state)
    (dbms-lock-table-reset!)

    (assert-true "default session level rc" (eq (dbms-current-session-isolation-level) 'READ-COMMITTED))
    (setq r (dbms-set-session-isolation-level! 'SERIALIZABLE))
    (assert-true "set session serializable ok" (eq r 'SERIALIZABLE))

    (setq r (dbms-exec-sql catalog "CREATE TABLE IF NOT EXISTS pages (id INT PRIMARY KEY, title TEXT NOT NULL);"))
    (assert-true "create table ok" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "INSERT INTO pages VALUES (1, 'seed');"))
    (assert-true "seed insert ok" (and (dbms-result-p r) (eq (second r) 'count)))

    ;; SERIALIZABLE: S lock should remain to tx end.
    (setq r (dbms-exec-sql catalog "BEGIN;"))
    (assert-true "begin ok" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq txid (dbms-tx-state-tx-id (dbms-current-tx-state)))
    (assert-true "tx level is serializable" (eq (dbms-current-tx-isolation-level) 'SERIALIZABLE))
    (setq r (dbms-exec-sql catalog "SELECT id FROM pages;"))
    (assert-true "select ok" (and (dbms-result-p r) (eq (second r) 'rows)))
    (setq snap (dbms-engine-lock-table-debug))
    (assert-true "S lock retained in serializable" (has-table-lock-p "pages" snap))

    ;; Conflict under SERIALIZABLE should abort current tx.
    (setq r (dbms-lock-acquire 880 "pages" 'X))
    (assert-true "external X blocked by retained S" (and (dbms-error-p r) (eq (dbms-error-code r) 'dbms/lock-conflict)))
    (dbms-lock-release-tx 880)

    ;; Inject conflict from external X then execute write; tx should abort.
    (dbms-lock-release-tx (dbms-tx-state-tx-id (dbms-current-tx-state)))
    (setq r (dbms-lock-acquire 881 "pages" 'X))
    (assert-true "external X acquire ok" (not (dbms-error-p r)))
    (setq r (dbms-exec-sql catalog "UPDATE pages SET title='s1' WHERE id = 1;"))
    (assert-true "update conflict reported" (and (dbms-result-p r)
                                                  (eq (second r) 'error)
                                                  (eq (result-error-code r) 'dbms/lock-conflict)))
    (assert-true "tx moved to aborted on conflict" (eq (dbms-tx-state-status (dbms-current-tx-state)) 'aborted))
    (setq snap (dbms-engine-lock-table-debug))
    (assert-true "aborted tx lock released"
                 (not (number-in-list-p txid (table-holders "pages" snap))))
    (dbms-lock-release-tx 881)

    ;; cleanup after aborted tx and switch back to READ COMMITTED
    (setq r (dbms-exec-sql catalog "ROLLBACK;"))
    (assert-true "rollback from aborted rejected as not-active"
                 (and (dbms-result-p r) (eq (second r) 'error) (eq (result-error-code r) 'dbms/tx-not-active)))
    (setq r (dbms-set-session-isolation-level! 'READ-COMMITTED))
    (assert-true "set session rc ok" (eq r 'READ-COMMITTED))

    ;; RC behavior still works.
    (setq r (dbms-exec-sql catalog "BEGIN;"))
    (assert-true "begin #2 ok" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "SELECT id FROM pages;"))
    (assert-true "select #2 ok" (and (dbms-result-p r) (eq (second r) 'rows)))
    (assert-true "S lock released in rc" (not (has-table-lock-p "pages" (dbms-engine-lock-table-debug))))
    (setq r (dbms-exec-sql catalog "COMMIT;"))
    (assert-true "commit #2 ok" (and (dbms-result-p r) (eq (second r) 'ok)))

    (format t "dbms serializable tests passed.~%")))

(catch 'dbms-assert-failed
  (run-tests))
