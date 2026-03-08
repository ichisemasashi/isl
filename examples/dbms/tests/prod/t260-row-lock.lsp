;; dbms/tests/prod/t260-row-lock.lsp
;; MT-001: row lock + table/row layered locking.

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
  (let* ((root-base (string-append "/tmp/dbms-row-lock-" (format nil "~A" (get-universal-time))))
         (root root-base)
         (suffix 0)
         (catalog '())
         (r '())
         (s '()))
    (while (not (null (probe-file root)))
      (setq suffix (+ suffix 1))
      (setq root (string-append root-base "-" (format nil "~A" suffix))))
    (setenv "DBMS_STORAGE_ROOT" root)

    (setq catalog (dbms-engine-init))
    (dbms-engine-reset-tx-state)
    (dbms-lock-table-reset!)

    (setq r (dbms-exec-sql catalog "CREATE TABLE pages (id INT PRIMARY KEY, title TEXT NOT NULL);"))
    (assert-true "create pages" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "INSERT INTO pages VALUES (1, 'a');"))
    (assert-true "insert #1" (and (dbms-result-p r) (eq (second r) 'count)))
    (setq r (dbms-exec-sql catalog "INSERT INTO pages VALUES (2, 'b');"))
    (assert-true "insert #2" (and (dbms-result-p r) (eq (second r) 'count)))

    ;; Layered locking: table S compatible + row X isolated by row key.
    (setq r (dbms-lock-acquire 1001 "pages" 'S))
    (assert-true "tx1001 table S" (not (dbms-error-p r)))
    (setq r (dbms-lock-acquire 1002 "pages" 'S))
    (assert-true "tx1002 table S" (not (dbms-error-p r)))

    (setq r (dbms-row-lock-acquire 1001 "pages" "1" 'X))
    (assert-true "tx1001 row1 X" (not (dbms-error-p r)))
    (setq r (dbms-row-lock-acquire 1002 "pages" "2" 'X))
    (assert-true "tx1002 row2 X" (not (dbms-error-p r)))
    (setq r (dbms-row-lock-acquire 1002 "pages" "1" 'X))
    (assert-true "tx1002 row1 X conflict" (and (dbms-error-p r) (eq (dbms-error-code r) 'dbms/lock-conflict)))

    (setq s (dbms-engine-row-lock-table-debug))
    (assert-true "row lock snapshot has 2 entries" (= (length s) 2))

    (dbms-lock-release-tx 1001)
    (dbms-lock-release-tx 1002)

    ;; Row-level deadlock detection.
    (setq r (dbms-row-lock-acquire 1101 "pages" "A" 'X))
    (assert-true "tx1101 rowA X" (not (dbms-error-p r)))
    (setq r (dbms-row-lock-acquire 1102 "pages" "B" 'X))
    (assert-true "tx1102 rowB X" (not (dbms-error-p r)))
    (setq r (dbms-row-lock-acquire 1102 "pages" "A" 'X))
    (assert-true "tx1102 waits rowA" (and (dbms-error-p r) (eq (dbms-error-code r) 'dbms/lock-conflict)))
    (setq r (dbms-row-lock-acquire 1101 "pages" "B" 'X))
    (assert-true "deadlock detected on row locks" (and (dbms-error-p r) (eq (dbms-error-code r) 'dbms/deadlock-detected)))
    (dbms-lock-release-tx 1101)
    (dbms-lock-release-tx 1102)

    ;; SQL path: update on locked row is blocked, different row can proceed.
    (setq r (dbms-row-lock-acquire 900 "pages" "1" 'X))
    (assert-true "external row lock row1" (not (dbms-error-p r)))

    (setq r (dbms-exec-sql catalog "BEGIN;"))
    (assert-true "begin tx for blocked update" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "UPDATE pages SET title='x' WHERE id = 1;"))
    (assert-true "update row1 blocked by row lock" (and (dbms-result-p r)
                                                            (eq (second r) 'error)
                                                            (eq (result-error-code r) 'dbms/lock-conflict)))
    (setq r (dbms-exec-sql catalog "ROLLBACK;"))
    (assert-true "rollback blocked tx" (and (dbms-result-p r) (eq (second r) 'ok)))

    (setq r (dbms-exec-sql catalog "BEGIN;"))
    (assert-true "begin tx for row2 update" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "UPDATE pages SET title='ok' WHERE id = 2;"))
    (assert-true "update row2 succeeds while row1 locked" (and (dbms-result-p r)
                                                                  (eq (second r) 'count)
                                                                  (= (third r) 1)))
    (setq r (dbms-exec-sql catalog "COMMIT;"))
    (assert-true "commit row2 update" (and (dbms-result-p r) (eq (second r) 'ok)))

    ;; Rollback path switch: disable row lock path and fallback to table-lock-only behavior.
    (setenv "DBMS_ENABLE_ROW_LOCKS" "0")
    (setq r (dbms-exec-sql catalog "BEGIN;"))
    (assert-true "begin tx with row lock disabled" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "UPDATE pages SET title='fallback-ok' WHERE id = 1;"))
    (assert-true "update row1 succeeds when row lock path disabled"
                 (and (dbms-result-p r) (eq (second r) 'count) (= (third r) 1)))
    (setq r (dbms-exec-sql catalog "COMMIT;"))
    (assert-true "commit row-lock-disabled update" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setenv "DBMS_ENABLE_ROW_LOCKS" "1")

    (dbms-lock-release-tx 900)

    (format t "dbms row lock tests passed.~%")))

(catch 'dbms-assert-failed
  (run-tests))
