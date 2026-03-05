;; dbms/tests/prod/t90-read-committed.lsp

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

(defun has-table-lock-p (table-name snapshot)
  (if (null snapshot)
      nil
      (if (string= (first (car snapshot)) table-name)
          t
          (has-table-lock-p table-name (cdr snapshot)))))

(defun run-tests ()
  (let* ((root-base (string-append "/tmp/dbms-read-committed-" (format nil "~A" (get-universal-time))))
         (root root-base)
         (suffix 0)
         (catalog '())
         (r '())
         (snap '()))
    (while (not (null (probe-file root)))
      (setq suffix (+ suffix 1))
      (setq root (string-append root-base "-" (format nil "~A" suffix))))
    (setenv "DBMS_STORAGE_ROOT" root)
    (setq catalog (dbms-engine-init))
    (dbms-engine-reset-tx-state)
    (dbms-lock-table-reset!)

    (setq r (dbms-exec-sql catalog "CREATE TABLE IF NOT EXISTS pages (id INT PRIMARY KEY, title TEXT NOT NULL);"))
    (assert-true "create table ok" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "INSERT INTO pages VALUES (1, 'seed');"))
    (assert-true "seed insert ok" (and (dbms-result-p r) (eq (second r) 'count)))

    ;; Statement-boundary release of S lock.
    (setq r (dbms-exec-sql catalog "BEGIN;"))
    (assert-true "begin ok" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "SELECT id FROM pages;"))
    (assert-true "select in tx ok" (and (dbms-result-p r) (eq (second r) 'rows)))
    (setq snap (dbms-engine-lock-table-debug))
    (assert-true "select S lock released at statement boundary" (not (has-table-lock-p "pages" snap)))

    ;; Because S lock is released, another tx can now get X lock.
    (setq r (dbms-lock-acquire 701 "pages" 'X))
    (assert-true "external X acquire after select ok" (not (dbms-error-p r)))
    (dbms-lock-release-tx 701)

    ;; Dirty read prevention: while another tx holds X, SELECT must fail.
    (setq r (dbms-lock-acquire 702 "pages" 'X))
    (assert-true "external X acquire for conflict ok" (not (dbms-error-p r)))
    (setq r (dbms-exec-sql catalog "SELECT id FROM pages;"))
    (assert-true "select blocked by X lock" (and (dbms-result-p r)
                                                  (eq (second r) 'error)
                                                  (eq (result-error-code r) 'dbms/lock-conflict)))
    (dbms-lock-release-tx 702)
    (setq r (dbms-exec-sql catalog "SELECT id FROM pages;"))
    (assert-true "select succeeds after X release" (and (dbms-result-p r) (eq (second r) 'rows)))

    ;; X lock acquired by write is held to tx end (not released at statement boundary).
    (setq r (dbms-exec-sql catalog "INSERT INTO pages VALUES (2, 'tx-write');"))
    (assert-true "insert in tx ok" (and (dbms-result-p r) (eq (second r) 'count)))
    (setq r (dbms-exec-sql catalog "SELECT id FROM pages;"))
    (assert-true "select after write ok" (and (dbms-result-p r) (eq (second r) 'rows)))
    (setq snap (dbms-engine-lock-table-debug))
    (assert-true "X lock still held until tx end" (has-table-lock-p "pages" snap))

    (setq r (dbms-exec-sql catalog "COMMIT;"))
    (assert-true "commit ok" (and (dbms-result-p r) (eq (second r) 'ok)))
    (assert-true "locks cleared after commit" (null (dbms-engine-lock-table-debug)))

    (format t "dbms read committed tests passed.~%")))

(catch 'dbms-assert-failed
  (run-tests))
