;; dbms/tests/prod/t60-startup-recovery.lsp

(load "./examples/dbms/app/repr.lsp")
(load "./examples/dbms/app/engine.lsp")
(load "./examples/dbms/app/storage.lsp")

(defun assert-true (label cond)
  (if cond
      t
      (error (string-append "assert failed: " label))))

(defun run-tests ()
  (let* ((root-base (string-append "/tmp/dbms-recovery-" (format nil "~A" (get-universal-time))))
         (root root-base)
         (suffix 0)
         (catalog '())
         (r '())
         (state '())
         (rows '())
         (committed-row (dbms-make-row 1 (list (list "id" 1)
                                               (list "title" "from-committed"))))
         (committed-state (dbms-make-table-state "pages" (list committed-row) 2))
         (uncommitted-row (dbms-make-row 2 (list (list "id" 2)
                                                 (list "title" "from-uncommitted"))))
         (uncommitted-state (dbms-make-table-state "pages" (list committed-row uncommitted-row) 3))
         (txid 0))
    (while (not (null (probe-file root)))
      (setq suffix (+ suffix 1))
      (setq root (string-append root-base "-" (format nil "~A" suffix))))
    (setenv "DBMS_STORAGE_ROOT" root)

    (setq catalog (dbms-engine-init))
    (setq r (dbms-exec-sql catalog "CREATE TABLE IF NOT EXISTS pages (id INT PRIMARY KEY, title TEXT NOT NULL);"))
    (assert-true "create table ok" (and (dbms-result-p r) (eq (second r) 'ok)))

    ;; Simulate crash window: committed tx WAL exists, data file not yet reflected.
    (setq r (dbms-storage-wal-append-record 1001 'begin 'tx-begin '()))
    (assert-true "wal begin committed-tx ok" (not (dbms-error-p r)))
    (setq r (dbms-storage-wal-append-record 1001 'data 'table-replace (list "pages" committed-state)))
    (assert-true "wal data committed-tx ok" (not (dbms-error-p r)))
    (setq r (dbms-storage-wal-append-record 1001 'commit 'tx-commit '()))
    (assert-true "wal commit committed-tx ok" (not (dbms-error-p r)))

    ;; Restart: recovery should redo committed tx.
    (setq catalog (dbms-engine-init))
    (setq state (dbms-storage-load-table-rows "pages"))
    (setq rows (third state))
    (assert-true "rows recovered after restart" (= (length rows) 1))
    (assert-true "recovered title matches" (string= (dbms-row-get-value (first rows) "title") "from-committed"))

    ;; Simulate uncommitted tx: begin+data only.
    (setq r (dbms-storage-wal-append-record 1002 'begin 'tx-begin '()))
    (assert-true "wal begin uncommitted-tx ok" (not (dbms-error-p r)))
    (setq r (dbms-storage-wal-append-record 1002 'data 'table-replace (list "pages" uncommitted-state)))
    (assert-true "wal data uncommitted-tx ok" (not (dbms-error-p r)))

    ;; Restart again: uncommitted tx must not be visible.
    (setq catalog (dbms-engine-init))
    (setq state (dbms-storage-load-table-rows "pages"))
    (setq rows (third state))
    (assert-true "uncommitted rows not applied" (= (length rows) 1))
    (assert-true "still committed title only" (string= (dbms-row-get-value (first rows) "title") "from-committed"))

    ;; txid should advance after restart based on WAL max txid.
    (setq r (dbms-exec-sql catalog "BEGIN;"))
    (assert-true "begin after recovery ok" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq txid (dbms-tx-state-tx-id (dbms-current-tx-state)))
    (assert-true "txid advanced after recovery" (> txid 1002))
    (setq r (dbms-exec-sql catalog "ROLLBACK;"))
    (assert-true "rollback after recovery ok" (and (dbms-result-p r) (eq (second r) 'ok)))

    (format t "dbms startup recovery tests passed.~%")))

(run-tests)
