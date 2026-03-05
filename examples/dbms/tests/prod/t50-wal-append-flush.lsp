;; dbms/tests/prod/t50-wal-append-flush.lsp

(load "./examples/dbms/app/repr.lsp")
(load "./examples/dbms/app/engine.lsp")
(load "./examples/dbms/app/storage.lsp")

(defun assert-true (label cond)
  (if cond
      t
      (error (string-append "assert failed: " label))))

(defun wal-has-op-p (records txid kind op)
  (if (null records)
      nil
      (let ((r (car records)))
        (if (and (dbms-wal-record-p r)
                 (= (third r) txid)
                 (eq (fourth r) kind)
                 (eq (fifth r) op))
            t
            (wal-has-op-p (cdr records) txid kind op)))))

(defun wal-lsn-monotonic-p (records)
  (if (or (null records) (null (cdr records)))
      t
      (if (< (second (car records)) (second (second records)))
          (wal-lsn-monotonic-p (cdr records))
          nil)))

(defun run-tests ()
  (let* ((root-base (string-append "/tmp/dbms-wal-" (format nil "~A" (get-universal-time))))
         (root root-base)
         (suffix 0)
         (catalog '())
         (r '())
         (txid 0)
         (records '())
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
    (setq txid (dbms-tx-state-tx-id (dbms-current-tx-state)))
    (assert-true "txid positive" (> txid 0))

    (setq r (dbms-exec-sql catalog "INSERT INTO pages VALUES (1, 'wal-row');"))
    (assert-true "insert ok" (and (dbms-result-p r) (eq (second r) 'count)))

    (setq r (dbms-exec-sql catalog "COMMIT;"))
    (assert-true "commit ok" (and (dbms-result-p r) (eq (second r) 'ok)))

    (assert-true "wal file exists" (not (null (probe-file (dbms-storage-wal-path)))))
    (setq records (dbms-storage-load-wal-records))
    (assert-true "wal has records" (>= (length records) 4))
    (assert-true "wal lsn monotonic" (wal-lsn-monotonic-p records))
    (assert-true "wal has begin record" (wal-has-op-p records txid 'begin 'tx-begin))
    (assert-true "wal has table data record" (wal-has-op-p records txid 'data 'table-replace))
    (assert-true "wal has commit marker" (wal-has-op-p records txid 'commit 'tx-commit))

    ;; committed data is visible after commit
    (setq state (dbms-storage-load-table-rows "pages"))
    (setq rows (third state))
    (assert-true "rows persisted" (= (length rows) 1))

    (format t "dbms wal append/flush tests passed.~%")))

(run-tests)
