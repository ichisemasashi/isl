;; dbms/tests/prod/t240-checkpoint-recovery.lsp
;; ST-002: checkpoint metadata + checkpoint-based recovery start.

(load "./examples/dbms/app/repr.lsp")
(load "./examples/dbms/app/storage.lsp")
(load "./examples/dbms/app/engine.lsp")

(defun assert-true (label cond)
  (if cond
      t
      (progn
        (format t "assert failed: ~A~%" label)
        (throw 'dbms-assert-failed label))))

(defun report-item-in (pairs key)
  (if (null pairs)
      '()
      (if (string= (first (car pairs)) key)
          (second (car pairs))
          (report-item-in (cdr pairs) key))))

(defun report-item (report key)
  (if (and (listp report) (not (null report)))
      (report-item-in (cdr report) key)
      '()))

(defun run-tests ()
  (let* ((root-base (string-append "/tmp/dbms-checkpoint-" (format nil "~A" (get-universal-time))))
         (root root-base)
         (suffix 0)
         (catalog '())
         (r '())
         (cp1 '())
         (cp2 '())
         (cp1-lsn 0)
         (cp2-max-txid 0)
         (wal-count-before 0)
         (wal-count-after 0)
         (report '())
         (txid 0))
    (while (not (null (probe-file root)))
      (setq suffix (+ suffix 1))
      (setq root (string-append root-base "-" (format nil "~A" suffix))))
    (setenv "DBMS_STORAGE_ROOT" root)

    (setq catalog (dbms-engine-init))

    (setq r (dbms-exec-sql catalog "CREATE TABLE pages (id INT PRIMARY KEY, title TEXT NOT NULL);"))
    (assert-true "create pages" (and (dbms-result-p r) (eq (second r) 'ok)))

    ;; tx #1
    (setq r (dbms-exec-sql catalog "BEGIN;"))
    (assert-true "begin #1" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "INSERT INTO pages VALUES (1, 'a');"))
    (assert-true "insert #1" (and (dbms-result-p r) (eq (second r) 'count)))
    (setq r (dbms-exec-sql catalog "COMMIT;"))
    (assert-true "commit #1" (and (dbms-result-p r) (eq (second r) 'ok)))

    ;; create checkpoint #1
    (setq r (dbms-admin-checkpoint-create))
    (assert-true "checkpoint #1 ok" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq cp1 (third r))
    (assert-true "checkpoint #1 shape" (dbms-checkpoint-meta-p cp1))
    (setq cp1-lsn (dbms-storage-checkpoint-last-lsn cp1))
    (assert-true "checkpoint #1 lsn positive" (> cp1-lsn 0))

    (setq wal-count-before (length (dbms-storage-load-wal-records)))

    ;; tx #2 (after checkpoint)
    (setq r (dbms-exec-sql catalog "BEGIN;"))
    (assert-true "begin #2" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "INSERT INTO pages VALUES (2, 'b');"))
    (assert-true "insert #2" (and (dbms-result-p r) (eq (second r) 'count)))
    (setq r (dbms-exec-sql catalog "COMMIT;"))
    (assert-true "commit #2" (and (dbms-result-p r) (eq (second r) 'ok)))

    (setq wal-count-after (length (dbms-storage-load-wal-records)))
    (assert-true "wal count grows after tx #2" (> wal-count-after wal-count-before))

    ;; recovery should scan only after checkpoint-lsn
    (setq report (dbms-storage-recover-from-wal))
    (assert-true "recovery report object" (and (listp report) (eq (first report) 'dbms-recovery-report)))
    (assert-true "report checkpoint-lsn matches" (= (report-item report "checkpoint-lsn") cp1-lsn))
    (assert-true "report total wal count" (= (report-item report "wal-total-record-count") wal-count-after))
    (assert-true "report scanned count reduced" (< (report-item report "wal-scanned-record-count") wal-count-after))
    (assert-true "report scanned count positive" (> (report-item report "wal-scanned-record-count") 0))

    ;; checkpoint #2 then clear WAL: txid monotonicity must still follow checkpoint max txid.
    (setq r (dbms-admin-checkpoint-create))
    (assert-true "checkpoint #2 ok" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq cp2 (third r))
    (setq cp2-max-txid (dbms-storage-checkpoint-max-txid cp2))
    (assert-true "checkpoint #2 max-txid positive" (> cp2-max-txid 0))

    (setq r (dbms-storage-save-wal-records '()))
    (assert-true "clear wal ok" (not (dbms-error-p r)))

    (setq catalog (dbms-engine-init))
    (setq r (dbms-exec-sql catalog "BEGIN;"))
    (assert-true "begin after wal clear" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq txid (dbms-tx-state-tx-id (dbms-current-tx-state)))
    (assert-true "txid monotonic after checkpoint" (> txid cp2-max-txid))
    (setq r (dbms-exec-sql catalog "ROLLBACK;"))
    (assert-true "rollback after monotonic check" (and (dbms-result-p r) (eq (second r) 'ok)))

    (format t "dbms checkpoint recovery tests passed.~%")))

(catch 'dbms-assert-failed
  (run-tests))
