;; dbms/tests/prod/t220-runbook-drill.lsp
;; P4-009: runbook drill (classification/triage/recovery) must be executable.

(load "./examples/dbms/app/repr.lsp")
(load "./examples/dbms/app/storage.lsp")
(load "./examples/dbms/app/engine.lsp")

(defun assert-true (label cond)
  (if cond
      t
      (progn
        (format t "assert failed: ~A~%" label)
        (throw 'dbms-assert-failed label))))

(defun result-rows (r)
  (second (third r)))

(defun metric-value (rows key)
  (if (null rows)
      '()
      (if (string= (first (car rows)) key)
          (second (car rows))
          (metric-value (cdr rows) key))))

(defun audit-action (rec)
  ;; (dbms-audit-record ts user action object result detail)
  (if (and (listp rec) (>= (length rec) 7))
      (fourth rec)
      '()))

(defun audit-has-action-p (logs action)
  (if (null logs)
      nil
      (if (eq (audit-action (car logs)) action)
          t
          (audit-has-action-p (cdr logs) action))))

(defun run-tests ()
  (let* ((root-base (string-append "/tmp/dbms-runbook-drill-" (format nil "~A" (get-universal-time))))
         (root root-base)
         (suffix 0)
         (catalog '())
         (r '())
         (backup '())
         (generation-id "")
         (restore-lsn 0)
         (rows '())
         (metrics '())
         (logs '()))
    (while (not (null (probe-file root)))
      (setq suffix (+ suffix 1))
      (setq root (string-append root-base "-" (format nil "~A" suffix))))
    (setenv "DBMS_STORAGE_ROOT" root)

    ;; bootstrap + baseline data
    (setq catalog (dbms-engine-init))
    (setq r (dbms-exec-sql catalog "CREATE TABLE pages (id INT PRIMARY KEY, title TEXT NOT NULL);"))
    (assert-true "create pages" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "BEGIN;"))
    (assert-true "begin seed tx" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "INSERT INTO pages VALUES (1, 'v1');"))
    (assert-true "insert seed #1" (and (dbms-result-p r) (eq (second r) 'count)))
    (setq r (dbms-exec-sql catalog "INSERT INTO pages VALUES (2, 'v2');"))
    (assert-true "insert seed #2" (and (dbms-result-p r) (eq (second r) 'count)))
    (setq r (dbms-exec-sql catalog "COMMIT;"))
    (assert-true "commit seed tx" (and (dbms-result-p r) (eq (second r) 'ok)))

    ;; runbook daily backup step
    (setq r (dbms-admin-backup-create))
    (assert-true "backup create ok" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq backup (third r))
    (setq generation-id (dbms-backup-generation-id backup))
    (setq restore-lsn (dbms-backup-generation-max-lsn backup))
    (assert-true "backup generation id" (and (stringp generation-id) (not (string= generation-id ""))))

    ;; simulated incident: unwanted write after backup point
    (setq r (dbms-exec-sql catalog "BEGIN;"))
    (assert-true "begin drift tx" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "UPDATE pages SET title='drift' WHERE id = 1;"))
    (assert-true "drift update applied" (and (dbms-result-p r) (eq (second r) 'count) (= (third r) 1)))
    (setq r (dbms-exec-sql catalog "COMMIT;"))
    (assert-true "commit drift tx" (and (dbms-result-p r) (eq (second r) 'ok)))

    ;; triage: metric collection (SLO indicators)
    (setq r (dbms-admin-metrics))
    (assert-true "metrics rows result" (and (dbms-result-p r) (eq (second r) 'rows)))
    (setq metrics (result-rows r))
    (assert-true "has tx-per-sec" (numberp (metric-value metrics "tx-per-sec")))
    (assert-true "has lock-wait-events" (numberp (metric-value metrics "lock-wait-events")))
    (assert-true "has cache-hit-ratio" (numberp (metric-value metrics "cache-hit-ratio")))
    (assert-true "has recovery-lag-records" (numberp (metric-value metrics "recovery-lag-records")))

    ;; recovery drill: PITR to backup point
    (setq r (dbms-admin-restore-pitr generation-id restore-lsn))
    (assert-true "restore pitr ok" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "SELECT id, title FROM pages ORDER BY id ASC;"))
    (assert-true "post-pitr select ok" (and (dbms-result-p r) (eq (second r) 'rows)))
    (setq rows (result-rows r))
    (assert-true "post-pitr rows=2" (= (length rows) 2))
    (assert-true "post-pitr id1 title rolled back" (string= (second (first rows)) "v1"))

    ;; incident/audit drill: induce lock conflict -> tx abort record
    (setq r (dbms-set-session-isolation-level! 'SERIALIZABLE))
    (assert-true "set serializable" (eq r 'SERIALIZABLE))
    (setq r (dbms-exec-sql catalog "BEGIN;"))
    (assert-true "begin conflict tx" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq *dbms-lock-table* (list (list "pages" 'X (list 999))))
    (setq r (dbms-exec-sql catalog "SELECT id FROM pages;"))
    (assert-true "conflict result error" (and (dbms-result-p r) (eq (second r) 'error)))
    (assert-true "conflict code" (eq (dbms-error-code (third r)) 'dbms/lock-conflict))
    (dbms-engine-reset-tx-state)

    (setq logs (dbms-storage-load-audit-log))
    (assert-true "audit has tx-abort" (audit-has-action-p logs 'TX-ABORT))

    (format t "dbms runbook drill tests passed.~%")))

(catch 'dbms-assert-failed
  (run-tests))

