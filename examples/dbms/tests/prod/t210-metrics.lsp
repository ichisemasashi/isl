;; dbms/tests/prod/t210-metrics.lsp
;; P4-008: metrics export (tx/sec, lock wait, cache hit, recovery lag).

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

(defun run-tests ()
  (let* ((root-base (string-append "/tmp/dbms-metrics-" (format nil "~A" (get-universal-time))))
         (root root-base)
         (suffix 0)
         (catalog '())
         (table-def '())
         (seed-state '())
         (r '())
         (rows '())
         (v '()))
    (while (not (null (probe-file root)))
      (setq suffix (+ suffix 1))
      (setq root (string-append root-base "-" (format nil "~A" suffix))))
    (setenv "DBMS_STORAGE_ROOT" root)

    ;; Build a recoverable WAL record so startup reports non-zero recovery lag.
    (setq table-def (dbms-make-table-def "seed"
                                         (list (dbms-make-column-def "id" 'INT '(PRIMARY-KEY)))
                                         '()
                                         '()))
    (dbms-storage-save-catalog (dbms-catalog-add-table-def (dbms-empty-catalog) table-def))
    (dbms-storage-save-table-state "seed" (dbms-make-table-state "seed" '() 1))
    (setq seed-state (dbms-make-table-state "seed"
                                            (list (dbms-make-row 1 (list (list "id" 1))))
                                            2))
    (setq r (dbms-storage-wal-append-record 901 'begin 'tx-begin '()))
    (assert-true "seed wal begin" (not (dbms-error-p r)))
    (setq r (dbms-storage-wal-append-record 901 'data 'table-replace (list "seed" seed-state)))
    (assert-true "seed wal data" (not (dbms-error-p r)))
    (setq r (dbms-storage-wal-append-record 901 'commit 'tx-commit '()))
    (assert-true "seed wal commit" (not (dbms-error-p r)))

    (setq catalog (dbms-engine-init))

    (setq r (dbms-admin-metrics))
    (assert-true "metrics rows kind after init" (and (dbms-result-p r) (eq (second r) 'rows)))
    (setq rows (result-rows r))
    (setq v (metric-value rows "recovery-lag-records"))
    (assert-true "recovery lag observed" (and (numberp v) (>= v 1)))

    (setq r (dbms-admin-metrics-reset))
    (assert-true "metrics reset ok" (and (dbms-result-p r) (eq (second r) 'ok)))

    (setq r (dbms-exec-sql catalog "CREATE TABLE pages (id INT PRIMARY KEY, title TEXT);"))
    (assert-true "create table" (and (dbms-result-p r) (eq (second r) 'ok)))

    ;; committed tx count + tx/sec
    (setq r (dbms-exec-sql catalog "BEGIN;"))
    (assert-true "begin #1" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "INSERT INTO pages VALUES (1, 'a');"))
    (assert-true "insert #1" (and (dbms-result-p r) (eq (second r) 'count)))
    (setq r (dbms-exec-sql catalog "COMMIT;"))
    (assert-true "commit #1" (and (dbms-result-p r) (eq (second r) 'ok)))

    ;; aborted tx count
    (setq r (dbms-exec-sql catalog "BEGIN;"))
    (assert-true "begin #2" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "ROLLBACK;"))
    (assert-true "rollback #2" (and (dbms-result-p r) (eq (second r) 'ok)))

    ;; lock wait/conflict count
    (setq r (dbms-exec-sql catalog "BEGIN;"))
    (assert-true "begin #3" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq *dbms-lock-table* (list (list "pages" 'X (list 999))))
    (setq r (dbms-exec-sql catalog "SELECT id FROM pages;"))
    (assert-true "select lock conflict kind" (and (dbms-result-p r) (eq (second r) 'error)))
    (assert-true "select lock conflict code" (eq (dbms-error-code (third r)) 'dbms/lock-conflict))
    (dbms-engine-reset-tx-state)

    ;; cache miss + hit: first read populates, second read should hit.
    (setq r (dbms-exec-sql catalog "SELECT id FROM pages ORDER BY id ASC;"))
    (assert-true "select #1 rows" (and (dbms-result-p r) (eq (second r) 'rows)))
    (setq r (dbms-exec-sql catalog "SELECT id FROM pages ORDER BY id ASC;"))
    (assert-true "select #2 rows" (and (dbms-result-p r) (eq (second r) 'rows)))

    (setq r (dbms-admin-metrics))
    (assert-true "metrics rows kind" (and (dbms-result-p r) (eq (second r) 'rows)))
    (setq rows (result-rows r))

    (setq v (metric-value rows "tx-committed"))
    (assert-true "tx-committed >=1" (and (numberp v) (>= v 1)))
    (setq v (metric-value rows "tx-aborted"))
    (assert-true "tx-aborted >=1" (and (numberp v) (>= v 1)))
    (setq v (metric-value rows "tx-per-sec"))
    (assert-true "tx-per-sec numeric" (numberp v))

    (setq v (metric-value rows "lock-wait-events"))
    (assert-true "lock-wait-events >=1" (and (numberp v) (>= v 1)))
    (setq v (metric-value rows "lock-conflicts"))
    (assert-true "lock-conflicts >=1" (and (numberp v) (>= v 1)))

    (setq v (metric-value rows "cache-hit"))
    (assert-true "cache-hit >=1" (and (numberp v) (>= v 1)))
    (setq v (metric-value rows "cache-miss"))
    (assert-true "cache-miss >=1" (and (numberp v) (>= v 1)))
    (setq v (metric-value rows "cache-hit-ratio"))
    (assert-true "cache-hit-ratio numeric" (numberp v))

    (format t "dbms metrics tests passed.~%")))

(catch 'dbms-assert-failed
  (run-tests))
