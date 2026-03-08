;; dbms/tests/prod/t290-replication.lsp
;; LT-001: replication (async->sync), follower apply, manual failover.

(load "./examples/dbms/app/repr.lsp")
(load "./examples/dbms/app/storage.lsp")
(load "./examples/dbms/app/engine.lsp")

(defun assert-true (label cond)
  (if cond
      t
      (progn
        (format t "assert failed: ~A~%" label)
        (throw 'dbms-assert-failed label))))

(defun result-ok-p (r)
  (and (dbms-result-p r) (eq (second r) 'ok)))

(defun restore-root! (root)
  (if (and (stringp root) (not (string= root "")))
      (setenv "DBMS_STORAGE_ROOT" root)
      (setenv "DBMS_STORAGE_ROOT" "")))

(defun table-row-count-on-root (root table-name)
  (let ((old-root (getenv "DBMS_STORAGE_ROOT"))
        (state '())
        (n -1))
    (restore-root! root)
    (setq state (dbms-storage-load-table-rows table-name))
    (if (dbms-error-p state)
        (setq n -1)
        (setq n (length (third state))))
    (restore-root! old-root)
    n))

(defun repl-follower-by-id (meta follower-id)
  (dbms-storage-repl-find-follower (dbms-storage-repl-followers meta) follower-id))

(defun run-tests ()
  (let* ((base (string-append "/tmp/dbms-repl-" (format nil "~A" (get-universal-time))))
         (suffix 0)
         (primary-root (string-append base "-primary"))
         (follower-root (string-append base "-follower"))
         (catalog '())
         (r '())
         (status '())
         (meta '())
         (f '()))

    (while (or (not (null (probe-file primary-root)))
               (not (null (probe-file follower-root))))
      (setq suffix (+ suffix 1))
      (setq primary-root (string-append base "-" (format nil "~A" suffix) "-primary"))
      (setq follower-root (string-append base "-" (format nil "~A" suffix) "-follower")))

    (setenv "DBMS_STORAGE_ROOT" primary-root)
    (setq catalog (dbms-engine-init))
    (dbms-engine-reset-tx-state)

    (setq r (dbms-exec-sql catalog "CREATE TABLE pages (id INT PRIMARY KEY, title TEXT NOT NULL);"))
    (assert-true "create pages" (result-ok-p r))
    (setq r (dbms-exec-sql catalog "INSERT INTO pages VALUES (1, 'p1');"))
    (assert-true "insert row1" (and (dbms-result-p r) (eq (second r) 'count) (= (third r) 1)))

    ;; register follower + bootstrap
    (setq r (dbms-admin-replication-set-mode "ASYNC"))
    (assert-true "set mode async" (result-ok-p r))
    (setq r (dbms-admin-replication-register-follower "f1" follower-root))
    (assert-true "register follower" (result-ok-p r))

    (assert-true "follower row-count after bootstrap" (= (table-row-count-on-root follower-root "pages") 1))

    ;; primary commit after bootstrap
    (setq r (dbms-exec-sql catalog "BEGIN;"))
    (assert-true "begin tx #1" (result-ok-p r))
    (setq r (dbms-exec-sql catalog "INSERT INTO pages VALUES (2, 'p2');"))
    (assert-true "insert row2" (and (dbms-result-p r) (eq (second r) 'count) (= (third r) 1)))
    (setq r (dbms-exec-sql catalog "COMMIT;"))
    (assert-true "commit #1" (result-ok-p r))

    ;; WAL ship only: follower data file still old until apply
    (setq r (dbms-admin-replication-ship))
    (assert-true "ship wal async" (result-ok-p r))
    (assert-true "follower still stale before apply" (= (table-row-count-on-root follower-root "pages") 1))

    (setq r (dbms-admin-replication-apply-follower "f1"))
    (assert-true "apply follower async" (result-ok-p r))
    (assert-true "follower catches up row2" (= (table-row-count-on-root follower-root "pages") 2))

    ;; switch to SYNC mode and verify state transitions.
    (setq r (dbms-admin-replication-set-mode "SYNC"))
    (assert-true "set mode sync" (result-ok-p r))

    (setq r (dbms-exec-sql catalog "BEGIN;"))
    (assert-true "begin tx #2" (result-ok-p r))
    (setq r (dbms-exec-sql catalog "INSERT INTO pages VALUES (3, 'p3');"))
    (assert-true "insert row3" (and (dbms-result-p r) (eq (second r) 'count) (= (third r) 1)))
    (setq r (dbms-exec-sql catalog "COMMIT;"))
    (assert-true "commit #2" (result-ok-p r))

    (setq r (dbms-admin-replication-ship))
    (assert-true "ship wal sync" (result-ok-p r))

    (setq status (dbms-admin-replication-status))
    (assert-true "status ok" (result-ok-p status))
    (setq meta (third status))
    (setq f (repl-follower-by-id meta "f1"))
    (assert-true "sync pending apply state" (string= (dbms-sixth f) "SYNC-PENDING-APPLY"))

    (setq r (dbms-admin-replication-apply-follower "f1"))
    (assert-true "apply follower sync" (result-ok-p r))
    (setq status (dbms-admin-replication-status))
    (setq meta (third status))
    (setq f (repl-follower-by-id meta "f1"))
    (assert-true "sync ack state" (string= (dbms-sixth f) "SYNC-ACK"))
    (assert-true "follower catches up row3" (= (table-row-count-on-root follower-root "pages") 3))

    ;; manual failover: switch active root to follower.
    (setq r (dbms-admin-replication-failover "f1"))
    (assert-true "manual failover ok" (result-ok-p r))
    (setq catalog (dbms-engine-init))
    (setq r (dbms-exec-sql catalog "SELECT id FROM pages ORDER BY id ASC;"))
    (assert-true "post-failover select rows" (and (dbms-result-p r)
                                                    (eq (second r) 'rows)
                                                    (= (length (second (third r))) 3)))

    (format t "dbms replication tests passed.~%")))

(catch 'dbms-assert-failed
  (run-tests))
