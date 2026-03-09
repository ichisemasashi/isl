;; dbms/tests/prod/t300-auto-failover.lsp
;; LT-002: health check + leader election + split-brain prevention.

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

(defun result-rows-p (r)
  (and (dbms-result-p r) (eq (second r) 'rows)))

(defun rows-of (r)
  (second (third r)))

(defun run-tests ()
  (let* ((base (string-append "/tmp/dbms-auto-failover-" (format nil "~A" (get-universal-time))))
         (primary-root base)
         (follower-root (string-append base "-f1"))
         (suffix 0)
         (catalog '())
         (r '())
         (status '())
         (meta '())
         (failover '())
         (now-sec (get-universal-time)))
    (while (or (not (null (probe-file primary-root)))
               (not (null (probe-file follower-root))))
      (setq suffix (+ suffix 1))
      (setq primary-root (string-append base "-" (format nil "~A" suffix)))
      (setq follower-root (string-append base "-" (format nil "~A" suffix) "-f1")))

    (setenv "DBMS_STORAGE_ROOT" primary-root)
    (setq catalog (dbms-engine-init))

    ;; baseline and follower bootstrap.
    (setq r (dbms-exec-sql catalog "CREATE TABLE pages (id INT PRIMARY KEY, title TEXT NOT NULL);"))
    (assert-true "create table pages" (result-ok-p r))
    (setq r (dbms-exec-sql catalog "INSERT INTO pages VALUES (1, 'seed');"))
    (assert-true "insert seed row" (and (dbms-result-p r) (eq (second r) 'count) (= (third r) 1)))
    (setq r (dbms-admin-replication-set-mode "SYNC"))
    (assert-true "set replication sync" (result-ok-p r))
    (setq r (dbms-admin-replication-register-follower "f1" follower-root))
    (assert-true "register follower f1" (result-ok-p r))

    ;; health/election helper checks (unit-ish).
    (setq meta (dbms-storage-load-failover-meta))
    (assert-true "primary healthy with fresh heartbeat"
                 (dbms-engine-failover-primary-healthy-p
                  (list 'dbms-failover-meta 1 t 10 now-sec "primary" (+ now-sec 10) 0)
                  now-sec))
    (assert-true "primary unhealthy when heartbeat stale"
                 (not
                  (dbms-engine-failover-primary-healthy-p
                   (list 'dbms-failover-meta 1 t 1 (- now-sec 10) "primary" (- now-sec 1) 0)
                   now-sec)))
    (assert-true "elect candidate by applied lsn then id"
                 (string=
                  (dbms-repl-follower-id
                   (dbms-engine-repl-elect-candidate
                    (list (dbms-make-repl-follower "f2" "/tmp/f2" 10 10 "SYNC-ACK")
                          (dbms-make-repl-follower "f1" "/tmp/f1" 10 10 "SYNC-ACK")
                          (dbms-make-repl-follower "f3" "/tmp/f3" 11 11 "SYNC-ACK"))))
                  "f3"))

    ;; auto-failover on: healthy primary -> skip.
    (setq r (dbms-admin-replication-auto-failover-enable 2))
    (assert-true "enable auto failover" (result-ok-p r))
    (setq r (dbms-admin-replication-heartbeat))
    (assert-true "heartbeat update" (result-ok-p r))
    (setq r (dbms-admin-replication-auto-failover-tick))
    (assert-true "tick skips healthy primary" (and (result-ok-p r)
                                                    (string= (dbms-assoc-get (third r) "reason") "PRIMARY_HEALTHY")))

    ;; replicate latest row, then force primary timeout and auto-promote.
    (setq r (dbms-exec-sql catalog "BEGIN;"))
    (assert-true "begin tx for row2" (result-ok-p r))
    (setq r (dbms-exec-sql catalog "INSERT INTO pages VALUES (2, 'row2');"))
    (assert-true "insert row2" (and (dbms-result-p r) (eq (second r) 'count) (= (third r) 1)))
    (setq r (dbms-exec-sql catalog "COMMIT;"))
    (assert-true "commit tx for row2" (result-ok-p r))
    (setq r (dbms-admin-replication-ship))
    (assert-true "ship before promote" (result-ok-p r))
    (setq r (dbms-admin-replication-apply-follower "f1"))
    (assert-true "apply follower before promote" (result-ok-p r))

    (setq now-sec (get-universal-time))
    (setq failover
          (list 'dbms-failover-meta 1 t 1 (- now-sec 10) "primary" (- now-sec 1) 0))
    (assert-true "inject stale primary heartbeat"
                 (not (dbms-error-p (dbms-storage-save-failover-meta failover))))

    (setq r (dbms-admin-replication-auto-failover-tick))
    (assert-true "tick promotes follower automatically" (and (result-ok-p r)
                                                              (string= (dbms-assoc-get (third r) "action") "PROMOTE")
                                                              (string= (dbms-assoc-get (third r) "follower-id") "f1")))
    (assert-true "active root switched to follower" (string= (dbms-storage-root) follower-root))

    (setq r (dbms-exec-sql catalog "SELECT id, title FROM pages ORDER BY id ASC;"))
    (assert-true "post-promotion select ok" (result-rows-p r))
    (assert-true "post-promotion row count=2" (= (length (rows-of r)) 2))

    (setq status (dbms-admin-replication-auto-failover-status))
    (assert-true "status after promotion ok" (result-ok-p status))
    (setq failover (dbms-assoc-get (third status) "failover"))
    (assert-true "lease owner switched from primary"
                 (and (dbms-failover-meta-p failover)
                      (not (string= (dbms-storage-failover-lease-owner failover) "primary"))
                      (> (dbms-storage-failover-fence-epoch failover) 0)))

    ;; split-brain guard: already promoted owner cannot promote again.
    (setq r (dbms-admin-replication-auto-failover-tick))
    (assert-true "second tick skips already promoted cluster" (and (result-ok-p r)
                                                                    (string= (dbms-assoc-get (third r) "reason") "ALREADY_PROMOTED")))

    (format t "dbms auto failover tests passed.~%")))

(catch 'dbms-assert-failed
  (run-tests))
