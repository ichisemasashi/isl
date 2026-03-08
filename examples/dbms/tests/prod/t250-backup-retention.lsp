;; dbms/tests/prod/t250-backup-retention.lsp
;; ST-003: backup retention policy (count/days) + prune API.

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

(defun generation-id-in-list-p (generation-id entries)
  (if (null entries)
      nil
      (if (string= generation-id (dbms-backup-generation-id (car entries)))
          t
          (generation-id-in-list-p generation-id (cdr entries)))))

(defun run-tests ()
  (let* ((root-base (string-append "/tmp/dbms-backup-retention-" (format nil "~A" (get-universal-time))))
         (root root-base)
         (suffix 0)
         (catalog '())
         (r '())
         (e1 '())
         (e2 '())
         (e3 '())
         (entries '())
         (oldest '())
         (old-id "")
         (old-dump "")
         (old-wal "")
         (mutated '())
         (rest '())
         (report '()))
    (while (not (null (probe-file root)))
      (setq suffix (+ suffix 1))
      (setq root (string-append root-base "-" (format nil "~A" suffix))))
    (setenv "DBMS_STORAGE_ROOT" root)
    (setenv "DBMS_BACKUP_KEEP_COUNT" "2")
    (setenv "DBMS_BACKUP_KEEP_DAYS" "0")

    (setq catalog (dbms-engine-init))
    (setq r (dbms-exec-sql catalog "CREATE TABLE pages (id INT PRIMARY KEY, title TEXT);"))
    (assert-true "create pages" (and (dbms-result-p r) (eq (second r) 'ok)))

    ;; create 3 generations: keep-count=2 should prune oldest automatically.
    (setq r (dbms-admin-backup-create))
    (assert-true "backup #1" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq e1 (third r))
    (setq r (dbms-admin-backup-create))
    (assert-true "backup #2" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq e2 (third r))
    (setq r (dbms-admin-backup-create))
    (assert-true "backup #3" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq e3 (third r))

    (setq r (dbms-admin-backup-list))
    (assert-true "backup list kind" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq entries (third r))
    (assert-true "backup list count=2 by keep-count" (= (length entries) 2))
    (assert-true "oldest generation removed from list" (not (generation-id-in-list-p (dbms-backup-generation-id e1) entries)))
    (assert-true "latest generation kept #2" (generation-id-in-list-p (dbms-backup-generation-id e2) entries))
    (assert-true "latest generation kept #3" (generation-id-in-list-p (dbms-backup-generation-id e3) entries))
    (assert-true "pruned oldest dump deleted" (null (probe-file (dbms-backup-generation-dump-path e1))))
    (assert-true "pruned oldest wal deleted" (null (probe-file (dbms-backup-generation-wal-path e1))))

    ;; keep-days pruning: mark oldest remaining as stale and prune via API.
    (setenv "DBMS_BACKUP_KEEP_COUNT" "0")
    (setenv "DBMS_BACKUP_KEEP_DAYS" "0")

    (setq r (dbms-admin-backup-list))
    (setq entries (third r))
    (setq oldest (first entries))
    (setq old-id (dbms-backup-generation-id oldest))
    (setq old-dump (dbms-backup-generation-dump-path oldest))
    (setq old-wal (dbms-backup-generation-wal-path oldest))

    (setq rest entries)
    (setq mutated '())
    (while (not (null rest))
      (if (string= (dbms-backup-generation-id (car rest)) old-id)
          (setq mutated
                (append mutated
                        (list (dbms-make-backup-generation
                               (dbms-backup-generation-id (car rest))
                               (- (get-universal-time) (* 3 86400))
                               (dbms-backup-generation-base-lsn (car rest))
                               (dbms-backup-generation-max-lsn (car rest))
                               (dbms-backup-generation-dump-path (car rest))
                               (dbms-backup-generation-wal-path (car rest))))))
          (setq mutated (append mutated (list (car rest)))))
      (setq rest (cdr rest)))

    (setq r (dbms-storage-save-backup-index (list 'dbms-backup-index 1 mutated)))
    (assert-true "mutate backup index for age test" (not (dbms-error-p r)))

    (setq r (dbms-admin-backup-prune 0 1))
    (assert-true "backup prune api ok" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq report (third r))
    (assert-true "prune report deleted >=1" (>= (report-item report "deleted-count") 1))

    (setq r (dbms-admin-backup-list))
    (setq entries (third r))
    (assert-true "backup list count after age prune=1" (= (length entries) 1))
    (assert-true "stale generation removed from list" (not (generation-id-in-list-p old-id entries)))
    (assert-true "stale dump deleted" (null (probe-file old-dump)))
    (assert-true "stale wal deleted" (null (probe-file old-wal)))

    (format t "dbms backup retention tests passed.~%")))

(catch 'dbms-assert-failed
  (run-tests))
