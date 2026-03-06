;; dbms/tests/prod/t200-backup-pitr.lsp
;; P4-007: backup generation management + PITR restore.

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

(defun run-tests ()
  (let* ((root-base (string-append "/tmp/dbms-backup-pitr-" (format nil "~A" (get-universal-time))))
         (root root-base)
         (suffix 0)
         (catalog '())
         (r '())
         (lsn-before-backup 0)
         (gen '())
         (gen-id "")
         (rows '()))
    (while (not (null (probe-file root)))
      (setq suffix (+ suffix 1))
      (setq root (string-append root-base "-" (format nil "~A" suffix))))
    (setenv "DBMS_STORAGE_ROOT" root)

    (setq catalog (dbms-engine-init))

    (setq r (dbms-exec-sql catalog "CREATE TABLE pages (id INT PRIMARY KEY, title TEXT NOT NULL);"))
    (assert-true "create table" (and (dbms-result-p r) (eq (second r) 'ok)))

    (setq r (dbms-exec-sql catalog "BEGIN;"))
    (assert-true "begin #1" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "INSERT INTO pages VALUES (1, 'v1');"))
    (assert-true "insert #1" (and (dbms-result-p r) (eq (second r) 'count)))
    (setq r (dbms-exec-sql catalog "COMMIT;"))
    (assert-true "commit #1" (and (dbms-result-p r) (eq (second r) 'ok)))

    (setq r (dbms-exec-sql catalog "BEGIN;"))
    (assert-true "begin #2" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "INSERT INTO pages VALUES (2, 'v2');"))
    (assert-true "insert #2" (and (dbms-result-p r) (eq (second r) 'count)))
    (setq r (dbms-exec-sql catalog "COMMIT;"))
    (assert-true "commit #2" (and (dbms-result-p r) (eq (second r) 'ok)))

    (setq lsn-before-backup (dbms-storage-wal-max-lsn))
    (assert-true "lsn-before-backup positive" (> lsn-before-backup 0))

    (setq r (dbms-admin-backup-create))
    (assert-true "backup create ok" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq gen (third r))
    (assert-true "backup generation shape" (dbms-backup-generation-p gen))
    (setq gen-id (dbms-backup-generation-id gen))
    (assert-true "backup generation id non-empty" (and (stringp gen-id) (not (string= gen-id ""))))

    (setq r (dbms-exec-sql catalog "BEGIN;"))
    (assert-true "begin #3" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "INSERT INTO pages VALUES (3, 'v3');"))
    (assert-true "insert #3" (and (dbms-result-p r) (eq (second r) 'count)))
    (setq r (dbms-exec-sql catalog "COMMIT;"))
    (assert-true "commit #3" (and (dbms-result-p r) (eq (second r) 'ok)))

    (setq r (dbms-exec-sql catalog "BEGIN;"))
    (assert-true "begin #4" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "UPDATE pages SET title='v1x' WHERE id = 1;"))
    (assert-true "update #4" (and (dbms-result-p r) (eq (second r) 'count) (= (third r) 1)))
    (setq r (dbms-exec-sql catalog "COMMIT;"))
    (assert-true "commit #4" (and (dbms-result-p r) (eq (second r) 'ok)))

    (setq r (dbms-exec-sql catalog "SELECT id, title FROM pages ORDER BY id ASC;"))
    (assert-true "pre-pitr select rows" (and (dbms-result-p r) (eq (second r) 'rows)))
    (setq rows (result-rows r))
    (assert-true "pre-pitr rows=3" (= (length rows) 3))

    (setq r (dbms-admin-restore-pitr gen-id lsn-before-backup))
    (assert-true "restore pitr ok" (and (dbms-result-p r) (eq (second r) 'ok)))

    (setq r (dbms-exec-sql catalog "SELECT id, title FROM pages ORDER BY id ASC;"))
    (assert-true "post-pitr select rows" (and (dbms-result-p r) (eq (second r) 'rows)))
    (setq rows (result-rows r))
    (assert-true "post-pitr rows=2" (= (length rows) 2))
    (assert-true "post-pitr row1 id=1" (= (first (first rows)) 1))
    (assert-true "post-pitr row1 title=v1" (string= (second (first rows)) "v1"))
    (assert-true "post-pitr row2 id=2" (= (first (second rows)) 2))
    (assert-true "post-pitr row2 title=v2" (string= (second (second rows)) "v2"))

    (setq r (dbms-admin-backup-list))
    (assert-true "backup list ok" (and (dbms-result-p r) (eq (second r) 'ok)))
    (assert-true "backup list has generation" (>= (length (third r)) 1))

    (assert-true "backup index exists" (not (null (probe-file (dbms-storage-backup-index-path)))))
    (assert-true "backup wal exists" (not (null (probe-file (dbms-storage-backup-wal-path gen-id)))))
    (assert-true "backup dump exists" (not (null (probe-file (dbms-storage-backup-dump-path gen-id)))))

    (format t "dbms backup+pitr tests passed.~%")))

(catch 'dbms-assert-failed
  (run-tests))
