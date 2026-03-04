;; dbms/tests/prod/t10-tx-state-management.lsp

(load "./examples/dbms/app/repr.lsp")
(load "./examples/dbms/app/engine.lsp")

(defun assert-true (label cond)
  (if cond
      t
      (progn
        (format t "assert failed: ~A~%" label)
        (throw 'dbms-assert-failed label))))

(defun string-in-list-p (s xs)
  (if (null xs)
      nil
      (if (string= s (car xs))
          t
          (string-in-list-p s (cdr xs)))))

(defun run-tests ()
  (let* ((root (string-append "/tmp/dbms-tx-state-" (format nil "~A" (get-universal-time))))
         (catalog '())
         (r '())
         (tx '())
         (read-set '())
         (write-set '()))
    (setenv "DBMS_STORAGE_ROOT" root)
    (setq catalog (dbms-engine-init))
    (dbms-engine-reset-tx-state)

    ;; idle 初期状態
    (setq tx (dbms-current-tx-state))
    (assert-true "tx state object" (dbms-tx-state-p tx))
    (assert-true "idle status" (eq (dbms-tx-state-status tx) 'idle))
    (assert-true "idle read-set empty" (null (dbms-tx-state-read-set tx)))
    (assert-true "idle write-set empty" (null (dbms-tx-state-write-set tx)))
    (assert-true "idle tx-id=0" (= (dbms-tx-state-tx-id tx) 0))

    ;; 準備
    (setq r (dbms-exec-sql catalog "CREATE TABLE pages (id INT PRIMARY KEY, title TEXT NOT NULL);"))
    (assert-true "create table ok" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "INSERT INTO pages VALUES (1, 'p1');"))
    (assert-true "seed insert ok" (and (dbms-result-p r) (eq (second r) 'count)))

    ;; BEGIN -> active
    (setq r (dbms-exec-sql catalog "BEGIN;"))
    (assert-true "begin ok" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq tx (dbms-current-tx-state))
    (assert-true "active status" (eq (dbms-tx-state-status tx) 'active))
    (assert-true "active tx-id positive" (> (dbms-tx-state-tx-id tx) 0))
    (assert-true "active read-set empty initially" (null (dbms-tx-state-read-set tx)))
    (assert-true "active write-set empty initially" (null (dbms-tx-state-write-set tx)))

    ;; SELECT -> read-set 記録
    (setq r (dbms-exec-sql catalog "SELECT * FROM pages;"))
    (assert-true "select ok" (and (dbms-result-p r) (eq (second r) 'rows)))
    (setq tx (dbms-current-tx-state))
    (setq read-set (dbms-tx-state-read-set tx))
    (setq write-set (dbms-tx-state-write-set tx))
    (assert-true "read-set includes pages" (string-in-list-p "pages" read-set))
    (assert-true "write-set still empty" (null write-set))

    ;; INSERT -> write-set 記録
    (setq r (dbms-exec-sql catalog "INSERT INTO pages VALUES (2, 'p2');"))
    (assert-true "insert in tx ok" (and (dbms-result-p r) (eq (second r) 'count)))
    (setq tx (dbms-current-tx-state))
    (setq read-set (dbms-tx-state-read-set tx))
    (setq write-set (dbms-tx-state-write-set tx))
    (assert-true "read-set keeps pages" (string-in-list-p "pages" read-set))
    (assert-true "write-set includes pages" (string-in-list-p "pages" write-set))

    ;; COMMIT -> idle
    (setq r (dbms-exec-sql catalog "COMMIT;"))
    (assert-true "commit ok" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq tx (dbms-current-tx-state))
    (assert-true "idle status after commit" (eq (dbms-tx-state-status tx) 'idle))
    (assert-true "read-set empty after commit" (null (dbms-tx-state-read-set tx)))
    (assert-true "write-set empty after commit" (null (dbms-tx-state-write-set tx)))

    (format t "dbms tx-state tests passed.~%")))

(catch 'dbms-assert-failed
  (run-tests))
