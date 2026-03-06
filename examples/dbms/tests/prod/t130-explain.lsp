;; dbms/tests/prod/t130-explain.lsp
;; P3-007: EXPLAIN shows chosen plan and cost basis.

(load "./examples/dbms/app/repr.lsp")
(load "./examples/dbms/app/engine.lsp")

(defun assert-true (label cond)
  (if cond
      t
      (progn
        (format t "assert failed: ~A~%" label)
        (throw 'dbms-assert-failed label))))

(defun explain-item-value (rows key)
  (if (null rows)
      ""
      (if (string= (first (car rows)) key)
          (second (car rows))
          (explain-item-value (cdr rows) key))))

(defun run-tests ()
  (let* ((root (string-append "/tmp/dbms-explain-" (format nil "~A" (get-universal-time))))
         (catalog '())
         (r '())
         (rows '())
         (scan-choice "")
         (seq-cost "")
         (idx-cost ""))
    (setenv "DBMS_STORAGE_ROOT" root)
    (setq catalog (dbms-engine-init))

    (setq r (dbms-exec-sql catalog "CREATE TABLE t (id INT PRIMARY KEY, v INT);"))
    (assert-true "create table" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "CREATE INDEX idx_t_v ON t(v);"))
    (assert-true "create index" (and (dbms-result-p r) (eq (second r) 'ok)))

    (dolist (sql (list "INSERT INTO t VALUES (1, 1);"
                       "INSERT INTO t VALUES (2, 1);"
                       "INSERT INTO t VALUES (3, 2);"
                       "INSERT INTO t VALUES (4, 3);"
                       "INSERT INTO t VALUES (5, 100);"))
      (setq r (dbms-exec-sql catalog sql))
      (assert-true "insert ok" (and (dbms-result-p r) (eq (second r) 'count))))

    (setq r (dbms-exec-sql catalog "ANALYZE t;"))
    (assert-true "analyze ok" (and (dbms-result-p r) (eq (second r) 'ok)))

    (setq r (dbms-exec-sql catalog "EXPLAIN SELECT id, v FROM t WHERE v = 100 ORDER BY v ASC LIMIT 1;"))
    (assert-true "explain result rows" (and (dbms-result-p r) (eq (second r) 'rows)))
    (assert-true "explain columns" (equal (first (third r)) '("item" "value")))
    (setq rows (second (third r)))
    (setq scan-choice (explain-item-value rows "scan-choice"))
    (setq seq-cost (explain-item-value rows "seq-cost"))
    (setq idx-cost (explain-item-value rows "index-cost"))
    (assert-true "explain has scan-choice" (not (string= scan-choice "")))
    (assert-true "scan-choice index or seq" (or (string= scan-choice "INDEX") (string= scan-choice "SEQ")))
    (assert-true "explain has seq-cost" (numberp seq-cost))
    (assert-true "explain has index-cost" (numberp idx-cost))

    (format t "dbms explain tests passed.~%")))

(catch 'dbms-assert-failed
  (run-tests))
