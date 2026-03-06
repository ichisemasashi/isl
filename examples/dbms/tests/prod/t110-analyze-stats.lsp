;; dbms/tests/prod/t110-analyze-stats.lsp
;; P3-005: ANALYZE updates catalog stats (row-count, NDV, null fraction).

(load "./examples/dbms/app/repr.lsp")
(load "./examples/dbms/app/engine.lsp")

(defun assert-true (label cond)
  (if cond
      t
      (progn
        (format t "assert failed: ~A~%" label)
        (throw 'dbms-assert-failed label))))

(defun stats-of-table (catalog table-name)
  (let* ((td (dbms-catalog-find-table catalog table-name))
         (opts (if (null td) '() (dbms-table-def-options td))))
    (dbms-assoc-get opts "stats")))

(defun stats-row-count (stats)
  (dbms-assoc-get stats "row-count"))

(defun stats-columns (stats)
  (dbms-assoc-get stats "columns"))

(defun stats-col-entry (col-stats col-name)
  (if (null col-stats)
      '()
      (if (string= (dbms-assoc-get (car col-stats) "name") col-name)
          (car col-stats)
          (stats-col-entry (cdr col-stats) col-name))))

(defun run-tests ()
  (let* ((root (string-append "/tmp/dbms-analyze-" (format nil "~A" (get-universal-time))))
         (catalog '())
         (r '())
         (cat '())
         (s1 '())
         (s2 '())
         (cols '())
         (cval '()))
    (setenv "DBMS_STORAGE_ROOT" root)
    (setq catalog (dbms-engine-init))

    (setq r (dbms-exec-sql catalog "CREATE TABLE t1 (id INT PRIMARY KEY, v INT, s TEXT);"))
    (assert-true "create t1" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "CREATE TABLE t2 (id INT PRIMARY KEY, x INT);"))
    (assert-true "create t2" (and (dbms-result-p r) (eq (second r) 'ok)))

    (dbms-exec-sql catalog "INSERT INTO t1 VALUES (1, 10, 'a');")
    (dbms-exec-sql catalog "INSERT INTO t1 VALUES (2, 10, NULL);")
    (dbms-exec-sql catalog "INSERT INTO t1 VALUES (3, 20, 'b');")
    (dbms-exec-sql catalog "INSERT INTO t2 VALUES (1, 7);")

    ;; table-scoped analyze
    (setq r (dbms-exec-sql catalog "ANALYZE t1;"))
    (assert-true "analyze t1 ok" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq cat (dbms-engine-current-catalog))
    (setq s1 (stats-of-table cat "t1"))
    (setq s2 (stats-of-table cat "t2"))
    (assert-true "t1 stats exists" (listp s1))
    (assert-true "t2 stats not yet set" (null s2))
    (assert-true "t1 row-count=3" (= (stats-row-count s1) 3))
    (setq cols (stats-columns s1))
    (setq cval (stats-col-entry cols "v"))
    (assert-true "t1.v ndv=2" (= (dbms-assoc-get cval "ndv") 2))
    (setq cval (stats-col-entry cols "s"))
    (assert-true "t1.s null-fraction=1/3" (equal (dbms-assoc-get cval "null-fraction") 1/3))

    ;; global analyze
    (setq r (dbms-exec-sql catalog "ANALYZE;"))
    (assert-true "analyze all ok" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq cat (dbms-engine-current-catalog))
    (setq s2 (stats-of-table cat "t2"))
    (assert-true "t2 stats exists after analyze all" (listp s2))
    (assert-true "t2 row-count=1" (= (stats-row-count s2) 1))

    (format t "dbms analyze stats tests passed.~%")))

(catch 'dbms-assert-failed
  (run-tests))
