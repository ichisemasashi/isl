;; dbms/tests/prod/t280-optimizer-join-complex.lsp
;; MT-003: optimizer improvements for JOIN and complex predicates.

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
      '()
      (if (string= (first (car rows)) key)
          (second (car rows))
          (explain-item-value (cdr rows) key))))

(defun seed-n (catalog table-name n)
  (let ((i 1)
        (r '()))
    (while (<= i n)
      (setq r (dbms-exec-sql catalog
                             (format nil "INSERT INTO ~A VALUES (~A, 'v~A');" table-name i i)))
      (if (and (dbms-result-p r) (eq (second r) 'count) (= (third r) 1))
          nil
          (throw 'dbms-assert-failed (string-append "seed failed: " table-name)))
      (setq i (+ i 1)))))

(defun run-tests ()
  (let* ((root-base (string-append "/tmp/dbms-opt-join-" (format nil "~A" (get-universal-time))))
         (root root-base)
         (suffix 0)
         (catalog '())
         (r '())
         (rows '())
         (join-count 0)
         (naive-order '())
         (optimized-order '())
         (chosen-order '())
         (naive-cost 0)
         (optimized-cost 0)
         (where-score 0)
         (order-items 0))
    (while (not (null (probe-file root)))
      (setq suffix (+ suffix 1))
      (setq root (string-append root-base "-" (format nil "~A" suffix))))
    (setenv "DBMS_STORAGE_ROOT" root)
    (setq catalog (dbms-engine-init))
    (dbms-engine-reset-tx-state)

    (setq r (dbms-exec-sql catalog "CREATE TABLE a (id INT PRIMARY KEY, v TEXT NOT NULL);"))
    (assert-true "create a" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "CREATE TABLE b (id INT PRIMARY KEY, v TEXT NOT NULL);"))
    (assert-true "create b" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "CREATE TABLE c (id INT PRIMARY KEY, v TEXT NOT NULL);"))
    (assert-true "create c" (and (dbms-result-p r) (eq (second r) 'ok)))

    ;; Cardinalities: a=24, b=4, c=12 -> optimizer should prefer b-first order.
    (seed-n catalog "a" 24)
    (seed-n catalog "b" 4)
    (seed-n catalog "c" 12)

    ;; Stats-aware path
    (setq r (dbms-exec-sql catalog "ANALYZE a;"))
    (assert-true "analyze a" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "ANALYZE b;"))
    (assert-true "analyze b" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "ANALYZE c;"))
    (assert-true "analyze c" (and (dbms-result-p r) (eq (second r) 'ok)))

    (setq r (dbms-exec-sql catalog
                           "EXPLAIN SELECT a.id, b.id, c.id FROM a JOIN b ON a.id = b.id JOIN c ON c.id = a.id WHERE (a.id = 1 AND b.id = 1) OR (c.id = 1) ORDER BY a.id DESC, b.id ASC LIMIT 10;"))
    (assert-true "explain rows" (and (dbms-result-p r) (eq (second r) 'rows)))

    (setq rows (second (third r)))
    (assert-true "query-kind select-wiki" (string= (explain-item-value rows "query-kind") "select-wiki"))

    (setq join-count (explain-item-value rows "join-count"))
    (setq naive-order (explain-item-value rows "naive-order"))
    (setq optimized-order (explain-item-value rows "optimized-order"))
    (setq chosen-order (explain-item-value rows "chosen-order"))
    (setq naive-cost (explain-item-value rows "join-cost-naive"))
    (setq optimized-cost (explain-item-value rows "join-cost-optimized"))
    (setq where-score (explain-item-value rows "where-complexity"))
    (setq order-items (explain-item-value rows "order-by-items"))

    (assert-true "join-count=2" (= join-count 2))
    (assert-true "naive starts with a" (string= (first naive-order) "a"))
    (assert-true "optimized starts with b" (string= (first optimized-order) "b"))
    (assert-true "chosen starts with b" (string= (first chosen-order) "b"))
    (assert-true "optimized cost <= naive cost" (<= optimized-cost naive-cost))
    (assert-true "where complexity positive" (> where-score 0))
    (assert-true "order-by items detected" (>= order-items 1))

    (format t "dbms optimizer join/complex tests passed.~%")))

(catch 'dbms-assert-failed
  (run-tests))
