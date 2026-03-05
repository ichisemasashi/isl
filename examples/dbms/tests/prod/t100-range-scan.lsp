;; dbms/tests/prod/t100-range-scan.lsp
;; P3-004: range scan + ORDER BY/LIMIT integration.

(load "./examples/dbms/app/repr.lsp")
(load "./examples/dbms/app/engine.lsp")
(load "./examples/dbms/app/storage.lsp")

(defun assert-true (label cond)
  (if cond
      t
      (progn
        (format t "assert failed: ~A~%" label)
        (throw 'dbms-assert-failed label))))

(defun result-rows (r)
  (second (third r)))

(defun run-tests ()
  (let* ((root (string-append "/tmp/dbms-range-scan-" (format nil "~A" (get-universal-time))))
         (catalog '())
         (r '())
         (rows '())
         (idx '())
         (rids '()))
    (setenv "DBMS_STORAGE_ROOT" root)
    (setq catalog (dbms-engine-init))

    (setq r (dbms-exec-sql catalog "CREATE TABLE nums (id INT PRIMARY KEY, score INT NOT NULL, note TEXT);"))
    (assert-true "create nums" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "CREATE INDEX idx_nums_score ON nums(score);"))
    (assert-true "create score index" (and (dbms-result-p r) (eq (second r) 'ok)))

    (dolist (sql (list "INSERT INTO nums VALUES (1, 30, 'a');"
                       "INSERT INTO nums VALUES (2, 10, 'b');"
                       "INSERT INTO nums VALUES (3, 25, 'c');"
                       "INSERT INTO nums VALUES (4, 20, 'd');"
                       "INSERT INTO nums VALUES (5, 40, 'e');"
                       "INSERT INTO nums VALUES (6, 35, 'f');"))
      (setq r (dbms-exec-sql catalog sql))
      (assert-true "insert ok" (and (dbms-result-p r) (eq (second r) 'count))))

    ;; storage-level range scan (asc)
    (setq idx (dbms-storage-load-index "nums" "idx_nums_score"))
    (assert-true "load score index file" (dbms-btree-index-p idx))
    (setq rids (dbms-storage-btree-range-search idx 20 t 35 t 10))
    (assert-true "range search returns some ids" (>= (length rids) 4))

    ;; executor range + ORDER BY ASC + LIMIT
    (setq r (dbms-exec-sql catalog "SELECT id, score FROM nums WHERE score >= 20 ORDER BY score ASC LIMIT 3;"))
    (assert-true "range asc limit kind rows" (and (dbms-result-p r) (eq (second r) 'rows)))
    (setq rows (result-rows r))
    (assert-true "range asc limit count=3" (= (length rows) 3))
    (assert-true "range asc #1 score=20" (= (second (first rows)) 20))
    (assert-true "range asc #2 score=25" (= (second (second rows)) 25))
    (assert-true "range asc #3 score=30" (= (second (third rows)) 30))

    ;; executor range + ORDER BY DESC + LIMIT
    (setq r (dbms-exec-sql catalog "SELECT id, score FROM nums WHERE score < 35 ORDER BY score DESC LIMIT 2;"))
    (assert-true "range desc limit kind rows" (and (dbms-result-p r) (eq (second r) 'rows)))
    (setq rows (result-rows r))
    (assert-true "range desc limit count=2" (= (length rows) 2))
    (assert-true "range desc #1 score=30" (= (second (first rows)) 30))
    (assert-true "range desc #2 score=25" (= (second (second rows)) 25))

    ;; ORDER BY/LIMIT only (no WHERE) should still work with index path.
    (setq r (dbms-exec-sql catalog "SELECT id, score FROM nums ORDER BY score ASC LIMIT 2;"))
    (assert-true "orderby-only limit kind rows" (and (dbms-result-p r) (eq (second r) 'rows)))
    (setq rows (result-rows r))
    (assert-true "orderby-only count=2" (= (length rows) 2))
    (assert-true "orderby-only #1 score=10" (= (second (first rows)) 10))
    (assert-true "orderby-only #2 score=20" (= (second (second rows)) 20))

    (format t "dbms range scan tests passed.~%")))

(catch 'dbms-assert-failed
  (run-tests))
