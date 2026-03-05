;; dbms/tests/prod/t98-btree-basic.lsp
;; P3-002: B+Tree basic insert/search integration.

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
  (let* ((root (string-append "/tmp/dbms-btree-basic-" (format nil "~A" (get-universal-time))))
         (catalog '())
         (r '())
         (rows '())
         (idx '())
         (hits '()))
    (setenv "DBMS_STORAGE_ROOT" root)
    (setq catalog (dbms-engine-init))

    (setq r (dbms-exec-sql catalog "CREATE TABLE items (id INT PRIMARY KEY, name TEXT NOT NULL);"))
    (assert-true "create table" (and (dbms-result-p r) (eq (second r) 'ok)))

    (setq r (dbms-exec-sql catalog "INSERT INTO items VALUES (1, 'a');"))
    (assert-true "insert #1" (and (dbms-result-p r) (eq (second r) 'count) (= (third r) 1)))
    (setq r (dbms-exec-sql catalog "INSERT INTO items VALUES (2, 'b');"))
    (assert-true "insert #2" (and (dbms-result-p r) (eq (second r) 'count) (= (third r) 1)))
    (setq r (dbms-exec-sql catalog "INSERT INTO items VALUES (3, 'c');"))
    (assert-true "insert #3" (and (dbms-result-p r) (eq (second r) 'count) (= (third r) 1)))

    (setq r (dbms-exec-sql catalog "CREATE INDEX idx_items_name ON items(name);"))
    (assert-true "create index single-column" (and (dbms-result-p r) (eq (second r) 'ok)))

    (setq idx (dbms-storage-load-index "items" "idx_items_name"))
    (assert-true "index file exists" (dbms-btree-index-p idx))
    (setq hits (dbms-storage-btree-search idx "c"))
    (assert-true "index search c has one rid" (= (length hits) 1))

    (setq r (dbms-exec-sql catalog "SELECT id, name FROM items WHERE name = 'c' ORDER BY id ASC;"))
    (assert-true "select c one row" (and (dbms-result-p r) (eq (second r) 'rows) (= (length (result-rows r)) 1)))
    (setq rows (result-rows r))
    (assert-true "select c id=3" (= (first (first rows)) 3))

    ;; duplicate key path
    (setq r (dbms-exec-sql catalog "INSERT INTO items VALUES (4, 'c');"))
    (assert-true "insert duplicate key value" (and (dbms-result-p r) (eq (second r) 'count)))
    (setq r (dbms-exec-sql catalog "SELECT id FROM items WHERE name = 'c' ORDER BY id ASC;"))
    (assert-true "select c two rows after insert" (and (dbms-result-p r) (eq (second r) 'rows) (= (length (result-rows r)) 2)))

    ;; update path should rebuild index
    (setq r (dbms-exec-sql catalog "UPDATE items SET name='x' WHERE id = 3;"))
    (assert-true "update one row" (and (dbms-result-p r) (eq (second r) 'count) (= (third r) 1)))
    (setq r (dbms-exec-sql catalog "SELECT id FROM items WHERE name = 'c' ORDER BY id ASC;"))
    (assert-true "select c one row after update" (and (dbms-result-p r) (eq (second r) 'rows) (= (length (result-rows r)) 1)))

    ;; delete path should rebuild index
    (setq r (dbms-exec-sql catalog "DELETE FROM items WHERE id = 4;"))
    (assert-true "delete one row" (and (dbms-result-p r) (eq (second r) 'count) (= (third r) 1)))
    (setq r (dbms-exec-sql catalog "SELECT id FROM items WHERE name = 'c' ORDER BY id ASC;"))
    (assert-true "select c zero row after delete" (and (dbms-result-p r) (eq (second r) 'rows) (= (length (result-rows r)) 0)))

    (format t "dbms btree basic tests passed.~%")))

(catch 'dbms-assert-failed
  (run-tests))
