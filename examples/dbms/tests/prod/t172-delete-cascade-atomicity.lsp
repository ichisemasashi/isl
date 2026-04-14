;; dbms/tests/prod/t172-delete-cascade-atomicity.lsp
;; DELETE + FK propagation must validate child changes before parent rows are persisted.

(load "./examples/dbms/app/repr.lsp")
(load "./examples/dbms/app/engine.lsp")

(defun assert-true (label cond)
  (if cond
      t
      (progn
        (format t "assert failed: ~A~%" label)
        (throw 'dbms-assert-failed label))))

(defun result-rows (result)
  (second (third result)))

(defun run-tests ()
  (let* ((root-base (string-append "/tmp/dbms-delete-atomicity-" (format nil "~A" (get-universal-time))))
         (root root-base)
         (suffix 0)
         (catalog '())
         (r '())
         (rows '()))
    (while (not (null (probe-file root)))
      (setq suffix (+ suffix 1))
      (setq root (string-append root-base "-" (format nil "~A" suffix))))
    (setenv "DBMS_STORAGE_ROOT" root)

    (setq catalog (dbms-engine-init))

    (setq r (dbms-exec-sql catalog "CREATE TABLE parents (id INT PRIMARY KEY, name TEXT NOT NULL);"))
    (assert-true "create parents" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "CREATE TABLE child_nn (id INT PRIMARY KEY, parent_id INT NOT NULL, v TEXT);"))
    (assert-true "create child_nn" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "ALTER TABLE child_nn ADD CONSTRAINT fk_child_nn FOREIGN KEY (parent_id) REFERENCES parents(id) ON DELETE SET NULL;"))
    (assert-true "add fk set null" (and (dbms-result-p r) (eq (second r) 'ok)))

    (setq r (dbms-exec-sql catalog "INSERT INTO parents VALUES (1, 'p1');"))
    (assert-true "insert parent" (and (dbms-result-p r) (eq (second r) 'count) (= (third r) 1)))
    (setq r (dbms-exec-sql catalog "INSERT INTO child_nn VALUES (10, 1, 'c10');"))
    (assert-true "insert child" (and (dbms-result-p r) (eq (second r) 'count) (= (third r) 1)))

    (setq r (dbms-exec-sql catalog "DELETE FROM parents WHERE id = 1;"))
    (assert-true "delete rejected" (and (dbms-result-p r) (eq (second r) 'error)))

    (setq r (dbms-exec-sql catalog "SELECT id, name FROM parents ORDER BY id ASC;"))
    (assert-true "select parents kind" (and (dbms-result-p r) (eq (second r) 'rows)))
    (setq rows (result-rows r))
    (assert-true "parent row remains after failed delete" (= (length rows) 1))
    (assert-true "parent id still 1" (= (first (first rows)) 1))

    (setq r (dbms-exec-sql catalog "SELECT id, parent_id, v FROM child_nn ORDER BY id ASC;"))
    (assert-true "select child kind" (and (dbms-result-p r) (eq (second r) 'rows)))
    (setq rows (result-rows r))
    (assert-true "child row remains after failed delete" (= (length rows) 1))
    (assert-true "child parent_id unchanged" (= (second (first rows)) 1))

    (format t "dbms delete cascade atomicity tests passed.~%")))

(catch 'dbms-assert-failed
  (run-tests))
