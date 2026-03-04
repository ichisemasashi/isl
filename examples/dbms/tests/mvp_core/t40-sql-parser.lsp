;; dbms/tests/mvp_core/t40-sql-parser.lsp

(load "./examples/dbms/app/repr.lsp")
(load "./examples/dbms/app/sql_parser.lsp")

(defun assert-true (label cond)
  (if cond
      t
      (error (string-append "assert failed: " label))))

(defun first-stmt (ast)
  (first (second ast)))

(defun stmt-kind (stmt) (second stmt))
(defun stmt-payload (stmt) (third stmt))

(defun payload-get (payload key)
  (if (null payload)
      '()
      (if (string= (first (first payload)) key)
          (second (first payload))
          (payload-get (cdr payload) key))))

(defun run-tests ()
  (let* ((ast-create (dbms-parse-sql "CREATE TABLE pages (id INT PRIMARY KEY, title TEXT NOT NULL, published BOOL);"))
         (ast-insert-a (dbms-parse-sql "INSERT INTO pages VALUES (1, 'hello', TRUE);"))
         (ast-insert-b (dbms-parse-sql "INSERT INTO pages (id, title, published) VALUES (2, 'world', FALSE);"))
         (ast-select (dbms-parse-sql "SELECT id, title FROM pages WHERE id >= 1 ORDER BY title DESC;"))
         (ast-update (dbms-parse-sql "UPDATE pages SET title='new', published=FALSE WHERE id = 1;"))
         (ast-delete (dbms-parse-sql "DELETE FROM pages WHERE id != 2;"))
         (err (dbms-parse-sql "SELECT FROM pages;")))

    (assert-true "create parses" (dbms-ast-p ast-create))
    (assert-true "create stmt kind"
                 (eq (stmt-kind (first-stmt ast-create)) 'create-table))
    (assert-true "create payload table-def"
                 (dbms-table-def-p (stmt-payload (first-stmt ast-create))))
    (assert-true "create columns count"
                 (= (length (dbms-table-def-columns (stmt-payload (first-stmt ast-create)))) 3))

    (assert-true "insert A parses" (dbms-ast-p ast-insert-a))
    (assert-true "insert A kind" (eq (stmt-kind (first-stmt ast-insert-a)) 'insert))
    (assert-true "insert A no column list"
                 (null (payload-get (stmt-payload (first-stmt ast-insert-a)) "columns")))
    (assert-true "insert A values count"
                 (= (length (payload-get (stmt-payload (first-stmt ast-insert-a)) "values")) 3))

    (assert-true "insert B parses" (dbms-ast-p ast-insert-b))
    (assert-true "insert B explicit columns"
                 (= (length (payload-get (stmt-payload (first-stmt ast-insert-b)) "columns")) 3))

    (assert-true "select parses" (dbms-ast-p ast-select))
    (assert-true "select kind" (eq (stmt-kind (first-stmt ast-select)) 'select))
    (assert-true "select where present"
                 (not (null (payload-get (stmt-payload (first-stmt ast-select)) "where"))))
    (assert-true "select order-by present"
                 (not (null (payload-get (stmt-payload (first-stmt ast-select)) "order-by"))))

    (assert-true "update parses" (dbms-ast-p ast-update))
    (assert-true "update kind" (eq (stmt-kind (first-stmt ast-update)) 'update))
    (assert-true "update set count"
                 (= (length (payload-get (stmt-payload (first-stmt ast-update)) "set")) 2))

    (assert-true "delete parses" (dbms-ast-p ast-delete))
    (assert-true "delete kind" (eq (stmt-kind (first-stmt ast-delete)) 'delete))
    (assert-true "delete where present"
                 (not (null (payload-get (stmt-payload (first-stmt ast-delete)) "where"))))

    (assert-true "invalid SQL parse error" (dbms-error-p err))
    (assert-true "invalid SQL error code" (eq (dbms-error-code err) 'dbms/parse-error))

    (format t "dbms sql parser tests passed.~%")))

(run-tests)
