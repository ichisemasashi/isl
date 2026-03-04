;; dbms/tests/wiki_compat_v1/t10-wiki-grammar.lsp
;; Wiki Compatibility v1 の拡張文法受理テスト（parse only）。

(load "./examples/dbms/app/repr.lsp")
(load "./examples/dbms/app/sql_parser.lsp")

(defun assert-true (label cond)
  (if cond
      t
      (error (string-append "assert failed: " label))))

(defun first-stmt (ast)
  (first (dbms-ast-statements ast)))

(defun stmt-kind (stmt)
  (second stmt))

(defun run-tests ()
  (let* ((q-with (dbms-parse-sql
                  "WITH latest AS (SELECT id FROM pages LIMIT 1) SELECT id FROM latest;"))
         (q-join (dbms-parse-sql
                  "SELECT p.id, r.id FROM pages p INNER JOIN page_revisions r ON p.id = r.page_id WHERE p.id = 1 LIMIT 10;"))
         (q-subq (dbms-parse-sql
                  "SELECT id, (SELECT MAX(id) FROM page_revisions) FROM pages ORDER BY id DESC, id ASC LIMIT 5;"))
         (q-logic (dbms-parse-sql
                   "SELECT id FROM pages WHERE NOT (title IS NULL) AND (id = 1 OR id = 2);"))
         (q-ins-sel (dbms-parse-sql
                     "INSERT INTO pages_archive (id, title) SELECT id, title FROM pages WHERE id >= 1;"))
         (q-on-conflict (dbms-parse-sql
                         "INSERT INTO pages (id, title, published) VALUES (1, 'x', TRUE) ON CONFLICT (id) DO NOTHING;"))
         (q-upd-from-ret (dbms-parse-sql
                          "UPDATE pages p SET title = r.title FROM page_revisions r WHERE p.id = r.page_id RETURNING p.id, p.title;"))
         (q-begin (dbms-parse-sql "BEGIN;"))
         (q-commit (dbms-parse-sql "COMMIT;"))
         (q-rollback (dbms-parse-sql "ROLLBACK;")))

    (assert-true "WITH parses" (dbms-ast-p q-with))
    (assert-true "WITH stmt kind" (eq (stmt-kind (first-stmt q-with)) 'with))

    (assert-true "JOIN parses" (dbms-ast-p q-join))
    (assert-true "JOIN select-wiki kind" (eq (stmt-kind (first-stmt q-join)) 'select-wiki))

    (assert-true "subquery parses" (dbms-ast-p q-subq))
    (assert-true "subquery select-wiki kind" (eq (stmt-kind (first-stmt q-subq)) 'select-wiki))

    (assert-true "AND/OR/NOT/IS NULL parses" (dbms-ast-p q-logic))
    (assert-true "logic select-wiki kind" (eq (stmt-kind (first-stmt q-logic)) 'select-wiki))

    (assert-true "INSERT ... SELECT parses" (dbms-ast-p q-ins-sel))
    (assert-true "INSERT ... SELECT kind" (eq (stmt-kind (first-stmt q-ins-sel)) 'insert-wiki))

    (assert-true "ON CONFLICT parses" (dbms-ast-p q-on-conflict))
    (assert-true "ON CONFLICT kind" (eq (stmt-kind (first-stmt q-on-conflict)) 'insert-wiki))

    (assert-true "UPDATE ... FROM ... RETURNING parses" (dbms-ast-p q-upd-from-ret))
    (assert-true "UPDATE ... FROM ... RETURNING kind" (eq (stmt-kind (first-stmt q-upd-from-ret)) 'update-wiki))

    (assert-true "BEGIN parses" (dbms-ast-p q-begin))
    (assert-true "BEGIN kind" (eq (stmt-kind (first-stmt q-begin)) 'begin))
    (assert-true "COMMIT parses" (dbms-ast-p q-commit))
    (assert-true "COMMIT kind" (eq (stmt-kind (first-stmt q-commit)) 'commit))
    (assert-true "ROLLBACK parses" (dbms-ast-p q-rollback))
    (assert-true "ROLLBACK kind" (eq (stmt-kind (first-stmt q-rollback)) 'rollback))

    (format t "dbms wiki grammar tests passed.~%")))

(run-tests)
