;; dbms/app/sql_parser.lsp
;; SQLパーサの雛形（MVP Core）。

(load "./examples/dbms/app/repr.lsp")
(load "./examples/dbms/app/sql_lexer.lsp")

(defun dbms-parse-sql (sql-text)
  (if (or (null sql-text) (string= sql-text ""))
      (dbms-make-error 'dbms/parse-error "empty sql" sql-text)
      (let ((tokens (dbms-lex-sql sql-text)))
        ;; 以後のステップで AST へ変換する。
        (dbms-make-ast
         (list (dbms-make-stmt 'raw-sql (list (list "sql" sql-text) (list "tokens" tokens))))
         (list (list "profile" "MVP Core"))))))
