;; dbms/app/sql_parser.lsp
;; SQLパーサの雛形（MVP Core）。

(load "./examples/dbms/app/sql_lexer.lsp")

(defun dbms-parse-sql (sql-text)
  (let ((tokens (dbms-lex-sql sql-text)))
    ;; 以後のステップで AST へ変換する。
    (list 'ast 'unimplemented tokens)))
