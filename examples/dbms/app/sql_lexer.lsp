;; dbms/app/sql_lexer.lsp
;; SQLトークナイザの雛形（MVP Core）。

(defun dbms-lex-sql (sql-text)
  ;; 以後のステップで字句解析を実装する。
  ;; 現時点は最小雛形として 1 トークン返却。
  (list (list 'raw sql-text)))
