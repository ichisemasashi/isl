;; dbms/app/engine.lsp
;; SQL実行エンジンの雛形（MVP Core）。

(load "./examples/dbms/app/profile.lsp")
(load "./examples/dbms/app/catalog.lsp")
(load "./examples/dbms/app/sql_parser.lsp")

(defun dbms-engine-init ()
  (dbms-catalog-load))

(defun dbms-exec-sql (catalog sql-text)
  (let ((ast (dbms-parse-sql sql-text)))
    ;; 以後のステップで AST 実行と結果契約を実装する。
    (list 'error 'dbms/not-implemented ast catalog)))
