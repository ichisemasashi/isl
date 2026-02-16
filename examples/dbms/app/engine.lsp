;; dbms/app/engine.lsp
;; SQL実行エンジンの雛形（MVP Core）。

(load "./examples/dbms/app/repr.lsp")
(load "./examples/dbms/app/profile.lsp")
(load "./examples/dbms/app/catalog.lsp")
(load "./examples/dbms/app/sql_parser.lsp")

(defun dbms-engine-init ()
  (dbms-catalog-load))

(defun dbms-exec-sql (catalog sql-text)
  (let ((ast (dbms-parse-sql sql-text)))
    (if (dbms-error-p ast)
        (dbms-make-result 'error ast)
        ;; 以後のステップで AST 実行と結果契約を実装する。
        (dbms-make-result-error 'dbms/not-implemented "execution is not implemented yet" (list ast catalog)))))
