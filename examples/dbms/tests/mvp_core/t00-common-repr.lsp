;; dbms/tests/mvp_core/t00-common-repr.lsp

(load "./examples/dbms/app/repr.lsp")

(defun assert-true (label cond)
  (if cond
      t
      (error (string-append "assert failed: " label))))

(defun run-tests ()
  (let* ((col (dbms-make-column-def "id" 'INT '(PRIMARY-KEY NOT-NULL)))
         (table (dbms-make-table-def "pages" (list col) '() '()))
         (catalog (dbms-catalog-add-table-def (dbms-empty-catalog) table))
         (row (dbms-make-row 1 (list (list "id" 1))))
         (state (dbms-make-table-state "pages" (list row) 2))
         (ast (dbms-make-ast (list (dbms-make-stmt 'select '())) '()))
         (ok (dbms-make-result-ok 'ok))
         (cnt (dbms-make-result-count 3))
         (rows (dbms-make-result-rows '("id") (list row)))
         (err (dbms-make-result-error 'dbms/parse-error "bad sql" "SELECT")))
    (assert-true "column def" (dbms-column-def-p col))
    (assert-true "table def" (dbms-table-def-p table))
    (assert-true "catalog" (dbms-catalog-p catalog))
    (assert-true "find table" (dbms-table-def-p (dbms-catalog-find-table-def catalog "pages")))
    (assert-true "row" (dbms-row-p row))
    (assert-true "table state" (dbms-table-state-p state))
    (assert-true "ast" (dbms-ast-p ast))
    (assert-true "result ok" (dbms-result-p ok))
    (assert-true "result count" (dbms-result-p cnt))
    (assert-true "result rows" (dbms-result-p rows))
    (assert-true "result error" (dbms-result-p err))
    (assert-true "error code" (eq (dbms-error-code (third err)) 'dbms/parse-error))
    (format t "dbms common representation tests passed.~%")))

(run-tests)
