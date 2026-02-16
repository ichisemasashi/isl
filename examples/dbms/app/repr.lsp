;; dbms/app/repr.lsp
;; DBMS内部の共通データ表現。

(defglobal *dbms-repr-version* 1)

(defglobal *dbms-error-codes*
  '(dbms/parse-error
    dbms/table-not-found
    dbms/column-not-found
    dbms/duplicate-table
    dbms/duplicate-column
    dbms/type-mismatch
    dbms/not-null-violation
    dbms/primary-key-violation
    dbms/arity-mismatch
    dbms/not-implemented
    dbms/no-input
    dbms/invalid-representation))

(defun dbms-member-eq (x xs)
  (if (null xs)
      nil
      (if (eq x (car xs))
          t
          (dbms-member-eq x (cdr xs)))))

(defun dbms-tagged-p (v tag)
  (and (listp v)
       (not (null v))
       (eq (car v) tag)))

(defun dbms-error-code-valid-p (code)
  (dbms-member-eq code *dbms-error-codes*))

(defun dbms-make-error (code message detail)
  (list 'dbms-error
        (if (dbms-error-code-valid-p code) code 'dbms/invalid-representation)
        message
        detail))

(defun dbms-error-p (v)
  (dbms-tagged-p v 'dbms-error))

(defun dbms-error-code (err)
  (if (dbms-error-p err)
      (second err)
      'dbms/invalid-representation))

(defun dbms-make-result (kind payload)
  (list 'dbms-result kind payload))

(defun dbms-result-p (v)
  (dbms-tagged-p v 'dbms-result))

(defun dbms-make-result-ok (payload)
  (dbms-make-result 'ok payload))

(defun dbms-make-result-count (count)
  (dbms-make-result 'count count))

(defun dbms-make-result-rows (columns rows)
  (dbms-make-result 'rows (list columns rows)))

(defun dbms-make-result-error (code message detail)
  (dbms-make-result 'error (dbms-make-error code message detail)))

(defun dbms-make-ast (statements meta)
  (list 'dbms-ast statements meta))

(defun dbms-ast-p (v)
  (dbms-tagged-p v 'dbms-ast))

(defun dbms-ast-statements (ast)
  (if (dbms-ast-p ast)
      (second ast)
      '()))

(defun dbms-make-stmt (kind payload)
  (list 'dbms-stmt kind payload))

(defun dbms-stmt-p (v)
  (dbms-tagged-p v 'dbms-stmt))

(defun dbms-make-column-def (name type attrs)
  (list 'dbms-column name type attrs))

(defun dbms-column-def-p (v)
  (dbms-tagged-p v 'dbms-column))

(defun dbms-make-table-def (name columns constraints options)
  (list 'dbms-table-def name columns constraints options))

(defun dbms-table-def-p (v)
  (dbms-tagged-p v 'dbms-table-def))

(defun dbms-table-def-name (table-def)
  (if (dbms-table-def-p table-def)
      (second table-def)
      ""))

(defun dbms-table-def-columns (table-def)
  (if (dbms-table-def-p table-def)
      (third table-def)
      '()))

(defun dbms-make-row (row-id values)
  ;; values: ((column-name value) ...)
  (list 'dbms-row row-id values))

(defun dbms-row-p (v)
  (dbms-tagged-p v 'dbms-row))

(defun dbms-make-table-state (table-name rows next-row-id)
  (list 'dbms-table-state table-name rows next-row-id))

(defun dbms-table-state-p (v)
  (dbms-tagged-p v 'dbms-table-state))

(defun dbms-empty-catalog ()
  ;; tables: ((table-name table-def) ...)
  (list 'dbms-catalog *dbms-repr-version* '()))

(defun dbms-catalog-p (v)
  (dbms-tagged-p v 'dbms-catalog))

(defun dbms-catalog-tables (catalog)
  (if (dbms-catalog-p catalog)
      (third catalog)
      '()))

(defun dbms-find-table-pair (table-name table-pairs)
  (if (null table-pairs)
      '()
      (let ((pair (car table-pairs)))
        (if (and (listp pair)
                 (= (length pair) 2)
                 (string= (first pair) table-name))
            pair
            (dbms-find-table-pair table-name (cdr table-pairs))))))

(defun dbms-catalog-find-table-def (catalog table-name)
  (let ((pair (dbms-find-table-pair table-name (dbms-catalog-tables catalog))))
    (if (null pair)
        '()
        (second pair))))

(defun dbms-catalog-add-table-def (catalog table-def)
  (let ((name (dbms-table-def-name table-def))
        (pairs (dbms-catalog-tables catalog)))
    (list 'dbms-catalog
          *dbms-repr-version*
          (cons (list name table-def) pairs))))
