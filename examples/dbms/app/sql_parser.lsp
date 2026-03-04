;; dbms/app/sql_parser.lsp
;; SQLパーサ（MVP Core）。

(load "./examples/dbms/app/repr.lsp")
(load "./examples/dbms/app/sql_lexer.lsp")

(defun dbms-parser-error (message detail)
  (dbms-make-error 'dbms/parse-error message detail))

(defun dbms-parser-ok (value rest)
  (list value rest))

(defun dbms-parser-ok-value (r) (first r))
(defun dbms-parser-ok-rest (r) (second r))

(defun dbms-token-is-kind-p (tok kind)
  (and (not (null tok))
       (eq (dbms-sql-token-kind tok) kind)))

(defun dbms-token-is-keyword-p (tok kw-upper)
  (and (dbms-token-is-kind-p tok 'keyword)
       (string= (fourth tok) kw-upper)))

(defun dbms-token-is-symbol-p (tok sym)
  (and (dbms-token-is-kind-p tok 'symbol)
       (string= (dbms-sql-token-lexeme tok) sym)))

(defun dbms-token-is-operator-p (tok op)
  (and (dbms-token-is-kind-p tok 'operator)
       (string= (dbms-sql-token-lexeme tok) op)))

(defun dbms-expect-keyword (tokens kw-upper)
  (if (or (null tokens) (not (dbms-token-is-keyword-p (first tokens) kw-upper)))
      (dbms-parser-error
       (string-append "expected keyword " kw-upper)
       (if (null tokens) 'eof (dbms-sql-token-lexeme (first tokens))))
      (dbms-parser-ok (first tokens) (cdr tokens))))

(defun dbms-expect-symbol (tokens sym)
  (if (or (null tokens) (not (dbms-token-is-symbol-p (first tokens) sym)))
      (dbms-parser-error
       (string-append "expected symbol " sym)
       (if (null tokens) 'eof (dbms-sql-token-lexeme (first tokens))))
      (dbms-parser-ok (first tokens) (cdr tokens))))

(defun dbms-expect-operator (tokens op)
  (if (or (null tokens) (not (dbms-token-is-operator-p (first tokens) op)))
      (dbms-parser-error
       (string-append "expected operator " op)
       (if (null tokens) 'eof (dbms-sql-token-lexeme (first tokens))))
      (dbms-parser-ok (first tokens) (cdr tokens))))

(defun dbms-expect-identifier (tokens)
  (if (or (null tokens) (not (dbms-token-is-kind-p (first tokens) 'identifier)))
      (dbms-parser-error
       "expected identifier"
       (if (null tokens) 'eof (dbms-sql-token-lexeme (first tokens))))
      (dbms-parser-ok (fourth (first tokens)) (cdr tokens))))

(defun dbms-expect-null-word (tokens)
  (if (null tokens)
      (dbms-parser-error "expected NULL" 'eof)
      (if (or (dbms-token-is-keyword-p (first tokens) "NULL")
              (dbms-token-is-kind-p (first tokens) 'literal-null))
          (dbms-parser-ok (first tokens) (cdr tokens))
          (dbms-parser-error "expected NULL" (dbms-sql-token-lexeme (first tokens))))))

(defun dbms-parse-type-symbol (tokens)
  (if (null tokens)
      (dbms-parser-error "expected type" 'eof)
      (let ((tok (first tokens)))
        (if (dbms-token-is-keyword-p tok "INT")
            (dbms-parser-ok 'INT (cdr tokens))
            (if (dbms-token-is-keyword-p tok "TEXT")
                (dbms-parser-ok 'TEXT (cdr tokens))
                (if (dbms-token-is-keyword-p tok "BOOL")
                    (dbms-parser-ok 'BOOL (cdr tokens))
                    (dbms-parser-error "expected type INT/TEXT/BOOL"
                                       (dbms-sql-token-lexeme tok))))))))

(defun dbms-parse-expr (tokens)
  (if (null tokens)
      (dbms-parser-error "expected expression" 'eof)
      (let ((tok (first tokens)))
        (if (dbms-token-is-kind-p tok 'number)
            (dbms-parser-ok (list 'dbms-expr 'literal 'INT (fourth tok)) (cdr tokens))
            (if (dbms-token-is-kind-p tok 'string)
                (dbms-parser-ok (list 'dbms-expr 'literal 'TEXT (fourth tok)) (cdr tokens))
                (if (dbms-token-is-kind-p tok 'literal-bool)
                    (dbms-parser-ok (list 'dbms-expr 'literal 'BOOL (fourth tok)) (cdr tokens))
                    (if (dbms-token-is-kind-p tok 'literal-null)
                        (dbms-parser-ok (list 'dbms-expr 'literal 'NULL 'NULL) (cdr tokens))
                        (if (dbms-token-is-kind-p tok 'identifier)
                            (dbms-parser-ok (list 'dbms-expr 'identifier (fourth tok)) (cdr tokens))
                            (dbms-parser-error "expected expression literal/identifier"
                                               (dbms-sql-token-lexeme tok))))))))))

(defun dbms-operator-compare-p (op)
  (or (string= op "=")
      (string= op "!=")
      (string= op "<")
      (string= op "<=")
      (string= op ">")
      (string= op ">=")
      (string= op "<>")))

(defun dbms-parse-predicate (tokens)
  (let ((r-col (dbms-expect-identifier tokens)))
    (if (dbms-error-p r-col)
        r-col
        (let ((col (dbms-parser-ok-value r-col))
              (rest1 (dbms-parser-ok-rest r-col)))
          (if (or (null rest1)
                  (not (dbms-token-is-kind-p (first rest1) 'operator))
                  (not (dbms-operator-compare-p (dbms-sql-token-lexeme (first rest1)))))
              (dbms-parser-error "expected comparison operator"
                                 (if (null rest1) 'eof (dbms-sql-token-lexeme (first rest1))))
              (let* ((op (dbms-sql-token-lexeme (first rest1)))
                     (r-expr (dbms-parse-expr (cdr rest1))))
                (if (dbms-error-p r-expr)
                    r-expr
                    (dbms-parser-ok
                     (list 'dbms-predicate col op (dbms-parser-ok-value r-expr))
                     (dbms-parser-ok-rest r-expr)))))))))

(defun dbms-parse-identifier-list (tokens)
  (let ((r-first (dbms-expect-identifier tokens)))
    (if (dbms-error-p r-first)
        r-first
        (let ((values (list (dbms-parser-ok-value r-first)))
              (rest (dbms-parser-ok-rest r-first))
              (failed '())
              (done nil))
          (while (not done)
            (if (and (not (null rest)) (dbms-token-is-symbol-p (first rest) ","))
                (let ((r-next (dbms-expect-identifier (cdr rest))))
                  (if (dbms-error-p r-next)
                      (progn
                        (setq failed r-next)
                        (setq done t))
                      (progn
                        (setq values (append values (list (dbms-parser-ok-value r-next))))
                        (setq rest (dbms-parser-ok-rest r-next)))))
                (setq done t)))
          (if (dbms-error-p failed)
              failed
              (dbms-parser-ok values rest))))))

(defun dbms-parse-expr-list (tokens)
  (let ((r-first (dbms-parse-expr tokens)))
    (if (dbms-error-p r-first)
        r-first
        (let ((values (list (dbms-parser-ok-value r-first)))
              (rest (dbms-parser-ok-rest r-first))
              (failed '())
              (done nil))
          (while (not done)
            (if (and (not (null rest)) (dbms-token-is-symbol-p (first rest) ","))
                (let ((r-next (dbms-parse-expr (cdr rest))))
                  (if (dbms-error-p r-next)
                      (progn
                        (setq failed r-next)
                        (setq done t))
                      (progn
                        (setq values (append values (list (dbms-parser-ok-value r-next))))
                        (setq rest (dbms-parser-ok-rest r-next)))))
                (setq done t)))
          (if (dbms-error-p failed)
              failed
              (dbms-parser-ok values rest))))))

(defun dbms-parse-column-def (tokens)
  (let ((r-name (dbms-expect-identifier tokens)))
    (if (dbms-error-p r-name)
        r-name
        (let ((name (dbms-parser-ok-value r-name))
              (rest1 (dbms-parser-ok-rest r-name)))
          (let ((r-type (dbms-parse-type-symbol rest1)))
            (if (dbms-error-p r-type)
                r-type
                (let ((attrs '())
                      (rest (dbms-parser-ok-rest r-type))
                      (failed '())
                      (done nil))
                  (while (not done)
                    (if (and (not (null rest))
                             (dbms-token-is-keyword-p (first rest) "PRIMARY"))
                        (let ((r-key (dbms-expect-keyword (cdr rest) "KEY")))
                          (if (dbms-error-p r-key)
                              (progn
                                (setq failed r-key)
                                (setq done t))
                              (progn
                                (setq attrs (append attrs '(PRIMARY-KEY)))
                                (setq rest (dbms-parser-ok-rest r-key)))))
                        (if (and (not (null rest))
                                 (dbms-token-is-keyword-p (first rest) "NOT"))
                            (let ((r-null (dbms-expect-null-word (cdr rest))))
                              (if (dbms-error-p r-null)
                                  (progn
                                    (setq failed r-null)
                                    (setq done t))
                                  (progn
                                    (setq attrs (append attrs '(NOT-NULL)))
                                    (setq rest (dbms-parser-ok-rest r-null)))))
                            (setq done t))))
                  (if (dbms-error-p failed)
                      failed
                      (dbms-parser-ok
                       (dbms-make-column-def name (dbms-parser-ok-value r-type) attrs)
                       rest)))))))))

(defun dbms-parse-column-def-list (tokens)
  (let ((r-first (dbms-parse-column-def tokens)))
    (if (dbms-error-p r-first)
        r-first
        (let ((values (list (dbms-parser-ok-value r-first)))
              (rest (dbms-parser-ok-rest r-first))
              (failed '())
              (done nil))
          (while (not done)
            (if (and (not (null rest)) (dbms-token-is-symbol-p (first rest) ","))
                (let ((r-next (dbms-parse-column-def (cdr rest))))
                  (if (dbms-error-p r-next)
                      (progn
                        (setq failed r-next)
                        (setq done t))
                      (progn
                        (setq values (append values (list (dbms-parser-ok-value r-next))))
                        (setq rest (dbms-parser-ok-rest r-next)))))
                (setq done t)))
          (if (dbms-error-p failed)
              failed
              (dbms-parser-ok values rest))))))

(defun dbms-parse-set-assignments (tokens)
  (let ((r-col (dbms-expect-identifier tokens)))
    (if (dbms-error-p r-col)
        r-col
        (let ((r-eq (dbms-expect-operator (dbms-parser-ok-rest r-col) "=")))
          (if (dbms-error-p r-eq)
              r-eq
              (let ((r-expr (dbms-parse-expr (dbms-parser-ok-rest r-eq))))
                (if (dbms-error-p r-expr)
                    r-expr
                    (let ((pairs (list (list (dbms-parser-ok-value r-col) (dbms-parser-ok-value r-expr))))
                          (rest (dbms-parser-ok-rest r-expr))
                          (failed '())
                          (done nil))
                      (while (not done)
                        (if (and (not (null rest)) (dbms-token-is-symbol-p (first rest) ","))
                            (let ((r-col2 (dbms-expect-identifier (cdr rest))))
                              (if (dbms-error-p r-col2)
                                  (progn
                                    (setq failed r-col2)
                                    (setq done t))
                                  (let ((r-eq2 (dbms-expect-operator (dbms-parser-ok-rest r-col2) "=")))
                                    (if (dbms-error-p r-eq2)
                                        (progn
                                          (setq failed r-eq2)
                                          (setq done t))
                                        (let ((r-expr2 (dbms-parse-expr (dbms-parser-ok-rest r-eq2))))
                                          (if (dbms-error-p r-expr2)
                                              (progn
                                                (setq failed r-expr2)
                                                (setq done t))
                                              (progn
                                                (setq pairs (append pairs (list (list (dbms-parser-ok-value r-col2)
                                                                                      (dbms-parser-ok-value r-expr2)))))
                                                (setq rest (dbms-parser-ok-rest r-expr2)))))))))
                            (setq done t)))
                      (if (dbms-error-p failed)
                          failed
                          (dbms-parser-ok pairs rest))))))))))

(defun dbms-parse-create-table (tokens)
  (let ((r-table (dbms-expect-identifier tokens)))
    (if (dbms-error-p r-table)
        r-table
        (let ((r-lp (dbms-expect-symbol (dbms-parser-ok-rest r-table) "(")))
          (if (dbms-error-p r-lp)
              r-lp
              (let ((r-cols (dbms-parse-column-def-list (dbms-parser-ok-rest r-lp))))
                (if (dbms-error-p r-cols)
                    r-cols
                    (let ((r-rp (dbms-expect-symbol (dbms-parser-ok-rest r-cols) ")")))
                      (if (dbms-error-p r-rp)
                          r-rp
                          (dbms-parser-ok
                           (dbms-make-stmt
                            'create-table
                            (dbms-make-table-def (dbms-parser-ok-value r-table)
                                                 (dbms-parser-ok-value r-cols)
                                                 '()
                                                 '()))
                           (dbms-parser-ok-rest r-rp)))))))))))

(defun dbms-parse-insert (tokens)
  (let ((r-table (dbms-expect-identifier tokens)))
    (if (dbms-error-p r-table)
        r-table
        (let ((table-name (dbms-parser-ok-value r-table))
              (rest (dbms-parser-ok-rest r-table))
              (columns '())
              (failed '()))
          (if (and (not (null rest)) (dbms-token-is-symbol-p (first rest) "("))
              (let ((r-cols (dbms-parse-identifier-list (cdr rest))))
                (if (dbms-error-p r-cols)
                    (setq failed r-cols)
                    (let ((r-rp (dbms-expect-symbol (dbms-parser-ok-rest r-cols) ")")))
                      (if (dbms-error-p r-rp)
                          (setq failed r-rp)
                          (progn
                            (setq columns (dbms-parser-ok-value r-cols))
                            (setq rest (dbms-parser-ok-rest r-rp)))))))
              nil)
          (if (dbms-error-p failed)
              failed
              (let ((r-values-kw (dbms-expect-keyword rest "VALUES")))
                (if (dbms-error-p r-values-kw)
                    r-values-kw
                    (let ((r-lp (dbms-expect-symbol (dbms-parser-ok-rest r-values-kw) "(")))
                      (if (dbms-error-p r-lp)
                          r-lp
                          (let ((r-vals (dbms-parse-expr-list (dbms-parser-ok-rest r-lp))))
                            (if (dbms-error-p r-vals)
                                r-vals
                                (let ((r-rp (dbms-expect-symbol (dbms-parser-ok-rest r-vals) ")")))
                                  (if (dbms-error-p r-rp)
                                      r-rp
                                      (dbms-parser-ok
                                       (dbms-make-stmt
                                        'insert
                                        (list (list "table" table-name)
                                              (list "columns" columns)
                                              (list "values" (dbms-parser-ok-value r-vals))))
                                       (dbms-parser-ok-rest r-rp)))))))))))))))

(defun dbms-parse-select-list (tokens)
  (if (and (not (null tokens)) (dbms-token-is-operator-p (first tokens) "*"))
      (dbms-parser-ok '* (cdr tokens))
      (dbms-parse-identifier-list tokens)))

(defun dbms-parse-select (tokens)
  (let ((r-cols (dbms-parse-select-list tokens)))
    (if (dbms-error-p r-cols)
        r-cols
        (let ((r-from (dbms-expect-keyword (dbms-parser-ok-rest r-cols) "FROM")))
          (if (dbms-error-p r-from)
              r-from
              (let ((r-table (dbms-expect-identifier (dbms-parser-ok-rest r-from))))
                (if (dbms-error-p r-table)
                    r-table
                    (let ((where '())
                          (order-by '())
                          (rest (dbms-parser-ok-rest r-table))
                          (failed '()))
                      (if (and (not (null rest)) (dbms-token-is-keyword-p (first rest) "WHERE"))
                          (let ((r-where (dbms-parse-predicate (cdr rest))))
                            (if (dbms-error-p r-where)
                                (setq failed r-where)
                                (progn
                                  (setq where (dbms-parser-ok-value r-where))
                                  (setq rest (dbms-parser-ok-rest r-where)))))
                          nil)
                      (if (and (null failed)
                               (not (null rest))
                               (dbms-token-is-keyword-p (first rest) "ORDER"))
                          (let ((r-by (dbms-expect-keyword (cdr rest) "BY")))
                            (if (dbms-error-p r-by)
                                (setq failed r-by)
                                (let ((r-col (dbms-expect-identifier (dbms-parser-ok-rest r-by))))
                                  (if (dbms-error-p r-col)
                                      (setq failed r-col)
                                      (let ((dir 'ASC)
                                            (rest2 (dbms-parser-ok-rest r-col)))
                                        (if (and (not (null rest2)) (dbms-token-is-keyword-p (first rest2) "ASC"))
                                            (progn
                                              (setq dir 'ASC)
                                              (setq rest2 (cdr rest2)))
                                            (if (and (not (null rest2)) (dbms-token-is-keyword-p (first rest2) "DESC"))
                                                (progn
                                                  (setq dir 'DESC)
                                                  (setq rest2 (cdr rest2)))
                                                nil))
                                        (setq order-by (list (dbms-parser-ok-value r-col) dir))
                                        (setq rest rest2))))))
                          nil)
                      (if (dbms-error-p failed)
                          failed
                          (dbms-parser-ok
                           (dbms-make-stmt
                            'select
                            (list (list "columns" (dbms-parser-ok-value r-cols))
                                  (list "table" (dbms-parser-ok-value r-table))
                                  (list "where" where)
                                  (list "order-by" order-by)))
                           rest))))))))))

(defun dbms-parse-update (tokens)
  (let ((r-table (dbms-expect-identifier tokens)))
    (if (dbms-error-p r-table)
        r-table
        (let ((r-set (dbms-expect-keyword (dbms-parser-ok-rest r-table) "SET")))
          (if (dbms-error-p r-set)
              r-set
              (let ((r-assign (dbms-parse-set-assignments (dbms-parser-ok-rest r-set))))
                (if (dbms-error-p r-assign)
                    r-assign
                    (let ((where '())
                          (rest (dbms-parser-ok-rest r-assign))
                          (failed '()))
                      (if (and (not (null rest)) (dbms-token-is-keyword-p (first rest) "WHERE"))
                          (let ((r-where (dbms-parse-predicate (cdr rest))))
                            (if (dbms-error-p r-where)
                                (setq failed r-where)
                                (progn
                                  (setq where (dbms-parser-ok-value r-where))
                                  (setq rest (dbms-parser-ok-rest r-where)))))
                          nil)
                      (if (dbms-error-p failed)
                          failed
                          (dbms-parser-ok
                           (dbms-make-stmt
                            'update
                            (list (list "table" (dbms-parser-ok-value r-table))
                                  (list "set" (dbms-parser-ok-value r-assign))
                                  (list "where" where)))
                           rest))))))))))

(defun dbms-parse-delete (tokens)
  (let ((r-from (dbms-expect-keyword tokens "FROM")))
    (if (dbms-error-p r-from)
        r-from
        (let ((r-table (dbms-expect-identifier (dbms-parser-ok-rest r-from))))
          (if (dbms-error-p r-table)
              r-table
              (let ((where '())
                    (rest (dbms-parser-ok-rest r-table))
                    (failed '()))
                (if (and (not (null rest)) (dbms-token-is-keyword-p (first rest) "WHERE"))
                    (let ((r-where (dbms-parse-predicate (cdr rest))))
                      (if (dbms-error-p r-where)
                          (setq failed r-where)
                          (progn
                            (setq where (dbms-parser-ok-value r-where))
                            (setq rest (dbms-parser-ok-rest r-where)))))
                    nil)
                (if (dbms-error-p failed)
                    failed
                    (dbms-parser-ok
                     (dbms-make-stmt
                      'delete
                      (list (list "table" (dbms-parser-ok-value r-table))
                            (list "where" where)))
                     rest))))))))

(defun dbms-parse-statement (tokens)
  (if (null tokens)
      (dbms-parser-error "empty token stream" '())
      (let ((tok (first tokens)))
        (if (dbms-token-is-keyword-p tok "CREATE")
            (let ((r-tablekw (dbms-expect-keyword (cdr tokens) "TABLE")))
              (if (dbms-error-p r-tablekw)
                  r-tablekw
                  (dbms-parse-create-table (dbms-parser-ok-rest r-tablekw))))
            (if (dbms-token-is-keyword-p tok "INSERT")
                (let ((r-into (dbms-expect-keyword (cdr tokens) "INTO")))
                  (if (dbms-error-p r-into)
                      r-into
                      (dbms-parse-insert (dbms-parser-ok-rest r-into))))
                (if (dbms-token-is-keyword-p tok "SELECT")
                    (dbms-parse-select (cdr tokens))
                    (if (dbms-token-is-keyword-p tok "UPDATE")
                        (dbms-parse-update (cdr tokens))
                        (if (dbms-token-is-keyword-p tok "DELETE")
                            (dbms-parse-delete (cdr tokens))
                            (dbms-parser-error "unsupported statement"
                                               (dbms-sql-token-lexeme tok))))))))))

(defun dbms-parse-sql (sql-text)
  (if (or (null sql-text) (string= sql-text ""))
      (dbms-make-error 'dbms/parse-error "empty sql" sql-text)
      (let ((tokens (dbms-lex-sql sql-text)))
        (if (dbms-error-p tokens)
            tokens
            (let ((r-stmt (dbms-parse-statement tokens)))
              (if (dbms-error-p r-stmt)
                  r-stmt
                  (let ((rest (dbms-parser-ok-rest r-stmt)))
                    (if (and (not (null rest)) (dbms-token-is-symbol-p (first rest) ";"))
                        (setq rest (cdr rest))
                        nil)
                    (if (null rest)
                        (dbms-make-ast
                         (list (dbms-parser-ok-value r-stmt))
                         (list (list "profile" "MVP Core")))
                        (dbms-parser-error "unexpected trailing tokens"
                                           (dbms-sql-token-lexeme (first rest)))))))))))
