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
        (cond
         ((dbms-token-is-keyword-p tok "INT")
          (dbms-parser-ok 'INT (cdr tokens)))
         ((dbms-token-is-keyword-p tok "INTEGER")
          (dbms-parser-ok 'INTEGER (cdr tokens)))
         ((dbms-token-is-keyword-p tok "BIGINT")
          (dbms-parser-ok 'BIGINT (cdr tokens)))
         ((dbms-token-is-keyword-p tok "BIGSERIAL")
          (dbms-parser-ok 'BIGSERIAL (cdr tokens)))
         ((dbms-token-is-keyword-p tok "TIMESTAMPTZ")
          (dbms-parser-ok 'TIMESTAMPTZ (cdr tokens)))
         ((dbms-token-is-keyword-p tok "TEXT")
          (dbms-parser-ok 'TEXT (cdr tokens)))
         ((dbms-token-is-keyword-p tok "BOOL")
          (dbms-parser-ok 'BOOL (cdr tokens)))
         (t
          (dbms-parser-error "expected type INT/INTEGER/BIGINT/BIGSERIAL/TEXT/BOOL/TIMESTAMPTZ"
                             (dbms-sql-token-lexeme tok)))))))

(defun dbms-split-top-level-comma (tokens)
  (let ((rest tokens)
        (depth 0)
        (cur '())
        (out '()))
    (while (not (null rest))
      (let ((tok (car rest)))
        (if (dbms-token-is-symbol-p tok "(")
            (setq depth (+ depth 1))
            (if (and (dbms-token-is-symbol-p tok ")") (> depth 0))
                (setq depth (- depth 1))
                nil))
        (if (and (= depth 0) (dbms-token-is-symbol-p tok ","))
            (progn
              (setq out (append out (list cur)))
              (setq cur '()))
            (setq cur (append cur (list tok)))))
      (setq rest (cdr rest)))
    (append out (list cur))))

(defun dbms-find-top-level-keyword-index (tokens kw-upper)
  (let ((rest tokens)
        (depth 0)
        (idx 0)
        (found '()))
    (while (and (null found) (not (null rest)))
      (let ((tok (car rest)))
        (if (dbms-token-is-symbol-p tok "(")
            (setq depth (+ depth 1))
            (if (and (dbms-token-is-symbol-p tok ")") (> depth 0))
                (setq depth (- depth 1))
                nil))
        (if (and (= depth 0) (dbms-token-is-keyword-p tok kw-upper))
            (setq found idx)
            nil))
      (setq idx (+ idx 1))
      (setq rest (cdr rest)))
    found))

(defun dbms-tokens-take (tokens n)
  (if (or (= n 0) (null tokens))
      '()
      (cons (car tokens) (dbms-tokens-take (cdr tokens) (- n 1)))))

(defun dbms-tokens-drop (tokens n)
  (if (or (= n 0) (null tokens))
      tokens
      (dbms-tokens-drop (cdr tokens) (- n 1))))

(defun dbms-parse-function-call-expr (tokens)
  ;; tokens starts with identifier followed by '('
  (let* ((name-raw (fourth (first tokens)))
         (name (dbms-sql-upper name-raw))
         (r-paren (dbms-read-parenthesized-tokens (cdr tokens))))
    (if (dbms-error-p r-paren)
        r-paren
        (let ((inner (dbms-parser-ok-value r-paren))
              (rest (dbms-parser-ok-rest r-paren)))
          (if (string= name "POSITION")
              (let ((in-pos (dbms-find-top-level-keyword-index inner "IN")))
                (if (or (null in-pos) (= in-pos 0) (= in-pos (- (length inner) 1)))
                    (dbms-parser-error "POSITION requires syntax POSITION(<substr> IN <text>)" inner)
                    (let ((lhs-toks (dbms-tokens-take inner in-pos))
                          (rhs-toks (dbms-tokens-drop inner (+ in-pos 1))))
                      (let ((r-lhs (dbms-parse-expr lhs-toks))
                            (r-rhs (dbms-parse-expr rhs-toks)))
                        (if (or (dbms-error-p r-lhs) (dbms-error-p r-rhs)
                                (not (null (dbms-parser-ok-rest r-lhs)))
                                (not (null (dbms-parser-ok-rest r-rhs))))
                            (dbms-parser-error "invalid POSITION arguments" inner)
                            (dbms-parser-ok
                             (list 'dbms-expr 'function name
                                   (list (dbms-parser-ok-value r-lhs)
                                         (dbms-parser-ok-value r-rhs)))
                             rest))))))
              (let ((arg-groups (if (null inner) '() (dbms-split-top-level-comma inner)))
                    (args '())
                    (failed '()))
                (dolist (g arg-groups)
                  (if (dbms-error-p failed)
                      nil
                      (let ((r (dbms-parse-expr g)))
                        (if (or (dbms-error-p r)
                                (not (null (dbms-parser-ok-rest r))))
                            (setq failed (dbms-parser-error "invalid function arguments" g))
                            (setq args (append args (list (dbms-parser-ok-value r))))))))
                (if (dbms-error-p failed)
                    failed
                    (dbms-parser-ok (list 'dbms-expr 'function name args) rest))))))))

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
                            (if (and (not (null (cdr tokens)))
                                     (dbms-token-is-symbol-p (second tokens) "("))
                                (dbms-parse-function-call-expr tokens)
                                (dbms-parser-ok (list 'dbms-expr 'identifier (fourth tok)) (cdr tokens)))
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

(defun dbms-keyword-in-list-p (kw kws)
  (if (null kws)
      nil
      (if (string= kw (car kws))
          t
          (dbms-keyword-in-list-p kw (cdr kws)))))

(defun dbms-token-keyword-upper-or-empty (tok)
  (if (dbms-token-is-kind-p tok 'keyword)
      (fourth tok)
      ""))

(defun dbms-token-list-contains-keyword-p (tokens kw-upper)
  (if (null tokens)
      nil
      (if (dbms-token-is-keyword-p (car tokens) kw-upper)
          t
          (dbms-token-list-contains-keyword-p (cdr tokens) kw-upper))))

(defun dbms-scan-until-top-level-keywords (tokens stop-kws)
  ;; Returns (segment rest stop-kw-or-empty)
  (let ((rest tokens)
        (seg '())
        (depth 0)
        (stop "")
        (done nil))
    (while (and (not done) (not (null rest)))
      (let ((tok (car rest)))
        (if (and (= depth 0)
                 (dbms-token-is-kind-p tok 'keyword)
                 (dbms-keyword-in-list-p (fourth tok) stop-kws))
            (progn
              (setq stop (fourth tok))
              (setq done t))
            (progn
              (if (dbms-token-is-symbol-p tok "(")
                  (setq depth (+ depth 1))
                  (if (and (dbms-token-is-symbol-p tok ")") (> depth 0))
                      (setq depth (- depth 1))
                      nil))
              (setq seg (append seg (list tok)))
              (setq rest (cdr rest))))))
    (list seg rest stop)))

(defun dbms-read-parenthesized-tokens (tokens)
  (if (or (null tokens) (not (dbms-token-is-symbol-p (first tokens) "(")))
      (dbms-parser-error "expected (" (if (null tokens) 'eof (dbms-sql-token-lexeme (first tokens))))
      (let ((rest (cdr tokens))
            (inner '())
            (depth 1)
            (done nil))
        (while (and (not done) (not (null rest)))
          (let ((tok (car rest)))
            (if (dbms-token-is-symbol-p tok "(")
                (progn
                  (setq depth (+ depth 1))
                  (setq inner (append inner (list tok)))
                  (setq rest (cdr rest)))
                (if (dbms-token-is-symbol-p tok ")")
                    (if (= depth 1)
                        (progn
                          (setq done t)
                          (setq rest (cdr rest)))
                        (progn
                          (setq depth (- depth 1))
                          (setq inner (append inner (list tok)))
                          (setq rest (cdr rest))))
                    (progn
                      (setq inner (append inner (list tok)))
                      (setq rest (cdr rest))))))
          )
        (if done
            (dbms-parser-ok inner rest)
            (dbms-parser-error "unclosed parenthesis" tokens)))))

(defun dbms-parse-simple-select-list-from-tokens (tokens)
  (if (and (= (length tokens) 1)
           (dbms-token-is-operator-p (first tokens) "*"))
      (dbms-parser-ok '* '())
      (dbms-parse-identifier-list tokens)))

(defun dbms-parse-simple-order-by-from-tokens (tokens)
  (if (null tokens)
      (dbms-parser-ok '() '())
      (let ((r-col (dbms-expect-identifier tokens)))
        (if (dbms-error-p r-col)
            r-col
            (let ((dir 'ASC)
                  (rest (dbms-parser-ok-rest r-col)))
              (if (and (not (null rest)) (dbms-token-is-keyword-p (first rest) "ASC"))
                  (setq rest (cdr rest))
                  (if (and (not (null rest)) (dbms-token-is-keyword-p (first rest) "DESC"))
                      (progn
                        (setq dir 'DESC)
                        (setq rest (cdr rest)))
                      nil))
              (if (null rest)
                  (dbms-parser-ok (list (dbms-parser-ok-value r-col) dir) '())
                  (dbms-parser-error "complex ORDER BY not allowed in MVP parser path" rest)))))))

(defun dbms-list-last (xs)
  (if (null (cdr xs))
      (car xs)
      (dbms-list-last (cdr xs))))

(defun dbms-list-butlast (xs)
  (if (null xs)
      '()
      (if (null (cdr xs))
          '()
          (cons (car xs) (dbms-list-butlast (cdr xs))))))

(defun dbms-drop-trailing-semicolon-token (tokens)
  (if (null tokens)
      tokens
      (let ((tail (dbms-list-last tokens)))
        (if (dbms-token-is-symbol-p tail ";")
            (dbms-list-butlast tokens)
            tokens))))

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
                    (cond
                     ((and (not (null rest))
                           (dbms-token-is-keyword-p (first rest) "PRIMARY"))
                      (let ((r-key (dbms-expect-keyword (cdr rest) "KEY")))
                        (if (dbms-error-p r-key)
                            (progn
                              (setq failed r-key)
                              (setq done t))
                            (progn
                              (setq attrs (append attrs '(PRIMARY-KEY)))
                              (setq rest (dbms-parser-ok-rest r-key))))))
                     ((and (not (null rest))
                           (dbms-token-is-keyword-p (first rest) "NOT"))
                      (let ((r-null (dbms-expect-null-word (cdr rest))))
                        (if (dbms-error-p r-null)
                            (progn
                              (setq failed r-null)
                              (setq done t))
                            (progn
                              (setq attrs (append attrs '(NOT-NULL)))
                              (setq rest (dbms-parser-ok-rest r-null))))))
                     ((and (not (null rest))
                           (dbms-token-is-keyword-p (first rest) "DEFAULT"))
                      (let ((r-now-name (dbms-expect-identifier (cdr rest))))
                        (if (dbms-error-p r-now-name)
                            (progn
                              (setq failed r-now-name)
                              (setq done t))
                            (if (string= (dbms-sql-upper (dbms-parser-ok-value r-now-name)) "NOW")
                                (let ((r-lp (dbms-expect-symbol (dbms-parser-ok-rest r-now-name) "(")))
                                  (if (dbms-error-p r-lp)
                                      (progn
                                        (setq failed r-lp)
                                        (setq done t))
                                      (let ((r-rp (dbms-expect-symbol (dbms-parser-ok-rest r-lp) ")")))
                                        (if (dbms-error-p r-rp)
                                            (progn
                                              (setq failed r-rp)
                                              (setq done t))
                                            (progn
                                              (setq attrs (append attrs '(DEFAULT-NOW)))
                                              (setq rest (dbms-parser-ok-rest r-rp)))))))
                                (progn
                                  (setq failed (dbms-parser-error "DEFAULT supports only now()" (dbms-parser-ok-value r-now-name)))
                                  (setq done t))))))
                     (t
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
  (let ((rest tokens)
        (if-not-exists nil))
    (if (and (not (null rest)) (dbms-token-is-keyword-p (first rest) "IF"))
        (let ((r-not (dbms-expect-keyword (cdr rest) "NOT")))
          (if (dbms-error-p r-not)
              r-not
              (let ((r-exists (dbms-expect-keyword (dbms-parser-ok-rest r-not) "EXISTS")))
                (if (dbms-error-p r-exists)
                    r-exists
                    (progn
                      (setq if-not-exists t)
                      (setq rest (dbms-parser-ok-rest r-exists)))))))
        nil)
    (let ((r-table (dbms-expect-identifier rest)))
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
                             (if if-not-exists
                                 (dbms-make-stmt
                                  'create-table-wiki
                                  (list (list "table-def"
                                              (dbms-make-table-def (dbms-parser-ok-value r-table)
                                                                   (dbms-parser-ok-value r-cols)
                                                                   '()
                                                                   '()))
                                        (list "if-not-exists" t)))
                                 (dbms-make-stmt
                                  'create-table
                                  (dbms-make-table-def (dbms-parser-ok-value r-table)
                                                       (dbms-parser-ok-value r-cols)
                                                       '()
                                                       '())))
                             (dbms-parser-ok-rest r-rp))))))))))))

(defun dbms-parse-id-list-from-paren (tokens)
  (let ((r-inner (dbms-read-parenthesized-tokens tokens)))
    (if (dbms-error-p r-inner)
        r-inner
        (let ((r-ids (dbms-parse-identifier-list (dbms-parser-ok-value r-inner))))
          (if (dbms-error-p r-ids)
              r-ids
              (if (null (dbms-parser-ok-rest r-ids))
                  (dbms-parser-ok (dbms-parser-ok-value r-ids) (dbms-parser-ok-rest r-inner))
                  (dbms-parser-error "invalid identifier list" (dbms-parser-ok-value r-inner))))))))

(defun dbms-parse-create-index (tokens)
  (let ((rest tokens)
        (if-not-exists nil))
    (if (and (not (null rest)) (dbms-token-is-keyword-p (first rest) "IF"))
        (let ((r-not (dbms-expect-keyword (cdr rest) "NOT")))
          (if (dbms-error-p r-not)
              r-not
              (let ((r-exists (dbms-expect-keyword (dbms-parser-ok-rest r-not) "EXISTS")))
                (if (dbms-error-p r-exists)
                    r-exists
                    (progn
                      (setq if-not-exists t)
                      (setq rest (dbms-parser-ok-rest r-exists)))))))
        nil)
    (let ((r-index-name (dbms-expect-identifier rest)))
      (if (dbms-error-p r-index-name)
          r-index-name
          (let ((r-on (dbms-expect-keyword (dbms-parser-ok-rest r-index-name) "ON")))
            (if (dbms-error-p r-on)
                r-on
                (let ((r-table (dbms-expect-identifier (dbms-parser-ok-rest r-on))))
                  (if (dbms-error-p r-table)
                      r-table
                      (let ((r-cols (dbms-parse-id-list-from-paren (dbms-parser-ok-rest r-table))))
                        (if (dbms-error-p r-cols)
                            r-cols
                            (dbms-parser-ok
                             (dbms-make-stmt
                              'create-index
                              (list (list "index-name" (dbms-parser-ok-value r-index-name))
                                    (list "table" (dbms-parser-ok-value r-table))
                                    (list "columns" (dbms-parser-ok-value r-cols))
                                    (list "if-not-exists" if-not-exists)))
                             (dbms-parser-ok-rest r-cols))))))))))))

(defun dbms-parse-add-check-constraint (table-name c-name tokens)
  (let ((r-check (dbms-read-parenthesized-tokens tokens)))
    (if (dbms-error-p r-check)
        r-check
        (dbms-parser-ok
         (dbms-make-stmt
          'alter-table
          (list (list "table" table-name)
                (list "action" "ADD-CONSTRAINT")
                (list "constraint"
                      (dbms-make-constraint 'check c-name (dbms-parser-ok-value r-check)))))
         (dbms-parser-ok-rest r-check)))))

(defun dbms-parse-add-foreign-key-constraint (table-name c-name tokens)
  (let ((r-local (dbms-parse-id-list-from-paren tokens)))
    (if (dbms-error-p r-local)
        r-local
        (let ((r-ref (dbms-expect-keyword (dbms-parser-ok-rest r-local) "REFERENCES")))
          (if (dbms-error-p r-ref)
              r-ref
              (let ((r-ref-table (dbms-expect-identifier (dbms-parser-ok-rest r-ref))))
                (if (dbms-error-p r-ref-table)
                    r-ref-table
                    (let ((r-ref-cols (dbms-parse-id-list-from-paren (dbms-parser-ok-rest r-ref-table))))
                      (if (dbms-error-p r-ref-cols)
                          r-ref-cols
                          (let ((r-on (dbms-expect-keyword (dbms-parser-ok-rest r-ref-cols) "ON")))
                            (if (dbms-error-p r-on)
                                r-on
                                (let ((r-delete (dbms-expect-keyword (dbms-parser-ok-rest r-on) "DELETE")))
                                  (if (dbms-error-p r-delete)
                                      r-delete
                                      (let ((tail (dbms-parser-ok-rest r-delete))
                                            (on-delete '()))
                                        (if (and (not (null tail))
                                                 (string= (dbms-sql-upper (dbms-sql-token-lexeme (first tail))) "CASCADE"))
                                            (progn
                                              (setq on-delete 'CASCADE)
                                              (setq tail (cdr tail)))
                                            (if (and (not (null tail))
                                                     (dbms-token-is-keyword-p (first tail) "SET")
                                                     (not (null (cdr tail)))
                                                     (or (dbms-token-is-keyword-p (second tail) "NULL")
                                                         (dbms-token-is-kind-p (second tail) 'literal-null)))
                                                (progn
                                                  (setq on-delete 'SET-NULL)
                                                  (setq tail (cdr (cdr tail))))
                                                nil))
                                        (if (null on-delete)
                                            (dbms-parser-error "ON DELETE must be CASCADE or SET NULL" (dbms-parser-ok-rest r-delete))
                                            (dbms-parser-ok
                                             (dbms-make-stmt
                                              'alter-table
                                              (list (list "table" table-name)
                                                    (list "action" "ADD-CONSTRAINT")
                                                    (list "constraint"
                                                          (dbms-make-constraint
                                                           'foreign-key
                                                           c-name
                                                           (list (dbms-parser-ok-value r-local)
                                                                 (dbms-parser-ok-value r-ref-table)
                                                                 (dbms-parser-ok-value r-ref-cols)
                                                                 on-delete)))))
                                             tail))))))))))))))))

(defun dbms-parse-add-constraint (table-name tokens)
  (let ((r-name (dbms-expect-identifier tokens)))
    (if (dbms-error-p r-name)
        r-name
        (let ((cname (dbms-parser-ok-value r-name))
              (rest (dbms-parser-ok-rest r-name)))
          (if (and (not (null rest)) (dbms-token-is-keyword-p (first rest) "CHECK"))
              (dbms-parse-add-check-constraint table-name cname (cdr rest))
              (if (and (not (null rest)) (dbms-token-is-keyword-p (first rest) "FOREIGN"))
                  (let ((r-key (dbms-expect-keyword (cdr rest) "KEY")))
                    (if (dbms-error-p r-key)
                        r-key
                        (dbms-parse-add-foreign-key-constraint table-name cname (dbms-parser-ok-rest r-key))))
                  (dbms-parser-error "ADD CONSTRAINT supports CHECK or FOREIGN KEY" rest)))))))

(defun dbms-parse-alter-table (tokens)
  (let ((r-table (dbms-expect-identifier tokens)))
    (if (dbms-error-p r-table)
        r-table
        (let ((table-name (dbms-parser-ok-value r-table))
              (rest (dbms-parser-ok-rest r-table)))
          (if (and (not (null rest)) (dbms-token-is-keyword-p (first rest) "DROP"))
              (let ((r-constraint (dbms-expect-keyword (cdr rest) "CONSTRAINT")))
                (if (dbms-error-p r-constraint)
                    r-constraint
                    (let ((rest2 (dbms-parser-ok-rest r-constraint))
                          (if-exists nil))
                      (if (and (not (null rest2)) (dbms-token-is-keyword-p (first rest2) "IF"))
                          (let ((r-exists (dbms-expect-keyword (cdr rest2) "EXISTS")))
                            (if (dbms-error-p r-exists)
                                r-exists
                                (progn
                                  (setq if-exists t)
                                  (setq rest2 (dbms-parser-ok-rest r-exists)))))
                          nil)
                      (let ((r-name (dbms-expect-identifier rest2)))
                        (if (dbms-error-p r-name)
                            r-name
                            (dbms-parser-ok
                             (dbms-make-stmt
                              'alter-table
                              (list (list "table" table-name)
                                    (list "action" "DROP-CONSTRAINT")
                                    (list "if-exists" if-exists)
                                    (list "constraint-name" (dbms-parser-ok-value r-name))))
                             (dbms-parser-ok-rest r-name)))))))
              (if (and (not (null rest)) (dbms-token-is-keyword-p (first rest) "ADD"))
                  (let ((r-constraint (dbms-expect-keyword (cdr rest) "CONSTRAINT")))
                    (if (dbms-error-p r-constraint)
                        r-constraint
                        (dbms-parse-add-constraint table-name (dbms-parser-ok-rest r-constraint))))
                  (dbms-parser-error "ALTER TABLE supports DROP/ADD CONSTRAINT" rest)))))))

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
              (let ((mode "")
                    (source '())
                    (on-conflict '()))
                (if (and (not (null rest)) (dbms-token-is-keyword-p (first rest) "VALUES"))
                    (let ((r-lp (dbms-expect-symbol (cdr rest) "(")))
                      (if (dbms-error-p r-lp)
                          (setq failed r-lp)
                          (let ((r-vals (dbms-parse-expr-list (dbms-parser-ok-rest r-lp))))
                            (if (dbms-error-p r-vals)
                                (setq failed r-vals)
                                (let ((r-rp (dbms-expect-symbol (dbms-parser-ok-rest r-vals) ")")))
                                  (if (dbms-error-p r-rp)
                                      (setq failed r-rp)
                                      (progn
                                        (setq mode "VALUES")
                                        (setq source (dbms-parser-ok-value r-vals))
                                        (setq rest (dbms-parser-ok-rest r-rp)))))))))
                    (if (and (not (null rest)) (dbms-token-is-keyword-p (first rest) "SELECT"))
                        (let ((r-sel (dbms-parse-select (cdr rest))))
                          (if (dbms-error-p r-sel)
                              (setq failed r-sel)
                              (progn
                                (setq mode "SELECT")
                                (setq source (dbms-parser-ok-value r-sel))
                                (setq rest (dbms-parser-ok-rest r-sel)))))
                        (setq failed (dbms-parser-error "INSERT requires VALUES or SELECT" rest))))

                (if (and (null failed)
                         (not (null rest))
                         (dbms-token-is-keyword-p (first rest) "ON"))
                    (let ((r-conflict (dbms-expect-keyword (cdr rest) "CONFLICT")))
                      (if (dbms-error-p r-conflict)
                          (setq failed r-conflict)
                          (let ((r-target (dbms-read-parenthesized-tokens (dbms-parser-ok-rest r-conflict))))
                            (if (dbms-error-p r-target)
                                (setq failed r-target)
                                (let ((r-do (dbms-expect-keyword (dbms-parser-ok-rest r-target) "DO")))
                                  (if (dbms-error-p r-do)
                                      (setq failed r-do)
                                      (let ((r-nothing (dbms-expect-keyword (dbms-parser-ok-rest r-do) "NOTHING")))
                                        (if (dbms-error-p r-nothing)
                                            (setq failed r-nothing)
                                            (progn
                                              (setq on-conflict (list (list "target" (dbms-parser-ok-value r-target))
                                                                      (list "action" "DO NOTHING")))
                                              (setq rest (dbms-parser-ok-rest r-nothing)))))))))))
                    nil)

                (if (dbms-error-p failed)
                    failed
                    (if (and (string= mode "VALUES") (null on-conflict))
                        (dbms-parser-ok
                         (dbms-make-stmt
                          'insert
                          (list (list "table" table-name)
                                (list "columns" columns)
                                (list "values" source)))
                         rest)
                        (dbms-parser-ok
                         (dbms-make-stmt
                          'insert-wiki
                          (list (list "table" table-name)
                                (list "columns" columns)
                                (list "source-kind" mode)
                                (list "source" source)
                                (list "on-conflict" on-conflict)))
                         rest)))))))))

(defun dbms-parse-select-list (tokens)
  (if (and (not (null tokens)) (dbms-token-is-operator-p (first tokens) "*"))
      (dbms-parser-ok '* (cdr tokens))
      (dbms-parse-identifier-list tokens)))

(defun dbms-parse-select (tokens)
  (let* ((scan-from (dbms-scan-until-top-level-keywords tokens '("FROM")))
         (select-list-toks (first scan-from))
         (rest0 (second scan-from)))
    (if (null rest0)
        (dbms-parser-error "SELECT requires FROM" tokens)
        (if (null select-list-toks)
            (dbms-parser-error "SELECT list must not be empty" tokens)
            (let* ((scan-where (dbms-scan-until-top-level-keywords (cdr rest0) '("WHERE" "ORDER" "LIMIT")))
               (from-toks (first scan-where))
               (rest1 (second scan-where))
               (where-toks '())
               (order-toks '())
               (limit-tok '())
               (rest rest1)
               (failed '()))
          (if (and (not (null rest)) (dbms-token-is-keyword-p (first rest) "WHERE"))
              (let ((scan-order (dbms-scan-until-top-level-keywords (cdr rest) '("ORDER" "LIMIT"))))
                (setq where-toks (first scan-order))
                (setq rest (second scan-order)))
              nil)
          (if (and (not (null rest)) (dbms-token-is-keyword-p (first rest) "ORDER"))
              (let ((r-by (dbms-expect-keyword (cdr rest) "BY")))
                (if (dbms-error-p r-by)
                    (setq failed r-by)
                    (let ((scan-limit (dbms-scan-until-top-level-keywords (dbms-parser-ok-rest r-by) '("LIMIT"))))
                      (setq order-toks (first scan-limit))
                      (setq rest (second scan-limit)))))
              nil)
          (if (and (null failed) (not (null rest)) (dbms-token-is-keyword-p (first rest) "LIMIT"))
              (if (or (null (cdr rest)) (not (dbms-token-is-kind-p (second rest) 'number)))
                  (setq failed (dbms-parser-error "LIMIT requires integer literal" rest))
                  (progn
                    (setq limit-tok (second rest))
                    (setq rest (cdr (cdr rest)))))
              nil)

          (if (dbms-error-p failed)
              failed
              (let* ((simple-list-r (dbms-parse-simple-select-list-from-tokens select-list-toks))
                     (simple-from-r (if (and (= (length from-toks) 1)
                                             (dbms-token-is-kind-p (first from-toks) 'identifier))
                                        (dbms-parser-ok (fourth (first from-toks)) '())
                                        (dbms-parser-error "complex FROM" from-toks)))
                     (simple-where-r (if (null where-toks)
                                         (dbms-parser-ok '() '())
                                         (let ((r (dbms-parse-predicate where-toks)))
                                           (if (dbms-error-p r)
                                               r
                                               (if (null (dbms-parser-ok-rest r))
                                                   r
                                                   (dbms-parser-error "complex WHERE" where-toks)))))
                     )
                     (simple-order-r (if (null order-toks)
                                         (dbms-parser-ok '() '())
                                         (dbms-parse-simple-order-by-from-tokens order-toks)))
                     (mvp-simple (and (not (dbms-error-p simple-list-r))
                                      (not (dbms-error-p simple-from-r))
                                      (not (dbms-error-p simple-where-r))
                                      (not (dbms-error-p simple-order-r))
                                      (null limit-tok)
                                      (not (dbms-token-list-contains-keyword-p from-toks "JOIN")))))
                (if mvp-simple
                    (dbms-parser-ok
                     (dbms-make-stmt
                      'select
                      (list (list "columns" (dbms-parser-ok-value simple-list-r))
                            (list "table" (dbms-parser-ok-value simple-from-r))
                            (list "where" (dbms-parser-ok-value simple-where-r))
                            (list "order-by" (dbms-parser-ok-value simple-order-r))))
                     rest)
                    (dbms-parser-ok
                     (dbms-make-stmt
                      'select-wiki
                      (list (list "select-list" select-list-toks)
                            (list "from" from-toks)
                            (list "where" where-toks)
                            (list "order-by" order-toks)
                            (list "limit" (if (null limit-tok) '() (fourth limit-tok)))))
                     rest)))))))))

(defun dbms-parse-update (tokens)
  (let ((r-table (dbms-expect-identifier tokens)))
    (if (dbms-error-p r-table)
        r-table
        (let ((rest-after-table (dbms-parser-ok-rest r-table))
              (table-alias '())
              (r-set '()))
          (if (and (not (null rest-after-table))
                   (dbms-token-is-kind-p (first rest-after-table) 'identifier))
              (progn
                (setq table-alias (fourth (first rest-after-table)))
                (setq rest-after-table (cdr rest-after-table)))
              nil)
          (setq r-set (dbms-expect-keyword rest-after-table "SET"))
          (if (dbms-error-p r-set)
              r-set
              (let* ((scan-main (dbms-scan-until-top-level-keywords (dbms-parser-ok-rest r-set) '("FROM" "WHERE" "RETURNING")))
                     (set-toks (first scan-main))
                     (rest (second scan-main))
                     (from-toks '())
                     (where-toks '())
                     (returning-toks '())
                     (failed '()))
                (if (and (not (null rest)) (dbms-token-is-keyword-p (first rest) "FROM"))
                    (let ((scan-where (dbms-scan-until-top-level-keywords (cdr rest) '("WHERE" "RETURNING"))))
                      (setq from-toks (first scan-where))
                      (setq rest (second scan-where)))
                    nil)
                (if (and (not (null rest)) (dbms-token-is-keyword-p (first rest) "WHERE"))
                    (let ((scan-ret (dbms-scan-until-top-level-keywords (cdr rest) '("RETURNING"))))
                      (setq where-toks (first scan-ret))
                      (setq rest (second scan-ret)))
                    nil)
                (if (and (not (null rest)) (dbms-token-is-keyword-p (first rest) "RETURNING"))
                    (progn
                      (setq returning-toks (cdr rest))
                      (setq rest '()))
                    nil)

                (let* ((simple-assign-r (dbms-parse-set-assignments set-toks))
                       (simple-where-r (if (null where-toks)
                                           (dbms-parser-ok '() '())
                                           (let ((r (dbms-parse-predicate where-toks)))
                                             (if (dbms-error-p r)
                                                 r
                                                 (if (null (dbms-parser-ok-rest r))
                                                     r
                                                     (dbms-parser-error "complex WHERE" where-toks))))))
                       (mvp-simple (and (null from-toks)
                                        (null table-alias)
                                        (null returning-toks)
                                        (not (dbms-error-p simple-assign-r))
                                        (not (dbms-error-p simple-where-r))))
                       )
                  (if mvp-simple
                      (dbms-parser-ok
                       (dbms-make-stmt
                        'update
                        (list (list "table" (dbms-parser-ok-value r-table))
                              (list "set" (dbms-parser-ok-value simple-assign-r))
                              (list "where" (dbms-parser-ok-value simple-where-r))))
                       rest)
                      (dbms-parser-ok
                       (dbms-make-stmt
                        'update-wiki
                        (list (list "table" (dbms-parser-ok-value r-table))
                              (list "alias" table-alias)
                              (list "set" set-toks)
                              (list "from" from-toks)
                              (list "where" where-toks)
                              (list "returning" returning-toks)))
                       rest)))))))))

(defun dbms-parse-delete (tokens)
  (let ((r-from (dbms-expect-keyword tokens "FROM")))
    (if (dbms-error-p r-from)
        r-from
        (let ((r-table (dbms-expect-identifier (dbms-parser-ok-rest r-from))))
          (if (dbms-error-p r-table)
              r-table
              (let ((rest (dbms-parser-ok-rest r-table))
                    (where-toks '()))
                (if (and (not (null rest)) (dbms-token-is-keyword-p (first rest) "WHERE"))
                    (progn
                      (setq where-toks (cdr rest))
                      (setq rest '()))
                    nil)
                (if (null where-toks)
                    (dbms-parser-ok
                     (dbms-make-stmt
                      'delete
                      (list (list "table" (dbms-parser-ok-value r-table))
                            (list "where" '())))
                     rest)
                    (let ((r-where (dbms-parse-predicate where-toks)))
                      (if (and (not (dbms-error-p r-where))
                               (null (dbms-parser-ok-rest r-where)))
                          (dbms-parser-ok
                           (dbms-make-stmt
                            'delete
                            (list (list "table" (dbms-parser-ok-value r-table))
                                  (list "where" (dbms-parser-ok-value r-where))))
                           rest)
                          (dbms-parser-ok
                           (dbms-make-stmt
                            'delete-wiki
                            (list (list "table" (dbms-parser-ok-value r-table))
                                  (list "where" where-toks)))
                           rest))))))))))

(defun dbms-parse-with (tokens)
  (let ((rest tokens)
        (ctes '())
        (failed '())
        (done nil))
    (while (and (null failed) (not done))
      (let ((r-name (dbms-expect-identifier rest)))
        (if (dbms-error-p r-name)
            (setq failed r-name)
            (let ((r-as (dbms-expect-keyword (dbms-parser-ok-rest r-name) "AS")))
              (if (dbms-error-p r-as)
                  (setq failed r-as)
                  (let ((r-subq (dbms-read-parenthesized-tokens (dbms-parser-ok-rest r-as))))
                    (if (dbms-error-p r-subq)
                        (setq failed r-subq)
                        (progn
                          (setq ctes (append ctes (list (list (dbms-parser-ok-value r-name)
                                                               (dbms-parser-ok-value r-subq)))))
                          (setq rest (dbms-parser-ok-rest r-subq))
                          (if (and (not (null rest)) (dbms-token-is-symbol-p (first rest) ","))
                              (setq rest (cdr rest))
                              (setq done t))))))))))
    (if (dbms-error-p failed)
        failed
        (let ((r-final (dbms-parse-statement rest)))
          (if (dbms-error-p r-final)
              r-final
              (dbms-parser-ok
               (dbms-make-stmt
                'with
                (list (list "ctes" ctes)
                      (list "statement" (dbms-parser-ok-value r-final))))
               (dbms-parser-ok-rest r-final)))))))

(defun dbms-parse-statement (tokens)
  (if (null tokens)
      (dbms-parser-error "empty token stream" '())
      (let ((tok (first tokens)))
        (cond
         ((dbms-token-is-keyword-p tok "WITH")
          (dbms-parse-with (cdr tokens)))
         ((dbms-token-is-keyword-p tok "BEGIN")
          (dbms-parser-ok (dbms-make-stmt 'begin '()) (cdr tokens)))
         ((dbms-token-is-keyword-p tok "COMMIT")
          (dbms-parser-ok (dbms-make-stmt 'commit '()) (cdr tokens)))
         ((dbms-token-is-keyword-p tok "ROLLBACK")
          (dbms-parser-ok (dbms-make-stmt 'rollback '()) (cdr tokens)))
         ((dbms-token-is-keyword-p tok "CREATE")
          (if (null (cdr tokens))
              (dbms-parser-error "CREATE requires TABLE or INDEX" 'eof)
              (if (dbms-token-is-keyword-p (second tokens) "TABLE")
                  (dbms-parse-create-table (cdr (cdr tokens)))
                  (if (dbms-token-is-keyword-p (second tokens) "INDEX")
                      (dbms-parse-create-index (cdr (cdr tokens)))
                      (dbms-parser-error "CREATE supports TABLE or INDEX" (second tokens))))))
         ((dbms-token-is-keyword-p tok "INSERT")
          (let ((r-into (dbms-expect-keyword (cdr tokens) "INTO")))
            (if (dbms-error-p r-into)
                r-into
                (dbms-parse-insert (dbms-parser-ok-rest r-into)))))
         ((dbms-token-is-keyword-p tok "SELECT")
          (dbms-parse-select (cdr tokens)))
         ((dbms-token-is-keyword-p tok "UPDATE")
          (dbms-parse-update (cdr tokens)))
         ((dbms-token-is-keyword-p tok "DELETE")
          (dbms-parse-delete (cdr tokens)))
         ((dbms-token-is-keyword-p tok "ALTER")
          (let ((r-table (dbms-expect-keyword (cdr tokens) "TABLE")))
            (if (dbms-error-p r-table)
                r-table
                (dbms-parse-alter-table (dbms-parser-ok-rest r-table)))))
         (t
          (dbms-parser-error "unsupported statement" (dbms-sql-token-lexeme tok)))))))

(defun dbms-parse-sql (sql-text)
  (if (or (null sql-text) (string= sql-text ""))
      (dbms-make-error 'dbms/parse-error "empty sql" sql-text)
      (let ((tokens (dbms-lex-sql sql-text)))
        (if (dbms-error-p tokens)
            tokens
            (let* ((tokens2 (dbms-drop-trailing-semicolon-token tokens))
                   (r-stmt (dbms-parse-statement tokens2)))
              (if (dbms-error-p r-stmt)
                  r-stmt
                  (let ((rest (dbms-parser-ok-rest r-stmt)))
                    (if (null rest)
                        (dbms-make-ast
                         (list (dbms-parser-ok-value r-stmt))
                         (list (list "profile" "MVP Core")))
                        (dbms-parser-error "unexpected trailing tokens"
                                           (dbms-sql-token-lexeme (first rest)))))))))))
