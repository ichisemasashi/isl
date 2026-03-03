;; dbms/tests/mvp_core/t30-sql-lexer.lsp

(load "./examples/dbms/app/repr.lsp")
(load "./examples/dbms/app/sql_lexer.lsp")

(defun assert-true (label cond)
  (if cond
      t
      (error (string-append "assert failed: " label))))

(defun token-kind (tok) (second tok))
(defun token-lexeme (tok) (third tok))
(defun token-value (tok) (fourth tok))

(defun nth-token (tokens idx)
  (if (= idx 0)
      (first tokens)
      (nth-token (cdr tokens) (- idx 1))))

(defun run-tests ()
  (let* ((t1 (dbms-lex-sql "SELECT id, name FROM pages WHERE id >= 10;"))
         (t2 (dbms-lex-sql "INSERT INTO t VALUES (NULL, TRUE, FALSE, 'abc', -42);"))
         (t3 (dbms-lex-sql "UPDATE t SET a=1, b!=2, c<=3, d<>4, e>5, f<6;"))
         (t4 (dbms-lex-sql "select 'abc")))

    (assert-true "t1 tokenized" (not (dbms-error-p t1)))
    (assert-true "SELECT keyword" (eq (token-kind (nth-token t1 0)) 'keyword))
    (assert-true "id identifier" (eq (token-kind (nth-token t1 1)) 'identifier))
    (assert-true "comma symbol" (eq (token-kind (nth-token t1 2)) 'symbol))
    (assert-true "FROM keyword case-insensitive"
                 (eq (token-kind (nth-token t1 4)) 'keyword))
    (assert-true ">= operator" (string= (token-lexeme (nth-token t1 8)) ">="))
    (assert-true "number kind" (eq (token-kind (nth-token t1 9)) 'number))
    (assert-true "number value" (= (token-value (nth-token t1 9)) 10))

    (assert-true "t2 tokenized" (not (dbms-error-p t2)))
    (assert-true "NULL literal" (eq (token-kind (nth-token t2 5)) 'literal-null))
    (assert-true "TRUE literal" (eq (token-kind (nth-token t2 7)) 'literal-bool))
    (assert-true "TRUE value" (eq (token-value (nth-token t2 7)) t))
    (assert-true "FALSE literal" (eq (token-kind (nth-token t2 9)) 'literal-bool))
    (assert-true "FALSE value" (null (token-value (nth-token t2 9))))
    (assert-true "string literal kind" (eq (token-kind (nth-token t2 11)) 'string))
    (assert-true "string literal body" (string= (token-value (nth-token t2 11)) "abc"))
    (assert-true "signed number kind" (eq (token-kind (nth-token t2 13)) 'number))
    (assert-true "signed number value" (= (token-value (nth-token t2 13)) -42))

    (assert-true "t3 tokenized" (not (dbms-error-p t3)))
    (assert-true "not-equal !=" (string= (token-lexeme (nth-token t3 8)) "!="))
    (assert-true "less-equal <=" (string= (token-lexeme (nth-token t3 12)) "<="))
    (assert-true "not-equal <> " (string= (token-lexeme (nth-token t3 16)) "<>"))
    (assert-true "greater >" (string= (token-lexeme (nth-token t3 20)) ">"))
    (assert-true "less <" (string= (token-lexeme (nth-token t3 24)) "<"))

    (assert-true "unterminated string is parse error" (dbms-error-p t4))
    (assert-true "error code parse-error" (eq (dbms-error-code t4) 'dbms/parse-error))

    (format t "dbms sql lexer tests passed.~%")))

(run-tests)
