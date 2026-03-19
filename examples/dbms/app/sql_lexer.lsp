;; dbms/app/sql_lexer.lsp
;; SQLトークナイザの雛形（MVP Core）。

(load "./examples/dbms/app/repr.lsp")

(defglobal *dbms-sql-keywords*
  '("CREATE" "TABLE" "INSERT" "INTO" "VALUES" "SELECT" "FROM" "WHERE"
    "ORDER" "BY" "ASC" "DESC" "UPDATE" "SET" "DELETE"
    "PRIMARY" "KEY" "NOT"
    "INT" "INTEGER" "BIGINT" "BIGSERIAL" "TEXT" "BOOL" "TIMESTAMPTZ"
    "ALTER" "ADD" "DROP" "COLUMN" "CONSTRAINT" "CHECK" "FOREIGN" "REFERENCES"
    "INDEX" "IF" "EXISTS" "ON" "CONFLICT" "DO" "NOTHING" "RETURNING" "CASCADE"
    "WITH" "AS" "LIMIT" "JOIN" "INNER" "AND" "OR" "IS" "IN" "DEFAULT"
    "BEGIN" "COMMIT" "ROLLBACK" "ANALYZE" "EXPLAIN" "VACUUM" "VALIDATE" "VALID"
    "USER" "ROLE" "GRANT" "REVOKE" "ALL" "TO"))

(defglobal *dbms-sql-operator-2ch* '("!=" "<=" ">=" "<>"))
(defglobal *dbms-sql-operator-1ch* '("=" "<" ">" "+" "-" "*" "/" "%"))
(defglobal *dbms-sql-symbols* '("(" ")" "," ";" "."))

(defun dbms-sql-alpha-char-p (ch)
  (not (null (string-index ch "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))))

(defun dbms-sql-digit-char-p (ch)
  (not (null (string-index ch "0123456789"))))

(defun dbms-sql-ident-char-p (ch)
  (or (dbms-sql-alpha-char-p ch)
      (dbms-sql-digit-char-p ch)
      (string= ch "_")))

(defun dbms-sql-whitespace-char-p (ch)
  (not (null (string-index ch " \t\r\n"))))

(defun dbms-sql-ascii-upper-char (ch)
  (let ((i (string-index ch "abcdefghijklmnopqrstuvwxyz")))
    (if (null i)
        ch
        (substring "ABCDEFGHIJKLMNOPQRSTUVWXYZ" i (+ i 1)))))

(defun dbms-sql-upper (s)
  (let ((i 0)
        (n (length s))
        (out ""))
    (while (< i n)
      (setq out (string-append out (dbms-sql-ascii-upper-char (substring s i (+ i 1)))))
      (setq i (+ i 1)))
    out))

(defun dbms-sql-string-in-list-p (needle xs)
  (if (null xs)
      nil
      (if (string= needle (car xs))
          t
          (dbms-sql-string-in-list-p needle (cdr xs)))))

(defun dbms-sql-make-token (kind lexeme value)
  (list 'dbms-sql-token kind lexeme value))

(defun dbms-sql-token-kind (tok) (second tok))
(defun dbms-sql-token-lexeme (tok) (third tok))

(defun dbms-sql-parse-int (s)
  (let ((i 0)
        (n (length s))
        (sign 1)
        (acc 0))
    (if (and (> n 0) (string= (substring s 0 1) "-"))
        (progn
          (setq sign -1)
          (setq i 1))
        nil)
    (while (< i n)
      (let ((p (string-index (substring s i (+ i 1)) "0123456789")))
        (setq acc (+ (* acc 10) p)))
      (setq i (+ i 1)))
    (* sign acc)))

(defun dbms-sql-can-start-signed-number-p (last-token)
  (if (null last-token)
      t
      (let ((k (dbms-sql-token-kind last-token))
            (lx (dbms-sql-token-lexeme last-token)))
        (or (eq k 'operator)
            (and (eq k 'symbol)
                 (or (string= lx "(")
                     (string= lx ",")))))))

(defun dbms-lex-sql (sql-text)
  (if (or (null sql-text) (string= sql-text ""))
      '()
      (let ((i 0)
            (n (length sql-text))
            (tokens '())
            (last-token '())
            (error-result '()))
        (while (and (< i n) (null error-result))
          (let ((ch (substring sql-text i (+ i 1))))
            (cond
             ((dbms-sql-whitespace-char-p ch)
              (setq i (+ i 1)))

             ((or (dbms-sql-alpha-char-p ch) (string= ch "_"))
              (let ((start i))
                (setq i (+ i 1))
                (while (and (< i n) (dbms-sql-ident-char-p (substring sql-text i (+ i 1))))
                  (setq i (+ i 1)))
                (let* ((lexeme (substring sql-text start i))
                       (upper (dbms-sql-upper lexeme))
                       (tok
                        (if (string= upper "NULL")
                            (dbms-sql-make-token 'literal-null lexeme 'NULL)
                            (if (string= upper "TRUE")
                                (dbms-sql-make-token 'literal-bool lexeme t)
                                (if (string= upper "FALSE")
                                    (dbms-sql-make-token 'literal-bool lexeme nil)
                                    (if (dbms-sql-string-in-list-p upper *dbms-sql-keywords*)
                                        (dbms-sql-make-token 'keyword lexeme upper)
                                        (dbms-sql-make-token 'identifier lexeme lexeme)))))))
                  (setq tokens (cons tok tokens))
                  (setq last-token tok))))

             ((dbms-sql-digit-char-p ch)
              (let ((start i))
                (setq i (+ i 1))
                (while (and (< i n) (dbms-sql-digit-char-p (substring sql-text i (+ i 1))))
                  (setq i (+ i 1)))
                (let* ((lexeme (substring sql-text start i))
                       (tok (dbms-sql-make-token 'number lexeme (dbms-sql-parse-int lexeme))))
                  (setq tokens (cons tok tokens))
                  (setq last-token tok))))

             ((string= ch "'")
              (let ((start i)
                    (closed nil))
                (setq i (+ i 1))
                (while (and (< i n) (not closed))
                  (if (string= (substring sql-text i (+ i 1)) "'")
                      (setq closed t)
                      (setq i (+ i 1))))
                (if (not closed)
                    (setq error-result (dbms-make-error 'dbms/parse-error
                                                        "unterminated string literal"
                                                        (substring sql-text start n)))
                    (let* ((body (substring sql-text (+ start 1) i))
                           (lexeme (substring sql-text start (+ i 1)))
                           (tok (dbms-sql-make-token 'string lexeme body)))
                      (setq i (+ i 1))
                      (setq tokens (cons tok tokens))
                      (setq last-token tok)))))

             ((and (string= ch "-")
                   (< (+ i 1) n)
                   (dbms-sql-digit-char-p (substring sql-text (+ i 1) (+ i 2)))
                   (dbms-sql-can-start-signed-number-p last-token))
              (let ((start i))
                (setq i (+ i 1))
                (while (and (< i n) (dbms-sql-digit-char-p (substring sql-text i (+ i 1))))
                  (setq i (+ i 1)))
                (let* ((lexeme (substring sql-text start i))
                       (tok (dbms-sql-make-token 'number lexeme (dbms-sql-parse-int lexeme))))
                  (setq tokens (cons tok tokens))
                  (setq last-token tok))))

             ((and (< (+ i 1) n)
                   (dbms-sql-string-in-list-p (substring sql-text i (+ i 2))
                                              *dbms-sql-operator-2ch*))
              (let* ((lexeme (substring sql-text i (+ i 2)))
                     (tok (dbms-sql-make-token 'operator lexeme lexeme)))
                (setq i (+ i 2))
                (setq tokens (cons tok tokens))
                (setq last-token tok)))

             ((dbms-sql-string-in-list-p ch *dbms-sql-operator-1ch*)
              (let ((tok (dbms-sql-make-token 'operator ch ch)))
                (setq i (+ i 1))
                (setq tokens (cons tok tokens))
                (setq last-token tok)))

             ((dbms-sql-string-in-list-p ch *dbms-sql-symbols*)
              (let ((tok (dbms-sql-make-token 'symbol ch ch)))
                (setq i (+ i 1))
                (setq tokens (cons tok tokens))
                (setq last-token tok)))

             (t
              (setq error-result
                    (dbms-make-error 'dbms/parse-error
                                     "invalid token"
                                     (substring sql-text i (+ i 1))))))))
        (if (null error-result)
            (dbms-reverse tokens)
            error-result))))
