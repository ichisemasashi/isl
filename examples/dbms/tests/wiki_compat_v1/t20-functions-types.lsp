;; dbms/tests/wiki_compat_v1/t20-functions-types.lsp
;; Wiki Compatibility v1: 関数・型の実装確認。

(load "./examples/dbms/app/repr.lsp")
(load "./examples/dbms/app/engine.lsp")

(defun assert-true (label cond)
  (if cond
      t
      (error (string-append "assert failed: " label))))

(defun run-tests ()
  (let* ((root (string-append "/tmp/dbms-wiki-fn-types-" (format nil "~A" (get-universal-time))))
         (catalog (dbms-engine-init))
         (r '())
         (rows '())
         (v '()))
    (setenv "DBMS_STORAGE_ROOT" root)
    (setq catalog (dbms-engine-init))

    (setq r (dbms-exec-sql
             catalog
             "CREATE TABLE wiki_types (id BIGSERIAL PRIMARY KEY, seq BIGINT, n INTEGER, slug TEXT NOT NULL, active BOOL, ts TIMESTAMPTZ);"))
    (assert-true "create wiki_types" (and (dbms-result-p r) (eq (second r) 'ok)))

    (setq r (dbms-exec-sql
             catalog
             "INSERT INTO wiki_types (seq, n, slug, active, ts) VALUES (10000000000, 7, LOWER('AbC'), COALESCE(NULL, TRUE), TO_CHAR(123456, 'YYYY-MM-DD HH24:MI:SS'));"))
    (assert-true "insert row1" (and (dbms-result-p r) (eq (second r) 'count) (= (third r) 1)))

    (setq r (dbms-exec-sql
             catalog
             "INSERT INTO wiki_types (seq, n, slug, active, ts) VALUES (10000000001, 8, LOWER('XYZ'), COALESCE(NULL, FALSE), TO_CHAR(234567, 'YYYY-MM-DD HH24:MI:SS'));"))
    (assert-true "insert row2" (and (dbms-result-p r) (eq (second r) 'count) (= (third r) 1)))

    (setq r (dbms-exec-sql catalog "SELECT * FROM wiki_types ORDER BY id ASC;"))
    (assert-true "select rows" (and (dbms-result-p r) (eq (second r) 'rows)))
    (setq rows (second (third r)))
    (assert-true "row count=2" (= (length rows) 2))
    (assert-true "bigserial id#1" (= (first (first rows)) 1))
    (assert-true "bigserial id#2" (= (first (second rows)) 2))
    (assert-true "LOWER value row1" (string= (fourth (first rows)) "abc"))
    (assert-true "LOWER value row2" (string= (fourth (second rows)) "xyz"))

    ;; 関数ヘルパ直接確認
    (setq v (dbms-eval-function "POSITION" (list (list 'TEXT "bc") (list 'TEXT "abcd"))))
    (assert-true "POSITION returns INT" (and (listp v) (eq (first v) 'INT) (= (second v) 2)))

    (setq v (dbms-eval-function "MAX" (list (list 'INT 3) (list 'INT 9) (list 'INT 5))))
    (assert-true "MAX returns INT 9" (and (listp v) (eq (first v) 'INT) (= (second v) 9)))

    (setq v (dbms-eval-function "COALESCE" (list (list 'NULL 'NULL) (list 'TEXT "x") (list 'TEXT "y"))))
    (assert-true "COALESCE returns first non-null" (and (eq (first v) 'TEXT) (string= (second v) "x")))

    (setq v (dbms-eval-function "TO_CHAR" (list (list 'TIMESTAMPTZ "2025-01-02 03:04:05+00") (list 'TEXT "YYYY-MM-DD HH24:MI:SS"))))
    (assert-true "TO_CHAR supported format" (and (eq (first v) 'TEXT) (string= (second v) "2025-01-02 03:04:05")))

    (format t "dbms wiki functions/types tests passed.~%")))

(run-tests)
