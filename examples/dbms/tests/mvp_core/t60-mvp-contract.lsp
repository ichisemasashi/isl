;; dbms/tests/mvp_core/t60-mvp-contract.lsp
;; SPEC.md の MVP 受け入れ基準を固定する統合テスト。

(load "./examples/dbms/app/repr.lsp")
(load "./examples/dbms/app/engine.lsp")

(defun assert-true (label cond)
  (if cond
      t
      (error (string-append "assert failed: " label))))

(defun result-kind (r) (second r))
(defun result-payload (r) (third r))
(defun result-error-code (r)
  (if (and (dbms-result-p r) (eq (result-kind r) 'error))
      (dbms-error-code (result-payload r))
      '()))

(defun run-tests ()
  (let* ((root (string-append "/tmp/dbms-mvp-contract-" (format nil "~A" (get-universal-time))))
         (catalog '())
         (r '()))
    (setenv "DBMS_STORAGE_ROOT" root)
    (setq catalog (dbms-engine-init))

    ;; 正常系5文: CREATE/INSERT/SELECT/UPDATE/DELETE
    (setq r (dbms-exec-sql catalog "CREATE TABLE pages (id INT PRIMARY KEY, title TEXT NOT NULL, published BOOL);"))
    (assert-true "CREATE TABLE ok" (and (dbms-result-p r) (eq (result-kind r) 'ok)))

    (setq r (dbms-exec-sql catalog "INSERT INTO pages VALUES (1, 'alpha', TRUE);"))
    (assert-true "INSERT count=1" (and (dbms-result-p r) (eq (result-kind r) 'count) (= (result-payload r) 1)))

    (setq r (dbms-exec-sql catalog "SELECT id, title FROM pages WHERE id = 1 ORDER BY title ASC;"))
    (assert-true "SELECT rows" (and (dbms-result-p r) (eq (result-kind r) 'rows)))
    (assert-true "SELECT one row" (= (length (second (result-payload r))) 1))

    (setq r (dbms-exec-sql catalog "UPDATE pages SET title='beta' WHERE id = 1;"))
    (assert-true "UPDATE count=1" (and (dbms-result-p r) (eq (result-kind r) 'count) (= (result-payload r) 1)))

    (setq r (dbms-exec-sql catalog "DELETE FROM pages WHERE id = 1;"))
    (assert-true "DELETE count=1" (and (dbms-result-p r) (eq (result-kind r) 'count) (= (result-payload r) 1)))

    ;; 永続化再起動（CREATE/INSERT 後）
    (setq r (dbms-exec-sql catalog "INSERT INTO pages VALUES (2, 'persist', FALSE);"))
    (assert-true "INSERT for restart" (and (dbms-result-p r) (eq (result-kind r) 'count)))
    (setq catalog (dbms-engine-init))
    (setq r (dbms-exec-sql catalog "SELECT id, title FROM pages WHERE id = 2;"))
    (assert-true "restart SELECT rows" (and (dbms-result-p r) (eq (result-kind r) 'rows)))
    (assert-true "restart row exists" (= (length (second (result-payload r))) 1))

    ;; 最小エラーコードを固定
    (setq r (dbms-exec-sql catalog "SELEC * FROM pages;"))
    (assert-true "dbms/parse-error" (eq (result-error-code r) 'dbms/parse-error))

    (setq r (dbms-exec-sql catalog "SELECT * FROM missing;"))
    (assert-true "dbms/table-not-found" (eq (result-error-code r) 'dbms/table-not-found))

    (setq r (dbms-exec-sql catalog "SELECT nope FROM pages;"))
    (assert-true "dbms/column-not-found" (eq (result-error-code r) 'dbms/column-not-found))

    (setq r (dbms-exec-sql catalog "CREATE TABLE pages (x INT);"))
    (assert-true "dbms/duplicate-table" (eq (result-error-code r) 'dbms/duplicate-table))

    (setq r (dbms-exec-sql catalog "CREATE TABLE dup_cols (id INT, id INT);"))
    (assert-true "dbms/duplicate-column" (eq (result-error-code r) 'dbms/duplicate-column))

    (setq r (dbms-exec-sql catalog "INSERT INTO pages VALUES ('bad', 'x', TRUE);"))
    (assert-true "dbms/type-mismatch" (eq (result-error-code r) 'dbms/type-mismatch))

    (setq r (dbms-exec-sql catalog "INSERT INTO pages VALUES (3, NULL, TRUE);"))
    (assert-true "dbms/not-null-violation" (eq (result-error-code r) 'dbms/not-null-violation))

    ;; PK重複
    (setq r (dbms-exec-sql catalog "INSERT INTO pages VALUES (2, 'dup', TRUE);"))
    (assert-true "dbms/primary-key-violation" (eq (result-error-code r) 'dbms/primary-key-violation))

    (setq r (dbms-exec-sql catalog "INSERT INTO pages VALUES (10, 'arity-only');"))
    (assert-true "dbms/arity-mismatch" (eq (result-error-code r) 'dbms/arity-mismatch))

    (format t "dbms MVP contract tests passed.~%")))

(run-tests)
