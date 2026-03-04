;; dbms/tests/mvp_core/t50-engine-mvp.lsp

(load "./examples/dbms/app/repr.lsp")
(load "./examples/dbms/app/engine.lsp")

(defun assert-true (label cond)
  (if cond
      t
      (error (string-append "assert failed: " label))))

(defun run-tests ()
  (let* ((root (string-append "/tmp/dbms-engine-test-" (format nil "~A" (get-universal-time))))
         (catalog '())
         (r '()))
    (setenv "DBMS_STORAGE_ROOT" root)
    (setq catalog (dbms-engine-init))

    ;; CREATE TABLE
    (setq r (dbms-exec-sql catalog "CREATE TABLE pages (id INT PRIMARY KEY, title TEXT NOT NULL, published BOOL);"))
    (assert-true "create table ok" (and (dbms-result-p r) (eq (second r) 'ok)))

    ;; INSERT x3
    (setq r (dbms-exec-sql catalog "INSERT INTO pages VALUES (1, 'zeta', TRUE);"))
    (assert-true "insert1 count" (and (dbms-result-p r) (eq (second r) 'count) (= (third r) 1)))
    (setq r (dbms-exec-sql catalog "INSERT INTO pages VALUES (2, 'alpha', FALSE);"))
    (assert-true "insert2 count" (and (dbms-result-p r) (eq (second r) 'count) (= (third r) 1)))
    (setq r (dbms-exec-sql catalog "INSERT INTO pages VALUES (3, 'mid', TRUE);"))
    (assert-true "insert3 count" (and (dbms-result-p r) (eq (second r) 'count) (= (third r) 1)))

    ;; SELECT WHERE + ORDER BY
    (setq r (dbms-exec-sql catalog "SELECT id, title FROM pages WHERE id >= 2 ORDER BY title ASC;"))
    (assert-true "select rows result kind" (and (dbms-result-p r) (eq (second r) 'rows)))
    (assert-true "select columns shape" (= (length (first (third r))) 2))
    (assert-true "select rows count" (= (length (second (third r))) 2))
    (assert-true "order by asc first row id=2"
                 (= (first (first (second (third r)))) 2))

    ;; UPDATE
    (setq r (dbms-exec-sql catalog "UPDATE pages SET title='beta' WHERE id = 2;"))
    (assert-true "update count" (and (dbms-result-p r) (eq (second r) 'count) (= (third r) 1)))

    ;; DELETE
    (setq r (dbms-exec-sql catalog "DELETE FROM pages WHERE id = 1;"))
    (assert-true "delete count" (and (dbms-result-p r) (eq (second r) 'count) (= (third r) 1)))

    ;; arity mismatch
    (setq r (dbms-exec-sql catalog "INSERT INTO pages VALUES (10, 'x');"))
    (assert-true "arity mismatch error" (and (dbms-result-p r)
                                             (eq (second r) 'error)
                                             (eq (dbms-error-code (third r)) 'dbms/arity-mismatch)))

    ;; pk violation
    (setq r (dbms-exec-sql catalog "INSERT INTO pages VALUES (2, 'dup', TRUE);"))
    (assert-true "pk violation error" (and (dbms-result-p r)
                                           (eq (second r) 'error)
                                           (eq (dbms-error-code (third r)) 'dbms/primary-key-violation)))

    ;; not null violation
    (setq r (dbms-exec-sql catalog "INSERT INTO pages VALUES (4, NULL, TRUE);"))
    (assert-true "not-null violation error" (and (dbms-result-p r)
                                                 (eq (second r) 'error)
                                                 (eq (dbms-error-code (third r)) 'dbms/not-null-violation)))

    ;; type mismatch
    (setq r (dbms-exec-sql catalog "INSERT INTO pages VALUES ('bad', 'x', TRUE);"))
    (assert-true "type mismatch error" (and (dbms-result-p r)
                                            (eq (second r) 'error)
                                            (eq (dbms-error-code (third r)) 'dbms/type-mismatch)))

    ;; table not found
    (setq r (dbms-exec-sql catalog "SELECT * FROM missing;"))
    (assert-true "table not found error" (and (dbms-result-p r)
                                              (eq (second r) 'error)
                                              (eq (dbms-error-code (third r)) 'dbms/table-not-found)))

    ;; column not found
    (setq r (dbms-exec-sql catalog "SELECT nope FROM pages;"))
    (assert-true "column not found error" (and (dbms-result-p r)
                                               (eq (second r) 'error)
                                               (eq (dbms-error-code (third r)) 'dbms/column-not-found)))

    (format t "dbms engine mvp tests passed.~%")))

(run-tests)
