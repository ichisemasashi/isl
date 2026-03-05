;; dbms/tests/prod/t99-unique-pk-index-integration.lsp
;; P3-003: integrate PK/UNIQUE checks via index path.

(load "./examples/dbms/app/repr.lsp")
(load "./examples/dbms/app/engine.lsp")
(load "./examples/dbms/app/storage.lsp")

(defun assert-true (label cond)
  (if cond
      t
      (progn
        (format t "assert failed: ~A~%" label)
        (throw 'dbms-assert-failed label))))

(defun run-tests ()
  (let* ((root (string-append "/tmp/dbms-p3-003-" (format nil "~A" (get-universal-time))))
         (catalog '())
         (r '())
         (table-def '())
         (pk-idx '())
         (s-users (dbms-make-stmt
                   'create-table
                   (dbms-make-table-def
                    "users"
                    (list (dbms-make-column-def "id" 'INT '(PRIMARY-KEY NOT-NULL))
                          (dbms-make-column-def "email" 'TEXT '(UNIQUE)))
                    '()
                    '()))))
    (setenv "DBMS_STORAGE_ROOT" root)
    (setq catalog (dbms-engine-init))

    ;; SQL path: PK table + duplicate check
    (setq r (dbms-exec-sql catalog "CREATE TABLE items (id INT PRIMARY KEY, name TEXT NOT NULL);"))
    (assert-true "create items ok" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq table-def (dbms-catalog-find-table (dbms-engine-current-catalog) "items"))
    (assert-true "items table exists" (dbms-table-def-p table-def))
    (assert-true "implicit pk index metadata exists"
                 (not (null (dbms-table-find-index-by-name (dbms-table-indexes table-def)
                                                           "__pk__items__id"))))
    (setq pk-idx (dbms-storage-load-index "items" "__pk__items__id"))
    (assert-true "implicit pk index file exists" (dbms-btree-index-p pk-idx))

    (setq r (dbms-exec-sql catalog "INSERT INTO items VALUES (1, 'a');"))
    (assert-true "insert items #1" (and (dbms-result-p r) (eq (second r) 'count) (= (third r) 1)))
    (setq r (dbms-exec-sql catalog "INSERT INTO items VALUES (1, 'dup');"))
    (assert-true "pk violation via index path"
                 (and (dbms-result-p r)
                      (eq (second r) 'error)
                      (eq (dbms-error-code (third r)) 'dbms/primary-key-violation)))

    ;; Direct table-def path: UNIQUE attribute enforcement
    (setq r (dbms-engine-create-table (dbms-engine-current-catalog) s-users))
    (assert-true "create users with unique attr" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq table-def (dbms-catalog-find-table (dbms-engine-current-catalog) "users"))
    (assert-true "users implicit unique index metadata"
                 (not (null (dbms-table-find-index-by-name (dbms-table-indexes table-def)
                                                           "__uq__users__email"))))

    (setq r (dbms-exec-sql catalog "INSERT INTO users VALUES (1, 'a@example.com');"))
    (assert-true "insert users #1" (and (dbms-result-p r) (eq (second r) 'count)))
    (setq r (dbms-exec-sql catalog "INSERT INTO users VALUES (2, 'a@example.com');"))
    (assert-true "unique violation via index path"
                 (and (dbms-result-p r)
                      (eq (second r) 'error)
                      (eq (dbms-error-code (third r)) 'dbms/primary-key-violation)))
    ;; SQL-compatible UNIQUE semantics: NULL duplicates allowed.
    (setq r (dbms-exec-sql catalog "INSERT INTO users VALUES (3, NULL);"))
    (assert-true "unique NULL #1 allowed" (and (dbms-result-p r) (eq (second r) 'count)))
    (setq r (dbms-exec-sql catalog "INSERT INTO users VALUES (4, NULL);"))
    (assert-true "unique NULL #2 allowed" (and (dbms-result-p r) (eq (second r) 'count)))

    (format t "dbms unique/pk index integration tests passed.~%")))

(catch 'dbms-assert-failed
  (run-tests))
