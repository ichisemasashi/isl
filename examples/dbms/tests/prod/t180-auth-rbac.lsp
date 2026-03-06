;; dbms/tests/prod/t180-auth-rbac.lsp
;; P4-005: auth/role/privilege metadata + object-level permission checks.

(load "./examples/dbms/app/repr.lsp")
(load "./examples/dbms/app/engine.lsp")
(load "./examples/dbms/app/storage.lsp")

(defun assert-true (label cond)
  (if cond
      t
      (progn
        (format t "assert failed: ~A~%" label)
        (throw 'dbms-assert-failed label))))

(defun result-error-code (r)
  (if (and (dbms-result-p r)
           (eq (second r) 'error)
           (dbms-error-p (third r)))
      (dbms-error-code (third r))
      '()))

(defun run-tests ()
  (let* ((root-base (string-append "/tmp/dbms-auth-rbac-" (format nil "~A" (get-universal-time))))
         (root root-base)
         (suffix 0)
         (catalog '())
         (r '())
         (audit '()))
    (while (not (null (probe-file root)))
      (setq suffix (+ suffix 1))
      (setq root (string-append root-base "-" (format nil "~A" suffix))))
    (setenv "DBMS_STORAGE_ROOT" root)

    ;; bootstrap as admin session
    (setq catalog (dbms-engine-init))
    (assert-true "session admin" (string= (dbms-current-session-user) "admin"))

    (setq r (dbms-exec-sql catalog "CREATE TABLE pages (id INT PRIMARY KEY, title TEXT);"))
    (assert-true "create pages" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "INSERT INTO pages VALUES (1, 'hello');"))
    (assert-true "seed pages" (and (dbms-result-p r) (eq (second r) 'count) (= (third r) 1)))

    ;; create users/role and grant privileges
    (setq r (dbms-exec-sql catalog "CREATE USER alice;"))
    (assert-true "create user alice" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "CREATE USER bob;"))
    (assert-true "create user bob" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "CREATE ROLE editor;"))
    (assert-true "create role editor" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "GRANT ROLE editor TO alice;"))
    (assert-true "grant role editor to alice" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "GRANT SELECT ON pages TO editor;"))
    (assert-true "grant select on pages to editor" (and (dbms-result-p r) (eq (second r) 'ok)))

    ;; bob has no privileges
    (setq *dbms-session-user* "bob")
    (setq *dbms-session-active-role* "")
    (setq r (dbms-exec-sql catalog "SELECT id FROM pages WHERE id = 1;"))
    (assert-true "bob select denied kind" (and (dbms-result-p r) (eq (second r) 'error)))
    (assert-true "bob select denied code" (eq (result-error-code r) 'dbms/permission-denied))

    ;; alice has SELECT through editor role
    (setq *dbms-session-user* "alice")
    (setq *dbms-session-active-role* "")
    (setq r (dbms-exec-sql catalog "SELECT id FROM pages WHERE id = 1;"))
    (assert-true "alice select allowed" (and (dbms-result-p r) (eq (second r) 'rows)))

    ;; INSERT denied before grant
    (setq r (dbms-exec-sql catalog "INSERT INTO pages VALUES (2, 'n');"))
    (assert-true "alice insert denied before grant" (and (dbms-result-p r) (eq (second r) 'error)))
    (assert-true "alice insert denied code before grant" (eq (result-error-code r) 'dbms/permission-denied))

    ;; grant INSERT and verify allowed
    (setq *dbms-session-user* "admin")
    (setq r (dbms-exec-sql catalog "GRANT INSERT ON pages TO editor;"))
    (assert-true "grant insert on pages to editor" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq *dbms-session-user* "alice")
    (setq r (dbms-exec-sql catalog "INSERT INTO pages VALUES (2, 'n');"))
    (assert-true "alice insert allowed after grant" (and (dbms-result-p r) (eq (second r) 'count) (= (third r) 1)))

    ;; revoke SELECT and verify denied
    (setq *dbms-session-user* "admin")
    (setq r (dbms-exec-sql catalog "REVOKE SELECT ON pages FROM editor;"))
    (assert-true "revoke select" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq *dbms-session-user* "alice")
    (setq r (dbms-exec-sql catalog "SELECT id FROM pages WHERE id = 1;"))
    (assert-true "alice select denied after revoke" (and (dbms-result-p r) (eq (second r) 'error)))
    (assert-true "alice select denied code after revoke" (eq (result-error-code r) 'dbms/permission-denied))

    ;; denied operations are audited
    (setq audit (dbms-storage-load-audit-log))
    (assert-true "audit log has denied events" (>= (length audit) 3))

    (format t "dbms auth rbac tests passed.~%")))

(catch 'dbms-assert-failed
  (run-tests))
