;; dbms/tests/wiki_compat_v1/t30-ddl-compat.lsp
;; DDL互換: CREATE TABLE/INDEX IF NOT EXISTS, ALTER TABLE DROP/ADD CONSTRAINT,
;; CHECK, FOREIGN KEY ... ON DELETE CASCADE|SET NULL

(load "./examples/dbms/app/repr.lsp")
(load "./examples/dbms/app/engine.lsp")

(defun assert-true (label cond)
  (if cond
      t
      (error (string-append "assert failed: " label))))

(defun run-tests ()
  (let* ((root (string-append "/tmp/dbms-wiki-ddl-" (format nil "~A" (get-universal-time))))
         (catalog (dbms-engine-init))
         (r '())
         (rows '()))
    (setenv "DBMS_STORAGE_ROOT" root)
    (setq catalog (dbms-engine-init))

    ;; CREATE TABLE IF NOT EXISTS
    (setq r (dbms-exec-sql catalog "CREATE TABLE IF NOT EXISTS parents (id BIGSERIAL PRIMARY KEY, name TEXT NOT NULL);"))
    (assert-true "create parents #1" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "CREATE TABLE IF NOT EXISTS parents (id BIGSERIAL PRIMARY KEY, name TEXT NOT NULL);"))
    (assert-true "create parents #2 no error" (and (dbms-result-p r) (eq (second r) 'ok)))

    (setq r (dbms-exec-sql catalog "CREATE TABLE child_c (id BIGSERIAL PRIMARY KEY, parent_id BIGINT, v TEXT);"))
    (assert-true "create child_c" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "CREATE TABLE child_n (id BIGSERIAL PRIMARY KEY, parent_id BIGINT, v TEXT);"))
    (assert-true "create child_n" (and (dbms-result-p r) (eq (second r) 'ok)))

    ;; CREATE INDEX IF NOT EXISTS (no-op success)
    (setq r (dbms-exec-sql catalog "CREATE INDEX IF NOT EXISTS idx_parents_name ON parents(name);"))
    (assert-true "create index if not exists" (and (dbms-result-p r) (eq (second r) 'ok)))

    ;; CHECK constraint add/drop
    (setq r (dbms-exec-sql catalog "ALTER TABLE parents ADD CONSTRAINT chk_name CHECK (name != '');"))
    (assert-true "add check" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "INSERT INTO parents (name) VALUES ('');"))
    (assert-true "check violation error"
                 (and (dbms-result-p r) (eq (second r) 'error)))
    (setq r (dbms-exec-sql catalog "ALTER TABLE parents DROP CONSTRAINT IF EXISTS chk_name;"))
    (assert-true "drop check if exists" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "INSERT INTO parents (name) VALUES ('');"))
    (assert-true "insert after drop check" (and (dbms-result-p r) (eq (second r) 'count)))

    ;; FK ON DELETE CASCADE / SET NULL
    (setq r (dbms-exec-sql catalog "ALTER TABLE child_c ADD CONSTRAINT fk_child_c FOREIGN KEY (parent_id) REFERENCES parents(id) ON DELETE CASCADE;"))
    (assert-true "add fk cascade" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "ALTER TABLE child_n ADD CONSTRAINT fk_child_n FOREIGN KEY (parent_id) REFERENCES parents(id) ON DELETE SET NULL;"))
    (assert-true "add fk set null" (and (dbms-result-p r) (eq (second r) 'ok)))

    ;; insert parent id=2 and child rows referencing it.
    (setq r (dbms-exec-sql catalog "INSERT INTO parents (name) VALUES ('p2');"))
    (assert-true "insert parent p2" (and (dbms-result-p r) (eq (second r) 'count)))
    (setq r (dbms-exec-sql catalog "INSERT INTO child_c (parent_id, v) VALUES (2, 'c1');"))
    (assert-true "insert child_c" (and (dbms-result-p r) (eq (second r) 'count)))
    (setq r (dbms-exec-sql catalog "INSERT INTO child_n (parent_id, v) VALUES (2, 'n1');"))
    (assert-true "insert child_n" (and (dbms-result-p r) (eq (second r) 'count)))

    ;; delete parent id=2 triggers cascade/set-null.
    (setq r (dbms-exec-sql catalog "DELETE FROM parents WHERE id = 2;"))
    (assert-true "delete parent" (and (dbms-result-p r) (eq (second r) 'count) (= (third r) 1)))

    (setq r (dbms-exec-sql catalog "SELECT * FROM child_c;"))
    (assert-true "child_c select rows" (and (dbms-result-p r) (eq (second r) 'rows)))
    (setq rows (second (third r)))
    (assert-true "child_c cascaded delete" (= (length rows) 0))

    (setq r (dbms-exec-sql catalog "SELECT * FROM child_n;"))
    (assert-true "child_n select rows" (and (dbms-result-p r) (eq (second r) 'rows)))
    (setq rows (second (third r)))
    (assert-true "child_n row remains" (= (length rows) 1))
    (assert-true "child_n parent_id NULL after set null" (eq (second (first rows)) 'NULL))

    (format t "dbms wiki ddl compatibility tests passed.~%")))

(run-tests)
