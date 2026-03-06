;; dbms/tests/prod/t170-constraint-validation-phase.lsp
;; P4-004: NOT VALID + VALIDATE CONSTRAINT phase separation.

(load "./examples/dbms/app/repr.lsp")
(load "./examples/dbms/app/engine.lsp")

(defun assert-true (label cond)
  (if cond
      t
      (progn
        (format t "assert failed: ~A~%" label)
        (throw 'dbms-assert-failed label))))

(defun run-tests ()
  (let* ((root-base (string-append "/tmp/dbms-constraint-phase-" (format nil "~A" (get-universal-time))))
         (root root-base)
         (suffix 0)
         (catalog '())
         (r '()))
    (while (not (null (probe-file root)))
      (setq suffix (+ suffix 1))
      (setq root (string-append root-base "-" (format nil "~A" suffix))))
    (setenv "DBMS_STORAGE_ROOT" root)

    (setq catalog (dbms-engine-init))
    (setq r (dbms-exec-sql catalog "CREATE TABLE parents (id INT PRIMARY KEY, name TEXT);"))
    (assert-true "create parents" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "CREATE TABLE child (id INT PRIMARY KEY, parent_id INT, v TEXT);"))
    (assert-true "create child" (and (dbms-result-p r) (eq (second r) 'ok)))

    ;; Seed existing rows that violate forthcoming FK.
    (setq r (dbms-exec-sql catalog "INSERT INTO child VALUES (1, 999, 'legacy-bad');"))
    (assert-true "insert legacy bad row" (and (dbms-result-p r) (eq (second r) 'count)))

    ;; ADD CONSTRAINT NOT VALID should be metadata-only success.
    (setq r (dbms-exec-sql catalog "ALTER TABLE child ADD CONSTRAINT fk_child_parent FOREIGN KEY (parent_id) REFERENCES parents(id) ON DELETE CASCADE NOT VALID;"))
    (assert-true "add fk not valid ok" (and (dbms-result-p r) (eq (second r) 'ok)))

    ;; New violating writes are still checked.
    (setq r (dbms-exec-sql catalog "INSERT INTO child VALUES (2, 888, 'new-bad');"))
    (assert-true "new violating row rejected kind" (and (dbms-result-p r) (eq (second r) 'error)))
    (assert-true "new violating row rejected code" (eq (dbms-error-code (third r)) 'dbms/table-not-found))

    ;; Existing violation blocks VALIDATE CONSTRAINT.
    (setq r (dbms-exec-sql catalog "ALTER TABLE child VALIDATE CONSTRAINT fk_child_parent;"))
    (assert-true "validate fails on legacy bad row" (and (dbms-result-p r) (eq (second r) 'error)))

    ;; Fix legacy row then validate succeeds.
    (setq r (dbms-exec-sql catalog "INSERT INTO parents VALUES (10, 'p10');"))
    (assert-true "insert parent for repair" (and (dbms-result-p r) (eq (second r) 'count)))
    (setq r (dbms-exec-sql catalog "UPDATE child SET parent_id = 10 WHERE id = 1;"))
    (assert-true "repair legacy row" (and (dbms-result-p r) (eq (second r) 'count) (= (third r) 1)))
    (setq r (dbms-exec-sql catalog "ALTER TABLE child VALIDATE CONSTRAINT fk_child_parent;"))
    (assert-true "validate after repair ok" (and (dbms-result-p r) (eq (second r) 'ok)))

    ;; Once validated, violating updates should fail (regular validated path).
    (setq r (dbms-exec-sql catalog "UPDATE child SET parent_id = 777 WHERE id = 1;"))
    (assert-true "validated fk blocks bad update kind" (and (dbms-result-p r) (eq (second r) 'error)))
    (assert-true "validated fk blocks bad update code" (eq (dbms-error-code (third r)) 'dbms/table-not-found))

    ;; Default ADD CONSTRAINT (without NOT VALID) validates existing rows immediately.
    (setq r (dbms-exec-sql catalog "CREATE TABLE tcheck (id INT PRIMARY KEY, v TEXT);"))
    (assert-true "create tcheck" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "INSERT INTO tcheck VALUES (1, '');"))
    (assert-true "insert violating check seed row" (and (dbms-result-p r) (eq (second r) 'count)))
    (setq r (dbms-exec-sql catalog "ALTER TABLE tcheck ADD CONSTRAINT chk_v CHECK (v != '');"))
    (assert-true "add check default validates existing rows" (and (dbms-result-p r) (eq (second r) 'error)))

    (format t "dbms constraint validation phase tests passed.~%")))

(catch 'dbms-assert-failed
  (run-tests))
