;; dbms/tests/prod/helpers/durability_check_worker.lsp

(load "./examples/dbms/app/repr.lsp")
(load "./examples/dbms/app/engine.lsp")
(load "./examples/dbms/app/storage.lsp")

(defun assert-true (label cond)
  (if cond
      t
      (progn
        (format t "assert failed: ~A~%" label)
        (throw 'dbms-check-failed label))))

(defun run-check ()
  (let ((catalog (dbms-engine-init))
        (r '())
        (rows '())
        (titles '()))
    (setq r (dbms-exec-sql catalog "SELECT id, title FROM pages ORDER BY id ASC;"))
    (assert-true "select rows result" (and (dbms-result-p r) (eq (second r) 'rows)))
    (setq rows (second (third r)))
    (assert-true "exactly one durable row" (= (length rows) 1))
    (setq titles (second (first rows)))
    (assert-true "durable title prefix" (and (stringp titles) (not (null (string-index "durability-" titles)))))
    (format t "durability check passed.~%")))

(catch 'dbms-check-failed
  (run-check))
