;; dbms/tests/prod/t150-vacuum-compaction.lsp
;; P4-002: vacuum compaction improves fragmentation metrics.

(load "./examples/dbms/app/repr.lsp")
(load "./examples/dbms/app/engine.lsp")
(load "./examples/dbms/app/storage.lsp")

(defun assert-true (label cond)
  (if cond
      t
      (error (string-append "assert failed: " label))))

(defun kv-get (pairs key)
  (if (null pairs)
      '()
      (if (string= (first (car pairs)) key)
          (second (car pairs))
          (kv-get (cdr pairs) key))))

(defun make-rows-range (start end)
  (if (> start end)
      '()
      (cons (dbms-make-row start (list (list "id" start)))
            (make-rows-range (+ start 1) end))))

(defun run-tests ()
  (let* ((root-base (string-append "/tmp/dbms-vacuum-" (format nil "~A" (get-universal-time))))
         (root root-base)
         (suffix 0)
         (catalog '())
         (r '())
         (saved '())
         (before '())
         (after '())
         (rows '())
         (line '()))
    (while (not (null (probe-file root)))
      (setq suffix (+ suffix 1))
      (setq root (string-append root-base "-" (format nil "~A" suffix))))
    (setenv "DBMS_STORAGE_ROOT" root)

    (setq catalog (dbms-engine-init))
    (setq r (dbms-exec-sql catalog "CREATE TABLE pages (id INT PRIMARY KEY);"))
    (assert-true "create table ok" (and (dbms-result-p r) (eq (second r) 'ok)))

    ;; Build fragmentation by shrinking after a larger persisted state.
    (setq saved (dbms-storage-save-table-rows "pages" (make-rows-range 1 130)))
    (assert-true "seed rows save ok" (dbms-table-state-p saved))
    (setq saved (dbms-storage-save-table-rows "pages" (make-rows-range 1 5)))
    (assert-true "shrink rows save ok" (dbms-table-state-p saved))

    (setq before (dbms-storage-table-pages-stats "pages"))
    (assert-true "before empty pages > 0" (> (kv-get before "empty-page-count") 0))

    (setq r (dbms-exec-sql catalog "VACUUM pages;"))
    (assert-true "vacuum result rows" (and (dbms-result-p r) (eq (second r) 'rows)))
    (setq rows (second (third r)))
    (assert-true "vacuum rows count=1" (= (length rows) 1))
    (setq line (car rows))
    (assert-true "before-frag > after-frag (result row)" (> (second line) (third line)))
    (assert-true "reclaimed pages > 0 (result row)" (> (fourth line) 0))
    (assert-true "reclaimed dead tuples > 0 (result row)" (> (fifth line) 0))

    (setq after (dbms-storage-table-pages-stats "pages"))
    (assert-true "empty pages improved" (< (kv-get after "empty-page-count") (kv-get before "empty-page-count")))
    (assert-true "fragmentation improved" (< (kv-get after "fragmentation-ratio") (kv-get before "fragmentation-ratio")))

    ;; VACUUM without table name (all tables) should also succeed.
    (setq r (dbms-exec-sql catalog "VACUUM;"))
    (assert-true "vacuum all ok" (and (dbms-result-p r) (eq (second r) 'rows)))

    ;; Current contract: VACUUM during active tx is rejected.
    (setq r (dbms-exec-sql catalog "BEGIN;"))
    (assert-true "begin for vacuum-tx guard" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "VACUUM pages;"))
    (assert-true "vacuum in tx rejected kind" (and (dbms-result-p r) (eq (second r) 'error)))
    (assert-true "vacuum in tx rejected code" (eq (dbms-error-code (third r)) 'dbms/not-implemented))
    (setq r (dbms-exec-sql catalog "ROLLBACK;"))
    (assert-true "rollback after vacuum guard" (and (dbms-result-p r) (eq (second r) 'ok)))

    (format t "dbms vacuum compaction tests passed.~%")))

(run-tests)
