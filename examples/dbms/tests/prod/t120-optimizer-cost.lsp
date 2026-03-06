;; dbms/tests/prod/t120-optimizer-cost.lsp
;; P3-006: minimal optimizer chooses scan by cost comparison.

(load "./examples/dbms/app/repr.lsp")
(load "./examples/dbms/app/engine.lsp")

(defun assert-true (label cond)
  (if cond
      t
      (progn
        (format t "assert failed: ~A~%" label)
        (throw 'dbms-assert-failed label))))

(defun make-test-table-def (row-count ndv-id ndv-v)
  (let* ((base (dbms-make-table-def
                "t"
                (list (dbms-make-column-def "id" 'INT '(PRIMARY-KEY NOT-NULL))
                      (dbms-make-column-def "v" 'INT '()))
                '()
                '()))
         (with-idx (dbms-ensure-constraint-indexes-on-table-def base))
         (stats (list (list "row-count" row-count)
                      (list "columns"
                            (list (list (list "name" "id") (list "ndv" ndv-id) (list "null-fraction" 0))
                                  (list (list "name" "v") (list "ndv" ndv-v) (list "null-fraction" 0)))))))
    (dbms-table-def-with-stats with-idx stats)))

(defun run-tests ()
  (let* ((table-def (make-test-table-def 10000 10000 100))
         (small-table-def (make-test-table-def 8 8 4))
         (eq-probe (list "id" 10 'INT))
         (range-probe (list "id" ">=" 500 'INT))
         (choice '()))
    ;; large table + highly selective equality should prefer INDEX
    (setq choice (dbms-optimizer-choose-scan table-def 10000 t eq-probe '() "" "id" 'ASC '()))
    (assert-true "eq selective uses index" (string= choice "INDEX"))

    ;; range + ORDER BY same key + LIMIT should prefer INDEX
    (setq choice (dbms-optimizer-choose-scan table-def 10000 t '() range-probe "id" "id" 'ASC 20))
    (assert-true "range order/limit uses index" (string= choice "INDEX"))

    ;; very small table should prefer SEQ even if index exists
    (setq choice (dbms-optimizer-choose-scan small-table-def 8 t eq-probe '() "" "id" 'ASC '()))
    (assert-true "small table uses seq" (string= choice "SEQ"))

    ;; cannot use index candidate => SEQ
    (setq choice (dbms-optimizer-choose-scan table-def 10000 nil eq-probe '() "" "id" 'ASC '()))
    (assert-true "ineligible falls back seq" (string= choice "SEQ"))

    (format t "dbms optimizer cost tests passed.~%")))

(catch 'dbms-assert-failed
  (run-tests))
