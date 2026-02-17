;; dbms/tests/mvp_core/t10-storage-persistence.lsp

(load "./examples/dbms/app/repr.lsp")
(load "./examples/dbms/app/storage.lsp")

(defun assert-true (label cond)
  (if cond
      t
      (error (string-append "assert failed: " label))))

(defun run-tests ()
  (let* ((root (string-append "/tmp/dbms-storage-test-" (format nil "~A" (get-universal-time))))
         (col (dbms-make-column-def "id" 'INT '(PRIMARY-KEY NOT-NULL)))
         (table-def (dbms-make-table-def "pages" (list col) '() '()))
         (catalog (dbms-catalog-add-table-def (dbms-empty-catalog) table-def))
         (rows (list (dbms-make-row 1 (list (list "id" 1)))
                     (dbms-make-row 2 (list (list "id" 2)))))
         (save-catalog-result '())
         (save-rows-result '())
         (loaded-catalog '())
         (loaded-state '())
         (catalog-path "")
         (table-path ""))
    (setenv "DBMS_STORAGE_ROOT" root)

    (setq save-catalog-result (dbms-storage-save-catalog catalog))
    (assert-true "save catalog ok" (dbms-catalog-p save-catalog-result))

    (setq catalog-path (dbms-storage-catalog-path))
    (assert-true "catalog file exists" (not (null (probe-file catalog-path))))

    (setq loaded-catalog (dbms-storage-load-catalog))
    (assert-true "load catalog" (dbms-catalog-p loaded-catalog))
    (assert-true "find table in loaded catalog"
                 (dbms-table-def-p (dbms-catalog-find-table-def loaded-catalog "pages")))

    (setq save-rows-result (dbms-storage-save-table-rows "pages" rows))
    (assert-true "save rows ok" (dbms-table-state-p save-rows-result))

    (setq table-path (dbms-storage-table-path "pages"))
    (assert-true "table file exists" (not (null (probe-file table-path))))

    (setq loaded-state (dbms-storage-load-table-rows "pages"))
    (assert-true "load table rows" (dbms-table-state-p loaded-state))
    (assert-true "row count" (= (length (third loaded-state)) 2))

    (format t "dbms storage persistence tests passed.~%")))

(run-tests)
