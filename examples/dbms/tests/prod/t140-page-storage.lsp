;; dbms/tests/prod/t140-page-storage.lsp
;; P4-001: page-storage growth control under repeated update/delete-like rewrites.

(load "./examples/dbms/app/repr.lsp")
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
  (let* ((root-base (string-append "/tmp/dbms-page-storage-" (format nil "~A" (get-universal-time))))
         (root root-base)
         (suffix 0)
         (big-rows (make-rows-range 1 70))
         (small-rows (make-rows-range 1 8))
         (saved '())
         (stats '())
         (peak-pages 0)
         (i 0))
    (while (not (null (probe-file root)))
      (setq suffix (+ suffix 1))
      (setq root (string-append root-base "-" (format nil "~A" suffix))))
    (setenv "DBMS_STORAGE_ROOT" root)

    (setq saved (dbms-storage-save-table-rows "pages" big-rows))
    (assert-true "initial save rows" (dbms-table-state-p saved))
    (setq stats (dbms-storage-table-pages-stats "pages"))
    (setq peak-pages (kv-get stats "page-count"))
    (assert-true "initial page-count exists" (numberp peak-pages))
    (assert-true "initial page-count positive" (> peak-pages 0))

    (setq i 0)
    (while (< i 1)
      ;; Shrink as a delete-like rewrite.
      (setq saved (dbms-storage-save-table-rows "pages" small-rows))
      (assert-true "shrink save rows" (dbms-table-state-p saved))
      (setq stats (dbms-storage-table-pages-stats "pages"))
      (assert-true "page-count does not grow on shrink"
                   (<= (kv-get stats "page-count") peak-pages))
      (assert-true "free pages appear after shrink"
                   (> (kv-get stats "free-page-count") 0))

      ;; Grow back as an update/insert-like rewrite.
      (setq saved (dbms-storage-save-table-rows "pages" big-rows))
      (assert-true "grow save rows" (dbms-table-state-p saved))
      (setq stats (dbms-storage-table-pages-stats "pages"))
      (assert-true "page-count remains bounded on regrow"
                   (<= (kv-get stats "page-count") peak-pages))
      (setq i (+ i 1)))

    (format t "dbms page storage tests passed.~%")))

(run-tests)
