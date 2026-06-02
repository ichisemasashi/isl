(load "./examples/dbms/app/repr.lsp")
(load "./examples/dbms/app/engine.lsp")
(load "./examples/dbms/app/storage.lsp")

(defun make-rows-range (start end)
  (if (> start end)
      '()
      (cons (dbms-make-row start (list (list "id" start)
                                       (list "title" (string-append "t" (format nil "~A" start)))))
            (make-rows-range (+ start 1) end))))

;; Test increasing row counts to find threshold
(defun test-repack-n (n)
  (format t "n=~A: " n)
  (let* ((root (string-append "/tmp/dbms-min-" (format nil "~A" n) "-" (format nil "~A" (get-universal-time))))
         (rows (make-rows-range 1 n))
         (pf (dbms-storage-new-page-file))
         (result (dbms-data-page-file-repack-with-rows pf rows)))
    (format t "~A pages~%" (length (dbms-data-page-file-pages result)))))

;; Run without engine context
(test-repack-n 50)
(test-repack-n 64)
(test-repack-n 65)
(test-repack-n 80)
(test-repack-n 100)
(test-repack-n 120)
(format t "ALL DONE~%")
