;; dbms/tests/mvp_core/t70-btree-page-format.lsp

(load "./examples/dbms/app/repr.lsp")

(defun assert-true (label cond)
  (if cond
      t
      (progn
        (format t "assert failed: ~A~%" label)
        (throw 'dbms-assert-failed label))))

(defun run-tests ()
  (let* ((h (dbms-make-btree-page-header 1 10 12345 64 4096 '()))
         (leaf-e1 (dbms-make-btree-leaf-entry 10 '(pages 1)))
         (leaf-e2 (dbms-make-btree-leaf-entry 20 '(pages 2)))
         (leaf (dbms-make-btree-page h 'leaf (list leaf-e1 leaf-e2) '() 2))
         (ih (dbms-make-btree-page-header 2 11 555 64 2048 '(root)))
         (ie1 (dbms-make-btree-internal-entry 20 8))
         (ie2 (dbms-make-btree-internal-entry 40 9))
         (internal (dbms-make-btree-page ih 'internal (list ie1 ie2) 7 '()))
         (bad-h (dbms-make-btree-page-header 3 0 1 3000 2000 '()))
         (bad-page (dbms-make-btree-page h 'leaf (list ie1) '() '())))
    (assert-true "header valid" (dbms-btree-page-header-p h))
    (assert-true "leaf entry valid" (dbms-btree-leaf-entry-p leaf-e1))
    (assert-true "internal entry valid" (dbms-btree-internal-entry-p ie1))
    (assert-true "leaf page valid" (dbms-btree-page-p leaf))
    (assert-true "internal page valid" (dbms-btree-page-p internal))
    (assert-true "bad header invalid" (not (dbms-btree-page-header-p bad-h)))
    (assert-true "bad page invalid" (not (dbms-btree-page-p bad-page)))
    (assert-true "leaf capacity positive" (> (dbms-btree-page-capacity-estimate 'leaf) 0))
    (assert-true "internal capacity positive" (> (dbms-btree-page-capacity-estimate 'internal) 0))
    (format t "dbms btree page format tests passed.~%")))

(catch 'dbms-assert-failed
  (run-tests))
