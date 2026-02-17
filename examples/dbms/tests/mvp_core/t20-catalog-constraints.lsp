;; dbms/tests/mvp_core/t20-catalog-constraints.lsp

(load "./examples/dbms/app/repr.lsp")
(load "./examples/dbms/app/catalog.lsp")

(defun assert-true (label cond)
  (if cond
      t
      (error (string-append "assert failed: " label))))

(defun run-tests ()
  (let* ((catalog (dbms-catalog-new))
         (pages-col-id (dbms-make-column-def "id" 'INT '(PRIMARY-KEY)))
         (pages-col-slug (dbms-make-column-def "slug" 'TEXT '(NOT-NULL UNIQUE)))
         (pages-check (dbms-make-constraint 'check "chk_slug" '(slug-not-blank "slug")))
         (pages-table (dbms-make-table-def "pages"
                                           (list pages-col-id pages-col-slug)
                                           (list pages-check)
                                           '()))
         (catalog2 (dbms-catalog-create-table catalog pages-table))

         (rev-col-id (dbms-make-column-def "id" 'INT '(PRIMARY-KEY)))
         (rev-col-page-id (dbms-make-column-def "page_id" 'INT '(NOT-NULL)))
         (rev-col-rev-no (dbms-make-column-def "rev_no" 'INT '(NOT-NULL)))
         (rev-uq (dbms-make-constraint 'unique "uq_page_rev" '("page_id" "rev_no")))
         (rev-fk (dbms-make-constraint 'foreign-key "fk_page"
                                       '(("page_id") "pages" ("id") CASCADE)))
         (revisions-table (dbms-make-table-def "page_revisions"
                                               (list rev-col-id rev-col-page-id rev-col-rev-no)
                                               (list rev-uq rev-fk)
                                               '()))
         (catalog3 (dbms-catalog-create-table catalog2 revisions-table))

         (dup-cols-table (dbms-make-table-def "dup_cols"
                                              (list (dbms-make-column-def "id" 'INT '())
                                                    (dbms-make-column-def "id" 'INT '()))
                                              '()
                                              '()))
         (dup-cols-err (dbms-catalog-create-table catalog3 dup-cols-table))

         (two-pk-table (dbms-make-table-def "two_pk"
                                            (list (dbms-make-column-def "a" 'INT '(PRIMARY-KEY))
                                                  (dbms-make-column-def "b" 'INT '(PRIMARY-KEY)))
                                            '()
                                            '()))
         (two-pk-err (dbms-catalog-create-table catalog3 two-pk-table))

         (mixed-pk-table (dbms-make-table-def
                          "mixed_pk"
                          (list (dbms-make-column-def "id" 'INT '(PRIMARY-KEY))
                                (dbms-make-column-def "slug" 'TEXT '(NOT-NULL)))
                          (list (dbms-make-constraint 'primary-key "pk_slug" '("slug")))
                          '()))
         (mixed-pk-err (dbms-catalog-create-table catalog3 mixed-pk-table))

         (bad-fk-table (dbms-make-table-def "bad_fk"
                                            (list (dbms-make-column-def "id" 'INT '(PRIMARY-KEY))
                                                  (dbms-make-column-def "page_id" 'INT '(NOT-NULL)))
                                            (list (dbms-make-constraint 'foreign-key "fk_missing"
                                                                        '(("page_id") "missing_table" ("id") CASCADE)))
                                            '()))
         (bad-fk-err (dbms-catalog-create-table catalog3 bad-fk-table))

         (pages-loaded (dbms-catalog-find-table catalog3 "pages")))

    (assert-true "catalog2 created" (dbms-catalog-p catalog2))
    (assert-true "catalog3 created" (dbms-catalog-p catalog3))
    (assert-true "pages exists" (dbms-table-def-p pages-loaded))

    ;; PK は NOT-NULL を含むよう正規化される。
    (assert-true "pk implies not-null"
                 (dbms-symbol-in-list-p 'NOT-NULL
                                        (dbms-column-def-attrs
                                         (first (dbms-table-def-columns pages-loaded)))))

    (assert-true "duplicate columns error" (dbms-error-p dup-cols-err))
    (assert-true "duplicate columns code"
                 (eq (dbms-error-code dup-cols-err) 'dbms/duplicate-column))

    (assert-true "two pk error" (dbms-error-p two-pk-err))
    (assert-true "two pk code"
                 (eq (dbms-error-code two-pk-err) 'dbms/primary-key-violation))

    (assert-true "mixed pk error" (dbms-error-p mixed-pk-err))
    (assert-true "mixed pk code"
                 (eq (dbms-error-code mixed-pk-err) 'dbms/primary-key-violation))

    (assert-true "bad fk error" (dbms-error-p bad-fk-err))
    (assert-true "bad fk code"
                 (eq (dbms-error-code bad-fk-err) 'dbms/table-not-found))

    (format t "dbms catalog constraints tests passed.~%")))

(run-tests)
