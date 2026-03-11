;; dbms/tests/prod/t310-storage-maintenance.lsp
;; LT-003: autovacuum/auto-analyze/compaction scheduler/corruption policy.

(load "./examples/dbms/app/repr.lsp")
(load "./examples/dbms/app/storage.lsp")
(load "./examples/dbms/app/engine.lsp")

(defun assert-true (label cond)
  (if cond
      t
      (progn
        (format t "assert failed: ~A~%" label)
        (throw 'dbms-assert-failed label))))

(defun result-ok-p (r)
  (and (dbms-result-p r) (eq (second r) 'ok)))

(defun make-rows (from-id to-id)
  (let ((i from-id)
        (out '()))
    (while (<= i to-id)
      (setq out (append out
                        (list (dbms-make-row i
                                             (list (list "id" i)
                                                   (list "body" (string-append "v" (format nil "~A" i))))))))
      (setq i (+ i 1)))
    out))

(defun first-vacuum-table-name (tick-result)
  (let* ((payload (third tick-result))
         (vacuumed (dbms-assoc-get payload "vacuumed")))
    (if (or (null vacuumed) (null (car vacuumed)))
        ""
        (dbms-assoc-get (car vacuumed) "table"))))

(defun force-maint-last-check-zero ()
  (let ((m (dbms-storage-load-maint-meta)))
    (dbms-storage-save-maint-meta
     (list 'dbms-maint-meta
           (second m)
           (dbms-storage-maint-enabled-p m)
           (dbms-storage-maint-vacuum-frag-threshold m)
           (dbms-storage-maint-analyze-interval-sec m)
           (dbms-storage-maint-vacuum-interval-sec m)
           (dbms-storage-maint-corruption-check-interval-sec m)
           0
           (dbms-storage-maint-max-vacuum-tables-per-tick m)
           (dbms-storage-maint-table-states m)))))

(defun corrupt-first-page-checksum! (table-name)
  (let* ((pf (dbms-storage-load-table-pages-file table-name))
         (pages (dbms-data-page-file-pages pf))
         (pair (if (null pages) '() (car pages)))
         (page (if (null pair) '() (second pair)))
         (header (if (null page) '() (second page)))
         (slots (if (null page) '() (third page))))
    (if (or (null pair) (null page))
        (dbms-make-error 'dbms/not-implemented "no page to corrupt" table-name)
        (let* ((bad-header (dbms-make-data-page-header (second header)
                                                       (third header)
                                                       (+ (fourth header) 1)
                                                       (fifth header)
                                                       (dbms-sixth header)))
               (bad-page (dbms-make-data-page bad-header slots))
               (bad-pf (dbms-make-data-page-file (second pf)
                                                 (third pf)
                                                 (fourth pf)
                                                 (fifth pf)
                                                 (dbms-data-page-file-free-pages pf)
                                                 (cons (list (first pair) bad-page) (cdr pages)))))
          (dbms-storage-save-table-pages-file table-name bad-pf)))))

(defun run-tests ()
  (let* ((root-base (string-append "/tmp/dbms-maint-" (format nil "~A" (get-universal-time))))
         (root root-base)
         (suffix 0)
         (catalog '())
         (r '())
         (before-docs '())
         (after-docs '())
         (tick '())
         (status '())
         (m '()))
    (while (not (null (probe-file root)))
      (setq suffix (+ suffix 1))
      (setq root (string-append root-base "-" (format nil "~A" suffix))))
    (setenv "DBMS_STORAGE_ROOT" root)
    (setq catalog (dbms-engine-init))

    ;; unit-ish: scheduler order is deterministic.
    (assert-true "vacuum candidate sort unit"
                 (string=
                  (first (first (dbms-engine-maint-sort-vacuum-candidates
                                 (list (list "b" 0.1 10)
                                       (list "a" 0.9 10)
                                       (list "c" 0.9 10)))))
                  "a"))

    ;; setup tables.
    (setq r (dbms-exec-sql catalog "CREATE TABLE docs (id INT PRIMARY KEY, body TEXT NOT NULL);"))
    (assert-true "create docs" (result-ok-p r))
    (setq r (dbms-exec-sql catalog "CREATE TABLE logs (id INT PRIMARY KEY, body TEXT NOT NULL);"))
    (assert-true "create logs" (result-ok-p r))

    ;; docs: make high fragmentation by shrinking row-set after multi-page allocation.
    (assert-true "save docs 80 rows" (not (dbms-error-p (dbms-storage-save-table-rows "docs" (make-rows 1 80)))))
    (assert-true "save docs 10 rows" (not (dbms-error-p (dbms-storage-save-table-rows "docs" (make-rows 1 10)))))

    ;; logs: low fragmentation.
    (assert-true "save logs 10 rows" (not (dbms-error-p (dbms-storage-save-table-rows "logs" (make-rows 1 10)))))

    (setq before-docs (dbms-storage-table-pages-stats "docs"))
    (assert-true "docs fragmented before maintenance"
                 (> (dbms-assoc-get before-docs "fragmentation-ratio") 0))

    ;; enable maintenance: aggressive thresholds to trigger now.
    (setq r (dbms-admin-maintenance-configure t 0.05 1 1 1 1))
    (assert-true "maintenance configure" (result-ok-p r))

    ;; integration: one tick should vacuum top fragmented table and auto analyze.
    (setq tick (dbms-admin-maintenance-tick))
    (assert-true "maintenance tick ok" (result-ok-p tick))
    (assert-true "tick maintained action" (string= (dbms-assoc-get (third tick) "action") "MAINTAINED"))
    (assert-true "tick vacuumed one table" (= (dbms-assoc-get (third tick) "vacuumed-count") 1))
    (assert-true "scheduler picked docs first" (string= (first-vacuum-table-name tick) "docs"))
    (assert-true "tick analyzed some table" (> (dbms-assoc-get (third tick) "analyzed-count") 0))

    (setq after-docs (dbms-storage-table-pages-stats "docs"))
    (assert-true "docs frag reduced"
                 (<= (dbms-assoc-get after-docs "fragmentation-ratio")
                     (dbms-assoc-get before-docs "fragmentation-ratio")))

    ;; corruption policy: detect corruption and disable auto maintenance.
    (assert-true "corrupt pages file"
                 (not (dbms-error-p (corrupt-first-page-checksum! "docs"))))
    (assert-true "force corruption check due"
                 (not (dbms-error-p (force-maint-last-check-zero))))
    (setq tick (dbms-admin-maintenance-tick))
    (assert-true "tick reports corruption" (result-ok-p tick))
    (assert-true "corruption action" (string= (dbms-assoc-get (third tick) "action") "ERROR-DETECTED"))
    (assert-true "corruption reason" (string= (dbms-assoc-get (third tick) "reason") "CORRUPTION_DETECTED"))
    (assert-true "corruption count > 0" (> (dbms-assoc-get (third tick) "error-count") 0))

    (setq status (dbms-admin-maintenance-status))
    (assert-true "maintenance status after corruption" (result-ok-p status))
    (setq m (third status))
    (assert-true "maintenance auto-disabled after corruption"
                 (and (dbms-maint-meta-p m)
                      (null (dbms-storage-maint-enabled-p m))))

    (format t "dbms storage maintenance tests passed.~%")))

(catch 'dbms-assert-failed
  (run-tests))
