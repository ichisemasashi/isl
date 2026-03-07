;; dbms/tests/prod/t230-concurrency-harness.lsp
;; ST-001: multi-session (2-4 session) concurrency harness.

(load "./examples/dbms/app/repr.lsp")
(load "./examples/dbms/app/engine.lsp")
(load "./examples/dbms/tests/prod/helpers/concurrency_harness.lsp")

(defun assert-true (label cond)
  (if cond
      t
      (progn
        (format t "assert failed: ~A~%" label)
        (throw 'dbms-assert-failed label))))

(defun assert-scenario-ok (result)
  (if (eq (ch-run-result-status result) 'ok)
      t
      (progn
        (format t "scenario failed: ~A~%" (ch-run-result-name result))
        (format t "  detail: ~A~%" (ch-run-result-detail result))
        (ch-print-timeline (ch-run-result-timeline result))
        (throw 'dbms-assert-failed (ch-run-result-name result)))))

(defun run-tests ()
  (let* ((root-base (string-append "/tmp/dbms-concurrency-harness-" (format nil "~A" (get-universal-time))))
         (root root-base)
         (suffix 0)
         (catalog '())
         (r '())
         (scenario-r '()))
    (while (not (null (probe-file root)))
      (setq suffix (+ suffix 1))
      (setq root (string-append root-base "-" (format nil "~A" suffix))))
    (setenv "DBMS_STORAGE_ROOT" root)
    (setq catalog (dbms-engine-init))
    (dbms-engine-reset-tx-state)
    (dbms-lock-table-reset!)

    (setq r (dbms-exec-sql catalog "CREATE TABLE IF NOT EXISTS pages (id INT PRIMARY KEY, title TEXT NOT NULL);"))
    (assert-true "create pages ok" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "CREATE TABLE IF NOT EXISTS t1 (id INT PRIMARY KEY, v TEXT);"))
    (assert-true "create t1 ok" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "CREATE TABLE IF NOT EXISTS t2 (id INT PRIMARY KEY, v TEXT);"))
    (assert-true "create t2 ok" (and (dbms-result-p r) (eq (second r) 'ok)))

    ;; Scenario A (2 sessions): dirty-read equivalent conflict.
    (dbms-lock-table-reset!)
    (setq scenario-r
          (ch-run-scenario
           "dirty-read-guard"
           (list (ch-make-acquire-step 1001 "pages" 'X 'ok)
                 (ch-make-acquire-step 1002 "pages" 'S 'dbms/lock-conflict)
                 (ch-make-release-step 1001)
                 (ch-make-acquire-step 1002 "pages" 'S 'ok)
                 (ch-make-release-step 1002))))
    (assert-scenario-ok scenario-r)

    ;; Scenario B (3 sessions): lost-update equivalent write conflict + reader block.
    (dbms-lock-table-reset!)
    (setq scenario-r
          (ch-run-scenario
           "lost-update-guard"
           (list (ch-make-acquire-step 1101 "pages" 'X 'ok)
                 (ch-make-acquire-step 1102 "pages" 'X 'dbms/lock-conflict)
                 (ch-make-acquire-step 1103 "pages" 'S 'dbms/lock-conflict)
                 (ch-make-release-step 1101)
                 (ch-make-acquire-step 1102 "pages" 'X 'ok)
                 (ch-make-release-step 1102))))
    (assert-scenario-ok scenario-r)

    ;; Scenario C (2 sessions): deadlock cycle detection.
    (dbms-lock-table-reset!)
    (setq scenario-r
          (ch-run-scenario
           "deadlock-detection"
           (list (ch-make-acquire-step 1201 "t1" 'X 'ok)
                 (ch-make-acquire-step 1202 "t2" 'X 'ok)
                 (ch-make-acquire-step 1202 "t1" 'X 'dbms/lock-conflict)
                 (ch-make-acquire-step 1201 "t2" 'X 'dbms/deadlock-detected)
                 (ch-make-release-step 1201)
                 (ch-make-release-step 1202))))
    (assert-scenario-ok scenario-r)

    (format t "dbms concurrency harness tests passed.~%")))

(catch 'dbms-assert-failed
  (run-tests))

