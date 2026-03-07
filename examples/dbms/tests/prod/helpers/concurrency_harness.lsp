;; dbms/tests/prod/helpers/concurrency_harness.lsp
;; ST-001 concurrency test harness (virtual sessions via lock manager txid).

(defun ch-make-acquire-step (session-id table-name mode expected)
  ;; (acquire <session-id> <table-name> <mode:S|X> <expected:ok|dbms/*>)
  (list 'acquire session-id table-name mode expected))

(defun ch-make-release-step (session-id)
  ;; (release <session-id>)
  (list 'release session-id))

(defun ch-step-op (step) (first step))
(defun ch-step-session-id (step) (second step))
(defun ch-step-table-name (step) (third step))
(defun ch-step-mode (step) (fourth step))
(defun ch-step-expected (step) (fifth step))

(defun ch-actual-code-from-lock-result (r)
  (if (dbms-error-p r)
      (dbms-error-code r)
      'ok))

(defun ch-format-step (step)
  (if (eq (ch-step-op step) 'acquire)
      (string-append "acquire tx="
                     (format nil "~A" (ch-step-session-id step))
                     " table="
                     (ch-step-table-name step)
                     " mode="
                     (format nil "~A" (ch-step-mode step))
                     " expect="
                     (format nil "~A" (ch-step-expected step)))
      (string-append "release tx="
                     (format nil "~A" (ch-step-session-id step)))))

(defun ch-log-event (timeline index step actual)
  (append timeline
          (list (list "index" index)
                (list "step" (ch-format-step step))
                (list "actual" (format nil "~A" actual)))))

(defun ch-make-run-result (status name detail timeline)
  ;; (ch-run-result <status> <scenario-name> <detail> <timeline>)
  (list 'ch-run-result status name detail timeline))

(defun ch-run-result-status (r) (second r))
(defun ch-run-result-name (r) (third r))
(defun ch-run-result-detail (r) (fourth r))
(defun ch-run-result-timeline (r) (fifth r))

(defun ch-run-scenario (name steps)
  (let ((rest steps)
        (idx 1)
        (timeline '())
        (failed '()))
    (while (and (null failed) (not (null rest)))
      (let* ((step (car rest))
             (op (ch-step-op step))
             (sid (ch-step-session-id step)))
        (if (eq op 'acquire)
            (let* ((table (ch-step-table-name step))
                   (mode (ch-step-mode step))
                   (expected (ch-step-expected step))
                   (lock-r (dbms-lock-acquire sid table mode))
                   (actual (ch-actual-code-from-lock-result lock-r)))
              (setq timeline (ch-log-event timeline idx step actual))
              (if (eq actual expected)
                  nil
                  (setq failed
                        (list "unexpected lock acquire result"
                              (list "index" idx)
                              (list "expected" expected)
                              (list "actual" actual)))))
            (if (eq op 'release)
                (progn
                  (dbms-lock-release-tx sid)
                  (setq timeline (ch-log-event timeline idx step 'ok)))
                (progn
                  (setq timeline (ch-log-event timeline idx step 'unknown-op))
                  (setq failed
                        (list "unknown step op"
                              (list "index" idx)
                              (list "op" op)))))))
      (setq idx (+ idx 1))
      (setq rest (cdr rest)))
    (if (null failed)
        (ch-make-run-result 'ok name 'ok timeline)
        (ch-make-run-result 'error name failed timeline))))

(defun ch-print-timeline (timeline)
  (let ((rest timeline))
    (while (not (null rest))
      (let ((entry (car rest)))
        (format t "  [timeline] #~A ~A => ~A~%"
                (second (first entry))
                (second (second entry))
                (second (third entry))))
      (setq rest (cdr rest)))))

