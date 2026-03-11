;; dbms/tests/prod/t320-security-hardening.lsp
;; LT-004: credential hashing, TLS policy, audit tamper detection.

(load "./examples/dbms/app/repr.lsp")
(load "./examples/dbms/app/engine.lsp")
(load "./examples/dbms/app/storage.lsp")

(defun assert-true (label cond)
  (if cond
      t
      (progn
        (format t "assert failed: ~A~%" label)
        (throw 'dbms-assert-failed label))))

(defun result-error-code (r)
  (if (and (dbms-result-p r)
           (eq (second r) 'error)
           (dbms-error-p (third r)))
      (dbms-error-code (third r))
      '()))

(defun corrupt-audit-detail! ()
  (let* ((logs (dbms-storage-load-audit-log))
         (entry (if (null logs) '() (car logs))))
    (if (null entry)
        (dbms-make-error 'dbms/not-implemented "no audit records" 'audit)
        (let* ((tampered (if (= (length entry) 9)
                             (list 'dbms-audit-record
                                   (second entry)
                                   (third entry)
                                   (fourth entry)
                                   (fifth entry)
                                   (dbms-sixth entry)
                                   'tampered-detail
                                   (dbms-eighth entry)
                                   (dbms-ninth entry))
                             entry))
               (saved (dbms-storage-atomic-replace (dbms-storage-audit-path)
                                                   (cons tampered (cdr logs)))))
          saved))))

(defun run-tests ()
  (let* ((root-base (string-append "/tmp/dbms-security-" (format nil "~A" (get-universal-time))))
         (root root-base)
         (suffix 0)
         (catalog '())
         (r '())
         (status '())
         (security '())
         (auth '())
         (cred '()))
    (while (not (null (probe-file root)))
      (setq suffix (+ suffix 1))
      (setq root (string-append root-base "-" (format nil "~A" suffix))))
    (setenv "DBMS_STORAGE_ROOT" root)

    (setq catalog (dbms-engine-init))
    (setq r (dbms-exec-sql catalog "CREATE USER alice;"))
    (assert-true "create alice" (and (dbms-result-p r) (eq (second r) 'ok)))

    ;; password hashing policy
    (setq r (dbms-admin-security-set-password "alice" "secret-1"))
    (assert-true "set alice password" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq security (dbms-engine-load-security-meta))
    (setq cred (dbms-storage-security-find-credential (dbms-security-meta-credentials security) "alice"))
    (assert-true "credential exists" (dbms-security-credential-p cred))
    (assert-true "credential digest not plain password" (not (string= (fifth cred) "secret-1")))
    (assert-true "credential validates" (dbms-security-credential-valid-p cred "secret-1"))

    ;; wrong password
    (setq r (dbms-admin-security-authenticate "alice" "wrong" "TCP" t))
    (assert-true "wrong password denied" (and (dbms-result-p r) (eq (second r) 'error)))
    (assert-true "wrong password code" (eq (result-error-code r) 'dbms/authentication-failed))

    ;; tls required policy
    (setq r (dbms-admin-security-configure-tls "REQUIRED" "/tmp/server.crt" "/tmp/server.key" "/tmp/ca.crt"))
    (assert-true "tls policy configure" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq status (dbms-admin-security-status))
    (assert-true "security status ok" (and (dbms-result-p status) (eq (second status) 'ok)))
    (assert-true "tls mode required" (string= (dbms-assoc-get (third status) "tls-mode") "REQUIRED"))

    (setq r (dbms-admin-security-authenticate "alice" "secret-1" "TCP" nil))
    (assert-true "remote auth without tls denied" (and (dbms-result-p r) (eq (second r) 'error)))
    (assert-true "tls required code" (eq (result-error-code r) 'dbms/tls-required))

    (setq r (dbms-admin-security-authenticate "alice" "secret-1" "TCP" t))
    (assert-true "remote auth with tls ok" (and (dbms-result-p r) (eq (second r) 'ok)))
    (assert-true "session switched to alice" (string= (dbms-current-session-user) "alice"))

    ;; audit integrity verify
    (setq r (dbms-admin-audit-verify-integrity))
    (assert-true "audit verify ok" (and (dbms-result-p r) (eq (second r) 'ok)))

    ;; tamper current audit log and verify detection
    (assert-true "tamper write succeeds" (not (dbms-error-p (corrupt-audit-detail!))))
    (setq r (dbms-admin-audit-verify-integrity))
    (assert-true "tamper verify denied" (and (dbms-result-p r) (eq (second r) 'error)))
    (assert-true "tamper detect code" (eq (result-error-code r) 'dbms/audit-tamper-detected))

    ;; auth metadata remained compatible
    (setq auth (dbms-engine-load-auth-meta))
    (assert-true "auth user alice still exists" (dbms-auth-string-list-member-p "alice" (dbms-auth-meta-users auth)))

    (format t "dbms security hardening tests passed.~%")))

(catch 'dbms-assert-failed
  (run-tests))
