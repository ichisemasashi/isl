;; dbms/tests/prod/t190-audit-log.lsp
;; P4-006: audit log for DDL/privilege/tx-abort + rotation.

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

(defun audit-action (rec)
  ;; (dbms-audit-record ts user action object result detail)
  (if (and (listp rec) (>= (length rec) 7))
      (fourth rec)
      '()))

(defun audit-has-action-p (logs action)
  (if (null logs)
      nil
      (if (eq (audit-action (car logs)) action)
          t
          (audit-has-action-p (cdr logs) action))))

(defun run-tests ()
  (let* ((root-base (string-append "/tmp/dbms-audit-" (format nil "~A" (get-universal-time))))
         (root root-base)
         (suffix 0)
         (catalog '())
         (r '())
         (logs '()))
    (while (not (null (probe-file root)))
      (setq suffix (+ suffix 1))
      (setq root (string-append root-base "-" (format nil "~A" suffix))))
    (setenv "DBMS_STORAGE_ROOT" root)
    (setenv "DBMS_AUDIT_MAX_ENTRIES" "100")

    (setq catalog (dbms-engine-init))

    ;; DDL + privilege changes should be audited.
    (setq r (dbms-exec-sql catalog "CREATE TABLE pages (id INT PRIMARY KEY, title TEXT);"))
    (assert-true "create table ok" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "CREATE USER alice;"))
    (assert-true "create user ok" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "CREATE ROLE editor;"))
    (assert-true "create role ok" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "GRANT ROLE editor TO alice;"))
    (assert-true "grant role ok" (and (dbms-result-p r) (eq (second r) 'ok)))

    ;; Force tx abort by lock conflict under SERIALIZABLE.
    (setq r (dbms-set-session-isolation-level! 'SERIALIZABLE))
    (assert-true "set serializable" (eq r 'SERIALIZABLE))
    (setq r (dbms-exec-sql catalog "BEGIN;"))
    (assert-true "begin ok" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq *dbms-lock-table* (list (list "pages" 'X (list 999))))
    (setq r (dbms-exec-sql catalog "SELECT id FROM pages;"))
    (assert-true "lock conflict error" (and (dbms-result-p r) (eq (second r) 'error)))
    (assert-true "lock conflict code" (eq (result-error-code r) 'dbms/lock-conflict))
    (assert-true "tx status aborted after conflict" (eq (dbms-tx-state-status (dbms-current-tx-state)) 'aborted))
    (dbms-engine-reset-tx-state)

    (setq logs (dbms-storage-load-audit-log))
    (assert-true "audit has DDL action" (audit-has-action-p logs 'DDL))
    (assert-true "audit has PRIVILEGE action" (audit-has-action-p logs 'PRIVILEGE))
    (assert-true "audit has TX-ABORT action" (audit-has-action-p logs 'TX-ABORT))

    ;; Rotation: cap entries and generate many auditable events.
    (setenv "DBMS_AUDIT_MAX_ENTRIES" "3")
    (setq r (dbms-exec-sql catalog "CREATE ROLE r1;"))
    (assert-true "create role r1" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "CREATE ROLE r2;"))
    (assert-true "create role r2" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "CREATE ROLE r3;"))
    (assert-true "create role r3" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "CREATE ROLE r4;"))
    (assert-true "create role r4" (and (dbms-result-p r) (eq (second r) 'ok)))
    (setq r (dbms-exec-sql catalog "CREATE ROLE r5;"))
    (assert-true "create role r5" (and (dbms-result-p r) (eq (second r) 'ok)))

    (setq logs (dbms-storage-load-audit-log))
    (assert-true "audit current length capped" (<= (length logs) 3))
    (assert-true "audit archive file exists" (not (null (probe-file (dbms-storage-audit-archive-path 1)))))

    (format t "dbms audit log tests passed.~%")))

(catch 'dbms-assert-failed
  (run-tests))
