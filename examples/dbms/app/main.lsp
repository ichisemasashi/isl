;; dbms/app/main.lsp
;; DBMS CLIエントリポイントの雛形。

(load "./examples/dbms/app/repr.lsp")
(load "./examples/dbms/app/profile.lsp")
(load "./examples/dbms/app/engine.lsp")

(defun dbms-read-stdin-text ()
  (with-open-file (s "/dev/stdin" :direction :input)
    (let ((line (read-line s #f))
          (acc "")
          (first-line t))
      (while (not (null line))
        (if first-line
            (progn
              (setq acc line)
              (setq first-line nil))
            (setq acc (string-append acc "\n" line)))
        (setq line (read-line s #f)))
      acc)))

(defun main ()
  (let* ((catalog (dbms-engine-init))
         (sql-text (dbms-read-stdin-text))
         (result (if (string= sql-text "")
                     (dbms-make-result-error 'dbms/no-input "stdin is empty" '())
                     (dbms-exec-sql catalog sql-text))))
    (format t "~A~%" result)))

(main)
