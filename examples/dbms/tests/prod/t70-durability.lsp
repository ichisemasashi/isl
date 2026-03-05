;; dbms/tests/prod/t70-durability.lsp
;; P1-009: kill/restart durability trial (10 iterations).

(load "./examples/dbms/app/repr.lsp")

(defun assert-true (label cond)
  (if cond
      t
      (progn
        (format t "assert failed: ~A~%" label)
        (throw 'dbms-assert-failed label))))

(defun shell-quote (s)
  (let ((i 0)
        (n (length s))
        (out "'"))
    (while (< i n)
      (let ((ch (substring s i (+ i 1))))
        (if (string= ch "'")
            (setq out (string-append out "'\\''"))
            (setq out (string-append out ch))))
      (setq i (+ i 1)))
    (string-append out "'")))

(defun read-number-file (path)
  (if (null (probe-file path))
      0
      (with-open-file (s path :direction :input)
        (let ((v (read s)))
          (if (numberp v) v 0)))))

(defun wait-file (path max-seconds)
  (let ((elapsed 0))
    (while (and (null (probe-file path)) (< elapsed max-seconds))
      (system "sleep 1")
      (setq elapsed (+ elapsed 1)))
    (not (null (probe-file path)))))

(defun run-case (runner base-dir case-id)
  (let* ((id-s (format nil "~A" case-id))
         (root (string-append base-dir "/case-" id-s))
         (marker (string-append base-dir "/marker-" id-s))
         (pidfile (string-append base-dir "/pid-" id-s))
         (out (string-append base-dir "/worker-" id-s ".out"))
         (err (string-append base-dir "/worker-" id-s ".err"))
         (cout (string-append base-dir "/check-" id-s ".out"))
         (cerr (string-append base-dir "/check-" id-s ".err"))
         (worker "./examples/dbms/tests/prod/helpers/durability_commit_worker.lsp")
         (checker "./examples/dbms/tests/prod/helpers/durability_check_worker.lsp")
         (launch-cmd "")
         (kill-cmd "")
         (check-cmd "")
         (pid 0)
         (ok-marker nil))
    (setq launch-cmd
          (string-append
           "DBMS_STORAGE_ROOT=" (shell-quote root)
           " DBMS_DURABILITY_ID=" (shell-quote id-s)
           " DBMS_TEST_COMMIT_DELAY_SEC='20'"
           " DBMS_TEST_COMMIT_MARKER_PATH=" (shell-quote marker)
           " " (shell-quote runner)
           " " (shell-quote worker)
           " >" (shell-quote out)
           " 2>" (shell-quote err)
           " & echo $! > " (shell-quote pidfile)))
    (assert-true (string-append "launch worker #" id-s) (= (system launch-cmd) 0))

    (setq ok-marker (wait-file marker 30))
    (assert-true (string-append "marker observed #" id-s) ok-marker)

    (setq pid (read-number-file pidfile))
    (assert-true (string-append "pid captured #" id-s) (> pid 0))
    (setq kill-cmd (string-append "kill -KILL " (format nil "~A" pid) " >/dev/null 2>&1 || true"))
    (system kill-cmd)
    (system "sleep 1")

    (setq check-cmd
          (string-append
           "DBMS_STORAGE_ROOT=" (shell-quote root)
           " DBMS_DURABILITY_ID=" (shell-quote id-s)
           " " (shell-quote runner)
           " " (shell-quote checker)
           " >" (shell-quote cout)
           " 2>" (shell-quote cerr)))
    (assert-true (string-append "recovery check #" id-s) (= (system check-cmd) 0))
    t))

(defun run-tests ()
  (let* ((runner-env (getenv "DBMS_TEST_RUNNER_BIN"))
         (runner (if (or (null runner-env) (string= runner-env "")) "./bin/isl" runner-env))
         (base (string-append "/tmp/dbms-durability-" (format nil "~A" (get-universal-time))))
         (i 1))
    (assert-true "mkdir base dir" (= (system (string-append "mkdir -p " (shell-quote base))) 0))
    (while (<= i 10)
      (run-case runner base i)
      (setq i (+ i 1)))
    (format t "dbms durability tests passed (10/10).~%")))

(catch 'dbms-assert-failed
  (run-tests))
