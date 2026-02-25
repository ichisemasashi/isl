(defun ws-root ()
  (let ((v (getenv "WEBSERVER_ROOT")))
    (if (not (null v))
        v
        (if (not (null (probe-file "examples/webserver/app/config.lsp")))
            "examples/webserver"
            (if (not (null (probe-file "app/config.lsp")))
                "."
                "/Volumes/SSD-PLU3/work/LISP/islisp/isl/examples/webserver")))))

(load (string-append (ws-root) "/app/config.lsp"))
(load (string-append (ws-root) "/app/http_server.lsp"))

(defun ws-ctl-usage ()
  (format t "webserver ctl usage:\n")
  (format t "  WEBSERVER_CMD=run      ./bin/isl examples/webserver/app/ctl.lsp\n")
  (format t "  WEBSERVER_CMD=start    ./bin/isl examples/webserver/app/ctl.lsp\n")
  (format t "  WEBSERVER_CMD=stop     ./bin/isl examples/webserver/app/ctl.lsp\n")
  (format t "  WEBSERVER_CMD=restart  ./bin/isl examples/webserver/app/ctl.lsp\n")
  (format t "  WEBSERVER_CMD=test     ./bin/isl examples/webserver/app/ctl.lsp\n")
  (format t "  WEBSERVER_CMD=-t       ./bin/isl examples/webserver/app/ctl.lsp\n")
  (format t "  WEBSERVER_CMD=logs     ./bin/isl examples/webserver/app/ctl.lsp\n")
  (format t "  (export WEBSERVER_ROOT and WEBSERVER_CONFIG first; see examples/webserver/README.md)\n"))

(defun ws-ctl-bin-path (root)
  (if (not (null (probe-file "./bin/isl")))
      "./bin/isl"
      (string-append (substring root 0 (- (length root) (length "/examples/webserver"))) "/bin/isl")))

(defun ws-ctl-main-path (root)
  (string-append root "/app/main.lsp"))

(defun ws-ctl-config-path (root)
  (ws-env-or "WEBSERVER_CONFIG" (string-append root "/conf/webserver.conf.lsp")))

(defun ws-ctl-pid-file (cfg)
  (ws-env-or "WEBSERVER_PID_FILE" (ws-config-get cfg 'pid_file)))

(defun ws-ctl-server-log-file (cfg)
  (ws-env-or "WEBSERVER_SERVER_LOG" (ws-config-get cfg 'server_log)))

(defun ws-ctl-access-log-file (cfg)
  (ws-env-or "WEBSERVER_ACCESS_LOG" (ws-config-get cfg 'access_log)))

(defun ws-ctl-error-log-file (cfg)
  (ws-env-or "WEBSERVER_ERROR_LOG" (ws-config-get cfg 'error_log)))

(defun ws-ctl-cgi-timeout-sec (cfg)
  (let ((v (getenv "WEBSERVER_CGI_TIMEOUT_SEC")))
    (if (null v)
        (ws-config-get cfg 'cgi_timeout_sec)
        (let ((n (ws-parse-int v)))
          (if (or (null n) (< n 1))
              (ws-config-get cfg 'cgi_timeout_sec)
              n)))))

(defun ws-ctl-transport (cfg)
  (let ((v (ws-env-or "WEBSERVER_TRANSPORT" (ws-config-get cfg 'transport))))
    (if (or (string= v "http") (string= v "https"))
        v
        (ws-die (format nil "transport must be http or https (got: ~A)" v)))))

(defun ws-ctl-daemon-p (cfg)
  (let ((v (getenv "WEBSERVER_DAEMON")))
    (if (null v)
        (ws-config-get cfg 'daemon)
        (not (string= v "0")))))

(defun ws-ctl-pid-running-p (pid)
  (= (system (string-append "sh -c 'kill -0 " pid " >/dev/null 2>&1'")) 0))

(defun ws-ctl-read-pid (pid-file)
  (if (null (probe-file pid-file))
      '()
      (let ((pid (ws-trim (ws-read-file-text pid-file)))
            (n '()))
        (setq n (ws-parse-int pid))
        (if (or (null n) (< n 1))
            '()
            pid))))

(defun ws-ctl-env-fragment (name)
  (let ((v (getenv name)))
    (if (null v)
        ""
        (string-append name "=" (ws-shell-quote v) " "))))

(defun ws-ctl-delete-pid-file (pid-file)
  (if (null (probe-file pid-file))
      nil
      (delete-file pid-file)))

(defun ws-ctl-sleep1 ()
  (system "sh -c 'sleep 1'"))

(defun ws-ctl-port-owner-pid (port)
  (if (not (ws-command-available-p "lsof"))
      '()
      (let* ((cmd (string-append
                   "lsof -nP -iTCP:"
                   (format nil "~A" port)
                   " -sTCP:LISTEN -Fp | sed -n \"s/^p//p\" | head -n 1"))
             (result (ws-command-output cmd))
             (status (car result))
             (out (ws-trim (second result)))
             (n (if (= status 0) (ws-parse-int out) '())))
        (if (null n) '() out))))

(defun ws-ctl-cleanup-stale-pid-file (pid-file)
  (let ((pid (ws-ctl-read-pid pid-file)))
    (if (null pid)
        (if (null (probe-file pid-file))
            nil
            (progn
              (ws-ctl-delete-pid-file pid-file)
              (ws-log (format nil "removed invalid pid file: ~A" pid-file))))
        (if (ws-ctl-pid-running-p pid)
            nil
            (progn
              (ws-ctl-delete-pid-file pid-file)
              (ws-log (format nil "removed stale pid file: ~A (pid=~A)" pid-file pid)))))))

(defun ws-ctl-kill (pid sig)
  (system (string-append "sh -c 'kill -" sig " " pid " >/dev/null 2>&1 || true'")))

(defun ws-ctl-wait-stopped (pid max-seconds)
  (let ((i 0))
    (while (and (< i max-seconds) (ws-ctl-pid-running-p pid))
      (ws-ctl-sleep1)
      (setq i (+ i 1)))
    (not (ws-ctl-pid-running-p pid))))

(defun ws-ctl-apply-runtime-env (cfg)
  (if (null (getenv "WEBSERVER_ACCESS_LOG"))
      (setenv "WEBSERVER_ACCESS_LOG" (ws-ctl-access-log-file cfg))
      nil)
  (if (null (getenv "WEBSERVER_ERROR_LOG"))
      (setenv "WEBSERVER_ERROR_LOG" (ws-ctl-error-log-file cfg))
      nil)
  (if (null (getenv "WEBSERVER_CGI_TIMEOUT_SEC"))
      (setenv "WEBSERVER_CGI_TIMEOUT_SEC" (format nil "~A" (ws-ctl-cgi-timeout-sec cfg)))
      nil))

(defun ws-ctl-start-daemon (cfg)
  (let* ((root (ws-root))
         (config-path (ws-ctl-config-path root))
         (pid-file (ws-ctl-pid-file cfg))
         (server-log (ws-ctl-server-log-file cfg))
         (transport (ws-ctl-transport cfg))
         (bin (ws-ctl-bin-path root))
         (main (ws-ctl-main-path root))
         (port (ws-config-get cfg 'listen_port))
         (access-log (ws-ctl-access-log-file cfg))
         (error-log (ws-ctl-error-log-file cfg))
         (cgi-timeout-sec (ws-ctl-cgi-timeout-sec cfg))
         (existing-pid (ws-ctl-read-pid pid-file)))
    (ws-print-config cfg)
    (ws-ctl-cleanup-stale-pid-file pid-file)
    (setq existing-pid (ws-ctl-read-pid pid-file))
    (if (and (not (null existing-pid)) (ws-ctl-pid-running-p existing-pid))
        (ws-die (format nil "already running with pid ~A" existing-pid))
        nil)
    (let ((owner (ws-ctl-port-owner-pid port)))
      (if (not (null owner))
          (ws-die (format nil "listen port already in use port=~A pid=~A" port owner))
          nil))
    (let* ((cmd (string-append
                 "sh -c \""
                 "WEBSERVER_ROOT=" (ws-shell-quote root) " "
                 "WEBSERVER_CONFIG=" (ws-shell-quote config-path) " "
                 "WEBSERVER_TRANSPORT=" (ws-shell-quote transport) " "
                 "WEBSERVER_ACCESS_LOG=" (ws-shell-quote access-log) " "
                 "WEBSERVER_ERROR_LOG=" (ws-shell-quote error-log) " "
                 "WEBSERVER_CGI_TIMEOUT_SEC=" (ws-shell-quote (format nil "~A" cgi-timeout-sec)) " "
                 (ws-ctl-env-fragment "WEBSERVER_MAX_ACCEPTS")
                 (ws-shell-quote bin) " " (ws-shell-quote main)
                 " < /dev/null"
                 " >> " (ws-shell-quote server-log)
                 " 2>&1 & echo \\$! > " (ws-shell-quote pid-file)
                 "\""))
           (st (system cmd))
           (pid (ws-ctl-read-pid pid-file)))
      (if (not (= st 0))
          (ws-die "failed to start server process")
          nil)
      (if (null pid)
          (ws-die "failed to acquire pid")
          nil)
      (let ((i 0)
            (ready #f))
        (while (and (< i 5) (not ready))
          (ws-ctl-sleep1)
          (if (ws-ctl-pid-running-p pid)
              (let ((owner (ws-ctl-port-owner-pid port)))
                (if (and (not (null owner)) (string= owner pid))
                    (setq ready #t)
                    nil))
              nil)
          (setq i (+ i 1)))
        (if (not ready)
            (progn
              (if (not (ws-ctl-pid-running-p pid))
                  (ws-ctl-delete-pid-file pid-file)
                  nil)
              (ws-die (format nil "server process not running pid=~A" pid)))
            nil))
      (ws-log (format nil "started pid=~A transport=~A config=~A" pid transport config-path)))))

(defun ws-ctl-start-foreground (cfg)
  (let* ((root (ws-root))
         (config-path (ws-ctl-config-path root))
         (transport (ws-ctl-transport cfg))
         (pid-file (ws-ctl-pid-file cfg))
         (port (ws-config-get cfg 'listen_port)))
    (ws-print-config cfg)
    (ws-ctl-cleanup-stale-pid-file pid-file)
    (let ((owner (ws-ctl-port-owner-pid port)))
      (if (not (null owner))
          (ws-die (format nil "listen port already in use port=~A pid=~A" port owner))
          nil))
    (ws-ctl-apply-runtime-env cfg)
    (if (string= transport "https")
        (ws-start-https-server cfg)
        (ws-start-http-server cfg))))

(defun ws-ctl-run (cfg)
  (ws-ctl-start-foreground cfg))

(defun ws-ctl-start (cfg)
  (if (ws-ctl-daemon-p cfg)
      (ws-ctl-start-daemon cfg)
      (ws-ctl-start-foreground cfg)))

(defun ws-ctl-stop (cfg)
  (let* ((pid-file (ws-ctl-pid-file cfg))
         (pid (ws-ctl-read-pid pid-file))
         (port (ws-config-get cfg 'listen_port)))
    (ws-ctl-cleanup-stale-pid-file pid-file)
    (setq pid (ws-ctl-read-pid pid-file))
    (if (null pid)
        (progn
          (let ((owner (ws-ctl-port-owner-pid port)))
            (if (null owner)
                (ws-log (format nil "pid file not found: ~A" pid-file))
                (ws-log (format nil "pid file not found: ~A (port ~A in use by pid ~A)"
                                pid-file port owner))))
          t)
        (progn
          (ws-ctl-kill pid "TERM")
          (if (ws-ctl-wait-stopped pid 5)
              nil)
          (if (ws-ctl-pid-running-p pid)
              (progn
                (ws-ctl-kill pid "KILL")
                (ws-ctl-wait-stopped pid 2))
              nil)
          (if (ws-ctl-pid-running-p pid)
              (ws-die (format nil "failed to stop pid=~A" pid))
              nil)
          (ws-ctl-delete-pid-file pid-file)
          (ws-log (format nil "stopped pid=~A" pid))))))

(defun ws-ctl-restart (cfg)
  (ws-ctl-stop cfg)
  (ws-ctl-start cfg))

(defun ws-ctl-test-config (cfg)
  (let* ((pid-file (ws-ctl-pid-file cfg))
         (pid (ws-ctl-read-pid pid-file))
         (port (ws-config-get cfg 'listen_port))
         (owner (ws-ctl-port-owner-pid port)))
    (ws-print-config cfg)
    (if (null pid)
        (if (null owner)
            (ws-log "state check passed (stopped)")
            (ws-die (format nil "state check failed: port=~A is in use by pid=~A but pid file is missing"
                            port owner)))
        (if (not (ws-ctl-pid-running-p pid))
            (ws-die (format nil "state check failed: stale pid file pid=~A file=~A"
                            pid pid-file))
            (if (null owner)
                (ws-die (format nil "state check failed: pid=~A is running but listen port ~A is not active"
                                pid port))
                (if (not (string= owner pid))
                    (ws-die (format nil "state check failed: pid file says ~A but port ~A is owned by ~A"
                                    pid port owner))
                    (ws-log (format nil "state check passed (running pid=~A port=~A)" pid port))))))))

(defun ws-ctl-log-tail (path lines)
  (if (null (probe-file path))
      (format t "[missing] ~A~%" path)
      (system (string-append "sh -c 'tail -n "
                             (format nil "~A" lines)
                             " " (ws-shell-quote path)
                             "'"))))

(defun ws-ctl-logs ()
  (let* ((root (ws-root))
         (config-path (ws-ctl-config-path root))
         (cfg (ws-validate-config-file config-path))
         (kind (ws-env-or "WEBSERVER_LOG_KIND" "both"))
         (lines-s (ws-env-or "WEBSERVER_TAIL_LINES" "50"))
         (lines (let ((n (ws-parse-int lines-s))) (if (null n) 50 n)))
         (access-log (ws-ctl-access-log-file cfg))
         (error-log (ws-ctl-error-log-file cfg))
         (server-log (ws-ctl-server-log-file cfg)))
    (if (or (string= kind "server") (string= kind "both"))
        (progn
          (format t "== server log ==~%")
          (ws-ctl-log-tail server-log lines))
        nil)
    (if (or (string= kind "access") (string= kind "both"))
        (progn
          (format t "== access log ==~%")
          (ws-ctl-log-tail access-log lines))
        nil)
    (if (or (string= kind "error") (string= kind "both"))
        (progn
          (format t "== error log ==~%")
          (ws-ctl-log-tail error-log lines))
        nil)))

(defun ws-ctl-main ()
  (let ((cmd (getenv "WEBSERVER_CMD"))
        (cfg '()))
    (if (null cmd)
        (progn
          (ws-ctl-usage)
          (ws-die "WEBSERVER_CMD is required"))
        (progn
          (if (or (string= cmd "logs") (string= cmd "log"))
              nil
              (let* ((root (ws-root))
                     (config-path (ws-ctl-config-path root)))
                (setq cfg (ws-validate-config-file config-path))))
          (if (string= cmd "run")
              (ws-ctl-run cfg)
              (if (string= cmd "start")
                  (ws-ctl-start cfg)
                  (if (or (string= cmd "stop") (string= cmd "quit"))
                      (ws-ctl-stop cfg)
                      (if (or (string= cmd "restart") (string= cmd "reload"))
                          (ws-ctl-restart cfg)
                          (if (or (string= cmd "test") (string= cmd "-t") (string= cmd "check"))
                              (ws-ctl-test-config cfg)
                              (if (or (string= cmd "logs") (string= cmd "log"))
                                  (ws-ctl-logs)
                                  (progn
                                    (ws-ctl-usage)
                                    (ws-die (format nil "unknown WEBSERVER_CMD: ~A" cmd)))))))))))))

(let ((result (catch 'ws-error
                (progn
                  (ws-ctl-main)
                  'ok))))
  (if (eq result 'ok)
      t
      (exit 1)))
