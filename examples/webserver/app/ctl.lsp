(defun ws-root ()
  (let ((v (getenv "WEBSERVER_ROOT")))
    (if (null v)
        "/Volumes/SSD-PLU3/work/LISP/islisp/isl/examples/webserver"
        v)))

(load (string-append (ws-root) "/app/config.lsp"))
(load (string-append (ws-root) "/app/http_server.lsp"))

(defun ws-ctl-usage ()
  (format t "webserver ctl usage:\n")
  (format t "  WEBSERVER_CMD=start  ./bin/isl examples/webserver/app/ctl.lsp\n")
  (format t "  WEBSERVER_CMD=stop   ./bin/isl examples/webserver/app/ctl.lsp\n")
  (format t "  WEBSERVER_CMD=test   ./bin/isl examples/webserver/app/ctl.lsp\n")
  (format t "  WEBSERVER_CMD=-t     ./bin/isl examples/webserver/app/ctl.lsp\n")
  (format t "  WEBSERVER_CMD=logs   ./bin/isl examples/webserver/app/ctl.lsp\n"))

(defun ws-ctl-bin-path (root)
  (string-append (substring root 0 (- (length root) (length "/examples/webserver"))) "/bin/isl"))

(defun ws-ctl-main-path (root)
  (string-append root "/app/main.lsp"))

(defun ws-ctl-config-path (root)
  (ws-env-or "WEBSERVER_CONFIG" (string-append root "/conf/webserver.conf.lsp")))

(defun ws-ctl-pid-file ()
  (ws-env-or "WEBSERVER_PID_FILE" "/tmp/webserver.pid"))

(defun ws-ctl-server-log-file ()
  (ws-env-or "WEBSERVER_SERVER_LOG" "/tmp/webserver.log"))

(defun ws-ctl-transport ()
  (let ((v (ws-env-or "WEBSERVER_TRANSPORT" "http")))
    (if (or (string= v "http") (string= v "https"))
        v
        (ws-die (format nil "WEBSERVER_TRANSPORT must be http or https (got: ~A)" v)))))

(defun ws-ctl-pid-running-p (pid)
  (= (system (string-append "sh -c 'kill -0 " pid " >/dev/null 2>&1'")) 0))

(defun ws-ctl-read-pid (pid-file)
  (if (null (probe-file pid-file))
      '()
      (ws-trim (ws-read-file-text pid-file))))

(defun ws-ctl-env-fragment (name)
  (let ((v (getenv name)))
    (if (null v)
        ""
        (string-append name "=" (ws-shell-quote v) " "))))

(defun ws-ctl-start-daemon ()
  (let* ((root (ws-root))
         (config-path (ws-ctl-config-path root))
         (pid-file (ws-ctl-pid-file))
         (server-log (ws-ctl-server-log-file))
         (transport (ws-ctl-transport))
         (bin (ws-ctl-bin-path root))
         (main (ws-ctl-main-path root))
         (cfg (ws-validate-config-file config-path))
         (existing-pid (ws-ctl-read-pid pid-file)))
    (ws-print-config cfg)
    (if (and (not (null existing-pid)) (ws-ctl-pid-running-p existing-pid))
        (ws-die (format nil "already running with pid ~A" existing-pid))
        nil)
    (let* ((cmd (string-append
                 "sh -c \""
                 "WEBSERVER_ROOT=" (ws-shell-quote root) " "
                 "WEBSERVER_CONFIG=" (ws-shell-quote config-path) " "
                 "WEBSERVER_TRANSPORT=" (ws-shell-quote transport) " "
                 (ws-ctl-env-fragment "WEBSERVER_MAX_ACCEPTS")
                 (ws-ctl-env-fragment "WEBSERVER_ACCESS_LOG")
                 (ws-ctl-env-fragment "WEBSERVER_ERROR_LOG")
                 (ws-ctl-env-fragment "WEBSERVER_CGI_TIMEOUT_SEC")
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
      (system "sh -c 'sleep 1'")
      (if (not (ws-ctl-pid-running-p pid))
          (ws-die (format nil "server process not running pid=~A" pid))
          nil)
      (ws-log (format nil "started pid=~A transport=~A config=~A" pid transport config-path)))))

(defun ws-ctl-start-foreground ()
  (let* ((root (ws-root))
         (config-path (ws-ctl-config-path root))
         (transport (ws-ctl-transport))
         (cfg (ws-validate-config-file config-path)))
    (ws-print-config cfg)
    (if (string= transport "https")
        (ws-start-https-server cfg)
        (ws-start-http-server cfg))))

(defun ws-ctl-start ()
  (let ((daemon (ws-env-or "WEBSERVER_DAEMON" "1")))
    (if (string= daemon "0")
        (ws-ctl-start-foreground)
        (ws-ctl-start-daemon))))

(defun ws-ctl-stop ()
  (let* ((pid-file (ws-ctl-pid-file))
         (pid (ws-ctl-read-pid pid-file)))
    (if (null pid)
        (progn
          (ws-log (format nil "pid file not found: ~A" pid-file))
          t)
        (progn
          (system (string-append "sh -c 'kill " pid " >/dev/null 2>&1 || true'"))
          (system (string-append "sh -c 'sleep 1'"))
          (if (ws-ctl-pid-running-p pid)
              (system (string-append "sh -c 'kill -9 " pid " >/dev/null 2>&1 || true'"))
              nil)
          (if (null (probe-file pid-file))
              nil
              (delete-file pid-file))
          (ws-log (format nil "stopped pid=~A" pid))))))

(defun ws-ctl-test-config ()
  (let* ((root (ws-root))
         (config-path (ws-ctl-config-path root))
         (cfg (ws-validate-config-file config-path)))
    (ws-print-config cfg)
    (ws-log "config check passed")))

(defun ws-ctl-log-tail (path lines)
  (if (null (probe-file path))
      (format t "[missing] ~A~%" path)
      (system (string-append "sh -c 'tail -n "
                             (format nil "~A" lines)
                             " " (ws-shell-quote path)
                             "'"))))

(defun ws-ctl-logs ()
  (let* ((kind (ws-env-or "WEBSERVER_LOG_KIND" "both"))
         (lines-s (ws-env-or "WEBSERVER_TAIL_LINES" "50"))
         (lines (let ((n (ws-parse-int lines-s))) (if (null n) 50 n)))
         (access-log (ws-env-or "WEBSERVER_ACCESS_LOG" "/tmp/webserver-access.log"))
         (error-log (ws-env-or "WEBSERVER_ERROR_LOG" "/tmp/webserver-error.log"))
         (server-log (ws-ctl-server-log-file)))
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
  (let ((cmd (getenv "WEBSERVER_CMD")))
    (if (null cmd)
        (progn
          (ws-ctl-usage)
          (ws-die "WEBSERVER_CMD is required"))
        (if (or (string= cmd "start") (string= cmd "run"))
            (ws-ctl-start)
            (if (or (string= cmd "stop") (string= cmd "quit"))
                (ws-ctl-stop)
                (if (or (string= cmd "test") (string= cmd "-t") (string= cmd "check"))
                    (ws-ctl-test-config)
                    (if (or (string= cmd "logs") (string= cmd "log"))
                        (ws-ctl-logs)
                        (progn
                          (ws-ctl-usage)
                          (ws-die (format nil "unknown WEBSERVER_CMD: ~A" cmd))))))))))

(ws-ctl-main)
