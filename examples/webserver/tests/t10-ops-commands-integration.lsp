(defun ws-test-root ()
  (let ((v (getenv "WEBSERVER_ROOT")))
    (if (null v)
        "/Volumes/SSD-PLU3/work/LISP/islisp/isl/examples/webserver"
        v)))

(load (string-append (ws-test-root) "/app/config.lsp"))
(load (string-append (ws-test-root) "/app/http_server.lsp"))

(defun assert-true (label pred)
  (if pred
      t
      (progn
        (format t "assert failed: ~A~%" label)
        (throw 'ws-test-failed label))))

(defun ws-contains (hay needle)
  (not (null (string-index needle hay))))

(defun ws-write-file-text (path text)
  (with-open-file (s path :direction :output :if-exists :overwrite)
    (format s "~A~%" text)))

(defun ws-read-file-text-local (path)
  (with-open-file (s path :direction :input)
    (let ((line (read-line s #f))
          (acc "")
          (first t))
      (while (not (null line))
        (if first
            (progn
              (setq acc line)
              (setq first nil))
            (setq acc (string-append acc "\n" line)))
        (setq line (read-line s #f)))
      acc)))

(defun ws-shell-quote-local (s)
  (if (ws-contains s "'")
      (throw 'ws-test-failed "single quote is not supported in shell path")
      (string-append "'" s "'")))

(defun ws-read-line-crlf (conn)
  (let ((acc "")
        (done nil))
    (while (not done)
      (let ((ch (tcp-receive-char conn #f)))
        (if (null ch)
            (if (= (length acc) 0)
                (setq done 'eof)
                (setq done #t))
            (if (string= (string ch) "\n")
                (setq done #t)
                (setq acc (string-append acc (string ch)))))))
    (if (eq done 'eof)
        '()
        (ws-strip-cr acc))))

(defun ws-parse-header-line (line)
  (let ((p (string-index ":" line)))
    (if (null p)
        '()
        (list (ws-ascii-downcase (ws-trim (substring line 0 p)))
              (ws-trim (substring line (+ p 1) (length line)))))))

(defun ws-read-response-from-conn* (conn skip-body)
  (let ((status-line (ws-read-line-crlf conn)))
    (if (null status-line)
        '()
        (let ((headers '())
              (done nil))
          (while (not done)
            (let ((line (ws-read-line-crlf conn)))
              (if (or (null line) (string= line ""))
                  (setq done #t)
                  (let ((h (ws-parse-header-line line)))
                    (if (null h)
                        (setq done #t)
                        (setq headers (cons h headers)))))))
          (setq headers (ws-reverse headers))
          (let* ((len-s (ws-header-get headers "content-length"))
                 (len (if (null len-s) 0 (ws-parse-int len-s)))
                 (body (if skip-body
                           ""
                           (if (and (not (null len)) (> len 0))
                               (ws-read-fixed-body conn len)
                               ""))))
            (list (list 'status-line status-line)
                  (list 'headers headers)
                  (list 'body body)))))))

(defun ws-request-once (port req)
  (let ((conn (tcp-connect "127.0.0.1" port)))
    (tcp-send conn req)
    (let ((resp (ws-read-response-from-conn* conn #f)))
      (tcp-close conn)
      resp)))

(defun ws-status-code (resp)
  (let* ((line (ws-config-get resp 'status-line))
         (a (ws-split-once line " ")))
    (if (null a)
        0
        (let ((b (ws-split-once (second a) " ")))
          (if (null b)
              0
              (ws-parse-int (car b)))))))

(defun run-tests ()
  (let* ((root (ws-test-root))
         (ctl (string-append root "/app/ctl.lsp"))
         (tmp-port (+ 24000 (mod (get-universal-time) 1000)))
         (cfg (string-append root "/tests/tmp-t10-webserver.conf.lsp"))
         (pid "/tmp/webserver-t10.pid")
         (server-log "/tmp/webserver-t10-server.log")
         (access-log "/tmp/webserver-t10-access.log")
         (error-log "/tmp/webserver-t10-error.log")
         (out-test "/tmp/webserver-t10-out-test.txt")
         (out-logs "/tmp/webserver-t10-out-logs.txt")
         (cmd-prefix (string-append
                      "WEBSERVER_ROOT=" (ws-shell-quote-local root) " "
                      "WEBSERVER_CONFIG=" (ws-shell-quote-local cfg) " "
                      "WEBSERVER_PID_FILE=" (ws-shell-quote-local pid) " "
                      "WEBSERVER_SERVER_LOG=" (ws-shell-quote-local server-log) " "
                      "WEBSERVER_ACCESS_LOG=" (ws-shell-quote-local access-log) " "
                      "WEBSERVER_ERROR_LOG=" (ws-shell-quote-local error-log) " ")))
    (system (string-append "rm -f "
                           (ws-shell-quote-local pid) " "
                           (ws-shell-quote-local server-log) " "
                           (ws-shell-quote-local access-log) " "
                           (ws-shell-quote-local error-log) " "
                           (ws-shell-quote-local out-test) " "
                           (ws-shell-quote-local out-logs)))

    (ws-write-file-text
     cfg
     (string-append
      "(webserver-config\n"
      "  (listen_port " (format nil "~A" tmp-port) ")\n"
      "  (document_root \"./examples/webserver/runtime/docroot\")\n"
      "  (tls_cert_file \"./examples/webserver/runtime/tls/server.crt\")\n"
      "  (tls_key_file \"./examples/webserver/runtime/tls/server.key\")\n"
      "  (cgi_enabled #t)\n"
      "  (cgi_bin_dir \"./examples/webserver/runtime/cgi-bin\")\n"
      "  (max_connections 100))"))

    ;; -t equivalent
    (assert-true
     "ctl test config exits 0"
     (= (system (string-append
                 "sh -c \"" cmd-prefix
                 "WEBSERVER_CMD=-t "
                 "./bin/isl " (ws-shell-quote-local ctl)
                 " > " (ws-shell-quote-local out-test) " 2>&1\""))
        0))
    (assert-true "ctl test output"
                 (ws-contains (ws-read-file-text-local out-test) "config check passed"))

    ;; start command (daemon mode default)
    (assert-true
     "ctl start exits 0"
     (= (system (string-append
                 "sh -c \"" cmd-prefix
                 "WEBSERVER_CMD=start "
                 "WEBSERVER_TRANSPORT=http "
                 "./bin/isl " (ws-shell-quote-local ctl)
                 "\""))
        0))
    (assert-true "pid file created" (not (null (probe-file pid))))

    (let ((r (ws-request-once
              tmp-port
              "GET /public/index.html HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")))
      (assert-true "started server responds 200" (= (ws-status-code r) 200)))

    (assert-true "sleep for access log flush" (= (system "sh -c 'sleep 1'") 0))

    ;; logs command
    (assert-true
     "ctl logs exits 0"
     (= (system (string-append
                 "sh -c \"" cmd-prefix
                 "WEBSERVER_CMD=logs "
                 "WEBSERVER_LOG_KIND=access "
                 "WEBSERVER_TAIL_LINES=20 "
                 "./bin/isl " (ws-shell-quote-local ctl)
                 " > " (ws-shell-quote-local out-logs) " 2>&1\""))
        0))
    (assert-true "ctl logs output contains status"
                 (ws-contains (ws-read-file-text-local out-logs) "status=200"))

    ;; stop command
    (assert-true
     "ctl stop exits 0"
     (= (system (string-append
                 "sh -c \"" cmd-prefix
                 "WEBSERVER_CMD=stop "
                 "./bin/isl " (ws-shell-quote-local ctl)
                 "\""))
        0))
    (assert-true "pid file removed" (null (probe-file pid)))

    (delete-file cfg)
    (format t "t10-ops-commands-integration: ok~%")))

(run-tests)
