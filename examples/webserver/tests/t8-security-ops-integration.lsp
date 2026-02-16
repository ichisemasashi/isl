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

(defun ws-start-server (root cfg-path max-accepts log-path pid-path access-log error-log cgi-timeout)
  (let* ((bin (string-append (substring root 0 (- (length root) (length "/examples/webserver"))) "/bin/isl"))
         (app (string-append root "/app/main.lsp"))
         (cmd (string-append
               "sh -c \""
               "WEBSERVER_ROOT=" (ws-shell-quote-local root) " "
               "WEBSERVER_CONFIG=" (ws-shell-quote-local cfg-path) " "
               "WEBSERVER_MAX_ACCEPTS=" (format nil "~A" max-accepts) " "
               "WEBSERVER_ACCESS_LOG=" (ws-shell-quote-local access-log) " "
               "WEBSERVER_ERROR_LOG=" (ws-shell-quote-local error-log) " "
               "WEBSERVER_CGI_TIMEOUT_SEC=" (format nil "~A" cgi-timeout) " "
               (ws-shell-quote-local bin) " "
               (ws-shell-quote-local app) " > "
               (ws-shell-quote-local log-path)
               " 2>&1 & echo \\$! > "
               (ws-shell-quote-local pid-path)
               "\"")))
    (assert-true "start server process" (= (system cmd) 0))
    (assert-true "sleep for startup" (= (system "sh -c 'sleep 1'") 0))))

(defun ws-stop-server-if-running (pid-path)
  (let ((cmd (string-append
              "sh -c \"if [ -f "
              (ws-shell-quote-local pid-path)
              " ]; then kill $(cat "
              (ws-shell-quote-local pid-path)
              ") >/dev/null 2>&1 || true; fi\"")))
    (system cmd)))

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

(defun ws-resp-get (resp key)
  (ws-config-get resp key))

(defun ws-status-code (resp)
  (let* ((line (ws-resp-get resp 'status-line))
         (a (ws-split-once line " ")))
    (if (null a)
        0
        (let ((b (ws-split-once (second a) " ")))
          (if (null b)
              0
              (ws-parse-int (car b)))))))

(defun ws-repeat-char (ch n)
  (let ((i 0)
        (out ""))
    (while (< i n)
      (setq out (string-append out ch))
      (setq i (+ i 1)))
    out))

(defun run-tests ()
  (let* ((root (ws-test-root))
         (tmp-port (+ 19000 (mod (get-universal-time) 1000)))
         (cfg (string-append root "/tests/tmp-t8-webserver.conf.lsp"))
         (log "/tmp/webserver-t8-integration.log")
         (pid "/tmp/webserver-t8-integration.pid")
         (access-log "/tmp/webserver-t8-access.log")
         (error-log "/tmp/webserver-t8-error.log")
         (cgi-dir (string-append root "/runtime/cgi-bin"))
         (cgi-slow (string-append cgi-dir "/t8-slow.cgi"))
         (long-header (ws-repeat-char "a" 9000)))
    (system (string-append "rm -f " (ws-shell-quote-local access-log) " " (ws-shell-quote-local error-log)))
    (ws-write-file-text cgi-slow "#!/bin/sh\nwhile :; do\n  sleep 1\ndone")
    (assert-true "chmod slow cgi" (= (system (string-append "chmod +x " (ws-shell-quote-local cgi-slow))) 0))
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
    (ws-start-server root cfg 6 log pid access-log error-log 1)

    (let* ((r-ok (ws-request-once
                  tmp-port
                  "GET /public/index.html HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"))
           (r-header-too-large (ws-request-once
                                tmp-port
                                (string-append
                                 "GET /public/index.html HTTP/1.1\r\n"
                                 "Host: localhost\r\n"
                                 "X-Long: " long-header "\r\n"
                                 "Connection: close\r\n\r\n")))
           (r-body-too-large (ws-request-once
                              tmp-port
                              "POST /submit HTTP/1.1\r\nHost: localhost\r\nContent-Length: 1048577\r\nConnection: close\r\n\r\n"))
           (r-cgi-timeout (ws-request-once
                           tmp-port
                           "GET /cgi-bin/t8-slow.cgi HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")))
      (assert-true "normal request 200" (= (ws-status-code r-ok) 200))
      (assert-true "header too large => 431" (= (ws-status-code r-header-too-large) 431))
      (assert-true "body too large => 413" (= (ws-status-code r-body-too-large) 413))
      (assert-true "cgi timeout => 504" (= (ws-status-code r-cgi-timeout) 504)))

    (assert-true "sleep for flush logs" (= (system "sh -c 'sleep 1'") 0))
    (let ((access (ws-read-file-text-local access-log))
          (errors (ws-read-file-text-local error-log)))
      (assert-true "access log has 200" (ws-contains access "status=200"))
      (assert-true "access log has 431" (ws-contains access "status=431"))
      (assert-true "access log has 413" (ws-contains access "status=413"))
      (assert-true "access log has 504" (ws-contains access "status=504"))
      (assert-true "access log has method/path"
                   (ws-contains access "method=GET path=/public/index.html"))
      (assert-true "error log has timeout event" (ws-contains errors "cgi timeout"))
      (assert-true "error log has rejected request event" (ws-contains errors "request rejected status=431")))

    (ws-stop-server-if-running pid)
    (delete-file cgi-slow)
    (delete-file cfg)
    (delete-file pid)
    (format t "t8-security-ops-integration: ok~%")))

(run-tests)
