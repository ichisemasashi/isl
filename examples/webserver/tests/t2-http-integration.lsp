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

(defun ws-read-file-text (path)
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

(defun ws-write-file-text (path text)
  (with-open-file (s path :direction :output :if-exists :overwrite)
    (format s "~A~%" text)))

(defun ws-shell-quote-local (s)
  (if (ws-contains s "'")
      (throw 'ws-test-failed "single quote is not supported in shell path")
      (string-append "'" s "'")))

(defun ws-start-server (root cfg-path max-accepts log-path pid-path)
  (let* ((bin (string-append (substring root 0 (- (length root) (length "/examples/webserver"))) "/bin/isl"))
         (app (string-append root "/app/main.lsp"))
         (cmd (string-append
               "sh -c \""
               "WEBSERVER_ROOT=" (ws-shell-quote-local root) " "
               "WEBSERVER_CONFIG=" (ws-shell-quote-local cfg-path) " "
               "WEBSERVER_MAX_ACCEPTS=" (format nil "~A" max-accepts) " "
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

(defun ws-read-response-from-conn (conn)
  (ws-read-response-from-conn* conn #f))

(defun ws-read-response-from-conn-no-body (conn)
  (ws-read-response-from-conn* conn #t))

(defun ws-resp-get (resp key)
  (ws-config-get resp key))

(defun ws-request-once* (port req skip-body)
  (let ((conn (tcp-connect "127.0.0.1" port)))
    (tcp-send conn req)
    (let ((resp (ws-read-response-from-conn* conn skip-body)))
      (tcp-close conn)
      resp)))

(defun ws-request-once (port req)
  (ws-request-once* port req #f))

(defun ws-request-once-no-body (port req)
  (ws-request-once* port req #t))

(defun run-tests ()
  (let* ((root (ws-test-root))
         (tmp-port 18081)
         (cfg (string-append root "/tests/tmp-t2-webserver.conf.lsp"))
         (log "/tmp/webserver-t2-integration.log")
         (pid "/tmp/webserver-t2-integration.pid"))
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

    (ws-start-server root cfg 6 log pid)

    (let ((r-get (ws-request-once
                  tmp-port
                  "GET / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"))
          (r-head (ws-request-once-no-body
                   tmp-port
                   "HEAD / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"))
          (r-post (ws-request-once
                   tmp-port
                   "POST /submit HTTP/1.1\r\nHost: localhost\r\nContent-Length: 5\r\nConnection: close\r\n\r\nhello"))
          (r-http10 (ws-request-once
                     tmp-port
                     "GET /legacy HTTP/1.0\r\nHost: localhost\r\n\r\n"))
          (r-bad (ws-request-once
                  tmp-port
                  "BADREQUEST\r\n\r\n")))
      (assert-true "GET status 200"
                   (ws-contains (ws-resp-get r-get 'status-line) "200 OK"))
      (assert-true "HEAD status 200"
                   (ws-contains (ws-resp-get r-head 'status-line) "200 OK"))
      (assert-true "HEAD body empty"
                   (= (length (ws-resp-get r-head 'body)) 0))
      (assert-true "POST status 200"
                   (ws-contains (ws-resp-get r-post 'status-line) "200 OK"))
      (assert-true "POST body includes bytes"
                   (ws-contains (ws-resp-get r-post 'body) "bytes=5"))
      (assert-true "HTTP/1.0 status 200"
                   (ws-contains (ws-resp-get r-http10 'status-line) "HTTP/1.0 200"))
      (assert-true "Bad request status 400"
                   (ws-contains (ws-resp-get r-bad 'status-line) "400 Bad Request")))

    (let ((conn (tcp-connect "127.0.0.1" tmp-port)))
      (tcp-send conn "GET /a HTTP/1.1\r\nHost: localhost\r\nConnection: keep-alive\r\n\r\n")
      (let ((r1 (ws-read-response-from-conn conn)))
        (assert-true "Keep-alive first response 200"
                     (ws-contains (ws-resp-get r1 'status-line) "200 OK"))
        (assert-true "Keep-alive first connection header"
                     (string= (ws-header-get (ws-resp-get r1 'headers) "connection") "keep-alive")))
      (tcp-send conn "GET /b HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")
      (let ((r2 (ws-read-response-from-conn conn)))
        (assert-true "Keep-alive second response 200"
                     (ws-contains (ws-resp-get r2 'status-line) "200 OK"))
        (assert-true "Keep-alive second connection header"
                     (string= (ws-header-get (ws-resp-get r2 'headers) "connection") "close")))
      (tcp-close conn))

    (assert-true "sleep for graceful stop" (= (system "sh -c 'sleep 1'") 0))
    (let ((server-log (ws-read-file-text log)))
      (assert-true "server log contains listening"
                   (ws-contains server-log "http server listening on port 18081"))
      (assert-true "server log contains stopped"
                   (ws-contains server-log "http server stopped")))

    (delete-file cfg)
    (delete-file pid)
    (format t "t2-http-integration: ok~%")))

(run-tests)
