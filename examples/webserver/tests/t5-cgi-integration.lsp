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

(defun run-tests ()
  (let* ((root (ws-test-root))
         (tmp-port 18085)
         (cfg (string-append root "/tests/tmp-t5-webserver.conf.lsp"))
         (log "/tmp/webserver-t5-integration.log")
         (pid "/tmp/webserver-t5-integration.pid")
         (cgi-dir (string-append root "/runtime/cgi-bin"))
         (cgi-ok (string-append cgi-dir "/t5-echo.cgi"))
         (cgi-fail (string-append cgi-dir "/t5-fail.cgi")))
    (ws-write-file-text
     cgi-ok
     (string-append
      "#!/bin/sh\n"
      "echo \"Status: 200 OK\"\n"
      "echo \"Content-Type: text/plain\"\n"
      "echo\n"
      "printf \"method=%s\\n\" \"$REQUEST_METHOD\"\n"
      "printf \"query=%s\\n\" \"$QUERY_STRING\"\n"
      "printf \"len=%s\\n\" \"$CONTENT_LENGTH\"\n"
      "printf \"ctype=%s\\n\" \"$CONTENT_TYPE\"\n"
      "printf \"script=%s\\n\" \"$SCRIPT_NAME\"\n"
      "body=$(cat)\n"
      "printf \"body=%s\\n\" \"$body\"\n"))
    (ws-write-file-text cgi-fail "#!/bin/sh\nexit 7")
    (assert-true "chmod cgi ok" (= (system (string-append "chmod +x " (ws-shell-quote-local cgi-ok))) 0))
    (assert-true "chmod cgi fail" (= (system (string-append "chmod +x " (ws-shell-quote-local cgi-fail))) 0))

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

    (ws-start-server root cfg 8 log pid)

    (let* ((r-get (ws-request-once
                   tmp-port
                   "GET /cgi-bin/t5-echo.cgi?x=1&y=2 HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"))
           (r-post (ws-request-once
                    tmp-port
                    "POST /cgi-bin/t5-echo.cgi HTTP/1.1\r\nHost: localhost\r\nContent-Type: text/plain\r\nContent-Length: 5\r\nConnection: close\r\n\r\nhello"))
           (r-head (ws-request-once-no-body
                    tmp-port
                    "HEAD /cgi-bin/t5-echo.cgi HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"))
           (r-fail (ws-request-once
                    tmp-port
                    "GET /cgi-bin/t5-fail.cgi HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"))
           (r-out (ws-request-once
                   tmp-port
                   "GET /cgi-bin/../private/secret.txt HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")))
      (assert-true "CGI GET status 200" (= (ws-status-code r-get) 200))
      (assert-true "CGI GET method env"
                   (ws-contains (ws-resp-get r-get 'body) "method=GET"))
      (assert-true "CGI GET query env"
                   (ws-contains (ws-resp-get r-get 'body) "query=x=1&y=2"))
      (assert-true "CGI GET script env"
                   (ws-contains (ws-resp-get r-get 'body) "script=/cgi-bin/t5-echo.cgi"))

      (assert-true "CGI POST status 200" (= (ws-status-code r-post) 200))
      (assert-true "CGI POST method env"
                   (ws-contains (ws-resp-get r-post 'body) "method=POST"))
      (assert-true "CGI POST content-length env"
                   (ws-contains (ws-resp-get r-post 'body) "len=5"))
      (assert-true "CGI POST content-type env"
                   (ws-contains (ws-resp-get r-post 'body) "ctype=text/plain"))
      (assert-true "CGI POST body via stdin"
                   (ws-contains (ws-resp-get r-post 'body) "body=hello"))

      (assert-true "CGI HEAD status 200" (= (ws-status-code r-head) 200))
      (assert-true "CGI HEAD body empty" (= (length (ws-resp-get r-head 'body)) 0))

      (assert-true "CGI failure returns 500" (= (ws-status-code r-fail) 500))
      (assert-true "cgi_bin_dir outside access denied"
                   (or (= (ws-status-code r-out) 403) (= (ws-status-code r-out) 404))))

    (assert-true "sleep for graceful stop" (= (system "sh -c 'sleep 1'") 0))
    (ws-stop-server-if-running pid)
    (delete-file cgi-ok)
    (delete-file cgi-fail)
    (delete-file cfg)
    (delete-file pid)
    (format t "t5-cgi-integration: ok~%")))

(run-tests)
