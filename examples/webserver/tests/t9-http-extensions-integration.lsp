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
         (tmp-port (+ 20000 (mod (get-universal-time) 1000)))
         (cfg (string-append root "/tests/tmp-t9-webserver.conf.lsp"))
         (log "/tmp/webserver-t9-integration.log")
         (pid "/tmp/webserver-t9-integration.pid")
         (index-path (string-append root "/runtime/docroot/public/index.html"))
         (index-body (ws-read-file-raw index-path))
         (index-prefix (substring index-body 0 10))
         (index-size (length index-body)))
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

    (ws-start-server root cfg 16 log pid)

    (let* ((r1 (ws-request-once
                tmp-port
                "GET /public/index.html HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"))
           (h1 (ws-resp-get r1 'headers))
           (last-modified (ws-header-get h1 "last-modified"))
           (r-ims (ws-request-once
                   tmp-port
                   (string-append
                    "GET /public/index.html HTTP/1.1\r\n"
                    "Host: localhost\r\n"
                    "If-Modified-Since: " last-modified "\r\n"
                    "Connection: close\r\n\r\n")))
           (r-range (ws-request-once
                     tmp-port
                     "GET /public/index.html HTTP/1.1\r\nHost: localhost\r\nRange: bytes=0-9\r\nConnection: close\r\n\r\n"))
           (r-range-head (ws-request-once-no-body
                          tmp-port
                          "HEAD /public/index.html HTTP/1.1\r\nHost: localhost\r\nRange: bytes=0-9\r\nConnection: close\r\n\r\n"))
           (r-range-bad (ws-request-once
                         tmp-port
                         (string-append
                          "GET /public/index.html HTTP/1.1\r\n"
                          "Host: localhost\r\n"
                          "Range: bytes=" (format nil "~A" (+ index-size 1000)) "-\r\n"
                          "Connection: close\r\n\r\n"))))
      (assert-true "GET 200" (= (ws-status-code r1) 200))
      (assert-true "GET has Last-Modified" (not (null last-modified)))
      (assert-true "GET has Accept-Ranges bytes"
                   (string= (ws-header-get h1 "accept-ranges") "bytes"))

      (assert-true "If-Modified-Since returns 304" (= (ws-status-code r-ims) 304))
      (assert-true "304 body is empty" (= (length (ws-resp-get r-ims 'body)) 0))

      (assert-true "Range GET returns 206" (= (ws-status-code r-range) 206))
      (assert-true "Range GET body exact" (string= (ws-resp-get r-range 'body) index-prefix))
      (assert-true "Range GET Content-Range"
                   (string= (ws-header-get (ws-resp-get r-range 'headers) "content-range")
                            (format nil "bytes 0-9/~A" index-size)))

      (assert-true "Range HEAD returns 206" (= (ws-status-code r-range-head) 206))
      (assert-true "Range HEAD body empty" (= (length (ws-resp-get r-range-head 'body)) 0))
      (assert-true "Range HEAD content-length=10"
                   (string= (ws-header-get (ws-resp-get r-range-head 'headers) "content-length") "10"))

      (assert-true "Unsat range returns 416" (= (ws-status-code r-range-bad) 416))
      (assert-true "416 includes */size"
                   (string= (ws-header-get (ws-resp-get r-range-bad 'headers) "content-range")
                            (format nil "bytes */~A" index-size))))

    ;; Keep-Alive: two requests over one connection.
    (let ((conn (tcp-connect "127.0.0.1" tmp-port)))
      (tcp-send conn "GET /public/index.html HTTP/1.1\r\nHost: localhost\r\nConnection: keep-alive\r\n\r\n")
      (let ((ka1 (ws-read-response-from-conn* conn #f)))
        (assert-true "keep-alive first status 200" (= (ws-status-code ka1) 200))
        (assert-true "keep-alive header" (string= (ws-header-get (ws-resp-get ka1 'headers) "connection") "keep-alive")))
      (tcp-send conn "GET /public/index.html HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")
      (let ((ka2 (ws-read-response-from-conn* conn #f)))
        (assert-true "keep-alive second status 200" (= (ws-status-code ka2) 200)))
      (tcp-close conn))

    (assert-true "sleep for graceful stop" (= (system "sh -c 'sleep 1'") 0))
    (ws-stop-server-if-running pid)
    (delete-file cfg)
    (delete-file pid)
    (format t "t9-http-extensions-integration: ok~%")))

(run-tests)
