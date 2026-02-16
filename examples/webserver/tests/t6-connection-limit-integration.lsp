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

(defun ws-read-response-from-conn (conn)
  (ws-read-response-from-conn* conn #f))

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

(defun ws-open-held-connection (port)
  (let ((conn (tcp-connect "127.0.0.1" port)))
    (tcp-send conn "GET /public/index.html HTTP/1.1\r\nHost: localhost\r\nConnection: keep-alive\r\n\r\n")
    (let ((resp (ws-read-response-from-conn conn)))
      (assert-true "held connection first response is 200" (= (ws-status-code resp) 200))
      (assert-true "held connection is keep-alive"
                   (string= (ws-header-get (ws-resp-get resp 'headers) "connection") "keep-alive")))
    conn))

(defun ws-close-held-connection (conn)
  (tcp-send conn "GET /public/index.html HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")
  (let ((resp (ws-read-response-from-conn conn)))
    (assert-true "held connection close response is 200" (= (ws-status-code resp) 200)))
  (tcp-close conn))

(defun run-tests ()
  (let* ((root (ws-test-root))
         (tmp-port 18086)
         (cfg (string-append root "/tests/tmp-t6-webserver.conf.lsp"))
         (log "/tmp/webserver-t6-integration.log")
         (pid "/tmp/webserver-t6-integration.pid")
         (held '())
         (i 0))
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

    (ws-start-server root cfg 102 log pid)

    ;; Keep 100 connections active to reach the configured limit.
    (while (< i 100)
      (setq held (cons (ws-open-held-connection tmp-port) held))
      (setq i (+ i 1)))

    ;; The 101st concurrent connection should be rejected (503 or close after overload).
    (let ((conn101 (tcp-connect "127.0.0.1" tmp-port)))
      (tcp-send conn101 "GET /public/index.html HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")
      (let ((r101 (ws-read-response-from-conn conn101)))
        (assert-true "over limit returns 503"
                     (= (ws-status-code r101) 503)))
      (tcp-close conn101))

    ;; Release one active connection and verify new connection can be accepted again.
    (ws-close-held-connection (car held))
    (setq held (cdr held))
    (assert-true "sleep after release" (= (system "sh -c 'sleep 1'") 0))
    (let ((conn-next (tcp-connect "127.0.0.1" tmp-port)))
      (tcp-send conn-next "GET /public/index.html HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")
      (let ((r-next (ws-read-response-from-conn conn-next)))
        (assert-true "after release returns 200"
                     (= (ws-status-code r-next) 200)))
      (tcp-close conn-next))

    ;; Cleanup held keep-alive connections.
    (while (not (null held))
      (ws-close-held-connection (car held))
      (setq held (cdr held)))

    (assert-true "sleep for graceful stop" (= (system "sh -c 'sleep 1'") 0))
    (ws-stop-server-if-running pid)
    (delete-file cfg)
    (delete-file pid)
    (format t "t6-connection-limit-integration: ok~%")))

(run-tests)
