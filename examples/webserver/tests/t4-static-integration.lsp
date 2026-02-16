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
         (tmp-port 18084)
         (cfg (string-append root "/tests/tmp-t4-webserver.conf.lsp"))
         (log "/tmp/webserver-t4-integration.log")
         (pid "/tmp/webserver-t4-integration.pid")
         (docroot (string-append root "/runtime/docroot"))
         (private-dir (string-append docroot "/private"))
         (private-file (string-append private-dir "/secret.txt"))
         (public-leak (string-append docroot "/public/leak.txt")))
    (assert-true "prepare private dir"
                 (= (system (string-append "mkdir -p " (ws-shell-quote-local private-dir))) 0))
    (ws-write-file-text private-file "top-secret")
    (system (string-append "rm -f " (ws-shell-quote-local public-leak)))
    (assert-true "create symlink to private"
                 (= (system (string-append "ln -s " (ws-shell-quote-local private-file)
                                           " " (ws-shell-quote-local public-leak))) 0))

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

    (let* ((r-ok (ws-request-once
                  tmp-port
                  "GET /public/index.html HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"))
           (r-head (ws-request-once-no-body
                    tmp-port
                    "HEAD /public/index.html HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"))
           (r-private (ws-request-once
                       tmp-port
                       "GET /private/secret.txt HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"))
           (r-traversal (ws-request-once
                         tmp-port
                         "GET /public/../private/secret.txt HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"))
           (r-symlink (ws-request-once
                       tmp-port
                       "GET /public/leak.txt HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"))
           (head-headers (ws-resp-get r-head 'headers))
           (get-headers (ws-resp-get r-ok 'headers))
           (code-private (ws-status-code r-private))
           (code-traversal (ws-status-code r-traversal))
           (code-symlink (ws-status-code r-symlink)))
      (assert-true "public index status 200"
                   (= (ws-status-code r-ok) 200))
      (assert-true "public index body content"
                   (ws-contains (ws-resp-get r-ok 'body) "<!doctype html>"))
      (assert-true "HEAD status 200"
                   (= (ws-status-code r-head) 200))
      (assert-true "HEAD body empty"
                   (= (length (ws-resp-get r-head 'body)) 0))
      (assert-true "HEAD and GET content-type are same"
                   (string= (ws-header-get head-headers "content-type")
                            (ws-header-get get-headers "content-type")))
      (assert-true "HEAD and GET content-length are same"
                   (string= (ws-header-get head-headers "content-length")
                            (ws-header-get get-headers "content-length")))
      (assert-true "private path is denied"
                   (or (= code-private 403) (= code-private 404)))
      (assert-true ".. traversal is denied"
                   (or (= code-traversal 403) (= code-traversal 404)))
      (assert-true "symlink escape is denied"
                   (or (= code-symlink 403) (= code-symlink 404))))

    (assert-true "sleep for graceful stop" (= (system "sh -c 'sleep 1'") 0))
    (ws-stop-server-if-running pid)
    (system (string-append "rm -f " (ws-shell-quote-local public-leak)))
    (delete-file private-file)
    (delete-file cfg)
    (delete-file pid)
    (format t "t4-static-integration: ok~%")))

(run-tests)
