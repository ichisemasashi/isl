(defun ws-test-root ()
  (let ((v (getenv "WEBSERVER_ROOT")))
    (if (null v)
        "/Volumes/SSD-PLU3/work/LISP/islisp/isl/examples/webserver"
        v)))

(load (string-append (ws-test-root) "/app/config.lsp"))

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

(defun ws-isl-bin (root)
  (string-append (substring root 0 (- (length root) (length "/examples/webserver"))) "/bin/isl"))

(defun ws-main-path (root)
  (string-append root "/app/main.lsp"))

(defun ws-start-https-server (root cfg-path max-accepts log-path pid-path)
  (let* ((bin (ws-isl-bin root))
         (app (ws-main-path root))
         (cmd (string-append
               "sh -c \""
               "WEBSERVER_ROOT=" (ws-shell-quote-local root) " "
               "WEBSERVER_CONFIG=" (ws-shell-quote-local cfg-path) " "
               "WEBSERVER_TRANSPORT=https "
               "WEBSERVER_MAX_ACCEPTS=" (format nil "~A" max-accepts) " "
               (ws-shell-quote-local bin) " "
               (ws-shell-quote-local app) " > "
               (ws-shell-quote-local log-path)
               " 2>&1 & echo \\$! > "
               (ws-shell-quote-local pid-path)
               "\"")))
    (assert-true "start https server process" (= (system cmd) 0))
    (assert-true "sleep for startup" (= (system "sh -c 'sleep 1'") 0))))

(defun ws-stop-server-if-running (pid-path)
  (let ((cmd (string-append
              "sh -c \"if [ -f "
              (ws-shell-quote-local pid-path)
              " ]; then kill $(cat "
              (ws-shell-quote-local pid-path)
              ") >/dev/null 2>&1 || true; fi\"")))
    (system cmd)))

(defun run-tests ()
  (let* ((root (ws-test-root))
         (port 18443)
         (cfg (string-append root "/tests/tmp-t3-webserver.conf.lsp"))
         (cfg-bad (string-append root "/tests/tmp-t3-webserver-bad.conf.lsp"))
         (invalid-cert (string-append root "/tests/tmp-t3-invalid-cert.pem"))
         (log "/tmp/webserver-t3-integration.log")
         (pid "/tmp/webserver-t3-integration.pid")
         (out-sclient "/tmp/webserver-t3-sclient.txt")
         (out-curl "/tmp/webserver-t3-curl.txt")
         (out-curl-cgi "/tmp/webserver-t3-curl-cgi.txt")
         (out-bad "/tmp/webserver-t3-bad.txt")
         (cert "./examples/webserver/runtime/tls/server.crt")
         (key "./examples/webserver/runtime/tls/server.key"))
    (ws-write-file-text
     cfg
     (string-append
      "(webserver-config\n"
      "  (listen_port " (format nil "~A" port) ")\n"
      "  (document_root \"./examples/webserver/runtime/docroot\")\n"
      "  (tls_cert_file \"" cert "\")\n"
      "  (tls_key_file \"" key "\")\n"
      "  (cgi_enabled #t)\n"
      "  (cgi_bin_dir \"./examples/webserver/runtime/cgi-bin\")\n"
      "  (max_connections 100))"))

    (ws-start-https-server root cfg 3 log pid)

    (let* ((sclient-cmd (string-append
                         "sh -c \""
                         "echo | openssl s_client -connect 127.0.0.1:" (format nil "~A" port)
                         " -servername localhost -showcerts > "
                         (ws-shell-quote-local out-sclient)
                         " 2>&1\""))
           (curl-cmd (string-append
                      "sh -c \"curl -k -sS -i https://127.0.0.1:"
                      (format nil "~A" port)
                      "/ > "
                      (ws-shell-quote-local out-curl)
                      "\""))
           (curl-cgi-cmd (string-append
                          "sh -c \"curl -k -sS -i https://127.0.0.1:"
                          (format nil "~A" port)
                          "/cgi-bin/health.cgi > "
                          (ws-shell-quote-local out-curl-cgi)
                      "\"")))
      (assert-true "openssl s_client succeeds" (= (system sclient-cmd) 0))
      (assert-true "curl https succeeds" (= (system curl-cmd) 0))
      (assert-true "curl https cgi succeeds" (= (system curl-cgi-cmd) 0)))

    (assert-true "sleep for graceful stop" (= (system "sh -c 'sleep 1'") 0))
    (let ((sclient-out (ws-read-file-text out-sclient))
          (curl-out (ws-read-file-text out-curl))
          (curl-cgi-out (ws-read-file-text out-curl-cgi))
          (server-log (ws-read-file-text log)))
      (assert-true "certificate is presented"
                   (ws-contains sclient-out "BEGIN CERTIFICATE"))
      (assert-true "curl gets 200"
                   (ws-contains curl-out "HTTP/1.1 200 OK"))
      (assert-true "curl cgi gets 200"
                   (ws-contains curl-cgi-out "HTTP/1.1 200 OK"))
      (assert-true "curl cgi body"
                   (ws-contains curl-cgi-out "ok"))
      (assert-true "https listener log"
                   (ws-contains server-log "https server listening on port 18443"))
      (assert-true "https stopped log"
                   (ws-contains server-log "https server stopped")))

    ;; Bad certificate path should fail to start with explicit error.
    (ws-write-file-text invalid-cert "this is not a certificate")
    (ws-write-file-text
     cfg-bad
     (string-append
      "(webserver-config\n"
      "  (listen_port " (format nil "~A" (+ port 1)) ")\n"
      "  (document_root \"./examples/webserver/runtime/docroot\")\n"
      "  (tls_cert_file \"" invalid-cert "\")\n"
      "  (tls_key_file \"" key "\")\n"
      "  (cgi_enabled #t)\n"
      "  (cgi_bin_dir \"./examples/webserver/runtime/cgi-bin\")\n"
      "  (max_connections 100))"))
    (let* ((bad-cmd (string-append
                     "sh -c \""
                     "WEBSERVER_ROOT=" (ws-shell-quote-local root) " "
                     "WEBSERVER_CONFIG=" (ws-shell-quote-local cfg-bad) " "
                     "WEBSERVER_TRANSPORT=https "
                     (ws-shell-quote-local (ws-isl-bin root)) " "
                     (ws-shell-quote-local (ws-main-path root))
                     " > " (ws-shell-quote-local out-bad) " 2>&1\""))
           (st (system bad-cmd))
           (bad-out (ws-read-file-text out-bad)))
      (assert-true "bad cert startup fails" (not (= st 0)))
      (assert-true "bad cert error contains certificate load failure"
                   (ws-contains bad-out "Couldn't load certificate")))

    (ws-stop-server-if-running pid)
    (delete-file cfg)
    (delete-file cfg-bad)
    (delete-file invalid-cert)
    (delete-file pid)
    (format t "t3-https-integration: ok~%")))

(run-tests)
