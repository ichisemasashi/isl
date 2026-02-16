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

(defun assert-error (label thunk)
  (let ((result (catch 'ws-error
                  (progn
                    (funcall thunk)
                    'no-error))))
    (let ((ok (not (eq result 'no-error))))
      (assert-true label ok))))

(defun write-config-text (path text)
  (with-open-file (s path :direction :output :if-exists :overwrite)
    (format s "~A~%" text)))

(defun run-tests ()
  (let* ((root (ws-test-root))
         (cfg-ok (string-append root "/conf/webserver.conf.lsp"))
         (tmp-bad-port (string-append root "/tests/tmp-bad-port.lsp"))
         (tmp-missing-key (string-append root "/tests/tmp-missing-key.lsp")))
    (let ((cfg (ws-validate-config-file cfg-ok)))
      (assert-true "valid config listen_port=18080" (= (ws-config-get cfg 'listen_port) 18080))
      (assert-true "document_root is absolute path"
                   (ws-starts-with (ws-config-get cfg 'document_root) "/")))

    (write-config-text
     tmp-bad-port
     "(webserver-config
  (listen_port 70000)
  (document_root \"./examples/webserver/runtime/docroot\")
  (tls_cert_file \"./examples/webserver/runtime/tls/server.crt\")
  (tls_key_file \"./examples/webserver/runtime/tls/server.key\")
  (cgi_enabled #t)
  (cgi_bin_dir \"./examples/webserver/runtime/cgi-bin\")
  (max_connections 100))")
    (assert-error
     "listen_port out of range must fail"
     (lambda () (ws-validate-config-file tmp-bad-port)))

    (write-config-text
     tmp-missing-key
     "(webserver-config
  (listen_port 18080)
  (document_root \"./examples/webserver/runtime/docroot\")
  (tls_cert_file \"./examples/webserver/runtime/tls/server.crt\")
  (tls_key_file \"./examples/webserver/runtime/tls/server.key\")
  (cgi_enabled #t)
  (max_connections 100))")
    (assert-error
     "missing key must fail"
     (lambda () (ws-validate-config-file tmp-missing-key)))

    (delete-file tmp-bad-port)
    (delete-file tmp-missing-key)
    (format t "t1-config-validation: ok~%")))

(run-tests)
