(defun ws-root ()
  (let ((v (getenv "WEBSERVER_ROOT")))
    (if (null v)
        "/Volumes/SSD-PLU3/work/LISP/islisp/isl/examples/webserver"
        v)))

(load (string-append (ws-root) "/app/config.lsp"))
(load (string-append (ws-root) "/app/http_server.lsp"))

(defun ws-main ()
  (let ((config-path (getenv "WEBSERVER_CONFIG"))
        (check-mode (getenv "WEBSERVER_CHECK_CONFIG")))
    (if (null config-path)
        (ws-die "WEBSERVER_CONFIG is required")
        (let ((cfg (ws-validate-config-file config-path)))
          (ws-print-config cfg)
          (if (and (not (null check-mode)) (string= check-mode "1"))
              (ws-log "config check passed")
              (ws-start-http-server cfg))))))

(ws-main)
