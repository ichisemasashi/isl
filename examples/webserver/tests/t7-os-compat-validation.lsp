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

(defun run-tests ()
  (let* ((root (ws-test-root))
         (cfg-path (string-append root "/conf/webserver.conf.lsp"))
         (cfg (ws-validate-config-file cfg-path))
         (os-family (ws-config-get cfg 'os_family)))
    (assert-true "detect linux" (string= (ws-detect-os-family "Linux") "linux"))
    (assert-true "detect freebsd" (string= (ws-detect-os-family "FreeBSD") "freebsd"))
    (assert-true "detect darwin" (string= (ws-detect-os-family "Darwin") "darwin"))
    (assert-true "detect unknown" (string= (ws-detect-os-family "SomethingElse") "unknown"))
    (assert-true "required sh command exists" (ws-command-available-p "sh"))
    (assert-true "required test command exists" (ws-command-available-p "test"))
    (assert-true "os family normalized"
                 (or (string= os-family "linux")
                     (string= os-family "freebsd")
                     (string= os-family "darwin")
                     (string= os-family "unknown"))))
  (format t "t7-os-compat-validation: ok~%"))

(run-tests)
