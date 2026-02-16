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

(defun assert-error (label thunk)
  (let ((result (catch 'ws-bad-request
                  (progn
                    (funcall thunk)
                    'no-error))))
    (assert-true label (not (eq result 'no-error)))))

(defun run-tests ()
  (let ((r1 (ws-parse-request-line "GET / HTTP/1.1"))
        (r2 (ws-parse-request-line "POST /submit HTTP/1.0")))
    (assert-true "parse method GET" (string= (car r1) "GET"))
    (assert-true "parse target /" (string= (second r1) "/"))
    (assert-true "parse version 1.1" (string= (third r1) "HTTP/1.1"))
    (assert-true "parse method POST" (string= (car r2) "POST"))
    (assert-true "parse version 1.0" (string= (third r2) "HTTP/1.0")))

  (assert-error
   "unsupported version must fail"
   (lambda ()
     (ws-parse-request-line "GET / HTTP/2.0")))

  (assert-true
   "keep-alive default for 1.1"
   (ws-connection-keep-alive-p "HTTP/1.1" '()))
  (assert-true
   "close for 1.1 connection: close"
   (not (ws-connection-keep-alive-p "HTTP/1.1" '(("connection" "close")))))
  (assert-true
   "close default for 1.0"
   (not (ws-connection-keep-alive-p "HTTP/1.0" '())))
  (assert-true
   "keep-alive opt-in for 1.0"
   (ws-connection-keep-alive-p "HTTP/1.0" '(("connection" "keep-alive"))))

  (format t "t2-http-unit: ok~%"))

(run-tests)
