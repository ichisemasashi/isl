(add-load-path "./src")
(use isl.compiler.runner)

(define (assert-equal label expected actual)
  (unless (equal? expected actual)
    (error "assertion failed" label expected actual)))

(define (run forms)
  (run-forms forms 'extended))

(assert-equal 'tls-1 #f (run '((tls-listener-p 1))))
(assert-equal 'tls-2 #f (run '((tls-connection-p 1))))
(assert-equal 'tls-3 'ok
              (run '((handler-case (tls-listen 0 "missing.crt" "missing.key")
                       (error (c) 'ok)))))
(assert-equal 'tls-4 'ok
              (run '((handler-case (tls-accept 1)
                       (error (c) 'ok)))))
(assert-equal 'tls-5 'ok
              (run '((handler-case (tls-send 1 "x")
                       (error (c) 'ok)))))
(assert-equal 'tls-6 'ok
              (run '((handler-case (tls-close 1)
                       (error (c) 'ok)))))

(assert-equal 'tcp-1 #f (run '((tcp-listener-p 1))))
(assert-equal 'tcp-2 #f (run '((tcp-connection-p 1))))
(assert-equal 'tcp-3 'ok
              (run '((handler-case (tcp-listen 0)
                       (error (c) 'ok)))))
(assert-equal 'tcp-4 'ok
              (run '((handler-case (tcp-accept 1)
                       (error (c) 'ok)))))
(assert-equal 'tcp-5 'ok
              (run '((handler-case (tcp-send 1 "x")
                       (error (c) 'ok)))))
(assert-equal 'tcp-6 'ok
              (run '((handler-case (tcp-close 1)
                       (error (c) 'ok)))))

(display "runtime TLS/TCP smoke passed\n")
