(add-load-path "./src")
(use isl.compiler.runner)

(define (assert-equal label expected actual)
  (unless (equal? expected actual)
    (error "assertion failed" label expected actual)))

(define (run forms)
  (run-forms forms 'extended))

(assert-equal 'rt-thread-1 #f (run '((thread-p 1))))
(assert-equal 'rt-thread-2 #f (run '((mutex-p 1))))
(assert-equal 'rt-thread-3 #t
              (run '((let ((m (mutex-open)))
                       (mutex-p m)))))
(assert-equal 'rt-thread-4 #t
              (run '((let ((m (mutex-open)))
                       (mutex-lock m)
                       (mutex-unlock m)))))
(assert-equal 'rt-thread-5 #t
              (run '((let ((th (thread-spawn (lambda () 9))))
                       (thread-p th)))))
(assert-equal 'rt-thread-6 9
              (run '((let ((th (thread-spawn (lambda () 9))))
                       (thread-join th)))))

(display "runtime thread smoke passed\n")
