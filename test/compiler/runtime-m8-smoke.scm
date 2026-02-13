(add-load-path "./src")
(use isl.compiler.runner)

(define (assert-equal label expected actual)
  (unless (equal? expected actual)
    (error "assertion failed" label expected actual)))

(define (run forms)
  (run-forms forms 'strict))

(assert-equal 'm8-1 #t (run '((zerop 0))))
(assert-equal 'm8-2 '(#t #t #t #t) (run '((list (plusp 1) (minusp -1) (oddp 3) (evenp 4)))))
(assert-equal 'm8-3 '(7 7 7 7) (run '((list (floor 7) (ceiling 7) (truncate 7) (round 7)))))
(assert-equal 'm8-4 9007199254740992 (run '((+ 9007199254740991 1))))
(assert-equal 'm8-5 'ok (run '((handler-case (< "a" 1) (error (c) 'ok)))))
(assert-equal 'm8-6 'ok (run '((handler-case (oddp 3.5) (error (c) 'ok)))))

(display "runtime M8 smoke passed\n")
