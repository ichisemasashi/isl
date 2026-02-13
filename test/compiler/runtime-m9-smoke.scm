(add-load-path "./src")
(use isl.compiler.runner)

(define (assert-equal label expected actual)
  (unless (equal? expected actual)
    (error "assertion failed" label expected actual)))

(define (run forms)
  (run-forms forms 'strict))

(assert-equal
 'm9-1
10
 (run
  '((defpackage :m9.p1 (:use :islisp) (:export foo))
    (in-package :m9.p1)
    (defun foo () 10)
    (in-package :islisp)
    (m9.p1:foo))))

(assert-equal
 'm9-2
10
 (run
  '((defpackage :m9.p1b (:use :islisp) (:export foo))
    (in-package :m9.p1b)
    (defun foo () 10)
    (defpackage :m9.p2b (:use :islisp))
    (in-package :m9.p2b)
    (use-package :m9.p1b)
    (foo))))

(assert-equal
 'm9-3
 #t
 (run
  '((defpackage :m9.p1c (:use :islisp))
    (in-package :islisp)
    (if (find-package :m9.p1c) #t #f))))

(assert-equal
 'm9-4
 'ok
 (run
  '((defpackage :m9.p1d (:use :islisp))
    (in-package :m9.p1d)
    (intern "BAR")
    (in-package :islisp)
    (handler-case (m9.p1d:bar) (error (c) 'ok)))))

(assert-equal
 'm9-5
 'bar
 (run
  '((defpackage :m9.p1e (:use :islisp))
    (in-package :m9.p1e)
    (defun bar () 'bar)
    (export 'bar)
    (in-package :islisp)
    (m9.p1e:bar))))

(display "runtime M9 smoke passed\n")
