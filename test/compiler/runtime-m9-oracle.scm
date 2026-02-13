(add-load-path "./src")
(use isl.core)
(use isl.compiler.runner)

(define ienv (make-initial-env 'strict))

(define (eval-interpreter forms)
  (guard (e (else (list 'error e)))
    (list 'ok (eval-islisp (cons 'progn forms) ienv))))

(define (eval-runtime forms)
  (guard (e (else (list 'error e)))
    (list 'ok (run-forms forms 'strict))))

(define (same-result? a b)
  (cond
   ((and (eq? (car a) 'ok) (eq? (car b) 'ok))
    (equal? (cadr a) (cadr b)))
   ((and (eq? (car a) 'error) (eq? (car b) 'error))
    #t)
   (else #f)))

(define (check-case forms)
  (let ((ia (eval-interpreter forms))
        (rb (eval-runtime forms)))
    (unless (same-result? ia rb)
      (error "mismatch" forms ia rb))))

(for-each
 check-case
 (list
  '((defpackage :m9.p1 (:use :islisp) (:export foo))
    (in-package :m9.p1)
    (defun foo () 10)
    (in-package :islisp)
    (m9.p1:foo))
  '((defpackage :m9.p1b (:use :islisp) (:export foo))
    (in-package :m9.p1b)
    (defun foo () 10)
    (defpackage :m9.p2b (:use :islisp))
    (in-package :m9.p2b)
    (use-package :m9.p1b)
    (foo))
  '((defpackage :m9.p1c (:use :islisp))
    (in-package :islisp)
    (if (find-package :m9.p1c) #t #f))
  '((defpackage :m9.p1d (:use :islisp))
    (in-package :m9.p1d)
    (intern "BAR")
    (in-package :islisp)
    (m9.p1d:bar))
  '((defpackage :m9.p1e (:use :islisp))
    (in-package :m9.p1e)
    (defun bar () 'bar)
    (export 'bar)
    (in-package :islisp)
    (m9.p1e:bar))))

(display "runtime M9 oracle passed\n")
