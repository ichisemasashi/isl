(add-load-path "./src")
(use isl.compiler.runner)
(use isl.compiler.runtime)

(define (assert-equal expected actual label)
  (unless (equal? expected actual)
    (error "assertion failed" label expected actual)))

(define (assert-runtime-error thunk expected-code label)
  (guard (e
          ((runtime-error? e)
           (unless (eq? (runtime-error-code e) expected-code)
             (error "unexpected runtime-error code" label expected-code (runtime-error-code e)))
           #t)
          (else
           (error "expected runtime-error" label e)))
    (thunk)
    (error "expected runtime-error but got value" label)))

(assert-equal
 15
 (run-forms '(((lambda (x)
                 ((lambda (f) (funcall f 5))
                  (lambda (y) (+ x y))))
               10)))
 "closure-capture")

(assert-equal
 '(1 (2 3 4))
 (run-forms '(((lambda (a &rest r)
                 (list a r))
               1 2 3 4)))
 "rest-params")

(assert-equal
 1
 (run-forms '(((lambda (x)
                 (progn
                   ((lambda (x) x) 9)
                   x))
               1)))
 "lexical-shadow")

(assert-equal
 7
 (run-forms '((defun add2 (x) (+ x 2))
              (add2 5)))
 "top-define-fun")

(assert-runtime-error
 (lambda ()
   (run-forms '(((lambda (x y) x) 1))))
 'arity
 "arity-mismatch")

(display "runtime M2 smoke passed\n")
