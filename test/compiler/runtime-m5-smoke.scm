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
 42
 (run-forms '((block done (return-from done 42) 0)) 'strict)
 "block-return-from")

(assert-equal
 99
 (run-forms '((catch 'k (throw 'k 99) 0)) 'strict)
 "catch-throw")

(assert-runtime-error
 (lambda () (run-forms '((throw 'no-target 1)) 'strict))
 'control-error
 "throw-without-catch")

(assert-equal
 7
 (run-forms '((block b (catch 'k (return-from b 7) 0) 1)) 'strict)
 "nested-nonlocal")

(assert-equal
 3
 (run-forms
  '((defglobal m5-x 0)
    (tagbody
     start
     (setq m5-x (+ m5-x 1))
     (if (< m5-x 3) (go start) 'done))
    m5-x)
  'strict)
 "tagbody-go-loop")

(display "runtime M5 smoke passed\n")
