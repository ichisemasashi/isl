(add-load-path "./src")
(use isl.compiler.runner)

(define (assert-equal expected actual label)
  (unless (equal? expected actual)
    (error "assertion failed" label expected actual)))

(assert-equal
 'caught
 (run-forms '((handler-case (/ 1 0) (error (c) 'caught))) 'strict)
 "handler-case-catch-div0")

(assert-equal
 3
 (run-forms '((handler-case (+ 1 2) (error (c) 'caught))) 'strict)
 "handler-case-no-error")

(assert-equal
 'ok
 (run-forms '((handler-case (progn (car 1) 'ng) (error (c) 'ok))) 'strict)
 "handler-case-catch-type")

(assert-equal
 'outer
 (run-forms '((handler-case (handler-case (car 1) (error (c) (throw 'm6 5)))
                             (error (c) 'outer)))
            'strict)
 "handler-case-nested")

(assert-equal
 'via-handler
 (run-forms '((catch 'm6
                (handler-case (car 1)
                  (error (c) (throw 'm6 'via-handler)))))
            'strict)
 "handler-case-throw-to-catch")

(display "runtime M6 smoke passed\n")
