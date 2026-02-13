(add-load-path "./src")
(use isl.compiler.jit)

(define st (make-jit-state 'strict))

(define (assert-equal label expected actual)
  (unless (equal? expected actual)
    (error "assertion failed" label expected actual)))

(define (assert-error label thunk)
  (let ((raised #f))
    (guard (e (else (set! raised #t)))
      (thunk))
    (unless raised
      (error "assertion failed (expected error)" label))))

(assert-equal
 'm11-1
 55
 (jit-eval-forms st
                 '((defun m11-fib (n)
                     (if (< n 2) n (+ (m11-fib (- n 1)) (m11-fib (- n 2)))))
                   (m11-fib 10))))

(assert-equal
 'm11-2
 '(2 3 3)
 (jit-eval-forms st
                 '((defglobal m11-x 1)
                   (defun m11-bump () (setq m11-x (+ m11-x 1)))
                   (list (m11-bump) (m11-bump) m11-x))))

(assert-equal
 'm11-3
 10
 (jit-eval-forms st
                 '((defmacro m11-m (x) (list '+ x 1))
                   (m11-m 9))))

(assert-equal
 'm11-4
 8
 (jit-eval-forms st
                 '((block b (catch 'k (throw 'k (return-from b 8)))))))

(assert-error
 'm11-5
 (lambda ()
   (jit-eval-forms st
                   '((defun m11-err () (throw 'none 1))
                     (m11-err)))))

(display "jit M11 smoke passed\n")
