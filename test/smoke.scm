(add-load-path "./src")
(use isl.core)

(define env (make-initial-env))

(define (check expected form)
  (let ((actual (eval-islisp form env)))
    (unless (equal? expected actual)
      (error "assertion failed" form expected actual))))

(check 3 '(+ 1 2))
(check 120 '(progn
              (defun fact (n)
                (if (= n 0)
                    1
                    (* n (fact (- n 1)))))
              (fact 5)))
(check '(1 2 3) '(list 1 2 3))
(check #t '(symbolp 'abc))
(check 'ok '(progn (defglobal g 10)
                   (setq g 20)
                   'ok))

(display "smoke tests passed\n")
