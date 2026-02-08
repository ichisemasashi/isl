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
(check 7 '(progn (defver v 7) v))
(check 1 '(progn (defvar dv 1) (defvar dv 99) dv))
(check 5 '(let ((x 2) (y 3)) (+ x y)))
(check 3 '(let* ((x 1) (y (+ x 2))) y))
(check 42 '(progn (defver s 0) (setf s 42) s))
(check '(9 . 2) '(progn (defver p (cons 1 2)) (setf (car p) 9) p))
(check '(9 . 8) '(progn (defver p2 (cons 9 2)) (setf (cdr p2) 8) p2))

(display "smoke tests passed\n")
