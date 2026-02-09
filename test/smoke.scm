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
(check 7 '(progn (defglobal v 7) v))
(check 1 '(progn (defvar dv 1) (defvar dv 99) dv))
(check 5 '(let ((x 2) (y 3)) (+ x y)))
(check 3 '(let* ((x 1) (y (+ x 2))) y))
(check 42 '(progn (defglobal s 0) (setf s 42) s))
(check '(9 . 2) '(progn (defglobal p (cons 1 2)) (setf (car p) 9) p))
(check '(9 . 8) '(progn (defglobal p2 (cons 9 2)) (setf (cdr p2) 8) p2))
(check 10 '(progn
             (defglobal wi 0)
             (defglobal wsum 0)
             (while (< wi 5)
               (setq wsum (+ wsum wi))
               (setq wi (+ wi 1)))
             wsum))
(check 15 '(progn
             (defglobal lsum 0)
             (loop for i from 1 to 5
                   do (setq lsum (+ lsum i)))
             lsum))
(check 6 '(progn
            (defglobal lsum2 0)
            (loop for i from 1 to 5
                  until (> i 3)
                  do (setq lsum2 (+ lsum2 i)))
            lsum2))
(check 120 '(do ((n 5 (- n 1))
                 (acc 1 (* acc n)))
                ((= n 0) acc)))
(check 6 '(progn
            (defglobal dlsum 0)
            (dolist (x '(1 2 3) dlsum)
              (setq dlsum (+ dlsum x)))))
(check 10 '(progn
             (defglobal dtsum 0)
             (dotimes (i 5 dtsum)
               (setq dtsum (+ dtsum i)))))
(check 2 '(if nil 1 2))
(check 'ok '(cond
             ((= 1 2) 'ng)
             ((= 2 2) 'ok)
             (t 'ng2)))
(check 'fallback '(cond
                   (nil 'ng)
                   (t 'fallback)))
(check 'two '(case 2
               ((1) 'one)
               ((2 3) 'two)
               (otherwise 'other)))
(check 'other '(case 9
                 ((1 2 3) 'small)
                 (otherwise 'other)))
(check #t '(not nil))
(check #f '(not t))
(check "Hello, ONE!" '(format nil "Hello, ~A!" "ONE"))
(check "(1 2)" '(format nil "~S" '(1 2)))
(check "A\nB~C" '(format nil "A~%B~~C"))
(check '() '(format t "visible: ~A~%" "ok"))
(check #t '(string= "abc" "abc"))
(check #f '(string= "abc" "abd"))
(check "abcdef" '(string-concat "ab" "cd" "ef"))
(check "bc" '(substring "abcd" 1 3))
(check "cd" '(substring "abcd" 2))
(check 4 '(length "abcd"))
(check 3 '(length '(1 2 3)))

(display "smoke tests passed\n")
