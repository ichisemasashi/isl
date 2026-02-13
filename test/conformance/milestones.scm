;; case format: (kind form expected)
;; kind: value | error | oracle

(define m0-cases
  (list
   (list 'value '(+ 1 2) 3)
   (list 'value '((lambda (x) (+ x 1)) 9) 10)
   (list 'value '(if #f 1 2) 2)
   (list 'error '(car 1) 'error)
   (list 'value '(progn (defglobal m0-x 7) m0-x) 7)))

(define m1-cases
  (list
   (list 'value '(progn (defmacro m1-id (x) x) (macroexpand-1 '(m1-id (+ 1 2)))) '(+ 1 2))
   (list 'value '(progn (defmacro m1-inc (x) (list '+ x 1)) (m1-inc 4)) 5)
   (list 'value '(macroexpand '(when #t 1 2)) '(when #t 1 2))
   (list 'value '(progn (defmacro m1-nest (x) (list 'if x 1 0)) (macroexpand '(m1-nest #t))) '(if #t 1 0))
   (list 'value '(progn (defmacro m1-q (x) (list 'quote x)) (m1-q abc)) 'abc)))

(define m2-cases
  (list
   (list 'value '((lambda (x) ((lambda (f) (funcall f 5)) (lambda (y) (+ x y)))) 10) 15)
   (list 'value '(progn (defun m2-rest (a &rest r) (list a r)) (m2-rest 1 2 3 4)) '(1 (2 3 4)))
   (list 'value '(let ((x 1)) (let ((x 2)) x)) 2)
   (list 'value '(let ((x 1)) ((lambda (x) x) 9) x) 1)
   (list 'error '((lambda (x y) x) 1) 'error)))

(define m3-cases
  (list
   (list 'value '(progn (defun m3-f (x) (+ x 3)) (m3-f 4)) 7)
   (list 'value '(progn (defun m3-if (x) (if x 1 2)) (m3-if #f)) 2)
   (list 'value '(progn (defun m3-let (x) (let ((y 5)) (+ x y))) (m3-let 7)) 12)
   (list 'value '(progn (defun m3-rec (n) (if (= n 0) 1 (* n (m3-rec (- n 1))))) (m3-rec 5)) 120)
   (list 'error '(progn (defun m3-bad (x) (+ x 1)) (m3-bad "a")) 'error)))

(define m4-cases
  (list
   (list 'value
         '(progn
            (defglobal m4-log '())
            (defun m4-push (x) (setq m4-log (append m4-log (list x))) x)
            (+ (m4-push 1) (m4-push 2))
            m4-log)
         '(1 2))
   (list 'value
         '(progn
            (defglobal m4-x 0)
            (defun m4-a () (setq m4-x (+ m4-x 1)) m4-x)
            (defun m4-b () (setq m4-x (+ m4-x 10)) m4-x)
            (list (m4-a) (m4-b) m4-x))
         '(1 11 11))
   (list 'value
         '(progn
            (defglobal m4-v (vector 0 0))
            (defglobal m4-i 0)
            (defun m4-idx () (setq m4-i (+ m4-i 1)) 0)
            (setf (vector-ref m4-v (m4-idx)) 9)
            (list m4-i (vector-ref m4-v 0)))
         '(1 9))
   (list 'value
         '(progn
            (defglobal m4-log2 '())
            (defun m4-mark (x) (setq m4-log2 (append m4-log2 (list x))) x)
            (if (m4-mark #f) (m4-mark 'then) (m4-mark 'else))
            m4-log2)
         '(#f else))
   (list 'value
         '(progn
            (defglobal m4-c 0)
            (let ((x (setq m4-c (+ m4-c 1)))
                  (y (setq m4-c (+ m4-c 10))))
              m4-c))
         11)))

(define m5-cases
  (list
   (list 'value '(block done (return-from done 42) 0) 42)
   (list 'value '(catch 'k (throw 'k 99) 0) 99)
   (list 'error '(throw 'no-target 1) 'error)
   (list 'value '(block b (catch 'k (return-from b 7) 0) 1) 7)
   (list 'value
         '(progn
            (defglobal m5-x 0)
            (tagbody
             start
             (setq m5-x (+ m5-x 1))
             (if (< m5-x 3) (go start) 'done))
            m5-x)
         3)))

(define m6-cases
  (list
   (list 'value '(handler-case (/ 1 0) (error (c) 'caught)) 'caught)
   (list 'value '(handler-case (+ 1 2) (error (c) 'caught)) 3)
   (list 'value '(handler-case (progn (car 1) 'ng) (error (c) 'ok)) 'ok)
   (list 'value '(handler-case (handler-case (car 1) (error (c) (throw 'm6 5))) (error (c) 'outer)) 'outer)
   (list 'value '(catch 'm6 (handler-case (car 1) (error (c) (throw 'm6 'via-handler)))) 'via-handler)))

(define m7-cases
  (list
   (list 'value
         '(progn
            (defclass m7-a () ((v :initarg :v :reader m7-v)))
            (defglobal m7-o (make-instance 'm7-a :v 9))
            (m7-v m7-o))
         9)
   (list 'value
         '(progn
            (defclass m7-a2 () ((v :initarg :v :reader m7-v2)))
            (defglobal m7-o2 (make-instance 'm7-a2 :v 8))
            (defgeneric m7-g2 (x))
            (defmethod m7-g2 ((x m7-a2)) 'a)
            (m7-g2 m7-o2))
         'a)
   (list 'value
         '(progn
            (defclass m7-a3 () ((v :initarg :v :reader m7-v3)))
            (defclass m7-b3 (m7-a3) ())
            (defglobal m7-ob3 (make-instance 'm7-b3 :v 3))
            (defgeneric m7-g3 (x))
            (defmethod m7-g3 ((x m7-a3)) 'a)
            (defmethod m7-g3 ((x m7-b3)) 'b)
            (m7-g3 m7-ob3))
         'b)
   (list 'value
         '(progn
            (defclass m7-a4 () ((v :initarg :v)))
            (defglobal m7-o4 (make-instance 'm7-a4 :v 9))
            (slot-value m7-o4 'v))
         9)
   (list 'value
         '(progn
            (defclass m7-a5 () ())
            (defclass m7-b5 (m7-a5) ())
            (defglobal m7-ob5 (make-instance 'm7-b5))
            (defgeneric m7-h5 (x y))
            (defmethod m7-h5 ((x m7-a5) y) 'left)
            (m7-h5 m7-ob5 1))
         'left)))

(define m8-cases
  (list
   (list 'value '(zerop 0) #t)
   (list 'value '(list (plusp 1) (minusp -1) (oddp 3) (evenp 4)) '(#t #t #t #t))
   (list 'oracle '(list (floor 7) (ceiling 7) (truncate 7) (round 7)) 'oracle)
   (list 'oracle '(+ 9007199254740991 1) 'oracle)
   (list 'error '(< "a" 1) 'error)))

(define m9-cases
  (list
   (list 'value
         '(progn
            (defpackage :m9.p1 (:use :islisp) (:export foo))
            (in-package :m9.p1)
            (defun foo () 10)
            (in-package :islisp)
            (m9.p1:foo))
         10)
   (list 'value
         '(progn
            (defpackage :m9.p1b (:use :islisp) (:export foo))
            (in-package :m9.p1b)
            (defun foo () 10)
            (defpackage :m9.p2b (:use :islisp))
            (in-package :m9.p2b)
            (use-package :m9.p1b)
            (foo))
         10)
   (list 'value
         '(progn
            (defpackage :m9.p1c (:use :islisp))
            (in-package :islisp)
            (if (find-package :m9.p1c) #t #f))
         #t)
   (list 'error
         '(progn
            (defpackage :m9.p1d (:use :islisp))
            (in-package :m9.p1d)
            (intern "BAR")
            (in-package :islisp)
            (m9.p1d:bar))
         'error)
   (list 'value
         '(progn
            (defpackage :m9.p1e (:use :islisp))
            (in-package :m9.p1e)
            (defun bar () 'bar)
            (export 'bar)
            (in-package :islisp)
            (m9.p1e:bar))
         'bar)))

(define m10-cases
  (list
   (list 'value
         '(progn
            (defun m10-sum (n acc)
              (if (= n 0) acc (m10-sum (- n 1) (+ acc n))))
            (m10-sum 100 0))
         5050)
   (list 'value
         '(progn
            (defglobal m10-c 0)
            (defun m10-f () (setq m10-c (+ m10-c 1)) m10-c)
            (+ (m10-f) (m10-f)))
         3)
   (list 'value
         '(progn
            (defun m10-g (x) (if x 1 (car 1)))
            (m10-g #t))
         1)
   (list 'value
         '(progn
            (defun m10-h (x) (* (+ x 1) (- x 1)))
            (m10-h 9))
         80)
   (list 'error '(progn (defun m10-e () (car 1)) (m10-e)) 'error)))

(define m11-cases
  (list
   (list 'value
         '(progn
            (defun m11-fib (n)
              (if (< n 2) n (+ (m11-fib (- n 1)) (m11-fib (- n 2)))))
            (m11-fib 10))
         55)
   (list 'value
         '(progn
            (defglobal m11-x 1)
            (defun m11-bump () (setq m11-x (+ m11-x 1)))
            (list (m11-bump) (m11-bump) m11-x))
         '(2 3 3))
   (list 'value
         '(progn
            (defmacro m11-m (x) (list '+ x 1))
            (m11-m 9))
         10)
   (list 'value '(block b (catch 'k (throw 'k (return-from b 8)))) 8)
   (list 'error '(progn (defun m11-err () (throw 'none 1)) (m11-err)) 'error)))

(define all-milestones
  (list
   (list "M0" m0-cases)
   (list "M1" m1-cases)
   (list "M2" m2-cases)
   (list "M3" m3-cases)
   (list "M4" m4-cases)
   (list "M5" m5-cases)
   (list "M6" m6-cases)
   (list "M7" m7-cases)
   (list "M8" m8-cases)
   (list "M9" m9-cases)
   (list "M10" m10-cases)
   (list "M11" m11-cases)))

(define strict-milestones
  (list
   (list "M0" m0-cases)
   (list "M1" m1-cases)
   (list "M2" m2-cases)
   (list "M3" m3-cases)
   (list "M4" m4-cases)
   (list "M5" m5-cases)
   (list "M6" m6-cases)
   (list "M7" m7-cases)
   (list "M8" m8-cases)
   (list "M10" m10-cases)
   (list "M11" m11-cases)))

(define extended-milestones all-milestones)
