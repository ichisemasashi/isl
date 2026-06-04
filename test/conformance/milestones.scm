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
   (list 'value '(macroexpand '(when #t 1 2)) '(if #t (progn 1 2) ()))
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

;; ----------------------------------------------------------------
;; Phase 1: 算術・型述語・超越関数
;; ----------------------------------------------------------------
(define phase1-arith-cases
  (list
   ;; 1-A: 算術関数
   (list 'value '(abs -5)       5)
   (list 'value '(abs 3)        3)
   (list 'value '(abs 0)        0)
   (list 'value '(max 1 2 3)    3)
   (list 'value '(max -1 -2)    -1)
   (list 'value '(min 1 2 3)    1)
   (list 'value '(min -1 -2)    -2)
   (list 'value '(expt 2 10)    1024)
   (list 'value '(expt 3 0)     1)
   (list 'value '(sqrt 9.0)     3.0)
   (list 'value '(sqrt 4)       2)
   (list 'value '(rem 7 3)      1)
   (list 'value '(rem -7 3)     -1)
   (list 'value '(rem 7 -3)     1)
   (list 'value '(gcd 12 8)     4)
   (list 'value '(gcd 0 5)      5)
   (list 'value '(gcd)          0)
   (list 'value '(lcm 4 6)      12)
   (list 'value '(lcm)          1)
   (list 'value '(signum 5)     1)
   (list 'value '(signum -3)    -1)
   (list 'value '(signum 0)     0)
   (list 'value '(negate 7)     -7)
   (list 'value '(negate -4)    4)
   (list 'value '(float 3)      3.0)
   (list 'value '(isqrt 9)      3)
   (list 'value '(isqrt 10)     3)
   (list 'value '(isqrt 0)      0)
   ;; 1-B: 数値変換
   (list 'value '(number->string 255 16)   "ff")
   (list 'value '(number->string 8 2)      "1000")
   (list 'value '(number->string 42)       "42")
   (list 'value '(string->number "ff" 16)  255)
   (list 'value '(string->number "1000" 2) 8)
   (list 'value '(string->number "42")     42)
   (list 'value '(string->number "xyz")    '())   ; 変換失敗 → nil
   ;; 1-C: 型述語 (ISLISP predicates return t / nil)
   (list 'value '(consp (cons 1 2))          #t)
   (list 'value '(consp '())                 '())
   (list 'value '(consp 3)                   '())
   (list 'value '(characterp #\a)            #t)
   (list 'value '(characterp "a")            '())
   (list 'value '(integerp 3)                #t)
   (list 'value '(integerp 3.0)              '())
   (list 'value '(floatp 1.5)                #t)
   (list 'value '(floatp 1)                  '())
   (list 'value '(functionp (lambda (x) x))  #t)
   (list 'value '(functionp 42)              '())
   (list 'value '(general-vector-p (vector 1 2)) #t)
   (list 'value '(general-vector-p '(1 2))       '())
   ;; ISLISP: general-array*-p is true only for arrays of rank /= 1, so a
   ;; rank-1 general vector is NOT a general-array* (was incorrectly #t before).
   (list 'value '(general-array*-p (vector 1 2)) '())
   (list 'value '(general-array*-p (create-array '(2 2) 0)) #t)
   ;; 1-D: 三角・超越関数 (結果は inexact float)
   (list 'value '(exp 0)    1.0)
   (list 'value '(log 1)    0.0)
   (list 'value '(sin 0)    0.0)
   (list 'value '(cos 0)    1.0)
   (list 'value '(tan 0)    0.0)
   (list 'value '(asin 0)   0.0)
   (list 'value '(acos 1)   0.0)
   (list 'value '(atan 0)   0.0)
   (list 'value '(atan 1 1) (atan 1 1))   ; Gauche で事前計算した値と一致するか確認
   ;; 1-E: floor/ceiling/truncate/round の 2 引数版、quotient
   (list 'value '(floor 7 2)      3)
   (list 'value '(floor -7 2)     -4)
   (list 'value '(ceiling 7 2)    4)
   (list 'value '(ceiling -7 2)   -3)
   (list 'value '(truncate 7 2)   3)
   (list 'value '(truncate -7 2)  -3)
   (list 'value '(round 7 2)      4)
   (list 'value '(round 5 2)      2)   ; 銀行丸め: 偶数方向
   (list 'value '(quotient 7 3)   2)
   (list 'value '(quotient -7 3)  -2)
   ;; エラーケース
   (list 'error '(abs "x")            'error)
   (list 'error '(max)                'error)
   (list 'error '(expt "a" 2)         'error)
   (list 'error '(rem 1.5 2)          'error)
   (list 'error '(quotient 1.5 2)     'error)
   (list 'error '(isqrt -1)           'error)
   (list 'error '(number->string "x") 'error)
   (list 'error '(string->number 42)  'error)))

;; ----------------------------------------------------------------
;; Phase 2: リスト・文字列・文字・ベクター・配列
;; ----------------------------------------------------------------
(define phase2-list-cases
  (list
   ;; reverse / nreverse
   (list 'value '(reverse '(1 2 3))           '(3 2 1))
   (list 'value '(reverse '())                '())
   (list 'value '(nreverse (list 1 2 3))      '(3 2 1))
   ;; map / for-each
   (list 'value '(map (lambda (x) (* x 2)) '(1 2 3))         '(2 4 6))
   (list 'value '(map (lambda (x y) (+ x y)) '(1 2) '(10 20)) '(11 22))
   (list 'value '(progn (defglobal p2-log '())
                        (for-each (lambda (x) (setq p2-log (cons x p2-log))) '(1 2 3))
                        p2-log)
         '(3 2 1))
   ;; mapc
   (list 'value '(progn (defglobal p2-acc '())
                        (mapc (lambda (x) (setq p2-acc (cons x p2-acc))) '(a b c))
                        p2-acc)
         '(c b a))
   ;; mapcan
   (list 'value '(mapcan (lambda (x) (list x x)) '(1 2 3))  '(1 1 2 2 3 3))
   ;; maplist
   (list 'value '(maplist (lambda (x) (car x)) '(1 2 3))    '(1 2 3))
   ;; member / assoc / remove
   (list 'value '(member 2 '(1 2 3))          '(2 3))
   (list 'value '(member 9 '(1 2 3))          '())
   (list 'value '(assoc 'b '((a 1)(b 2)(c 3))) '(b 2))
   (list 'value '(assoc 'z '((a 1)))           '())
   (list 'value '(remove 2 '(1 2 3 2))        '(1 3))
   (list 'value '(remove 9 '(1 2 3))          '(1 2 3))
   ;; last-pair / nconc
   (list 'value '(last-pair '(1 2 3))         '(3))
   (list 'value '(nconc (list 1 2) (list 3 4)) '(1 2 3 4))
   (list 'value '(nconc '() '(1 2))           '(1 2))
   ;; list-ref / list-tail
   (list 'value '(list-ref '(a b c) 1)        'b)
   (list 'value '(list-tail '(a b c) 2)       '(c))
   ;; create-list / list-copy / nthcdr
   (list 'value '(create-list 3 0)            '(0 0 0))
   (list 'value '(create-list 0)              '())
   (list 'value '(list-copy '(1 2 3))         '(1 2 3))
   (list 'value '(nthcdr 2 '(a b c d))        '(c d))
   ;; sort
   (list 'value '(sort '(3 1 4 1 5 2) (lambda (a b) (< a b)))  '(1 1 2 3 4 5))
   ;; エラーケース
   (list 'error '(reverse 42)                 'error)
   (list 'error '(map (lambda (x) x))         'error)
   (list 'error '(list-ref '(a b) 5)          'error)
   (list 'error '(last-pair '())              'error)))

(define phase2-string-cases
  (list
   (list 'value '(string-length "hello")      5)
   (list 'value '(string-length "")           0)
   (list 'value '(string-ref "hello" 1)       #\e)
   (list 'value '(string-copy "abc")          "abc")
   (list 'value '(string-upcase "hello")      "HELLO")
   (list 'value '(string-downcase "HELLO")    "hello")
   (list 'error '(string-length 42)           'error)
   (list 'error '(string-ref "abc" 5)         'error)))

(define phase2-char-cases
  (list
   (list 'value '(char= #\a #\a)              #t)
   (list 'value '(char= #\a #\b)              '())
   (list 'value '(char/= #\a #\b)             #t)
   (list 'value '(char/= #\a #\a)             '())
   (list 'value '(char< #\a #\b)              #t)
   (list 'value '(char< #\b #\a)              '())
   (list 'value '(char> #\b #\a)              #t)
   (list 'value '(char<= #\a #\a)             #t)
   (list 'value '(char>= #\b #\a)             #t)
   (list 'value '(char-upcase #\a)            #\A)
   (list 'value '(char-downcase #\A)          #\a)
   (list 'error '(char= #\a "a")              'error)))

(define phase2-vector-cases
  (list
   ;; create-vector / general-vector-ref
   (list 'value '(general-vector-ref (vector 10 20 30) 1)          20)
   (list 'value '(general-vector-ref (create-vector 3 99) 0)       99)
   (list 'value '(length (create-vector 5))                         5)
   ;; create-array / array-dimensions
   (list 'value '(array-dimensions (vector 1 2 3))                 '(3))
   (list 'value '(array-dimensions (create-array 4 0))             '(4))
   (list 'error '(general-vector-ref (vector 1) 5)                 'error)
   (list 'error '(array-dimensions '(1 2))                         'error)))

;; ---- Phase 3 test cases ----
(define phase3-flet-cases
  (list
   ;; 3-A: flet
   (list 'value '(flet ((f (x) (+ x 1))) (f 9))                          10)
   (list 'value '(flet ((sq (x) (* x x))) (sq 7))                        49)
   (list 'value '(flet ((greet (name) name)) (greet 'hello))             'hello)
   ;; flet is not recursive
   (list 'value '(let ((x 10)) (flet ((f (y) (+ x y))) (f 5)))           15)
   ;; 3-A: labels (mutual recursion)
   (list 'value '(labels ((fact (n) (if (= n 0) 1 (* n (fact (- n 1))))))
                   (fact 5))
         120)
   (list 'value '(labels ((even? (n) (if (= n 0) t (odd? (- n 1))))
                           (odd?  (n) (if (= n 0) nil (even? (- n 1)))))
                   (even? 4))
         #t)
   (list 'value '(labels ((even? (n) (if (= n 0) t (odd? (- n 1))))
                           (odd?  (n) (if (= n 0) nil (even? (- n 1)))))
                   (odd? 3))
         #t)))

(define phase3-the-cases
  (list
   ;; 3-B: the
   (list 'value '(the <integer> 42)               42)
   (list 'value '(the <string>  "hello")          "hello")
   (list 'value '(the <number>  3.14)             3.14)
   (list 'error '(the <integer> "not-int")        'error)
   (list 'error '(the <string>  99)               'error)))

(define phase3-unwind-cases
  (list
   ;; 3-C: unwind-protect
   (list 'value '(unwind-protect 42 nil)          42)
   (list 'value '(unwind-protect (+ 1 2) nil)     3)
   ;; cleanup runs but return value comes from protected form
   (list 'value '(let ((x 0))
                   (unwind-protect
                     (progn (setq x 1) x)
                     (setq x 99)))
         1)))

(define phase3-function-cases
  (list
   ;; 3-F: function special form
   (list 'value '(functionp (function +))         #t)
   (list 'value '(functionp (function car))       #t)
   (list 'value '(functionp (function (lambda (x) x))) #t)))

(define phase3-defconstant-cases
  (list
   ;; 3-E: defconstant
   (list 'value '(progn (defconstant +pi+ 3) +pi+)   3)
   (list 'value '(progn (defconstant +answer+ 42) +answer+) 42)))

(define phase3-io-macro-cases
  (list
   ;; 3-D: with-open-input-file / with-open-output-file
   ;; Write a string and read it back using line-based I/O
   (list 'value
         '(progn
            (with-open-output-file (s "/tmp/isl-p3-test.txt")
              (write-line "hello-phase3" s))
            (with-open-input-file (s "/tmp/isl-p3-test.txt")
              (read-line s)))
         "hello-phase3")))

;; ---- Phase 4: I/O streams ----

(define phase4-char-io-cases
  (list
   ;; 4-A: write-char / read-char round-trip
   (list 'value
         '(progn
            (with-open-output-file (s "/tmp/isl-p4-ms-chars.txt")
              (write-char #\H s)
              (write-char #\i s))
            (with-open-input-file (s "/tmp/isl-p4-ms-chars.txt")
              (list (read-char s) (read-char s))))
         '(#\H #\i))
   ;; peek-char does not consume
   (list 'value
         '(progn
            (with-open-output-file (s "/tmp/isl-p4-ms-peek.txt")
              (write-char #\Z s))
            (with-open-input-file (s "/tmp/isl-p4-ms-peek.txt")
              (let* ((p (peek-char s))
                     (r (read-char s)))
                (list p r))))
         '(#\Z #\Z))
   ;; peek-char at EOF returns nil
   (list 'value
         '(progn
            (with-open-output-file (s "/tmp/isl-p4-ms-eof.txt") nil)
            (with-open-input-file (s "/tmp/isl-p4-ms-eof.txt")
              (peek-char s)))
         '())
   ;; write-char returns the character
   (list 'value
         '(with-open-output-file (s "/tmp/isl-p4-ms-wc-ret.txt")
            (write-char #\X s))
         #\X)
   ;; read-char raises error at EOF
   (list 'error
         '(progn
            (with-open-output-file (s "/tmp/isl-p4-ms-eof2.txt") nil)
            (with-open-input-file (s "/tmp/isl-p4-ms-eof2.txt")
              (read-char s)))
         'error)))

(define phase4-write-cases
  (list
   ;; 4-B: write outputs readable representation
   (list 'value
         '(progn
            (with-open-output-file (s "/tmp/isl-p4-ms-write.txt")
              (write "abc" s))
            (with-open-input-file (s "/tmp/isl-p4-ms-write.txt")
              (read-line s)))
         "\"abc\"")
   ;; terpri returns nil
   (list 'value
         '(with-open-output-file (s "/tmp/isl-p4-ms-terpri.txt")
            (terpri s))
         '())
   ;; print returns the object
   (list 'value
         '(with-open-output-file (s "/tmp/isl-p4-ms-print.txt")
            (print 77 s))
         77)))

(define phase4-stream-cases
  (list
   ;; 4-C: open-output-stream / open-input-stream / close
   (list 'value
         '(let ((s (open-output-stream "/tmp/isl-p4-ms-ois.txt")))
            (write-char #\Q s)
            (close s)
            (let ((r (open-input-stream "/tmp/isl-p4-ms-ois.txt")))
              (let ((c (read-char r)))
                (close r)
                c)))
         #\Q)
   ;; close returns nil
   (list 'value
         '(let ((s (open-output-stream "/tmp/isl-p4-ms-close.txt")))
            (close s))
         '())
   ;; 4-D: input-stream-p / output-stream-p
   (list 'value
         '(input-stream-p *standard-input*)
         #t)
   (list 'value
         '(output-stream-p *standard-output*)
         #t)
   (list 'value
         '(output-stream-p *error-output*)
         #t)
   (list 'value
         '(input-stream-p 42)
         '())
   (list 'value
         '(output-stream-p "foo")
         '())))

(define phase4-readline-cases
  (list
   ;; 4-F: read-line with 0 args (reads from current input) – tested via stream arg
   (list 'value
         '(progn
            (with-open-output-file (s "/tmp/isl-p4-ms-rl.txt")
              (write-char #\h s) (write-char #\e s) (write-char #\l s)
              (write-char #\l s) (write-char #\o s))
            (with-open-input-file (s "/tmp/isl-p4-ms-rl.txt")
              (read-line s)))
         "hello")
   ;; read-line with eof-error-p = nil returns nil at EOF
   (list 'value
         '(progn
            (with-open-output-file (s "/tmp/isl-p4-ms-rl-eof.txt") nil)
            (with-open-input-file (s "/tmp/isl-p4-ms-rl-eof.txt")
              (read-line s nil)))
         '())))

(define phase5-error-cases
  (list
   ;; 5-B: error + condition-message
   (list 'value
         '(handler-case
             (error "test error")
             (<error> (c) (condition-message c)))
         "test error")
   ;; 5-B: error with irritants still gives message
   (list 'value
         '(handler-case
             (error "msg" 1 2 3)
             (<error> (c) (condition-message c)))
         "msg")
   ;; 5-B: unhandled error raises condition
   (list 'error '(error "boom") 'error)
   ;; 5-C: cerror is continuable
   (list 'value
         '(handler-case
             (cerror "continue" "recoverable")
             (<simple-error> (c) (condition-continuable c)))
         #t)
   ;; 5-C: error is not continuable
   (list 'value
         '(handler-case
             (error "non-recoverable")
             (<error> (c) (if (condition-continuable c) #t #f)))
         #f)
   ;; 5-D: cerror message accessible
   (list 'value
         '(handler-case
             (cerror "continue-msg" "cerror-error")
             (<simple-error> (c) (condition-message c)))
         "cerror-error")))

(define phase5-handler-cases
  (list
   ;; 5-E: with-handler catches error
   (list 'value
         '(with-handler
             (lambda (c) (string-append "got: " (condition-message c)))
             (error "oops"))
         "got: oops")
   ;; 5-E: with-handler returns protected value when no error
   (list 'value
         '(with-handler
             (lambda (c) "error")
             42)
         42)
   ;; 5-E: nested with-handler – inner fires first
   (list 'value
         '(with-handler
             (lambda (c) "outer")
             (with-handler
               (lambda (c) "inner")
               (error "test")))
         "inner")))

(define phase5-hierarchy-cases
  (list
   ;; 5-F: <simple-error> caught by <error>
   (list 'value
         '(handler-case
             (error "test")
             (<error> (c) "caught"))
         "caught")
   ;; 5-F: <error> caught by <serious-condition>
   (list 'value
         '(handler-case
             (error "test")
             (<serious-condition> (c) "caught"))
         "caught")
   ;; 5-F: first matching clause wins
   (list 'value
         '(handler-case
             (error "test")
             (<simple-error> (c) "simple")
             (<error>        (c) "error"))
         "simple")
   ;; 5-F: <end-of-stream> caught by <stream-error>
   (list 'value
         '(progn
            (with-open-output-file (s "/tmp/isl-p5-ms-eos.txt") nil)
            (handler-case
              (with-open-input-file (s "/tmp/isl-p5-ms-eos.txt")
                (read-char s))
              (<stream-error> (c) "stream-error")))
         "stream-error")))

(define phase5-ignore-cases
  (list
   ;; 5-G: ignore-errors suppresses error → nil
   (list 'value
         '(ignore-errors (error "ignored"))
         '())
   ;; 5-G: ignore-errors returns value on success
   (list 'value
         '(ignore-errors 42)
         42)
   ;; 5-G: ignore-errors returns last form on success
   (list 'value
         '(ignore-errors (+ 1 2) 99)
         99)))

;; ---- Phase 6-B: defvar / dynamic / dynamic-let ----
(define phase6-dynamic-cases
  (list
   ;; defvar introduces a dynamic variable
   (list 'value
         '(progn (defvar *p6-x* 10) (dynamic *p6-x*))
         10)
   ;; defvar is idempotent — second defvar does not rebind
   (list 'value
         '(progn (defvar *p6-idem* 42) (defvar *p6-idem* 99) (dynamic *p6-idem*))
         42)
   ;; dynamic-let provides temporary rebinding
   (list 'value
         '(progn
            (defvar *p6-y* 1)
            (dynamic-let ((*p6-y* 2))
              (dynamic *p6-y*)))
         2)
   ;; After dynamic-let, original value is restored
   (list 'value
         '(progn
            (defvar *p6-z* 1)
            (dynamic-let ((*p6-z* 2))
              (dynamic *p6-z*))
            (dynamic *p6-z*))
         1)
   ;; Nested dynamic-let — inner wins
   (list 'value
         '(progn
            (defvar *p6-n* 0)
            (dynamic-let ((*p6-n* 10))
              (dynamic-let ((*p6-n* 20))
                (dynamic *p6-n*))))
         20)
   ;; dynamic-let with multiple bindings
   (list 'value
         '(progn
            (defvar *p6-a* 1)
            (defvar *p6-b* 2)
            (dynamic-let ((*p6-a* 10) (*p6-b* 20))
              (+ (dynamic *p6-a*) (dynamic *p6-b*))))
         30)))

;; ---- Phase 6-A: #f / nil ----
(define phase6-sanitize-cases
  (list
   ;; In extended mode, nil is '()
   (list 'value '(null '()) #t)
   ;; t is #t
   (list 'value '(eq t t) #t)))

;; ---- Phase 7: OOP 完全化 ----
(define phase7-next-method-cases
  (list
   ;; 7-A: call-next-method in primary methods
   (list 'value
         '(progn
            (defclass <p7-animal> () ())
            (defgeneric p7-speak (a))
            (defmethod p7-speak ((a <p7-animal>)) "woof")
            (defclass <p7-dog> (<p7-animal>) ())
            (defmethod p7-speak ((d <p7-dog>))
              (string-append "Dog: " (call-next-method)))
            (p7-speak (make-instance (class <p7-dog>))))
         "Dog: woof")
   ;; next-method-p with a next method
   (list 'value
         '(progn
            (defclass <p7-base> () ())
            (defclass <p7-sub> (<p7-base>) ())
            (defgeneric p7-nm-test (x))
            (defmethod p7-nm-test ((x <p7-base>)) "base")
            (defmethod p7-nm-test ((x <p7-sub>))
              (if (next-method-p) #t '()))
            (p7-nm-test (make-instance (class <p7-sub>))))
         #t)
   ;; next-method-p without a next method
   (list 'value
         '(progn
            (defclass <p7-lone> () ())
            (defgeneric p7-lone-fn (x))
            (defmethod p7-lone-fn ((x <p7-lone>))
              (if (next-method-p) #t '()))
            (p7-lone-fn (make-instance (class <p7-lone>))))
         '())))

(define phase7-aux-method-cases
  (list
   ;; 7-B: :before/:after order
   (list 'value
         '(progn
            (defvar p7-log (quote ()))
            (defclass <p7-evt> () ())
            (defgeneric p7-fire (x))
            (defmethod p7-fire ((x <p7-evt>)) (setq p7-log (cons (quote main) p7-log)) (quote ok))
            (defmethod p7-fire :before ((x <p7-evt>)) (setq p7-log (cons (quote before) p7-log)))
            (defmethod p7-fire :after  ((x <p7-evt>)) (setq p7-log (cons (quote after)  p7-log)))
            (p7-fire (make-instance (class <p7-evt>)))
            p7-log)
         '(after main before))
   ;; 7-B: :around wraps primary
   (list 'value
         '(progn
            (defclass <p7-wrap> () ())
            (defgeneric p7-wrap-fn (x))
            (defmethod p7-wrap-fn ((x <p7-wrap>)) "inner")
            (defmethod p7-wrap-fn :around ((x <p7-wrap>))
              (string-append "[" (call-next-method) "]"))
            (p7-wrap-fn (make-instance (class <p7-wrap>))))
         "[inner]")))

(define phase7-subclassp-cases
  (list
   ;; 7-C: subclassp
   (list 'value
         '(progn
            (defclass <p7-animal> () ())
            (defclass <p7-cat> (<p7-animal>) ())
            (subclassp (class <p7-cat>) (class <p7-animal>)))
         #t)
   (list 'value
         '(progn
            (defclass <p7-x> () ())
            (defclass <p7-y> () ())
            (subclassp (class <p7-x>) (class <p7-y>)))
         '())
   ;; 7-C: (class <name>) form
   (list 'value
         '(progn
            (defclass <p7-named> () ())
            (if (class <p7-named>) #t '()))
         #t)))

(define phase7-standard-object-cases
  (list
   ;; 7-D: user class inherits from <standard-object>
   (list 'value
         '(progn
            (defclass <p7-user> () ())
            (subclassp (class <p7-user>) (class <standard-object>)))
         #t)
   ;; <standard-object> exists
   (list 'value
         '(if (class <standard-object>) #t '())
         #t)
   ;; <built-in-class> exists
   (list 'value
         '(if (class <built-in-class>) #t '())
         #t)))

(define phase7-slot-cases
  (list
   ;; 7-F: slot-boundp
   (list 'value
         '(progn
            (defclass <p7-s> () ((v :initarg :v :initform 1)))
            (defvar p7-s-obj (make-instance (class <p7-s>)))
            (slot-boundp p7-s-obj (quote v)))
         #t)
   ;; slot-makunbound then slot-boundp
   (list 'value
         '(progn
            (defclass <p7-s2> () ((w :initform 2)))
            (defvar p7-s2-obj (make-instance (class <p7-s2>)))
            (slot-makunbound p7-s2-obj (quote w))
            (slot-boundp p7-s2-obj (quote w)))
         '())))

;; ---- Phase 8: format ----

(define phase8-format-cases
  (list
   ;; 8-A: format nil ~a number → string
   (list 'value
         '(format nil "~a" 42)
         "42")
   ;; 8-A: format nil ~a string → string
   (list 'value
         '(format nil "~a" "hello")
         "hello")
   ;; 8-A: format nil multiple args
   (list 'value
         '(format nil "~a and ~a" 1 2)
         "1 and 2")
   ;; 8-A: format nil ~s → quoted string
   (list 'value
         '(format nil "~s" "hi")
         "\"hi\"")
   ;; 8-A: format nil ~~ → tilde
   (list 'value
         '(format nil "~~")
         "~")
   ;; 8-A: format nil ~% → newline
   (list 'value
         '(format nil "line1~%line2")
         "line1\nline2")
   ;; 8-A: format nil ~d decimal
   (list 'value
         '(format nil "~d" 255)
         "255")
   ;; 8-A: format nil ~x hexadecimal
   (list 'value
         '(format nil "~x" 255)
         "ff")
   ;; 8-A: format nil ~o octal
   (list 'value
         '(format nil "~o" 255)
         "377")
   ;; 8-A: format nil ~b binary
   (list 'value
         '(format nil "~b" 10)
         "1010")
   ;; 8-A: format nil ~c character
   (list 'value
         '(format nil "~c" #\A)
         "A")
   ;; 8-A: format nil ~i ignores arg
   (list 'value
         '(format nil "~ihello" 42)
         "hello")
   ;; 8-A: format t returns nil
   (list 'value
         '(format t "~a" 1)
         '())
   ;; 8-A: too few args → error
   (list 'error
         '(format nil "~a")
         'error)
   ;; 8-A: bad destination → error
   (list 'error
         '(format 42 "~a" 1)
         'error)))

;; ---- std-A: class-of and built-in class specializers ----
(define std-builtin-class-cases
  (list
   ;; class-of returns object with correct name
   (list 'value '(eq (class-of 42) (class <integer>))       #t)
   (list 'value '(eq (class-of 1.5) (class <float>))        #t)
   (list 'value '(eq (class-of "hi") (class <string>))      #t)
   (list 'value '(eq (class-of #\a) (class <character>))    #t)
   (list 'value '(eq (class-of 'foo) (class <symbol>))      #t)
   (list 'value '(eq (class-of '()) (class <null>))         #t)
   (list 'value '(eq (class-of '(1 2)) (class <cons>))      #t)
   ;; defmethod with built-in specializer
   (list 'value
         '(progn
            (defgeneric std-type-name (x))
            (defmethod std-type-name ((x <integer>)) "integer")
            (defmethod std-type-name ((x <string>))  "string")
            (defmethod std-type-name ((x <cons>))    "cons")
            (list (std-type-name 42) (std-type-name "hi") (std-type-name '(1 2))))
         '("integer" "string" "cons"))
   ;; subclassp with built-in classes
   (list 'value '(subclassp (class <integer>) (class <number>))  #t)
   (list 'value '(subclassp (class <float>)   (class <number>))  #t)
   (list 'value '(subclassp (class <cons>)    (class <list>))    #t)
   (list 'value '(subclassp (class <null>)    (class <list>))    #t)))

;; ---- std-B: apply variadic form ----
(define std-apply-cases
  (list
   (list 'value '(apply + '(1 2 3))         6)
   (list 'value '(apply + 1 '(2 3))         6)
   (list 'value '(apply + 1 2 '(3 4))       10)
   (list 'value '(apply list 1 2 '(3 4))    '(1 2 3 4))
   (list 'error '(apply + 1 2)              'error)))  ; last arg not a list

;; ---- std-C: assoc with equal test ----
(define std-assoc-equal-cases
  (list
   (list 'value '(assoc "b" '(("a" 1) ("b" 2) ("c" 3)))  '("b" 2))
   (list 'value '(assoc "x" '(("a" 1) ("b" 2)))          '())
   (list 'value '(assoc '(1) '(((1) a) ((2) b)))          '((1) a))
   (list 'value '(assoc 'b '((a 1) (b 2)))                '(b 2))))

;; ---- std-D: setf places ----
(define std-setf-place-cases
  (list
   ;; setf string-ref
   (list 'value
         '(let ((s (create-string 3 #\x)))
            (setf (string-ref s 1) #\y)
            s)
         "xyx")
   ;; setf general-vector-ref
   (list 'value
         '(let ((v (create-vector 3 0)))
            (setf (general-vector-ref v 1) 99)
            (general-vector-ref v 1))
         99)
   ;; setf dynamic
   (list 'value
         '(progn
            (defvar *dv-setf* 1)
            (setf (dynamic *dv-setf*) 42)
            (dynamic *dv-setf*))
         42)))

;; ---- std-E: standard naming ----
(define std-naming-cases
  (list
   (list 'value '(char-code #\A)                65)
   (list 'value '(code-char 65)                 #\A)
   (list 'value '(list-length '(a b c))         3)
   (list 'value '(basic-vector-p (vector 1 2))  #t)
   (list 'value '(basic-vector-p "hello")       #t)
   (list 'value '(basic-vector-p '(1 2))        '())
   (list 'value '(vector-length (vector 1 2 3)) 3)
   (list 'value '(symbol-name 'hello)           "hello")))

;; ---- std-F: character predicates ----
(define std-char-pred-cases
  (list
   (list 'value '(char-alphabetic-p #\a)    #t)
   (list 'value '(char-alphabetic-p #\1)    '())
   (list 'value '(char-numeric-p #\5)       #t)
   (list 'value '(char-numeric-p #\a)       '())
   (list 'value '(char-whitespace-p #\space) #t)
   (list 'value '(char-whitespace-p #\a)    '())
   (list 'value '(char-upper-case-p #\A)    #t)
   (list 'value '(char-upper-case-p #\a)    '())
   (list 'value '(char-lower-case-p #\a)    #t)
   (list 'value '(char-lower-case-p #\A)    '())))

;; ---- std-G: some/every/notany/notevery ----
(define std-higher-order-cases
  (list
   ;; some returns the pred result (oddp returns #t), not the element
   (list 'value '(some oddp '(2 4 5 6))     #t)
   (list 'value '(some oddp '(2 4 6))       '())
   (list 'value '(every oddp '(1 3 5))      #t)
   (list 'value '(every oddp '(1 2 5))      '())
   (list 'value '(notany oddp '(2 4 6))     #t)
   (list 'value '(notany oddp '(2 3 6))     '())
   (list 'value '(notevery oddp '(1 2 3))   #t)
   (list 'value '(notevery oddp '(1 3 5))   '())))

;; ---- std-H: read EOF raises <end-of-stream> ----
(define std-eof-cases
  (list
   (list 'value
         '(handler-case
            (let ((s (open-input-stream "/dev/null")))
              (read s))
            (<end-of-stream> (c) "caught"))
         "caught")))

;; std-Loop: ISLISP infinite (loop body*) terminated by return-from/block
(define std-loop-cases
  (list
   ;; basic counted loop via block+return-from
   (list 'value
         '(progn
            (defglobal std-loop-n 0)
            (block done
              (loop
                (setq std-loop-n (+ std-loop-n 1))
                (when (= std-loop-n 3) (return-from done std-loop-n))))
            std-loop-n)
         3)
   ;; loop with accumulator
   (list 'value
         '(let ((acc 0) (i 0))
            (block done
              (loop
                (setq i (+ i 1))
                (setq acc (+ acc i))
                (when (= i 5) (return-from done acc)))))
         15)
   ;; return-from returns the value
   (list 'value
         '(block b (loop (return-from b 42)))
         42)))

;; std-Instancep: (instancep obj class) 2-argument form
(define std-instancep-cases
  (list
   (list 'value '(instancep 42 <integer>) #t)
   (list 'value '(instancep 3.14 <float>) #t)
   (list 'value '(instancep "hi" <string>) #t)
   (list 'value '(instancep 'foo <symbol>) #t)
   (list 'value '(instancep #\a <character>) #t)
   (list 'value '(instancep '(1 2) <cons>) #t)
   (list 'value '(instancep '() <null>) #t)
   (list 'value '(instancep 42 <number>) #t)   ; integer is-a number
   (list 'value '(instancep 42 <string>) '())  ; false
   (list 'value '(instancep "x" <integer>) '())))

;; std-Convert: type conversion between built-in types
(define std-convert-cases
  (list
   (list 'value '(convert 3.7 <integer>) 3)
   (list 'value '(convert 5 <float>) 5.0)
   (list 'value '(convert "hello" <symbol>) 'hello)
   (list 'value '(convert 'world <string>) "world")
   (list 'value '(convert #\A <integer>) 65)
   (list 'value '(convert 65 <character>) #\A)
   (list 'value '(convert '(#\a #\b #\c) <string>) "abc")
   (list 'value '(convert "abc" <list>) '(#\a #\b #\c))
   (list 'value '(convert 42 <string>) "42")
   (list 'error '(convert "x" <integer>) 'error)))

;; std-Misc: apply, identity, eval, convert, symbol-name, vector-length, list-length
(define std-misc-cases
  (list
   (list 'value '(apply + '(1 2 3)) 6)
   (list 'value '(apply + 1 2 '(3 4)) 10)
   (list 'value '(identity 42) 42)
   (list 'value '(identity "hello") "hello")
   (list 'value '(symbol-name 'foo) "foo")
   (list 'value '(list-length '(a b c)) 3)
   (list 'value '(vector-length (vector 1 2 3)) 3)
   (list 'value '(char-code #\A) 65)
   (list 'value '(code-char 65) #\A)
   (list 'value '(basic-array-p (vector 1 2)) #t)
   (list 'value '(basic-array-p "str") #t)
   (list 'value '(basic-array-p 42) '())
   ;; basic-array*-p: multi-dim basic arrays (not in this impl)
   (list 'value '(basic-array*-p (vector 1 2)) '())
   (list 'value '(basic-array*-p "str") '())))

;; std-MapcarMulti: mapcar with multiple lists (ISLISP §15)
(define std-mapcar-multi-cases
  (list
   (list 'value '(mapcar + '(1 2 3) '(4 5 6)) '(5 7 9))
   (list 'value '(mapcar * '(1 2 3) '(4 5 6)) '(4 10 18))
   (list 'value '(mapcar + '(1 2) '(10 20) '(100 200)) '(111 222))
   (list 'value '(mapcar + '()) '())
   (list 'value '(mapcar cons '(1 2 3) '(a b c)) '((1 . a) (2 . b) (3 . c)))))

;; std-SymbolpNil: symbolp on nil (ISLISP §14: nil IS a symbol)
(define std-symbolp-nil-cases
  (list
   (list 'value '(symbolp nil) #t)
   (list 'value '(symbolp 'foo) #t)
   (list 'value '(symbolp 42) '())
   (list 'value '(symbolp "str") '())
   (list 'value '(atom nil) #t)
   (list 'value '(atom 42) #t)
   (list 'value '(atom '(1 2)) '())
   (list 'value '(eql 42 42) #t)
   (list 'value '(eql 'a 'a) #t)
   (list 'value '(eql 42 43) '())))

;; std-NewPredicates: gensym, create-string, aref, string comparisons
(define std-new-predicates-cases
  (list
   (list 'value '(symbolp (gensym)) #t)
   (list 'value '(create-string 4 #\z) "zzzz")
   (list 'value '(create-string 0 #\x) "")
   (list 'value '(aref (vector 10 20 30) 1) 20)
   (list 'value '(aref "hello" 0) #\h)
   (list 'value '(string/= "abc" "abd") #t)
   (list 'value '(string/= "abc" "abc") '())
   (list 'value '(string<= "abc" "abc") #t)
   (list 'value '(string<= "abc" "abd") #t)
   (list 'value '(string<= "abd" "abc") '())
   (list 'value '(string>= "abc" "abc") #t)
   (list 'value '(string>= "abd" "abc") #t)
   (list 'value '(string>= "abc" "abd") '())))

;; std-SetfCarCdr: setf on car/cdr places
(define std-setf-car-cdr-cases
  (list
   (list 'value
         '(let ((x (list 1 2 3)))
            (setf (car x) 99)
            x)
         '(99 2 3))
   (list 'value
         '(let ((x (list 1 2 3)))
            (setf (cdr x) (list 9 8))
            x)
         '(1 9 8))
   (list 'value
         '(let ((x (list 1 2 3)))
            (setf (car x) 10)
            (setf (cdr x) '(20 30))
            x)
         '(10 20 30))))

;; std-For: ISLISP §10.8 for special form
(define std-for-cases
  (list
   (list 'value '(for ((i 0 (+ i 1))) ((= i 3) i)) 3)
   (list 'value '(for ((i 0 (+ i 1)) (s 0 (+ s i))) ((= i 5) s)) 10)
   (list 'value '(let ((acc '())) (for ((x '(1 2 3) (cdr x))) ((null x) (reverse acc)) (setq acc (cons (* (car x) 2) acc)))) '(2 4 6))
   (list 'value '(for () (#t 42)) 42)))

;; std-Defdynamic: ISLISP §23.1 defdynamic
(define std-defdynamic-cases
  (list
   (list 'value
         '(progn (defdynamic *dyn-x* 10) (dynamic *dyn-x*))
         10)
   (list 'value
         '(progn (defdynamic *dyn-y* 0)
                 (dynamic-let ((*dyn-y* 99)) (dynamic *dyn-y*)))
         99)
   (list 'value
         '(progn (defdynamic *dyn-z* 1) (defdynamic *dyn-z* 2) (dynamic *dyn-z*))
         2)))

;; std-CaseElse: ISLISP §10.2 case with else/otherwise/t
(define std-case-else-cases
  (list
   (list 'value '(case 5 ((1) "one") (else "other")) "other")
   (list 'value '(case 5 ((1) "one") (otherwise "other")) "other")
   (list 'value '(case 5 ((1) "one") (t "other")) "other")
   (list 'value '(case 1 ((1) "one") (else "other")) "one")
   (list 'value '(case 99 ((1 2 3) "low") (else "high")) "high")))

;; std-SymbolpT: ISLISP §9.6 symbolp of t and symbol-name
(define std-symbolp-t-cases
  (list
   (list 'value '(symbolp t) #t)
   (list 'value '(symbolp nil) #t)
   (list 'value '(symbol-name nil) "nil")
   (list 'value '(symbol-name t) "t")
   (list 'value '(symbol-name 'foo) "foo")))

;; std-ConditionHandling: ISL condition classes catchable by handler-case
(define std-condition-handling-cases
  (list
   ;; division by zero signals <division-by-zero> which is-a <error>
   (list 'value '(handler-case (/ 1 0) (<error> (c) 'caught)) 'caught)
   (list 'value '(handler-case (/ 1 0) (<arithmetic-error> (c) 'caught)) 'caught)
   (list 'value '(handler-case (/ 1 0) (<division-by-zero> (c) 'caught)) 'caught)
   ;; car/cdr of non-list signals <domain-error>
   (list 'value '(handler-case (car 42) (<error> (c) 'caught)) 'caught)
   (list 'value '(handler-case (cdr 42) (<domain-error> (c) 'caught)) 'caught)
   ;; car/cdr of nil returns nil per ISLISP §15.1
   (list 'value '(car nil) '())
   (list 'value '(cdr nil) '())
   ;; no-applicable-method signals condition
   (list 'value
         '(progn
            (defgeneric std-gen-nomethod (x))
            (handler-case (std-gen-nomethod 42) (<program-error> (c) 'no-method)))
         'no-method)))

;; std-MakeCondition: make-condition and condition-class (§22)
(define std-make-condition-cases
  (list
   (list 'value
         '(let ((c (make-condition '<simple-error> "test error")))
            (condition-message c))
         "test error")
   (list 'value
         '(handler-case
            (signal-condition (make-condition '<simple-error> "sig") nil)
            (<error> (c) (condition-message c)))
         "sig")
   (list 'value
         '(instancep (handler-case (error "x") (<error> (c) c)) <error>)
         #t)
   (list 'value
         '(instancep (handler-case (error "x") (<error> (c) c)) <serious-condition>)
         #t)))

;; std-SetfArfString: setf aref on strings (§16.3)
(define std-setf-aref-string-cases
  (list
   (list 'value
         '(let ((s (create-string 5 #\a)))
            (setf (aref s 0) #\H)
            s)
         "Haaaa")
   (list 'value
         '(let ((s (string #\h #\e #\l #\l #\o)))
            (setf (aref s 4) #\!)
            s)
         "hell!")))

;; std-Expt: expt with negative exponent returns float (§11.8)
(define std-expt-cases
  (list
   (list 'value '(expt 2 10) 1024)
   (list 'value '(expt 3 0)  1)
   (list 'value '(expt 2 -1) 0.5)
   (list 'value '(expt 4 -1) 0.25)
   (list 'value '(floatp (expt 2 -1)) #t)))

;; std-CharIndex: char-index and out-of-range (§17.4)
(define std-char-index-cases
  (list
   (list 'value '(char-index #\o "hello") 4)
   (list 'value '(char-index #\x "hello") '())
   (list 'value '(char-index #\l "hello" 3) 3)
   (list 'value '(char-index #\l "hello" 10) '())  ;; out-of-range → nil
   (list 'value '(char-index #\h "hello" 0) 0)))

;; std-Substring: substring 2-arg form (§17.3)
(define std-substring-cases
  (list
   (list 'value '(substring "hello" 1 3) "el")
   (list 'value '(substring "hello" 1)   "ello")  ;; 2-arg form
   (list 'value '(substring "hello" 0)   "hello")
   (list 'value '(substring "hello" 5)   "")))

;; std-StreamFunctions: standard-input/output as zero-arg functions (§18.1)
(define std-stream-fn-cases
  (list
   (list 'value '(input-stream-p  (standard-input))  #t)
   (list 'value '(output-stream-p (standard-output)) #t)
   (list 'value '(functionp (function standard-input))  #t)
   (list 'value '(functionp (function standard-output)) #t)))

;; std-IOFunctions: fresh-line, write-string, prin1, princ, stream-ready-p (§18.5/7)
(define std-io-fn-cases
  (list
   (list 'value '(with-open-output-file (s "/tmp/isl-io-fn-test.txt")
                    (write-string "hello" s)
                    nil)
         '())
   (list 'value '(with-open-input-file (s "/tmp/isl-io-fn-test.txt")
                    (read-line s))
         "hello")
   (list 'value '(with-open-output-file (s "/tmp/isl-io-fn-test2.txt")
                    (prin1 "hello" s)
                    nil)
         '())
   (list 'value '(stream-ready-p (standard-input)) #t)))

;; std-VectorSet: vector-set! (§16)
(define std-vector-set-cases
  (list
   (list 'value '(let ((v (make-vector 3 0)))
                    (vector-set! v 1 42)
                    (vector-ref v 1))
         42)
   (list 'value '(let ((v (vector 1 2 3)))
                    (vector-set! v 0 99)
                    v)
         #(99 2 3))))

;; std-Mapc: mapc returns first list (§15.8)
(define std-mapc-cases
  (list
   (list 'value
         '(let* ((result '())
                 (first-list '(1 2 3))
                 (ret (mapc (lambda (x) (setq result (cons x result))) first-list)))
            (eq ret first-list))
         #t)
   (list 'value
         '(let ((acc '()))
            (mapc (lambda (x) (setq acc (cons x acc))) '(1 2 3))
            acc)
         '(3 2 1))))

;; std-Macrolet: macrolet defines local macros (§22)
(define std-macrolet-cases
  (list
   (list 'value
         '(macrolet ((double (x) (list '+ x x)))
            (double 5))
         10)
   (list 'value
         '(macrolet ((swap! (a b)
                      (list 'let (list (list 'tmp a))
                            (list 'setq a b)
                            (list 'setq b 'tmp)
                            'tmp)))
            (let ((x 1) (y 2))
              (swap! x y)
              (list x y)))
         '(2 1))
   (list 'value
         '(macrolet ((my-and (a b) (list 'if a b 'nil)))
            (my-and #t 42))
         42)))

;; std-CharConvert: char-to-integer, integer-to-char canonical ISLISP names (§12.3)
(define std-char-convert-cases
  (list
   (list 'value '(char-to-integer #\A) 65)
   (list 'value '(char-to-integer #\a) 97)
   (list 'value '(integer-to-char 65) #\A)
   (list 'value '(integer-to-char 97) #\a)
   (list 'value '(= (char-to-integer (integer-to-char 42)) 42) #t)))

;; std-StringList: string-to-list, list-to-string (§17.4)
(define std-string-list-cases
  (list
   (list 'value '(string-to-list "abc") '(#\a #\b #\c))
   (list 'value '(list-to-string '(#\x #\y #\z)) "xyz")
   (list 'value '(string-to-list "") '())
   (list 'value '(list-to-string (string-to-list "hello")) "hello")))

;; std-Elt: elt sequence accessor (§15.3)
(define std-elt-cases
  (list
   (list 'value '(elt '(10 20 30) 0) 10)
   (list 'value '(elt '(10 20 30) 2) 30)
   (list 'value '(elt (vector 7 8 9) 1) 8)
   (list 'value '(elt "hello" 1) #\e)
   (list 'error '(elt '(1 2 3) 5) 'error)))

;; std-Subseq: subseq — extract subsequence (§15.3)
(define std-subseq-cases
  (list
   (list 'value '(subseq '(1 2 3 4 5) 1 3) '(2 3))
   (list 'value '(subseq "hello" 1 4) "ell")
   (list 'value '(subseq (vector 10 20 30 40) 0 2) (vector 10 20))
   (list 'value '(subseq '(1 2 3) 0 0) '())))

;; std-IntegerLength: integer-length (§11.2)
(define std-integer-length-cases
  (list
   (list 'value '(integer-length 0) 0)
   (list 'value '(integer-length 1) 1)
   (list 'value '(integer-length 7) 3)
   (list 'value '(integer-length 255) 8)
   (list 'value '(integer-length -1) 0)))

;; std-SetCarCdr: set-car, set-cdr callable functions (§15.1)
(define std-set-car-cdr-cases
  (list
   ;; ISLISP §15.1: (set-car obj cons) / (set-cdr obj cons) — value first.
   (list 'value '(let ((p (cons 1 2))) (set-car 99 p) (car p)) 99)
   (list 'value '(let ((p (cons 1 2))) (set-cdr 88 p) (cdr p)) 88)
   (list 'error '(set-car 1 42) 'error)))

;; std-AppendBang: append! destructive append (§15.8)
(define std-append-bang-cases
  (list
   (list 'value '(let ((a (list 1 2)) (b (list 3 4))) (append! a b) a) '(1 2 3 4))
   (list 'value '(append! '() '(1 2)) '(1 2))))

;; std-GeneralVectorSet: general-vector-set (standard name without !) (§16)
(define std-general-vector-set-cases
  (list
   (list 'value '(let ((v (vector 1 2 3))) (general-vector-set 99 v 0) (general-vector-ref v 0)) 99)
   (list 'value '(let ((v (vector 10 20 30))) (general-vector-set 55 v 2) v) #(10 20 55))))

;; std-SetAref: set-aref (§16)
(define std-set-aref-cases
  (list
   (list 'value '(let ((v (vector 1 2 3))) (set-aref 77 v 1) (aref v 1)) 77)
   (list 'error '(set-aref 1 "hello" 0) 'error)))  ; string char required

;; std-FormatRadix: ~nR format directive (§20)
(define std-format-radix-cases
  (list
   (list 'value '(format nil "~2R" 10) "1010")
   (list 'value '(format nil "~8R" 255) "377")
   (list 'value '(format nil "~16R" 255) "ff")
   (list 'value '(format nil "~10R" 42) "42")))

;; std-MemqAssq: memq, memql, assq, assql (§15.3)
(define std-memq-assq-cases
  (list
   (list 'value '(memq 'b '(a b c)) '(b c))
   (list 'value '(memq 'd '(a b c)) '())
   (list 'value '(memql 2 '(1 2 3)) '(2 3))
   (list 'value '(assq 'b '((a 1) (b 2) (c 3))) '(b 2))
   (list 'value '(assql 2 '((1 a) (2 b) (3 c))) '(2 b))))

;; std-MapConMapl: mapl, mapcon (§15.6)
(define std-mapcon-mapl-cases
  (list
   (list 'value
         '(let ((result '()))
            (mapl (lambda (xs) (setq result (cons (car xs) result))) '(1 2 3))
            result)
         '(3 2 1))
   (list 'value '(mapcon (lambda (xs) (if (oddp (car xs)) xs '())) '(1 2 3 4 5))
         '(1 2 3 4 5 3 4 5 5))
   (list 'value '(mapcon (lambda (xs) (list (car xs))) '(a b c)) '(a b c))))

;; std-Fill: fill sequence with value (§15.7)
(define std-fill-cases
  (list
   (list 'value '(let ((v (vector 1 2 3))) (fill v 0) v) #(0 0 0))
   (list 'value '(let ((s (string-copy "abc"))) (fill s #\x) s) "xxx")
   (list 'value '(let ((lst (list 1 2 3))) (fill lst 9) lst) '(9 9 9))))

;; std-CopyList: copy-list (§15.2.2)
(define std-copy-list-cases
  (list
   (list 'value '(let* ((orig '(1 2 3)) (copy (copy-list orig)))
                   (eq orig copy))
         '())
   (list 'value '(equal (copy-list '(a b c)) '(a b c)) #t)))

;; std-NumberString: number-to-string, parse-number (§20.2 / §11.8)
(define std-number-string-cases
  (list
   (list 'value '(number-to-string 42) "42")
   (list 'value '(number-to-string 255 16) "ff")
   (list 'value '(parse-number "42") 42)
   (list 'value '(parse-number "ff" 16) 255)
   (list 'error '(parse-number "not-a-number") 'error)))

;; std-SymbolPackage: symbol-package (§14.1.2)
(define std-symbol-package-cases
  (list
   (list 'value '(stringp (symbol-package 'foo)) #t)
   (list 'value '(symbol-package 'nil) "isl")
   (list 'value '(symbol-package 't) "isl")))

;; std-OpenFile: open-input-file, open-output-file (§18.2 canonical names)
(define std-open-file-cases
  (list
   ;; open-input-file on a known system file
   (list 'value
         '(let* ((f (open-input-file "/dev/null"))
                 (_ (close f)))
            #t)
         #t)
   ;; open-output-file creates a writable stream
   (list 'value
         '(let* ((tmp "/tmp/isl-test-open.txt")
                 (f   (open-output-file tmp))
                 (_   (write-char #\x f))
                 (_   (close f)))
            #t)
         #t)))

;; std-FinishOutput: finish-output flushes without error (§18.7)
(define std-finish-output-cases
  (list
   (list 'value '(progn (finish-output) #t) #t)
   ;; finish-output on an output-file should not error
   (list 'value
         '(let* ((f (open-output-file "/tmp/isl-flush-test.txt"))
                 (_ (write-char #\z f))
                 (_ (finish-output f))
                 (_ (close f)))
            #t)
         #t)))

;; std-ClassNameOf: class-name returns a symbol (§6.3.1)
(define std-class-name-cases
  (list
   (list 'value '(class-name (class-of 42)) '<integer>)
   (list 'value '(class-name (class-of "hello")) '<string>)
   (list 'value '(class-name (class-of #\a)) '<character>)
   (list 'value '(class-name (class-of '())) '<null>)
   (list 'value '(symbolp (class-name (class-of #t))) #t)))

;; ---- Round 8 additions ----
(define std-streamp-cases
  (list
   (list 'value '(streamp (standard-input)) #t)
   (list 'value '(streamp (standard-output)) #t)
   (list 'value '(streamp 42) '())
   (list 'value '(streamp "hello") '())))

(define std-set-elt-cases
  (list
   (list 'value '(let ((v (vector 1 2 3))) (set-elt 99 v 1) (elt v 1)) 99)
   (list 'value '(let ((lst (list 10 20 30))) (set-elt 55 lst 2) (elt lst 2)) 55)
   (list 'value '(let ((s (create-string 3 #\a))) (set-elt #\z s 1) (elt s 1)) #\z)
   (list 'error '(set-elt 0 (vector 1 2) 5) 'error)))

(define std-condition-cont-cases
  (list
   (list 'value
         '(let ((c (make-condition '<error> "test" nil)))
            (condition-continuable-p c))
         '())
   (list 'value '(functionp (function condition-continuable-p)) #t)))

;; ---- Round 7 additions ----
(define std-neq-cases
  (list
   (list 'value '(/= 1 2) #t)
   (list 'value '(/= 2 2) '())
   (list 'value '(/= 1.0 2.0) #t)
   (list 'value '(/= 5 5) '())))

(define std-reciprocal-cases
  (list
   (list 'value '(floatp (reciprocal 2)) #t)
   (list 'value '(reciprocal 1) 1.0)
   (list 'value '(< (abs (- (reciprocal 4) 0.25)) 1e-10) #t)
   (list 'error '(reciprocal 0) 'error)))

(define std-cxr-cases
  (list
   (list 'value '(caar '((1 2) 3)) 1)
   (list 'value '(cadr '(1 2 3)) 2)
   (list 'value '(cdar '((1 2) 3)) '(2))
   (list 'value '(cddr '(1 2 3)) '(3))
   (list 'value '(caddr '(1 2 3)) 3)
   (list 'value '(cdddr '(1 2 3 4)) '(4))
   (list 'value '(cadddr '(1 2 3 4)) 4)
   (list 'value '(caaar '(((1)))) 1)))

(define std-position-cases
  (list
   (list 'value '(position 2 '(1 2 3)) 1)
   (list 'value '(position 9 '(1 2 3)) '())
   (list 'value '(position #\b "abc") 1)
   (list 'value '(position 3 (vector 1 2 3)) 2)))

(define std-count-cases
  (list
   (list 'value '(count 2 '(1 2 3 2 4)) 2)
   (list 'value '(count 9 '(1 2 3)) 0)
   (list 'value '(count #\a "banana") 3)
   (list 'value '(count 1 (vector 1 2 1 3)) 2)))

(define std-newline-cases
  (list
   (list 'value '(progn (newline) #t) #t)
   (list 'value '(let ((s (open-output-file "/tmp/isl-nl-test.txt")))
                   (newline s)
                   (close s)
                   #t) #t)))

(define std-make-string-cases
  (list
   (list 'value '(string-length (make-string 5)) 5)
   (list 'value '(make-string 3 #\x) "xxx")
   (list 'value '(make-string 0) "")))

(define std-string-set-cases
  (list
   (list 'value '(let ((s (make-string 3 #\a)))
                   (string-set s 1 #\b)
                   s) "aba")
   (list 'value '(let ((s (create-string 4 #\z)))
                   (string-set s 0 #\a)
                   (string-ref s 0)) #\a)
   (list 'error '(let ((s "abc")) (string-set s 5 #\x)) 'error)))

(define std-with-standard-stream-cases
  (list
   (list 'value
         '(let ((s (open-output-file "/tmp/isl-wso-test.txt")))
            (with-standard-output s
              (progn (princ "hello") (close s) #t)))
         #t)
   (list 'value
         '(with-standard-output (standard-output) 42)
         42)))

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
   (list "M11" m11-cases)
   (list "Phase1-Arith" phase1-arith-cases)
   (list "Phase2-List"   phase2-list-cases)
   (list "Phase2-String" phase2-string-cases)
   (list "Phase2-Char"   phase2-char-cases)
   (list "Phase2-Vector" phase2-vector-cases)
   (list "Phase3-Flet"       phase3-flet-cases)
   (list "Phase3-The"        phase3-the-cases)
   (list "Phase3-Unwind"     phase3-unwind-cases)
   (list "Phase3-Function"   phase3-function-cases)
   (list "Phase3-Defconstant" phase3-defconstant-cases)
   (list "Phase3-IO-Macro"   phase3-io-macro-cases)
   (list "Phase4-CharIO"     phase4-char-io-cases)
   (list "Phase4-Write"      phase4-write-cases)
   (list "Phase4-Stream"     phase4-stream-cases)
   (list "Phase4-ReadLine"   phase4-readline-cases)
   (list "Phase5-Error"      phase5-error-cases)
   (list "Phase5-Handler"    phase5-handler-cases)
   (list "Phase5-Hierarchy"  phase5-hierarchy-cases)
   (list "Phase5-Ignore"     phase5-ignore-cases)
   (list "Phase6-Dynamic"    phase6-dynamic-cases)
   (list "Phase6-Sanitize"   phase6-sanitize-cases)
   (list "Phase7-NextMethod" phase7-next-method-cases)
   (list "Phase7-AuxMethod"  phase7-aux-method-cases)
   (list "Phase7-Subclassp"  phase7-subclassp-cases)
   (list "Phase7-StdObject"  phase7-standard-object-cases)
   (list "Phase7-Slots"      phase7-slot-cases)
   (list "Phase8-Format"     phase8-format-cases)
   (list "std-BuiltinClass"  std-builtin-class-cases)
   (list "std-Apply"         std-apply-cases)
   (list "std-AssocEqual"    std-assoc-equal-cases)
   (list "std-SetfPlace"     std-setf-place-cases)
   (list "std-Naming"        std-naming-cases)
   (list "std-CharPred"      std-char-pred-cases)
   (list "std-HigherOrder"   std-higher-order-cases)
   (list "std-EofCondition"  std-eof-cases)
   (list "std-Loop"          std-loop-cases)
   (list "std-Instancep"     std-instancep-cases)
   (list "std-Convert"       std-convert-cases)
   (list "std-Misc"          std-misc-cases)
   (list "std-MapcarMulti"   std-mapcar-multi-cases)
   (list "std-SymbolpNil"    std-symbolp-nil-cases)
   (list "std-NewPredicates" std-new-predicates-cases)
   (list "std-SetfCarCdr"    std-setf-car-cdr-cases)
   (list "std-For"           std-for-cases)
   (list "std-Defdynamic"    std-defdynamic-cases)
   (list "std-CaseElse"      std-case-else-cases)
   (list "std-SymbolpT"      std-symbolp-t-cases)
   (list "std-ConditionHandling" std-condition-handling-cases)
   (list "std-MakeCondition" std-make-condition-cases)
   (list "std-SetfArfString" std-setf-aref-string-cases)
   (list "std-Expt"          std-expt-cases)
   (list "std-CharIndex"     std-char-index-cases)
   (list "std-Substring"     std-substring-cases)
   (list "std-StreamFn"      std-stream-fn-cases)
   (list "std-IOFn"          std-io-fn-cases)
   (list "std-VectorSet"     std-vector-set-cases)
   (list "std-Mapc"          std-mapc-cases)
   (list "std-Macrolet"      std-macrolet-cases)
   (list "std-CharConvert"   std-char-convert-cases)
   (list "std-StringList"    std-string-list-cases)
   (list "std-Elt"           std-elt-cases)
   (list "std-Subseq"        std-subseq-cases)
   (list "std-IntegerLength" std-integer-length-cases)
   (list "std-SetCarCdr"     std-set-car-cdr-cases)
   (list "std-AppendBang"    std-append-bang-cases)
   (list "std-GenVecSet"     std-general-vector-set-cases)
   (list "std-SetAref"       std-set-aref-cases)
   (list "std-FormatRadix"   std-format-radix-cases)
   (list "std-MemqAssq"      std-memq-assq-cases)
   (list "std-MapConMapl"    std-mapcon-mapl-cases)
   (list "std-Fill"          std-fill-cases)
   (list "std-CopyList"      std-copy-list-cases)
   (list "std-NumberString"  std-number-string-cases)
   (list "std-SymbolPackage" std-symbol-package-cases)
   (list "std-OpenFile"      std-open-file-cases)
   (list "std-FinishOutput"  std-finish-output-cases)
   (list "std-ClassName"     std-class-name-cases)
   (list "std-Neq"           std-neq-cases)
   (list "std-Reciprocal"    std-reciprocal-cases)
   (list "std-Cxr"           std-cxr-cases)
   (list "std-Position"      std-position-cases)
   (list "std-Count"         std-count-cases)
   (list "std-Newline"       std-newline-cases)
   (list "std-MakeString"    std-make-string-cases)
   (list "std-StringSet"     std-string-set-cases)
   (list "std-WithStdStream" std-with-standard-stream-cases)
   (list "std-Streamp"       std-streamp-cases)
   (list "std-SetElt"        std-set-elt-cases)
   (list "std-CondContP"     std-condition-cont-cases)))

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
   (list "M9" m9-cases)
   (list "M10" m10-cases)
   (list "M11" m11-cases)
   (list "Phase1-Arith" phase1-arith-cases)
   (list "Phase2-List"   phase2-list-cases)
   (list "Phase2-String" phase2-string-cases)
   (list "Phase2-Char"   phase2-char-cases)
   (list "Phase2-Vector" phase2-vector-cases)
   (list "Phase3-Flet"       phase3-flet-cases)
   (list "Phase3-The"        phase3-the-cases)
   (list "Phase3-Unwind"     phase3-unwind-cases)
   (list "Phase3-Function"   phase3-function-cases)
   (list "Phase3-Defconstant" phase3-defconstant-cases)
   (list "Phase3-IO-Macro"   phase3-io-macro-cases)
   (list "Phase4-CharIO"     phase4-char-io-cases)
   (list "Phase4-Write"      phase4-write-cases)
   (list "Phase4-Stream"     phase4-stream-cases)
   (list "Phase4-ReadLine"   phase4-readline-cases)
   (list "Phase5-Error"      phase5-error-cases)
   (list "Phase5-Handler"    phase5-handler-cases)
   (list "Phase5-Hierarchy"  phase5-hierarchy-cases)
   (list "Phase5-Ignore"     phase5-ignore-cases)
   (list "Phase6-Dynamic"    phase6-dynamic-cases)
   (list "Phase6-Sanitize"   phase6-sanitize-cases)
   (list "Phase7-NextMethod" phase7-next-method-cases)
   (list "Phase7-AuxMethod"  phase7-aux-method-cases)
   (list "Phase7-Subclassp"  phase7-subclassp-cases)
   (list "Phase7-StdObject"  phase7-standard-object-cases)
   (list "Phase7-Slots"      phase7-slot-cases)
   (list "Phase8-Format"     phase8-format-cases)
   (list "std-BuiltinClass"  std-builtin-class-cases)
   (list "std-Apply"         std-apply-cases)
   (list "std-AssocEqual"    std-assoc-equal-cases)
   (list "std-SetfPlace"     std-setf-place-cases)
   (list "std-Naming"        std-naming-cases)
   (list "std-CharPred"      std-char-pred-cases)
   (list "std-HigherOrder"   std-higher-order-cases)
   (list "std-EofCondition"  std-eof-cases)
   (list "std-Loop"          std-loop-cases)
   (list "std-Instancep"     std-instancep-cases)
   (list "std-Convert"       std-convert-cases)
   (list "std-Misc"          std-misc-cases)
   (list "std-MapcarMulti"   std-mapcar-multi-cases)
   (list "std-SymbolpNil"    std-symbolp-nil-cases)
   (list "std-NewPredicates" std-new-predicates-cases)
   (list "std-SetfCarCdr"    std-setf-car-cdr-cases)
   (list "std-For"           std-for-cases)
   (list "std-Defdynamic"    std-defdynamic-cases)
   (list "std-CaseElse"      std-case-else-cases)
   (list "std-SymbolpT"      std-symbolp-t-cases)
   (list "std-ConditionHandling" std-condition-handling-cases)
   (list "std-MakeCondition" std-make-condition-cases)
   (list "std-SetfArfString" std-setf-aref-string-cases)
   (list "std-Expt"          std-expt-cases)
   (list "std-CharIndex"     std-char-index-cases)
   (list "std-Substring"     std-substring-cases)
   (list "std-StreamFn"      std-stream-fn-cases)
   (list "std-IOFn"          std-io-fn-cases)
   (list "std-VectorSet"     std-vector-set-cases)
   (list "std-Mapc"          std-mapc-cases)
   (list "std-Macrolet"      std-macrolet-cases)
   (list "std-CharConvert"   std-char-convert-cases)
   (list "std-StringList"    std-string-list-cases)
   (list "std-Elt"           std-elt-cases)
   (list "std-Subseq"        std-subseq-cases)
   (list "std-IntegerLength" std-integer-length-cases)
   (list "std-SetCarCdr"     std-set-car-cdr-cases)
   (list "std-AppendBang"    std-append-bang-cases)
   (list "std-GenVecSet"     std-general-vector-set-cases)
   (list "std-SetAref"       std-set-aref-cases)
   (list "std-FormatRadix"   std-format-radix-cases)
   (list "std-MemqAssq"      std-memq-assq-cases)
   (list "std-MapConMapl"    std-mapcon-mapl-cases)
   (list "std-Fill"          std-fill-cases)
   (list "std-CopyList"      std-copy-list-cases)
   (list "std-NumberString"  std-number-string-cases)
   (list "std-SymbolPackage" std-symbol-package-cases)
   (list "std-OpenFile"      std-open-file-cases)
   (list "std-FinishOutput"  std-finish-output-cases)
   (list "std-ClassName"     std-class-name-cases)
   (list "std-Neq"           std-neq-cases)
   (list "std-Reciprocal"    std-reciprocal-cases)
   (list "std-Cxr"           std-cxr-cases)
   (list "std-Position"      std-position-cases)
   (list "std-Count"         std-count-cases)
   (list "std-Newline"       std-newline-cases)
   (list "std-MakeString"    std-make-string-cases)
   (list "std-StringSet"     std-string-set-cases)
   (list "std-WithStdStream" std-with-standard-stream-cases)
   (list "std-Streamp"       std-streamp-cases)
   (list "std-SetElt"        std-set-elt-cases)
   (list "std-CondContP"     std-condition-cont-cases)))

(define extended-milestones all-milestones)
