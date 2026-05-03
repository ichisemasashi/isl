(add-load-path "./src")
(use isl.compiler.runner)
(use isl.compiler.runtime)

(define (assert-equal expected actual label)
  (unless (equal? expected actual)
    (error "assertion failed" label expected actual)))

(define (assert-runtime-error thunk label)
  (guard (e
          ((runtime-error? e) #t)
          (else (error "expected runtime-error" label e)))
    (thunk)
    (error "expected runtime-error but got value" label)))

;; ---- 2-A: リスト関数 ----
(assert-equal '(3 2 1)   (run-forms '((reverse '(1 2 3))))         "reverse")
(assert-equal '()        (run-forms '((reverse '())))              "reverse nil")
(assert-equal '(3 2 1)   (run-forms '((nreverse (list 1 2 3))))    "nreverse")
(assert-equal '(2 4 6)   (run-forms '((map (lambda (x) (* x 2)) '(1 2 3)))) "map")
(assert-equal '(11 22)   (run-forms '((map (lambda (x y) (+ x y)) '(1 2) '(10 20)))) "map multi-list")
(assert-equal '()        (run-forms '((defglobal p2-log '())
                                       (for-each (lambda (x) (setq p2-log (cons x p2-log))) '())
                                       p2-log))                    "for-each empty")
(assert-equal '(c b a)   (run-forms '((defglobal p2-acc '())
                                       (mapc (lambda (x) (setq p2-acc (cons x p2-acc))) '(a b c))
                                       p2-acc))                    "mapc")
(assert-equal '(1 1 2 2 3 3) (run-forms '((mapcan (lambda (x) (list x x)) '(1 2 3)))) "mapcan")
(assert-equal '(1 2 3)   (run-forms '((maplist (lambda (x) (car x)) '(1 2 3)))) "maplist")
(assert-equal '(2 3)     (run-forms '((member 2 '(1 2 3))))        "member found")
(assert-equal '()        (run-forms '((member 9 '(1 2 3))))        "member not found")
(assert-equal '(b 2)     (run-forms '((assoc 'b '((a 1)(b 2)))))   "assoc found")
(assert-equal '()        (run-forms '((assoc 'z '((a 1)))))        "assoc not found")
(assert-equal '(1 3)     (run-forms '((remove 2 '(1 2 3 2))))      "remove")
(assert-equal '(3)       (run-forms '((last-pair '(1 2 3))))       "last-pair")
(assert-equal '(1 2 3 4) (run-forms '((nconc (list 1 2) (list 3 4)))) "nconc")
(assert-equal '(1 2)     (run-forms '((nconc '() '(1 2))))         "nconc nil head")
(assert-equal 'b         (run-forms '((list-ref '(a b c) 1)))      "list-ref")
(assert-equal '(c)       (run-forms '((list-tail '(a b c) 2)))     "list-tail")
(assert-equal '(0 0 0)   (run-forms '((create-list 3 0)))          "create-list")
(assert-equal '()        (run-forms '((create-list 0)))            "create-list zero")
(assert-equal '(1 2 3)   (run-forms '((list-copy '(1 2 3))))       "list-copy")
(assert-equal '(c d)     (run-forms '((nthcdr 2 '(a b c d))))      "nthcdr")
(assert-equal '(1 1 2 3 4 5) (run-forms '((sort '(3 1 4 1 5 2) (lambda (a b) (< a b))))) "sort")

;; ---- 2-B: 文字列関数 ----
(assert-equal 5          (run-forms '((string-length "hello")))    "string-length")
(assert-equal 0          (run-forms '((string-length "")))         "string-length empty")
(assert-equal #\e        (run-forms '((string-ref "hello" 1)))     "string-ref")
(assert-equal "abc"      (run-forms '((string-copy "abc")))        "string-copy")
(assert-equal "HELLO"    (run-forms '((string-upcase "hello")))    "string-upcase")
(assert-equal "hello"    (run-forms '((string-downcase "HELLO")))  "string-downcase")

;; ---- 2-C: 文字比較関数 ----
(assert-equal #t (run-forms '((char= #\a #\a)))   "char= eq")
(assert-equal #f (run-forms '((char= #\a #\b)))   "char= neq")
(assert-equal #t (run-forms '((char/= #\a #\b)))  "char/=")
(assert-equal #t (run-forms '((char< #\a #\b)))   "char<")
(assert-equal #f (run-forms '((char< #\b #\a)))   "char< false")
(assert-equal #t (run-forms '((char> #\b #\a)))   "char>")
(assert-equal #t (run-forms '((char<= #\a #\a)))  "char<=")
(assert-equal #t (run-forms '((char>= #\b #\a)))  "char>=")
(assert-equal #\A (run-forms '((char-upcase #\a))) "char-upcase")
(assert-equal #\a (run-forms '((char-downcase #\A))) "char-downcase")

;; ---- 2-D: ベクター ISLISP 標準名 ----
(assert-equal 20  (run-forms '((general-vector-ref (vector 10 20 30) 1)))  "general-vector-ref")
(assert-equal 99  (run-forms '((general-vector-ref (create-vector 3 99) 0))) "create-vector init")
(assert-equal 5   (run-forms '((length (create-vector 5))))                 "create-vector size")

;; ---- 2-E: 配列関数 ----
(assert-equal '(3) (run-forms '((array-dimensions (vector 1 2 3))))    "array-dimensions")
(assert-equal '(4) (run-forms '((array-dimensions (create-array 4 0)))) "create-array dims")

;; ---- エラーケース ----
(assert-runtime-error (lambda () (run-forms '((reverse 42))))          "reverse type")
(assert-runtime-error (lambda () (run-forms '((list-ref '(a b) 5))))   "list-ref range")
(assert-runtime-error (lambda () (run-forms '((string-length 42))))    "string-length type")
(assert-runtime-error (lambda () (run-forms '((string-ref "abc" 5))))  "string-ref range")
(assert-runtime-error (lambda () (run-forms '((char= #\a "a"))))       "char= type")
(assert-runtime-error (lambda () (run-forms '((array-dimensions '(1))))) "array-dimensions type")

(display "runtime Phase2 smoke passed\n")
