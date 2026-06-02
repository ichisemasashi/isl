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

;; ---- 3-A: flet ----
(assert-equal 10     (run-forms '((flet ((f (x) (+ x 1))) (f 9))))                          "flet basic")
(assert-equal 49     (run-forms '((flet ((sq (x) (* x x))) (sq 7))))                         "flet square")
(assert-equal 'hello (run-forms '((flet ((greet (name) name)) (greet 'hello))))               "flet symbol")
(assert-equal 15     (run-forms '((let ((x 10)) (flet ((f (y) (+ x y))) (f 5)))))            "flet closes over let")
;; flet with multiple bindings
(assert-equal 5      (run-forms '((flet ((add1 (x) (+ x 1))
                                         (mul2 (x) (* x 2)))
                                    (add1 (mul2 2)))))                                         "flet multi-binding")

;; ---- 3-A: labels (recursion) ----
(assert-equal 120    (run-forms '((labels ((fact (n) (if (= n 0) 1 (* n (fact (- n 1))))))
                                    (fact 5))))                                                "labels recursion")
(assert-equal #t     (run-forms '((labels ((even? (n) (if (= n 0) t (odd? (- n 1))))
                                            (odd?  (n) (if (= n 0) nil (even? (- n 1)))))
                                    (even? 4))))                                               "labels mutual recursion even")
(assert-equal #t     (run-forms '((labels ((even? (n) (if (= n 0) t (odd? (- n 1))))
                                            (odd?  (n) (if (= n 0) nil (even? (- n 1)))))
                                    (odd? 3))))                                                "labels mutual recursion odd")

;; ---- 3-B: the ----
(assert-equal 42     (run-forms '((the <integer> 42)))                                        "the integer ok")
(assert-equal "hi"   (run-forms '((the <string> "hi")))                                       "the string ok")
(assert-equal 3.14   (run-forms '((the <number> 3.14)))                                       "the number ok")
(assert-equal "anything" (run-forms '((the <object> "anything")))                            "the object ok")
(assert-runtime-error (lambda () (run-forms '((the <integer> "not-int"))))                    "the integer type error")
(assert-runtime-error (lambda () (run-forms '((the <string> 99))))                            "the string type error")

;; ---- 3-C: unwind-protect ----
(assert-equal 42     (run-forms '((unwind-protect 42 nil)))                                   "unwind-protect basic")
(assert-equal 3      (run-forms '((unwind-protect (+ 1 2) nil)))                              "unwind-protect value")
;; cleanup runs, but return value is from protected form
(assert-equal 1      (run-forms '((defglobal up-x 0)
                                   (unwind-protect
                                     (progn (setq up-x 1) up-x)
                                     (setq up-x 99))))                                        "unwind-protect cleanup runs")
;; cleanup runs even when error would propagate
(assert-equal 1      (run-forms '((defglobal cleanup-ran 0)
                                   (handler-case
                                     (unwind-protect
                                       (error "test")
                                       (setq cleanup-ran 1))
                                     (error (e) cleanup-ran))))                               "unwind-protect cleanup on error")

;; ---- 3-E: defconstant ----
(assert-equal 3      (run-forms '((defconstant +pi+ 3) +pi+))                                 "defconstant lookup")
(assert-equal 42     (run-forms '((defconstant +answer+ 42) +answer+))                        "defconstant value")

;; ---- 3-F: function ----
(assert-equal #t     (run-forms '((functionp (function +))))                                  "function builtin")
(assert-equal #t     (run-forms '((functionp (function car))))                                "function car")
(assert-equal #t     (run-forms '((functionp (function (lambda (x) x)))))                    "function lambda")
(assert-equal 5      (run-forms '((let ((f (function +))) (funcall f 2 3))))                  "function funcall")

;; ---- 3-D: with-open-input-file / with-open-output-file (macro expansion) ----
;; Test that the macros expand correctly to with-open-file.
;; Just verify a stream is returned (write-char/read-char are Phase 4).
(assert-equal #t
              (let ((result (run-forms '((with-open-output-file (s "/tmp/isl-p3-smoke.txt")
                                           s)))))
                (or (output-port? result) (input-port? result)))
              "with-open-output-file macro expansion")
(assert-equal #t
              (let ((result (run-forms '((with-open-input-file (s "/dev/null")
                                           s)))))
                (or (output-port? result) (input-port? result) (port? result)))
              "with-open-input-file macro expansion")

(display "runtime Phase3 smoke passed\n")
