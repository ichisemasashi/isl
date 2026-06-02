(add-load-path "./src")
(use isl.compiler.runner)
(use isl.compiler.runtime)

(define (assert-equal expected actual label)
  (unless (equal? expected actual)
    (error "assertion failed" label expected actual)))

(define (assert-isl-error thunk label)
  ;; Accepts runtime-error, isl-condition, or any Gauche error (e.g. from render-format)
  (guard (e
          ((runtime-error? e) #t)
          ((with-module isl.core (isl-condition? e)) #t)
          ((condition? e) #t)   ; plain Gauche condition (from render-format etc.)
          (else (error "expected some error" label e)))
    (thunk)
    (error "expected error but got value" label)))

;; ---- 8-A: format destination handling ----

;; (format nil ...) returns a string
(assert-equal "42"
              (run-forms '((format nil "~a" 42)))
              "8-A: format nil ~a number → string")

(assert-equal "hello"
              (run-forms '((format nil "~a" "hello")))
              "8-A: format nil ~a string → string")

(assert-equal "(1 2 3)"
              (run-forms '((format nil "~a" '(1 2 3))))
              "8-A: format nil ~a list → string")

;; ~s uses write representation (quoted strings)
(assert-equal "\"hello\""
              (run-forms '((format nil "~s" "hello")))
              "8-A: format nil ~s string → quoted string")

(assert-equal "'foo"
              (run-forms '((format nil "~s" '(quote foo))))
              "8-A: format nil ~s quoted symbol → string")

;; ~% inserts newline
(assert-equal "line1\nline2"
              (run-forms '((format nil "line1~%line2")))
              "8-A: format nil ~% → newline")

;; ~~ inserts literal tilde
(assert-equal "~"
              (run-forms '((format nil "~~")))
              "8-A: format nil ~~ → tilde")

;; multiple args
(assert-equal "1 and 2"
              (run-forms '((format nil "~a and ~a" 1 2)))
              "8-A: format nil multiple args")

;; (format t ...) writes to stdout and returns nil
(assert-equal '()
              (run-forms '((format t "~a" 42)))
              "8-A: format t returns nil")

;; (format stream ...) writes to a stream and returns nil
(assert-equal '()
              (run-forms '((let ((s (open-output-stream "/tmp/isl-p8-test.txt")))
                             (format s "hello ~a" 99)
                             (close s))))
              "8-A: format stream returns nil")

;; verify stream content was written
(assert-equal "hello 99"
              (run-forms '((let ((s (open-input-stream "/tmp/isl-p8-test.txt")))
                             (let ((content (read-line s)))
                               (close s)
                               content))))
              "8-A: format stream writes content correctly")

;; ---- 8-A: format directives ----

;; ~d — decimal integer
(assert-equal "255"
              (run-forms '((format nil "~d" 255)))
              "8-A: ~d decimal")

;; ~x — hexadecimal
(assert-equal "ff"
              (run-forms '((format nil "~x" 255)))
              "8-A: ~x hexadecimal")

;; ~o — octal
(assert-equal "377"
              (run-forms '((format nil "~o" 255)))
              "8-A: ~o octal")

;; ~b — binary
(assert-equal "1010"
              (run-forms '((format nil "~b" 10)))
              "8-A: ~b binary")

;; ~c — character
(assert-equal "A"
              (run-forms '((format nil "~c" #\A)))
              "8-A: ~c character")

;; ~i — ignore argument (Phase 8 new directive)
(assert-equal "hello"
              (run-forms '((format nil "~ihello" 42)))
              "8-A: ~i ignores one arg")

(assert-equal "ab"
              (run-forms '((format nil "~ia~ib" 1 2)))
              "8-A: ~i ignores two args")

;; ---- 8-A: error cases ----

;; too few arguments
(assert-isl-error
  (lambda () (run-forms '((format nil "~a"))))
  "8-A: format nil ~a with no arg raises error")

;; bad destination
(assert-isl-error
  (lambda () (run-forms '((format 42 "~a" 1))))
  "8-A: format with invalid destination raises error")

;; missing format string
(assert-isl-error
  (lambda () (run-forms '((format nil))))
  "8-A: format with only one arg raises error")

;; ---- 8-A: strict mode ----
;; format nil in strict mode also works (nil = '())
(assert-equal "ok"
              (run-forms '((format nil "ok")) 'strict)
              "8-A: format nil works in strict mode")

(display "runtime Phase8 smoke passed\n")
