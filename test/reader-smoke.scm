;; Reader smoke test: ISLISP function-quote  #'foo == (function foo)
;;
;; These cases must be read THROUGH the ISLISP reader (read-all / isl-read),
;; not by Gauche's loader, because Gauche's native reader rejects #'.  We
;; therefore feed source text via string input ports.
;;
;; Run from repo root:  gosh test/reader-smoke.scm

(add-load-path "./src")
(use isl.core)

(define failures 0)

(define (read-one s)
  (car (read-all (open-input-string s))))

(define (read-many s)
  (read-all (open-input-string s)))

(define (check-read label src expected)
  (let ((actual (read-one src)))
    (unless (equal? actual expected)
      (set! failures (+ failures 1))
      (format #t "FAIL ~A: read ~S => ~S, expected ~S~%"
              label src actual expected))))

(define env (make-initial-env))

(define (check-eval label src expected)
  (let ((forms (read-many src)))
    (let loop ((fs forms) (result '()))
      (if (null? fs)
          (unless (equal? result expected)
            (set! failures (+ failures 1))
            (format #t "FAIL ~A: eval ~S => ~S, expected ~S~%"
                    label src result expected))
          (loop (cdr fs) (eval-islisp (car fs) env))))))

;; --- reader structure ---
(check-read "fq-symbol"  "#'foo"            '(function foo))
(check-read "fq-plus"    "#'+"              '(function +))
(check-read "fq-lambda"  "#'(lambda (x) x)" '(function (lambda (x) x)))
(check-read "fq-in-call" "(mapcar #'car xs)" '(mapcar (function car) xs))
(check-read "fq-nested"  "(a #'b (c #'d))"  '(a (function b) (c (function d))))

;; #' must NOT be confused with the character literal #\'
(check-read "char-quote"  "#\\'" #\')
(check-read "char-hash"   "#\\#" #\#)

;; other # syntax still delegates to Gauche's reader
(check-read "vector"  "#(1 2 3)" #(1 2 3))
(check-read "bool-t"  "#t" #t)
(check-read "dotted"  "(1 . 2)" '(1 . 2))

;; quote sugar around #'
(check-read "quote-fq"     "'#'f"  '(quote (function f)))
(check-read "qq-fq"        "`(,#'f)" '(quasiquote ((unquote (function f)))))

;; #' is inert inside strings and comments
(check-read "string"  "\"#'not-a-function\"" "#'not-a-function")
(check-eval "line-comment" "; #'ignored\n(+ 1 2)" 3)
(check-eval "block-comment" "#| #'ignored |# (+ 3 4)" 7)

;; --- evaluation round trips ---
(check-eval "apply-fq"   "(apply #'+ '(1 2 3 4))" 10)
(check-eval "mapcar-fq"  "(mapcar #'car '((1 2) (3 4)))" '(1 3))
(check-eval "funcall-fq" "(progn (defun rs-sq (x) (* x x)) (funcall #'rs-sq 7))" 49)
(check-eval "funcall-lambda-fq" "(funcall #'(lambda (x) (+ x 1)) 41)" 42)

(if (= failures 0)
    (begin (display "reader smoke passed") (newline))
    (begin (format #t "reader smoke FAILED: ~A failure(s)~%" failures)
           (exit 1)))
