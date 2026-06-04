;; Native (LLVM/AOT) backend smoke test for control/binding forms and I/O:
;; cond, let, let*, eq, and format/standard-output.
;;
;; Also runs the textbook calculator end-to-end on the native backend.
;;
;; Run from repo root:  gosh test/compiler/native-control-smoke.scm

(add-load-path "./src")
(use isl.compiler.frontend)
(use isl.compiler.codegen)
(use gauche.process)
(use file.util)
(use srfi.13)

(define (contains? text needle)
  (let ((n (string-length text)) (m (string-length needle)))
    (let loop ((i 0))
      (cond ((> (+ i m) n) #f)
            ((string=? (substring text i (+ i m)) needle) #t)
            (else (loop (+ i 1)))))))

(define (assert-not-contains text needle label)
  (when (contains? text needle)
    (error "assertion failed (unexpected)" label needle)))

;; ---- Part 1: codegen text ----
;; cond/let must be lowered to core IR, never left as an unsupported stub.
(define env (make-frontend-env 'extended))
(define units
  (frontend-compile-forms
   '((print (cond ((= 1 2) 10) (t 20)))
     (print (let ((a 1) (b 2)) (+ a b))))
   env))
(define aot-text (ll-units->llvm-aot-module units))
(assert-not-contains aot-text "lowering pending" "cond/let-not-lowered")

(display "native control codegen checks passed\n")

;; ---- Part 2: end-to-end via ./bin/islc --run (needs clang) ----
(define (clang-available?)
  (guard (e (else #f))
    (let ((p (run-process '("clang" "--version") :output :null :error :null :wait #t)))
      (zero? (process-exit-status p)))))

(define (run-native-src src)
  (let ((tmp "test/tmp-native-control.lsp"))
    (call-with-output-file tmp (lambda (p) (display src p)))
    (let ((out (process-output->string `("./bin/islc" "--run" ,tmp) :error :null)))
      (delete-files tmp)
      (string-trim-both out))))

(define (run-native-file path)
  (string-trim-both (process-output->string `("./bin/islc" "--run" ,path) :error :null)))

(if (not (clang-available?))
    (display "native control e2e SKIPPED (clang not found)\n")
    (begin
      (let ((cases
             (list
              (cons "(print (cond ((= 1 2) 10) (t 20)))" "20")
              (cons "(print (cond ((= 1 2) 10) ((= 3 3) 30) (t 99)))" "30")
              (cons "(print (cond ((= 1 2) 10)))" "nil")
              (cons "(print (cond (7)))" "7")
              (cons "(print (let ((x 5)) (* x x)))" "25")
              (cons "(print (let ((a 1) (b 2)) (+ a b)))" "3")
              (cons "(print (let* ((x 1) (y (+ x 1))) (+ x y)))" "3")
              (cons "(print (let () 42))" "42")
              (cons "(print (eq 'a 'a))" "#t")
              (cons "(print (eq 'a 'b))" "#f")
              (cons "(print (eql 2 2))" "#t")
              (cons "(format (standard-output) \"~S => ~A~%\" '(+ 1 2) 3)" "(+ 1 2) => 3"))))
        (for-each
         (lambda (c)
           (let ((got (run-native-src (car c))))
             (unless (string=? got (cdr c))
               (error "native control e2e mismatch" (car c) 'expected (cdr c) 'got got))))
         cases))
      ;; The textbook calculator must run to completion on the native backend.
      (let ((out (run-native-file
                  "docs/textBook/part1-core/02-basic/code/isl-calc-v1.lsp")))
        (for-each
         (lambda (needle)
           (unless (contains? out needle)
             (error "textbook calculator native output missing line" needle 'full out)))
         '("(+ 1 2) => 3"
           "(+ 1 (* 2 3)) => 7"
           "(* (+ 1 2) (- 9 4)) => 15"
           "(+ 1 2 3 4) => 10"
           "(- 5) => -5")))
      (display "native control e2e checks passed\n")))

(display "native control smoke passed\n")
