;; Native (LLVM/AOT) backend smoke test for funcall and lambda values.
;;
;; Covers the fix for: "(funcall <function-value> ...) => unbound variable"
;; in the native backend, plus lambda lifting for lambdas in value position.
;;
;; Part 1 inspects the generated LLVM text (no clang needed).
;; Part 2 compiles+runs via ./bin/islc --run when clang is available.
;;
;; Run from repo root:  gosh test/compiler/native-funcall-smoke.scm

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

(define (assert-contains text needle label)
  (unless (contains? text needle)
    (error "assertion failed (missing)" label needle)))

(define (assert-not-contains text needle label)
  (when (contains? text needle)
    (error "assertion failed (unexpected)" label needle)))

;; ---- Part 1: codegen text ----
(define env (make-frontend-env 'extended))
;; NOTE: this file is read by Gauche's loader, which does not understand #';
;; use (function sq) here.  The #' surface syntax itself is exercised in Part 2
;; below, where islc reads the source through the ISLISP reader.
(define units
  (frontend-compile-forms
   '((defun sq (x) (* x x))
     (print (funcall (function sq) 7))
     (print (funcall (lambda (a b) (+ a (* b b))) 3 4))
     (print ((lambda (x) (* x x)) 9)))
   env))

(define aot-text (ll-units->llvm-aot-module units))

;; lambdas in value position must be outlined into @isl_lambda_N functions
(assert-contains aot-text "@isl_lambda_0" "lambda-outlined")
(assert-contains aot-text "@isl_rt_make_compiled_fun" "lambda-materialized")
;; funcall is a plain call through isl_rt_call; the lambda body must NOT fall
;; back to the unsupported stub.
(assert-not-contains aot-text "lowering pending" "no-unsupported-lambda")

(display "native funcall codegen checks passed\n")

;; ---- Part 2: end-to-end via ./bin/islc --run (needs clang) ----
(define (clang-available?)
  (guard (e (else #f))
    (let ((p (run-process '("clang" "--version") :output :null :error :null :wait #t)))
      (zero? (process-exit-status p)))))

(define (run-native src)
  (let ((tmp "test/tmp-native-funcall.lsp"))
    (call-with-output-file tmp (lambda (p) (display src p)))
    (let* ((out (process-output->string
                 `("./bin/islc" "--run" ,tmp)
                 :error :null)))
      (delete-files tmp)
      (string-trim-both out))))

(if (not (clang-available?))
    (display "native funcall e2e SKIPPED (clang not found)\n")
    (let ((cases
           (list
            (cons "(print (funcall (lambda (x) (* x x)) 7))" "49")
            (cons "(print ((lambda (x) (* x x)) 7))" "49")
            (cons "(defun sq (x) (* x x)) (print (funcall #'sq 7))" "49")
            (cons "(print (funcall #'+ 3 4))" "7")
            (cons "(print (funcall (lambda (a b) (+ a (* b b))) 3 4))" "19"))))
      (for-each
       (lambda (c)
         (let ((got (run-native (car c))))
           (unless (string=? got (cdr c))
             (error "native e2e mismatch" (car c) 'expected (cdr c) 'got got))))
       cases)
      (display "native funcall e2e checks passed\n")))

(display "native funcall smoke passed\n")
