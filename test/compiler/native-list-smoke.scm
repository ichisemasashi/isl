;; Native (LLVM/AOT) backend smoke test for cons/list values and apply.
;;
;; Part 1 inspects the generated LLVM text (no clang needed).
;; Part 2 compiles+runs via ./bin/islc --run when clang is available.
;;
;; Run from repo root:  gosh test/compiler/native-list-smoke.scm

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
;; A quoted list literal must be built with isl_rt_cons, not fall back to the
;; "unsupported const" stub.
(define env (make-frontend-env 'extended))
(define units
  (frontend-compile-forms
   '((print '(1 2 3))
     (print (apply (function +) (list 1 2 3))))
   env))
(define aot-text (ll-units->llvm-aot-module units))

(assert-contains aot-text "@isl_rt_cons" "quoted-list-built-with-cons")
(assert-not-contains aot-text "unsupported const" "no-unsupported-const")

(display "native list codegen checks passed\n")

;; ---- Part 2: end-to-end via ./bin/islc --run (needs clang) ----
(define (clang-available?)
  (guard (e (else #f))
    (let ((p (run-process '("clang" "--version") :output :null :error :null :wait #t)))
      (zero? (process-exit-status p)))))

(define (run-native src)
  (let ((tmp "test/tmp-native-list.lsp"))
    (call-with-output-file tmp (lambda (p) (display src p)))
    (let ((out (process-output->string `("./bin/islc" "--run" ,tmp) :error :null)))
      (delete-files tmp)
      (string-trim-both out))))

(if (not (clang-available?))
    (display "native list e2e SKIPPED (clang not found)\n")
    (let ((cases
           (list
            (cons "(print (cons 1 2))" "(1 . 2)")
            (cons "(print (list 1 2 3))" "(1 2 3)")
            (cons "(print '(1 (2 3) 4))" "(1 (2 3) 4)")
            (cons "(print (car (list 7 8 9)))" "7")
            (cons "(print (cdr (list 7 8 9)))" "(8 9)")
            (cons "(print (consp (list 1)))" "#t")
            (cons "(print (null '()))" "#t")
            (cons "(print (apply #'+ (list 1 2 3 4)))" "10")
            (cons "(print (apply #'+ 1 2 (list 3 4)))" "10")
            (cons "(print (apply #'+ '(10 20 30)))" "60")
            (cons "(print (apply (lambda (a b c) (+ a (* b c))) (list 1 2 3)))" "7"))))
      (for-each
       (lambda (c)
         (let ((got (run-native (car c))))
           (unless (string=? got (cdr c))
             (error "native list e2e mismatch" (car c) 'expected (cdr c) 'got got))))
       cases)
      (display "native list e2e checks passed\n")))

(display "native list smoke passed\n")
