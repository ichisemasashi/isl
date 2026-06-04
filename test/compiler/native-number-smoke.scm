;; Native (LLVM/AOT) backend smoke test for the exact-rational numeric tower.
;;
;; (/ 7 2) must yield the exact rational 7/2 (not integer-truncated 3), and
;; rational arithmetic must reduce and collapse to integers where appropriate.
;;
;; Run from repo root:  gosh test/compiler/native-number-smoke.scm

(add-load-path "./src")
(use gauche.process)
(use file.util)
(use srfi.13)

(define (clang-available?)
  (guard (e (else #f))
    (let ((p (run-process '("clang" "--version") :output :null :error :null :wait #t)))
      (zero? (process-exit-status p)))))

(define (run-native src)
  (let ((tmp "test/tmp-native-number.lsp"))
    (call-with-output-file tmp (lambda (p) (display src p)))
    (let ((out (process-output->string `("./bin/islc" "--run" ,tmp) :error :null)))
      (delete-files tmp)
      (string-trim-both out))))

(if (not (clang-available?))
    (display "native number e2e SKIPPED (clang not found)\n")
    (let ((cases
           (list
            (cons "(print (/ 7 2))" "7/2")        ; exact rational, not 3
            (cons "(print (/ 6 2))" "3")          ; collapses to int
            (cons "(print (/ 1 3))" "1/3")
            (cons "(print (/ 5))" "1/5")          ; unary reciprocal
            (cons "(print (+ (/ 1 2) (/ 1 3)))" "5/6")
            (cons "(print (- (/ 1 2) (/ 1 6)))" "1/3")
            (cons "(print (* (/ 2 3) (/ 3 4)))" "1/2")
            (cons "(print (* 2 (/ 3 2)))" "3")    ; collapses to int
            (cons "(print (+ (/ 1 2) (/ 1 2)))" "1")
            (cons "(print (/ 10 4))" "5/2")       ; reduced
            (cons "(print (< (/ 1 3) (/ 1 2)))" "#t")
            (cons "(print (= (/ 2 4) (/ 1 2)))" "#t")
            (cons "(print (eql (/ 1 2) (/ 1 2)))" "#t")
            (cons "(print (- (/ 3 4)))" "-3/4"))))
      (for-each
       (lambda (c)
         (let ((got (run-native (car c))))
           (unless (string=? got (cdr c))
             (error "native number e2e mismatch" (car c) 'expected (cdr c) 'got got))))
       cases)
      (display "native number e2e checks passed\n")))

(display "native number smoke passed\n")
