(define-module isl.main
  (use isl.core)
  (export main))

(select-module isl.main)

(define (run-file path env)
  (call-with-input-file path
    (lambda (p)
      (for-each (lambda (form)
                  (eval-islisp form env))
                (read-all p)))))

(define (main args)
  (let ((env (make-initial-env))
        (rest (cdr args)))
    (if (null? rest)
        (begin
          (display "ISLISP interpreter (Gauche 0.9.15)\n")
          (display "Ctrl-D to exit.\n")
          (repl env)
          0)
        (begin
          (for-each (lambda (path)
                      (run-file path env))
                    rest)
          0))))
