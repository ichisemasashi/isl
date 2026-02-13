(add-load-path "./src")
(use isl.compiler.frontend)

(define (assert-equal expected actual label)
  (unless (equal? expected actual)
    (error "assertion failed" label expected actual)))

(define env (make-frontend-env 'extended))

;; read
(call-with-input-file "examples/hello.lsp"
  (lambda (p)
    (let ((forms (frontend-read-all p)))
      (unless (>= (length forms) 1)
        (error "frontend-read-all returned empty list")))))

;; macroexpand alignment
(frontend-compile-forms
 '((defmacro inc1 (x) (list '+ x 1)))
 env)
(assert-equal
 '(+ 3 1)
 (frontend-macroexpand '(inc1 3) env)
 "macroexpand")

;; normalize core forms
(assert-equal
 '(expr (if (var t) (const 1) (const 2)))
 (frontend-normalize '(if t 1 2))
 "normalize-if")

;; compile pipeline with compile-time macro definition.
(let* ((forms '((defmacro m1a (x) (list '+ x 10))
                (m1a 5)
                (if #f 0 1)))
       (units (frontend-compile-forms forms env)))
  (assert-equal 3 (length units) "unit-count")
  (let ((u2 (list-ref units 1)))
    (assert-equal '(+ 5 10) (cadr (assoc 'expanded (cdr u2))) "expanded-second")))

(display "frontend smoke passed\n")
