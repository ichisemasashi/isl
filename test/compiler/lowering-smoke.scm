(add-load-path "./src")
(use srfi-1)
(use isl.compiler.frontend)

(define (assert-true pred label)
  (unless pred
    (error "assertion failed" label)))

(define env (make-frontend-env 'strict))

(define units
  (frontend-compile-forms
   '((defun choose (x)
       (if x
           (+ 1 2)
           (+ 3 4)))
     (choose #t))
   env))

(define ll-fun (cadr (assoc 'll (cdr (car units)))))
(define ll-call (cadr (assoc 'll (cdr (cadr units)))))

(define (collect-terms blocks)
  (map (lambda (b) (cadddr b)) blocks))

(define (collect-instr-rhs blocks)
  (apply append
         (map (lambda (b)
                (map caddr (caddr b)))
              blocks)))

(define (contains-op? xs op)
  (any (lambda (x) (and (pair? x) (eq? (car x) op))) xs))

(assert-true (eq? (car ll-fun) 'll-define-fun) "ll-define-fun-tag")
(let* ((cfg (cadddr ll-fun))
       (blocks (caddr cfg))
       (terms (collect-terms blocks))
       (rhs (collect-instr-rhs blocks)))
  (assert-true (contains-op? terms 'br) "branch-exists")
  (assert-true (contains-op? rhs 'phi) "phi-exists")
  (assert-true (contains-op? rhs 'call) "call-exists"))

(assert-true (eq? (car ll-call) 'll-expr) "ll-expr-tag")

(display "lowering smoke passed\n")
