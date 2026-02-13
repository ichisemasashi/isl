(add-load-path "./src")
(use isl.compiler.frontend)

(define (assert form label)
  (unless form
    (error "assertion failed" label)))

(define env (make-frontend-env 'strict))
(define units (frontend-compile-forms '((+ 1 2) (setq x (+ 3 4))) env))

(define ir1 (cadr (assoc 'ir (cdr (car units)))))
(define ir2 (cadr (assoc 'ir (cdr (cadr units)))))

(assert (and (pair? ir1)
             (eq? (car ir1) 'expr)
             (pair? (cadr ir1))
             (eq? (caadr ir1) 'effect))
        "top-call-is-effect")

(define setq-payload (caddr (cadr ir2)))
(define setq-entry (car setq-payload))
(define setq-rhs (cadr setq-entry))

(assert (and (pair? ir2)
             (eq? (car ir2) 'expr)
             (pair? (cadr ir2))
             (eq? (caadr ir2) 'special)
             (eq? (cadadr ir2) 'setq)
             (pair? setq-entry)
             (pair? setq-rhs)
             (eq? (car setq-rhs) 'effect))
        "setq-rhs-is-effect")

(display "ir M4 smoke passed\n")
