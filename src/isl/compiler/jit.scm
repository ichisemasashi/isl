(define-module isl.compiler.jit
  (use isl.compiler.frontend)
  (use isl.compiler.runtime)
  (export make-jit-state
          jit-eval-forms
          jit-run-file))

(select-module isl.compiler.jit)

(define (jit-frontend-env st) (vector-ref st 1))
(define (jit-runtime-state st) (vector-ref st 2))

(define (make-jit-state . maybe-profile)
  (let ((profile (if (null? maybe-profile) 'extended (car maybe-profile))))
    (vector 'jit-state
            (make-frontend-env profile)
            (make-runtime-state profile))))

(define (jit-eval-forms st forms)
  (let* ((fenv (jit-frontend-env st))
         (rstate (jit-runtime-state st))
         (units (frontend-compile-forms forms fenv))
         (rv (runtime-load-units! rstate units)))
    (runtime-value->host rv)))

(define (jit-run-file st path)
  (let* ((fenv (jit-frontend-env st))
         (rstate (jit-runtime-state st))
         (units (frontend-compile-file path fenv))
         (rv (runtime-load-units! rstate units)))
    (runtime-value->host rv)))
