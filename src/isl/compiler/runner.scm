(define-module isl.compiler.runner
  (use isl.compiler.frontend)
  (use isl.compiler.runtime)
  (export compile-forms->units
          compile-file->units
          run-units
          run-forms
          run-file))

(select-module isl.compiler.runner)

(define (compile-forms->units forms . maybe-profile)
  (let ((profile (if (null? maybe-profile) 'extended (car maybe-profile))))
    (let ((env (make-frontend-env profile)))
      (frontend-compile-forms forms env))))

(define (compile-file->units path . maybe-profile)
  (let ((profile (if (null? maybe-profile) 'extended (car maybe-profile))))
    (let ((env (make-frontend-env profile)))
      (frontend-compile-file path env))))

(define (run-units units . maybe-profile)
  (let ((profile (if (null? maybe-profile) 'extended (car maybe-profile))))
    (let* ((state (make-runtime-state profile))
           (rv (runtime-load-units! state units)))
      (runtime-value->host rv))))

(define (run-forms forms . maybe-profile)
  (let ((profile (if (null? maybe-profile) 'extended (car maybe-profile))))
    (run-units (compile-forms->units forms profile) profile)))

(define (run-file path . maybe-profile)
  (let ((profile (if (null? maybe-profile) 'extended (car maybe-profile))))
    (run-units (compile-file->units path profile) profile)))
