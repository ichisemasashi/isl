(define-module isl.compiler.frontend
  (use isl.core)
  (use isl.compiler.ir)
  (use isl.compiler.lowering)
  (export make-frontend-env
          frontend-read-all
          frontend-macroexpand
          frontend-normalize
          frontend-lower
          frontend-compile-forms
          frontend-compile-file
          frontend-dump))

(select-module isl.compiler.frontend)

(define (make-frontend-env . maybe-profile)
  (if (null? maybe-profile)
      (make-initial-env)
      (make-initial-env (car maybe-profile))))

(define (frontend-read-all port)
  (read-all port))

(define (frontend-macroexpand form env)
  ;; Reuse interpreter macro expansion to keep semantics aligned.
  (eval-islisp (list 'macroexpand (list 'quote form)) env))

(define (frontend-normalize expanded-form)
  (normalize-top-level-form expanded-form))

(define (frontend-lower top-ir)
  (lower-top-level-form top-ir))

(define (compile-time-form? form)
  (and (pair? form)
       (symbol? (car form))
       (memq (car form) '(defmacro defpackage in-package))))

(define (frontend-compile-forms forms env)
  (let loop ((xs forms) (acc '()))
    (if (null? xs)
        (reverse acc)
        (let* ((form (car xs))
               (expanded (frontend-macroexpand form env))
               (ir (frontend-normalize expanded))
               (ll (frontend-lower ir)))
          ;; Keep compile-time environment synchronized for subsequent expansions.
          (when (compile-time-form? form)
            (eval-islisp form env))
          (loop (cdr xs)
                (cons (list 'unit
                            (list 'source form)
                            (list 'expanded expanded)
                            (list 'ir ir)
                            (list 'll ll))
                      acc))))))

(define (frontend-compile-file path env)
  (call-with-input-file path
    (lambda (p)
      (frontend-compile-forms (frontend-read-all p) env))))

(define (frontend-dump units . maybe-port)
  (let ((port (if (null? maybe-port) (current-output-port) (car maybe-port))))
    (let loop ((xs units) (idx 1))
      (unless (null? xs)
        (let* ((u (car xs))
               (src (cadr (assoc 'source (cdr u))))
               (expanded (cadr (assoc 'expanded (cdr u))))
               (ir (cadr (assoc 'ir (cdr u))))
               (ll (cadr (assoc 'll (cdr u)))))
          (display "[" port)
          (display idx port)
          (display "] source: " port)
          (write src port)
          (newline port)
          (display "    expanded: " port)
          (write expanded port)
          (newline port)
          (display "    ir: " port)
          (display (render-ir ir) port)
          (newline port)
          (display "    ll: " port)
          (display (render-ll ll) port)
          (newline port))
        (loop (cdr xs) (+ idx 1))))))
