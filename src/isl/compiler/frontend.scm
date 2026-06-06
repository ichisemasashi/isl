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
  ;; Handle macrolet specially: expand body with local macro bindings.
  (if (and (pair? form) (eq? (car form) 'macrolet) (>= (length form) 2))
      (let ((bindings (cadr form))
            (body     (cddr form)))
        ;; Install local macros in a fresh child env, then expand each body form.
        (let ((menv (make-frame env)))
          (for-each
           (lambda (binding)
             (when (and (list? binding) (>= (length binding) 3) (symbol? (car binding)))
               (let* ((mname  (resolve-binding-symbol (car binding)))
                      (params (cadr binding))
                      (mbody  (cddr binding))
                      (m      (make-macro params mbody menv mname)))
                 (frame-define! menv mname m))))
           bindings)
          ;; Expand body using macroexpand* with the local env (direct call, not eval)
          (let ((expanded (map (lambda (f) (macroexpand* f menv)) body)))
            (if (= (length expanded) 1)
                (car expanded)
                (cons 'progn expanded)))))
      ;; Recursively expand macros at every level (macroexpand-all).  The
      ;; interpreter's `macroexpand` only expands the outermost form, so a
      ;; nested user macro such as (print (my-macro ...)) would otherwise reach
      ;; the IR as a plain call and fail at runtime.  We expand the top form to
      ;; a fixpoint, then walk evaluated subforms — skipping quoted data.
      (fe-expand-all form env)))

;; Expand `form` (and all evaluated subforms) until no macro remains.
;; Uses single-step `macroexpand-1` guarded against errors: a form whose head
;; is a macro is expanded one step and re-walked; a non-macro head is left in
;; place and its subforms are walked.  Guarding keeps malformed input (e.g. a
;; bad let binding) untouched so the downstream IR/runtime reports its own
;; precise error instead of an expansion-time package-resolution failure.
(define (fe-expand-all form env)
  (cond
   ((not (pair? form)) form)
   ((not (symbol? (car form))) (fe-expand-list form env))  ; e.g. ((lambda ..) ..)
   ((eq? (car form) 'quote) form)                          ; never expand quoted data
   ((eq? (car form) 'quasiquote) form)                     ; leave quasiquote as-is
   ((and (eq? (car form) 'macrolet) (>= (length form) 2))
    (frontend-macroexpand form env))                       ; nested macrolet
   (else
    (let ((step (guard (e (else #f))
                  (eval-islisp (list 'macroexpand-1 (list 'quote form)) env))))
      (cond
       ((not step) form)                                   ; expansion errored → leave as-is
       ((equal? step form) (fe-expand-list form env))      ; not a macro → walk subforms
       (else (fe-expand-all step env)))))))                ; macro → re-walk expansion

;; Recursively expand each element of a (possibly improper) list.
(define (fe-expand-list lst env)
  (let loop ((xs lst) (acc '()))
    (cond
     ((null? xs) (reverse acc))
     ((pair? xs) (loop (cdr xs) (cons (fe-expand-all (car xs) env) acc)))
     (else (append (reverse acc) xs)))))   ; dotted tail (rest-param symbol)

(define (frontend-normalize expanded-form)
  (normalize-top-level-form expanded-form))

(define (frontend-lower top-ir)
  (lower-top-level-form top-ir))

(define (compile-time-form? form)
  (and (pair? form)
       (symbol? (car form))
       (memq (car form) '(defmacro defpackage in-package use-package export import intern))))

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
