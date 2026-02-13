(define-module isl.compiler.ir
  (export normalize-expr normalize-top-level-form render-ir))

(select-module isl.compiler.ir)

(define (self-evaluating? x)
  (or (number? x)
      (string? x)
      (char? x)
      (boolean? x)
      (null? x)
      (keyword? x)
      (vector? x)))

(define (normalize-body forms)
  (cond
   ((null? forms) '(const ()))
   ((null? (cdr forms)) (normalize-expr (car forms)))
   (else
    (cons 'seq (map normalize-expr forms)))))

(define (normalize-setq-args args)
  (if (or (null? args) (null? (cdr args)))
      #f
      (let loop ((xs args) (acc '()))
        (cond
         ((null? xs) (reverse acc))
         ((or (null? (cdr xs)) (not (symbol? (car xs)))) #f)
         (else
          (loop (cddr xs)
                (cons (list (car xs) (normalize-expr (cadr xs))) acc)))))))

(define (normalize-let-bindings raw)
  (if (not (list? raw))
      #f
      (let loop ((xs raw) (acc '()))
        (cond
         ((null? xs) (reverse acc))
         ((symbol? (car xs))
          (loop (cdr xs) (cons (list (car xs) '(const ())) acc)))
         ((and (pair? (car xs))
               (symbol? (caar xs))
               (or (null? (cdar xs))
                   (and (pair? (cdar xs)) (null? (cddar xs)))))
          (let* ((entry (car xs))
                 (name (car entry))
                 (init (if (null? (cdr entry)) '(const ()) (normalize-expr (cadr entry)))))
            (loop (cdr xs) (cons (list name init) acc))))
         (else #f)))))

(define (normalize-place place)
  (if (and (pair? place)
           (symbol? (car place))
           (eq? (car place) 'vector-ref)
           (= (length place) 3))
      (list 'place-vector-ref
            (normalize-expr (cadr place))
            (normalize-expr (caddr place)))
      #f))

(define (normalize-tagbody-items items)
  (let loop ((xs items) (acc '()))
    (if (null? xs)
        (reverse acc)
        (let ((x (car xs)))
          (if (or (symbol? x) (integer? x))
              (loop (cdr xs) (cons (list 'label x) acc))
              (loop (cdr xs) (cons (list 'form (normalize-expr x)) acc)))))))

(define (normalize-handler-clause clause)
  (if (and (list? clause) (>= (length clause) 2))
      (let ((tag (car clause))
            (rest (cdr clause))
            (var #f))
        (if (not (symbol? tag))
            #f
            (begin
              (when (and (pair? rest) (list? (car rest)))
                (let ((vs (car rest)))
                  (if (or (null? vs)
                          (and (= (length vs) 1) (symbol? (car vs))))
                      (begin
                        (set! var (and (pair? vs) (car vs)))
                        (set! rest (cdr rest)))
                      (set! rest #f))))
              (if (not rest)
                  #f
                  (list tag var (normalize-body rest))))))
      #f))

(define (normalize-special op args)
  (case op
    ((quote)
     (if (= (length args) 1)
         (list 'const (car args))
         (list 'invalid-special op args)))
    ((if)
     (let ((test (if (null? args) '() (car args)))
           (then (if (or (null? args) (null? (cdr args))) '() (cadr args)))
           (else-form (if (or (null? args) (null? (cdr args)) (null? (cddr args))) '() (caddr args))))
       (list 'if
             (normalize-expr test)
             (normalize-expr then)
             (normalize-expr else-form))))
    ((progn)
     (normalize-body args))
    ((lambda)
     (if (>= (length args) 2)
         (list 'lambda (car args) (normalize-body (cdr args)))
         (list 'invalid-special op args)))
    ((setq)
     (let ((pairs (normalize-setq-args args)))
       (if pairs
           (list 'special 'setq pairs)
           (list 'invalid-special op args))))
    ((let)
     (if (null? args)
         (list 'invalid-special op args)
         (let ((bindings (normalize-let-bindings (car args))))
           (if bindings
               (list 'special 'let
                     (list bindings
                           (normalize-body (cdr args))))
               (list 'invalid-special op args)))))
    ((setf)
     (if (= (length args) 2)
         (let ((place (normalize-place (car args))))
           (if place
               (list 'special 'setf
                     (list place (normalize-expr (cadr args))))
               (list 'invalid-special op args)))
         (list 'invalid-special op args)))
    ((block)
     (if (>= (length args) 1)
         (let ((name (car args)))
           (if (symbol? name)
               (list 'special 'block
                     (list name (normalize-body (cdr args))))
               (list 'invalid-special op args)))
         (list 'invalid-special op args)))
    ((return-from)
     (if (or (= (length args) 1) (= (length args) 2))
         (let ((name (car args)))
           (if (symbol? name)
               (list 'special 'return-from
                     (list name
                           (if (= (length args) 2)
                               (normalize-expr (cadr args))
                               '(const ()))))
               (list 'invalid-special op args)))
         (list 'invalid-special op args)))
    ((catch)
     (if (>= (length args) 1)
         (list 'special 'catch
               (list (normalize-expr (car args))
                     (normalize-body (cdr args))))
         (list 'invalid-special op args)))
    ((throw)
     (if (= (length args) 2)
         (list 'special 'throw
               (list (normalize-expr (car args))
                     (normalize-expr (cadr args))))
         (list 'invalid-special op args)))
    ((go)
     (if (= (length args) 1)
         (let ((tag (car args)))
           (if (or (symbol? tag) (integer? tag))
               (list 'special 'go (list tag))
               (list 'invalid-special op args)))
         (list 'invalid-special op args)))
    ((tagbody)
     (list 'special 'tagbody (normalize-tagbody-items args)))
    ((handler-case)
     (if (>= (length args) 2)
         (let ((protected (normalize-expr (car args)))
               (clauses (map normalize-handler-clause (cdr args))))
           (if (let loop ((xs clauses))
                 (and (pair? xs)
                      (or (not (car xs)) (loop (cdr xs)))))
               (list 'invalid-special op args)
               (list 'special 'handler-case (list protected clauses))))
         (list 'invalid-special op args)))
    (else
     (list 'special op (map normalize-expr args)))))

(define (normalize-call op args)
  ;; M4: side-effect-capable operation is wrapped by explicit effect barrier.
  (list 'effect
        (cons 'call
              (cons (normalize-expr op)
                    (map normalize-expr args)))))

(define (normalize-expr form)
  (cond
   ((self-evaluating? form) (list 'const form))
   ((symbol? form) (list 'var form))
   ((pair? form)
    (let ((op (car form))
          (args (cdr form)))
      (if (symbol? op)
          (case op
            ((quote if progn lambda setq let setf block return-from catch throw go tagbody handler-case)
             (normalize-special op args))
            (else (normalize-call op args)))
          (normalize-call op args))))
   (else
    (list 'const form))))

(define (normalize-top-level-form form)
  (if (and (pair? form) (symbol? (car form)))
      (let ((op (car form))
            (args (cdr form)))
        (case op
          ((defun)
           (if (>= (length args) 3)
               (list 'define-fun (car args) (cadr args) (normalize-body (cddr args)))
               (list 'invalid-top form)))
          ((defglobal)
           (if (= (length args) 2)
               (list 'define-global (car args) (normalize-expr (cadr args)))
               (list 'invalid-top form)))
          ((defmacro)
           (if (>= (length args) 3)
               (list 'define-macro (car args) (cadr args) (cddr args))
               (list 'invalid-top form)))
          (else
           (list 'expr (normalize-expr form)))))
      (list 'expr (normalize-expr form))))

(define (render-ir ir)
  (let ((p (open-output-string)))
    (write ir p)
    (get-output-string p)))
