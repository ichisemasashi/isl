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
    (else
     (list 'special op (map normalize-expr args)))))

(define (normalize-call op args)
  (cons 'call
        (cons (normalize-expr op)
              (map normalize-expr args))))

(define (normalize-expr form)
  (cond
   ((self-evaluating? form) (list 'const form))
   ((symbol? form) (list 'var form))
   ((pair? form)
    (let ((op (car form))
          (args (cdr form)))
      (if (symbol? op)
          (case op
            ((quote if progn lambda) (normalize-special op args))
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
