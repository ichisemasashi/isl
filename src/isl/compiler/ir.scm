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
  (cond
   ((not (pair? place)) #f)
   ((not (symbol? (car place))) #f)
   ((and (eq? (car place) 'vector-ref) (= (length place) 3))
    (list 'place-vector-ref
          (normalize-expr (cadr place))
          (normalize-expr (caddr place))))
   ((and (eq? (car place) 'general-vector-ref) (= (length place) 3))
    (list 'place-general-vector-ref
          (normalize-expr (cadr place))
          (normalize-expr (caddr place))))
   ((and (eq? (car place) 'aref) (= (length place) 3))
    (list 'place-aref
          (normalize-expr (cadr place))
          (normalize-expr (caddr place))))
   ((and (eq? (car place) 'car) (= (length place) 2))
    (list 'place-car
          (normalize-expr (cadr place))))
   ((and (eq? (car place) 'cdr) (= (length place) 2))
    (list 'place-cdr
          (normalize-expr (cadr place))))
   ((and (eq? (car place) 'string-ref) (= (length place) 3))
    (list 'place-string-ref
          (normalize-expr (cadr place))
          (normalize-expr (caddr place))))
   (else #f)))

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

(define (all-truthy? xs)
  (if (null? xs)
      #t
      (and (car xs) (all-truthy? (cdr xs)))))

(define (normalize-defclass-slot slot-spec)
  (cond
   ((symbol? slot-spec)
    (list 'slot-spec
          slot-spec
          '(const ())
          (string->symbol (string-append ":" (symbol->string slot-spec)))
          '()
          '()))
   ((and (list? slot-spec) (pair? slot-spec) (symbol? (car slot-spec)))
    (let ((name (car slot-spec))
          (rest (cdr slot-spec))
          (initform '(const ()))
          (initarg #f)
          (readers '())
          (writers '()))
      (let parse ((xs rest))
        (cond
         ((null? xs)
          (list 'slot-spec
                name
                initform
                (if initarg
                    initarg
                    (string->symbol (string-append ":" (symbol->string name))))
                (reverse readers)
                (reverse writers)))
         ((or (null? (cdr xs))
              (not (or (symbol? (car xs)) (keyword? (car xs)))))
          #f)
         (else
          (let ((k (car xs))
                (v (cadr xs)))
            (cond
             ((eq? k ':initform)
              (set! initform (normalize-expr v))
              (parse (cddr xs)))
             ((eq? k ':accessor)
              (if (symbol? v)
                  (begin
                    (set! readers (cons v readers))
                    (set! writers (cons v writers))
                    (parse (cddr xs)))
                  #f))
             ((eq? k ':reader)
              (if (symbol? v)
                  (begin
                    (set! readers (cons v readers))
                    (parse (cddr xs)))
                  #f))
             ((eq? k ':writer)
              (if (symbol? v)
                  (begin
                    (set! writers (cons v writers))
                    (parse (cddr xs)))
                  #f))
             ((eq? k ':initarg)
              (if (or (symbol? v) (keyword? v))
                  (begin
                    (set! initarg v)
                    (parse (cddr xs)))
                  #f))
             (else #f))))))))
   (else #f)))

(define (normalize-defclass-payload args)
  (if (= (length args) 3)
      (let ((name (car args))
            (supers (cadr args))
            (slots (caddr args)))
        (if (and (symbol? name) (list? supers) (list? slots))
            (let ((norm-slots (map normalize-defclass-slot slots)))
              (if (all-truthy? norm-slots)
                  (list name supers norm-slots)
                  #f))
            #f))
      #f))

(define (normalize-defgeneric-payload args)
  (if (and (= (length args) 2) (symbol? (car args)) (list? (cadr args)))
      (list (car args) (cadr args))
      #f))

(define (normalize-defmethod-param p)
  (cond
   ((symbol? p) p)
   ((and (list? p) (= (length p) 2) (symbol? (car p)) (symbol? (cadr p))) p)
   (else #f)))

(define (keyword-symbol-ir? x)
  (and (symbol? x)
       (> (string-length (symbol->string x)) 0)
       (char=? (string-ref (symbol->string x) 0) #\:)))

(define (normalize-defmethod-payload args)
  (if (>= (length args) 3)
      (let* (;; Optional qualifier: :before, :after, :around
             (has-qual? (and (>= (length args) 4)
                             (keyword-symbol-ir? (cadr args))
                             (memq (cadr args) '(:before :after :around))))
             (qualifier (if has-qual? (cadr args) #f))
             (rest (if has-qual? (cddr args) (cdr args)))
             (name (car args))
             (params (car rest))
             (body (cdr rest)))
        (if (and (symbol? name) (list? params))
            (let ((norm-params (map normalize-defmethod-param params)))
              (if (all-truthy? norm-params)
                  (list name qualifier norm-params (normalize-body body))
                  #f))
            #f))
      #f))

;; ---- normalize-special: 共有ヘルパ（旧 internal define を持ち上げ） ----
(define (normalize-with-open-file spec body-forms)
  (if (and (list? spec) (>= (length spec) 2) (symbol? (car spec)))
      (let ((var (car spec))
            (path-form (cadr spec))
            (rest (cddr spec)))
        (let loop ((xs rest) (acc '()))
          (cond
           ((null? xs)
            (list 'special 'with-open-file
                  (list var
                        (normalize-expr path-form)
                        (reverse acc)
                        (normalize-body body-forms))))
           ((or (null? (cdr xs))
                (not (or (symbol? (car xs)) (keyword? (car xs)))))
            (list 'invalid-special op args))
           (else
            (loop (cddr xs)
                  (cons (list (car xs) (normalize-expr (cadr xs))) acc))))))
      (list 'invalid-special op args)))
(define (normalize-cond-clauses clauses)
  (if (not (list? clauses))
      #f
      (let loop ((xs clauses) (acc '()))
        (cond
         ((null? xs) (reverse acc))
         ((or (not (list? (car xs))) (null? (car xs)))
          #f)
         (else
          (let ((clause (car xs)))
            (loop (cdr xs)
                  (cons (list (normalize-expr (car clause))
                              (if (null? (cdr clause))
                                  #f
                                  (normalize-body (cdr clause))))
                        acc))))))))
(define (normalize-dolist args*)
  (if (null? args*)
      (list 'invalid-special op args)
      (let ((spec (car args*))
            (body (cdr args*)))
        (if (and (list? spec)
                 (or (= (length spec) 2) (= (length spec) 3))
                 (symbol? (car spec)))
            (list 'special 'dolist
                  (list (car spec)
                        (normalize-expr (cadr spec))
                        (if (= (length spec) 3)
                            (normalize-expr (caddr spec))
                            #f)
                        (normalize-body body)))
            (list 'invalid-special op args)))))

;; ---- normalize-special: 特殊形式ごとの正規化（旧 case 枝を抽出） ----
(define (normalize-special-quote op args)
     (if (= (length args) 1)
         (list 'const (car args))
         (list 'invalid-special op args)))

(define (normalize-special-if op args)
     (let ((test (if (null? args) '() (car args)))
           (then (if (or (null? args) (null? (cdr args))) '() (cadr args)))
           (else-form (if (or (null? args) (null? (cdr args)) (null? (cddr args))) '() (caddr args))))
       (list 'if
             (normalize-expr test)
             (normalize-expr then)
             (normalize-expr else-form))))

(define (normalize-special-cond op args)
     (let ((clauses (normalize-cond-clauses args)))
       (if clauses
           (list 'special 'cond clauses)
           (list 'invalid-special op args))))

(define (normalize-special-and op args)
     (list 'special 'and (map normalize-expr args)))

(define (normalize-special-or op args)
     (list 'special 'or (map normalize-expr args)))

(define (normalize-special-progn op args)
     (normalize-body args))

(define (normalize-special-lambda op args)
     (if (>= (length args) 2)
         (list 'lambda (car args) (normalize-body (cdr args)))
         (list 'invalid-special op args)))

(define (normalize-special-setq op args)
     (let ((pairs (normalize-setq-args args)))
       (if pairs
           (list 'special 'setq pairs)
           (list 'invalid-special op args))))

(define (normalize-special-let op args)
     (if (null? args)
         (list 'invalid-special op args)
         (let ((bindings (normalize-let-bindings (car args))))
           (if bindings
               (list 'special 'let
                     (list bindings
                           (normalize-body (cdr args))))
               (list 'invalid-special op args)))))

(define (normalize-special-let* op args)
     (if (null? args)
         (list 'invalid-special op args)
         (let ((bindings (normalize-let-bindings (car args))))
           (if bindings
               (list 'special 'let*
                     (list bindings
                           (normalize-body (cdr args))))
               (list 'invalid-special op args)))))

(define (normalize-special-setf op args)
     (if (= (length args) 2)
         (let ((place (normalize-place (car args))))
           (if place
               (list 'special 'setf
                     (list place (normalize-expr (cadr args))))
               (list 'invalid-special op args)))
         (list 'invalid-special op args)))

(define (normalize-special-block op args)
     (if (>= (length args) 1)
         (let ((name (car args)))
           (if (symbol? name)
               (list 'special 'block
                     (list name (normalize-body (cdr args))))
               (list 'invalid-special op args)))
         (list 'invalid-special op args)))

(define (normalize-special-return-from op args)
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

(define (normalize-special-catch op args)
     (if (>= (length args) 1)
         (list 'special 'catch
               (list (normalize-expr (car args))
                     (normalize-body (cdr args))))
         (list 'invalid-special op args)))

(define (normalize-special-throw op args)
     (if (= (length args) 2)
         (list 'special 'throw
               (list (normalize-expr (car args))
                     (normalize-expr (cadr args))))
         (list 'invalid-special op args)))

(define (normalize-special-go op args)
     (if (= (length args) 1)
         (let ((tag (car args)))
           (if (or (symbol? tag) (integer? tag))
               (list 'special 'go (list tag))
               (list 'invalid-special op args)))
         (list 'invalid-special op args)))

(define (normalize-special-tagbody op args)
     (list 'special 'tagbody (normalize-tagbody-items args)))

(define (normalize-special-with-open-file op args)
     (if (and (pair? args) (pair? (cdr args)))
         (normalize-with-open-file (car args) (cdr args))
         (list 'invalid-special op args)))

(define (normalize-special-dolist op args)
     (normalize-dolist args))

(define (normalize-special-while op args)
     (if (>= (length args) 1)
         (list 'special 'while
               (list (normalize-expr (car args))
                     (normalize-body (cdr args))))
         (list 'invalid-special op args)))

(define (normalize-special-handler-case op args)
     (if (>= (length args) 2)
         (let ((protected (normalize-expr (car args)))
               (clauses (map normalize-handler-clause (cdr args))))
           (if (let loop ((xs clauses))
                 (and (pair? xs)
                      (or (not (car xs)) (loop (cdr xs)))))
               (list 'invalid-special op args)
               (list 'special 'handler-case (list protected clauses))))
         (list 'invalid-special op args)))

(define (normalize-special-defpackage op args)
     (if (>= (length args) 1)
         (list 'special 'defpackage args)
         (list 'invalid-special op args)))

(define (normalize-special-in-package op args)
     (if (= (length args) 1)
         (list 'special 'in-package args)
         (list 'invalid-special op args)))

(define (normalize-special-defclass op args)
     (let ((p (normalize-defclass-payload args)))
       (if p
           (list 'special 'defclass p)
           (list 'invalid-special op args))))

(define (normalize-special-defgeneric op args)
     (let ((p (normalize-defgeneric-payload args)))
       (if p
           (list 'special 'defgeneric p)
           (list 'invalid-special op args))))

(define (normalize-special-defmethod op args)
     (let ((p (normalize-defmethod-payload args)))
       (if p
           (list 'special 'defmethod p)
           (list 'invalid-special op args))))

(define (normalize-special-flet-labels op args)
     ;; (flet/labels ((name params body...) ...) body...)
     (if (and (>= (length args) 2) (list? (car args)))
         (let* ((recursive? (eq? op 'labels))
                (raw-bindings (car args))
                (body (cdr args))
                (norm-bindings
                 (map (lambda (b)
                        (if (and (list? b) (>= (length b) 3) (symbol? (car b)) (list? (cadr b)))
                            (list (car b) (cadr b) (normalize-body (cddr b)))
                            #f))
                      raw-bindings)))
           (if (any not norm-bindings)
               (list 'invalid-special op args)
               (list 'special op (list norm-bindings (normalize-body body)))))
         (list 'invalid-special op args)))

(define (normalize-special-macrolet op args)
     ;; (macrolet ((name params body...) ...) body...)
     ;; Treated like flet but macros are local — expand at runtime as closures
     (if (and (>= (length args) 2) (list? (car args)))
         (let* ((raw-bindings (car args))
                (body (cdr args))
                (norm-bindings
                 (map (lambda (b)
                        (if (and (list? b) (>= (length b) 3) (symbol? (car b)) (list? (cadr b)))
                            (list (car b) (cadr b) (normalize-body (cddr b)))
                            #f))
                      raw-bindings)))
           (if (any not norm-bindings)
               (list 'invalid-special op args)
               (list 'special 'macrolet (list norm-bindings (normalize-body body)))))
         (list 'invalid-special op args)))

(define (normalize-special-the op args)
     ;; (the class-name form)
     (if (= (length args) 2)
         (list 'special 'the (list (car args) (normalize-expr (cadr args))))
         (list 'invalid-special op args)))

(define (normalize-special-unwind-protect op args)
     ;; (unwind-protect protected cleanup...)
     (if (>= (length args) 1)
         (list 'special 'unwind-protect
               (list (normalize-expr (car args))
                     (map normalize-expr (cdr args))))
         (list 'invalid-special op args)))

(define (normalize-special-function op args)
     ;; (function name) or (function (lambda ...))
     (if (= (length args) 1)
         (let ((name (car args)))
           (cond
            ((symbol? name) (list 'var name))
            ((and (pair? name) (eq? (car name) 'lambda))
             (normalize-special 'lambda (cdr name)))
            (else (list 'invalid-special op args))))
         (list 'invalid-special op args)))

(define (normalize-special-with-handler op args)
     ;; (with-handler handler-form protected-form)
     (if (= (length args) 2)
         (list 'special 'with-handler
               (list (normalize-expr (car args))
                     (normalize-expr (cadr args))))
         (list 'invalid-special op args)))

(define (normalize-special-dynamic op args)
     ;; (dynamic var-name) → payload = (sym)
     (if (and (= (length args) 1) (symbol? (car args)))
         (list 'special 'dynamic (list (car args)))
         (list 'invalid-special op args)))

(define (normalize-special-dynamic-let op args)
     ;; (dynamic-let ((var val) ...) body...)
     (if (and (>= (length args) 2) (list? (car args)))
         (let ((bindings (car args))
               (body     (cdr args)))
           (let ((norm-bindings
                  (map (lambda (b)
                         (if (and (list? b) (= (length b) 2) (symbol? (car b)))
                             (list (car b) (normalize-expr (cadr b)))
                             #f))
                       bindings)))
             (if (any not norm-bindings)
                 (list 'invalid-special op args)
                 (list 'special 'dynamic-let
                       (list norm-bindings (normalize-body body))))))
         (list 'invalid-special op args)))

(define (normalize-special-class op args)
     (if (and (= (length args) 1) (symbol? (car args)))
         (list 'special 'class (list (car args)))
         (list 'invalid-special op args)))

(define (normalize-special-case op args)
     ;; (case keyform ((key...) body...)... [(otherwise|t|else body...)])
     (if (< (length args) 1)
         (list 'invalid-special op args)
         (let* ((keyform (normalize-expr (car args)))
                (clauses-raw (cdr args))
                (norm-clauses
                 (let loop ((cs clauses-raw) (acc '()))
                   (if (null? cs)
                       (reverse acc)
                       (let ((c (car cs)))
                         (if (and (list? c) (>= (length c) 1))
                             (let ((keys (car c))
                                   (body (cdr c)))
                               (cond
                                ((or (eq? keys 'otherwise) (eq? keys 't) (eq? keys 'else))
                                 (loop (cdr cs)
                                       (cons (list 'else (normalize-body body)) acc)))
                                ((list? keys)
                                 (loop (cdr cs)
                                       (cons (list keys (normalize-body body)) acc)))
                                (else #f)))
                             #f))))))
           (if (or (not norm-clauses) (any not norm-clauses))
               (list 'invalid-special op args)
               (list 'special 'case (list keyform norm-clauses))))))

(define (normalize-special-dotimes op args)
     ;; (dotimes (var count [result]) body...)
     (if (null? args)
         (list 'invalid-special op args)
         (let ((spec (car args))
               (body (cdr args)))
           (if (and (list? spec)
                    (or (= (length spec) 2) (= (length spec) 3))
                    (symbol? (car spec)))
               (list 'special 'dotimes
                     (list (car spec)
                           (normalize-expr (cadr spec))
                           (if (= (length spec) 3)
                               (normalize-expr (caddr spec))
                               #f)
                           (normalize-body body)))
               (list 'invalid-special op args)))))

(define (normalize-special-for-do op args)
     ;; (for ((var init [step]) ...) (end-test result...) body...)
     (if (< (length args) 2)
         (list 'invalid-special op args)
         (let* ((var-specs-raw (car args))
                (end-clause    (cadr args))
                (body-forms    (cddr args)))
           (if (and (list? var-specs-raw)
                    (list? end-clause)
                    (>= (length end-clause) 1))
               (let* ((norm-specs
                       (map (lambda (vs)
                              (if (and (list? vs)
                                       (or (= (length vs) 2) (= (length vs) 3))
                                       (symbol? (car vs)))
                                  (list (car vs)
                                        (normalize-expr (cadr vs))
                                        (if (= (length vs) 3)
                                            (normalize-expr (caddr vs))
                                            #f))
                                  #f))
                            var-specs-raw))
                      (end-test (normalize-expr (car end-clause)))
                      (result-forms (cdr end-clause))
                      (result-ir (if (null? result-forms)
                                     '(const ())
                                     (normalize-body result-forms)))
                      (body-ir (normalize-body body-forms)))
                 (if (any not norm-specs)
                     (list 'invalid-special op args)
                     (list 'special 'for
                           (list norm-specs end-test result-ir body-ir))))
               (list 'invalid-special op args)))))

(define (normalize-special op args)
  (case op
    ((quote) (normalize-special-quote op args))
    ((if) (normalize-special-if op args))
    ((cond) (normalize-special-cond op args))
    ((and) (normalize-special-and op args))
    ((or) (normalize-special-or op args))
    ((progn) (normalize-special-progn op args))
    ((lambda) (normalize-special-lambda op args))
    ((setq) (normalize-special-setq op args))
    ((let) (normalize-special-let op args))
    ((let*) (normalize-special-let* op args))
    ((setf) (normalize-special-setf op args))
    ((block) (normalize-special-block op args))
    ((return-from) (normalize-special-return-from op args))
    ((catch) (normalize-special-catch op args))
    ((throw) (normalize-special-throw op args))
    ((go) (normalize-special-go op args))
    ((tagbody) (normalize-special-tagbody op args))
    ((with-open-file) (normalize-special-with-open-file op args))
    ((dolist) (normalize-special-dolist op args))
    ((while) (normalize-special-while op args))
    ((handler-case) (normalize-special-handler-case op args))
    ((defpackage) (normalize-special-defpackage op args))
    ((in-package) (normalize-special-in-package op args))
    ((defclass) (normalize-special-defclass op args))
    ((defgeneric) (normalize-special-defgeneric op args))
    ((defmethod) (normalize-special-defmethod op args))
    ((flet labels) (normalize-special-flet-labels op args))
    ((macrolet) (normalize-special-macrolet op args))
    ((the) (normalize-special-the op args))
    ((unwind-protect) (normalize-special-unwind-protect op args))
    ((function) (normalize-special-function op args))
    ((with-handler) (normalize-special-with-handler op args))
    ((dynamic) (normalize-special-dynamic op args))
    ((dynamic-let) (normalize-special-dynamic-let op args))
    ((class) (normalize-special-class op args))
    ((case) (normalize-special-case op args))
    ((dotimes) (normalize-special-dotimes op args))
    ((for do) (normalize-special-for-do op args))
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
            ((quote if cond and or progn lambda setq let let* setf block return-from catch throw go tagbody with-open-file dolist while handler-case
                    defpackage in-package
                    defclass defgeneric defmethod
                    flet labels macrolet the unwind-protect function
                    with-handler
                    dynamic dynamic-let class
                    case dotimes for do)
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
          ((defglobal defconstant)
           (if (= (length args) 2)
               (list 'define-global (car args) (normalize-expr (cadr args)))
               (list 'invalid-top form)))
          ((defmacro)
           (if (>= (length args) 3)
               (list 'define-macro (car args) (cadr args) (cddr args))
               (list 'invalid-top form)))
          ((defvar)
           (if (= (length args) 2)
               (list 'define-var (car args) (normalize-expr (cadr args)))
               (list 'invalid-top form)))
          ((defdynamic)
           (if (= (length args) 2)
               (list 'define-dynamic (car args) (normalize-expr (cadr args)))
               (list 'invalid-top form)))
          (else
           (list 'expr (normalize-expr form)))))
      (list 'expr (normalize-expr form))))

(define (render-ir ir)
  (let ((p (open-output-string)))
    (write ir p)
    (get-output-string p)))
