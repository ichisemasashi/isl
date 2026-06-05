;;; core/conditions.scm
;;; isl.core: 条件システム・多次元配列・プロパティリスト・クラス階層述語・
;;;           ハンドラスタック・動的変数表のデータ表現
;;; src/isl/core.scm から (include ...) され isl.core 本体へ展開（単独ロード不可）。

;; ============================================================
;; Phase 5: ISLISP condition system
;; ============================================================

;; ---- 5-A: isl-condition representation ----
;; slot 5 (`extra`) is a property list carrying typed accessor data such as
;; arithmetic-error operation / domain-error expected-class / undefined-entity
;; namespace.  It is optional so existing 4-argument call sites keep working.
(define (make-isl-condition class-sym message continuable? irritants . maybe-extra)
  (vector 'isl-condition class-sym message continuable? irritants
          (if (pair? maybe-extra) (car maybe-extra) '())))
(define (isl-condition? x)
  (and (vector? x) (>= (vector-length x) 5) (eq? (vector-ref x 0) 'isl-condition)))
(define (isl-condition-class       x) (vector-ref x 1))
(define (isl-condition-message     x) (vector-ref x 2))
(define (isl-condition-continuable? x) (vector-ref x 3))
(define (isl-condition-irritants   x) (vector-ref x 4))
(define (isl-condition-extra       x)
  (if (>= (vector-length x) 6) (vector-ref x 5) '()))
(define (isl-condition-extra-ref   x key . default)
  (let loop ((p (isl-condition-extra x)))
    (cond ((or (null? p) (null? (cdr p)))
           (if (pair? default) (car default) '()))
          ((eq? (car p) key) (cadr p))
          (else (loop (cddr p))))))

;; ---- multi-dimensional general array (rank >= 2) representation ----
;; Rank-1 general vectors stay as plain host vectors; rank >= 2 arrays use this
;; tagged structure with row-major flat storage.  ISLISP class: <general-array*>.
(define (make-isl-array dims storage) (vector 'isl-array dims storage))
(define (isl-array? x)
  (and (vector? x) (= (vector-length x) 3) (eq? (vector-ref x 0) 'isl-array)))
(define (isl-array-dims    x) (vector-ref x 1))
(define (isl-array-storage x) (vector-ref x 2))
;; A plain host vector that is NOT one of our tagged structures (condition/array).
(define (isl-plain-vector? x)
  (and (vector? x) (not (isl-array? x)) (not (isl-condition? x))))
;; Row-major flat index for `idxs` within `dims`; #f on rank/bounds mismatch.
(define (isl-array-flat-index dims idxs)
  (let loop ((ds dims) (is idxs) (acc 0))
    (cond ((and (null? ds) (null? is)) acc)
          ((or (null? ds) (null? is)) #f)
          (else
           (let ((d (car ds)) (i (car is)))
             (if (and (integer? i) (>= i 0) (< i d))
                 (loop (cdr ds) (cdr is) (+ (* acc d) i))
                 #f))))))

;; ---- symbol property lists (ISLISP §10) ----
;; symbol -> alist of (property-name . value).
(define *property-table* (make-hash-table 'eq?))
(define (isl-property-get sym key default)
  (let ((p (assq key (hash-table-get *property-table* sym '()))))
    (if p (cdr p) default)))
(define (isl-property-set! sym key val)
  (let* ((alist (hash-table-get *property-table* sym '()))
         (p (assq key alist)))
    (if p
        (set-cdr! p val)
        (hash-table-put! *property-table* sym (cons (cons key val) alist))))
  val)
(define (isl-property-remove! sym key)
  (let* ((alist (hash-table-get *property-table* sym '()))
         (p (assq key alist)))
    (if p
        (begin
          (hash-table-put! *property-table* sym
                           (let loop ((xs alist))
                             (cond ((null? xs) '())
                                   ((eq? (caar xs) key) (cdr xs))
                                   (else (cons (car xs) (loop (cdr xs)))))))
          (cdr p))
        '())))

;; ---- 5-A: class hierarchy predicate ----
;; Returns #t if sub-sym equals or is a registered subclass of super-sym.
;; Handles both package-qualified (ISLISP::<foo>) and bare (<foo>) names.
(define (isl-subclassp? sub-sym super-sym)
  ;; Normalise to bare name for comparison (strip package prefix)
  (define (bare-name sym)
    (string->symbol (symbol-base-name sym)))
  (or (eq? sub-sym super-sym)
      ;; Also match if bare names are equal (handles resolved vs unresolved)
      (eq? (bare-name sub-sym) (bare-name super-sym))
      (let ((obj (or (class-table-ref sub-sym)
                     ;; Try resolved name if bare lookup failed
                     (class-table-ref (resolve-binding-symbol (bare-name sub-sym))))))
        (and obj
             (let loop ((supers (class-supers obj)))
               (and (pair? supers)
                    (or (isl-subclassp? (class-name (car supers)) super-sym)
                        (loop (cdr supers)))))))))

;; ---- Phase 7-A: next-method support ----
(define *next-methods* (make-parameter '()))
(define *current-method-args* (make-parameter '()))

(define (sort-applicable methods args)
  (let* ((scored (filter-map
                   (lambda (m)
                     (let ((score (method-applicability-score m args)))
                       (and score (cons score m))))
                   methods))
         (sorted (sort scored (lambda (a b) (score<? (car a) (car b))))))
    (map cdr sorted)))

(define (invoke-chain procs extra-fn current-args)
  ;; Call procs[0]; *next-methods* leads to procs[1..n] then extra-fn.
  ;; Only advertise a next method if it would succeed.
  (if (null? procs)
      (if extra-fn
          (extra-fn current-args)
          (error "call-next-method: no next method available"))
      (parameterize ((*next-methods*
                      (if (or (pair? (cdr procs)) extra-fn)
                          (list (lambda (a) (invoke-chain (cdr procs) extra-fn a)))
                          '()))
                     (*current-method-args* current-args))
        (apply-islisp (car procs) current-args))))

;; ---- 5-B: handler stack and signalling ----
(define *isl-handler-stack* (make-parameter '()))

;; ---- Phase 6-B: Dynamic variable table ----
;; Maps resolved symbol → current dynamic value. Thread-local via Gauche parameter.
(define *dynamic-var-table* (make-parameter (make-hash-table 'equal?)))

(define (isl-signal-condition cond-obj)
  (let loop ((stack (*isl-handler-stack*)))
    (if (null? stack)
        (raise cond-obj)                      ; uncaught → Gauche exception
        (let ((handler (car stack)))
          (parameterize ((*isl-handler-stack* (cdr stack)))
            (handler cond-obj))))))           ; handler return = signal return

;; ISLISP §11: integer-division operators signal a catchable <division-by-zero>
;; (with arithmetic-error operation/operands) rather than a raw host error.
(define (isl-signal-div0 op operands)
  (isl-signal-condition
   (make-isl-condition (resolve-binding-symbol '<division-by-zero>)
     "division by zero" #f operands
     (list 'operation op 'operands operands))))

(define (isl-zero-number? x) (and (number? x) (zero? x)))

;; ISLISP §22: unbound variable / undefined function references signal a
;; catchable <unbound-variable> / <undefined-function> condition (subclasses of
;; <undefined-entity>) carrying the entity name and namespace, instead of a raw
;; host error, so handler-case and the undefined-entity-* accessors can see them.
(define (isl-signal-unbound-variable sym)
  (isl-signal-condition
   (make-isl-condition (resolve-binding-symbol '<unbound-variable>)
     (string-append "unbound variable: " (symbol->string sym)) #f (list sym)
     (list 'name sym 'namespace 'variable))))
(define (isl-signal-undefined-function sym)
  (isl-signal-condition
   (make-isl-condition (resolve-binding-symbol '<undefined-function>)
     (string-append "undefined function: " (symbol->string sym)) #f (list sym)
     (list 'name sym 'namespace 'function))))

