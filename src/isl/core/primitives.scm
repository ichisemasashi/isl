;;; core/primitives.scm
;;; isl.core: 組込みプリミティブの登録（install-* 群）
;;; このファイルは src/isl/core.scm から (include ...) され、
;;; モジュール isl.core の本体に展開される（単独ロード不可）。

;; ---- install-primitives! が用いる共有ヘルパ（純粋・env 非依存） ----
;; ---- 5-A: EOF-stream helper (must be define, before any expressions) ----
(define (eof-stream-error who)
  (isl-signal-condition
   (make-isl-condition
    (resolve-binding-symbol '<end-of-stream>)
    (string-append "end-of-stream in " who) #f '())))
(define (list-element-at obj index who)
  (let loop ((rest obj) (i index))
    (unless (pair? rest)
      (error "list accessor needs a list with enough elements" who obj))
    (if (= i 0)
        (car rest)
        (loop (cdr rest) (- i 1)))))
(define (vector-size-from-dim dim who)
  (cond
   ((and (integer? dim) (>= dim 0)) dim)
   ((and (list? dim) (= (length dim) 1) (integer? (car dim)) (>= (car dim) 0))
    (car dim))
   (else
    (error who "dimension must be non-negative integer or one-element list" dim))))
(define (build-vector-from-args args who)
  (unless (>= (length args) 1)
    (error who "needs at least a dimension argument" args))
  (let* ((n (vector-size-from-dim (car args) who))
         (rest (cdr args))
         (has-init-element #f)
         (init-element '())
         (has-init-contents #f)
         (init-contents '()))
    (cond
     ((null? rest) 'ok)
     ((keyword-symbol? (car rest))
      (let parse ((xs rest))
        (unless (null? xs)
          (unless (pair? (cdr xs))
            (error who "keyword arguments must be key/value pairs" xs))
          (let ((k (car xs))
                (v (cadr xs)))
            (cond
             ((eq? k ':initial-element)
              (set! has-init-element #t)
              (set! init-element v))
             ((eq? k ':initial-contents)
              (set! has-init-contents #t)
              (set! init-contents v))
             (else
              (error who "unknown keyword" k))))
          (parse (cddr xs)))))
     ((= (length rest) 1)
      ;; Backward-compatible positional initializer.
      (set! has-init-element #t)
      (set! init-element (car rest)))
     (else
      (error who "invalid arguments" args)))
    (when (and has-init-element has-init-contents)
      (error who "cannot use both :initial-element and :initial-contents"))
    (if has-init-contents
        (let ((vals
               (cond
                ((list? init-contents) init-contents)
                ((vector? init-contents) (vector->list init-contents))
                (else
                 (error who ":initial-contents must be list or vector" init-contents)))))
          (unless (= (length vals) n)
            (error who ":initial-contents length must match dimension" (length vals) n))
          (list->vector vals))
        (make-vector n (if has-init-element init-element '())))))


(define (install-primitives! env)
  (define (def name proc)
    (let ((sym (resolve-binding-symbol name)))
      (frame-define! env sym (make-primitive name proc))
      (when *current-package*
        (package-export! *current-package* name))))
  ;; テーマ別インストーラ（登録順は従来と同一）
  (install-numeric-primitives! def env)
  (install-vector-array-primitives! def env)
  (install-list-primitives! def env)
  (install-sequence-primitives! def env)
  (install-string-primitives! def env)
  (install-char-primitives! def env)
  (install-io-primitives! def env)
  (install-condition-primitives! def env)
  (install-system-primitives! def env)
  (install-database-primitives! def env)
  (install-socket-primitives! def env)
  (install-concurrency-net-primitives! def env)
  (install-ffi-time-load-primitives! def env)
  (install-core-clos-primitives! def env)
  (install-method-package-symbol-primitives! def env)
  )

(define (install-numeric-primitives! def env)
  (def '+ +)
  (def '- -)
  (def '* *)
  ;; ISLISP §11: / signals <division-by-zero> on zero divisor
  (def '/
    (lambda args
      (let ((divisors (if (>= (length args) 2) (cdr args) args)))
        (if (let lp ((xs divisors))
              (cond ((null? xs) #f)
                    ((isl-zero-number? (car xs)) #t)
                    (else (lp (cdr xs)))))
            (isl-signal-div0 '/ args)
            (apply / args)))))
  (def 'mod
    (lambda (n d)
      (if (isl-zero-number? d)
          (isl-signal-div0 'mod (list n d))
          (modulo n d))))
  ;; ISLISP §11.10: floor/ceiling/truncate/round return exact integers.
  (def 'floor
    (lambda args
      (cond
       ((= (length args) 1)
        (inexact->exact (floor (car args))))
       ((= (length args) 2)
        (let ((x (car args)) (d (cadr args)))
          (unless (number? x) (error "floor x must be number" x))
          (unless (number? d) (error "floor divisor must be number" d))
          (inexact->exact (floor (/ x d)))))
       (else (error "floor takes 1 or 2 arguments" args)))))
  (def 'ceiling
    (lambda args
      (cond
       ((= (length args) 1) (inexact->exact (ceiling (car args))))
       ((= (length args) 2)
        (let ((x (car args)) (d (cadr args)))
          (unless (number? x) (error "ceiling x must be number" x))
          (unless (number? d) (error "ceiling divisor must be number" d))
          (inexact->exact (ceiling (/ x d)))))
       (else (error "ceiling takes 1 or 2 arguments" args)))))
  (def 'truncate
    (lambda args
      (cond
       ((= (length args) 1) (inexact->exact (truncate (car args))))
       ((= (length args) 2)
        (let ((x (car args)) (d (cadr args)))
          (unless (number? x) (error "truncate x must be number" x))
          (unless (number? d) (error "truncate divisor must be number" d))
          (inexact->exact (truncate (/ x d)))))
       (else (error "truncate takes 1 or 2 arguments" args)))))
  (def 'round
    (lambda args
      (cond
       ((= (length args) 1) (inexact->exact (round (car args))))
       ((= (length args) 2)
        (let ((x (car args)) (d (cadr args)))
          (unless (number? x) (error "round x must be number" x))
          (unless (number? d) (error "round divisor must be number" d))
          (inexact->exact (round (/ x d)))))
       (else (error "round takes 1 or 2 arguments" args)))))
  (def 'quotient
    (lambda (n d)
      (unless (integer? n) (error "quotient n must be integer" n))
      (unless (integer? d) (error "quotient d must be integer" d))
      (if (isl-zero-number? d)
          (isl-signal-div0 'quotient (list n d))
          (quotient n d))))

  ;; --- Phase 1-A: 算術関数 ---
  (def 'abs
    (lambda (x)
      (unless (number? x) (error "abs needs a number" x))
      (abs x)))
  (def 'max
    (lambda xs
      (unless (and (pair? xs) (every number? xs))
        (error "max needs at least one number" xs))
      (apply max xs)))
  (def 'min
    (lambda xs
      (unless (and (pair? xs) (every number? xs))
        (error "min needs at least one number" xs))
      (apply min xs)))
  (def 'expt
    (lambda (base exp)
      (unless (number? base) (error "expt base must be number" base))
      (unless (number? exp)  (error "expt exponent must be number" exp))
      ;; ISLISP §11.8: non-integer rational result (e.g. 2^-1 = 1/2) → float
      (let ((result (expt base exp)))
        (if (and (rational? result) (not (integer? result)))
            (exact->inexact result)
            result))))
  (def 'sqrt
    (lambda (x)
      (unless (number? x) (error "sqrt needs a number" x))
      (when (and (real? x) (negative? x))
        (error "sqrt of negative number" x))
      (sqrt x)))
  (def 'rem
    (lambda (n d)
      (unless (integer? n) (error "rem dividend must be integer" n))
      (unless (integer? d) (error "rem divisor must be integer" d))
      (if (isl-zero-number? d)
          (isl-signal-div0 'rem (list n d))
          (remainder n d))))
  (def 'gcd
    (lambda xs
      (if (null? xs) 0 (apply gcd xs))))
  (def 'lcm
    (lambda xs
      (if (null? xs) 1 (apply lcm xs))))
  ;; ISLISP §11: div — floor division (greatest integer <= z1/z2).
  (def 'div
    (lambda (z1 z2)
      (unless (number? z1) (error "div: dividend must be a number" z1))
      (unless (number? z2) (error "div: divisor must be a number" z2))
      (if (isl-zero-number? z2)
          (isl-signal-div0 'div (list z1 z2))
          (inexact->exact (floor (/ z1 z2))))))
  ;; ISLISP §11: hyperbolic functions (defined via exp for portability).
  (def 'sinh (lambda (x) (unless (number? x) (error "sinh needs a number" x))
                         (/ (- (exp x) (exp (- x))) 2)))
  (def 'cosh (lambda (x) (unless (number? x) (error "cosh needs a number" x))
                         (/ (+ (exp x) (exp (- x))) 2)))
  (def 'tanh (lambda (x) (unless (number? x) (error "tanh needs a number" x))
                         (let ((ex (exp x)) (enx (exp (- x))))
                           (/ (- ex enx) (+ ex enx)))))
  (def 'atanh (lambda (x) (unless (number? x) (error "atanh needs a number" x))
                          (when (or (>= x 1) (<= x -1))
                            (error "atanh: argument must be in (-1, 1)" x))
                          (* 0.5 (log (/ (+ 1 x) (- 1 x))))))
  ;; ISLISP §11: atan2 — two-argument arctangent.
  (def 'atan2 (lambda (y x)
                (unless (number? y) (error "atan2: y must be a number" y))
                (unless (number? x) (error "atan2: x must be a number" x))
                (atan y x)))
  ;; ISLISP §11: generic-function-p
  (def 'generic-function-p (lambda (x) (if (generic? x) #t '())))
  ;; ISLISP §11: implementation-defined float magnitude limits (IEEE-754 double).
  (frame-define! env (resolve-binding-symbol '*most-positive-float*)
                 1.7976931348623157e308)
  (frame-define! env (resolve-binding-symbol '*most-negative-float*)
                 -1.7976931348623157e308)
  (when *current-package*
    (package-export! *current-package* '*most-positive-float*)
    (package-export! *current-package* '*most-negative-float*))
  (def 'signum
    (lambda (x)
      (unless (number? x) (error "signum needs a number" x))
      (cond ((positive? x) 1) ((negative? x) -1) (else 0))))
  (def 'negate
    (lambda (x)
      (unless (number? x) (error "negate needs a number" x))
      (- x)))
  (def 'float
    (lambda (x)
      (unless (number? x) (error "float needs a number" x))
      (exact->inexact x)))
  (def 'isqrt
    (lambda (n)
      (unless (and (integer? n) (>= n 0))
        (error "isqrt needs a non-negative integer" n))
      (exact (floor (sqrt n)))))

  ;; --- Phase 1-B: 数値変換 ---
  (def 'number->string
    (lambda args
      (unless (and (pair? args) (number? (car args)))
        (error "number->string needs a number" args))
      (let ((n     (car args))
            (radix (if (and (pair? (cdr args)) (integer? (cadr args)))
                       (cadr args) 10)))
        (unless (memv radix '(2 8 10 16))
          (error "number->string radix must be 2, 8, 10 or 16" radix))
        (number->string n radix))))
  ;; ISLISP §20.2 canonical name: number-to-string
  (def 'number-to-string
    (lambda args
      (unless (and (pair? args) (number? (car args)))
        (error "number-to-string needs a number" args))
      (let ((n     (car args))
            (radix (if (and (pair? (cdr args)) (integer? (cadr args)))
                       (cadr args) 10)))
        (number->string n radix))))
  (def 'string->number
    (lambda args
      (unless (and (pair? args) (string? (car args)))
        (error "string->number needs a string" args))
      (let ((s     (car args))
            (radix (if (and (pair? (cdr args)) (integer? (cadr args)))
                       (cadr args) 10)))
        (let ((result (string->number s radix)))
          (if result result '())))))
  ;; ISLISP §11.8: parse-number — parse number from string
  (def 'parse-number
    (lambda args
      (unless (and (pair? args) (string? (car args)))
        (error "parse-number needs a string" args))
      (let* ((s (car args))
             (radix (if (and (pair? (cdr args)) (integer? (cadr args)))
                        (cadr args) 10))
             (result (string->number s radix)))
        (unless result (error "parse-number: cannot parse as number" s))
        result)))
  ;; ISLISP canonical alias: string-to-number (returns nil on failure)
  (def 'string-to-number
    (lambda args
      (unless (and (pair? args) (string? (car args)))
        (error "string-to-number needs a string" args))
      (let ((s     (car args))
            (radix (if (and (pair? (cdr args)) (integer? (cadr args)))
                       (cadr args) 10)))
        (let ((result (string->number s radix)))
          (if result result '())))))

  ;; --- Phase 1-C: 型述語 ---
  (def 'consp (lambda (x) (if (pair? x) #t '())))
  (def 'characterp (lambda (x) (if (char? x) #t '())))
  (def 'integerp (lambda (x) (if (and (integer? x) (exact? x)) #t '())))
  (def 'floatp (lambda (x) (if (inexact? x) #t '())))
  (def 'functionp
    (lambda (x)
      (if (or (closure? x) (primitive? x)) #t '())))
  ;; general-vector = rank-1 general vector (plain host vector, not a tagged
  ;; condition/N-D array); general-array* = general array of rank /= 1.
  (def 'general-vector-p (lambda (x) (if (isl-plain-vector? x) #t '())))
  (def 'general-array*-p (lambda (x) (if (isl-array? x) #t '())))

  ;; --- Phase 1-D: 三角関数・超越関数 ---
  (def 'exp  (lambda (x) (unless (number? x) (error "exp needs number" x))  (exp x)))
  (def 'log  (lambda (x) (unless (number? x) (error "log needs number" x))  (log x)))
  (def 'sin  (lambda (x) (unless (number? x) (error "sin needs number" x))  (sin x)))
  (def 'cos  (lambda (x) (unless (number? x) (error "cos needs number" x))  (cos x)))
  (def 'tan  (lambda (x) (unless (number? x) (error "tan needs number" x))  (tan x)))
  (def 'asin (lambda (x) (unless (number? x) (error "asin needs number" x)) (asin x)))
  (def 'acos (lambda (x) (unless (number? x) (error "acos needs number" x)) (acos x)))
  (def 'atan
    (lambda args
      (cond
       ((= (length args) 1)
        (unless (number? (car args)) (error "atan needs number" (car args)))
        (atan (car args)))
       ((= (length args) 2)
        (unless (number? (car args))  (error "atan y must be number" (car args)))
        (unless (number? (cadr args)) (error "atan x must be number" (cadr args)))
        (atan (car args) (cadr args)))
       (else (error "atan takes 1 or 2 arguments" args)))))

  (def '= (lambda (a b) (if (= a b) #t '())))
  (def '/= (lambda (a b) (if (= a b) '() #t)))
  (def '< (lambda (a b) (if (< a b) #t '())))
  (def '> (lambda (a b) (if (> a b) #t '())))
  (def '<= (lambda (a b) (if (<= a b) #t '())))
  (def '>= (lambda (a b) (if (>= a b) #t '())))
  ;; ISLISP §11: reciprocal returns 1/x as float
  (def 'reciprocal
    (lambda (x)
      (unless (number? x) (error "reciprocal: argument must be a number" x))
      (when (= x 0) (error "reciprocal: division by zero" x))
      (exact->inexact (/ 1 x))))
  (def 'cons cons)
  ;; ISLISP §15.2: c[ad]+r composite accessors
  (def 'caar (lambda (x) (car (car x))))
  (def 'cadr (lambda (x) (car (cdr x))))
  (def 'cdar (lambda (x) (cdr (car x))))
  (def 'cddr (lambda (x) (cdr (cdr x))))
  (def 'caaar (lambda (x) (car (car (car x)))))
  (def 'caadr (lambda (x) (car (car (cdr x)))))
  (def 'cadar (lambda (x) (car (cdr (car x)))))
  (def 'caddr (lambda (x) (car (cdr (cdr x)))))
  (def 'cdaar (lambda (x) (cdr (car (car x)))))
  (def 'cdadr (lambda (x) (cdr (car (cdr x)))))
  (def 'cddar (lambda (x) (cdr (cdr (car x)))))
  (def 'cdddr (lambda (x) (cdr (cdr (cdr x)))))
  (def 'caaaar (lambda (x) (car (car (car (car x))))))
  (def 'caaadr (lambda (x) (car (car (car (cdr x))))))
  (def 'caadar (lambda (x) (car (car (cdr (car x))))))
  (def 'caaddr (lambda (x) (car (car (cdr (cdr x))))))
  (def 'cadaar (lambda (x) (car (cdr (car (car x))))))
  (def 'cadadr (lambda (x) (car (cdr (car (cdr x))))))
  (def 'caddar (lambda (x) (car (cdr (cdr (car x))))))
  (def 'cadddr (lambda (x) (car (cdr (cdr (cdr x))))))
  (def 'cdaaar (lambda (x) (cdr (car (car (car x))))))
  (def 'cdaadr (lambda (x) (cdr (car (car (cdr x))))))
  (def 'cdadar (lambda (x) (cdr (car (cdr (car x))))))
  (def 'cdaddr (lambda (x) (cdr (car (cdr (cdr x))))))
  (def 'cddaar (lambda (x) (cdr (cdr (car (car x))))))
  (def 'cddadr (lambda (x) (cdr (cdr (car (cdr x))))))
  (def 'cdddar (lambda (x) (cdr (cdr (cdr (car x))))))
  (def 'cddddr (lambda (x) (cdr (cdr (cdr (cdr x))))))
  ;; ISLISP §15.1: car/cdr of nil returns nil; car/cdr of non-pair signals <domain-error>
  (def 'car
    (lambda (x)
      (cond ((null? x) '())
            ((pair? x) (car x))
            (else (isl-signal-condition
                   (make-isl-condition (resolve-binding-symbol '<domain-error>)
                     "car: argument is not a list"
                     #f (list x)
                     (list 'object x 'expected-class '<list>)))))))
  (def 'cdr
    (lambda (x)
      (cond ((null? x) '())
            ((pair? x) (cdr x))
            (else (isl-signal-condition
                   (make-isl-condition (resolve-binding-symbol '<domain-error>)
                     "cdr: argument is not a list"
                     #f (list x)
                     (list 'object x 'expected-class '<list>)))))))
  (def 'vector (lambda xs (list->vector xs)))
  (def 'make-array
    (lambda args
      (build-vector-from-args args "make-array")))
  (def 'make-vector
    (lambda args
      (build-vector-from-args args "make-vector")))
  (def 'vector-ref
    (lambda (v i)
      (unless (vector? v)
        (error "vector-ref target must be vector" v))
      (unless (and (integer? i) (>= i 0) (< i (vector-length v)))
        (error "vector-ref index out of range" i))
      (vector-ref v i)))
  (def 'aref
    (lambda (v . idxs)
      (cond
       ((isl-array? v)
        (let ((flat (isl-array-flat-index (isl-array-dims v) idxs)))
          (unless flat (error "aref: index out of range or rank mismatch" idxs))
          (vector-ref (isl-array-storage v) flat)))
       ((isl-plain-vector? v)
        (let ((i (car idxs)))
          (unless (and (pair? idxs) (null? (cdr idxs)))
            (error "aref: vector needs exactly one index" idxs))
          (unless (and (integer? i) (>= i 0) (< i (vector-length v)))
            (error "aref: index out of range" i))
          (vector-ref v i)))
       ((string? v)
        (let ((i (car idxs)))
          (unless (and (pair? idxs) (null? (cdr idxs)))
            (error "aref: string needs exactly one index" idxs))
          (unless (and (integer? i) (>= i 0) (< i (string-length v)))
            (error "aref: string index out of range" i))
          (string-ref v i)))
       (else
        (error "aref: first argument must be an array" v)))))
  ;; ISLISP §13: garef — like aref but only for general arrays (not strings)
  (def 'garef
    (lambda (v . idxs)
      (cond
       ((isl-array? v)
        (let ((flat (isl-array-flat-index (isl-array-dims v) idxs)))
          (unless flat (error "garef: index out of range or rank mismatch" idxs))
          (vector-ref (isl-array-storage v) flat)))
       ((isl-plain-vector? v)
        (let ((i (car idxs)))
          (unless (and (pair? idxs) (null? (cdr idxs)))
            (error "garef: vector needs exactly one index" idxs))
          (unless (and (integer? i) (>= i 0) (< i (vector-length v)))
            (error "garef: index out of range" i))
          (vector-ref v i)))
       (else
        (error "garef: first argument must be a general array" v)))))
  (def 'vector-set!
    (lambda (v i x)
      (unless (vector? v)
        (error "vector-set! target must be vector" v))
      (unless (and (integer? i) (>= i 0) (< i (vector-length v)))
        (error "vector-set! index out of range" i))
      (vector-set! v i x)
      x))
  (def 'vectorp (lambda (x) (isl-plain-vector? x)))

  )

(define (install-vector-array-primitives! def env)
  ;; --- Phase 2-D: ベクター ISLISP 標準名 ---
  (def 'create-vector
    (lambda args
      (build-vector-from-args args "create-vector")))
  (def 'general-vector-ref
    (lambda (v i)
      (unless (vector? v) (error "general-vector-ref target must be vector" v))
      (unless (and (integer? i) (>= i 0) (< i (vector-length v)))
        (error "general-vector-ref index out of range" i))
      (vector-ref v i)))
  ;; ISLISP §16: general-vector-set (without !) is the standard name
  (def 'general-vector-set
    (lambda (val v i)
      (unless (vector? v) (error "general-vector-set target must be vector" v))
      (unless (and (integer? i) (>= i 0) (< i (vector-length v)))
        (error "general-vector-set index out of range" i))
      (vector-set! v i val)
      val))
  ;; ISLISP §16: set-aref — mutate a basic array (vector, string, or N-D array)
  (def 'set-aref
    (lambda (val arr . idxs)
      (when (null? idxs) (error "set-aref: index required"))
      (cond
       ((isl-array? arr)
        (let ((flat (isl-array-flat-index (isl-array-dims arr) idxs)))
          (unless flat (error "set-aref: index out of range or rank mismatch" idxs))
          (vector-set! (isl-array-storage arr) flat val)
          val))
       ((isl-plain-vector? arr)
        (let ((i (car idxs)))
          (unless (and (integer? i) (>= i 0) (< i (vector-length arr)))
            (error "set-aref: vector index out of range" i))
          (vector-set! arr i val)
          val))
       ((string? arr)
        (let ((i (car idxs)))
          (unless (and (integer? i) (>= i 0) (< i (string-length arr)))
            (error "set-aref: string index out of range" i))
          (string-set! arr i val)
          val))
       (else (error "set-aref: first argument must be an array" arr)))))
  ;; ISLISP §13: set-garef — like set-aref but for general arrays (not strings)
  (def 'set-garef
    (lambda (val arr . idxs)
      (when (null? idxs) (error "set-garef: index required"))
      (cond
       ((isl-array? arr)
        (let ((flat (isl-array-flat-index (isl-array-dims arr) idxs)))
          (unless flat (error "set-garef: index out of range or rank mismatch" idxs))
          (vector-set! (isl-array-storage arr) flat val)
          val))
       ((isl-plain-vector? arr)
        (let ((i (car idxs)))
          (unless (and (integer? i) (>= i 0) (< i (vector-length arr)))
            (error "set-garef: vector index out of range" i))
          (vector-set! arr i val)
          val))
       (else (error "set-garef: first argument must be a general array" arr)))))

  ;; --- Phase 2-E: 配列関数 ---
  (def 'create-array
    (lambda args
      (when (null? args) (error "create-array needs a dimension argument" args))
      (let ((dims (car args)))
        (if (and (list? dims) (> (length dims) 1))
            ;; rank >= 2 → multi-dimensional general array (row-major storage)
            (let* ((init (if (pair? (cdr args)) (cadr args) '()))
                   (total (let lp ((ds dims) (p 1))
                            (if (null? ds)
                                p
                                (begin
                                  (unless (and (integer? (car ds)) (>= (car ds) 0))
                                    (error "create-array: dimension must be a non-negative integer" (car ds)))
                                  (lp (cdr ds) (* p (car ds))))))))
              (make-isl-array dims (make-vector total init)))
            ;; rank 1 (or scalar dim) → plain general vector
            (build-vector-from-args args "create-array")))))
  (def 'array-dimensions
    (lambda (arr)
      (cond
       ((isl-array? arr) (isl-array-dims arr))
       ((isl-plain-vector? arr) (list (vector-length arr)))
       ((string? arr) (list (string-length arr)))
       (else (error "array-dimensions needs an array" arr)))))

  (def 'make-hash-table
    (lambda args
      (let ((test #f)
            (size 0))
        (if (and (pair? args) (keyword-symbol? (car args)))
            (let parse ((xs args))
              (unless (null? xs)
                (unless (pair? (cdr xs))
                  (error "make-hash-table keyword arguments must be pairs" xs))
                (let ((k (car xs))
                      (v (cadr xs)))
                  (cond
                   ((eq? k ':test) (set! test v))
                   ((eq? k ':size)
                    (unless (and (integer? v) (>= v 0))
                      (error "make-hash-table :size must be non-negative integer" v))
                    (set! size v))
                   (else
                    (error "unknown make-hash-table keyword" k))))
                (parse (cddr xs))))
            (when (pair? args)
              (set! test (car args))
              (when (pair? (cdr args))
                (let ((v (cadr args)))
                  (unless (and (integer? v) (>= v 0))
                    (error "make-hash-table size must be non-negative integer" v))
                  (set! size v)))))
        (make-hash-table (hash-comparator-from-test test) size))))
  (def 'hash-table-p hash-table?)
  (def 'gethash
    (lambda args
      (unless (or (= (length args) 2) (= (length args) 3))
        (error "gethash takes key table and optional default" args))
      (let ((key (car args))
            (table (cadr args))
            (default (if (= (length args) 3) (caddr args) '())))
        (unless (hash-table? table)
          (error "gethash table must be hash-table" table))
        (if (hash-table-exists? table key)
            (hash-table-get table key)
            default))))
  (def 'puthash
    (lambda (key value table)
      (unless (hash-table? table)
        (error "puthash table must be hash-table" table))
      (hash-table-put! table key value)
      value))
  (def 'remhash
    (lambda (key table)
      (unless (hash-table? table)
        (error "remhash table must be hash-table" table))
      (let ((existed (hash-table-exists? table key)))
        (when existed
          (hash-table-delete! table key))
        existed)))
  (def 'clrhash
    (lambda (table)
      (unless (hash-table? table)
        (error "clrhash table must be hash-table" table))
      (hash-table-clear! table)
      table))
  (def 'hash-table-count
    (lambda (table)
      (unless (hash-table? table)
        (error "hash-table-count table must be hash-table" table))
      (hash-table-num-entries table)))
  (def 'hash-table-keys
    (lambda (table)
      (unless (hash-table? table)
        (error "hash-table-keys table must be hash-table" table))
      (hash-table-keys table)))
  (def 'hash-table-values
    (lambda (table)
      (unless (hash-table? table)
        (error "hash-table-values table must be hash-table" table))
      (hash-table-values table)))
  (def 'first   (lambda (x) (list-element-at x 0 'first)))
  (def 'second  (lambda (x) (list-element-at x 1 'second)))
  (def 'third   (lambda (x) (list-element-at x 2 'third)))
  (def 'fourth  (lambda (x) (list-element-at x 3 'fourth)))
  (def 'fifth   (lambda (x) (list-element-at x 4 'fifth)))
  (def 'sixth   (lambda (x) (list-element-at x 5 'sixth)))
  (def 'seventh (lambda (x) (list-element-at x 6 'seventh)))
  (def 'eighth  (lambda (x) (list-element-at x 7 'eighth)))
  (def 'ninth   (lambda (x) (list-element-at x 8 'ninth)))
  (def 'tenth   (lambda (x) (list-element-at x 9 'tenth)))
  (def 'list list)
  (def 'append append)

  )

(define (install-list-primitives! def env)
  ;; --- Phase 2-A: リスト関数 ---
  (def 'reverse
    (lambda (xs)
      (unless (list? xs) (error "reverse needs a list" xs))
      (reverse xs)))
  (def 'nreverse
    (lambda (xs)
      (unless (list? xs) (error "nreverse needs a list" xs))
      (reverse! xs)))
  (def 'map
    (lambda (f . lists)
      (when (null? lists) (error "map needs at least one list"))
      (for-each (lambda (l) (unless (list? l) (error "map needs lists" l))) lists)
      (apply map (lambda xs (apply-islisp f xs)) lists)))
  (def 'for-each
    (lambda (f . lists)
      (when (null? lists) (error "for-each needs at least one list"))
      (for-each (lambda (l) (unless (list? l) (error "for-each needs lists" l))) lists)
      (apply for-each (lambda xs (apply-islisp f xs)) lists)
      '()))
  (def 'mapc
    (lambda (f . lists)
      (when (null? lists) (error "mapc needs at least one list"))
      (for-each (lambda (l) (unless (list? l) (error "mapc needs lists" l))) lists)
      (apply for-each (lambda xs (apply-islisp f xs)) lists)
      (car lists)))
  (def 'mapcan
    (lambda (f . lists)
      (when (null? lists) (error "mapcan needs at least one list"))
      (for-each (lambda (l) (unless (list? l) (error "mapcan needs lists" l))) lists)
      (apply append (apply map (lambda xs (apply-islisp f xs)) lists))))
  (def 'maplist
    (lambda (f . lists)
      (when (null? lists) (error "maplist needs at least one list"))
      (for-each (lambda (l) (unless (list? l) (error "maplist needs lists" l))) lists)
      (let loop ((tails lists) (acc '()))
        (if (any null? tails)
            (reverse acc)
            (loop (map cdr tails)
                  (cons (apply-islisp f tails) acc))))))
  ;; ISLISP §15.6: mapl — like maplist but for side effects, returns first list
  (def 'mapl
    (lambda (f . lists)
      (when (null? lists) (error "mapl needs at least one list"))
      (for-each (lambda (l) (unless (list? l) (error "mapl needs lists" l))) lists)
      (let ((first-list (car lists)))
        (let loop ((tails lists))
          (unless (any null? tails)
            (apply-islisp f tails)
            (loop (map cdr tails))))
        first-list)))
  ;; ISLISP §15.6: mapcon — nconc of maplist results
  (def 'mapcon
    (lambda (f . lists)
      (when (null? lists) (error "mapcon needs at least one list"))
      (for-each (lambda (l) (unless (list? l) (error "mapcon needs lists" l))) lists)
      (let loop ((tails lists) (acc '()))
        (if (any null? tails)
            (apply append (reverse acc))
            (loop (map cdr tails)
                  (cons (apply-islisp f tails) acc))))))
  (def 'member
    (lambda (obj lst . opts)
      (let ((test (if (and (pair? opts) (eq? (car opts) ':test))
                      (cadr opts) #f)))
        (unless (list? lst) (error "member needs a list" lst))
        (let loop ((rest lst))
          (cond
           ((null? rest) '())
           (test
            (if (truthy? (apply-islisp test (list obj (car rest))))
                rest (loop (cdr rest))))
           (else
            (if (eqv? obj (car rest))
                rest (loop (cdr rest)))))))))
  (def 'assoc
    (lambda (key alist . opts)
      (let ((test (if (and (pair? opts) (eq? (car opts) ':test))
                      (cadr opts) #f)))
        (unless (list? alist) (error "assoc needs an alist" alist))
        (let loop ((rest alist))
          (cond
           ((null? rest) '())
           ((not (pair? (car rest))) (loop (cdr rest)))
           (test
            (if (truthy? (apply-islisp test (list key (caar rest))))
                (car rest) (loop (cdr rest))))
           (else
            (if (equal? key (caar rest))
                (car rest) (loop (cdr rest)))))))))
  ;; ISLISP §15.3: memq (eq test), memql (eql test)
  (def 'memq
    (lambda (obj lst)
      (unless (list? lst) (error "memq needs a list" lst))
      (let loop ((rest lst))
        (cond
         ((null? rest) '())
         ((eq? obj (car rest)) rest)
         (else (loop (cdr rest)))))))
  (def 'memql
    (lambda (obj lst)
      (unless (list? lst) (error "memql needs a list" lst))
      (let loop ((rest lst))
        (cond
         ((null? rest) '())
         ((eqv? obj (car rest)) rest)
         (else (loop (cdr rest)))))))
  ;; ISLISP §15.3: assq (eq test), assql (eql test)
  (def 'assq
    (lambda (key alist)
      (unless (list? alist) (error "assq needs an alist" alist))
      (let loop ((rest alist))
        (cond
         ((null? rest) '())
         ((not (pair? (car rest))) (loop (cdr rest)))
         ((eq? key (caar rest)) (car rest))
         (else (loop (cdr rest)))))))
  (def 'assql
    (lambda (key alist)
      (unless (list? alist) (error "assql needs an alist" alist))
      (let loop ((rest alist))
        (cond
         ((null? rest) '())
         ((not (pair? (car rest))) (loop (cdr rest)))
         ((eqv? key (caar rest)) (car rest))
         (else (loop (cdr rest)))))))
  (def 'remove
    (lambda (obj lst . opts)
      (let ((test (if (and (pair? opts) (eq? (car opts) ':test))
                      (cadr opts) #f)))
        (unless (list? lst) (error "remove needs a list" lst))
        (let loop ((rest lst) (acc '()))
          (cond
           ((null? rest) (reverse acc))
           ((if test
                (truthy? (apply-islisp test (list obj (car rest))))
                (eqv? obj (car rest)))
            (loop (cdr rest) acc))
           (else
            (loop (cdr rest) (cons (car rest) acc))))))))
  (def 'last-pair
    (lambda (lst)
      (unless (pair? lst) (error "last-pair needs a non-empty list" lst))
      (let loop ((rest lst))
        (if (null? (cdr rest)) rest (loop (cdr rest))))))
  (def 'nconc
    (lambda lists
      (let loop ((ls lists))
        (cond
         ((null? ls) '())
         ((null? (cdr ls)) (car ls))
         ((null? (car ls)) (loop (cdr ls)))
         (else
          (let ((tail (loop (cdr ls))))
            (set-cdr! (let find-last ((x (car ls)))
                        (if (null? (cdr x)) x (find-last (cdr x))))
                      tail)
            (car ls)))))))
  ;; ISLISP §15.8: append! — destructive append (alias for nconc)
  (def 'append!
    (lambda lists
      (let loop ((ls lists))
        (cond
         ((null? ls) '())
         ((null? (cdr ls)) (car ls))
         ((null? (car ls)) (loop (cdr ls)))
         (else
          (let ((tail (loop (cdr ls))))
            (set-cdr! (let find-last ((x (car ls)))
                        (if (null? (cdr x)) x (find-last (cdr x))))
                      tail)
            (car ls)))))))
  ;; ISLISP §15.1: set-car, set-cdr — mutate pair
  ;; ISLISP §15.1: argument order is (set-car obj cons) / (set-cdr obj cons).
  (def 'set-car
    (lambda (val pair)
      (unless (pair? pair) (error "set-car: second argument must be a pair" pair))
      (set-car! pair val)
      val))
  (def 'set-cdr
    (lambda (val pair)
      (unless (pair? pair) (error "set-cdr: second argument must be a pair" pair))
      (set-cdr! pair val)
      val))
  )

(define (install-sequence-primitives! def env)
  ;; ISLISP §15.3: elt — element of a sequence (list, vector, or string)
  (def 'elt
    (lambda (seq z)
      (cond
       ((list? seq)
        (unless (and (integer? z) (>= z 0)) (error "elt: index must be non-negative integer" z))
        (let loop ((rest seq) (n z))
          (cond
           ((null? rest) (error "elt: index out of range" z))
           ((= n 0) (car rest))
           (else (loop (cdr rest) (- n 1))))))
       ((vector? seq)
        (unless (and (integer? z) (>= z 0) (< z (vector-length seq)))
          (error "elt: vector index out of range" z))
        (vector-ref seq z))
       ((string? seq)
        (unless (and (integer? z) (>= z 0) (< z (string-length seq)))
          (error "elt: string index out of range" z))
        (string-ref seq z))
       (else (error "elt: first argument must be a sequence" seq)))))
  ;; ISLISP §15.2: set-elt — sequence element setter
  (def 'set-elt
    (lambda (val seq z)
      (cond
       ((list? seq)
        (unless (and (integer? z) (>= z 0)) (error "set-elt: index must be non-negative integer" z))
        (let loop ((rest seq) (n z))
          (cond
           ((null? rest) (error "set-elt: index out of range" z))
           ((= n 0) (set-car! rest val) val)
           (else (loop (cdr rest) (- n 1))))))
       ((vector? seq)
        (unless (and (integer? z) (>= z 0) (< z (vector-length seq)))
          (error "set-elt: vector index out of range" z))
        (vector-set! seq z val)
        val)
       ((string? seq)
        (unless (and (integer? z) (>= z 0) (< z (string-length seq)))
          (error "set-elt: string index out of range" z))
        (unless (char? val) (error "set-elt: value for string must be a character" val))
        (string-set! seq z val)
        val)
       (else (error "set-elt: first argument must be a sequence" seq)))))
  ;; ISLISP §15.3: subseq — extract subsequence
  (def 'subseq
    (lambda (seq z1 z2)
      (cond
       ((list? seq)
        (unless (and (integer? z1) (>= z1 0)) (error "subseq: start must be non-negative integer" z1))
        (unless (and (integer? z2) (>= z2 z1)) (error "subseq: end must be >= start" z2))
        (let* ((len (length seq)))
          (unless (<= z2 len) (error "subseq: end out of range" z2))
          (let collect ((rest seq) (i 0) (acc '()))
            (if (or (null? rest) (>= i z2))
                (reverse acc)
                (collect (cdr rest) (+ i 1)
                         (if (>= i z1) (cons (car rest) acc) acc))))))
       ((vector? seq)
        (unless (and (integer? z1) (>= z1 0)) (error "subseq: invalid start" z1))
        (unless (and (integer? z2) (>= z2 z1) (<= z2 (vector-length seq)))
          (error "subseq: invalid end" z2))
        (let* ((new-v (make-vector (- z2 z1))))
          (let lp ((i z1))
            (unless (>= i z2)
              (vector-set! new-v (- i z1) (vector-ref seq i))
              (lp (+ i 1))))
          new-v))
       ((string? seq)
        (unless (and (integer? z1) (>= z1 0)) (error "subseq: invalid start" z1))
        (unless (and (integer? z2) (>= z2 z1) (<= z2 (string-length seq)))
          (error "subseq: invalid end" z2))
        (substring seq z1 z2))
       (else (error "subseq: first argument must be a sequence" seq)))))
  ;; ISLISP §11.2: integer-length — bits needed to represent n
  (def 'integer-length
    (lambda (n)
      (unless (integer? n) (error "integer-length needs an integer" n))
      (let ((m (if (< n 0) (- -1 n) n)))
        (if (= m 0) 0
            (let loop ((bits 0) (x m))
              (if (= x 0) bits (loop (+ bits 1) (quotient x 2))))))))
  (def 'list-ref
    (lambda (lst i)
      (unless (list? lst) (error "list-ref needs a list" lst))
      (unless (and (integer? i) (>= i 0)) (error "list-ref index must be non-negative integer" i))
      (let loop ((rest lst) (n i))
        (cond
         ((null? rest) (error "list-ref index out of range" i))
         ((= n 0) (car rest))
         (else (loop (cdr rest) (- n 1)))))))
  (def 'list-tail
    (lambda (lst i)
      (unless (list? lst) (error "list-tail needs a list" lst))
      (unless (and (integer? i) (>= i 0)) (error "list-tail index must be non-negative integer" i))
      (let loop ((rest lst) (n i))
        (cond
         ((= n 0) rest)
         ((null? rest) (error "list-tail index out of range" i))
         (else (loop (cdr rest) (- n 1)))))))
  (def 'create-list
    (lambda args
      (unless (and (pair? args) (integer? (car args)) (>= (car args) 0))
        (error "create-list needs a non-negative integer" args))
      (let ((n    (car args))
            (init (if (pair? (cdr args)) (cadr args) '())))
        (let loop ((i n) (acc '()))
          (if (= i 0) acc (loop (- i 1) (cons init acc)))))))
  (def 'list-copy
    (lambda (lst)
      (unless (list? lst) (error "list-copy needs a list" lst))
      (map (lambda (x) x) lst)))
  ;; ISLISP §15.2.2: copy-list — canonical ISLISP name
  (def 'copy-list
    (lambda (lst)
      (unless (list? lst) (error "copy-list needs a list" lst))
      (map (lambda (x) x) lst)))
  ;; ISLISP §15.7: fill — fill sequence with value
  (def 'fill
    (lambda (seq obj)
      (cond
       ((vector? seq)
        (let loop ((i 0))
          (unless (>= i (vector-length seq))
            (vector-set! seq i obj)
            (loop (+ i 1))))
        seq)
       ((string? seq)
        (unless (char? obj) (error "fill: value for string must be a character" obj))
        (let loop ((i 0))
          (unless (>= i (string-length seq))
            (string-set! seq i obj)
            (loop (+ i 1))))
        seq)
       ((list? seq)
        ;; ISLISP fill on list: destructively replace each car
        (let loop ((rest seq))
          (unless (null? rest)
            (set-car! rest obj)
            (loop (cdr rest))))
        seq)
       (else (error "fill: first argument must be a sequence" seq)))))
  (def 'nthcdr
    (lambda (n lst)
      (unless (and (integer? n) (>= n 0)) (error "nthcdr n must be non-negative integer" n))
      (let loop ((rest lst) (i n))
        (if (= i 0) rest
            (begin
              (unless (pair? rest) (error "nthcdr list too short" n))
              (loop (cdr rest) (- i 1)))))))
  (def 'sort
    (lambda (lst pred)
      (unless (list? lst) (error "sort needs a list" lst))
      (sort lst (lambda (a b) (truthy? (apply-islisp pred (list a b)))))))

  (def 'mapcar
    (lambda (f . lists)
      (when (null? lists)
        (error "mapcar needs at least one list"))
      (for-each (lambda (xs)
                  (unless (list? xs)
                    (error "mapcar: all arguments after function must be lists" xs)))
                lists)
      (let loop ((tails lists) (acc '()))
        (if (any null? tails)
            (reverse acc)
            (loop (map cdr tails)
                  (cons (apply-islisp f (map car tails)) acc))))))
  (def 'reduce
    (lambda args
      (unless (or (= (length args) 2) (= (length args) 3))
        (error "reduce takes function, list, and optional initial-value" args))
      (let ((f (car args))
            (xs (cadr args))
            (has-init (= (length args) 3))
            (init (if (= (length args) 3) (caddr args) '())))
        (unless (list? xs)
          (error "reduce needs a list as second argument" xs))
        (if has-init
            (let loop ((acc init) (rest xs))
              (if (null? rest)
                  acc
                  (loop (apply-islisp f (list acc (car rest)))
                        (cdr rest))))
            (if (null? xs)
                (error "reduce on empty list needs initial-value")
                (let loop ((acc (car xs)) (rest (cdr xs)))
                  (if (null? rest)
                      acc
                      (loop (apply-islisp f (list acc (car rest)))
                            (cdr rest)))))))))
  (def 'remove-if
    (lambda (pred xs)
      (unless (list? xs)
        (error "remove-if needs a list as second argument" xs))
      (let loop ((rest xs) (acc '()))
        (if (null? rest)
            (reverse acc)
            (let ((v (car rest)))
              (if (truthy? (apply-islisp pred (list v)))
                  (loop (cdr rest) acc)
                  (loop (cdr rest) (cons v acc))))))))
  (def 'remove-if-not
    (lambda (pred xs)
      (unless (list? xs)
        (error "remove-if-not needs a list as second argument" xs))
      (let loop ((rest xs) (acc '()))
        (if (null? rest)
            (reverse acc)
            (let ((v (car rest)))
              (if (truthy? (apply-islisp pred (list v)))
                  (loop (cdr rest) (cons v acc))
                  (loop (cdr rest) acc)))))))
  (def 'find
    (lambda args
      (unless (>= (length args) 2)
        (error "find needs item and sequence" args))
      (let ((item (car args))
            (seq (cadr args))
            (opts (cddr args))
            (test #f)
            (key #f))
        (let parse ((rest opts))
          (unless (null? rest)
            (unless (pair? (cdr rest))
              (error "find keyword arguments must be pairs" rest))
            (let ((k (car rest))
                  (v (cadr rest)))
              (cond
               ((eq? k ':test) (set! test v))
               ((eq? k ':key) (set! key v))
               (else (error "unknown find keyword" k))))
            (parse (cddr rest))))
        (letrec
            ((apply-key
              (lambda (x)
                (if key
                    (apply-islisp key (list x))
                    x)))
             (match?
              (lambda (x)
                (if test
                    (truthy? (apply-islisp test (list item (apply-key x))))
                    (eqv? item (apply-key x))))))
          (cond
           ((list? seq)
            (let loop ((rest seq))
              (if (null? rest)
                  '()
                  (let ((v (car rest)))
                    (if (match? v)
                        v
                        (loop (cdr rest)))))))
           ((vector? seq)
            (let loop ((i 0) (n (vector-length seq)))
              (if (>= i n)
                  '()
                  (let ((v (vector-ref seq i)))
                    (if (match? v)
                        v
                        (loop (+ i 1) n))))))
           ((string? seq)
            (let loop ((i 0) (n (string-length seq)))
              (if (>= i n)
                  '()
                  (let ((v (string-ref seq i)))
                    (if (match? v)
                        v
                        (loop (+ i 1) n))))))
           (else
            (error "find needs list/vector/string as sequence" seq)))))))
  ;; ISLISP §15.5: position — first index of matching element (nil if not found)
  (def 'position
    (lambda args
      (unless (>= (length args) 2)
        (error "position needs item and sequence" args))
      (let ((item (car args))
            (seq (cadr args))
            (opts (cddr args))
            (test #f)
            (key #f))
        (let parse ((rest opts))
          (unless (null? rest)
            (unless (pair? (cdr rest))
              (error "position keyword arguments must be pairs" rest))
            (let ((k (car rest))
                  (v (cadr rest)))
              (cond
               ((eq? k ':test) (set! test v))
               ((eq? k ':key) (set! key v))
               (else (error "unknown position keyword" k))))
            (parse (cddr rest))))
        (letrec
            ((apply-key
              (lambda (x)
                (if key (apply-islisp key (list x)) x)))
             (match?
              (lambda (x)
                (if test
                    (truthy? (apply-islisp test (list item (apply-key x))))
                    (eqv? item (apply-key x))))))
          (cond
           ((list? seq)
            (let loop ((rest seq) (i 0))
              (if (null? rest)
                  '()
                  (if (match? (car rest))
                      i
                      (loop (cdr rest) (+ i 1))))))
           ((vector? seq)
            (let loop ((i 0) (n (vector-length seq)))
              (if (>= i n)
                  '()
                  (if (match? (vector-ref seq i))
                      i
                      (loop (+ i 1) n)))))
           ((string? seq)
            (let loop ((i 0) (n (string-length seq)))
              (if (>= i n)
                  '()
                  (if (match? (string-ref seq i))
                      i
                      (loop (+ i 1) n)))))
           (else
            (error "position needs list/vector/string as sequence" seq)))))))
  ;; ISLISP §15.7: count — number of matching elements
  (def 'count
    (lambda args
      (unless (>= (length args) 2)
        (error "count needs item and sequence" args))
      (let ((item (car args))
            (seq (cadr args))
            (opts (cddr args))
            (test #f)
            (key #f))
        (let parse ((rest opts))
          (unless (null? rest)
            (unless (pair? (cdr rest))
              (error "count keyword arguments must be pairs" rest))
            (let ((k (car rest))
                  (v (cadr rest)))
              (cond
               ((eq? k ':test) (set! test v))
               ((eq? k ':key) (set! key v))
               (else (error "unknown count keyword" k))))
            (parse (cddr rest))))
        (letrec
            ((apply-key
              (lambda (x)
                (if key (apply-islisp key (list x)) x)))
             (match?
              (lambda (x)
                (if test
                    (truthy? (apply-islisp test (list item (apply-key x))))
                    (eqv? item (apply-key x))))))
          (cond
           ((list? seq)
            (let loop ((rest seq) (n 0))
              (if (null? rest)
                  n
                  (loop (cdr rest) (if (match? (car rest)) (+ n 1) n)))))
           ((vector? seq)
            (let loop ((i 0) (len (vector-length seq)) (n 0))
              (if (>= i len)
                  n
                  (loop (+ i 1) len (if (match? (vector-ref seq i)) (+ n 1) n)))))
           ((string? seq)
            (let loop ((i 0) (len (string-length seq)) (n 0))
              (if (>= i len)
                  n
                  (loop (+ i 1) len (if (match? (string-ref seq i)) (+ n 1) n)))))
           (else
            (error "count needs list/vector/string as sequence" seq)))))))
  (def 'eq (lambda (a b) (if (eq? a b) #t '())))
  (def 'eql (lambda (a b) (if (eqv? a b) #t '())))
  (def 'equal (lambda (a b) (if (equal? a b) #t '())))
  (def 'null (lambda (x) (if (null? x) #t '())))
  (def 'atom (lambda (x) (if (not (pair? x)) #t '())))
  (def 'numberp (lambda (x) (if (number? x) #t '())))
  (def 'zerop (lambda (x) (if (zero? x) #t '())))
  (def 'plusp
    (lambda (x)
      (if (> x 0) #t '())))
  (def 'minusp
    (lambda (x)
      (if (< x 0) #t '())))
  (def 'evenp
    (lambda (x)
      (unless (integer? x)
        (error "evenp needs an integer" x))
      (if (zero? (modulo x 2)) #t '())))
  (def 'oddp
    (lambda (x)
      (unless (integer? x)
        (error "oddp needs an integer" x))
      (if (not (zero? (modulo x 2))) #t '())))
  ;; In ISLISP, nil ('()) and t (#t) ARE symbols (§9.6), so treat them accordingly
  (def 'symbolp (lambda (x) (if (or (null? x) (eq? x #t) (symbol? x)) #t '())))
  (def 'listp (lambda (x) (if (list? x) #t '())))
  (def 'stringp (lambda (x) (if (string? x) #t '())))
  (def 'string
    (lambda args
      (if (null? args)
          ""
          (let ((x (car args)))
            (if (null? (cdr args))
                (cond
                 ((char? x) (string x))
                 ((string? x) x)
                 ((symbol? x) (symbol->string x))
                 (else
                  (error "string needs character/string/symbol or multiple characters" x)))
                (begin
                  (for-each (lambda (c) (ensure-char c "string")) args)
                  (list->string args)))))))
  (def 'create-string
    (lambda args
      (unless (or (= (length args) 1) (= (length args) 2))
        (error "create-string takes length and optional fill-char" args))
      (let ((n (car args))
            (fill (if (= (length args) 2) (cadr args) #\space)))
        (ensure-nonnegative-integer n "create-string")
        (ensure-char fill "create-string")
        (make-string n fill))))
  (def 'string=
    (lambda (a b)
      (ensure-string a "string=")
      (ensure-string b "string=")
      (if (string=? a b) #t '())))
  (def 'string/=
    (lambda (a b)
      (ensure-string a "string/=")
      (ensure-string b "string/=")
      (if (string=? a b) '() #t)))
  (def 'string<
    (lambda (a b)
      (ensure-string a "string<")
      (ensure-string b "string<")
      (if (string<? a b) #t '())))
  (def 'string<=
    (lambda (a b)
      (ensure-string a "string<=")
      (ensure-string b "string<=")
      (if (string<=? a b) #t '())))
  (def 'string>
    (lambda (a b)
      (ensure-string a "string>")
      (ensure-string b "string>")
      (if (string>? a b) #t '())))
  (def 'string>=
    (lambda (a b)
      (ensure-string a "string>=")
      (ensure-string b "string>=")
      (if (string>=? a b) #t '())))
  (def 'string-concat
    (lambda xs
      (for-each (lambda (x) (ensure-string x "string-concat")) xs)
      (apply string-append xs)))
  (def 'string-append
    (lambda xs
      (for-each (lambda (x) (ensure-string x "string-append")) xs)
      (apply string-append xs)))
  (def 'substring
    (lambda args
      (cond
       ((= (length args) 2)
        (isl-substring (car args) (cadr args) (string-length (car args))))
       ((= (length args) 3)
        (isl-substring (car args) (cadr args) (caddr args)))
       (else
        (error "substring takes string start [end]" args)))))
  (def 'char-index
    (lambda args
      (unless (or (= (length args) 2) (= (length args) 3))
        (error "char-index takes character string and optional start" args))
      (let* ((ch (car args))
             (s (cadr args))
             (start (if (= (length args) 3) (caddr args) 0)))
        (ensure-char ch "char-index")
        (string-index* s (string ch) start "char-index"))))
  (def 'string-index
    (lambda args
      (unless (or (= (length args) 2) (= (length args) 3))
        (error "string-index takes needle haystack and optional start" args))
      (let ((needle (car args))
            (haystack (cadr args))
            (start (if (= (length args) 3) (caddr args) 0)))
        (string-index* haystack needle start "string-index"))))

  )

(define (install-string-primitives! def env)
  ;; --- Phase 2-B: 文字列関数 ---
  (def 'string-length
    (lambda (s)
      (ensure-string s "string-length")
      (string-length s)))
  (def 'string-ref
    (lambda (s i)
      (ensure-string s "string-ref")
      (ensure-nonnegative-integer i "string-ref")
      (let ((n (string-length s)))
        (unless (< i n) (error "string-ref index out of range" i n))
        (string-ref s i))))
  ;; ISLISP §17.2: string-set (mutation)
  (def 'string-set
    (lambda (s i c)
      (ensure-string s "string-set")
      (ensure-nonnegative-integer i "string-set")
      (ensure-char c "string-set")
      (let ((n (string-length s)))
        (unless (< i n) (error "string-set: index out of range" i n))
        (string-set! s i c)
        c)))
  ;; ISLISP §17: make-string as alias for create-string
  (def 'make-string
    (lambda args
      (unless (or (= (length args) 1) (= (length args) 2))
        (error "make-string takes length and optional fill-char" args))
      (let ((n (car args))
            (fill (if (= (length args) 2) (cadr args) #\space)))
        (ensure-nonnegative-integer n "make-string")
        (ensure-char fill "make-string")
        (make-string n fill))))
  (def 'string-copy
    (lambda (s)
      (ensure-string s "string-copy")
      (string-copy s)))
  (def 'string-upcase
    (lambda (s)
      (ensure-string s "string-upcase")
      (string-map char-upcase s)))
  (def 'string-downcase
    (lambda (s)
      (ensure-string s "string-downcase")
      (string-map char-downcase s)))
  ;; ISLISP standard: string->list and list->string
  (def 'string->list
    (lambda (s)
      (ensure-string s "string->list")
      (string->list s)))
  (def 'list->string
    (lambda (lst)
      (unless (list? lst)
        (error "list->string needs a list" lst))
      (for-each (lambda (c) (ensure-char c "list->string")) lst)
      (list->string lst)))

  )

(define (install-char-primitives! def env)
  ;; --- Phase 2-C: 文字比較関数 ---
  (def 'char=
    (lambda (a b)
      (ensure-char a "char=") (ensure-char b "char=")
      (if (char=? a b) #t '())))
  (def 'char/=
    (lambda (a b)
      (ensure-char a "char/=") (ensure-char b "char/=")
      (if (char=? a b) '() #t)))
  (def 'char<
    (lambda (a b)
      (ensure-char a "char<") (ensure-char b "char<")
      (if (char<? a b) #t '())))
  (def 'char>
    (lambda (a b)
      (ensure-char a "char>") (ensure-char b "char>")
      (if (char>? a b) #t '())))
  (def 'char<=
    (lambda (a b)
      (ensure-char a "char<=") (ensure-char b "char<=")
      (if (char<=? a b) #t '())))
  (def 'char>=
    (lambda (a b)
      (ensure-char a "char>=") (ensure-char b "char>=")
      (if (char>=? a b) #t '())))
  (def 'char-upcase
    (lambda (c)
      (ensure-char c "char-upcase")
      (char-upcase c)))
  (def 'char-downcase
    (lambda (c)
      (ensure-char c "char-downcase")
      (char-downcase c)))

  (def 'char->integer
    (lambda (ch)
      (cond
       ((char? ch)
        (char->integer ch))
       ((and (string? ch) (= (string-length ch) 1))
        (char->integer (string-ref ch 0)))
       (else
        (error "char->integer needs a character or one-character string" ch)))))
  (def 'integer->char
    (lambda (code)
      (unless (integer? code)
        (error "integer->char needs an integer" code))
      (unless (and (>= code 0) (<= code #x10FFFF))
        (error "integer->char code out of range (0..1114111)" code))
      (integer->char code)))
  ;; ISLISP §12.3 canonical hyphenated names
  (def 'char-to-integer
    (lambda (ch)
      (cond
       ((char? ch) (char->integer ch))
       ((and (string? ch) (= (string-length ch) 1))
        (char->integer (string-ref ch 0)))
       (else (error "char-to-integer needs a character" ch)))))
  (def 'integer-to-char
    (lambda (code)
      (unless (integer? code)
        (error "integer-to-char needs an integer" code))
      (unless (and (>= code 0) (<= code #x10FFFF))
        (error "integer-to-char code out of range (0..1114111)" code))
      (integer->char code)))
  ;; ISLISP §17.4: string-to-list, list-to-string
  (def 'string-to-list
    (lambda (s)
      (unless (string? s) (error "string-to-list needs a string" s))
      (string->list s)))
  (def 'list-to-string
    (lambda (lst)
      (unless (list? lst) (error "list-to-string needs a list" lst))
      (for-each
       (lambda (c)
         (unless (char? c)
           (error "list-to-string: list must contain only characters" c)))
       lst)
      (list->string lst)))
  (def 'length isl-length)
  )

(define (install-io-primitives! def env)
  ;; ---- 4-B: print (stream-aware), write, terpri ----
  (def 'print
    (lambda args
      (unless (pair? args) (error "print needs an object" args))
      (let ((obj  (car args))
            (port (if (pair? (cdr args)) (cadr args) (current-output-port))))
        (unless (output-port? port) (error "print second arg must be an output stream" port))
        (write obj port)
        (newline port)
        obj)))
  (def 'write
    (lambda args
      (unless (pair? args) (error "write needs an object" args))
      (let ((obj  (car args))
            (port (if (pair? (cdr args)) (cadr args) (current-output-port))))
        (unless (output-port? port) (error "write second arg must be an output stream" port))
        (write obj port)
        obj)))
  (def 'terpri
    (lambda args
      (let ((port (if (pair? args) (car args) (current-output-port))))
        (unless (output-port? port) (error "terpri arg must be an output stream" port))
        (newline port)
        '())))
  ;; ISLISP §18: newline — output a newline to stream (same as terpri but ISLISP name)
  (def 'newline
    (lambda args
      (let ((port (if (pair? args) (car args) (current-output-port))))
        (unless (output-port? port) (error "newline: arg must be an output stream" port))
        (newline port)
        '())))
  ;; ---- §18.5: prin1, princ, fresh-line, write-string ----
  (def 'prin1
    (lambda args
      (unless (pair? args) (error "prin1 needs an object" args))
      (let ((obj  (car args))
            (port (if (pair? (cdr args)) (cadr args) (current-output-port))))
        (unless (output-port? port) (error "prin1 second arg must be an output stream" port))
        (write obj port)
        obj)))
  (def 'princ
    (lambda args
      (unless (pair? args) (error "princ needs an object" args))
      (let ((obj  (car args))
            (port (if (pair? (cdr args)) (cadr args) (current-output-port))))
        (unless (output-port? port) (error "princ second arg must be an output stream" port))
        (display obj port)
        obj)))
  (def 'fresh-line
    (lambda args
      (let ((port (if (pair? args) (car args) (current-output-port))))
        (unless (output-port? port) (error "fresh-line arg must be an output stream" port))
        (newline port)
        '())))
  (def 'write-string
    (lambda args
      (unless (pair? args) (error "write-string needs a string" args))
      (let ((s    (car args))
            (port (if (pair? (cdr args)) (cadr args) (current-output-port))))
        (ensure-string s "write-string")
        (unless (output-port? port) (error "write-string second arg must be an output stream" port))
        (display s port)
        '())))
  ;; ---- §18.7: stream-ready-p ----
  (def 'stream-ready-p
    (lambda args
      (unless (pair? args) (error "stream-ready-p needs a stream" args))
      (let ((p (car args)))
        (unless (input-port? p) (error "stream-ready-p expects an input stream" p))
        #t)))
  ;; ---- 4-A / 5-A: read-char, write-char, peek-char (EOF → <end-of-stream>) ----
  (def 'read-char
    (lambda args
      (let ((port (if (pair? args) (car args) (current-input-port))))
        (unless (input-port? port) (error "read-char needs an input stream" port))
        (let ((ch (read-char port)))
          (if (eof-object? ch)
              (eof-stream-error "read-char")
              ch)))))
  (def 'write-char
    (lambda args
      (unless (pair? args) (error "write-char needs a character" args))
      (let ((ch   (car args))
            (port (if (pair? (cdr args)) (cadr args) (current-output-port))))
        (unless (char? ch) (error "write-char first arg must be a character" ch))
        (unless (output-port? port) (error "write-char second arg must be an output stream" port))
        (write-char ch port)
        ch)))
  (def 'peek-char
    (lambda args
      (let ((port (if (pair? args) (car args) (current-input-port))))
        (unless (input-port? port) (error "peek-char needs an input stream" port))
        (let ((ch (peek-char port)))
          (if (eof-object? ch)
              '()     ; EOF → nil (ISLISP: may also signal <end-of-stream>)
              ch)))))
  ;; ISLISP §18.1: preview-char — alias for peek-char
  (def 'preview-char
    (lambda args
      (let ((port (if (pair? args) (car args) (current-input-port))))
        (unless (input-port? port) (error "preview-char needs an input stream" port))
        (let ((ch (peek-char port)))
          (if (eof-object? ch)
              '()
              ch)))))
  ;; ---- write-line (with optional stream) ----
  (def 'write-line
    (lambda (s . maybe-stream)
      (ensure-string s "write-line")
      (if (null? maybe-stream)
          (begin
            (display s)
            (newline)
            s)
          (if (= (length maybe-stream) 1)
              (let ((stream (car maybe-stream)))
                (display s stream)
                (newline stream)
                s)
              (error "write-line takes string and optional stream" maybe-stream)))))
  ;; ---- 4-F / 5-A: read (0 or 1 arg), read-line (0, 1, or 2 args) ----
  (def 'read
    (lambda args
      (cond
       ((null? args)
        (let ((obj (isl-read (current-input-port))))
          (if (eof-object? obj)
              (eof-stream-error "read")
              obj)))
       ((= (length args) 1)
        (let ((obj (isl-read (car args))))
          (if (eof-object? obj)
              (eof-stream-error "read")
              obj)))
       (else
        (error "read takes zero or one stream argument" args)))))
  (def 'read-line
    (lambda args
      (cond
       ((null? args)
        (let ((line (read-line)))
          (if (eof-object? line)
              (eof-stream-error "read-line")
              line)))
       ((= (length args) 1)
        (let ((line (read-line (car args))))
          (if (eof-object? line)
              (eof-stream-error "read-line")
              line)))
       ((= (length args) 2)
        (let ((line (read-line (car args)))
              (eof-error-p (cadr args)))
          (if (eof-object? line)
              (if (truthy? eof-error-p)
                  (eof-stream-error "read-line")
                  '())
              line)))
       (else
        (error "read-line takes optional stream and optional eof-error-p" args)))))
  ;; ---- 4-C: open-input-stream, open-output-stream, open-io-stream, close ----
  (def 'open-input-stream
    (lambda (filename)
      (ensure-string filename "open-input-stream")
      (open-input-file filename)))
  ;; ISLISP §18.2 canonical names: open-input-file, open-output-file, open-io-file
  (def 'open-input-file
    (lambda (filename)
      (ensure-string filename "open-input-file")
      (open-input-file filename)))
  (def 'open-output-stream
    (lambda (filename)
      (ensure-string filename "open-output-stream")
      (open-output-file filename :if-exists :supersede
                                 :if-does-not-exist :create)))
  (def 'open-output-file
    (lambda (filename)
      (ensure-string filename "open-output-file")
      (open-output-file filename :if-exists :supersede
                                 :if-does-not-exist :create)))
  (def 'open-io-stream
    (lambda (filename)
      (ensure-string filename "open-io-stream")
      ;; open-input-output-file opens the file for reading and writing
      (open-file filename (logior O_RDWR O_CREAT) #o666)))
  (def 'open-io-file
    (lambda (filename)
      (ensure-string filename "open-io-file")
      (open-file filename (logior O_RDWR O_CREAT) #o666)))
  (def 'close
    (lambda (stream)
      (cond
       ((input-port? stream)  (close-input-port stream))
       ((output-port? stream) (close-output-port stream))
       (else (error "close needs a stream" stream)))
      '()))
  ;; ISLISP §18.7: finish-output — flush output buffer
  (def 'finish-output
    (lambda args
      (let ((port (if (pair? args) (car args) (current-output-port))))
        (unless (output-port? port) (error "finish-output needs an output stream" port))
        (flush port)
        '())))
  ;; ---- §18.4: string streams ----
  (def 'create-string-input-stream
    (lambda (s)
      (ensure-string s "create-string-input-stream")
      (open-input-string s)))
  (def 'create-string-output-stream
    (lambda ()
      (open-output-string)))
  (def 'get-output-stream-string
    (lambda (stream)
      (unless (output-port? stream)
        (error "get-output-stream-string needs a string output stream" stream))
      (get-output-string stream)))
  ;; ---- §23.3: granular formatted output (the ISLISP format-* family) ----
  (def 'format-object
    (lambda (stream obj quote-p)
      (unless (output-port? stream) (error "format-object needs an output stream" stream))
      (if (truthy? quote-p) (write obj stream) (display obj stream))
      '()))
  (def 'format-char
    (lambda (stream ch)
      (unless (output-port? stream) (error "format-char needs an output stream" stream))
      (unless (char? ch) (error "format-char needs a character" ch))
      (write-char ch stream)
      '()))
  (def 'format-integer
    (lambda (stream n radix)
      (unless (output-port? stream) (error "format-integer needs an output stream" stream))
      (unless (integer? n) (error "format-integer needs an integer" n))
      (unless (and (integer? radix) (<= 2 radix 36))
        (error "format-integer radix must be 2..36" radix))
      (display (number->string n radix) stream)
      '()))
  (def 'format-float
    (lambda (stream x)
      (unless (output-port? stream) (error "format-float needs an output stream" stream))
      (unless (real? x) (error "format-float needs a real number" x))
      (display (exact->inexact x) stream)
      '()))
  (def 'format-fresh-line
    (lambda (stream)
      (unless (output-port? stream) (error "format-fresh-line needs an output stream" stream))
      (newline stream)
      '()))
  (def 'format-tab
    (lambda (stream column)
      (unless (output-port? stream) (error "format-tab needs an output stream" stream))
      (unless (and (integer? column) (>= column 0))
        (error "format-tab column must be a non-negative integer" column))
      ;; A string stream cannot track its column, so approximate by emitting
      ;; `column` spaces (enough for the common "indent to N" use).
      (let loop ((i 0)) (when (< i column) (write-char #\space stream) (loop (+ i 1))))
      '()))
  ;; ISLISP §18.3: read-byte, write-byte
  (def 'read-byte
    (lambda args
      (let ((port (if (pair? args) (car args) (current-input-port))))
        (unless (input-port? port) (error "read-byte needs an input stream" port))
        (let ((b (read-u8 port)))
          (if (eof-object? b)
              (eof-stream-error "read-byte")
              b)))))
  (def 'write-byte
    (lambda args
      (unless (pair? args) (error "write-byte needs a byte" args))
      (let ((b    (car args))
            (port (if (pair? (cdr args)) (cadr args) (current-output-port))))
        (unless (and (integer? b) (>= b 0) (<= b 255))
          (error "write-byte: argument must be a byte (0-255)" b))
        (unless (output-port? port) (error "write-byte needs an output stream" port))
        (write-u8 b port)
        b)))
  ;; ---- 4-D: input-stream-p, output-stream-p ----
  (def 'streamp
    (lambda (obj)
      (if (or (input-port? obj) (output-port? obj)) #t '())))
  (def 'input-stream-p
    (lambda (obj)
      (if (input-port? obj) #t '())))
  (def 'output-stream-p
    (lambda (obj)
      (if (output-port? obj) #t '())))
  )

(define (install-condition-primitives! def env)
  ;; ---- 5-B: signal-condition, condition-message, condition-continuable ----
  (def 'signal-condition
    (lambda (cond-obj . rest)
      ;; ISLISP: (signal-condition condition continuable)
      ;; continuable arg is accepted but we use isl-condition-continuable? instead
      (unless (isl-condition? cond-obj)
        (error "signal-condition needs an isl condition object" cond-obj))
      (isl-signal-condition cond-obj)))
  ;; condition-message is a convenience accessor (non-standard).  For a
  ;; simple-error it returns the report message with the format string applied
  ;; to its arguments; if formatting fails (e.g. surplus irritants) it falls
  ;; back to the raw format string.
  (def 'condition-message
    (lambda (cond-obj)
      (cond
       ((isl-condition? cond-obj)
        (let ((msg (isl-condition-message cond-obj))
              (irr (isl-condition-irritants cond-obj)))
          (if (and (string? msg) (pair? irr))
              (guard (e (else msg)) (render-format msg irr))
              msg)))
       ((condition? cond-obj)
        (condition/report-string cond-obj))
       (else
        (write-to-string cond-obj)))))
  (def 'condition-continuable
    (lambda (cond-obj)
      (unless (isl-condition? cond-obj)
        (error "condition-continuable needs an isl condition object" cond-obj))
      (if (isl-condition-continuable? cond-obj) #t '())))
  ;; ISLISP §29: condition-continuable-p (standard name alias)
  (def 'condition-continuable-p
    (lambda (cond-obj)
      (unless (isl-condition? cond-obj)
        (error "condition-continuable-p needs an isl condition object" cond-obj))
      (if (isl-condition-continuable? cond-obj) #t '())))
  ;; ---- 5-B': typed condition accessors (ISLISP §22) ----
  (def 'simple-error-format-string
    (lambda (c)
      (unless (isl-condition? c) (error "simple-error-format-string: not a condition" c))
      (isl-condition-message c)))
  (def 'simple-error-format-arguments
    (lambda (c)
      (unless (isl-condition? c) (error "simple-error-format-arguments: not a condition" c))
      (isl-condition-irritants c)))
  (def 'arithmetic-error-operation
    (lambda (c)
      (unless (isl-condition? c) (error "arithmetic-error-operation: not a condition" c))
      (isl-condition-extra-ref c 'operation)))
  (def 'arithmetic-error-operands
    (lambda (c)
      (unless (isl-condition? c) (error "arithmetic-error-operands: not a condition" c))
      (isl-condition-extra-ref c 'operands (isl-condition-irritants c))))
  (def 'domain-error-object
    (lambda (c)
      (unless (isl-condition? c) (error "domain-error-object: not a condition" c))
      (isl-condition-extra-ref c 'object)))
  (def 'domain-error-expected-class
    (lambda (c)
      (unless (isl-condition? c) (error "domain-error-expected-class: not a condition" c))
      (let ((cs (isl-condition-extra-ref c 'expected-class)))
        (if (symbol? cs)
            (let ((p (frame-find-pair env (resolve-binding-symbol cs))))
              (if p (cdr p) cs))
            cs))))
  (def 'undefined-entity-name
    (lambda (c)
      (unless (isl-condition? c) (error "undefined-entity-name: not a condition" c))
      (isl-condition-extra-ref c 'name)))
  (def 'undefined-entity-namespace
    (lambda (c)
      (unless (isl-condition? c) (error "undefined-entity-namespace: not a condition" c))
      (isl-condition-extra-ref c 'namespace)))
  ;; ISLISP §22: report-condition — write the condition's report to a stream.
  ;; The simple-error report applies the format string to its arguments.
  (def 'report-condition
    (lambda (c stream)
      (unless (isl-condition? c) (error "report-condition: not a condition" c))
      (let ((msg (isl-condition-message c))
            (irr (isl-condition-irritants c)))
        (display (if (and (string? msg) (pair? irr)) (render-format msg irr) msg)
                 stream))
      '()))
  ;; ---- 5-C: continue-condition ----
  (def 'continue-condition
    (lambda args
      (unless (and (pair? args) (isl-condition? (car args)))
        (error "continue-condition needs an isl condition object" args))
      (let ((cond-obj (car args))
            (value    (if (pair? (cdr args)) (cadr args) '())))
        (unless (isl-condition-continuable? cond-obj)
          (error "condition is not continuable" cond-obj))
        value)))
  ;; ---- 5-D: cerror ----
  (def 'cerror
    (lambda (continue-string error-string . irritants)
      (unless (string? continue-string)
        (error "cerror continue-string must be string" continue-string))
      (unless (string? error-string)
        (error "cerror error-string must be string" error-string))
      (isl-signal-condition
       (make-isl-condition
        (resolve-binding-symbol '<simple-error>)
        error-string #t (cons continue-string irritants)))))
  ;; ISLISP §22: make-condition
  (def 'make-condition
    (lambda (class . rest)
      ;; (make-condition class-or-symbol message) or (make-condition class-or-symbol message continuable)
      (let* ((class-sym (cond
                         ((and (class? class) (class-name class)) (class-name class))
                         ((symbol? class) class)
                         (else (error "make-condition: first arg must be a condition class" class))))
             ;; Resolve to fully-qualified symbol for consistent class hierarchy lookup
             (resolved-sym (resolve-binding-symbol (string->symbol (symbol-base-name class-sym))))
             (msg  (if (pair? rest) (car rest) "condition"))
             (cont (if (and (pair? rest) (pair? (cdr rest))) (truthy? (cadr rest)) #f)))
        (unless (string? msg)
          (error "make-condition: message must be a string" msg))
        (make-isl-condition resolved-sym msg cont '()))))
  ;; ISLISP §22: condition-class — returns the class of a condition object
  (def 'condition-class
    (lambda (cond-obj)
      (cond
       ((isl-condition? cond-obj)
        ;; Return the ISL class object for the condition's class symbol
        (let* ((class-sym (isl-condition-class cond-obj))
               (resolved (resolve-binding-symbol class-sym))
               (pair (frame-find-pair env resolved)))
          (if pair (cdr pair) class-sym)))
       (else
        (error "condition-class: argument must be a condition" cond-obj)))))
  ;; ---- 5-B: error function (signals <simple-error>) ----
  (def 'error
    (lambda (msg . irritants)
      (unless (string? msg) (error "error first arg must be string" msg))
      (isl-signal-condition
       (make-isl-condition
        (resolve-binding-symbol '<simple-error>)
        msg #f irritants))))
  )

(define (install-system-primitives! def env)
  (def 'debug
    (lambda args
      (cond
       ((null? args)
        *debug-mode*)
       ((= (length args) 1)
        (set! *debug-mode* (truthy? (car args)))
        *debug-mode*)
       (else
        (error "debug takes zero or one argument" args)))))
  (def 'break
    (lambda args
      (let ((msg
             (cond
              ((null? args) "break")
              ((string? (car args))
               (if (= (length args) 1)
                   (car args)
                   (render-format (car args) (cdr args))))
              (else
               (write-to-string args)))))
        (break-loop env msg)
        '())))
  (def 'probe-file
    (lambda (filename)
      (unless (string? filename)
        (error "probe-file filename must be a string" filename))
      (if (file-exists? filename)
          filename
          '())))
  (def 'delete-file
    (lambda (filename)
      (unless (string? filename)
        (error "delete-file filename must be a string" filename))
      (if (file-exists? filename)
          (begin
            (sys-unlink filename)
            #t)
          '())))
  (def 'make-directory*
    (lambda (path)
      (unless (string? path)
        (error "make-directory* path must be a string" path))
      (gauche-make-directory* path)
      #t))
  (def 'chmod-file
    (lambda (path mode)
      (unless (string? path)
        (error "chmod-file path must be a string" path))
      (unless (string? mode)
        (error "chmod-file mode must be an octal string" mode))
      (sys-chmod path (parse-octal-string mode))
      #t))
  (def 'rename-file
    (lambda (source dest)
      (unless (string? source)
        (error "rename-file source must be a string" source))
      (unless (string? dest)
        (error "rename-file dest must be a string" dest))
      (sys-rename source dest)
      #t))
  (def 'copy-file
    (lambda (source dest)
      (unless (string? source)
        (error "copy-file source must be a string" source))
      (unless (string? dest)
        (error "copy-file dest must be a string" dest))
      (gauche-copy-file source dest)
      #t))
  (def 'directory-list
    (lambda (path)
      (unless (string? path)
        (error "directory-list path must be a string" path))
      (gauche-directory-list path :children? #t)))
  (def 'glob
    (lambda (pattern)
      (unless (string? pattern)
        (error "glob pattern must be a string" pattern))
      (sys-glob pattern)))
  (def 'glob-newest-first
    (lambda (pattern)
      (unless (string? pattern)
        (error "glob-newest-first pattern must be a string" pattern))
      (sort (sys-glob pattern)
            (lambda (a b)
              (> (file-mtime a) (file-mtime b))))))
  (def 'file-size
    (lambda (path)
      (unless (string? path)
        (error "file-size path must be a string" path))
      (gauche-file-size path)))
  (def 'file-readable-p
    (lambda (path)
      (unless (string? path)
        (error "file-readable-p path must be a string" path))
      (if (and (file-exists? path)
               (gauche-file-is-readable? path))
          #t
          #f)))
  (def 'file-executable-p
    (lambda (path)
      (unless (string? path)
        (error "file-executable-p path must be a string" path))
      (if (and (file-exists? path)
               (gauche-file-is-executable? path))
          #t
          #f)))
  (def 'file-symlink-p
    (lambda (path)
      (unless (string? path)
        (error "file-symlink-p path must be a string" path))
      (if (and (file-exists? path)
               (gauche-file-is-symlink? path))
          #t
          #f)))
  (def 'directory-path-p
    (lambda (path)
      (unless (string? path)
        (error "directory-path-p path must be a string" path))
      (if (and (file-exists? path)
               (eq? (gauche-file-type path) 'directory))
          #t
          #f)))
  (def 'command-available-p
    (lambda (cmd)
      (unless (string? cmd)
        (error "command-available-p cmd must be a string" cmd))
      (if (command-available? cmd) #t #f)))
  (def 'copy-file-to-stdout
    (lambda (path)
      (unless (string? path)
        (error "copy-file-to-stdout path must be a string" path))
      (call-with-input-file path
        (lambda (in)
          (copy-port in (current-output-port))))
      #t))
  (def 'copy-stdin-to-file
    (lambda (path)
      (unless (string? path)
        (error "copy-stdin-to-file path must be a string" path))
      (call-with-output-file path
        (lambda (out)
          (copy-port (current-input-port) out))
        :if-exists :supersede)
      #t))
  (def 'capture-output-string
    (lambda (thunk)
      (unless (or (primitive? thunk) (closure? thunk))
        (error "capture-output-string thunk must be callable" thunk))
      (let ((p (open-output-string)))
        (parameterize ((current-output-port p))
          (force-value (apply-islisp thunk '())))
        (get-output-string p))))
  (def 'uname
    (lambda ()
      (sys-uname)))
  (def 'sleep-seconds
    (lambda (seconds)
      (unless (number? seconds)
        (error "sleep-seconds seconds must be numeric" seconds))
      (unless (>= seconds 0)
        (error "sleep-seconds seconds must be non-negative" seconds))
      (sys-nanosleep (inexact->exact (round (* seconds 1000000000))))
      #t))
  (def 'base64-encode-file
    (lambda (path)
      (unless (string? path)
        (error "base64-encode-file path must be a string" path))
      (call-with-input-file path
        (lambda (in)
          (gauche-base64-encode-string (port->string in) :line-width #f)))))
  (def 'generate-token
    (lambda ()
      (gauche-uuid->string (gauche-uuid4))))
  (def 'date-utc-format
    (lambda (fmt)
      (unless (string? fmt)
        (error "date-utc-format fmt must be a string" fmt))
      (gauche-date->string
       (gauche-time-utc->date
        (gauche-current-time gauche-time-utc)
        0)
       fmt)))
  (def 'date-utc-iso8601
    (lambda ()
      (gauche-date->string
       (gauche-time-utc->date
        (gauche-current-time gauche-time-utc)
        0)
       "~Y-~m-~dT~H:~M:~SZ")))
  (def 'date-http-from-epoch
    (lambda (epoch-sec)
      (unless (integer? epoch-sec)
        (error "date-http-from-epoch epoch-sec must be an integer" epoch-sec))
      (gauche-date->string
       (gauche-time-utc->date
        (gauche-make-time gauche-time-utc 0 epoch-sec)
        0)
       (http-date-format))))
  (def 'epoch-from-http-date
    (lambda (http-date)
      (unless (string? http-date)
        (error "epoch-from-http-date http-date must be a string" http-date))
      (http-date->utc-seconds http-date)))
  (def 'getenv
    (lambda (name)
      (unless (string? name)
        (error "getenv name must be a string" name))
      (let ((v (sys-getenv name)))
        (if v v '()))))
  (def 'setenv
    (lambda args
      (unless (or (= (length args) 2) (= (length args) 3))
        (error "setenv takes name, value, and optional overwrite flag" args))
      (let ((name (car args))
            (value (cadr args))
            (overwrite (if (= (length args) 3) (caddr args) #t)))
        (unless (string? name)
          (error "setenv name must be a string" name))
        (unless (string? value)
          (error "setenv value must be a string" value))
        (sys-setenv name value (truthy? overwrite))
        value)))
  (def 'system
    (lambda (command)
      (unless (string? command)
        (error "system command must be a string" command))
      (let ((raw (sys-system command)))
        (if (and (integer? raw) (>= raw 0))
            (quotient raw 256)
            raw))))
  (def 'system-timeout
    (lambda (command timeout-sec)
      (unless (string? command)
        (error "system-timeout command must be a string" command))
      (unless (and (integer? timeout-sec) (>= timeout-sec 0))
        (error "system-timeout timeout-sec must be non-negative integer" timeout-sec))
      (let* ((p (run-process "sh" "-c" command :output :null :error :null))
             (waiter (gauche-make-thread (lambda () (process-wait p)))))
        (gauche-thread-start! waiter)
        (if (eq? (gauche-thread-join! waiter timeout-sec 'timeout) 'timeout)
            (begin
              (guard (e (else #f))
                (process-kill p))
              (if (eq? (gauche-thread-join! waiter 1 'timeout) 'timeout)
                  (guard (e (else #f))
                    (process-wait p))
                  #f)
              (list (decode-process-exit-status (process-exit-status p)) #t))
            (list (decode-process-exit-status (process-exit-status p)) #f)))))
  )

(define (install-database-primitives! def env)
  (def 'sqlite-open
    (lambda (path)
      (unless (string? path)
        (error "sqlite-open path must be a string" path))
      (make-sqlite-connection path)))
  (def 'sqlite-db-p
    (lambda (obj)
      (sqlite-connection? obj)))
  (def 'sqlite-close
    (lambda (db)
      (unless (sqlite-connection? db)
        (error "sqlite-close needs sqlite db object" db))
      #t))
  (def 'sqlite-exec
    (lambda (db sql)
      (unless (sqlite-connection? db)
        (error "sqlite-exec needs sqlite db object" db))
      (sqlite-run (sqlite-connection-path db) sql)
      #t))
  (def 'sqlite-query
    (lambda (db sql)
      (unless (sqlite-connection? db)
        (error "sqlite-query needs sqlite db object" db))
      (sql-output->rows (sqlite-run (sqlite-connection-path db) sql) #\x1f)))
  (def 'sqlite-query-one
    (lambda (db sql)
      (unless (sqlite-connection? db)
        (error "sqlite-query-one needs sqlite db object" db))
      (let ((rows (sql-output->rows (sqlite-run (sqlite-connection-path db) sql) #\x1f)))
        (if (null? rows)
            '()
            (if (null? (car rows))
                '()
                (caar rows))))))
  (def 'postgres-open
    (lambda (conninfo)
      (unless (string? conninfo)
        (error "postgres-open connection info must be a string" conninfo))
      (make-postgres-connection conninfo)))
  (def 'postgres-db-p
    (lambda (obj)
      (postgres-connection? obj)))
  (def 'postgres-close
    (lambda (db)
      (unless (postgres-connection? db)
        (error "postgres-close needs postgres db object" db))
      #t))
  (def 'postgres-exec
    (lambda (db sql)
      (unless (postgres-connection? db)
        (error "postgres-exec needs postgres db object" db))
      (postgres-run (postgres-connection-info db) sql)
      #t))
  (def 'postgres-query
    (lambda (db sql)
      (unless (postgres-connection? db)
        (error "postgres-query needs postgres db object" db))
      (sql-output->rows (postgres-run (postgres-connection-info db) sql) #\x1f #\x1e)))
  (def 'postgres-query-one
    (lambda (db sql)
      (unless (postgres-connection? db)
        (error "postgres-query-one needs postgres db object" db))
      (let ((rows (sql-output->rows (postgres-run (postgres-connection-info db) sql) #\x1f #\x1e)))
        (if (null? rows)
            '()
            (if (null? (car rows))
                '()
                (caar rows))))))
  (def 'mysql-open
    (lambda (host port user password database)
      (make-mysql-connection host port user password database)))
  (def 'mysql-db-p
    (lambda (obj)
      (mysql-connection? obj)))
  (def 'mysql-close
    (lambda (db)
      (unless (mysql-connection? db)
        (error "mysql-close needs mysql db object" db))
      #t))
  (def 'mysql-exec
    (lambda (db sql)
      (unless (mysql-connection? db)
        (error "mysql-exec needs mysql db object" db))
      (mysql-run (mysql-connection-host db)
                 (mysql-connection-port db)
                 (mysql-connection-user db)
                 (mysql-connection-password db)
                 (mysql-connection-database db)
                 sql)
      #t))
  (def 'mysql-query
    (lambda (db sql)
      (unless (mysql-connection? db)
        (error "mysql-query needs mysql db object" db))
      (sql-output->rows
       (mysql-run (mysql-connection-host db)
                  (mysql-connection-port db)
                  (mysql-connection-user db)
                  (mysql-connection-password db)
                  (mysql-connection-database db)
                  sql)
       #\tab)))
  (def 'mysql-query-one
    (lambda (db sql)
      (unless (mysql-connection? db)
        (error "mysql-query-one needs mysql db object" db))
      (let ((rows
             (sql-output->rows
              (mysql-run (mysql-connection-host db)
                         (mysql-connection-port db)
                         (mysql-connection-user db)
                         (mysql-connection-password db)
                         (mysql-connection-database db)
                         sql)
              #\tab)))
        (if (null? rows)
            '()
            (if (null? (car rows))
                '()
                (caar rows))))))
  )

(define (install-socket-primitives! def env)
  (def 'tcp-connect
    (lambda (host port)
      (unless (string? host)
        (error "tcp-connect host must be a string" host))
      (unless (and (integer? port) (>= port 1) (<= port 65535))
        (error "tcp-connect port must be integer in 1..65535" port))
      (let ((sock (make-client-socket 'inet host port)))
        (make-tcp-connection sock
                             (socket-input-port sock)
                             (socket-output-port sock)))))
  (def 'tcp-listen
    (lambda (port)
      (unless (and (integer? port) (>= port 1) (<= port 65535))
        (error "tcp-listen port must be integer in 1..65535" port))
      (let ((sock (make-server-socket 'inet port :reuse-addr? #t)))
        (make-tcp-listener sock))))
  (def 'tcp-listener-p
    (lambda (obj)
      (tcp-listener? obj)))
  (def 'tcp-accept
    (lambda (listener)
      (unless (tcp-listener? listener)
        (error "tcp-accept needs tcp listener object" listener))
      (call-with-values
          (lambda ()
            (socket-accept (tcp-listener-socket listener)))
        (lambda vals
          (let ((sock (car vals)))
            (make-tcp-connection sock
                                 (socket-input-port sock)
                                 (socket-output-port sock)))))))
  (def 'tcp-listener-close
    (lambda (listener)
      (unless (tcp-listener? listener)
        (error "tcp-listener-close needs tcp listener object" listener))
      (guard (e (else #t))
        (socket-close (tcp-listener-socket listener)))
      #t))
  (def 'tcp-connection-p
    (lambda (obj)
      (tcp-connection? obj)))
  (def 'tcp-send
    (lambda (conn text)
      (unless (tcp-connection? conn)
        (error "tcp-send needs tcp connection object" conn))
      (unless (string? text)
        (error "tcp-send text must be a string" text))
      (display text (tcp-connection-output conn))
      (flush (tcp-connection-output conn))
      #t))
  (def 'tcp-send-line
    (lambda (conn text)
      (unless (tcp-connection? conn)
        (error "tcp-send-line needs tcp connection object" conn))
      (unless (string? text)
        (error "tcp-send-line text must be a string" text))
      (display text (tcp-connection-output conn))
      (newline (tcp-connection-output conn))
      (flush (tcp-connection-output conn))
      #t))
  (def 'tcp-send-file
    (lambda (conn path)
      (unless (tcp-connection? conn)
        (error "tcp-send-file needs tcp connection object" conn))
      (unless (string? path)
        (error "tcp-send-file path must be a string" path))
      (call-with-input-file
       path
       (lambda (in)
         (let loop ((b (read-byte in)))
           (unless (eof-object? b)
             (write-byte b (tcp-connection-output conn))
             (loop (read-byte in))))))
      (flush (tcp-connection-output conn))
      #t))
  (def 'tcp-receive-line
    (lambda args
      (unless (or (= (length args) 1) (= (length args) 2))
        (error "tcp-receive-line takes connection and optional eof-error-p" args))
      (let ((conn (car args))
            (eof-error-p (if (= (length args) 2) (cadr args) #t)))
        (unless (tcp-connection? conn)
          (error "tcp-receive-line needs tcp connection object" conn))
        (let ((line (read-line (tcp-connection-input conn))))
          (if (eof-object? line)
              (if (truthy? eof-error-p)
                  (error "tcp-receive-line reached EOF")
                  '())
              line)))))
  (def 'tcp-receive-char
    (lambda args
      (unless (or (= (length args) 1) (= (length args) 2))
        (error "tcp-receive-char takes connection and optional eof-error-p" args))
      (let ((conn (car args))
            (eof-error-p (if (= (length args) 2) (cadr args) #t)))
        (unless (tcp-connection? conn)
          (error "tcp-receive-char needs tcp connection object" conn))
        (let ((ch (read-char (tcp-connection-input conn))))
          (if (eof-object? ch)
              (if (truthy? eof-error-p)
                  (error "tcp-receive-char reached EOF")
                  '())
              ch)))))
  (def 'tcp-send-byte
    (lambda (conn byte)
      (unless (tcp-connection? conn)
        (error "tcp-send-byte needs tcp connection object" conn))
      (unless (and (integer? byte) (>= byte 0) (<= byte 255))
        (error "tcp-send-byte value must be integer in 0..255" byte))
      (write-byte byte (tcp-connection-output conn))
      (flush (tcp-connection-output conn))
      #t))
  (def 'tcp-receive-byte
    (lambda args
      (unless (or (= (length args) 1) (= (length args) 2))
        (error "tcp-receive-byte takes connection and optional eof-error-p" args))
      (let ((conn (car args))
            (eof-error-p (if (= (length args) 2) (cadr args) #t)))
        (unless (tcp-connection? conn)
          (error "tcp-receive-byte needs tcp connection object" conn))
        (let ((b (read-byte (tcp-connection-input conn))))
          (if (eof-object? b)
              (if (truthy? eof-error-p)
                  (error "tcp-receive-byte reached EOF")
                  '())
              b)))))
  (def 'tcp-flush
    (lambda (conn)
      (unless (tcp-connection? conn)
        (error "tcp-flush needs tcp connection object" conn))
      (flush (tcp-connection-output conn))
      #t))
  (def 'tcp-close
    (lambda (conn)
      (unless (tcp-connection? conn)
        (error "tcp-close needs tcp connection object" conn))
      (guard (e (else #t))
        (close-input-port (tcp-connection-input conn)))
      (guard (e (else #t))
        (close-output-port (tcp-connection-output conn)))
      (guard (e (else #t))
        (socket-close (tcp-connection-socket conn)))
      #t))
  (def 'tls-listen
    (lambda args
      (unless (or (= (length args) 3) (= (length args) 4))
        (error "tls-listen takes port cert-file key-file [key-password]" args))
      (let ((port (car args))
            (cert-file (cadr args))
            (key-file (caddr args))
            (key-password (if (= (length args) 4) (list-ref args 3) "")))
        (unless (and (integer? port) (>= port 1) (<= port 65535))
          (error "tls-listen port must be integer in 1..65535" port))
        (unless (string? cert-file)
          (error "tls-listen cert-file must be a string" cert-file))
        (unless (string? key-file)
          (error "tls-listen key-file must be a string" key-file))
        (unless (string? key-password)
          (error "tls-listen key-password must be a string" key-password))
        (let ((tls (gauche-make-tls)))
          (gauche-tls-load-certificate tls cert-file)
          (gauche-tls-load-private-key tls key-file key-password)
          (gauche-tls-bind tls "0.0.0.0" port 'tcp)
          (make-tls-listener tls)))))
  (def 'tls-listener-p
    (lambda (obj)
      (tls-listener? obj)))
  (def 'tls-accept
    (lambda (listener)
      (unless (tls-listener? listener)
        (error "tls-accept needs tls listener object" listener))
      (let ((conn (gauche-tls-accept (tls-listener-raw listener))))
        (make-tls-connection conn
                             (gauche-connection-input-port conn)
                             (gauche-connection-output-port conn)))))
  (def 'tls-listener-close
    (lambda (listener)
      (unless (tls-listener? listener)
        (error "tls-listener-close needs tls listener object" listener))
      (guard (e (else #t))
        (gauche-tls-close (tls-listener-raw listener)))
      #t))
  (def 'tls-connection-p
    (lambda (obj)
      (tls-connection? obj)))
  (def 'tls-send
    (lambda (conn text)
      (unless (tls-connection? conn)
        (error "tls-send needs tls connection object" conn))
      (unless (string? text)
        (error "tls-send text must be a string" text))
      (display text (tls-connection-output conn))
      (flush (tls-connection-output conn))
      #t))
  (def 'tls-send-line
    (lambda (conn text)
      (unless (tls-connection? conn)
        (error "tls-send-line needs tls connection object" conn))
      (unless (string? text)
        (error "tls-send-line text must be a string" text))
      (display text (tls-connection-output conn))
      (newline (tls-connection-output conn))
      (flush (tls-connection-output conn))
      #t))
  (def 'tls-send-file
    (lambda (conn path)
      (unless (tls-connection? conn)
        (error "tls-send-file needs tls connection object" conn))
      (unless (string? path)
        (error "tls-send-file path must be a string" path))
      (call-with-input-file
       path
       (lambda (in)
         (let loop ((b (read-byte in)))
           (unless (eof-object? b)
             (write-byte b (tls-connection-output conn))
             (loop (read-byte in))))))
      (flush (tls-connection-output conn))
      #t))
  (def 'tls-receive-line
    (lambda args
      (unless (or (= (length args) 1) (= (length args) 2))
        (error "tls-receive-line takes connection and optional eof-error-p" args))
      (let ((conn (car args))
            (eof-error-p (if (= (length args) 2) (cadr args) #t)))
        (unless (tls-connection? conn)
          (error "tls-receive-line needs tls connection object" conn))
        (let ((line (read-line (tls-connection-input conn))))
          (if (eof-object? line)
              (if (truthy? eof-error-p)
                  (error "tls-receive-line reached EOF")
                  '())
              line)))))
  (def 'tls-receive-char
    (lambda args
      (unless (or (= (length args) 1) (= (length args) 2))
        (error "tls-receive-char takes connection and optional eof-error-p" args))
      (let ((conn (car args))
            (eof-error-p (if (= (length args) 2) (cadr args) #t)))
        (unless (tls-connection? conn)
          (error "tls-receive-char needs tls connection object" conn))
        (let ((ch (read-char (tls-connection-input conn))))
          (if (eof-object? ch)
              (if (truthy? eof-error-p)
                  (error "tls-receive-char reached EOF")
                  '())
              ch)))))
  (def 'tls-send-byte
    (lambda (conn byte)
      (unless (tls-connection? conn)
        (error "tls-send-byte needs tls connection object" conn))
      (unless (and (integer? byte) (>= byte 0) (<= byte 255))
        (error "tls-send-byte value must be integer in 0..255" byte))
      (write-byte byte (tls-connection-output conn))
      (flush (tls-connection-output conn))
      #t))
  (def 'tls-receive-byte
    (lambda args
      (unless (or (= (length args) 1) (= (length args) 2))
        (error "tls-receive-byte takes connection and optional eof-error-p" args))
      (let ((conn (car args))
            (eof-error-p (if (= (length args) 2) (cadr args) #t)))
        (unless (tls-connection? conn)
          (error "tls-receive-byte needs tls connection object" conn))
        (let ((b (read-byte (tls-connection-input conn))))
          (if (eof-object? b)
              (if (truthy? eof-error-p)
                  (error "tls-receive-byte reached EOF")
                  '())
              b)))))
  (def 'tls-flush
    (lambda (conn)
      (unless (tls-connection? conn)
        (error "tls-flush needs tls connection object" conn))
      (flush (tls-connection-output conn))
      #t))
  (def 'tls-close
    (lambda (conn)
      (unless (tls-connection? conn)
        (error "tls-close needs tls connection object" conn))
      (guard (e (else #t))
        (close-input-port (tls-connection-input conn)))
      (guard (e (else #t))
        (close-output-port (tls-connection-output conn)))
      (guard (e (else #t))
        (gauche-connection-close (tls-connection-raw conn)))
      #t))
  )

(define (install-concurrency-net-primitives! def env)
  (def 'thread-spawn
    (lambda (fn . args)
      (unless (or (primitive? fn) (closure? fn))
        (error "thread-spawn needs callable function as first argument" fn))
      (let ((th (gauche-make-thread
                 (lambda ()
                   (guard (e (else e))
                     (apply-islisp fn args))))))
        (gauche-thread-start! th)
        th)))
  (def 'thread-p
    (lambda (obj)
      (gauche-thread? obj)))
  (def 'thread-join
    (lambda (th)
      (unless (gauche-thread? th)
        (error "thread-join needs thread object" th))
      (gauche-thread-join! th)))
  (def 'thread-join-timeout
    (lambda (th timeout-sec)
      (unless (gauche-thread? th)
        (error "thread-join-timeout needs thread object" th))
      (unless (and (integer? timeout-sec) (>= timeout-sec 0))
        (error "thread-join-timeout timeout-sec must be non-negative integer" timeout-sec))
      (gauche-thread-join! th timeout-sec 'timeout)))
  (def 'mutex-open
    (lambda ()
      (gauche-make-mutex)))
  (def 'mutex-p
    (lambda (obj)
      (gauche-mutex? obj)))
  (def 'mutex-lock
    (lambda (m)
      (unless (gauche-mutex? m)
        (error "mutex-lock needs mutex object" m))
      (gauche-mutex-lock! m)
      #t))
  (def 'mutex-unlock
    (lambda (m)
      (unless (gauche-mutex? m)
        (error "mutex-unlock needs mutex object" m))
      (gauche-mutex-unlock! m)
      #t))
  (def 'udp-open
    (lambda ()
      (make-udp-socket (make-socket PF_INET SOCK_DGRAM 0))))
  (def 'udp-socket-p
    (lambda (obj)
      (udp-socket? obj)))
  (def 'udp-bind
    (lambda (sock host port)
      (unless (udp-socket? sock)
        (error "udp-bind needs udp socket object" sock))
      (unless (string? host)
        (error "udp-bind host must be a string" host))
      (unless (and (integer? port) (>= port 1) (<= port 65535))
        (error "udp-bind port must be integer in 1..65535" port))
      (socket-bind (udp-socket-raw sock)
                   (make <sockaddr-in> :host host :port port))
      #t))
  (def 'udp-connect
    (lambda (sock host port)
      (unless (udp-socket? sock)
        (error "udp-connect needs udp socket object" sock))
      (unless (string? host)
        (error "udp-connect host must be a string" host))
      (unless (and (integer? port) (>= port 1) (<= port 65535))
        (error "udp-connect port must be integer in 1..65535" port))
      (socket-connect (udp-socket-raw sock)
                      (make <sockaddr-in> :host host :port port))
      #t))
  (def 'udp-send
    (lambda (sock text)
      (unless (udp-socket? sock)
        (error "udp-send needs udp socket object" sock))
      (unless (string? text)
        (error "udp-send text must be a string" text))
      (socket-send (udp-socket-raw sock) text)))
  (def 'udp-receive
    (lambda (sock size)
      (unless (udp-socket? sock)
        (error "udp-receive needs udp socket object" sock))
      (unless (and (integer? size) (> size 0))
        (error "udp-receive size must be positive integer" size))
      (socket-recv (udp-socket-raw sock) size)))
  (def 'udp-sendto
    (lambda (sock host port text)
      (unless (udp-socket? sock)
        (error "udp-sendto needs udp socket object" sock))
      (unless (string? host)
        (error "udp-sendto host must be a string" host))
      (unless (and (integer? port) (>= port 1) (<= port 65535))
        (error "udp-sendto port must be integer in 1..65535" port))
      (unless (string? text)
        (error "udp-sendto text must be a string" text))
      (socket-sendto (udp-socket-raw sock)
                     text
                     (make <sockaddr-in> :host host :port port))))
  (def 'udp-receive-from
    (lambda (sock size)
      (unless (udp-socket? sock)
        (error "udp-receive-from needs udp socket object" sock))
      (unless (and (integer? size) (> size 0))
        (error "udp-receive-from size must be positive integer" size))
      (receive (payload addr)
          (socket-recvfrom (udp-socket-raw sock) size)
        (list payload
              (sockaddr-name addr)
              (sockaddr-family addr)))))
  (def 'udp-close
    (lambda (sock)
      (unless (udp-socket? sock)
        (error "udp-close needs udp socket object" sock))
      (guard (e (else #t))
        (socket-close (udp-socket-raw sock)))
      #t))
  (def 'http-get
    (lambda args
      (unless (>= (length args) 2)
        (error "http-get needs server and request-uri" args))
      (let ((server (car args))
            (request-uri (cadr args))
            (opts (cddr args)))
        (unless (string? server)
          (error "http-get server must be a string" server))
        (unless (string? request-uri)
          (error "http-get request-uri must be a string" request-uri))
        (unless (even? (length opts))
          (error "http-get keyword options must be key/value pairs" opts))
        (receive (status headers body)
            (apply gauche-http-get
                   server
                   request-uri
                   (append (list ':receiver gauche-http-string-receiver) opts))
          body))))
  (def 'http-head
    (lambda args
      (unless (>= (length args) 2)
        (error "http-head needs server and request-uri" args))
      (let ((server (car args))
            (request-uri (cadr args))
            (opts (cddr args)))
        (unless (string? server)
          (error "http-head server must be a string" server))
        (unless (string? request-uri)
          (error "http-head request-uri must be a string" request-uri))
        (unless (even? (length opts))
          (error "http-head keyword options must be key/value pairs" opts))
        (receive (status headers body)
            (apply gauche-http-head
                   server
                   request-uri
                   (append (list ':receiver gauche-http-string-receiver) opts))
          (list status headers body)))))
  (def 'http-post
    (lambda args
      (unless (>= (length args) 3)
        (error "http-post needs server, request-uri and body" args))
      (let ((server (car args))
            (request-uri (cadr args))
            (body (caddr args))
            (opts (cdddr args)))
        (unless (string? server)
          (error "http-post server must be a string" server))
        (unless (string? request-uri)
          (error "http-post request-uri must be a string" request-uri))
        (unless (string? body)
          (error "http-post body must be a string" body))
        (unless (even? (length opts))
          (error "http-post keyword options must be key/value pairs" opts))
        (receive (status headers resp-body)
            (apply gauche-http-post
                   server
                   request-uri
                   body
                   (append (list ':receiver gauche-http-string-receiver) opts))
          resp-body))))
  )

(define (install-ffi-time-load-primitives! def env)
  (def 'ffi-call
    (lambda (library symbol-name return-type arg-types arg-values)
      (unless (string? library)
        (error "ffi-call library must be a string" library))
      (unless (string? symbol-name)
        (error "ffi-call symbol name must be a string" symbol-name))
      (unless (list? arg-types)
        (error "ffi-call arg-types must be a list" arg-types))
      (unless (list? arg-values)
        (error "ffi-call arg-values must be a list" arg-values))
      (unless (= (length arg-types) (length arg-values))
        (error "ffi-call arg-types and arg-values length mismatch"
               arg-types arg-values))
      (let ((ret (normalize-ffi-type return-type "return"))
            (types (map (lambda (t) (normalize-ffi-type t "argument")) arg-types)))
        (ensure-ffi-bridge!)
        (let* ((bridge-ret (if (string=? ret "bool") "int" ret))
               (bridge-types (map (lambda (t) (if (string=? t "bool") "int" t)) types))
               (pairs
                (apply append
                       (map (lambda (t bt v) (list bt (ffi-arg->string t v)))
                            types bridge-types arg-values)))
               (argv (append (list *ffi-bridge-binary*
                                   library
                                   symbol-name
                                   bridge-ret
                                   (number->string (length bridge-types)))
                             pairs))
               (result (run-command/capture argv))
               (status (car result))
               (out (cadr result))
               (err (caddr result)))
          (unless (or (eq? status 0) (eq? status #t))
            (error "ffi-call failed"
                   (if (string=? err "") out err)))
          (let* ((parts (extract-ffi-result out))
                 (side-out (car parts))
                 (ret-out (cadr parts)))
            (unless (string=? side-out "")
              (display side-out))
            (parse-ffi-result ret ret-out))))))
  (def 'load-foreign-library
    (lambda (path)
      (unless (string? path)
        (error "load-foreign-library path must be a string" path))
      (unless (file-exists? path)
        (error "shared library file does not exist" path))
      (set! *ffi-current-library* path)
      path))
  (def 'current-foreign-library
    (lambda ()
      (if *ffi-current-library*
          *ffi-current-library*
          (error "No foreign library loaded. Call load-foreign-library first."))))
  (def 'get-universal-time
    (lambda ()
      ;; Convert Unix epoch seconds (1970-01-01) to CL/ISLISP universal-time
      ;; seconds since 1900-01-01 UTC.
      (+ (sys-time) 2208988800)))
  (def 'internal-time-units-per-second
    (lambda ()
      (internal-time-units)))
  (def 'get-internal-real-time
    (lambda ()
      (internal-real-time)))
  (def 'get-internal-run-time
    (lambda ()
      (internal-run-time)))
  (def 'load
    (lambda (filename)
      (eval-load-file filename env)))
  (def 'provide
    (lambda (feature)
      (register-feature! feature)))
  (def 'require
    (lambda args
      (unless (or (= (length args) 1) (= (length args) 2))
        (error "require takes feature-name and optional pathname" args))
      (let ((feature (car args)))
        (unless (feature-provided? feature)
          (if (= (length args) 2)
              (let ((path (cadr args)))
                (unless (string? path)
                  (error "require pathname must be a string" path))
                (eval-load-file path env)
                (register-feature! feature))
              (error "Feature is not provided" feature)))
        #t)))
  )

(define (install-core-clos-primitives! def env)
  (def 'format
    ;; ISLISP standard: (format output-stream format-string obj*)
    ;; output-stream: t → standard-output, nil → return string, output-port → write to port
    (lambda args
      (unless (>= (length args) 2)
        (error "format: needs at least destination and control-string"))
      (let ((dest    (car args))
            (fmt     (cadr args))
            (fmtargs (cddr args)))
        (unless (string? fmt)
          (error "format: second argument must be a string" fmt))
        (let ((rendered (render-format fmt fmtargs)))
          (cond
           ;; t → current standard output
           ((eq? dest #t)
            (display rendered)
            '())
           ;; nil (ISLISP) = '() or #f → return as string
           ((or (null? dest) (eq? dest #f))
            rendered)
           ;; output port / stream → write to it
           ((output-port? dest)
            (display rendered dest)
            '())
           (else
            (error "format: destination must be t, nil, or an output stream" dest)))))))
  (def 'not
    (lambda (x)
      (if (truthy? x) '() #t)))
  (def 'apply
    (lambda (f . rest)
      (when (null? rest)
        (error "apply: needs at least a function and a list" f))
      ;; ISLISP: (apply f arg* list) — last arg must be a list, preceding are prepended
      (let ((last-arg (car (reverse rest)))
            (front-args (reverse (cdr (reverse rest)))))
        (unless (list? last-arg)
          (error "apply: last argument must be a list" last-arg))
        (apply-islisp f (append front-args last-arg)))))
  (def 'funcall
    (lambda (f . xs)
      (apply-islisp f xs)))
  (def 'make-instance
    (lambda args
      (unless (>= (length args) 1)
        (error "make-instance needs at least one class designator" args))
      (let* ((class-obj (resolve-class-designator (car args) env))
             (raw-initargs (cdr args)))
        (unless (even? (length raw-initargs))
          (error "make-instance initargs must be key/value pairs" raw-initargs))
        (let* ((slot-specs (class-effective-slots class-obj))
               (slots
                (map (lambda (s)
                       (cons (slot-spec-name s)
                             (eval-islisp* (slot-spec-initform s) env #f)))
                     slot-specs))
               (obj (make-instance-object class-obj slots)))
          (let apply-initargs ((rest raw-initargs))
            (unless (null? rest)
              (let* ((k (car rest))
                     (v (cadr rest)))
                (unless (symbol? k)
                  (error "make-instance initarg key must be symbol" k))
                (let ((spec (find-slot-spec-entry slot-specs k)))
                  (unless spec
                    (error "Unknown initarg for make-instance" k))
                  (instance-slot-set! obj (slot-spec-name spec) v))
                (apply-initargs (cddr rest)))))
          ;; Phase 7-E: call initialize-object hook
          (let ((init-sym (resolve-binding-symbol 'initialize-object))
                (global-frame (global-frame env)))
            (when (frame-bound? global-frame init-sym)
              (apply-islisp (frame-ref global-frame init-sym)
                            (list obj raw-initargs))))
          obj))))
  ;; ISLISP §21: create — the standard instance-creation generic function.
  ;; (create class . initargs); shares make-instance's semantics.
  (def 'create
    (lambda args
      (apply-islisp (frame-ref env (resolve-binding-symbol 'make-instance)) args)))
  (def 'class-of
    (lambda (obj)
      (cond
       ((instance? obj) (instance-class obj))
       ((class? obj) obj)
       (else
        (let ((cls (native-value->class-obj obj)))
          (or cls (error "class-of: cannot determine class" obj)))))))
  ;; ISLISP §6.3.1: class-name — returns the name (symbol) of a class
  (def 'class-name
    (lambda (cls)
      (cond
       ((class? cls)
        ;; Strip package prefix: ISLISP::<integer> → <integer>
        (string->symbol (symbol-base-name (class-name cls))))
       (else (error "class-name: argument must be a class" cls)))))
  (def 'instancep
    (lambda args
      (cond
       ((= (length args) 1)
        ;; isl 拡張: 1引数は instance? 相当
        (instance? (car args)))
       ((= (length args) 2)
        ;; ISLISP 標準: (instancep obj class)
        (let* ((obj       (car args))
               (class-arg (cadr args))
               (cname     (cond
                           ((class? class-arg) (class-name class-arg))
                           ((symbol? class-arg) class-arg)
                           (else (error "instancep: second arg must be class or class-name" class-arg)))))
          (if (isl-instance-of? obj cname) #t '())))
       (else
        (error "instancep: expects 1 or 2 arguments" args)))))

  ;; ISLISP §13: convert — type conversion between classes
  (def 'convert
    (lambda (obj class-arg)
      (let* ((raw (cond
                    ((class? class-arg) (class-name class-arg))
                    ((symbol? class-arg) class-arg)
                    (else (error "convert: second arg must be class" class-arg))))
             ;; Strip package prefix so ISLISP::<integer> == <integer>
             (cname (string->symbol (symbol-base-name raw))))
        (cond
         ((eq? cname '<integer>)
          (cond ((and (integer? obj) (exact? obj)) obj)
                ((number? obj) (inexact->exact (truncate obj)))
                ((char? obj) (char->integer obj))
                (else (error "convert: cannot convert to <integer>" obj))))
         ((eq? cname '<float>)
          (cond ((number? obj) (exact->inexact obj))
                (else (error "convert: cannot convert to <float>" obj))))
         ((eq? cname '<string>)
          (cond ((string? obj)  obj)
                ((symbol? obj)  (symbol->string obj))
                ((char? obj)    (string obj))
                ((number? obj)  (number->string obj))
                ((list? obj)    (list->string obj))
                (else (error "convert: cannot convert to <string>" obj))))
         ((eq? cname '<symbol>)
          (cond ((symbol? obj) obj)
                ((string? obj) (string->symbol obj))
                (else (error "convert: cannot convert to <symbol>" obj))))
         ((eq? cname '<list>)
          (cond ((list? obj)   obj)
                ((string? obj) (string->list obj))
                ((vector? obj) (vector->list obj))
                (else (error "convert: cannot convert to <list>" obj))))
         ((eq? cname '<general-vector>)
          (cond ((vector? obj) obj)
                ((list? obj)   (list->vector obj))
                ((string? obj) (list->vector (string->list obj)))
                (else (error "convert: cannot convert to <general-vector>" obj))))
         ((eq? cname '<character>)
          (cond ((char? obj) obj)
                ((and (integer? obj) (exact? obj)) (integer->char obj))
                (else (error "convert: cannot convert to <character>" obj))))
         (else
          (error "convert: unsupported target class" cname))))))

  ;; identity
  (def 'identity (lambda (x) x))

  ;; basic-array-p: #t for any array — rank-1 vectors/strings and N-D arrays.
  (def 'basic-array-p
    (lambda (x) (if (or (isl-plain-vector? x) (string? x) (isl-array? x)) #t '())))
  ;; basic-array*-p: #t for basic arrays of rank /= 1 (our N-D general arrays).
  (def 'basic-array*-p (lambda (x) (if (isl-array? x) #t '())))

  ;; map-into: destructively fill vector from applying fn to lists
  (def 'map-into
    (lambda (target fn . lists)
      (when (null? lists) (error "map-into: needs at least one source list"))
      (unless (vector? target) (error "map-into: target must be a vector" target))
      (let loop ((i 0) (tails lists))
        (if (or (any null? tails) (>= i (vector-length target)))
            target
            (begin
              (vector-set! target i (apply-islisp fn (map car tails)))
              (loop (+ i 1) (map cdr tails)))))))

  ;; eval: evaluate an ISLISP form in the current global environment
  (def 'eval
    (lambda (form)
      (eval-islisp form env)))
  (def 'slot-value
    (lambda (obj slot)
      (instance-slot-ref obj slot)))
  )

(define (install-method-package-symbol-primitives! def env)
  ;; ---- Phase 7-A ----
  (def 'call-next-method
    (lambda args
      (let ((stack (*next-methods*)))
        (when (null? stack)
          (isl-signal-condition
           (make-isl-condition '<control-error> "call-next-method: no next method available" #f '())))
        (let* ((next-fn (car stack))
               (effective-args (if (null? args) (*current-method-args*) args)))
          (parameterize ((*next-methods* (cdr stack))
                         (*current-method-args* effective-args))
            (next-fn effective-args))))))
  (def 'next-method-p
    (lambda ()
      (if (null? (*next-methods*)) '() #t)))
  ;; ---- Phase 7-C ----
  (def 'subclassp
    (lambda (sub super)
      (let ((sub-sym (cond ((class? sub) (class-name sub))
                           ((symbol? sub) (resolve-binding-symbol sub))
                           (else (error "subclassp: first arg must be symbol or class" sub))))
            (sup-sym (cond ((class? super) (class-name super))
                           ((symbol? super) (resolve-binding-symbol super))
                           (else (error "subclassp: second arg must be symbol or class" super)))))
        (if (isl-subclassp? sub-sym sup-sym) #t '()))))
  ;; Phase 7-E: initialize-object is installed as a generic in make-initial-env
  ;; ---- Phase 7-F ----
  (def 'slot-boundp
    (lambda (obj slot-name)
      (unless (instance? obj) (error "slot-boundp needs an instance" obj))
      (unless (symbol? slot-name) (error "slot-boundp slot-name must be symbol" slot-name))
      (if (find-instance-slot-entry (instance-slots obj) slot-name) #t '())))
  (def 'slot-makunbound
    (lambda (obj slot-name)
      (unless (instance? obj) (error "slot-makunbound needs an instance" obj))
      (unless (symbol? slot-name) (error "slot-makunbound slot-name must be symbol" slot-name))
      (let ((entry (find-instance-slot-entry (instance-slots obj) slot-name)))
        (unless entry (error "slot-makunbound: no such slot" slot-name obj))
        (set-instance-slots! obj
          (filter (lambda (p) (not (eq? (car p) (car entry))))
                  (instance-slots obj)))
        obj)))
  (def 'current-package
    (lambda ()
      (string->symbol (package-name *current-package*))))
  (def 'find-package
    (lambda (name)
      (let ((p (find-package name)))
        (if p
            (string->symbol (package-name p))
            '()))))
  (def 'use-package
    (lambda args
      (unless (or (= (length args) 1) (= (length args) 2))
        (error "use-package takes package-name and optional target-package" args))
      (let* ((used (ensure-package! (car args)))
             (target (if (= (length args) 2)
                         (ensure-package! (cadr args))
                         *current-package*)))
        (package-use! target used)
        #t)))
  (def 'export
    (lambda args
      (unless (or (= (length args) 1) (= (length args) 2))
        (error "export takes symbol(s) and optional package-name" args))
      (let ((syms (car args))
            (pkg (if (= (length args) 2)
                     (ensure-package! (cadr args))
                     *current-package*)))
        (cond
         ((symbol? syms)
          (package-export! pkg syms))
         ((list? syms)
          (for-each
           (lambda (s)
             (unless (symbol? s)
               (error "export list must contain symbols" s))
             (package-export! pkg s))
           syms))
         (else
          (error "export expects symbol or list of symbols" syms)))
        #t)))
  (def 'intern
    (lambda args
      (unless (or (= (length args) 1) (= (length args) 2))
        (error "intern takes name and optional package-name" args))
      (let* ((name-arg (car args))
             (name (cond
                    ((symbol? name-arg) (symbol->string name-arg))
                    ((string? name-arg) name-arg)
                    (else (error "intern name must be symbol or string" name-arg))))
             (pkg (if (= (length args) 2)
                      (ensure-package! (cadr args))
                      *current-package*)))
        (package-intern! pkg name))))
  (def 'import
    (lambda args
      (unless (or (= (length args) 1) (= (length args) 2))
        (error "import takes symbol(s) and optional package-name" args))
      (let ((vals (car args))
            (pkg (if (= (length args) 2)
                     (ensure-package! (cadr args))
                     *current-package*)))
        (cond
         ((symbol? vals)
          (package-import-symbol! pkg vals))
         ((list? vals)
          (for-each
           (lambda (s)
             (unless (symbol? s)
               (error "import list must contain symbols" s))
             (package-import-symbol! pkg s))
           vals))
         (else
          (error "import expects symbol or list of symbols" vals)))
        #t)))
  ;; ISLISP §10: symbol property lists.
  ;;   (property symbol property-name [default]) -> value or default (nil)
  ;;   (set-property obj symbol property-name)   -> obj
  ;;   (remove-property symbol property-name)     -> removed value or nil
  (def 'property
    (lambda (sym key . maybe-default)
      (unless (symbol? sym) (error "property: first argument must be a symbol" sym))
      (unless (symbol? key) (error "property: property-name must be a symbol" key))
      (isl-property-get sym key (if (pair? maybe-default) (car maybe-default) '()))))
  (def 'set-property
    (lambda (val sym key)
      (unless (symbol? sym) (error "set-property: symbol argument must be a symbol" sym))
      (unless (symbol? key) (error "set-property: property-name must be a symbol" key))
      (isl-property-set! sym key val)))
  (def 'remove-property
    (lambda (sym key)
      (unless (symbol? sym) (error "remove-property: first argument must be a symbol" sym))
      (unless (symbol? key) (error "remove-property: property-name must be a symbol" key))
      (isl-property-remove! sym key)))
  (def 'gensym
    (lambda args
      (unless (or (null? args) (= (length args) 1))
        (error "gensym takes optional prefix argument" args))
      (let ((prefix
             (if (null? args)
                 "G"
                 (let ((x (car args)))
                   (cond
                    ((string? x) x)
                    ((symbol? x) (symbol->string x))
                    (else
                     (error "gensym prefix must be string or symbol" x)))))))
        (make-gensym prefix))))
  (def 'macroexpand-1
    (lambda (form)
      (macroexpand-1* form env)))
  (def 'macroexpand
    (lambda (form)
      (macroexpand* form env)))
  (def 'exit
    (lambda args
      (apply exit args)))
  ;; ---- Modification E: standard naming ----
  ;; E-1. char-code / code-char (ISLISP standard names for char<->integer)
  (def 'char-code
    (lambda (c)
      (unless (char? c) (error "char-code needs char" c))
      (char->integer c)))
  (def 'code-char
    (lambda (n)
      (unless (integer? n) (error "code-char needs integer" n))
      (integer->char n)))
  ;; E-2. list-length (ISLISP standard name for length on lists)
  (def 'list-length
    (lambda (lst)
      (unless (list? lst) (error "list-length needs a list" lst))
      (length lst)))
  ;; E-3. basic-vector-p (ISLISP: true for rank-1 vectors and strings)
  (def 'basic-vector-p
    (lambda (x) (if (or (isl-plain-vector? x) (string? x)) #t '())))
  ;; E-4. vector-length (ISLISP standard name) - using a local binding to avoid conflict
  (def 'vector-length
    (lambda (v)
      (unless (vector? v) (error "vector-length needs a vector" v))
      (vector-length v)))
  ;; E-5. symbol-name (ISLISP standard name)
  (def 'symbol-name
    (lambda (s)
      ;; ISLISP §14.2: nil and t are symbols with names "nil" and "t"
      (cond ((null? s) "nil")
            ((eq? s #t) "t")
            ((symbol? s) (symbol->string s))
            (else (error "symbol-name needs a symbol" s)))))
  ;; ISLISP §14.1.2: symbol-package — returns the home package name or nil
  (def 'symbol-package
    (lambda (s)
      (cond ((null? s)   "isl")   ; nil is in the isl package
            ((eq? s #t)  "isl")   ; t is in the isl package
            ((symbol? s) "isl")   ; all symbols in this impl are in "isl"
            (else (error "symbol-package needs a symbol" s)))))
  ;; ---- Modification F: character predicates ----
  (def 'char-alphabetic-p
    (lambda (c)
      (unless (char? c) (error "char-alphabetic-p: char required" c))
      (if (char-alphabetic? c) #t '())))
  (def 'char-numeric-p
    (lambda (c)
      (unless (char? c) (error "char-numeric-p: char required" c))
      (if (char-numeric? c) #t '())))
  (def 'char-whitespace-p
    (lambda (c)
      (unless (char? c) (error "char-whitespace-p: char required" c))
      (if (char-whitespace? c) #t '())))
  (def 'char-upper-case-p
    (lambda (c)
      (unless (char? c) (error "char-upper-case-p: char required" c))
      (if (char-upper-case? c) #t '())))
  (def 'char-lower-case-p
    (lambda (c)
      (unless (char? c) (error "char-lower-case-p: char required" c))
      (if (char-lower-case? c) #t '())))
  ;; ---- Modification G: some/every/notany/notevery ----
  (def 'some
    (lambda (pred . lists)
      (when (null? lists) (error "some: needs at least one list"))
      (let loop ((tails lists))
        (if (any null? tails)
            '()   ; nil = false
            (let ((result (apply-islisp pred (map car tails))))
              (if (truthy? result)
                  result
                  (loop (map cdr tails))))))))
  (def 'every
    (lambda (pred . lists)
      (when (null? lists) (error "every: needs at least one list"))
      (let loop ((tails lists) (last #t))
        (if (any null? tails)
            last
            (let ((result (apply-islisp pred (map car tails))))
              (if (truthy? result)
                  (loop (map cdr tails) result)
                  '()))))))
  (def 'notany
    (lambda (pred . lists)
      (when (null? lists) (error "notany: needs at least one list"))
      (let loop ((tails lists))
        (if (any null? tails)
            #t
            (let ((result (apply-islisp pred (map car tails))))
              (if (truthy? result)
                  '()
                  (loop (map cdr tails))))))))
  (def 'notevery
    (lambda (pred . lists)
      (when (null? lists) (error "notevery: needs at least one list"))
      (let loop ((tails lists))
        (if (any null? tails)
            '()
            (let ((result (apply-islisp pred (map car tails))))
              (if (truthy? result)
                  (loop (map cdr tails))
                  #t))))))
  ;; ---- ISLISP §18.2: with-standard-input/output/error-output helpers ----
  (def '%with-si-helper
    (lambda (port thunk)
      (parameterize ((current-input-port port))
        (apply-islisp thunk '()))))
  (def '%with-so-helper
    (lambda (port thunk)
      (parameterize ((current-output-port port))
        (apply-islisp thunk '()))))
  (def '%with-eo-helper
    (lambda (port thunk)
      (parameterize ((current-error-port port))
        (apply-islisp thunk '()))))
  )


