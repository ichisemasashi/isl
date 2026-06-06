(define-module isl.compiler.runtime
  (use isl.core)
  (use isl.compiler.frontend)
  (use gauche.net)
  (use gauche.threads)
  (use gauche.fcntl)
  (use rfc.tls)
  (use rfc.base64)
  (use rfc.uuid)
  (use file.util)
  (use srfi.19)
  (use srfi-1)
  (use srfi-13)
  (export make-runtime-state
          runtime-load-units!
          runtime-eval-top
          runtime-eval-expr
          runtime-value->host
          host->runtime-value
          runtime-error?
          runtime-error-code
          runtime-error-message
          runtime-error-details))

(select-module isl.compiler.runtime)

;; gauche-* ラッパ・make-go-signal/go-signal?/go-signal-tag・score<?/score=? は
;; isl.core に集約し (use isl.core) 経由で取り込む（R7: core/runtime 重複排除）。

;; Runtime error object shared by frontend/backend paths.
(define (make-runtime-error code message details)
  (vector 'runtime-error code message details))

(define (runtime-error? obj)
  (and (vector? obj)
       (= (vector-length obj) 4)
       (eq? (vector-ref obj 0) 'runtime-error)))

(define (runtime-error-code err) (vector-ref err 1))
(define (runtime-error-message err) (vector-ref err 2))
(define (runtime-error-details err) (vector-ref err 3))

(define (runtime-raise code message . details)
  (raise (make-runtime-error code message details)))

(define (make-nonlocal-return name value)
  (vector 'nonlocal-return name value))

(define (nonlocal-return? obj)
  (and (vector? obj)
       (= (vector-length obj) 3)
       (eq? (vector-ref obj 0) 'nonlocal-return)))

(define (nonlocal-return-name s) (vector-ref s 1))
(define (nonlocal-return-value s) (vector-ref s 2))

(define (make-throw-signal tag value)
  (vector 'throw-signal tag value))

(define (throw-signal? obj)
  (and (vector? obj)
       (= (vector-length obj) 3)
       (eq? (vector-ref obj 0) 'throw-signal)))

(define (throw-signal-tag s) (vector-ref s 1))
(define (throw-signal-value s) (vector-ref s 2))

;; make-go-signal / go-signal? / go-signal-tag は isl.core から取り込む（R7）。

;; Dynamic nonlocal target stacks.
(define *runtime-catch-stack* '())

(define (runtime-catch-active? tag)
  (let loop ((xs *runtime-catch-stack*))
    (and (pair? xs)
         (or (eqv? (car xs) tag)
             (loop (cdr xs))))))

;; Tagged value representation used by compiler runtime.
(define (make-rt-value tag payload)
  (vector 'rt-value tag payload))

(define (rt-value? v)
  (and (vector? v)
       (= (vector-length v) 3)
       (eq? (vector-ref v 0) 'rt-value)))

(define (rt-tag v) (vector-ref v 1))
(define (rt-payload v) (vector-ref v 2))

(define (classify-host-value v)
  (cond
   ((number? v) 'number)
   ((string? v) 'string)
   ((char? v) 'char)
   ((boolean? v) 'boolean)
   ((null? v) 'null)
   ((symbol? v) 'symbol)
   ((safe-pair? v) 'pair)
   ((vector? v) 'vector)
   (else 'object)))

(define (host->runtime-value v)
  (if (rt-value? v)
      v
      (make-rt-value (classify-host-value v) v)))

(define (runtime-value->host rv)
  (if (rt-value? rv)
      (rt-payload rv)
      rv))

(define (runtime-truthy? rv)
  (let ((v (runtime-value->host rv)))
    (and (not (eq? v #f))
         (not (safe-null? v)))))

(define (safe-null? x)
  (guard (e (else #f))
    (null? x)))

(define (safe-pair? x)
  (guard (e (else #f))
    (pair? x)))

(define (proper-list-host? v)
  (let loop ((x v))
    (if (safe-null? x)
        #t
        (if (safe-pair? x)
            (loop (cdr x))
            #f))))

(define (runtime-ir-node-like? x)
  (if (safe-pair? x)
      (symbol? (car x))
      #f))

(define (runtime-eval-maybe-ir x env state)
  (if (runtime-ir-node-like? x)
      (runtime-eval-expr x env state)
      (host->runtime-value x)))

(define (make-let-binding name init)
  (vector 'let-binding name init))

(define (let-binding? x)
  (and (vector? x)
       (= (vector-length x) 3)
       (eq? (vector-ref x 0) 'let-binding)))

(define (let-binding-name x)
  (vector-ref x 1))

(define (let-binding-init x)
  (vector-ref x 2))

(define (runtime-parse-let-binding b)
  (cond
   ((and (symbol? b)
         (not (with-module isl.core (keyword-symbol? b))))
    (make-let-binding b '(const ())))
   ((proper-list-host? b)
    (if (= (length b) 2)
        (if (and (symbol? (car b))
                 (not (with-module isl.core (keyword-symbol? (car b)))))
            (make-let-binding (car b) (cadr b))
            #f)
        #f))
   ((safe-pair? b)
    (if (and (symbol? (car b))
             (not (with-module isl.core (keyword-symbol? (car b)))))
        ;; dotted pair form: (name . init)
        (make-let-binding (car b) (cdr b))
        #f))
   (else #f)))

;; Lexical environment.
(define (make-env parent)
  (vector 'env parent '()))

(define (env-parent env) (vector-ref env 1))
(define (env-bindings env) (vector-ref env 2))
(define (set-env-bindings! env bindings) (vector-set! env 2 bindings))

(define (runtime-resolve-symbol sym)
  (cond
   ((not (symbol? sym)) sym)
   ;; Compiler-internal operators (lowering artifacts such as %handler-case,
   ;; %catch, %dynamic-get) use a reserved `%` prefix and bypass package
   ;; resolution, so they bind/look-up consistently in any current package.
   ((let ((s (symbol->string sym)))
      (and (> (string-length s) 0) (char=? (string-ref s 0) #\%)))
    sym)
   (else (with-module isl.core (resolve-symbol-in-package sym)))))

(define (env-define! env sym val)
  (set! sym (runtime-resolve-symbol sym))
  (let ((bindings (env-bindings env)))
    (let loop ((xs bindings) (acc '()))
      (cond
       ((null? xs)
        (set-env-bindings! env (append (reverse acc) (list (cons sym val)))))
       ((eq? (caar xs) sym)
        (set-env-bindings! env (append (reverse acc) (cons (cons sym val) (cdr xs)))))
       (else
        (loop (cdr xs) (cons (car xs) acc)))))))

(define (env-find-pair env sym)
  (set! sym (runtime-resolve-symbol sym))
  (let loop ((e env))
    (if (not e)
        #f
        (let ((p (assoc sym (env-bindings e))))
          (if p p (loop (env-parent e)))))))

(define (env-ref env sym)
  (set! sym (runtime-resolve-symbol sym))
  (let ((p (env-find-pair env sym)))
    (if p
        (cdr p)
        (runtime-raise 'unbound-variable "Unbound variable" sym))))

(define (env-set! env sym val)
  (set! sym (runtime-resolve-symbol sym))
  (let loop ((e env))
    (if (not e)
        (env-define! env sym val)
        (let ((p (assoc sym (env-bindings e))))
          (if p
              (set-cdr! p val)
              (loop (env-parent e)))))))

;; Object system (M7 minimal runtime representation)
(define (make-rt-slot-spec name initform initarg readers writers)
  (vector 'rt-slot-spec name initform initarg readers writers))

(define (rt-slot-spec? x)
  (and (vector? x) (= (vector-length x) 6) (eq? (vector-ref x 0) 'rt-slot-spec)))

(define (rt-slot-name s) (vector-ref s 1))
(define (rt-slot-initform s) (vector-ref s 2))
(define (rt-slot-initarg s) (vector-ref s 3))
(define (rt-slot-readers s) (vector-ref s 4))
(define (rt-slot-writers s) (vector-ref s 5))

(define (make-rt-class name supers own-slots)
  (vector 'rt-class name supers own-slots))

(define (rt-class? x)
  (and (vector? x) (= (vector-length x) 4) (eq? (vector-ref x 0) 'rt-class)))

(define (rt-class-name c) (vector-ref c 1))
(define (rt-class-supers c) (vector-ref c 2))
(define (rt-class-own-slots c) (vector-ref c 3))

(define (make-rt-instance class slots)
  (vector 'rt-instance class slots))

(define (rt-instance? x)
  (and (vector? x) (= (vector-length x) 3) (eq? (vector-ref x 0) 'rt-instance)))

(define (rt-instance-class x) (vector-ref x 1))
(define (rt-instance-slots x) (vector-ref x 2))
(define (rt-instance-set-slots! x slots) (vector-set! x 2 slots))

(define (make-rt-method qualifier specializers params fn-val)
  (vector 'rt-method qualifier specializers params fn-val))

(define (rt-method? x)
  (and (vector? x) (= (vector-length x) 5) (eq? (vector-ref x 0) 'rt-method)))

(define (rt-method-qualifier m) (vector-ref m 1))
(define (rt-method-specializers m) (vector-ref m 2))
(define (rt-method-params m) (vector-ref m 3))
(define (rt-method-fn m) (vector-ref m 4))

(define (make-rt-generic name params methods)
  (vector 'rt-generic name params methods))

(define (rt-generic? x)
  (and (vector? x) (= (vector-length x) 4) (eq? (vector-ref x 0) 'rt-generic)))

(define (rt-generic-name g) (vector-ref g 1))
(define (rt-generic-params g) (vector-ref g 2))
(define (rt-generic-methods g) (vector-ref g 3))
(define (set-rt-generic-methods! g methods) (vector-set! g 3 methods))

;; Function values.
(define (make-closure name params body env)
  (vector 'closure name params body env 'ir))

(define (make-closure/cfg name params cfg env)
  (vector 'closure name params cfg env 'cfg))

(define (closure? x)
  (and (vector? x) (= (vector-length x) 6) (eq? (vector-ref x 0) 'closure)))

(define (closure-name c) (vector-ref c 1))
(define (closure-params c) (vector-ref c 2))
(define (closure-body c) (vector-ref c 3))
(define (closure-env c) (vector-ref c 4))
(define (closure-kind c) (vector-ref c 5))

(define (make-primitive name proc)
  (vector 'primitive name proc))

(define (primitive? x)
  (and (vector? x) (= (vector-length x) 3) (eq? (vector-ref x 0) 'primitive)))

(define (primitive-name p) (vector-ref p 1))
(define (primitive-proc p) (vector-ref p 2))

(define (make-fn-value f)
  (make-rt-value 'function f))

(define (fn-from-value rv)
  (if (and (rt-value? rv) (eq? (rt-tag rv) 'function))
      (rt-payload rv)
      (runtime-raise 'type-error "Attempt to call non-function" rv)))

(define (parse-params params)
  (cond
   ((symbol? params)
    ;; Variadic shorthand: (defun f args ...)
    (list '() params))
   ((list? params)
    (let loop ((xs params) (required '()))
      (cond
       ((null? xs) (list (reverse required) #f))
       ((eq? (car xs) '&rest)
        (unless (and (pair? (cdr xs)) (symbol? (cadr xs)) (null? (cddr xs)))
          (runtime-raise 'invalid-params "invalid &rest parameter list" params))
        (list (reverse required) (cadr xs)))
       ((symbol? (car xs))
        (loop (cdr xs) (cons (car xs) required)))
       (else
        (runtime-raise 'invalid-params "parameter must be symbol" (car xs))))))
   ((pair? params)
    ;; Dotted variadic form: (a b . rest)
    (let dotted-loop ((xs params) (required '()))
      (cond
       ((pair? xs)
        (let ((x (car xs)))
          (unless (symbol? x)
            (runtime-raise 'invalid-params "parameter must be symbol" x))
          (when (or (eq? x '&optional) (eq? x '&rest) (eq? x '&body))
            (runtime-raise 'invalid-params "lambda-list keywords are not allowed in dotted parameter prefix" params))
          (dotted-loop (cdr xs) (cons x required))))
       ((symbol? xs)
        (list (reverse required) xs))
       (else
        (runtime-raise 'invalid-params "invalid dotted parameter list tail" xs)))))
   (else
    (runtime-raise 'invalid-params "invalid parameter list" params))))

(define (validate-params params)
  (parse-params params)
  #t)

(define (bind-params! call-env params args)
  (let* ((spec (parse-params params))
         (required (car spec))
         (rest-sym (cadr spec)))
    (when (< (length args) (length required))
      (runtime-raise 'arity "too few arguments" params args))
    (when (and (not rest-sym) (> (length args) (length required)))
      (runtime-raise 'arity "too many arguments" params args))
    (let bind-required ((ps required) (as args))
      (unless (null? ps)
        (env-define! call-env (car ps) (car as))
        (bind-required (cdr ps) (cdr as))))
    (when rest-sym
      (env-define! call-env
                   rest-sym
                   (host->runtime-value (map runtime-value->host (list-tail args (length required))))))))

(define (runtime-number rv who)
  (let ((v (runtime-value->host rv)))
    (if (number? v)
        v
        (runtime-raise 'type-error (string-append who " expects number") rv))))

(define (runtime-integer rv who)
  (let ((v (runtime-number rv who)))
    (if (integer? v)
        v
        (runtime-raise 'type-error (string-append who " expects integer") rv))))

(define (runtime-string rv who)
  (let ((v (runtime-value->host rv)))
    (if (string? v)
        v
        (runtime-raise 'type-error (string-append who " expects string") rv))))

(define (runtime-port-number rv who)
  (let ((v (runtime-integer rv who)))
    (if (and (>= v 1) (<= v 65535))
        v
        (runtime-raise 'type-error (string-append who " expects integer in 1..65535") rv))))

(define (runtime-byte rv who)
  (let ((v (runtime-integer rv who)))
    (if (and (>= v 0) (<= v 255))
        v
        (runtime-raise 'type-error (string-append who " expects integer in 0..255") rv))))

(define (runtime-vector rv who)
  (let ((v (runtime-value->host rv)))
    (if (vector? v)
        v
        (runtime-raise 'type-error (string-append who " expects vector") rv))))

(define (runtime-list rv who)
  (let ((v (runtime-value->host rv)))
    (if (proper-list-host? v)
        v
        (runtime-raise 'type-error (string-append who " expects list") rv))))

(define (runtime-package-designator x who)
  (if (or (symbol? x) (string? x))
      x
      (runtime-raise 'type-error (string-append who " expects package designator") x)))

(define (runtime-canonical-option-key k)
  (let ((s (cond
            ((keyword? k) (keyword->string k))
            ((symbol? k) (symbol->string k))
            (else ""))))
    (if (and (> (string-length s) 0)
             (char=? (string-ref s 0) #\:))
        (substring s 1 (string-length s))
        s)))

(define (runtime-option-ref options key)
  (let loop ((xs options))
    (if (null? xs)
        #f
        (let ((entry (car xs)))
          (if (and (pair? entry)
                   (= (length entry) 2)
                   (string=? (runtime-canonical-option-key (car entry)) key))
              (cadr entry)
              (loop (cdr xs)))))))

(define (runtime-object rv who pred)
  (let ((v (runtime-value->host rv)))
    (if (pred v)
        v
        (runtime-raise 'type-error (string-append who " received invalid object") rv))))

(define (make-runtime-tcp-connection socket in out)
  (vector 'tcp-connection socket in out))

(define (runtime-tcp-connection? obj)
  (and (vector? obj)
       (= (vector-length obj) 4)
       (eq? (vector-ref obj 0) 'tcp-connection)))

(define (runtime-tcp-connection-socket conn)
  (vector-ref conn 1))

(define (runtime-tcp-connection-input conn)
  (vector-ref conn 2))

(define (runtime-tcp-connection-output conn)
  (vector-ref conn 3))

(define (make-runtime-tcp-listener socket)
  (vector 'tcp-listener socket))

(define (runtime-tcp-listener? obj)
  (and (vector? obj)
       (= (vector-length obj) 2)
       (eq? (vector-ref obj 0) 'tcp-listener)))

(define (runtime-tcp-listener-socket listener)
  (vector-ref listener 1))

(define (make-runtime-tls-connection tls in out)
  (vector 'tls-connection tls in out))

(define (runtime-tls-connection? obj)
  (and (vector? obj)
       (= (vector-length obj) 4)
       (eq? (vector-ref obj 0) 'tls-connection)))

(define (runtime-tls-connection-raw conn)
  (vector-ref conn 1))

(define (runtime-tls-connection-input conn)
  (vector-ref conn 2))

(define (runtime-tls-connection-output conn)
  (vector-ref conn 3))

(define (make-runtime-tls-listener tls)
  (vector 'tls-listener tls))

(define (runtime-tls-listener? obj)
  (and (vector? obj)
       (= (vector-length obj) 2)
       (eq? (vector-ref obj 0) 'tls-listener)))

(define (runtime-tls-listener-raw listener)
  (vector-ref listener 1))

(define (resolve-class-designator obj env)
  (cond
   ((rt-class? obj) obj)
   ((symbol? obj)
    (let ((v (runtime-value->host (env-ref env obj))))
      (if (rt-class? v)
          v
          (runtime-raise 'type-error "class designator does not resolve to class" obj))))
   (else
    (runtime-raise 'type-error "invalid class designator" obj))))

(define (slot-spec-list-merge specs)
  (let loop ((rest specs) (acc '()))
    (if (null? rest)
        (reverse acc)
        (let* ((s (car rest))
               (name (rt-slot-name s))
               (acc2 (filter (lambda (e) (not (eq? (rt-slot-name e) name))) acc)))
          (loop (cdr rest) (cons s acc2))))))

(define (rt-class-effective-slots class-obj)
  (let collect ((c class-obj) (seen '()))
    (if (memq c seen)
        (runtime-raise 'invalid-ir "Cyclic class precedence list" (rt-class-name c))
        (let ((seen2 (cons c seen)))
          (slot-spec-list-merge
           (append
            (apply append
                   (map (lambda (s) (collect s seen2))
                        (rt-class-supers c)))
            (rt-class-own-slots c)))))))

(define (find-slot-spec-entry specs slot)
  (or (find (lambda (s) (eq? (rt-slot-initarg s) slot)) specs)
      (find (lambda (s) (eq? (rt-slot-name s) slot)) specs)))

(define (find-instance-slot-entry slots slot)
  (assoc slot slots))

(define (rt-instance-slot-ref obj slot)
  (unless (rt-instance? obj)
    (runtime-raise 'type-error "slot-value target must be instance" obj))
  (unless (symbol? slot)
    (runtime-raise 'type-error "slot-value slot must be symbol" slot))
  (let ((entry (find-instance-slot-entry (rt-instance-slots obj) slot)))
    (if entry
        (cdr entry)
        (runtime-raise 'invalid-ir "No such slot in instance" slot))))

(define (rt-instance-slot-set! obj slot value)
  (unless (rt-instance? obj)
    (runtime-raise 'type-error "slot-value target must be instance" obj))
  (unless (symbol? slot)
    (runtime-raise 'type-error "slot-value slot must be symbol" slot))
  (let ((entry (find-instance-slot-entry (rt-instance-slots obj) slot)))
    (if entry
        (begin
          (set-cdr! entry value)
          value)
        (runtime-raise 'invalid-ir "No such slot in instance" slot))))

(define (rt-class-distance sub super)
  (let walk ((current sub) (seen '()))
    (cond
     ((eq? current super) 0)
     ((memq current seen) #f)
     (else
      (let ((seen2 (cons current seen)))
        (let loop ((supers (rt-class-supers current)) (best #f))
          (if (null? supers)
              best
              (let ((d (walk (car supers) seen2)))
                (if d
                    (let ((cand (+ d 1)))
                      (if (or (not best) (< cand best))
                          (loop (cdr supers) cand)
                          (loop (cdr supers) best)))
                    (loop (cdr supers) best))))))))))

;; score<? / score=? は isl.core から取り込む（R7）。

;; Map a native host value to its rt-class object via the runtime env.
(define (rt-native-value->class-obj val env)
  (define (lookup name)
    (let ((p (env-find-pair env name)))
      (if p (runtime-value->host (cdr p)) #f)))
  (cond
   ((and (integer? val) (exact? val)) (lookup '<integer>))
   ((and (number? val) (inexact? val)) (lookup '<float>))
   ((number? val)   (lookup '<number>))
   ((string? val)   (lookup '<string>))
   ((symbol? val)   (lookup '<symbol>))
   ((char? val)     (lookup '<character>))
   ((pair? val)     (lookup '<cons>))
   ((null? val)     (lookup '<null>))
   ((vector? val)   (lookup '<general-vector>))
   ((procedure? val)(lookup '<function>))
   ((or (input-port? val) (output-port? val)) (lookup '<stream>))
   (else            (lookup '<object>))))

;; Check if a value is an instance of a named class (including native values).
(define (rt-isl-instance-of? obj cname env)
  (let* ((class-pair (env-find-pair env cname))
         (target-class (if class-pair (runtime-value->host (cdr class-pair)) #f)))
    (if (not (rt-class? target-class))
        #f
        (let ((obj-class (if (rt-instance? obj)
                             (rt-instance-class obj)
                             (rt-native-value->class-obj obj env))))
          (if obj-class
              (if (rt-class-distance obj-class target-class) #t #f)
              #f)))))

(define (method-applicability-score method args env)
  (let ((specializers (rt-method-specializers method)))
    (if (< (length args) (length specializers))
        #f
        (let loop ((ss specializers) (as args) (acc '()))
          (if (null? ss)
              (reverse acc)
              (let ((spec (car ss))
                    (arg  (runtime-value->host (car as))))
                (if spec
                    (let ((arg-class (if (rt-instance? arg)
                                        (rt-instance-class arg)
                                        (rt-native-value->class-obj arg env))))
                      (if arg-class
                          (let ((d (rt-class-distance arg-class spec)))
                            (if d
                                (loop (cdr ss) (cdr as) (cons d acc))
                                #f))
                          #f))
                    (loop (cdr ss) (cdr as) (cons 1000000 acc)))))))))

(define (select-generic-method generic args env)
  (let loop ((methods (rt-generic-methods generic))
             (best-method #f)
             (best-score #f))
    (if (null? methods)
        best-method
        (let* ((m (car methods))
               (score (method-applicability-score m args env)))
          (cond
           ((not score)
            (loop (cdr methods) best-method best-score))
           ((or (not best-score) (score<? score best-score))
            (loop (cdr methods) m score))
           ((score=? score best-score)
            (runtime-raise 'invalid-ir
                           "Ambiguous applicable methods in generic function"
                           (rt-generic-name generic)))
           (else
            (loop (cdr methods) best-method best-score)))))))

(define (ensure-generic-function! sym env params)
  (let ((pair (env-find-pair env sym)))
    (cond
     ((and pair
           (rt-value? (cdr pair))
           (eq? (rt-tag (cdr pair)) 'function)
           (rt-generic? (rt-payload (cdr pair))))
      (rt-payload (cdr pair)))
     (pair
      (runtime-raise 'type-error "Binding already exists and is not a generic function" sym))
     (else
      (let ((g (make-rt-generic sym params '())))
        (env-define! env sym (make-fn-value g))
        g)))))

(define (parse-method-params params env)
  (unless (list? params)
    (runtime-raise 'invalid-ir "defmethod parameter list must be list" params))
  (let parse-required ((xs params) (plain-required '()) (specializers '()))
    (if (or (null? xs)
            (eq? (car xs) '&optional)
            (eq? (car xs) '&rest)
            (eq? (car xs) '&body))
        (let* ((plain-params (append (reverse plain-required) xs))
               (spec (parse-params plain-params))
               (required-spec (car spec)))
          (when (null? required-spec)
            (runtime-raise 'invalid-ir "defmethod requires at least one required parameter" params))
          (unless (= (length required-spec) (length specializers))
            (runtime-raise 'invalid-ir "defmethod required parameter parse mismatch" params))
          (list plain-params (reverse specializers)))
        (let ((x (car xs)))
          (cond
           ((symbol? x)
            (parse-required (cdr xs)
                            (cons x plain-required)
                            (cons #f specializers)))
           ((and (list? x) (= (length x) 2) (symbol? (car x)))
            (parse-required (cdr xs)
                            (cons (car x) plain-required)
                            (cons (resolve-class-designator (cadr x) env) specializers)))
           (else
            (runtime-raise 'invalid-ir "invalid defmethod required parameter" x)))))))

(define (runtime-compare-binop who pred args)
  (unless (= (length args) 2)
    (runtime-raise 'arity (string-append who " expects 2 arguments") args))
  (let ((a (runtime-number (car args) who))
        (b (runtime-number (cadr args) who)))
    (host->runtime-value (if (pred a b) #t '()))))

(define (runtime-decode-process-exit-status raw)
  (if (and (integer? raw) (>= raw 0))
      (quotient raw 256)
      raw))

(define (runtime-arith who op identity args)
  (if (null? args)
      (host->runtime-value identity)
      (let loop ((xs args) (acc (runtime-number (car args) who)) (rest (cdr args)))
        (if (null? rest)
            (host->runtime-value acc)
            (loop xs (op acc (runtime-number (car rest) who)) (cdr rest))))))

;; Module-level handler-case tag matcher (shared by the %handler-case primitive
;; that top-level handler-case lowers into).  `tag` is a resolved class symbol.
(define (rt-htag-match? tag cond-obj)
  (cond
   ((or (eq? tag 't) (eq? tag 'otherwise)) #t)
   (else
    (with-module isl.core
      (let ((bare (symbol-base-name tag)))
        (if (isl-condition? cond-obj)
            (or (isl-subclassp? (isl-condition-class cond-obj) tag)
                (and (string=? bare "error")
                     (isl-subclassp? (isl-condition-class cond-obj)
                                     (resolve-binding-symbol '<error>)))
                (and (string=? bare "serious-condition")
                     (isl-subclassp? (isl-condition-class cond-obj)
                                     (resolve-binding-symbol '<serious-condition>)))
                (and (string=? bare "condition")
                     (isl-subclassp? (isl-condition-class cond-obj)
                                     (resolve-binding-symbol '<condition>))))
            (or (string=? bare "error")
                (string=? bare "condition")
                (string=? bare "serious-condition"))))))))

;; clock ticks per second (sys-times[4]) or fallback 100; used by
;; get-internal-real-time / get-internal-run-time primitives.
(define (rt-internal-time-units)
  (guard (e (else 100))
    (let ((ts (sys-times)))
      (if (and (list? ts) (>= (length ts) 5)
               (integer? (list-ref ts 4)) (> (list-ref ts 4) 0))
          (list-ref ts 4)
          100))))

;; nth list accessor for sixth..tenth primitives (args = single rt list).
(define (rt-list-ref args n who)
  (unless (= (length args) 1)
    (runtime-raise 'arity (string-append who " expects 1 argument") args))
  (let ((lst (runtime-list (car args) who)))
    (if (> (length lst) n)
        (list-ref lst n)
        (runtime-raise 'domain (string-append who ": list too short") lst))))

(include "runtime/primitives.scm")
(include "runtime/special-forms.scm")
(define (runtime-eval-top top-ir state)
  (unless (and (pair? top-ir) (symbol? (car top-ir)))
    (runtime-raise 'invalid-ir "Top IR must be tagged" top-ir))
  (let ((global (state-global-env state)))
    (case (car top-ir)
      ((define-fun)
       (if (= (length top-ir) 4)
           (let ((name (cadr top-ir))
                 (params (caddr top-ir))
                 (body (cadddr top-ir)))
             (unless (symbol? name)
               (runtime-raise 'invalid-ir "define-fun name must be symbol" name))
             (let ((fn (make-fn-value (make-closure name params body global))))
               (env-define! global name fn)
               fn))
           (runtime-raise 'invalid-ir "define-fun expects name params body" top-ir)))
      ((ll-define-fun)
       (if (= (length top-ir) 4)
           (let ((name (cadr top-ir))
                 (params (caddr top-ir))
                 (cfg (cadddr top-ir)))
             (unless (symbol? name)
               (runtime-raise 'invalid-ir "ll-define-fun name must be symbol" name))
             (let ((fn (make-fn-value (make-closure/cfg name params cfg global))))
               (env-define! global name fn)
               fn))
           (runtime-raise 'invalid-ir "ll-define-fun expects name params cfg" top-ir)))
      ((define-global)
       (if (= (length top-ir) 3)
           (let ((name (cadr top-ir))
                 (expr-ir (caddr top-ir)))
             (unless (symbol? name)
               (runtime-raise 'invalid-ir "define-global name must be symbol" name))
             (let ((val (runtime-eval-expr expr-ir global state)))
               (env-define! global name val)
               val))
           (runtime-raise 'invalid-ir "define-global expects name expr" top-ir)))
      ((ll-define-global)
       (if (= (length top-ir) 3)
           (let ((name (cadr top-ir))
                 (cfg (caddr top-ir)))
             (unless (symbol? name)
               (runtime-raise 'invalid-ir "ll-define-global name must be symbol" name))
             (let ((val (runtime-exec-cfg cfg global state)))
               (env-define! global name val)
               val))
           (runtime-raise 'invalid-ir "ll-define-global expects name cfg" top-ir)))
      ((define-macro)
       ;; Macros are compile-time only and should be eliminated from executable IR.
       (host->runtime-value '()))
      ((ll-define-macro)
       ;; compile-time only
       (host->runtime-value '()))
      ;; ---- Phase 6-B: defvar with define-if-not-bound semantics ----
      ((define-var)
       (if (= (length top-ir) 3)
           (let ((name     (cadr top-ir))
                 (expr-ir  (caddr top-ir)))
             (unless (symbol? name)
               (runtime-raise 'invalid-ir "define-var name must be symbol" name))
             (if (env-find-pair global name)
                 ;; Already bound — do nothing, return name
                 (host->runtime-value name)
                 (let ((val (runtime-eval-expr expr-ir global state)))
                   (env-define! global name val)
                   ;; Register in dynamic variable table
                   (let ((tbl (*dynamic-var-table*)))
                     (unless (hash-table-exists? tbl name)
                       (hash-table-put! tbl name (runtime-value->host val))))
                   (host->runtime-value name))))
           (runtime-raise 'invalid-ir "define-var expects name expr" top-ir)))
      ;; ---- defdynamic (§23.1) — always (re)defines and registers dynamic ----
      ((define-dynamic)
       (if (= (length top-ir) 3)
           (let ((name    (cadr top-ir))
                 (expr-ir (caddr top-ir)))
             (unless (symbol? name)
               (runtime-raise 'invalid-ir "define-dynamic name must be symbol" name))
             (let ((val (runtime-eval-expr expr-ir global state)))
               (env-define! global name val)
               (hash-table-put! (*dynamic-var-table*) name (runtime-value->host val))
               (host->runtime-value name)))
           (runtime-raise 'invalid-ir "define-dynamic expects name expr" top-ir)))
      ((ll-define-var)
       ;; Phase 6-B: lowered defvar — use CFG executor
       (if (= (length top-ir) 3)
           (let ((name (cadr top-ir))
                 (cfg  (caddr top-ir)))
             (unless (symbol? name)
               (runtime-raise 'invalid-ir "ll-define-var name must be symbol" name))
             (if (env-find-pair global name)
                 (host->runtime-value name)
                 (let ((val (runtime-exec-cfg cfg global state)))
                   (env-define! global name val)
                   (let ((tbl (*dynamic-var-table*)))
                     (unless (hash-table-exists? tbl name)
                       (hash-table-put! tbl name (runtime-value->host val))))
                   (host->runtime-value name))))
           (runtime-raise 'invalid-ir "ll-define-var expects name cfg" top-ir)))
      ;; ---- ll-define-dynamic: defdynamic lowered form ----
      ((ll-define-dynamic)
       (if (= (length top-ir) 3)
           (let ((name (cadr top-ir))
                 (cfg  (caddr top-ir)))
             (unless (symbol? name)
               (runtime-raise 'invalid-ir "ll-define-dynamic name must be symbol" name))
             (let ((val (runtime-exec-cfg cfg global state)))
               (env-define! global name val)
               (hash-table-put! (*dynamic-var-table*) name (runtime-value->host val))
               (host->runtime-value name)))
           (runtime-raise 'invalid-ir "ll-define-dynamic expects name cfg" top-ir)))
      ((expr)
       (if (= (length top-ir) 2)
           (runtime-eval-expr (cadr top-ir) global state)
           (runtime-raise 'invalid-ir "expr expects one payload" top-ir)))
      ((ll-expr)
       (if (= (length top-ir) 2)
           (runtime-exec-cfg (cadr top-ir) global state)
           (runtime-raise 'invalid-ir "ll-expr expects one cfg payload" top-ir)))
      ((invalid-top)
       (runtime-raise 'invalid-ir "invalid top-level form" top-ir))
      (else
       (runtime-raise 'invalid-ir "unknown top-level IR node" top-ir)))))

(define (runtime-load-units! state units)
  (let loop ((xs units) (last (host->runtime-value '())))
    (if (null? xs)
        (begin
          (set-state-last-value! state last)
          last)
        (let* ((unit (car xs))
               (ll-pair (and (pair? unit) (assoc 'll (cdr unit))))
               (ir-pair (and (pair? unit) (assoc 'ir (cdr unit))))
               (top (cond
                     (ll-pair (cadr ll-pair))
                     (ir-pair (cadr ir-pair))
                     (else #f))))
          (unless top
            (runtime-raise 'invalid-unit "unit has no ll/ir field" unit))
          (loop (cdr xs)
                (guard (e
                        ((nonlocal-return? e)
                         (runtime-raise 'control-error "No enclosing block for return-from" (nonlocal-return-name e)))
                        ((throw-signal? e)
                         (runtime-raise 'control-error "No enclosing catch for throw" (throw-signal-tag e)))
                        ((go-signal? e)
                         (runtime-raise 'control-error "No enclosing tagbody for go" (go-signal-tag e)))
                        (else (raise e)))
                  (runtime-eval-top top state)))))))

(define (runtime-load-file! state path)
  (let ((units (frontend-compile-file path (state-frontend-env state))))
    (runtime-load-units! state units)))
