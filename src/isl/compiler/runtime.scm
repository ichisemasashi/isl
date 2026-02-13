(define-module isl.compiler.runtime
  (use srfi-1)
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
   ((pair? v) 'pair)
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
         (not (null? v)))))

;; Lexical environment.
(define (make-env parent)
  (vector 'env parent '()))

(define (env-parent env) (vector-ref env 1))
(define (env-bindings env) (vector-ref env 2))
(define (set-env-bindings! env bindings) (vector-set! env 2 bindings))

(define (env-define! env sym val)
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
  (let loop ((e env))
    (if (not e)
        #f
        (let ((p (assoc sym (env-bindings e))))
          (if p p (loop (env-parent e)))))))

(define (env-ref env sym)
  (let ((p (env-find-pair env sym)))
    (if p
        (cdr p)
        (runtime-raise 'unbound-variable "Unbound variable" sym))))

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
   (else
    (runtime-raise 'invalid-params "invalid parameter list" params))))

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

(define (runtime-compare-binop who pred args)
  (unless (= (length args) 2)
    (runtime-raise 'arity (string-append who " expects 2 arguments") args))
  (let ((a (runtime-number (car args) who))
        (b (runtime-number (cadr args) who)))
    (host->runtime-value (if (pred a b) #t #f))))

(define (runtime-arith who op identity args)
  (if (null? args)
      (host->runtime-value identity)
      (let loop ((xs args) (acc (runtime-number (car args) who)) (rest (cdr args)))
        (if (null? rest)
            (host->runtime-value acc)
            (loop xs (op acc (runtime-number (car rest) who)) (cdr rest))))))

(define (install-primitives! env)
  (define (def name proc)
    (env-define! env name (make-fn-value (make-primitive name proc))))
  (def '+
    (lambda (args state)
      (runtime-arith "+" + 0 args)))
  (def '*
    (lambda (args state)
      (runtime-arith "*" * 1 args)))
  (def '-
    (lambda (args state)
      (cond
       ((null? args)
        (runtime-raise 'arity "- expects at least 1 argument" args))
       ((null? (cdr args))
        (host->runtime-value (- (runtime-number (car args) "-"))))
       (else
        (let loop ((acc (runtime-number (car args) "-")) (rest (cdr args)))
          (if (null? rest)
              (host->runtime-value acc)
              (loop (- acc (runtime-number (car rest) "-")) (cdr rest))))))))
  (def '/
    (lambda (args state)
      (cond
       ((null? args)
        (runtime-raise 'arity "/ expects at least 1 argument" args))
       ((null? (cdr args))
        (host->runtime-value (/ 1 (runtime-number (car args) "/"))))
       (else
        (let loop ((acc (runtime-number (car args) "/")) (rest (cdr args)))
          (if (null? rest)
              (host->runtime-value acc)
              (loop (/ acc (runtime-number (car rest) "/")) (cdr rest))))))))
  (def '=
    (lambda (args state)
      (runtime-compare-binop "=" = args)))
  (def '<
    (lambda (args state)
      (runtime-compare-binop "<" < args)))
  (def '>
    (lambda (args state)
      (runtime-compare-binop ">" > args)))
  (def '<=
    (lambda (args state)
      (runtime-compare-binop "<=" <= args)))
  (def '>=
    (lambda (args state)
      (runtime-compare-binop ">=" >= args)))
  (def 'list
    (lambda (args state)
      (host->runtime-value (map runtime-value->host args))))
  (def 'funcall
    (lambda (args state)
      (unless (>= (length args) 1)
        (runtime-raise 'arity "funcall expects at least 1 argument" args))
      (runtime-apply (car args) (cdr args) state)))
  (def 'not
    (lambda (args state)
      (unless (= (length args) 1)
        (runtime-raise 'arity "not expects 1 argument" args))
      (host->runtime-value (if (runtime-truthy? (car args)) #f #t))))
  (def 'print
    (lambda (args state)
      (unless (= (length args) 1)
        (runtime-raise 'arity "print expects 1 argument" args))
      (write (runtime-value->host (car args)))
      (newline)
      (car args))))

(define (make-runtime-state . maybe-profile)
  (let ((profile (if (null? maybe-profile) 'extended (car maybe-profile)))
        (global-env (make-env #f)))
    (env-define! global-env 'nil (host->runtime-value '()))
    (env-define! global-env 't (host->runtime-value #t))
    (install-primitives! global-env)
    (vector 'runtime-state profile global-env (host->runtime-value '()))))

(define (state-global-env state) (vector-ref state 2))
(define (state-last-value state) (vector-ref state 3))
(define (set-state-last-value! state v) (vector-set! state 3 v))

(define (runtime-eval-seq exprs env state)
  (let loop ((xs exprs) (last (host->runtime-value '())))
    (if (null? xs)
        last
        (loop (cdr xs) (runtime-eval-expr (car xs) env state)))))

(define (find-block cfg label)
  (let* ((blocks (caddr cfg))
         (p (find (lambda (b) (and (pair? b) (eq? (cadr b) label))) blocks)))
    (if p
        p
        (runtime-raise 'invalid-ir "unknown block label" label))))

(define (phi-pairs rhs)
  (if (and (pair? rhs) (eq? (car rhs) 'phi) (pair? (cdr rhs)))
      (cadr rhs)
      #f))

(define (eval-rhs rhs env state pred-label)
  (unless (and (pair? rhs) (symbol? (car rhs)))
    (runtime-raise 'invalid-ir "invalid assignment rhs" rhs))
  (case (car rhs)
    ((const)
     (if (= (length rhs) 2)
         (host->runtime-value (cadr rhs))
         (runtime-raise 'invalid-ir "const rhs expects one payload" rhs)))
    ((var)
     (if (= (length rhs) 2)
         (env-ref env (cadr rhs))
         (runtime-raise 'invalid-ir "var rhs expects one symbol" rhs)))
    ((call)
     (let ((fn (env-ref env (cadr rhs)))
           (args (map (lambda (s) (env-ref env s)) (caddr rhs))))
       (runtime-apply fn args state)))
    ((lambda)
     (if (= (length rhs) 3)
         (make-fn-value (make-closure #f (cadr rhs) (caddr rhs) env))
         (runtime-raise 'invalid-ir "lambda rhs expects params/body" rhs)))
    ((special)
     (runtime-raise 'unsupported-special "special form is not yet lowered" rhs))
    ((invalid-special)
     (runtime-raise 'invalid-ir "invalid special form shape in rhs" rhs))
    ((phi)
     (let ((pairs (phi-pairs rhs)))
       (unless (list? pairs)
         (runtime-raise 'invalid-ir "phi rhs expects predecessor pairs" rhs))
       (let ((selected
              (if pred-label
                  (find (lambda (x) (and (pair? x) (eq? (car x) pred-label))) pairs)
                  #f)))
         (if selected
             (env-ref env (cadr selected))
             (if (null? pairs)
                 (runtime-raise 'invalid-ir "phi has no incoming pairs" rhs)
                 (env-ref env (cadar pairs)))))))
    (else
     (runtime-raise 'invalid-ir "unknown assignment rhs op" rhs))))

(define (exec-block-instrs instrs env state pred-label)
  (for-each
   (lambda (instr)
     (unless (and (pair? instr) (eq? (car instr) 'assign) (= (length instr) 3))
       (runtime-raise 'invalid-ir "instruction must be (assign dst rhs)" instr))
     (let ((dst (cadr instr))
           (rhs (caddr instr)))
       (unless (symbol? dst)
         (runtime-raise 'invalid-ir "assign destination must be symbol" dst))
       (env-define! env dst (eval-rhs rhs env state pred-label))))
   instrs))

(define (runtime-exec-cfg cfg env state)
  (unless (and (pair? cfg) (eq? (car cfg) 'cfg) (= (length cfg) 3))
    (runtime-raise 'invalid-ir "cfg shape must be (cfg entry blocks)" cfg))
  (let ((entry (cadr cfg)))
    (let loop ((label entry) (pred #f))
      (let* ((block (find-block cfg label))
             (instrs (caddr block))
             (term (cadddr block)))
        (exec-block-instrs instrs env state pred)
        (unless (and (pair? term) (symbol? (car term)))
          (runtime-raise 'invalid-ir "block terminator is missing or invalid" block))
        (case (car term)
          ((ret)
           (if (= (length term) 2)
               (env-ref env (cadr term))
               (runtime-raise 'invalid-ir "ret expects one value symbol" term)))
          ((jmp)
           (if (= (length term) 2)
               (loop (cadr term) label)
               (runtime-raise 'invalid-ir "jmp expects target label" term)))
          ((br)
           (if (= (length term) 4)
               (let ((cond-v (env-ref env (cadr term))))
                 (if (runtime-truthy? cond-v)
                     (loop (caddr term) label)
                     (loop (cadddr term) label)))
               (runtime-raise 'invalid-ir "br expects cond then else labels" term)))
          (else
           (runtime-raise 'invalid-ir "unknown terminator op" term)))))))

(define (runtime-apply fn-val arg-vals state)
  (let ((fn (fn-from-value fn-val)))
    (cond
     ((primitive? fn)
      ((primitive-proc fn) arg-vals state))
     ((closure? fn)
      (let ((call-env (make-env (closure-env fn))))
        (bind-params! call-env (closure-params fn) arg-vals)
        (if (eq? (closure-kind fn) 'cfg)
            (runtime-exec-cfg (closure-body fn) call-env state)
            (runtime-eval-expr (closure-body fn) call-env state))))
     (else
      (runtime-raise 'type-error "invalid function object" fn)))))

(define (runtime-eval-expr ir env state)
  (unless (and (pair? ir) (symbol? (car ir)))
    (runtime-raise 'invalid-ir "IR expression must be a tagged list" ir))
  (case (car ir)
    ((const)
     (if (= (length ir) 2)
         (host->runtime-value (cadr ir))
         (runtime-raise 'invalid-ir "const expects one payload" ir)))
    ((var)
     (if (= (length ir) 2)
         (env-ref env (cadr ir))
         (runtime-raise 'invalid-ir "var expects one symbol" ir)))
    ((if)
     (if (= (length ir) 4)
         (let ((test (runtime-eval-expr (cadr ir) env state)))
           (if (runtime-truthy? test)
               (runtime-eval-expr (caddr ir) env state)
               (runtime-eval-expr (cadddr ir) env state)))
         (runtime-raise 'invalid-ir "if expects test/then/else" ir)))
    ((seq)
     (runtime-eval-seq (cdr ir) env state))
    ((lambda)
     (if (= (length ir) 3)
         (make-fn-value (make-closure #f (cadr ir) (caddr ir) env))
         (runtime-raise 'invalid-ir "lambda expects params/body" ir)))
    ((call)
     (if (>= (length ir) 2)
         (let ((fn (runtime-eval-expr (cadr ir) env state))
               (args (map (lambda (x) (runtime-eval-expr x env state)) (cddr ir))))
           (runtime-apply fn args state))
         (runtime-raise 'invalid-ir "call expects callee" ir)))
    ((special)
     (runtime-raise 'unsupported-special "special form is not yet lowered" ir))
    ((invalid-special)
     (runtime-raise 'invalid-ir "invalid special form shape" ir))
    (else
     (runtime-raise 'invalid-ir "unknown IR node" ir))))

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
                (runtime-eval-top top state))))))
