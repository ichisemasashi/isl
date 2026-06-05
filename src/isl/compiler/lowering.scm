(define-module isl.compiler.lowering
  (export lower-expr-to-cfg
          lower-top-level-form
          render-ll))

(select-module isl.compiler.lowering)

;; Lower-state
;; #(lower-state next-temp next-label blocks order)
;; blocks: ((label . #(instrs term)) ...)

(define (make-state)
  (vector 'lower-state 0 0 '() '()))

(define (state-next-temp st) (vector-ref st 1))
(define (state-next-label st) (vector-ref st 2))
(define (state-blocks st) (vector-ref st 3))
(define (state-order st) (vector-ref st 4))
(define (set-state-next-temp! st v) (vector-set! st 1 v))
(define (set-state-next-label! st v) (vector-set! st 2 v))
(define (set-state-blocks! st v) (vector-set! st 3 v))
(define (set-state-order! st v) (vector-set! st 4 v))

(define (fresh-temp st)
  (let ((n (state-next-temp st)))
    (set-state-next-temp! st (+ n 1))
    (string->symbol (string-append "t" (number->string n)))))

(define (fresh-label st)
  (let ((n (state-next-label st)))
    (set-state-next-label! st (+ n 1))
    (string->symbol (string-append "bb" (number->string n)))))

(define (block-cell instrs term)
  (vector instrs term))

(define (block-instrs c) (vector-ref c 0))
(define (block-term c) (vector-ref c 1))
(define (set-block-instrs! c v) (vector-set! c 0 v))
(define (set-block-term! c v) (vector-set! c 1 v))

(define (ensure-block! st label)
  (let ((p (assq label (state-blocks st))))
    (if p
        (cdr p)
        (let ((c (block-cell '() #f)))
          (set-state-blocks! st (cons (cons label c) (state-blocks st)))
          (set-state-order! st (append (state-order st) (list label)))
          c))))

(define (emit! st label instr)
  (let* ((c (ensure-block! st label))
         (xs (block-instrs c)))
    (set-block-instrs! c (append xs (list instr)))))

(define (terminate! st label term)
  (let ((c (ensure-block! st label)))
    (set-block-term! c term)))

(define (terminated? st label)
  (let ((c (ensure-block! st label)))
    (if (block-term c) #t #f)))

(define (build-block-list st)
  (map (lambda (label)
         (let ((c (ensure-block! st label)))
           (list 'block label (block-instrs c) (block-term c))))
       (state-order st)))

;; Returns (list end-label result-temp)
(define (lower-expr expr st label)
  (unless (and (pair? expr) (symbol? (car expr)))
    (error "lower-expr expects normalized IR expression" expr))
  (case (car expr)
    ((const)
     (let ((dst (fresh-temp st)))
       (emit! st label (list 'assign dst (list 'const (cadr expr))))
       (list label dst)))
    ((var)
     (let ((dst (fresh-temp st)))
       (emit! st label (list 'assign dst (list 'var (cadr expr))))
       (list label dst)))
    ((lambda)
     (let ((dst (fresh-temp st)))
       ;; Lambda body stays in normalized form for now; codegen can outline later.
       (emit! st label (list 'assign dst (list 'lambda (cadr expr) (caddr expr))))
       (list label dst)))
    ((seq)
     (let loop ((xs (cdr expr)) (cur label) (last #f))
       (if (null? xs)
           (if last
               (list cur last)
               (let ((dst (fresh-temp st)))
                 (emit! st cur (list 'assign dst (list 'const '())))
                 (list cur dst)))
           (let* ((r (lower-expr (car xs) st cur))
                  (next-label (car r))
                  (v (cadr r)))
             (loop (cdr xs) next-label v)))))
    ((call)
     (let* ((callee-r (lower-expr (cadr expr) st label))
            (cur (car callee-r))
            (fvar (cadr callee-r)))
       (let loop ((xs (cddr expr)) (lbl cur) (acc '()))
         (if (null? xs)
             (let ((dst (fresh-temp st)))
               (emit! st lbl (list 'assign dst (list 'call fvar (reverse acc))))
               (list lbl dst))
             (let* ((r (lower-expr (car xs) st lbl))
                    (lbl2 (car r))
                    (v (cadr r)))
               (loop (cdr xs) lbl2 (cons v acc)))))))
    ((effect)
     (if (= (length expr) 2)
         (lower-expr (cadr expr) st label)
         (error "effect node expects one child" expr)))
    ((if)
     (let* ((test-r (lower-expr (cadr expr) st label))
            (after-test (car test-r))
            (test-v (cadr test-r))
            (then-label (fresh-label st))
            (else-label (fresh-label st))
            (merge-label (fresh-label st)))
       (terminate! st after-test (list 'br test-v then-label else-label))
       (let* ((then-r (lower-expr (caddr expr) st then-label))
              (then-end (car then-r))
              (then-v (cadr then-r))
              (else-r (lower-expr (cadddr expr) st else-label))
              (else-end (car else-r))
              (else-v (cadr else-r)))
         (unless (terminated? st then-end)
           (terminate! st then-end (list 'jmp merge-label)))
         (unless (terminated? st else-end)
           (terminate! st else-end (list 'jmp merge-label)))
         (let ((dst (fresh-temp st)))
           (emit! st merge-label
                  (list 'assign dst
                        (list 'phi
                              (list (list then-end then-v)
                                    (list else-end else-v)))))
           (list merge-label dst)))))
    ((special)
     (let* ((sop (cadr expr))
            (payload (caddr expr))
            (generic
             (lambda ()
               (let ((dst (fresh-temp st)))
                 (emit! st label (list 'assign dst (list 'special sop payload)))
                 (list label dst)))))
       (cond
        ;; Lower control/binding special forms into core IR (if/lambda/call) so
        ;; backends that only understand those (e.g. the native LLVM codegen)
        ;; can run them.  The IR interpreter consumes the same lowered CFG.
        ;; Malformed binding lists fall through to the generic `special` node so
        ;; the IR interpreter still reports the precise validation error.
        ((eq? sop 'cond) (lower-cond payload st label))
        ((eq? sop 'setq) (lower-setq payload st label))
        ((eq? sop 'while) (lower-while payload st label))
        ((eq? sop 'dotimes) (lower-dotimes payload st label))
        ((eq? sop 'dolist) (lower-dolist payload st label))
        ((eq? sop 'for) (lower-for payload st label))
        ((eq? sop 'tagbody) (lower-tagbody payload st label))
        ((eq? sop 'go) (lower-go payload st label))
        ((eq? sop 'flet) (lower-flet payload st label))
        ((eq? sop 'labels) (lower-labels payload st label))
        ;; the/assure: assert the value's class (%the checks built-in classes;
        ;; unknown classes pass through) and return it.
        ((eq? sop 'the)
         (lower-expr (mk-call '%the (list 'const (car payload)) (cadr payload)) st label))
        ((eq? sop 'assure)
         (lower-expr (mk-call '%the (list 'const (car payload)) (cadr payload)) st label))
        ((eq? sop 'dynamic)
         (lower-expr (mk-call '%dynamic-get (list 'const (car payload))) st label))
        ((eq? sop 'dynamic-let) (lower-dynamic-let payload st label))
        ((eq? sop 'unwind-protect)
         (lower-expr (list 'call (list 'var '%unwind-protect)
                           (list 'lambda '() (car payload))
                           (list 'lambda '() (cons 'seq (cadr payload))))
                     st label))
        ((eq? sop 'and) (lower-and payload st label))
        ((eq? sop 'or) (lower-or payload st label))
        ((eq? sop 'case) (lower-case payload st label))
        ;; Non-local exit: rewrite into runtime-primitive calls.  The body of a
        ;; block/catch becomes a zero-argument thunk so the C runtime can wrap a
        ;; setjmp around it (see %catch/%block in llvm_runtime.c).
        ((eq? sop 'block)
         (lower-expr (list 'call (list 'var '%block)
                           (list 'const (car payload))
                           (list 'lambda '() (cadr payload)))
                     st label))
        ((eq? sop 'return-from)
         (lower-expr (list 'call (list 'var '%return-from)
                           (list 'const (car payload))
                           (cadr payload))
                     st label))
        ((eq? sop 'catch)
         (lower-expr (list 'call (list 'var '%catch)
                           (car payload)
                           (list 'lambda '() (cadr payload)))
                     st label))
        ((eq? sop 'throw)
         (lower-expr (list 'call (list 'var '%throw)
                           (car payload)
                           (cadr payload))
                     st label))
        ((and (eq? sop 'let) (lowerable-let-bindings? (car payload)))
         (lower-expr (let-bindings->call (car payload) (cadr payload)) st label))
        ((and (eq? sop 'let*) (lowerable-let-bindings? (car payload)))
         (lower-expr (let*-bindings->call (car payload) (cadr payload)) st label))
        (else (generic)))))
    ((invalid-special)
     (let ((dst (fresh-temp st)))
       (emit! st label (list 'assign dst (list 'invalid-special expr)))
       (list label dst)))
    (else
     (error "Unknown normalized node" expr))))

;; Only ordinary symbol variables can be lowered to lambda parameters; a
;; malformed binding (e.g. a keyword) is left to the IR interpreter to reject.
(define (lowerable-let-bindings? bindings)
  (let loop ((xs bindings))
    (cond ((null? xs) #t)
          ((not (pair? xs)) #f)
          ((and (pair? (car xs))
                (symbol? (caar xs))
                (not (keyword? (caar xs))))
           (loop (cdr xs)))
          (else #f))))

;; (let ((v init)...) body) == ((lambda (v...) body) init...)  — parallel binding.
(define (let-bindings->call bindings body)
  (cons 'call
        (cons (list 'lambda (map car bindings) body)
              (map cadr bindings))))

;; (let* ((v init)...) body) == nested single-binding lambdas — sequential.
(define (let*-bindings->call bindings body)
  (if (null? bindings)
      (cons 'call (list (list 'lambda '() body)))
      (let ((b (car bindings)))
        (list 'call
              (list 'lambda (list (car b))
                    (let*-bindings->call (cdr bindings) body))
              (cadr b)))))

;; Lower a cond into a branch/phi chain.  clauses: ((test body-or-#f) ...).
;; A clause with no body yields the test value when truthy; if no clause
;; matches the result is nil.  Returns (list end-label result-temp).
(define (lower-cond clauses st label)
  (let ((merge (fresh-label st)))
    (let loop ((cs clauses) (cur label) (phis '()))
      (if (null? cs)
          ;; fall-through: nothing matched -> nil
          (let ((dst (fresh-temp st)))
            (emit! st cur (list 'assign dst (list 'const '())))
            (let ((phis* (cons (list cur dst) phis)))
              (unless (terminated? st cur)
                (terminate! st cur (list 'jmp merge)))
              (let ((res (fresh-temp st)))
                (emit! st merge (list 'assign res (list 'phi (reverse phis*))))
                (list merge res))))
          (let* ((clause (car cs))
                 (test (car clause))
                 (body (cadr clause))
                 (test-r (lower-expr test st cur))
                 (after-test (car test-r))
                 (test-v (cadr test-r))
                 (then-label (fresh-label st))
                 (next-label (fresh-label st)))
            (terminate! st after-test (list 'br test-v then-label next-label))
            (if body
                (let* ((body-r (lower-expr body st then-label))
                       (body-end (car body-r))
                       (body-v (cadr body-r)))
                  (unless (terminated? st body-end)
                    (terminate! st body-end (list 'jmp merge)))
                  (loop (cdr cs) next-label (cons (list body-end body-v) phis)))
                ;; no body: the test value is the result
                (begin
                  (unless (terminated? st then-label)
                    (terminate! st then-label (list 'jmp merge)))
                  (loop (cdr cs) next-label (cons (list then-label test-v) phis)))))))))

;; (and e1 e2 ... en): evaluate left to right, short-circuit to the first
;; falsy value; if all truthy the value of the last form is the result.
;; Each subform is evaluated at most once (branch/phi form).
(define (lower-and forms st label)
  (if (null? forms)
      (let ((dst (fresh-temp st)))
        (emit! st label (list 'assign dst (list 'const #t)))
        (list label dst))
      (let ((merge (fresh-label st)))
        (let loop ((fs forms) (cur label) (phis '()))
          (let* ((r (lower-expr (car fs) st cur))
                 (after (car r))
                 (v (cadr r)))
            (if (null? (cdr fs))
                (begin
                  (unless (terminated? st after)
                    (terminate! st after (list 'jmp merge)))
                  (let ((res (fresh-temp st)))
                    (emit! st merge
                           (list 'assign res
                                 (list 'phi (reverse (cons (list after v) phis)))))
                    (list merge res)))
                (let ((cont (fresh-label st))
                      (shortc (fresh-label st)))
                  ;; v truthy -> continue; v falsy -> short-circuit with v
                  (terminate! st after (list 'br v cont shortc))
                  (terminate! st shortc (list 'jmp merge))
                  (loop (cdr fs) cont (cons (list shortc v) phis)))))))))

;; (or e1 e2 ... en): evaluate left to right, short-circuit to the first truthy
;; value; if all falsy the value of the last form is the result.
(define (lower-or forms st label)
  (if (null? forms)
      (let ((dst (fresh-temp st)))
        (emit! st label (list 'assign dst (list 'const '())))
        (list label dst))
      (let ((merge (fresh-label st)))
        (let loop ((fs forms) (cur label) (phis '()))
          (let* ((r (lower-expr (car fs) st cur))
                 (after (car r))
                 (v (cadr r)))
            (if (null? (cdr fs))
                (begin
                  (unless (terminated? st after)
                    (terminate! st after (list 'jmp merge)))
                  (let ((res (fresh-temp st)))
                    (emit! st merge
                           (list 'assign res
                                 (list 'phi (reverse (cons (list after v) phis)))))
                    (list merge res)))
                (let ((truec (fresh-label st))
                      (cont (fresh-label st)))
                  ;; v truthy -> short-circuit with v; v falsy -> continue
                  (terminate! st after (list 'br v truec cont))
                  (terminate! st truec (list 'jmp merge))
                  (loop (cdr fs) cont (cons (list truec v) phis)))))))))

;; (case key (clause ...)) where clause = ((datum ...) body) | (else body).
;; Lower by binding the key once to a fresh variable and rewriting into a cond
;; whose tests compare the key against each datum with `eql` (OR-combined).
;; Reuses let/cond/or lowering so the native backend can run it.
(define (fresh-case-sym st)
  (let ((n (state-next-temp st)))
    (set-state-next-temp! st (+ n 1))
    ;; leading space makes this name impossible to collide with user symbols
    (string->symbol (string-append " case-key-" (number->string n)))))

(define (case-clause->cond-clause g clause)
  (let ((data (car clause))
        (body (cadr clause)))
    (if (eq? data 'else)
        (list (list 'const #t) body)
        (let ((tests (map (lambda (d)
                            (list 'call (list 'var 'eql)
                                  (list 'var g) (list 'const d)))
                          data)))
          (list (if (= (length tests) 1)
                    (car tests)
                    (list 'special 'or tests))
                body)))))

(define (lower-case payload st label)
  (let ((keyform (car payload))
        (clauses (cadr payload))
        (g (fresh-case-sym st)))
    (let ((cond-clauses (map (lambda (cl) (case-clause->cond-clause g cl)) clauses)))
      (lower-expr
       (list 'call
             (list 'lambda (list g) (list 'special 'cond cond-clauses))
             keyform)
       st label))))

;; (setq (name val)...): assign each variable in turn via a `set` instruction
;; that both backends implement against the mutable runtime environment.  The
;; result is the value of the last assignment (nil if no pairs).
(define (lower-setq payload st label)
  (let loop ((xs payload) (cur label) (last #f))
    (cond
     ((null? xs)
      (if last
          (list cur last)
          (let ((d (fresh-temp st)))
            (emit! st cur (list 'assign d (list 'const '())))
            (list cur d))))
     (else
      (let* ((entry (car xs)))
        (unless (and (pair? entry) (= (length entry) 2) (symbol? (car entry)))
          (error "setq entry must be (symbol expr)" entry))
        (let* ((name (car entry))
               (r (lower-expr (cadr entry) st cur))
               (vlbl (car r))
               (vt (cadr r))
               (d (fresh-temp st)))
          (emit! st vlbl (list 'assign d (list 'set name vt)))
          (loop (cdr xs) vlbl d)))))))

;; ---- iteration: while / dotimes / dolist / for ----
;; Loops compile to a CFG with a back-edge.  Variables live in the mutable
;; runtime environment (read by name, written by `set`), so no phi nodes are
;; needed across the back-edge — each block recomputes its values from the env.

(define (fresh-loop-sym st base)
  (let ((n (state-next-temp st)))
    (set-state-next-temp! st (+ n 1))
    ;; leading space => cannot collide with a user-written symbol
    (string->symbol (string-append " " base "-" (number->string n)))))

(define (mk-call op . args) (cons 'call (cons (list 'var op) args)))
(define (mk-seq . forms) (cons 'seq forms))
(define (mk-setq name val) (list 'special 'setq (list (list name val))))

;; (while test body): test in a loop header, body then back-edge, nil after.
(define (lower-while payload st label)
  (unless (and (list? payload) (= (length payload) 2))
    (error "while payload must be (test body)" payload))
  (let ((test (car payload))
        (body (cadr payload))
        (header (fresh-label st))
        (body-lbl (fresh-label st))
        (after (fresh-label st)))
    (terminate! st label (list 'jmp header))
    (let* ((tr (lower-expr test st header)) (h-end (car tr)) (tv (cadr tr)))
      (terminate! st h-end (list 'br tv body-lbl after)))
    (let* ((bres (lower-expr body st body-lbl)) (b-end (car bres)))
      (unless (terminated? st b-end) (terminate! st b-end (list 'jmp header))))
    (let ((d (fresh-temp st)))
      (emit! st after (list 'assign d (list 'const '())))
      (list after d))))

;; (dotimes (var count result? body)) -> let + while + setq.
(define (lower-dotimes payload st label)
  (let* ((var (list-ref payload 0))
         (count (list-ref payload 1))
         (result (list-ref payload 2))
         (body (list-ref payload 3))
         (limit (fresh-loop-sym st "limit"))
         (loop (list 'special 'while
                     (list (mk-call '< (list 'var var) (list 'var limit))
                           (mk-seq body (mk-setq var (mk-call '+ (list 'var var) (list 'const 1)))))))
         (res (if result result (list 'const '())))
         (form (list 'special 'let
                     (list (list (list var (list 'const 0)) (list limit count))
                           (mk-seq loop res)))))
    (lower-expr form st label)))

;; (dolist (var list result? body)) -> let + while + setq.
(define (lower-dolist payload st label)
  (let* ((var (list-ref payload 0))
         (lst (list-ref payload 1))
         (result (list-ref payload 2))
         (body (list-ref payload 3))
         (rest (fresh-loop-sym st "rest"))
         (loopbody (mk-seq (mk-setq var (mk-call 'car (list 'var rest)))
                           body
                           (mk-setq rest (mk-call 'cdr (list 'var rest)))))
         (loop (list 'special 'while
                     (list (mk-call 'consp (list 'var rest)) loopbody)))
         (res (if result result (list 'const '())))
         (form (list 'special 'let
                     (list (list (list rest lst) (list var (list 'const '())))
                           (mk-seq loop res)))))
    (lower-expr form st label)))

;; (for (specs end-test result body)) with specs = ((sym init step?) ...).
;; Parallel step: bind each step value to a temp, then assign all.
(define (lower-for payload st label)
  (let* ((specs (list-ref payload 0))
         (end-test (list-ref payload 1))
         (result (list-ref payload 2))
         (body (list-ref payload 3))
         (inits (map (lambda (s) (list (car s) (cadr s))) specs))
         (stepped (let loop ((xs specs) (acc '()))
                    (cond ((null? xs) (reverse acc))
                          ((caddr (car xs)) (loop (cdr xs) (cons (car xs) acc)))
                          (else (loop (cdr xs) acc)))))
         (temps (map (lambda (s) (cons (car s) (fresh-loop-sym st "step"))) stepped))
         (pstep
          (if (null? stepped)
              (list 'const '())
              (list 'special 'let
                    (list (map (lambda (s) (list (cdr (assq (car s) temps)) (caddr s))) stepped)
                          (apply mk-seq
                                 (map (lambda (s) (mk-setq (car s) (list 'var (cdr (assq (car s) temps)))))
                                      stepped))))))
         (loop (list 'special 'while
                     (list (mk-call 'not end-test) (mk-seq body pstep))))
         (form (list 'special 'let (list inits (mk-seq loop result)))))
    (lower-expr form st label)))

;; ---- tagbody / go ----
;; Each tag becomes a CFG block; `go` is an intra-procedural jmp.  A stack of
;; tag->label maps (innermost first) is kept during lowering so a (possibly
;; nested) `go` can resolve its target lexically.
(define *tagbody-labels* (make-parameter '()))

(define (tagbody-lookup tag)
  (let loop ((frames (*tagbody-labels*)))
    (cond ((null? frames) #f)
          ((assq tag (car frames)) => cdr)
          (else (loop (cdr frames))))))

(define (lower-go payload st label)
  (let* ((tag (car payload))
         (target (tagbody-lookup tag)))
    (unless target (error "go: no enclosing tagbody label" tag))
    (unless (terminated? st label) (terminate! st label (list 'jmp target)))
    ;; code textually following a `go` is unreachable; give it a fresh block
    (let ((dead (fresh-label st)) (d (fresh-temp st)))
      (emit! st dead (list 'assign d (list 'const '())))
      (list dead d))))

(define (lower-tagbody payload st label)
  (let* ((label-map
          (let loop ((xs payload) (acc '()))
            (cond ((null? xs) (reverse acc))
                  ((and (pair? (car xs)) (eq? (caar xs) 'label))
                   (loop (cdr xs) (cons (cons (cadr (car xs)) (fresh-label st)) acc)))
                  (else (loop (cdr xs) acc)))))
         (after (fresh-label st)))
    (parameterize ((*tagbody-labels* (cons label-map (*tagbody-labels*))))
      (let loop ((xs payload) (cur label))
        (if (null? xs)
            (unless (terminated? st cur) (terminate! st cur (list 'jmp after)))
            (let ((item (car xs)))
              (cond
               ((and (pair? item) (eq? (car item) 'label))
                (let ((llbl (cdr (assq (cadr item) label-map))))
                  (unless (terminated? st cur) (terminate! st cur (list 'jmp llbl)))
                  (loop (cdr xs) llbl)))
               ((and (pair? item) (eq? (car item) 'form))
                (loop (cdr xs) (car (lower-expr (cadr item) st cur))))
               (else (error "tagbody item must be (label name) or (form expr)" item)))))))
    ;; tagbody evaluates to nil
    (let ((d (fresh-temp st)))
      (emit! st after (list 'assign d (list 'const '())))
      (list after d))))

;; ---- flet / labels (local functions) ----
;; A local function binding is (name params . body-forms).  In this backend the
;; operator and variable namespaces share the environment, so a local function
;; is just a variable bound to a closure.
;;   flet   : non-recursive — bind names to closures captured in the outer env
;;            => (let ((name (lambda params body)) ...) body)
;;   labels : mutually recursive — bind placeholders, then setq each to a closure
;;            captured in the (shared) labels env => closures see each other.
(define (flet-fn-body body-forms)
  (if (and (pair? body-forms) (null? (cdr body-forms)))
      (car body-forms)
      (cons 'seq body-forms)))

(define (lower-flet payload st label)
  (let* ((bindings (car payload))
         (body (cadr payload))
         (let-binds (map (lambda (b)
                           (list (car b)
                                 (list 'lambda (cadr b) (flet-fn-body (cddr b)))))
                         bindings))
         (form (list 'special 'let (list let-binds body))))
    (lower-expr form st label)))

(define (lower-labels payload st label)
  (let* ((bindings (car payload))
         (body (cadr payload))
         (placeholders (map (lambda (b) (list (car b) (list 'const '()))) bindings))
         (assigns (map (lambda (b)
                         (mk-setq (car b)
                                  (list 'lambda (cadr b) (flet-fn-body (cddr b)))))
                       bindings))
         (form (list 'special 'let
                     (list placeholders (apply mk-seq (append assigns (list body)))))))
    (lower-expr form st label)))

;; ---- dynamic variables ----
;; dynamic-let reduces to: save current values + evaluate new values (in the
;; outer dynamic env), then set the new values, run body, and restore on any
;; exit via unwind-protect.  %dynamic-get / %dynamic-set are runtime primitives.
(define (lower-dynamic-let payload st label)
  (let* ((bindings (car payload))
         (body (cadr payload))
         (saves (map (lambda (b) (cons (car b) (fresh-loop-sym st "dyn-old"))) bindings))
         (vals  (map (lambda (b) (cons (car b) (fresh-loop-sym st "dyn-val"))) bindings))
         (let-binds
          (apply append
                 (map (lambda (b)
                        (let ((name (car b)))
                          (list (list (cdr (assq name saves))
                                      (mk-call '%dynamic-get (list 'const name)))
                                (list (cdr (assq name vals)) (cadr b)))))
                      bindings)))
         (set-news (map (lambda (b)
                          (mk-call '%dynamic-set (list 'const (car b))
                                   (list 'var (cdr (assq (car b) vals)))))
                        bindings))
         (restores (map (lambda (b)
                          (mk-call '%dynamic-set (list 'const (car b))
                                   (list 'var (cdr (assq (car b) saves)))))
                        bindings))
         (protected (apply mk-seq (append set-news (list body))))
         (cleanup (apply mk-seq restores))
         (form (list 'special 'let
                     (list let-binds
                           (list 'special 'unwind-protect (list protected (list cleanup)))))))
    (lower-expr form st label)))

(define (lower-expr-to-cfg expr)
  (let* ((st (make-state))
         (entry (fresh-label st))
         (r (lower-expr expr st entry))
         (end (car r))
         (retv (cadr r)))
    (unless (terminated? st end)
      (terminate! st end (list 'ret retv)))
    (list 'cfg entry (build-block-list st))))

(define (lower-top-level-form top)
  (unless (and (pair? top) (symbol? (car top)))
    (error "lower-top-level-form expects top-level normalized IR" top))
  (case (car top)
    ((define-fun)
     (list 'll-define-fun
           (cadr top)
           (caddr top)
           (lower-expr-to-cfg (cadddr top))))
    ((define-global)
     (list 'll-define-global
           (cadr top)
           (lower-expr-to-cfg (caddr top))))
    ((define-var)
     ;; Phase 6-B: defvar — lower initializer to CFG, keep as ll-define-var
     (list 'll-define-var
           (cadr top)
           (lower-expr-to-cfg (caddr top))))
    ((define-dynamic)
     ;; §23.1: defdynamic — always defines and registers dynamic variable
     (list 'll-define-dynamic
           (cadr top)
           (lower-expr-to-cfg (caddr top))))
    ((define-macro)
     (list 'll-define-macro (cadr top) (caddr top) (cadddr top)))
    ((expr)
     (list 'll-expr (lower-expr-to-cfg (cadr top))))
    ((invalid-top)
     (list 'll-invalid top))
    (else
     (list 'll-invalid top))))

(define (render-ll ll)
  (let ((p (open-output-string)))
    (write ll p)
    (get-output-string p)))
