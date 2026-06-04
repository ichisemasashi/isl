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
        ((eq? sop 'and) (lower-and payload st label))
        ((eq? sop 'or) (lower-or payload st label))
        ((eq? sop 'case) (lower-case payload st label))
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
