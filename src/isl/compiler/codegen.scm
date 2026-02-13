(define-module isl.compiler.codegen
  (use srfi-1)
  (export ll-units->llvm-module))

(select-module isl.compiler.codegen)

(define *cg-string-pool* '()) ; ((text . @name) ...)
(define *cg-string-counter* 0)

(define (cg-reset!)
  (set! *cg-string-pool* '())
  (set! *cg-string-counter* 0))

(define (mangle-char ch)
  (if (or (char-alphabetic? ch) (char-numeric? ch) (char=? ch #\_))
      (string ch)
      "_"))

(define (mangle-symbol sym)
  (let* ((s (if (symbol? sym) (symbol->string sym) (x->string sym)))
         (out (apply string-append (map mangle-char (string->list s)))))
    (if (string=? out "") "anon" out)))

(define (llvm-val sym)
  (string-append "%" (mangle-symbol sym)))

(define (escape-llvm-string s)
  (let loop ((xs (string->list s)) (acc '()))
    (if (null? xs)
        (apply string-append (reverse acc))
        (let* ((ch (car xs))
               (code (char->integer ch)))
          (if (and (>= code 32) (<= code 126)
                   (not (char=? ch #\\))
                   (not (char=? ch #\")))
              (loop (cdr xs) (cons (string ch) acc))
              (let ((hex (string-upcase (number->string code 16))))
                (loop (cdr xs)
                      (cons (string-append "\\"
                                           (if (= (string-length hex) 1) "0" "")
                                           hex)
                            acc))))))))

(define (intern-string! s)
  (let ((p (find (lambda (e) (string=? (car e) s)) *cg-string-pool*)))
    (if p
        (cdr p)
        (let ((name (string-append "@.str." (number->string *cg-string-counter*))))
          (set! *cg-string-counter* (+ *cg-string-counter* 1))
          (set! *cg-string-pool* (append *cg-string-pool* (list (cons s name))))
          name))))

(define (str-ptr s)
  (let ((g (intern-string! s))
        (len (+ (string-length s) 1)))
    (string-append "getelementptr inbounds (["
                   (number->string len)
                   " x i8], ptr "
                   g
                   ", i64 0, i64 0)")))

(define (emit-string-constant e)
  (let ((text (car e))
        (name (cdr e)))
    (string-append name
                   " = private unnamed_addr constant ["
                   (number->string (+ (string-length text) 1))
                   " x i8] c\""
                   (escape-llvm-string text)
                   "\\00\", align 1")))

(define (indent n s)
  (string-append (make-string n #\space) s))

(define (join-lines xs)
  (let loop ((ys xs) (acc ""))
    (if (null? ys)
        acc
        (loop (cdr ys)
              (if (string=? acc "")
                  (car ys)
                  (string-append acc "\n" (car ys)))))))

(define (join-comma xs)
  (let loop ((ys xs) (acc ""))
    (if (null? ys)
        acc
        (loop (cdr ys)
              (if (string=? acc "")
                  (car ys)
                  (string-append acc ", " (car ys)))))))

(define (param-index params sym)
  (let loop ((xs params) (i 0))
    (cond
     ((null? xs) #f)
     ((eq? (car xs) sym) i)
     (else (loop (cdr xs) (+ i 1))))))

(define (emit-rhs dst rhs params cg-ref)
  (let ((op (car rhs)))
    (cond
      ((eq? op 'const)
       (let ((v (cadr rhs)))
         (cond
           ((integer? v)
            (list (indent 2 (string-append dst " = call ptr @isl_rt_make_int(i64 " (number->string v) ")"))))
           ((eq? v #t)
            (list (indent 2 (string-append dst " = call ptr @isl_rt_true()"))))
           ((eq? v #f)
            (list (indent 2 (string-append dst " = call ptr @isl_rt_false()"))))
           ((null? v)
            (list (indent 2 (string-append dst " = call ptr @isl_rt_nil()"))))
           ((symbol? v)
            (list (indent 2 (string-append dst " = call ptr @isl_rt_make_symbol(ptr " (str-ptr (symbol->string v)) ")"))))
           ((string? v)
            (list (indent 2 (string-append dst " = call ptr @isl_rt_make_string(ptr " (str-ptr v) ")"))))
           (else
            (list (indent 2 (string-append dst " = call ptr @isl_rt_unsupported(ptr " (str-ptr "unsupported const") ")")))))))
      ((eq? op 'var)
       (let ((idx (param-index params (cadr rhs))))
         (if idx
             (list (indent 2 (string-append dst " = call ptr @isl_rt_lookup_param(ptr %env, i32 " (number->string idx) ")")))
             (list (indent 2 (string-append dst " = call ptr @isl_rt_lookup(ptr %env, ptr " (str-ptr (symbol->string (cadr rhs))) ")"))))))
      ((eq? op 'call)
       (let* ((f (llvm-val (cadr rhs)))
              (args (caddr rhs))
              (argc (length args)))
         ;; M3 core mapping: `call` node is lowered to runtime call site.
         ;; Argument vector materialization is deferred to next iteration.
         (list
           (indent 2
                   (string-append dst
                                  " = call ptr @isl_rt_call(ptr %env, ptr "
                                  f
                                  ", i32 "
                                  (number->string argc)
                                  ", ptr null)")))))
      ((eq? op 'phi)
       (let* ((pairs (cadr rhs))
              (items (map (lambda (pr)
                            (string-append "[ " (llvm-val (cadr pr)) ", %" (mangle-symbol (car pr)) " ]"))
                          pairs)))
         (list (indent 2 (string-append dst " = phi ptr " (join-comma items))))))
      (else
       (list (indent 2 (string-append dst " = call ptr @isl_rt_unsupported(ptr " (str-ptr "lowering pending") ")")))))))

(define (emit-terminator term cg-ref)
  (case (car term)
    ((ret)
     (list (indent 2 (string-append "ret ptr " (llvm-val (cadr term))))))
    ((jmp)
     (list (indent 2 (string-append "br label %" (mangle-symbol (cadr term))))))
    ((br)
     (let* ((n (car cg-ref))
            (c (string-append "%cg" (number->string n))))
       (set-car! cg-ref (+ n 1))
       (list
        (indent 2 (string-append c " = call i1 @isl_rt_truthy(ptr " (llvm-val (cadr term)) ")"))
        (indent 2 (string-append "br i1 " c ", label %" (mangle-symbol (caddr term)) ", label %" (mangle-symbol (cadddr term)))))))
    (else
     (list (indent 2 "ret ptr null")))))

(define (emit-function llvm-name params cfg)
  (let ((cg-ref (list 0))
        (blocks (caddr cfg)))
    (append
     (list (string-append "define ptr @" llvm-name "(ptr %env) {"))
     (apply append
            (map
             (lambda (b)
               (let ((label (cadr b))
                     (instrs (caddr b))
                     (term (cadddr b)))
                 (append
                  (list (string-append (mangle-symbol label) ":"))
                  (apply append
                         (map (lambda (ins)
                                (emit-rhs (llvm-val (cadr ins)) (caddr ins) params cg-ref))
                              instrs))
                  (emit-terminator term cg-ref))))
             blocks))
     (list "}"))))

(define (emit-top ll idx)
  (case (car ll)
    ((ll-define-fun)
     (emit-function (string-append "isl_fun_" (mangle-symbol (cadr ll)))
                    (caddr ll)
                    (cadddr ll)))
    ((ll-define-global)
     (emit-function (string-append "isl_global_init_" (mangle-symbol (cadr ll)))
                    '()
                    (caddr ll)))
    ((ll-expr)
     (emit-function (string-append "isl_expr_" (number->string idx))
                    '()
                    (cadr ll)))
    ((ll-define-macro)
     (list (string-append ";; skipped macro " (symbol->string (cadr ll)))) )
    (else
     (list ";; skipped invalid top"))))

(define (ll-units->llvm-module units)
  (cg-reset!)
  (let* ((fun-lines
          (apply append
                 (let loop ((xs units) (i 0) (acc '()))
                   (if (null? xs)
                       (reverse acc)
                       (let* ((u (car xs))
                              (llp (assoc 'll (cdr u)))
                              (chunk (if llp (emit-top (cadr llp) i) '())))
                         (loop (cdr xs) (+ i 1) (cons chunk acc)))))))
         (decls
          (list
           "declare ptr @isl_rt_make_int(i64)"
           "declare ptr @isl_rt_make_symbol(ptr)"
           "declare ptr @isl_rt_make_string(ptr)"
           "declare ptr @isl_rt_nil()"
           "declare ptr @isl_rt_true()"
           "declare ptr @isl_rt_false()"
           "declare ptr @isl_rt_lookup(ptr, ptr)"
           "declare ptr @isl_rt_lookup_param(ptr, i32)"
           "declare ptr @isl_rt_call(ptr, ptr, i32, ptr)"
           "declare i1 @isl_rt_truthy(ptr)"
           "declare ptr @isl_rt_unsupported(ptr)"))
         (consts (map emit-string-constant *cg-string-pool*)))
    (join-lines
     (append
      (list ";; Auto-generated by isl.compiler.codegen")
      decls
      (if (null? consts) '() (append (list "") consts))
      (if (null? fun-lines) '() (append (list "") fun-lines))))))
