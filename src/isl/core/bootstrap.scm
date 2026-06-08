;;; core/bootstrap.scm
;;; isl.core: 初期環境の構築（make-initial-env と補助）
;;; このファイルは src/isl/core.scm から (include ...) され、
;;; モジュール isl.core の本体に展開される（単独ロード不可）。

(define *extended-primitive-symbols*
  '(debug break getenv setenv system system-timeout
    capture-output-string
    sqlite-open sqlite-db-p sqlite-close sqlite-exec sqlite-query sqlite-query-one
    postgres-open postgres-db-p postgres-close postgres-exec postgres-query postgres-query-one
    mysql-open mysql-db-p mysql-close mysql-exec mysql-query mysql-query-one
    tcp-connect tcp-listen tcp-listener-p tcp-accept tcp-listener-close
    tcp-connection-p tcp-send tcp-send-line tcp-receive-line tcp-receive-char
    tcp-send-byte tcp-send-file tcp-receive-byte tcp-flush tcp-close
    tls-listen tls-listener-p tls-accept tls-listener-close
    tls-connection-p tls-send tls-send-line tls-receive-line tls-receive-char
    tls-send-byte tls-send-file tls-receive-byte tls-flush tls-close
    thread-spawn thread-p thread-join thread-join-timeout mutex-open mutex-p mutex-lock mutex-unlock
    udp-open udp-socket-p udp-bind udp-connect udp-send udp-receive udp-sendto udp-receive-from udp-close
    http-get http-head http-post
    ffi-call load-foreign-library current-foreign-library))

(define (disable-extended-primitives! env)
  (for-each
   (lambda (name)
     (frame-unbind! env (resolve-binding-symbol name)))
   *extended-primitive-symbols*))

(define (make-initial-env . maybe-profile)
  (let ((profile (normalize-profile (if (null? maybe-profile) 'extended (car maybe-profile)))))
    (set! *isl-profile* profile)
  (let ((env (make-frame #f)))
    (set! *package-registry* '())
    (set! *class-table* '())
    (set! *accessor-slot-table* '())
    (set! *provided-features* '())
    (set! *ffi-current-library* #f)
    (let ((islisp (ensure-package! "ISLISP"))
          (common-lisp (ensure-package! "COMMON-LISP"))
          (user (ensure-package! "ISLISP-USER")))
      (package-use! common-lisp islisp)
      (package-use! user islisp)
      (set! *current-package* islisp)
      (frame-define! env (resolve-binding-symbol 'nil) '())
      (package-export! *current-package* 'nil)
      (frame-define! env (resolve-binding-symbol 't) #t)
      (package-export! *current-package* 't)
      (install-primitives! env)
      ;; ---- Phase 4-E: standard stream variables ----
      (let ((si (resolve-binding-symbol '*standard-input*))
            (so (resolve-binding-symbol '*standard-output*))
            (se (resolve-binding-symbol '*error-output*)))
        (frame-define! env si (current-input-port))
        (frame-define! env so (current-output-port))
        (frame-define! env se (current-error-port))
        (package-export! *current-package* '*standard-input*)
        (package-export! *current-package* '*standard-output*)
        (package-export! *current-package* '*error-output*)
        ;; §18.1: standard-input/standard-output/error-output as zero-arg functions
        (frame-define! env (resolve-binding-symbol 'standard-input)
          (make-primitive 'standard-input (lambda () (frame-ref env si))))
        (frame-define! env (resolve-binding-symbol 'standard-output)
          (make-primitive 'standard-output (lambda () (frame-ref env so))))
        (frame-define! env (resolve-binding-symbol 'error-output)
          (make-primitive 'error-output (lambda () (frame-ref env se))))
        (package-export! *current-package* 'standard-input)
        (package-export! *current-package* 'standard-output)
        (package-export! *current-package* 'error-output))
      ;; ---- Phase 7-D: root OOP classes + built-in class hierarchy ----
      (install-builtin-classes! env islisp)
      ;; ---- Phase 7-E: initialize-object generic function ----
      (install-condition-system! env islisp)
      ;; ---- ISLISP §16.3/§16.4: when/unless macros ----
      (install-standard-macros! env)
      (when (strict-profile?)
        (disable-extended-primitives! env))
      ;; Lightweight CFFI-compatible facade (extended profile only).
      (install-ffi-facade! env islisp)
      (set! *current-package* user))
    env)))

;; make-initial-env のサブステップ（初期化処理を単一責務へ分離）
(define (install-builtin-classes! env islisp)
      ;; Helper: intern name in ISLISP package, create class, register it.
      (let* ((mk (lambda (raw-name supers)
                   (let* ((name (resolve-binding-symbol raw-name))
                          (c    (make-class name supers '())))
                     (class-table-set! name c)
                     (frame-define! (global-frame env) name c)
                     c)))
             (obj  (mk '<object>         '()))
             (num  (mk '<number>         (list obj)))
             (int  (mk '<integer>        (list num)))
             (flt  (mk '<float>          (list num)))
             (chr  (mk '<character>      (list obj)))
             (sym  (mk '<symbol>         (list obj)))
             (fun  (mk '<function>       (list obj)))
             (stm  (mk '<stream>         (list obj)))
             (ba   (mk '<basic-array>    (list obj)))
             (bv   (mk '<basic-vector>   (list ba)))
             (gv   (mk '<general-vector> (list bv)))
             (str  (mk '<string>         (list bv)))
             (baa  (mk '<basic-array*>   (list ba)))
             (gaa  (mk '<general-array*> (list baa)))
             (gf   (mk '<generic-function> (list fun)))
             (sgf  (mk '<standard-generic-function> (list gf)))
             (bl   (mk '<basic-list>     (list obj)))
             (lst  (mk '<list>           (list bl)))
             (co   (mk '<cons>           (list lst)))
             (nll  (mk '<null>           (list lst)))
             (so   (mk '<standard-object>(list obj)))
             (bic  (mk '<built-in-class> (list obj)))
             (sc   (mk '<standard-class> (list obj))))
        ;; Export all built-in class names from ISLISP package
        (for-each (lambda (name) (package-export! islisp name))
                  '(<object> <number> <integer> <float> <character> <symbol>
                    <function> <generic-function> <standard-generic-function>
                    <stream> <basic-array> <basic-vector> <general-vector>
                    <string> <basic-array*> <general-array*> <basic-list> <list> <cons> <null>
                    <standard-object> <built-in-class> <standard-class>)))
  )

;; make-initial-env のサブステップ（初期化処理を単一責務へ分離）
(define (install-condition-system! env islisp)
      (for-each (lambda (form) (eval-islisp form env))
        '((defgeneric initialize-object (obj initargs))
          (defmethod initialize-object ((obj <standard-object>) initargs) obj)))
      (package-export! islisp 'initialize-object)
      ;; ---- Phase 5-A: ISLISP condition class hierarchy ----
      (for-each (lambda (form) (eval-islisp form env))
        '((defclass <condition>                   ()                       ())
          (defclass <serious-condition>           (<condition>)            ())
          (defclass <error>                       (<serious-condition>)    ())
          (defclass <arithmetic-error>            (<error>)                ())
          (defclass <division-by-zero>            (<arithmetic-error>)     ())
          (defclass <floating-point-overflow>     (<arithmetic-error>)     ())
          (defclass <floating-point-underflow>    (<arithmetic-error>)     ())
          (defclass <control-error>               (<error>)                ())
          (defclass <parse-error>                 (<error>)                ())
          (defclass <program-error>               (<error>)                ())
          (defclass <domain-error>                (<program-error>)        ())
          (defclass <undefined-entity>            (<program-error>)        ())
          (defclass <unbound-variable>            (<undefined-entity>)     ())
          (defclass <undefined-function>          (<undefined-entity>)     ())
          (defclass <arity-error>                 (<program-error>)        ())
          (defclass <index-out-of-range>          (<program-error>)        ())
          (defclass <type-error>                  (<program-error>)        ())
          (defclass <simple-error>                (<error>)                ())
          (defclass <stream-error>                (<error>)                ())
          (defclass <end-of-stream>               (<stream-error>)         ())
          (defclass <storage-exhausted>           (<serious-condition>)    ())))
      ;; Export condition classes from ISLISP package so ISLISP-USER inherits them.
      (for-each (lambda (name) (package-export! islisp name))
        '(<condition> <serious-condition> <error>
          <arithmetic-error> <division-by-zero>
          <floating-point-overflow> <floating-point-underflow>
          <control-error> <parse-error> <program-error>
          <domain-error> <undefined-entity>
          <unbound-variable> <undefined-function>
          <arity-error> <index-out-of-range> <type-error>
          <simple-error> <stream-error> <end-of-stream> <storage-exhausted>))
  )

;; make-initial-env のサブステップ（初期化処理を単一責務へ分離）
(define (install-standard-macros! env)
      (eval-islisp
       '(defmacro when (condition &rest body)
          (list 'if condition (cons 'progn body) nil))
       env)
      (package-export! *current-package* 'when)
      (eval-islisp
       '(defmacro unless (condition &rest body)
          (list 'if (list 'not condition) (cons 'progn body) nil))
       env)
      (package-export! *current-package* 'unless)
      ;; ---- Phase 5-G: ignore-errors macro ----
      (eval-islisp
       '(defmacro ignore-errors (&rest forms)
          (list 'handler-case
                (cons 'progn forms)
                (list '<serious-condition> '() nil)))
       env)
      (package-export! *current-package* 'ignore-errors)
      ;; ---- Phase 3-D: with-open-input-file / with-open-output-file macros ----
      (eval-islisp
       '(defmacro with-open-input-file (spec &rest body)
          (let ((var      (car spec))
                (filename (car (cdr spec))))
            (cons 'with-open-file
                  (cons (list var filename :direction :input)
                        body))))
       env)
      (package-export! *current-package* 'with-open-input-file)
      (eval-islisp
       '(defmacro with-open-output-file (spec &rest body)
          (let ((var      (car spec))
                (filename (car (cdr spec))))
            (cons 'with-open-file
                  (cons (list var filename :direction :output)
                        body))))
       env)
      (package-export! *current-package* 'with-open-output-file)
      ;; ---- ISLISP §18.2: with-standard-input/output/error-output macros ----
      (eval-islisp
       '(defmacro with-standard-input (stream &rest body)
          (list '%with-si-helper stream (list 'lambda '() (cons 'progn body))))
       env)
      (package-export! *current-package* 'with-standard-input)
      (eval-islisp
       '(defmacro with-standard-output (stream &rest body)
          (list '%with-so-helper stream (list 'lambda '() (cons 'progn body))))
       env)
      (package-export! *current-package* 'with-standard-output)
      (eval-islisp
       '(defmacro with-error-output (stream &rest body)
          (list '%with-eo-helper stream (list 'lambda '() (cons 'progn body))))
       env)
      (package-export! *current-package* 'with-error-output)
  )

;; make-initial-env のサブステップ（初期化処理を単一責務へ分離）
(define (install-ffi-facade! env islisp)
      (unless (strict-profile?)
        (let ((saved *current-package*)
              (ffi (ensure-package! "FFI"))
              (cffi (ensure-package! "CFFI")))
          (package-use! ffi islisp)
          (let ((isl-ffi-call (resolve-binding-symbol 'ffi-call))
                (isl-load-lib (resolve-binding-symbol 'load-foreign-library))
                (isl-cur-lib (resolve-binding-symbol 'current-foreign-library))
                (ffi-ffi-call (package-intern! ffi "ffi-call"))
                (ffi-load-lib (package-intern! ffi "load-foreign-library"))
                (ffi-cur-lib (package-intern! ffi "current-foreign-library")))
            (frame-define! env ffi-ffi-call (frame-ref env isl-ffi-call))
            (frame-define! env ffi-load-lib (frame-ref env isl-load-lib))
            (frame-define! env ffi-cur-lib (frame-ref env isl-cur-lib))
            (package-export! ffi 'ffi-call)
            (package-export! ffi 'load-foreign-library)
            (package-export! ffi 'current-foreign-library))
          (package-use! cffi ffi)
          (set! *current-package* ffi)
          (eval-islisp
           '(defmacro define-foreign-function (foreign-name spec &rest options)
              (let* ((lisp-name (first spec))
                     (params (cdr spec))
                     (return-type :void)
                     (arg-types '()))
                (let ((xs options))
                  (while xs
                    (let ((k (first xs))
                          (v (second xs)))
                      (cond
                       ((eq k :returning) (setq return-type v))
                       ((eq k :arguments) (setq arg-types v))
                       (t (error "define-foreign-function: unsupported option" k)))
                      (setq xs (cdr (cdr xs))))))
                (if (not (= (length params) (length arg-types)))
                    (error "define-foreign-function: param/type length mismatch" spec arg-types)
                    `(progn
                       (defun ,lisp-name ,params
                         (ffi:ffi-call (ffi:current-foreign-library)
                                       ,foreign-name
                                       ',return-type
                                       ',arg-types
                                       (list ,@params)))
                       ',lisp-name))))
           env)
          (package-export! ffi 'define-foreign-function)
          (package-export! cffi 'define-foreign-function)
          (set! *current-package* saved)))
  )


