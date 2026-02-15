;; Minimal ISLISP core evaluator on Gauche 0.9.15.
;; The implementation keeps dependencies close to R7RS-level primitives.
(define-module isl.core
  (use srfi-1)
  (use gauche.process)
  (use gauche.net)
  (use rfc.http)
  (export make-initial-env eval-islisp apply-islisp repl read-all))

(select-module isl.core)

(define gauche-http-get http-get)
(define gauche-http-post http-post)
(define gauche-http-head http-head)
(define gauche-http-string-receiver http-string-receiver)

(define (truthy? v)
  (and (not (eq? v #f))
       (not (null? v))))

(define (make-frame parent)
  (vector 'frame '() parent))

(define (frame-bindings frame)
  (vector-ref frame 1))

(define (frame-parent frame)
  (vector-ref frame 2))

(define (set-frame-bindings! frame bindings)
  (vector-set! frame 1 bindings))

(define (frame-find-pair frame sym)
  (let loop ((f frame))
    (if (not f)
        #f
        (let ((pair (assoc sym (frame-bindings f))))
          (if pair
              pair
              (loop (frame-parent f)))))))

(define (frame-define! frame sym val)
  (let ((bindings (frame-bindings frame)))
    (let loop ((xs bindings) (acc '()))
      (cond
       ((null? xs)
        (set-frame-bindings! frame (append (reverse acc) (list (cons sym val)))))
       ((eq? (caar xs) sym)
        (set-frame-bindings! frame
                             (append (reverse acc) (cons (cons sym val) (cdr xs)))))
       (else
        (loop (cdr xs) (cons (car xs) acc)))))))

(define (frame-unbind! frame sym)
  (let ((bindings (frame-bindings frame)))
    (let loop ((xs bindings) (acc '()))
      (cond
       ((null? xs)
        (set-frame-bindings! frame (reverse acc)))
       ((eq? (caar xs) sym)
        (loop (cdr xs) acc))
       (else
        (loop (cdr xs) (cons (car xs) acc)))))))

(define (frame-set! frame sym val)
  (let ((pair (frame-find-pair frame sym)))
    (if pair
        (set-cdr! pair val)
        (error "Unbound variable" sym))))

(define (global-frame env)
  (let loop ((f env))
    (if (frame-parent f)
        (loop (frame-parent f))
        f)))

(define (frame-ref frame sym)
  (let ((pair (frame-find-pair frame sym)))
    (if pair
        (cdr pair)
        (error "Unbound variable" sym))))

(define (frame-bound? frame sym)
  (if (frame-find-pair frame sym) #t #f))

(define (assoc-string key alist)
  (let loop ((xs alist))
    (if (null? xs)
        #f
        (if (string=? key (caar xs))
            (car xs)
            (loop (cdr xs))))))

(define *package-registry* '())
(define *current-package* #f)
(define *provided-features* '())
;; Default shared library used by ffi compatibility layer.
(define *ffi-current-library* #f)
(define *isl-profile* 'extended)

(define (strict-profile?)
  (eq? *isl-profile* 'strict))

(define (normalize-profile profile)
  (cond
   ((or (not profile) (eq? profile 'extended) (eq? profile ':extended)) 'extended)
   ((or (eq? profile 'strict) (eq? profile ':strict)) 'strict)
   ((string? profile)
    (let ((up (list->string (map char-upcase (string->list profile)))))
      (cond
       ((string=? up "EXTENDED") 'extended)
       ((string=? up "STRICT") 'strict)
       (else
        (error "Unknown profile. Expected strict or extended." profile)))))
   (else
    (error "Unknown profile. Expected strict or extended." profile))))

(define (make-package name)
  (vector 'package name '() '() '()))

(define (package-name pkg) (vector-ref pkg 1))
(define (package-internals pkg) (vector-ref pkg 2))
(define (package-externals pkg) (vector-ref pkg 3))
(define (package-uses pkg) (vector-ref pkg 4))
(define (set-package-internals! pkg v) (vector-set! pkg 2 v))
(define (set-package-externals! pkg v) (vector-set! pkg 3 v))
(define (set-package-uses! pkg v) (vector-set! pkg 4 v))

(define (normalize-package-name x)
  (let ((s
         (cond
          ((symbol? x) (symbol->string x))
          ((string? x) x)
          (else
           (error "package name must be symbol or string" x)))))
    (let* ((up (list->string (map char-upcase (string->list s))))
           (n (string-length up)))
      (if (and (> n 0) (char=? (string-ref up 0) #\:))
          (substring up 1 n)
          up))))

(define (normalize-feature-name x)
  (let ((raw
         (cond
          ((symbol? x) (symbol-base-name x))
          ((string? x) x)
          (else
           (error "feature name must be symbol or string" x)))))
    (list->string (map char-upcase (string->list raw)))))

(define (feature-provided? x)
  (let ((name (normalize-feature-name x)))
    (if (member name *provided-features* string=?)
        #t
        #f)))

(define (register-feature! x)
  (let ((name (normalize-feature-name x)))
    (unless (member name *provided-features* string=?)
      (set! *provided-features* (cons name *provided-features*)))
    #t))

(define *ffi-bridge-source* "src/isl/ffi_bridge.c")
(define *ffi-bridge-binary* "src/isl/ffi_bridge")
(define *ffi-result-prefix* "__ISLISP_FFI_RESULT__:")
(define *ffi-null-token* "__ISLISP_FFI_NULL__")

(define (make-sqlite-connection path)
  (vector 'sqlite-connection path))

(define (sqlite-connection? obj)
  (and (vector? obj)
       (= (vector-length obj) 2)
       (eq? (vector-ref obj 0) 'sqlite-connection)))

(define (sqlite-connection-path conn)
  (vector-ref conn 1))

(define (make-postgres-connection conninfo)
  (vector 'postgres-connection conninfo))

(define (postgres-connection? obj)
  (and (vector? obj)
       (= (vector-length obj) 2)
       (eq? (vector-ref obj 0) 'postgres-connection)))

(define (postgres-connection-info conn)
  (vector-ref conn 1))

(define (make-mysql-connection host port user password database)
  (vector 'mysql-connection host port user password database))

(define (mysql-connection? obj)
  (and (vector? obj)
       (= (vector-length obj) 6)
       (eq? (vector-ref obj 0) 'mysql-connection)))

(define (mysql-connection-host conn)
  (vector-ref conn 1))

(define (mysql-connection-port conn)
  (vector-ref conn 2))

(define (mysql-connection-user conn)
  (vector-ref conn 3))

(define (mysql-connection-password conn)
  (vector-ref conn 4))

(define (mysql-connection-database conn)
  (vector-ref conn 5))

(define (make-tcp-connection socket in out)
  (vector 'tcp-connection socket in out))

(define (tcp-connection? obj)
  (and (vector? obj)
       (= (vector-length obj) 4)
       (eq? (vector-ref obj 0) 'tcp-connection)))

(define (tcp-connection-socket conn)
  (vector-ref conn 1))

(define (tcp-connection-input conn)
  (vector-ref conn 2))

(define (tcp-connection-output conn)
  (vector-ref conn 3))

(define (make-tcp-listener socket)
  (vector 'tcp-listener socket))

(define (tcp-listener? obj)
  (and (vector? obj)
       (= (vector-length obj) 2)
       (eq? (vector-ref obj 0) 'tcp-listener)))

(define (tcp-listener-socket listener)
  (vector-ref listener 1))

(define (make-udp-socket socket)
  (vector 'udp-socket socket))

(define (udp-socket? obj)
  (and (vector? obj)
       (= (vector-length obj) 2)
       (eq? (vector-ref obj 0) 'udp-socket)))

(define (udp-socket-raw socket)
  (vector-ref socket 1))

(define (decode-process-exit-status raw)
  (if (and (integer? raw) (>= raw 0))
      (quotient raw 256)
      raw))

(define (run-command/capture argv)
  (let* ((p (run-process argv :output :pipe :error :pipe))
         (out (port->string (process-output p)))
         (err (port->string (process-error p))))
    (process-wait p)
    (list (decode-process-exit-status (process-exit-status p)) out err)))

(define (command-available? cmd)
  (let ((status (decode-process-exit-status
                 (sys-system (string-append "sh -c 'command -v "
                                            cmd
                                            " >/dev/null 2>&1'")))))
    (or (eq? status 0) (eq? status #t))))

(define (sqlite3-available?)
  (command-available? "sqlite3"))

(define (ensure-sqlite3-available!)
  (unless (sqlite3-available?)
    (error "sqlite3 command is required for sqlite integration")))

(define (sqlite-run path sql)
  (ensure-sqlite3-available!)
  (unless (string? path)
    (error "sqlite path must be a string" path))
  (unless (string? sql)
    (error "sqlite sql must be a string" sql))
  (let* ((argv (list "sqlite3" "-batch" "-noheader" "-separator" (string #\x1f)
                     path sql))
         (result (run-command/capture argv))
         (status (car result))
         (out (cadr result))
         (err (caddr result)))
    (unless (or (eq? status 0) (eq? status #t))
      (error "sqlite command failed"
             (if (string=? err "") out err)))
    out))

(define (psql-available?)
  (command-available? "psql"))

(define (ensure-psql-available!)
  (unless (psql-available?)
    (error "psql command is required for postgres integration")))

(define (postgres-run conninfo sql)
  (ensure-psql-available!)
  (unless (string? conninfo)
    (error "postgres connection info must be a string" conninfo))
  (unless (string? sql)
    (error "postgres sql must be a string" sql))
  (let* ((argv (list "psql" "-X" "-A" "-t" "-F" (string #\x1f)
                     "-d" conninfo "-c" sql))
         (result (run-command/capture argv))
         (status (car result))
         (out (cadr result))
         (err (caddr result)))
    (unless (or (eq? status 0) (eq? status #t))
      (error "postgres command failed"
             (if (string=? err "") out err)))
    out))

(define (mysql-cli-command)
  (cond
   ((command-available? "mysql") "mysql")
   ((command-available? "mariadb") "mariadb")
   (else #f)))

(define (ensure-mysql-cli-available!)
  (unless (mysql-cli-command)
    (error "mysql/mariadb command is required for mysql integration")))

(define (mysql-run host port user password database sql)
  (let ((cmd (mysql-cli-command)))
    (ensure-mysql-cli-available!)
    (unless (string? host)
      (error "mysql host must be a string" host))
    (unless (and (integer? port) (>= port 1) (<= port 65535))
      (error "mysql port must be integer in 1..65535" port))
    (unless (string? user)
      (error "mysql user must be a string" user))
    (unless (string? password)
      (error "mysql password must be a string" password))
    (unless (string? database)
      (error "mysql database must be a string" database))
    (unless (string? sql)
      (error "mysql sql must be a string" sql))
    (let* ((argv-base
            (list cmd "--batch" "--raw" "--silent" "--skip-column-names"
                  "--protocol=tcp"
                  "-h" host
                  "-P" (number->string port)
                  "-u" user))
           (argv-auth (if (string=? password "")
                          argv-base
                          (append argv-base (list (string-append "-p" password)))))
           (argv (append argv-auth (list database "-e" sql)))
           (result (run-command/capture argv))
           (status (car result))
           (out (cadr result))
           (err (caddr result)))
      (unless (or (eq? status 0) (eq? status #t))
        (error "mysql command failed"
               (if (string=? err "") out err)))
      out)))

(define (string-split-char s ch)
  (let ((n (string-length s)))
    (let loop ((i 0) (start 0) (acc '()))
      (if (= i n)
          (reverse (cons (substring s start n) acc))
          (if (char=? (string-ref s i) ch)
              (loop (+ i 1) (+ i 1) (cons (substring s start i) acc))
              (loop (+ i 1) start acc))))))

(define (sql-output->rows out separator)
  (call-with-input-string
   out
   (lambda (p)
     (let loop ((acc '()))
       (let ((line (read-line p)))
         (if (eof-object? line)
             (reverse acc)
             (loop (cons (string-split-char line separator) acc))))))))

(define (ensure-ffi-bridge!)
  (unless (file-exists? *ffi-bridge-source*)
    (error "ffi bridge source is missing" *ffi-bridge-source*))
  (let ((needs-build
         (or (not (file-exists? *ffi-bridge-binary*))
             (> (file-mtime *ffi-bridge-source*)
                (file-mtime *ffi-bridge-binary*)))))
    (when needs-build
      ;; On Darwin, -ldl is unnecessary and may fail; try with -ldl then fallback.
      (let* ((with-dl (run-command/capture
                       (list "cc" "-O2" "-std=c99"
                             "-o" *ffi-bridge-binary* *ffi-bridge-source*
                             "-ldl")))
             (st1 (car with-dl)))
        (unless (or (eq? st1 0) (eq? st1 #t))
          (let* ((without-dl (run-command/capture
                              (list "cc" "-O2" "-std=c99"
                                    "-o" *ffi-bridge-binary* *ffi-bridge-source*)))
                 (st2 (car without-dl)))
            (unless (or (eq? st2 0) (eq? st2 #t))
              (error "failed to build ffi bridge"
                     (string-append (caddr with-dl) (caddr without-dl))))))))))

(define (normalize-ffi-type x where)
  (let* ((raw
          (cond
           ((symbol? x) (symbol-base-name x))
           ((string? x) x)
           (else
            (error "ffi type must be symbol or string" where x))))
         (low (list->string (map char-downcase (string->list raw)))))
    (cond
     ((or (string=? low "int")
          (string=? low "long")
          (string=? low "short")
          (string=? low "char")
          (string=? low "signed")
          (string=? low "unsigned")
          (string=? low "size_t")
          (string=? low "ssize_t")
          (string=? low "uintptr_t")
          (string=? low "intptr_t")
          (string=? low "ptr")
          (string=? low "pointer"))
      "int")
     ((or (string=? low "double")
          (string=? low "float"))
      "double")
     ((or (string=? low "string")
          (string=? low "c-string")
          (string=? low "cstring")
          (string=? low "char*")
          (string=? low "char-ptr"))
      "string")
     ((or (string=? low "bool")
          (string=? low "boolean"))
      "bool")
     ((string=? low "void") "void")
     (else
      (error "unsupported ffi type" where x)))))

(define (ffi-arg->string type value)
  (cond
   ((string=? type "int")
    (unless (integer? value)
      (error "ffi int argument must be integer" value))
    (number->string value))
   ((string=? type "double")
    (unless (number? value)
      (error "ffi double argument must be number" value))
    (number->string (exact->inexact value)))
   ((string=? type "string")
    (unless (string? value)
      (error "ffi string argument must be string" value))
    value)
   ((string=? type "bool")
    (cond
     ((or (eq? value #t) (eq? value t)) "1")
     ((or (eq? value #f) (null? value)) "0")
     ((integer? value)
      (if (= value 0) "0" "1"))
     (else
      (error "ffi bool argument must be boolean/nil or integer" value))))
   (else
    (error "unsupported ffi argument type" type))))

(define (parse-ffi-result ret-type out)
  (cond
   ((string=? ret-type "void") '())
   ((or (string=? ret-type "int")
        (string=? ret-type "double"))
    (let ((n (string->number out)))
      (if n
          n
          (error "ffi result parse error" out))))
   ((string=? ret-type "string")
    (if (string=? out *ffi-null-token*) '() out))
   ((string=? ret-type "bool")
    (let ((n (string->number out)))
      (if n
          (if (= n 0) #f #t)
          (let ((low (list->string (map char-downcase (string->list out)))))
            (cond
             ((string=? low "true") #t)
             ((string=? low "false") #f)
             (else
              (error "ffi result parse error" out)))))))
   (else
    (error "unsupported ffi return type" ret-type))))

(define (extract-ffi-result out)
  (let ((idx (string-find-substr out *ffi-result-prefix*)))
    (unless idx
      (error "ffi bridge returned malformed output" out))
    (let* ((head (substring out 0 idx))
           (start (+ idx (string-length *ffi-result-prefix*)))
           (tail (substring out start (string-length out))))
      (list head tail))))

(define (eval-load-file filename env)
  (unless (string? filename)
    (error "load filename must be a string" filename))
  (call-with-input-file
   filename
   (lambda (p)
     (eval-sequence (read-all p) env))))

(define (find-package name)
  (let ((entry (assoc-string (normalize-package-name name) *package-registry*)))
    (and entry (cdr entry))))

(define (register-package-name! name pkg)
  (let* ((n (normalize-package-name name))
         (entry (assoc-string n *package-registry*)))
    (cond
     ((not entry)
      (set! *package-registry* (cons (cons n pkg) *package-registry*))
      pkg)
     ((eq? (cdr entry) pkg)
      pkg)
     (else
      (error "Package name already in use" name)))))

(define (ensure-package! name)
  (let* ((n (normalize-package-name name))
         (p (find-package n)))
    (if p
        p
        (let ((pkg (make-package n)))
          (register-package-name! n pkg)
          pkg))))

(define (package-internal-symbol pkg sym-name)
  (let ((entry (assoc-string sym-name (package-internals pkg))))
    (and entry (cdr entry))))

(define (package-external-symbol pkg sym-name)
  (let ((entry (assoc-string sym-name (package-externals pkg))))
    (and entry (cdr entry))))

(define (make-qualified-symbol pkg-name sym-name)
  (string->symbol (string-append pkg-name "::" sym-name)))

(define (package-intern! pkg sym-name)
  (let ((entry (assoc-string sym-name (package-internals pkg))))
    (if entry
        (cdr entry)
        (let ((qsym (make-qualified-symbol (package-name pkg) sym-name)))
          (set-package-internals! pkg (cons (cons sym-name qsym) (package-internals pkg)))
          qsym))))

(define (package-export! pkg sym-or-name)
  (let* ((sym-name (if (symbol? sym-or-name) (symbol-base-name sym-or-name) sym-or-name))
         (sym (package-intern! pkg sym-name)))
    (unless (assoc-string sym-name (package-externals pkg))
      (set-package-externals! pkg (cons (cons sym-name sym) (package-externals pkg))))
    sym))

(define (package-use! pkg used-pkg)
  (unless (memq used-pkg (package-uses pkg))
    (set-package-uses! pkg (cons used-pkg (package-uses pkg)))))

(define (package-visible-externals pkg sym-name seen)
  (if (memq pkg seen)
      '()
      (let* ((seen2 (cons pkg seen))
             (local (let ((e (package-external-symbol pkg sym-name)))
                      (if e (list e) '())))
             (from-uses
              (apply append
                     (map (lambda (u) (package-visible-externals u sym-name seen2))
                          (package-uses pkg)))))
        (delete-duplicates (append local from-uses) eq?))))

(define (package-import-symbol! pkg sym)
  (let ((name (symbol-base-name sym))
        (entry (assoc-string (symbol-base-name sym) (package-internals pkg))))
    (cond
     ((not entry)
      (set-package-internals! pkg (cons (cons name sym) (package-internals pkg)))
      sym)
     ((eq? (cdr entry) sym)
      sym)
     (else
      (error "import symbol conflicts with existing symbol" name)))))

(define (package-shadow-symbol! pkg sym-or-name)
  (let* ((name (if (symbol? sym-or-name) (symbol-base-name sym-or-name) sym-or-name))
         (sym (make-qualified-symbol (package-name pkg) name)))
    (set-package-internals!
     pkg
     (cons (cons name sym)
           (filter (lambda (e) (not (string=? (car e) name)))
                   (package-internals pkg))))
    sym))

(define (package-shadowing-import-symbol! pkg sym)
  (let ((name (symbol-base-name sym)))
    (set-package-internals!
     pkg
     (cons (cons name sym)
           (filter (lambda (e) (not (string=? (car e) name)))
                   (package-internals pkg))))
    sym))

(define (package-find-external-symbol-by-name pkg sym-or-name)
  (package-external-symbol pkg
                           (if (symbol? sym-or-name)
                               (symbol-base-name sym-or-name)
                               sym-or-name)))

(define (string-find-substr s pat)
  (let ((n (string-length s))
        (m (string-length pat)))
    (let loop ((i 0))
      (cond
       ((> (+ i m) n) #f)
       ((string=? (substring s i (+ i m)) pat) i)
       (else (loop (+ i 1)))))))

(define (symbol-split-package sym)
  (let* ((s (symbol->string sym))
         (idx2 (string-find-substr s "::")))
    (if idx2
        (list (substring s 0 idx2) (substring s (+ idx2 2) (string-length s)) 'internal)
        (let ((idx1 (string-find-substr s ":")))
          (if idx1
              (list (substring s 0 idx1) (substring s (+ idx1 1) (string-length s)) 'external)
              #f)))))

(define (resolve-symbol-in-package sym)
  (let ((split (symbol-split-package sym)))
    (if split
        (let* ((pkg (find-package (car split)))
               (name (cadr split))
               (kind (caddr split)))
          (unless pkg
            (error "Unknown package" (car split)))
          (if (eq? kind 'internal)
              (package-intern! pkg name)
              (let ((x (package-external-symbol pkg name)))
                (unless x
                  (error "Symbol is not exported" sym))
                x)))
        (let* ((name (symbol->string sym))
               (here *current-package*)
               (internal (and here (or (package-internal-symbol here name)
                                       (package-external-symbol here name)))))
          (if internal
              internal
              (let ((hits
                     (if here
                         (delete-duplicates
                          (apply append
                                 (map (lambda (pkg)
                                        (package-visible-externals pkg name '()))
                                      (package-uses here)))
                          eq?)
                         '())))
                (cond
                 ((null? hits) (if here (package-intern! here name) sym))
                 ((null? (cdr hits)) (car hits))
                 (else (error "Ambiguous symbol reference" sym)))))))))

(define (resolve-binding-symbol sym)
  (unless (symbol? sym)
    (error "binding name must be symbol" sym))
  (when (keyword-symbol? sym)
    (error "keyword cannot be used as binding name" sym))
  (resolve-symbol-in-package sym))

(define (closure? obj)
  (and (vector? obj) (= (vector-length obj) 5) (eq? (vector-ref obj 0) 'closure)))

(define (make-closure params body env name)
  (vector 'closure params body env name))

(define (closure-params c) (vector-ref c 1))
(define (closure-body c) (vector-ref c 2))
(define (closure-env c) (vector-ref c 3))
(define (closure-name c) (vector-ref c 4))

(define (primitive? obj)
  (and (vector? obj) (= (vector-length obj) 3) (eq? (vector-ref obj 0) 'primitive)))

(define (make-primitive name proc)
  (vector 'primitive name proc))

(define (primitive-name p) (vector-ref p 1))
(define (primitive-proc p) (vector-ref p 2))

(define (macro? obj)
  (and (vector? obj) (= (vector-length obj) 5) (eq? (vector-ref obj 0) 'macro)))

(define (make-macro params body env name)
  (vector 'macro params body env name))

(define (macro-params m) (vector-ref m 1))
(define (macro-body m) (vector-ref m 2))
(define (macro-env m) (vector-ref m 3))
(define (macro-name m) (vector-ref m 4))

(define (generic? obj)
  (and (vector? obj) (= (vector-length obj) 4) (eq? (vector-ref obj 0) 'generic)))

(define (make-generic name params methods)
  (vector 'generic name params methods))

(define (generic-name g) (vector-ref g 1))
(define (generic-params g) (vector-ref g 2))
(define (generic-methods g) (vector-ref g 3))
(define (set-generic-methods! g methods) (vector-set! g 3 methods))

(define (method? obj)
  (and (vector? obj) (= (vector-length obj) 4) (eq? (vector-ref obj 0) 'method)))

(define (make-method specializers params proc)
  (vector 'method specializers params proc))

(define (method-specializers m) (vector-ref m 1))
(define (method-params m) (vector-ref m 2))
(define (method-proc m) (vector-ref m 3))

(define (class? obj)
  (and (vector? obj) (= (vector-length obj) 4) (eq? (vector-ref obj 0) 'class)))

(define (make-class name supers own-slots)
  (vector 'class name supers own-slots))

(define (class-name c) (vector-ref c 1))
(define (class-supers c) (vector-ref c 2))
(define (class-own-slots c) (vector-ref c 3))

(define (slot-spec? obj)
  (and (vector? obj) (= (vector-length obj) 7) (eq? (vector-ref obj 0) 'slot-spec)))

(define (make-slot-spec name initform accessor initarg readers writers)
  (vector 'slot-spec name initform accessor initarg readers writers))

(define (slot-spec-name s) (vector-ref s 1))
(define (slot-spec-initform s) (vector-ref s 2))
(define (slot-spec-accessor s) (vector-ref s 3))
(define (slot-spec-initarg s) (vector-ref s 4))
(define (slot-spec-readers s) (vector-ref s 5))
(define (slot-spec-writers s) (vector-ref s 6))

(define (instance? obj)
  (and (vector? obj) (= (vector-length obj) 3) (eq? (vector-ref obj 0) 'instance)))

(define (make-instance-object class slot-alist)
  (vector 'instance class slot-alist))

(define (instance-class obj) (vector-ref obj 1))
(define (instance-slots obj) (vector-ref obj 2))

(define (set-instance-slots! obj slots)
  (vector-set! obj 2 slots))

(define (validate-params params)
  (parse-params params)
  params)

(define (parse-params params)
  (unless (list? params)
    (error "Parameter list must be a list" params))
  (let loop ((xs params)
             (mode 'required)
             (required '())
             (optional '())
             (rest-sym #f))
    (if (null? xs)
        (list (reverse required) (reverse optional) rest-sym)
        (let ((x (car xs)))
          (cond
           ((eq? x '&optional)
            (when (eq? mode 'optional)
              (error "Duplicate &optional in parameter list" params))
            (when rest-sym
              (error "&optional cannot appear after &rest" params))
            (loop (cdr xs) 'optional required optional rest-sym))
           ((or (eq? x '&rest) (eq? x '&body))
            (when rest-sym
              (error "Duplicate &rest/&body in parameter list" params))
            (unless (and (pair? (cdr xs))
                         (symbol? (cadr xs))
                         (null? (cddr xs)))
              (error "&rest/&body must be followed by one symbol at the end" params))
            (list (reverse required) (reverse optional) (cadr xs)))
           ((eq? mode 'required)
            (unless (symbol? x)
              (error "Required parameter must be a symbol" x))
            (loop (cdr xs) mode (cons x required) optional rest-sym))
           (else
            (cond
             ((symbol? x)
              (loop (cdr xs) mode required (cons (list x #f '()) optional) rest-sym))
             ((and (list? x) (= (length x) 2) (symbol? (car x)))
              (loop (cdr xs) mode required (cons (list (car x) #t (cadr x)) optional) rest-sym))
             (else
              (error "Invalid &optional parameter specifier" x)))))))))

(define (tail-call? obj)
  (and (vector? obj) (= (vector-length obj) 3) (eq? (vector-ref obj 0) 'tail-call)))

(define (make-tail-call fn args)
  (vector 'tail-call fn args))

(define (tail-call-fn tc) (vector-ref tc 1))
(define (tail-call-args tc) (vector-ref tc 2))

(define *block-stack* '())
(define *catch-stack* '())
(define *class-table* '())
(define *accessor-slot-table* '())
(define *trace-table* '())
(define *trace-macro-set* '())
(define *trace-depth* 0)
(define *debug-mode* #f)
(define *break-level* 0)
(define *gensym-counter* 0)

(define (keyword-symbol? sym)
  (and (symbol? sym)
       (> (string-length (symbol->string sym)) 0)
       (char=? (string-ref (symbol->string sym) 0) #\:)))

(define (go-signal? obj)
  (and (vector? obj) (= (vector-length obj) 2) (eq? (vector-ref obj 0) 'go-signal)))

(define (make-go-signal tag)
  (vector 'go-signal tag))

(define (go-signal-tag s) (vector-ref s 1))

(define (make-gensym prefix)
  (set! *gensym-counter* (+ *gensym-counter* 1))
  (string->symbol
   (string-append prefix (number->string *gensym-counter*))))

(define (class-table-set! sym class-obj)
  (set! *class-table*
        (cons (cons sym class-obj)
              (filter (lambda (p) (not (eq? (car p) sym))) *class-table*))))

(define (class-table-ref sym)
  (let ((entry (assoc sym *class-table*)))
    (and entry (cdr entry))))

(define (accessor-slot-set! accessor slot)
  (set! *accessor-slot-table*
        (cons (cons accessor slot)
              (filter (lambda (p) (not (eq? (car p) accessor))) *accessor-slot-table*))))

(define (accessor-slot-ref accessor)
  (let ((entry (assoc accessor *accessor-slot-table*)))
    (and entry (cdr entry))))

(define (resolve-class-designator designator env)
  (cond
   ((class? designator) designator)
   ((symbol? designator)
    (let* ((sym (if (frame-bound? env designator)
                    designator
                    (resolve-binding-symbol designator)))
           (pair (frame-find-pair env sym))
           (bound (and pair (cdr pair)))
           (tbl (class-table-ref sym)))
      (cond
       ((class? bound) bound)
       ((class? tbl) tbl)
       (else
        (error "Unknown class designator" designator)))))
   (else
    (error "Class designator must be symbol or class object" designator))))

(define (parse-slot-spec slot-spec)
  (cond
   ((symbol? slot-spec)
    (let ((name (resolve-binding-symbol slot-spec)))
      (make-slot-spec name
                      '()
                      #f
                      (string->symbol (string-append ":" (symbol-base-name name)))
                      '()
                      '())))
   ((and (list? slot-spec) (pair? slot-spec) (symbol? (car slot-spec)))
    (let ((name (resolve-binding-symbol (car slot-spec)))
          (rest (cdr slot-spec))
          (initform '())
          (accessor #f)
          (initarg #f)
          (readers '())
          (writers '()))
      (let parse ((xs rest))
        (unless (null? xs)
          (unless (and (pair? xs) (pair? (cdr xs)))
            (error "slot options must be keyword/value pairs" slot-spec))
          (let ((k (car xs))
                (v (cadr xs)))
            (cond
             ((eq? k ':initform)
              (set! initform v))
             ((eq? k ':accessor)
              (unless (symbol? v)
                (error ":accessor value must be symbol" v))
              (let ((sym (resolve-binding-symbol v)))
                (set! accessor sym)
                (set! readers (cons sym readers))))
             ((eq? k ':reader)
              (unless (symbol? v)
                (error ":reader value must be symbol" v))
              (set! readers (cons (resolve-binding-symbol v) readers)))
             ((eq? k ':writer)
              (unless (symbol? v)
                (error ":writer value must be symbol" v))
              (set! writers (cons (resolve-binding-symbol v) writers)))
             ((eq? k ':initarg)
              (unless (symbol? v)
                (error ":initarg value must be symbol" v))
              (set! initarg v))
             (else
              (error "unsupported slot option in defclass" k))))
          (parse (cddr xs))))
      (make-slot-spec name
                      initform
                      accessor
                      (if initarg
                          initarg
                          (string->symbol (string-append ":" (symbol-base-name name))))
                      (delete-duplicates (reverse readers) eq?)
                      (delete-duplicates (reverse writers) eq?))))
   (else
    (error "Invalid slot specifier in defclass" slot-spec))))

(define (slot-spec-list-merge specs)
  (let loop ((rest specs) (acc '()))
    (if (null? rest)
        (reverse acc)
        (let* ((s (car rest))
               (name (slot-spec-name s))
               (acc2 (filter (lambda (e) (not (eq? (slot-spec-name e) name))) acc)))
          (loop (cdr rest) (cons s acc2))))))

(define (class-effective-slots class-obj)
  (let collect ((c class-obj) (seen '()))
    (if (memq c seen)
        (error "Cyclic class precedence list" (class-name c))
        (let ((seen2 (cons c seen)))
          (slot-spec-list-merge
           (append
            (apply append
                   (map (lambda (s) (collect s seen2))
                        (class-supers c)))
            (class-own-slots c)))))))

(define (symbol-base-name sym)
  (let ((split (symbol-split-package sym)))
    (if split
        (cadr split)
        (let ((s (symbol->string sym)))
          (if (and (> (string-length s) 0)
                   (char=? (string-ref s 0) #\:))
              (substring s 1 (string-length s))
              s)))))

(define (find-instance-slot-entry slots slot)
  (or (assoc slot slots)
      (find (lambda (entry)
              (and (symbol? (car entry))
                   (string=? (symbol-base-name (car entry))
                             (symbol-base-name slot))))
            slots)))

(define (find-slot-spec-entry specs slot)
  (or (find (lambda (s) (eq? (slot-spec-initarg s) slot)) specs)
      (find (lambda (s) (eq? (slot-spec-name s) slot)) specs)
      (find (lambda (s)
              (string=? (symbol-base-name (slot-spec-name s))
                        (symbol-base-name slot)))
            specs)))

(define (instance-slot-ref obj slot)
  (unless (instance? obj)
    (error "slot-value target must be instance" obj))
  (unless (symbol? slot)
    (error "slot-value slot name must be symbol" slot))
  (let ((entry (find-instance-slot-entry (instance-slots obj) slot)))
    (if entry
        (cdr entry)
        (error "No such slot in instance" slot))))

(define (instance-slot-set! obj slot value)
  (unless (instance? obj)
    (error "slot-value target must be instance" obj))
  (unless (symbol? slot)
    (error "slot-value slot name must be symbol" slot))
  (let ((entry (find-instance-slot-entry (instance-slots obj) slot)))
    (if entry
        (begin
          (set-cdr! entry value)
          value)
        (error "No such slot in instance" slot))))

(define (class-distance sub super)
  (let walk ((current sub) (seen '()))
    (cond
     ((eq? current super) 0)
     ((memq current seen) #f)
     (else
      (let ((seen2 (cons current seen)))
        (let loop ((supers (class-supers current)) (best #f))
          (if (null? supers)
              best
              (let ((d (walk (car supers) seen2)))
                (if d
                    (let ((cand (+ d 1)))
                      (if (or (not best) (< cand best))
                          (loop (cdr supers) cand)
                          (loop (cdr supers) best)))
                    (loop (cdr supers) best))))))))))

(define (parse-method-params params env)
  (unless (list? params)
    (error "defmethod parameter list must be a list" params))
  (let parse-required ((xs params)
                       (plain-required '())
                       (specializers '()))
    (if (or (null? xs)
            (eq? (car xs) '&optional)
            (eq? (car xs) '&rest)
            (eq? (car xs) '&body))
        (let* ((plain-params (append (reverse plain-required) xs))
               (spec (parse-params plain-params))
               (required-spec (car spec)))
          (when (null? required-spec)
            (error "defmethod requires at least one required parameter for dispatch"))
          (unless (= (length required-spec) (length specializers))
            (error "defmethod required parameter parse mismatch" params))
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
            (error "invalid defmethod required parameter" x)))))))

(define (score<? a b)
  (let loop ((xs a) (ys b))
    (cond
     ((null? xs) #f)
     ((< (car xs) (car ys)) #t)
     ((> (car xs) (car ys)) #f)
     (else
      (loop (cdr xs) (cdr ys))))))

(define (score=? a b)
  (equal? a b))

(define (method-applicability-score method args)
  (let ((specializers (method-specializers method)))
    (if (< (length args) (length specializers))
        #f
        (let loop ((ss specializers)
                   (as args)
                   (acc '()))
          (if (null? ss)
              (reverse acc)
              (let ((spec (car ss))
                    (arg (car as)))
                (if spec
                    (if (instance? arg)
                        (let ((d (class-distance (instance-class arg) spec)))
                          (if d
                              (loop (cdr ss) (cdr as) (cons d acc))
                              #f))
                        #f)
                    (loop (cdr ss) (cdr as) (cons 1000000 acc)))))))))

(define (method-more-specific-conflict? m1 m2)
  (let ((s1 (method-specializers m1))
        (s2 (method-specializers m2)))
    (and (not (equal? s1 s2))
         (not (null? s1)))))

(define (generic-method-score method args)
  (method-applicability-score method args))

(define (select-generic-method generic args)
  (let loop ((methods (generic-methods generic))
             (best-method #f)
             (best-score #f))
    (if (null? methods)
        best-method
        (let* ((m (car methods))
               (score (generic-method-score m args)))
          (cond
           ((not score)
            (loop (cdr methods) best-method best-score))
           ((or (not best-score) (score<? score best-score))
            (loop (cdr methods) m score))
           ((score=? score best-score)
            (if (and best-method
                     (method-more-specific-conflict? best-method m))
                (error "Ambiguous applicable methods in generic function"
                       (generic-name generic)
                       (list (method-specializers best-method)
                             (method-specializers m))
                       args)
                (loop (cdr methods) best-method best-score)))
           (else
            (loop (cdr methods) best-method best-score)))))))

(define (apply-generic generic args)
  (let ((m (select-generic-method generic args)))
    (if m
        (apply-islisp (method-proc m) args)
        (error "No applicable method for generic function" (generic-name generic) args))))

(define (trace-entry sym)
  (assoc sym *trace-table*))

(define (macro-traced? sym)
  (if (memq sym *trace-macro-set*) #t #f))

(define (add-macro-trace! sym)
  (unless (macro-traced? sym)
    (set! *trace-macro-set* (cons sym *trace-macro-set*))))

(define (remove-macro-trace! sym)
  (set! *trace-macro-set*
        (filter (lambda (s) (not (eq? s sym))) *trace-macro-set*)))

(define (remove-trace-entry sym)
  (set! *trace-table*
        (filter (lambda (p) (not (eq? (car p) sym))) *trace-table*)))

(define (trace-indent-string)
  (make-string (* 2 *trace-depth*) #\space))

(define (trace-wrap-function sym fn)
  (make-primitive
   sym
   (lambda args
     (display (trace-indent-string))
     (display "-> ")
     (write sym)
     (display " ")
     (write args)
     (newline)
     (set! *trace-depth* (+ *trace-depth* 1))
     (guard (e
             (else
              (set! *trace-depth* (- *trace-depth* 1))
              (display (trace-indent-string))
              (display "<! ")
              (write sym)
              (display " ERROR ")
              (write e)
              (newline)
              (raise e)))
       (let ((result (apply-islisp fn args)))
         (set! *trace-depth* (- *trace-depth* 1))
         (display (trace-indent-string))
         (display "<- ")
         (write sym)
         (display " ")
         (write result)
         (newline)
         result)))))

(define (trace-apply-macro sym macro raw-args)
  (display (trace-indent-string))
  (display "=>MACRO ")
  (write sym)
  (newline)
  (display (trace-indent-string))
  (display "  EXPAND-BEFORE ")
  (write (cons sym raw-args))
  (newline)
  (set! *trace-depth* (+ *trace-depth* 1))
  (guard (e
          (else
           (set! *trace-depth* (- *trace-depth* 1))
           (display (trace-indent-string))
           (display "<!MACRO ")
           (write sym)
            (display " ERROR ")
           (write e)
           (newline)
           (raise e)))
    (let ((result (apply-macro macro raw-args)))
      (set! *trace-depth* (- *trace-depth* 1))
      (display (trace-indent-string))
      (display "  EXPAND-AFTER ")
      (write result)
      (newline)
      (display (trace-indent-string))
      (display "<=MACRO ")
      (write sym)
      (newline)
      result)))

(define (trace-one! env sym)
  (let ((pair (frame-find-pair env sym)))
    (unless pair
      (error "Cannot trace unbound function" sym))
    (let ((current (cdr pair)))
      (cond
       ((or (primitive? current) (closure? current))
        (unless (trace-entry sym)
          (set! *trace-table* (cons (cons sym current) *trace-table*))
          (set-cdr! pair (trace-wrap-function sym current))))
       ((macro? current)
        (add-macro-trace! sym))
       (else
        (error "trace supports functions or macros only" sym)))))
  sym)

(define (untrace-one! env sym)
  (let ((entry (trace-entry sym)))
    (when entry
      (let ((pair (frame-find-pair env sym)))
        (when pair
          (set-cdr! pair (cdr entry))))
      (remove-trace-entry sym)))
  (remove-macro-trace! sym)
  sym)

(define (untrace-all! env)
  (for-each
   (lambda (entry)
     (let ((pair (frame-find-pair env (car entry))))
       (when pair
         (set-cdr! pair (cdr entry)))))
   *trace-table*)
  (set! *trace-table* '())
  (set! *trace-macro-set* '())
  '())

(define (debug-write-result x)
  (cond
   ((closure? x)
    (let ((name (closure-name x)))
      (if name
          (begin
            (display "#<function ")
            (display name)
            (display ">"))
          (display "#<closure>"))))
   ((primitive? x)
    (begin
      (display "#<primitive ")
      (display (primitive-name x))
      (display ">")))
   ((macro? x)
    (let ((name (macro-name x)))
      (if name
          (begin
            (display "#<macro ")
            (display name)
            (display ">"))
          (display "#<macro>"))))
   (else
    (write x))))

(define (break-loop env message)
  (set! *break-level* (+ *break-level* 1))
  (dynamic-wind
    (lambda ()
      (display "*** BREAK [level ")
      (display *break-level*)
      (display "] ")
      (display message)
      (display " (Ctrl-D to continue)")
      (newline))
    (lambda ()
      (let loop ()
        (display "BREAK> ")
        (flush)
        (let ((x (read)))
          (if (eof-object? x)
              '()
              (begin
                (guard (e (else
                           (display "Break Error: ")
                           (write e)
                           (newline)))
                  (let ((result (eval-islisp x env)))
                    (debug-write-result result)
                    (newline)))
                (loop))))))
    (lambda ()
      (set! *break-level* (- *break-level* 1)))))

(define (force-value v)
  (let loop ((x v))
    (if (tail-call? x)
        (loop (apply-islisp (tail-call-fn x) (tail-call-args x)))
        x)))

(define (eval-sequence* forms env tail?)
  (let loop ((xs forms))
    (cond
     ((null? xs) '())
     ((null? (cdr xs))
      (eval-islisp* (car xs) env tail?))
     (else
      (force-value (eval-islisp* (car xs) env #f))
      (loop (cdr xs))))))

(define (eval-sequence forms env)
  (force-value (eval-sequence* forms env #f)))

(define (eval-list xs env)
  (map (lambda (x) (force-value (eval-islisp* x env #f))) xs))

(define (write-to-string x)
  (let ((p (open-output-string)))
    (write x p)
    (get-output-string p)))

(define (display-to-string x)
  (let ((p (open-output-string)))
    (display x p)
    (get-output-string p)))

(define (render-format fmt args)
  (unless (string? fmt)
    (error "format control string must be a string" fmt))
  (letrec ((require-arg
            (lambda (rest)
              (when (null? rest)
                (error "too few arguments for format" fmt))
              (car rest)))
           (require-number
            (lambda (rest who)
              (let ((v (require-arg rest)))
                (unless (number? v)
                  (error who "needs a number argument" v))
                v)))
           (num-arg->string
            (lambda (rest base who)
              (let ((v (require-number rest who)))
                (number->string v base)))))
    (let ((p (open-output-string)))
      (let loop ((i 0) (rest args))
        (if (>= i (string-length fmt))
            (begin
              (unless (null? rest)
                (error "too many arguments for format" rest))
              (get-output-string p))
            (let ((ch (string-ref fmt i)))
              (if (char=? ch #\~)
                  (if (>= (+ i 1) (string-length fmt))
                      (error "unterminated format directive" fmt)
                      (let* ((d (string-ref fmt (+ i 1)))
                             (u (char-upcase d)))
                        (cond
                         ((or (char=? u #\A) (char=? u #\W))
                          (display (display-to-string (require-arg rest)) p)
                          (loop (+ i 2) (cdr rest)))
                         ((char=? u #\S)
                          (display (write-to-string (require-arg rest)) p)
                          (loop (+ i 2) (cdr rest)))
                         ((char=? u #\D)
                          (display (num-arg->string rest 10 "format ~D") p)
                          (loop (+ i 2) (cdr rest)))
                         ((char=? u #\B)
                          (display (num-arg->string rest 2 "format ~B") p)
                          (loop (+ i 2) (cdr rest)))
                         ((char=? u #\O)
                          (display (num-arg->string rest 8 "format ~O") p)
                          (loop (+ i 2) (cdr rest)))
                         ((char=? u #\X)
                          (display (num-arg->string rest 16 "format ~X") p)
                          (loop (+ i 2) (cdr rest)))
                         ((char=? u #\R)
                          (display (num-arg->string rest 10 "format ~R") p)
                          (loop (+ i 2) (cdr rest)))
                         ((char=? u #\F)
                          (display (number->string (exact->inexact (require-number rest "format ~F"))) p)
                          (loop (+ i 2) (cdr rest)))
                         ((char=? u #\E)
                          (display (number->string (exact->inexact (require-number rest "format ~E"))) p)
                          (loop (+ i 2) (cdr rest)))
                         ((char=? u #\G)
                          (display (number->string (exact->inexact (require-number rest "format ~G"))) p)
                          (loop (+ i 2) (cdr rest)))
                         ((char=? u #\P)
                          (let ((v (require-arg rest)))
                            (unless (number? v)
                              (error "format ~P needs a number argument" v))
                            (unless (= v 1)
                              (write-char #\s p))
                            (loop (+ i 2) (cdr rest))))
                         ((char=? u #\C)
                          (let ((v (require-arg rest)))
                            (unless (char? v)
                              (error "format ~C needs a character argument" v))
                            (write-char v p)
                            (loop (+ i 2) (cdr rest))))
                         ((char=? d #\newline)
                          (let skip ((j (+ i 2)))
                            (if (and (< j (string-length fmt))
                                     (char-whitespace? (string-ref fmt j)))
                                (skip (+ j 1))
                                (loop j rest))))
                         ((char=? u #\%)
                          (newline p)
                          (loop (+ i 2) rest))
                         ((char=? u #\&)
                          (newline p)
                          (loop (+ i 2) rest))
                         ((char=? u #\T)
                          (write-char #\tab p)
                          (loop (+ i 2) rest))
                         ((char=? u #\|)
                          (write-char #\x0c p)
                          (loop (+ i 2) rest))
                         ((char=? u #\_)
                          (write-char #\space p)
                          (loop (+ i 2) rest))
                         ((char=? u #\?)
                          (let ((sub-fmt (require-arg rest))
                                (sub-args (require-arg (cdr rest))))
                            (unless (string? sub-fmt)
                              (error "format ~? needs a string as control argument" sub-fmt))
                            (unless (list? sub-args)
                              (error "format ~? needs a list as argument list" sub-args))
                            (display (render-format sub-fmt sub-args) p)
                            (loop (+ i 2) (cddr rest))))
                         ((char=? d #\~)
                          (write-char #\~ p)
                          (loop (+ i 2) rest))
                         (else
                          (error "unsupported format directive" d)))))
                  (begin
                    (write-char ch p)
                    (loop (+ i 1) rest)))))))))

(define (ensure-string x who)
  (unless (string? x)
    (error who "needs a string" x)))

(define (ensure-char x who)
  (unless (char? x)
    (error who "needs a character" x)))

(define (ensure-nonnegative-integer x who)
  (unless (and (integer? x) (>= x 0))
    (error who "needs a non-negative integer" x)))

(define (string-index* haystack needle start who)
  (ensure-string haystack who)
  (ensure-string needle who)
  (ensure-nonnegative-integer start who)
  (let ((n (string-length haystack))
        (m (string-length needle)))
    (when (> start n)
      (error who "start index out of range" start n))
    (if (= m 0)
        start
        (let loop ((i start))
          (cond
           ((> (+ i m) n) '())
           ((string=? (substring haystack i (+ i m)) needle) i)
           (else (loop (+ i 1))))))))

(define (isl-substring s start end)
  (ensure-string s "substring")
  (ensure-nonnegative-integer start "substring")
  (ensure-nonnegative-integer end "substring")
  (let ((n (string-length s)))
    (when (> start end)
      (error "substring start must be <= end" start end))
    (when (> end n)
      (error "substring end out of range" end n))
    (substring s start end)))

(define (isl-length x)
  (cond
   ((string? x) (string-length x))
   ((list? x) (length x))
   ((vector? x) (vector-length x))
   (else
    (error "length supports string/list/vector only" x))))

(define (hash-comparator-from-test test)
  (cond
   ((or (not test) (eq? test 'eq) (eq? test 'eq?)) 'eq?)
   ((or (eq? test 'eql) (eq? test 'eqv?)) 'eqv?)
   ((or (eq? test 'equal) (eq? test 'equal?)) 'equal?)
   ((or (eq? test 'string=) (eq? test 'string=?)) 'string=?)
   ((procedure? test) test)
   (else
    (error "unsupported hash-table test" test))))

(define (internal-time-units)
  (guard (e
          (else 100))
    (let ((ts (sys-times)))
      (if (and (list? ts)
               (>= (length ts) 5)
               (integer? (list-ref ts 4))
               (> (list-ref ts 4) 0))
          (list-ref ts 4)
          100))))

(define (internal-real-time)
  (inexact->exact
   (floor (* (time->seconds (current-time))
             (internal-time-units)))))

(define (internal-run-time)
  (guard (e
          (else 0))
    (let ((ts (sys-times)))
      (if (and (list? ts)
               (>= (length ts) 2)
               (integer? (list-ref ts 0))
               (integer? (list-ref ts 1)))
          (+ (list-ref ts 0) (list-ref ts 1))
          0))))

(define (eval-while args env who)
  (if (>= (length args) 1)
      (let ((test-form (car args))
            (body (cdr args)))
        (let loop ()
          (if (truthy? (eval-islisp test-form env))
              (begin
                (eval-sequence body env)
                (loop))
              '())))
      (error who "needs test expression and optional body" args)))

(define (eval-loop args env)
  (let ((for-var #f)
        (start-form #f)
        (end-form #f)
        (until-form #f)
        (body '()))
    (let parse ((xs args))
      (cond
       ((null? xs) 'done)
       ((eq? (car xs) 'for)
        (when for-var
          (error "loop for clause appears multiple times" args))
        (unless (and (pair? (cdr xs))
                     (pair? (cddr xs))
                     (pair? (cdddr xs))
                     (pair? (cddddr xs))
                     (pair? (cddddr (cdr xs)))
                     (symbol? (cadr xs))
                     (eq? (caddr xs) 'from)
                     (eq? (car (cddddr xs)) 'to))
          (error "invalid loop for clause" xs))
        (set! for-var (cadr xs))
        (set! start-form (cadddr xs))
        (set! end-form (cadr (cddddr xs)))
        (parse (cddr (cddddr xs))))
       ((eq? (car xs) 'until)
        (when until-form
          (error "loop until clause appears multiple times" args))
        (unless (pair? (cdr xs))
          (error "loop until clause needs a test form" xs))
        (set! until-form (cadr xs))
        (parse (cddr xs)))
       ((eq? (car xs) 'do)
        (set! body (cdr xs)))
       (else
        (error "unknown loop clause" (car xs)))))
    (unless (or for-var until-form)
      (error "loop needs at least for-clause or until-clause" args))
    (let ((loop-env (make-frame env)))
      (if for-var
          (let ((start (eval-islisp start-form env))
                (end (eval-islisp end-form env)))
            (unless (and (integer? start) (integer? end))
              (error "loop from/to currently requires integers" start end))
            (frame-define! loop-env for-var start)
            (let loop-iter ()
              (let ((i (frame-ref loop-env for-var)))
                (if (> i end)
                    '()
                    (if (and until-form (truthy? (eval-islisp until-form loop-env)))
                        '()
                        (begin
                          (eval-sequence body loop-env)
                          (frame-set! loop-env for-var (+ i 1))
                          (loop-iter)))))))
          (let loop-iter ()
            (if (and until-form (truthy? (eval-islisp until-form loop-env)))
                '()
                (begin
                  (eval-sequence body loop-env)
                  (loop-iter))))))))

(define (eval-tagbody args env)
  (let ((items args))
    (let make-labels ((xs items) (idx 0) (acc '()))
      (if (null? xs)
          (let ((labels (reverse acc))
                (len (length items)))
            (let loop ((i 0))
              (if (>= i len)
                  '()
                  (let ((item (list-ref items i)))
                    (if (or (symbol? item) (integer? item))
                        (loop (+ i 1))
                        (let ((next-i
                               (guard (e
                                       ((go-signal? e)
                                        (let ((p (assoc (go-signal-tag e) labels)))
                                          (if p
                                              (cdr p)
                                              (raise e))))
                                       (else
                                        (raise e)))
                                 (force-value (eval-islisp* item env #f))
                                 (+ i 1))))
                          (loop next-i)))))))
          (let ((x (car xs)))
            (if (or (symbol? x) (integer? x))
                (if (assoc x acc)
                    (error "duplicate tagbody label" x)
                    (make-labels (cdr xs) (+ idx 1) (cons (cons x idx) acc)))
                (make-labels (cdr xs) (+ idx 1) acc)))))))

(define (eval-block args env tail?)
  (if (>= (length args) 1)
      (let ((name (resolve-binding-symbol (car args)))
            (body (cdr args)))
        (unless (symbol? name)
          (error "block name must be a symbol" name))
        (call/cc
         (lambda (escape)
           (dynamic-wind
             (lambda ()
               (set! *block-stack* (cons (cons name escape) *block-stack*)))
             (lambda ()
               (eval-sequence* body env tail?))
             (lambda ()
               (set! *block-stack* (cdr *block-stack*)))))))
      (error "block needs name and optional body" args)))

(define (eval-catch args env tail?)
  (if (>= (length args) 1)
      (let ((tag (eval-islisp* (car args) env #f))
            (body (cdr args)))
        (call/cc
         (lambda (escape)
           (dynamic-wind
             (lambda ()
               (set! *catch-stack* (cons (cons tag escape) *catch-stack*)))
             (lambda ()
               ;; Keep non-local exits inside catch dynamic extent even for tail calls.
               (force-value (eval-sequence* body env tail?)))
             (lambda ()
               (set! *catch-stack* (cdr *catch-stack*)))))))
      (error "catch needs tag and optional body" args)))

(define (throw-to-catch tag value)
  (let loop ((xs *catch-stack*))
    (if (null? xs)
        (error "No enclosing catch for throw" tag)
        (let ((entry (car xs)))
          (if (eqv? (car entry) tag)
              ((cdr entry) value)
              (loop (cdr xs)))))))

(define (init-do-bindings bindings env do-env)
  (for-each
   (lambda (b)
     (unless (and (list? b) (>= (length b) 2) (<= (length b) 3) (symbol? (car b)))
       (error "invalid do binding" b))
     (frame-define! do-env (car b) (eval-islisp (cadr b) env)))
   bindings))

(define (step-do-bindings! bindings do-env)
  (let ((nexts
         (map
          (lambda (b)
            (let ((sym (car b)))
              (cons sym
                    (if (= (length b) 3)
                        (eval-islisp (caddr b) do-env)
                        (frame-ref do-env sym)))))
          bindings)))
    (for-each (lambda (p) (frame-set! do-env (car p) (cdr p))) nexts)))

(define (eval-do args env tail?)
  (if (>= (length args) 2)
      (let ((bindings (car args))
            (end-clause (cadr args))
            (body (cddr args)))
        (unless (list? bindings)
          (error "do bindings must be a list" bindings))
        (unless (and (list? end-clause) (not (null? end-clause)))
          (error "do end clause must be (test . result-forms)" end-clause))
        (let ((test-form (car end-clause))
              (result-forms (cdr end-clause))
              (do-env (make-frame env)))
          (init-do-bindings bindings env do-env)
          (let loop ()
            (if (truthy? (eval-islisp test-form do-env))
                (if (null? result-forms)
                    '()
                    (if tail?
                        (eval-sequence* result-forms do-env #t)
                        (eval-sequence result-forms do-env)))
                (begin
                  (eval-sequence body do-env)
                  (step-do-bindings! bindings do-env)
                  (loop))))))
      (error "do needs bindings, end clause and optional body" args)))

(define (eval-dolist args env tail?)
  (if (>= (length args) 1)
      (let ((spec (car args))
            (body (cdr args)))
        (unless (and (list? spec) (>= (length spec) 2) (<= (length spec) 3) (symbol? (car spec)))
          (error "dolist spec must be (var list [result])" spec))
        (let* ((var (car spec))
               (list-form (cadr spec))
               (result-form (and (= (length spec) 3) (caddr spec)))
               (values (eval-islisp list-form env))
               (loop-env (make-frame env)))
          (unless (list? values)
            (error "dolist requires a list value" values))
          (frame-define! loop-env var '())
          (for-each
           (lambda (v)
             (frame-set! loop-env var v)
             (eval-sequence body loop-env))
           values)
          (frame-set! loop-env var '())
          (if result-form
              (if tail?
                  (eval-islisp* result-form loop-env #t)
                  (eval-islisp result-form loop-env))
              '())))
      (error "dolist needs (var list [result]) and optional body" args)))

(define (eval-dotimes args env tail?)
  (if (>= (length args) 1)
      (let ((spec (car args))
            (body (cdr args)))
        (unless (and (list? spec) (>= (length spec) 2) (<= (length spec) 3) (symbol? (car spec)))
          (error "dotimes spec must be (var count [result])" spec))
        (let* ((var (car spec))
               (count (eval-islisp (cadr spec) env))
               (result-form (and (= (length spec) 3) (caddr spec)))
               (loop-env (make-frame env)))
          (unless (and (integer? count) (>= count 0))
            (error "dotimes count must be a non-negative integer" count))
          (frame-define! loop-env var 0)
          (let loop ((i 0))
            (if (< i count)
                (begin
                  (frame-set! loop-env var i)
                  (eval-sequence body loop-env)
                  (loop (+ i 1)))
                (begin
                  (frame-set! loop-env var count)
                  (if result-form
                      (if tail?
                          (eval-islisp* result-form loop-env #t)
                          (eval-islisp result-form loop-env))
                      '()))))))
      (error "dotimes needs (var count [result]) and optional body" args)))

(define (eval-defclass args env)
  (unless (= (length args) 3)
    (error "defclass takes class-name, superclasses and slot-specs" args))
  (let* ((raw-name (car args))
         (name (resolve-binding-symbol raw-name))
         (super-forms (cadr args))
         (slot-forms (caddr args)))
    (unless (symbol? name)
      (error "defclass name must be symbol" raw-name))
    (unless (list? super-forms)
      (error "defclass superclass list must be a list" super-forms))
    (unless (list? slot-forms)
      (error "defclass slot spec list must be a list" slot-forms))
    (let* ((supers
            (map (lambda (s)
                   (unless (symbol? s)
                     (error "defclass superclass must be symbol" s))
                   (resolve-class-designator s env))
                 super-forms))
           (own-slots (map parse-slot-spec slot-forms))
           (class-obj (make-class name supers own-slots))
           (global (global-frame env)))
      (frame-define! global name class-obj)
      (class-table-set! name class-obj)
      (for-each
       (lambda (s)
         (let ((slot-name (slot-spec-name s)))
           (for-each
            (lambda (reader)
              (let* ((method-proc
                     (make-closure '(obj)
                                    (list (list 'slot-value 'obj (list 'quote slot-name)))
                                    env
                                    reader))
                     (generic (ensure-generic-function! reader env '(obj)))
                     (method (make-method (list class-obj) '(obj) method-proc)))
                (set-generic-methods! generic (cons method (generic-methods generic)))))
            (slot-spec-readers s))
           (for-each
            (lambda (writer)
              (let* ((method-proc
                      (make-closure '(obj value)
                                    (list (list 'setf
                                                (list 'slot-value 'obj (list 'quote slot-name))
                                                'value))
                                    env
                                    writer))
                     (generic (ensure-generic-function! writer env '(obj value)))
                     (method (make-method (list class-obj #f) '(obj value) method-proc)))
                (set-generic-methods! generic (cons method (generic-methods generic)))))
            (slot-spec-writers s))
           (let ((acc (slot-spec-accessor s)))
             (when acc
               (accessor-slot-set! acc slot-name)))))
       own-slots)
      name)))

(define (ensure-generic-function! sym env params)
  (let* ((global (global-frame env))
         (pair (frame-find-pair global sym)))
    (cond
     ((and pair (generic? (cdr pair)))
      (cdr pair))
     (pair
      (error "Binding already exists and is not a generic function" sym))
     (else
      (let ((g (make-generic sym params '())))
        (frame-define! global sym g)
        g)))))

(define (eval-defgeneric args env)
  (unless (= (length args) 2)
    (error "defgeneric takes name and parameter list" args))
  (let* ((name (resolve-binding-symbol (car args)))
         (params (cadr args)))
    (unless (symbol? name)
      (error "defgeneric name must be symbol" name))
    (validate-params params)
    (ensure-generic-function! name env params)
    name))

(define (eval-defmethod args env)
  (unless (>= (length args) 3)
    (error "defmethod takes name, parameter list and body" args))
  (let* ((name (resolve-binding-symbol (car args)))
         (raw-params (cadr args))
         (body (cddr args)))
    (unless (symbol? name)
      (error "defmethod name must be symbol" name))
    (let* ((parsed (parse-method-params raw-params env))
           (plain-params (car parsed))
           (specializers (cadr parsed))
           (method-proc (make-closure plain-params body env name))
           (generic (ensure-generic-function! name env plain-params))
           (method (make-method specializers plain-params method-proc)))
      ;; Newer methods of same specificity should win.
      (set-generic-methods! generic (cons method (generic-methods generic)))
      name)))

(define (eval-with-open-file args env tail?)
  (if (>= (length args) 1)
      (let ((spec (car args))
            (body (cdr args)))
        (unless (and (list? spec) (>= (length spec) 2) (symbol? (car spec)))
          (error "with-open-file spec must be (var filename &key :direction ...)" spec))
        (let* ((var (car spec))
               (filename (eval-islisp* (cadr spec) env #f))
               (opts (cddr spec))
               (direction ':input)
               (if-exists ':supersede)
               (if-does-not-exist ':default))
          (unless (string? filename)
            (error "with-open-file filename must evaluate to string" filename))
          (let parse ((xs opts))
            (unless (null? xs)
              (unless (and (pair? xs) (pair? (cdr xs)))
                (error "with-open-file options must be keyword/value pairs" opts))
              (let ((k (car xs))
                    (v (eval-islisp* (cadr xs) env #f)))
                (cond
                 ((eq? k ':direction)
                  (set! direction v))
                 ((eq? k ':if-exists)
                  (set! if-exists v))
                 ((eq? k ':if-does-not-exist)
                  (set! if-does-not-exist v))
                 (else
                  (error "unsupported with-open-file option" k))))
              (parse (cddr xs))))
          (let ((port
                 (cond
                  ((eq? direction ':input)
                   (let ((mode
                          (cond
                           ((eq? if-does-not-exist ':default) #f)
                           ((or (null? if-does-not-exist) (eq? if-does-not-exist #f)) #f)
                           ((eq? if-does-not-exist ':error) :error)
                           (else
                            (error "with-open-file :if-does-not-exist for :input must be nil or :error"
                                   if-does-not-exist)))))
                     (open-input-file filename :if-does-not-exist mode)))
                  ((eq? direction ':output)
                   (let ((exists-mode
                          (cond
                           ((or (eq? if-exists ':overwrite) (eq? if-exists ':supersede)) :supersede)
                           ((eq? if-exists ':append) :append)
                           ((eq? if-exists ':error) :error)
                           ((or (null? if-exists) (eq? if-exists #f)) #f)
                           (else
                            (error "with-open-file :if-exists must be :overwrite, :supersede, :append, :error or nil"
                                   if-exists))))
                         (missing-mode
                          (cond
                           ((eq? if-does-not-exist ':default) :create)
                           ((eq? if-does-not-exist ':create) :create)
                           ((eq? if-does-not-exist ':error) :error)
                           ((or (null? if-does-not-exist) (eq? if-does-not-exist #f)) #f)
                           (else
                            (error "with-open-file :if-does-not-exist for :output must be :create, :error or nil"
                                   if-does-not-exist)))))
                     (open-output-file filename
                                       :if-exists exists-mode
                                       :if-does-not-exist missing-mode)))
                  (else
                   (error "with-open-file :direction must be :input or :output" direction)))))
            (let ((file-env (make-frame env)))
              (frame-define! file-env var (if port port '()))
              (if port
                  (dynamic-wind
                    (lambda () #f)
                    (lambda ()
                      ;; Evaluate body before closing the stream.
                      (force-value (eval-sequence* body file-env tail?)))
                    (lambda ()
                      (if (eq? direction ':input)
                          (close-input-port port)
                          (close-output-port port))))
                  (eval-sequence* body file-env tail?))))))
      (error "with-open-file needs (var filename &key ...) and optional body" args)))

(define (parse-handler-case-clause clause)
  (unless (and (list? clause) (>= (length clause) 2))
    (error "invalid handler-case clause" clause))
  (let ((tag (car clause))
        (rest (cdr clause))
        (var #f))
    (unless (symbol? tag)
      (error "handler-case clause tag must be symbol" tag))
    (when (and (pair? rest) (list? (car rest)))
      (let ((vs (car rest)))
        (unless (or (null? vs)
                    (and (= (length vs) 1) (symbol? (car vs))))
          (error "handler-case variable list must be () or (var)" vs))
        (set! var (and (pair? vs) (car vs)))
        (set! rest (cdr rest))))
    (list tag var rest)))

(define (handler-case-tag-match? tag)
  (or (eq? tag 'error)
      (eq? tag 'condition)
      (eq? tag 'serious-condition)
      (eq? tag 't)
      (eq? tag 'otherwise)))

(define (eval-handler-case args env tail?)
  (unless (>= (length args) 1)
    (error "handler-case needs a protected form and clauses" args))
  (let ((protected (car args))
        (clauses (map parse-handler-case-clause (cdr args))))
    (guard (e
            (else
             (let loop ((cs clauses))
               (if (null? cs)
                   (raise e)
                   (let* ((c (car cs))
                          (tag (car c))
                          (var (cadr c))
                          (body (caddr c)))
                     (if (handler-case-tag-match? tag)
                         (let ((henv (make-frame env)))
                           (when var
                             (frame-define! henv var e))
                           (eval-sequence* body henv tail?))
                         (loop (cdr cs))))))))
      ;; Keep exceptions from tail calls within handler-case dynamic extent.
      (force-value (eval-islisp* protected env tail?)))))

(define (eval-defpackage args)
  (unless (>= (length args) 1)
    (error "defpackage needs package name" args))
  (let* ((name (car args))
         (pkg (ensure-package! name)))
    (define (require-symbol-list vals who)
      (for-each
       (lambda (x)
         (unless (symbol? x)
           (error who "expects symbols" x)))
       vals))
    (for-each
     (lambda (opt)
       (unless (and (list? opt) (not (null? opt)))
         (error "invalid defpackage option" opt))
       (let ((tag (car opt))
             (vals (cdr opt)))
         (cond
          ((eq? tag ':nicknames)
           (for-each
            (lambda (x)
              (unless (or (symbol? x) (string? x))
                (error "defpackage :nicknames expects symbol/string designators" x))
              (register-package-name! x pkg))
            vals))
          ((eq? tag ':use)
           (for-each
            (lambda (x)
              (package-use! pkg (ensure-package! x)))
            vals))
          ((eq? tag ':intern)
           (require-symbol-list vals "defpackage :intern")
           (for-each
            (lambda (x)
              (package-intern! pkg (symbol-base-name x)))
            vals))
          ((eq? tag ':shadow)
           (require-symbol-list vals "defpackage :shadow")
           (for-each
            (lambda (x)
              (package-shadow-symbol! pkg x))
            vals))
          ((eq? tag ':import-from)
           (unless (>= (length vals) 2)
             (error "defpackage :import-from expects package and at least one symbol" opt))
           (let ((from (ensure-package! (car vals)))
                 (syms (cdr vals)))
             (require-symbol-list syms "defpackage :import-from")
             (for-each
              (lambda (x)
                (let ((s (package-find-external-symbol-by-name from x)))
                  (unless s
                    (error "defpackage :import-from symbol is not exported"
                           (car vals)
                           x))
                  (package-import-symbol! pkg s)))
              syms)))
          ((eq? tag ':shadowing-import-from)
           (unless (>= (length vals) 2)
             (error "defpackage :shadowing-import-from expects package and at least one symbol" opt))
           (let ((from (ensure-package! (car vals)))
                 (syms (cdr vals)))
             (require-symbol-list syms "defpackage :shadowing-import-from")
             (for-each
              (lambda (x)
                (let ((s (package-find-external-symbol-by-name from x)))
                  (unless s
                    (error "defpackage :shadowing-import-from symbol is not exported"
                           (car vals)
                           x))
                  (package-shadowing-import-symbol! pkg s)))
              syms)))
          ((eq? tag ':export)
           (for-each
            (lambda (x)
              (unless (symbol? x)
                (error "defpackage :export expects symbols" x))
              (package-export! pkg x))
            vals))
          ((eq? tag ':size)
           (unless (= (length vals) 1)
             (error "defpackage :size expects one integer value" opt))
           (unless (and (integer? (car vals)) (>= (car vals) 0))
             (error "defpackage :size expects non-negative integer" (car vals))))
          ((eq? tag ':documentation)
           (unless (= (length vals) 1)
             (error "defpackage :documentation expects one string value" opt))
           (unless (string? (car vals))
             (error "defpackage :documentation expects string" (car vals))))
          (else
           (error "unsupported defpackage option" tag)))))
     (cdr args))
    (string->symbol (package-name pkg))))

(define (eval-in-package args)
  (unless (= (length args) 1)
    (error "in-package takes exactly one argument" args))
  (let ((pkg (find-package (car args))))
    (unless pkg
      (error "Unknown package in in-package" (car args)))
    (set! *current-package* pkg)
    (string->symbol (package-name pkg))))

(define (bind-params! frame param-spec args)
  (let ((required (car param-spec))
        (optional (cadr param-spec))
        (rest-sym (caddr param-spec)))
    (when (< (length args) (length required))
      (error "Too few arguments" args))
    (let bind-required ((rs required) (as args))
      (if (null? rs)
          (let bind-optional ((os optional) (rest as))
            (if (null? os)
                (if rest-sym
                    (begin
                      (frame-define! frame rest-sym rest)
                      frame)
                    (if (null? rest)
                        frame
                        (error "Too many arguments" rest)))
                (let* ((entry (car os))
                       (sym (car entry))
                       (has-init (cadr entry))
                       (init-form (caddr entry)))
                  (if (null? rest)
                      (begin
                        (frame-define! frame
                                       sym
                                       (if has-init
                                           (force-value (eval-islisp* init-form frame #f))
                                           '()))
                        (bind-optional (cdr os) rest))
                      (begin
                        (frame-define! frame sym (car rest))
                        (bind-optional (cdr os) (cdr rest)))))))
          (begin
            (frame-define! frame (car rs) (car as))
            (bind-required (cdr rs) (cdr as)))))))

(define (quasiquote-eval expr env level)
  (define (qq-list xs)
    (if (null? xs)
        '()
        (let ((x (car xs)))
          (if (and (= level 1)
                   (pair? x)
                   (symbol? (car x))
                   (eq? (car x) 'unquote-splicing))
              (begin
                (unless (= (length x) 2)
                  (error "unquote-splicing takes one argument" x))
                (let ((vals (force-value (eval-islisp* (cadr x) env #f))))
                  (unless (list? vals)
                    (error "unquote-splicing value must be a list" vals))
                  (append vals (qq-list (cdr xs)))))
              (cons (quasiquote-eval x env level)
                    (qq-list (cdr xs)))))))
  (cond
   ((pair? expr)
    (cond
     ((and (symbol? (car expr)) (eq? (car expr) 'quasiquote))
      (unless (= (length expr) 2)
        (error "quasiquote takes one argument" expr))
      (list 'quasiquote (quasiquote-eval (cadr expr) env (+ level 1))))
     ((and (symbol? (car expr)) (eq? (car expr) 'unquote))
      (unless (= (length expr) 2)
        (error "unquote takes one argument" expr))
      (if (= level 1)
          (force-value (eval-islisp* (cadr expr) env #f))
          (list 'unquote (quasiquote-eval (cadr expr) env (- level 1)))))
     ((and (symbol? (car expr)) (eq? (car expr) 'unquote-splicing))
      (unless (= (length expr) 2)
        (error "unquote-splicing takes one argument" expr))
      (if (= level 1)
          (error "unquote-splicing is invalid outside list context" expr)
          (list 'unquote-splicing (quasiquote-eval (cadr expr) env (- level 1)))))
     ((list? expr)
      (qq-list expr))
     (else
      (cons (quasiquote-eval (car expr) env level)
            (quasiquote-eval (cdr expr) env level)))))
   (else expr)))

(define (apply-macro m raw-args)
  (let ((params (macro-params m))
        (body (macro-body m))
        (menv (macro-env m)))
    (let ((param-spec (parse-params params))
          (call-env (make-frame menv)))
      (bind-params! call-env param-spec raw-args)
      (force-value (eval-sequence* body call-env #t)))))

(define (macroexpand-1-with-flag form env)
  (if (and (pair? form) (symbol? (car form)))
      (let ((op (car form))
            (args (cdr form)))
        (if (special-form? op)
            (cons #f form)
            (let* ((op-sym (if (frame-bound? env op)
                               op
                               (resolve-symbol-in-package op)))
                   (op-pair (frame-find-pair env op-sym))
                   (op-val (and op-pair (cdr op-pair))))
              (if (and op-pair (macro? op-val))
                  (cons #t (if (macro-traced? op-sym)
                               (trace-apply-macro op-sym op-val args)
                               (apply-macro op-val args)))
                  (cons #f form)))))
      (cons #f form)))

(define (macroexpand-1* form env)
  (cdr (macroexpand-1-with-flag form env)))

(define (macroexpand* form env)
  (let loop ((f form))
    (let ((r (macroexpand-1-with-flag f env)))
      (if (car r)
          (loop (cdr r))
          (cdr r)))))

(define (apply-islisp fn args)
  (let loop ((current-fn fn)
             (current-args args))
    (cond
     ((generic? current-fn)
      (apply-generic current-fn current-args))
     ((primitive? current-fn)
      (apply (primitive-proc current-fn) current-args))
     ((closure? current-fn)
      (let ((params (closure-params current-fn))
            (body (closure-body current-fn))
            (fenv (closure-env current-fn))
            (fname (closure-name current-fn)))
        (let ((param-spec (parse-params params)))
        (let ((call-env (make-frame fenv)))
          (bind-params! call-env param-spec current-args)
          (let ((result
                 (if fname
                     (call/cc
                      (lambda (escape)
                        (dynamic-wind
                          (lambda ()
                            (set! *block-stack* (cons (cons fname escape) *block-stack*)))
                          (lambda ()
                            (eval-sequence* body call-env #t))
                          (lambda ()
                            (set! *block-stack* (cdr *block-stack*))))))
                     (eval-sequence* body call-env #t))))
            (if (tail-call? result)
                (loop (tail-call-fn result) (tail-call-args result))
                result))))))
     (else
      (error "Attempt to call non-function" current-fn)))))

(define (special-form? sym)
  (memq sym '(quote quasiquote if cond case and or loop while do dolist dotimes return-from catch throw go tagbody trace untrace lambda defpackage in-package defglobal defvar setq setf incf defun defmacro defgeneric defmethod progn block let let* with-open-file handler-case defclass)))

(define (extended-special-form? sym)
  (memq sym '(trace untrace)))

(define (eval-special form env tail?)
  (let ((op (car form))
        (args (cdr form)))
    (when (and (strict-profile?) (extended-special-form? op))
      (error "Special form is unavailable in strict profile" op))
    (case op
      ((quote)
       (if (= (length args) 1)
           (car args)
           (error "quote takes one argument" form)))
      ((quasiquote)
       (if (= (length args) 1)
           (quasiquote-eval (car args) env 1)
           (error "quasiquote takes one argument" form)))
      ((if)
       (if (or (= (length args) 2) (= (length args) 3))
           (let ((test (eval-islisp* (car args) env #f)))
             (if (truthy? test)
                 (eval-islisp* (cadr args) env tail?)
                 (if (= (length args) 3)
                     (eval-islisp* (caddr args) env tail?)
                     '())))
           (error "if takes 2 or 3 arguments" form)))
      ((cond)
       (let loop ((clauses args))
         (if (null? clauses)
             '()
             (let* ((clause (car clauses))
                    (test-form (and (pair? clause) (car clause)))
                    (body (and (pair? clause) (cdr clause))))
               (unless (and (list? clause) (not (null? clause)))
                 (error "invalid cond clause" clause))
               (let ((test-val (eval-islisp* test-form env #f)))
                 (if (truthy? test-val)
                   (if (null? body)
                       test-val
                       (eval-sequence* body env tail?))
                   (loop (cdr clauses))))))))
      ((case)
       (if (>= (length args) 1)
           (let ((key (eval-islisp* (car args) env #f))
                 (clauses (cdr args)))
             (let loop ((xs clauses))
               (if (null? xs)
                   '()
                   (let* ((clause (car xs))
                          (keys (and (pair? clause) (car clause)))
                          (body (and (pair? clause) (cdr clause))))
                     (unless (and (list? clause) (>= (length clause) 1))
                       (error "invalid case clause" clause))
                     (if (or (eq? keys 'otherwise) (eq? keys 't))
                         (if (null? body)
                             '()
                             (eval-sequence* body env tail?))
                         (let ((matched?
                                (if (list? keys)
                                    (any (lambda (k) (eqv? key k)) keys)
                                    (eqv? key keys))))
                           (if matched?
                               (if (null? body)
                                   '()
                                   (eval-sequence* body env tail?))
                               (loop (cdr xs)))))))))
           (error "case needs key and clauses" form)))
      ((and)
       (let loop ((rest args) (last #t))
         (if (null? rest)
             last
             (let ((value (eval-islisp* (car rest) env #f)))
               (if (truthy? value)
                   (loop (cdr rest) value)
                   value)))))
      ((or)
       (let loop ((rest args))
         (if (null? rest)
             '()
             (let ((value (eval-islisp* (car rest) env #f)))
               (if (truthy? value)
                   value
                   (loop (cdr rest)))))))
      ((loop)
       (eval-loop args env))
      ((while)
       (eval-while args env "while"))
      ((do)
       (eval-do args env tail?))
      ((dolist)
       (eval-dolist args env tail?))
      ((dotimes)
       (eval-dotimes args env tail?))
      ((defclass)
       (eval-defclass args env))
      ((with-open-file)
       (eval-with-open-file args env tail?))
      ((handler-case)
       (eval-handler-case args env tail?))
      ((return-from)
       (if (or (= (length args) 1) (= (length args) 2))
           (let* ((name (resolve-binding-symbol (car args)))
                  (value (if (= (length args) 2)
                             (eval-islisp* (cadr args) env #f)
                             '())))
             (unless (symbol? name)
               (error "return-from name must be a symbol" name))
             (let ((entry (assoc name *block-stack*)))
               (unless entry
                 (error "No enclosing block for return-from" name))
               ((cdr entry) value)))
           (error "return-from takes block-name and optional value" form)))
      ((catch)
       (eval-catch args env tail?))
      ((throw)
       (if (= (length args) 2)
           (let ((tag (eval-islisp* (car args) env #f))
                 (value (eval-islisp* (cadr args) env #f)))
             (throw-to-catch tag value))
           (error "throw takes tag and value" form)))
      ((go)
       (if (= (length args) 1)
           (let ((tag (car args)))
             (unless (or (symbol? tag) (integer? tag))
               (error "go tag must be a symbol or integer" tag))
             (raise (make-go-signal tag)))
           (error "go takes one tag argument" form)))
      ((tagbody)
       (eval-tagbody args env))
      ((trace)
       (if (null? args)
           (delete-duplicates (append (map car *trace-table*) *trace-macro-set*) eq?)
           (map (lambda (s)
                  (unless (symbol? s)
                    (error "trace arguments must be symbols" s))
                  (trace-one! env (resolve-binding-symbol s)))
                args)))
      ((untrace)
       (if (null? args)
           (untrace-all! env)
           (map (lambda (s)
                  (unless (symbol? s)
                    (error "untrace arguments must be symbols" s))
                  (untrace-one! env (resolve-binding-symbol s)))
                args)))
      ((lambda)
       (if (>= (length args) 2)
           (let ((params (car args))
                 (body (cdr args)))
             (validate-params params)
             (make-closure params body env #f))
           (error "lambda needs params and body" form)))
      ((defglobal)
       (if (= (length args) 2)
           (let ((sym (resolve-binding-symbol (car args)))
                 (val (eval-islisp* (cadr args) env #f)))
             (unless (symbol? sym)
               (error "defglobal needs a symbol" sym))
             (let ((global (global-frame env)))
               (frame-define! global sym val)
               sym))
           (error "defglobal takes symbol and expression" form)))
      ((defvar)
       (if (= (length args) 2)
           (let ((sym (resolve-binding-symbol (car args)))
                 (global (global-frame env)))
             (unless (symbol? sym)
               (error "defvar needs a symbol" sym))
             (unless (frame-bound? global sym)
               (frame-define! global sym (eval-islisp* (cadr args) env #f)))
             sym)
           (error "defvar takes symbol and expression" form)))
      ((setq)
       (if (= (length args) 2)
           (let* ((raw (car args))
                  (sym (if (frame-bound? env raw)
                           raw
                           (resolve-binding-symbol raw)))
                 (val (eval-islisp* (cadr args) env #f)))
             (unless (symbol? sym)
               (error "setq needs a symbol" sym))
             (frame-set! env sym val)
             val)
           (error "setq takes symbol and expression" form)))
      ((setf)
       (if (= (length args) 2)
           (let* ((place (car args))
                  (val (eval-islisp* (cadr args) env #f)))
             (cond
              ((symbol? place)
               (frame-set! env
                           (if (frame-bound? env place)
                               place
                               (resolve-binding-symbol place))
                           val)
               val)
              ((and (pair? place) (eq? (car place) 'car) (= (length place) 2))
               (let ((target (eval-islisp* (cadr place) env #f)))
                 (set-car! target val)
                 val))
              ((and (pair? place) (eq? (car place) 'cdr) (= (length place) 2))
               (let ((target (eval-islisp* (cadr place) env #f)))
                 (set-cdr! target val)
                 val))
              ((and (pair? place)
                    (or (eq? (car place) 'vector-ref) (eq? (car place) 'aref))
                    (= (length place) 3))
               (let ((target (eval-islisp* (cadr place) env #f))
                     (index (eval-islisp* (caddr place) env #f)))
                (unless (vector? target)
                  (error "setf aref/vector-ref target must be vector" target))
                (unless (and (integer? index) (>= index 0) (< index (vector-length target)))
                  (error "setf aref/vector-ref index out of range" index))
                (vector-set! target index val)
                val))
              ((and (pair? place) (eq? (car place) 'gethash) (or (= (length place) 3) (= (length place) 4)))
               (let ((key (eval-islisp* (cadr place) env #f))
                     (table (eval-islisp* (caddr place) env #f)))
                 (unless (hash-table? table)
                   (error "setf gethash table must be hash-table" table))
                 (hash-table-put! table key val)
                 val))
              ((and (pair? place) (eq? (car place) 'slot-value) (= (length place) 3))
               (let ((target (eval-islisp* (cadr place) env #f))
                     (slot (eval-islisp* (caddr place) env #f)))
                 (instance-slot-set! target slot val)))
              ((and (pair? place) (symbol? (car place)) (= (length place) 2))
               (let* ((raw-op (car place))
                      (op (if (frame-bound? env raw-op)
                              raw-op
                              (resolve-binding-symbol raw-op)))
                      (slot (accessor-slot-ref op)))
                 (if slot
                     (let ((target (eval-islisp* (cadr place) env #f)))
                       (instance-slot-set! target slot val))
                     (error "Unsupported setf place" place))))
              (else
               (error "Unsupported setf place" place))))
           (error "setf takes place and expression" form)))
      ((incf)
       (if (or (= (length args) 1) (= (length args) 2))
           (let* ((place (car args))
                  (delta (if (= (length args) 2)
                             (eval-islisp* (cadr args) env #f)
                             1)))
             (cond
              ((symbol? place)
               (let* ((sym (if (frame-bound? env place)
                               place
                               (resolve-binding-symbol place)))
                      (new (+ (frame-ref env sym) delta)))
                 (frame-set! env sym new)
                 new))
              ((and (pair? place) (eq? (car place) 'car) (= (length place) 2))
               (let* ((target (eval-islisp* (cadr place) env #f))
                      (new (+ (car target) delta)))
                 (set-car! target new)
                 new))
              ((and (pair? place) (eq? (car place) 'cdr) (= (length place) 2))
               (let* ((target (eval-islisp* (cadr place) env #f))
                      (new (+ (cdr target) delta)))
                 (set-cdr! target new)
                 new))
              (else
               (error "Unsupported incf place" place))))
           (error "incf takes place and optional delta" form)))
      ((defun)
       (if (>= (length args) 3)
           (let ((name (resolve-binding-symbol (car args)))
                 (params (cadr args))
                 (body (cddr args)))
             (unless (symbol? name)
               (error "defun name must be symbol" name))
             (validate-params params)
             (let ((fun (make-closure params body env name)))
               (frame-define! (global-frame env) name fun)
               name))
           (error "defun needs name, params and body" form)))
      ((defmacro)
       (if (>= (length args) 3)
           (let ((name (resolve-binding-symbol (car args)))
                 (params (cadr args))
                 (body (cddr args)))
             (unless (symbol? name)
               (error "defmacro name must be symbol" name))
             (validate-params params)
             (let ((m (make-macro params body env name)))
               (frame-define! (global-frame env) name m)
               name))
           (error "defmacro needs name, params and body" form)))
      ((defgeneric)
       (eval-defgeneric args env))
      ((defmethod)
       (eval-defmethod args env))
      ((defpackage)
       (eval-defpackage args))
      ((in-package)
       (eval-in-package args))
      ((progn)
       (eval-sequence* args env tail?))
      ((block)
       (eval-block args env tail?))
      ((let)
       (if (>= (length args) 2)
           (let ((bindings (car args))
                 (body (cdr args)))
             (unless (list? bindings)
               (error "let bindings must be a list" bindings))
             (let ((let-env (make-frame env)))
               (for-each
               (lambda (b)
                  (unless (and (list? b) (= (length b) 2) (symbol? (car b)))
                    (error "invalid let binding" b))
                  (frame-define! let-env (car b) (eval-islisp* (cadr b) env #f)))
               bindings)
               (eval-sequence* body let-env tail?)))
           (error "let needs bindings and body" form)))
      ((let*)
       (if (>= (length args) 2)
           (let ((bindings (car args))
                 (body (cdr args)))
             (unless (list? bindings)
               (error "let* bindings must be a list" bindings))
             (let ((let-env (make-frame env)))
               (for-each
                (lambda (b)
                  (unless (and (list? b) (= (length b) 2) (symbol? (car b)))
                    (error "invalid let* binding" b))
                  (frame-define! let-env (car b) (eval-islisp* (cadr b) let-env #f)))
                bindings)
               (eval-sequence* body let-env tail?)))
           (error "let* needs bindings and body" form)))
      (else
       (error "Unknown special form" op)))))

(define (eval-islisp* form env tail?)
  (cond
   ((or (number? form) (string? form) (char? form) (boolean? form)) form)
   ((symbol? form)
    (if (keyword-symbol? form)
        form
        (let ((pair (frame-find-pair env form)))
          (if pair
              (cdr pair)
              (frame-ref env (resolve-symbol-in-package form))))))
   ((pair? form)
    (let ((op (car form))
          (args (cdr form)))
      (if (and (symbol? op) (special-form? op))
          (eval-special form env tail?)
          (let* ((op-sym (and (symbol? op)
                              (if (frame-bound? env op)
                                  op
                                  (resolve-symbol-in-package op))))
                 (op-pair (and op-sym (frame-find-pair env op-sym)))
                 (op-val (and op-pair (cdr op-pair))))
            (if (and op-pair (macro? op-val))
                (eval-islisp* (if (macro-traced? op-sym)
                                  (trace-apply-macro op-sym op-val args)
                                  (apply-macro op-val args))
                             env
                             tail?)
                (let ((fn (eval-islisp* op env #f))
                      (vals (eval-list args env)))
                  (if tail?
                      (make-tail-call fn vals)
                      (apply-islisp fn vals))))))))
   ((null? form) '())
   (else
    form)))

(define (eval-islisp form env)
  (force-value (eval-islisp* form env #t)))

(define (install-primitives! env)
  (define (def name proc)
    (let ((sym (resolve-binding-symbol name)))
      (frame-define! env sym (make-primitive name proc))
      (when *current-package*
        (package-export! *current-package* name))))
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

  (def '+ +)
  (def '- -)
  (def '* *)
  (def '/ /)
  (def 'mod modulo)
  (def 'floor floor)
  (def 'ceiling ceiling)
  (def 'truncate truncate)
  (def 'round round)
  (def '= =)
  (def '< <)
  (def '> >)
  (def '<= <=)
  (def '>= >=)
  (def 'cons cons)
  (def 'car car)
  (def 'cdr cdr)
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
    (lambda (v i)
      (unless (vector? v)
        (error "aref target must be vector" v))
      (unless (and (integer? i) (>= i 0) (< i (vector-length v)))
        (error "aref index out of range" i))
      (vector-ref v i)))
  (def 'vector-set!
    (lambda (v i x)
      (unless (vector? v)
        (error "vector-set! target must be vector" v))
      (unless (and (integer? i) (>= i 0) (< i (vector-length v)))
        (error "vector-set! index out of range" i))
      (vector-set! v i x)
      x))
  (def 'vectorp vector?)
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
  (def 'mapcar
    (lambda (f xs)
      (unless (list? xs)
        (error "mapcar needs a list as second argument" xs))
      (map (lambda (x) (apply-islisp f (list x))) xs)))
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
  (def 'eq eq?)
  (def 'eql eqv?)
  (def 'equal equal?)
  (def 'null (lambda (x) (null? x)))
  (def 'atom (lambda (x) (not (pair? x))))
  (def 'numberp number?)
  (def 'zerop zero?)
  (def 'plusp
    (lambda (x)
      (> x 0)))
  (def 'minusp
    (lambda (x)
      (< x 0)))
  (def 'evenp
    (lambda (x)
      (unless (integer? x)
        (error "evenp needs an integer" x))
      (zero? (modulo x 2))))
  (def 'oddp
    (lambda (x)
      (unless (integer? x)
        (error "oddp needs an integer" x))
      (not (zero? (modulo x 2)))))
  (def 'symbolp symbol?)
  (def 'listp list?)
  (def 'stringp string?)
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
      (string=? a b)))
  (def 'string/=
    (lambda (a b)
      (ensure-string a "string/=")
      (ensure-string b "string/=")
      (if (string=? a b) #f #t)))
  (def 'string<
    (lambda (a b)
      (ensure-string a "string<")
      (ensure-string b "string<")
      (if (string<? a b) #t #f)))
  (def 'string<=
    (lambda (a b)
      (ensure-string a "string<=")
      (ensure-string b "string<=")
      (if (string<=? a b) #t #f)))
  (def 'string>
    (lambda (a b)
      (ensure-string a "string>")
      (ensure-string b "string>")
      (if (string>? a b) #t #f)))
  (def 'string>=
    (lambda (a b)
      (ensure-string a "string>=")
      (ensure-string b "string>=")
      (if (string>=? a b) #t #f)))
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
  (def 'length isl-length)
  (def 'print
    (lambda (x)
      (write x)
      (newline)
      x))
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
  (def 'read
    (lambda args
      (cond
       ((null? args)
        (read))
       ((= (length args) 1)
        (read (car args)))
       (else
        (error "read takes zero or one stream argument" args)))))
  (def 'read-line
    (lambda args
      (cond
       ((= (length args) 1)
        (let ((line (read-line (car args))))
          (if (eof-object? line)
              (error "read-line reached EOF")
              line)))
       ((= (length args) 2)
        (let ((line (read-line (car args)))
              (eof-error-p (cadr args)))
          (if (eof-object? line)
              (if (truthy? eof-error-p)
                  (error "read-line reached EOF")
                  '())
              line)))
       (else
        (error "read-line takes stream and optional eof-error-p" args)))))
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
      (sql-output->rows (postgres-run (postgres-connection-info db) sql) #\x1f)))
  (def 'postgres-query-one
    (lambda (db sql)
      (unless (postgres-connection? db)
        (error "postgres-query-one needs postgres db object" db))
      (let ((rows (sql-output->rows (postgres-run (postgres-connection-info db) sql) #\x1f)))
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
  (def 'format
    (lambda args
      (cond
       ((null? args)
        (error "format needs at least a control string"))
       ;; String destination: append formatted output and return new string.
       ((and (>= (length args) 2)
             (string? (car args))
             (string? (cadr args)))
        (string-append (car args)
                       (render-format (cadr args) (cddr args))))
       ;; Backward-compat shorthand: (format "fmt" ...)
       ((string? (car args))
        (let ((out (render-format (car args) (cdr args))))
          (display out)
          '()))
       ((< (length args) 2)
        (error "format needs destination and control string"))
       ((eq? (car args) #t)
        (let ((out (render-format (cadr args) (cddr args))))
          (display out)
          '()))
       ((or (null? (car args)) (eq? (car args) #f))
        (render-format (cadr args) (cddr args)))
       ((output-port? (car args))
        (let ((out (render-format (cadr args) (cddr args))))
          (display out (car args))
          '()))
       (else
        (error "unsupported format destination" (car args))))))
  (def 'not
    (lambda (x)
      (if (truthy? x) #f #t)))
  (def 'apply
    (lambda (f xs)
      (apply-islisp f xs)))
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
          obj))))
  (def 'class-of
    (lambda (obj)
      (cond
       ((instance? obj) (instance-class obj))
       ((class? obj) obj)
       (else
        (error "class-of target must be instance or class" obj)))))
  (def 'instancep
    (lambda (obj)
      (instance? obj)))
  (def 'slot-value
    (lambda (obj slot)
      (instance-slot-ref obj slot)))
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
      (apply exit args))))

(define *extended-primitive-symbols*
  '(debug break getenv setenv system
    sqlite-open sqlite-db-p sqlite-close sqlite-exec sqlite-query sqlite-query-one
    postgres-open postgres-db-p postgres-close postgres-exec postgres-query postgres-query-one
    mysql-open mysql-db-p mysql-close mysql-exec mysql-query mysql-query-one
    tcp-connect tcp-listen tcp-listener-p tcp-accept tcp-listener-close
    tcp-connection-p tcp-send tcp-send-line tcp-receive-line tcp-receive-char
    tcp-send-byte tcp-receive-byte tcp-flush tcp-close
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
      (when (strict-profile?)
        (disable-extended-primitives! env))
      ;; Lightweight CFFI-compatible facade (extended profile only).
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
      (set! *current-package* user))
    env)))

(define (read-all port)
  (let loop ((acc '()))
    (let ((x (read port)))
      (if (eof-object? x)
          (reverse acc)
          (loop (cons x acc))))))

(define (repl env)
  (define (write-result x)
    (cond
     ((closure? x)
      (let ((name (closure-name x)))
        (if name
            (begin
              (display "#<function ")
              (display name)
              (display ">"))
            (display "#<closure>"))))
     ((primitive? x)
      (begin
        (display "#<primitive ")
        (display (primitive-name x))
        (display ">")))
     ((macro? x)
      (let ((name (macro-name x)))
        (if name
            (begin
              (display "#<macro ")
              (display name)
              (display ">"))
            (display "#<macro>"))))
     (else
      (write x))))
  (display "ISLISP> ")
  (flush)
  (let ((x (read)))
    (if (eof-object? x)
        (begin
          (newline)
          'bye)
        (begin
          (guard (e (else
                     (display "Error: ")
                     (write e)
                     (newline)
                     (when *debug-mode*
                       (break-loop env (write-to-string e)))))
            (let ((result (eval-islisp x env)))
              (write-result result)
              (newline)))
          (repl env)))))
