;; Minimal ISLISP core evaluator on Gauche 0.9.15.
;; The implementation keeps dependencies close to R7RS-level primitives.
(define-module isl.core
  (use srfi-1)
  (use file.util)
  (use gauche.process)
  (use gauche.net)
  (use gauche.threads)
  (use gauche.fcntl)
  (use rfc.base64)
  (use rfc.uuid)
  (use rfc.tls)
  (use rfc.http)
  (use srfi.19)
  (use gauche.vport)
  (export make-initial-env eval-islisp apply-islisp repl read-all
          ;; Phase 5: condition infrastructure (used by compiler runtime)
          make-isl-condition isl-condition?
          isl-condition-class isl-condition-message
          isl-condition-continuable? isl-condition-irritants
          *isl-handler-stack* isl-signal-condition isl-subclassp?
          ;; Phase 6: dynamic variables + strict sanitization
          *dynamic-var-table* resolve-binding-symbol sanitize-for-strict
          strict-profile?
          ;; Phase 7-A: next-method support
          *next-methods* *current-method-args*
          ;; Phase 8: format engine (used by compiler runtime)
          render-format display-to-string write-to-string
          ;; macrolet support (used by compiler frontend)
          make-frame frame-define! make-macro macroexpand*))

(select-module isl.core)

(define gauche-http-get http-get)
(define gauche-http-post http-post)
(define gauche-http-head http-head)
(define gauche-http-string-receiver http-string-receiver)
(define gauche-make-directory* make-directory*)
(define gauche-directory-list directory-list)
(define gauche-copy-file copy-file)
(define gauche-move-file move-file)
(define gauche-file-size file-size)
(define gauche-file-is-readable? file-is-readable?)
(define gauche-file-is-executable? file-is-executable?)
(define gauche-file-is-symlink? file-is-symlink?)
(define gauche-file-type file-type)
(define gauche-base64-encode-string base64-encode-string)
(define gauche-uuid4 uuid4)
(define gauche-uuid->string uuid->string)
(define gauche-make-time make-time)
(define gauche-time-utc time-utc)
(define gauche-current-time current-time)
(define gauche-time-utc->date time-utc->date)
(define gauche-date->string date->string)
(define gauche-string->date string->date)
(define gauche-date->time-utc date->time-utc)
(define gauche-time-second time-second)
;; Keep references to Gauche TLS procedures before we define ISL primitives
;; with similar names.
(define gauche-make-tls make-tls)
(define gauche-tls-bind tls-bind)
(define gauche-tls-accept tls-accept)
(define gauche-tls-close tls-close)
(define gauche-tls-load-certificate tls-load-certificate)
(define gauche-tls-load-private-key tls-load-private-key)
(define gauche-connection-input-port connection-input-port)
(define gauche-connection-output-port connection-output-port)
(define gauche-connection-close connection-close)
(define gauche-make-thread make-thread)
(define gauche-thread-start! thread-start!)
(define gauche-thread-join! thread-join!)
(define gauche-thread? thread?)
(define gauche-make-mutex make-mutex)
(define gauche-mutex-lock! mutex-lock!)
(define gauche-mutex-unlock! mutex-unlock!)
(define gauche-mutex? mutex?)

(define (truthy? v)
  (and (not (eq? v #f))
       (not (null? v))))

;; ---- Phase 6-A: Strict-mode source form sanitization ----
;; In ISLISP strict mode, #f in source code means nil (= '()).
;; Apply this recursively to forms before evaluation.
(define (sanitize-for-strict form)
  (cond
   ((eq? form #f) '())           ; #f → nil
   ((eq? form #t) #t)            ; #t stays as #t (= t)
   ((pair? form)
    (cons (sanitize-for-strict (car form))
          (sanitize-for-strict (cdr form))))
   ((vector? form)
    (vector-map sanitize-for-strict form))
   (else form)))

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

(define (frame-find-local-pair frame sym)
  (assoc sym (frame-bindings frame)))

(define (frame-define! frame sym val)
  (let ((pair (frame-find-local-pair frame sym)))
    (if pair
        (set-cdr! pair val)
        (set-frame-bindings! frame
                             (cons (cons sym val)
                                   (frame-bindings frame))))))

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

(define (make-tls-connection tls in out)
  (vector 'tls-connection tls in out))

(define (tls-connection? obj)
  (and (vector? obj)
       (= (vector-length obj) 4)
       (eq? (vector-ref obj 0) 'tls-connection)))

(define (tls-connection-raw conn)
  (vector-ref conn 1))

(define (tls-connection-input conn)
  (vector-ref conn 2))

(define (tls-connection-output conn)
  (vector-ref conn 3))

(define (make-tls-listener tls)
  (vector 'tls-listener tls))

(define (tls-listener? obj)
  (and (vector? obj)
       (= (vector-length obj) 2)
       (eq? (vector-ref obj 0) 'tls-listener)))

(define (tls-listener-raw listener)
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
  (define (find-colon path start)
    (let loop ((i start))
      (cond
       ((>= i (string-length path)) #f)
       ((char=? (string-ref path i) #\:) i)
       (else (loop (+ i 1))))))
  (define (split-path path)
    (let loop ((start 0) (acc '()))
      (if (> start (string-length path))
          (reverse acc)
          (let ((next (find-colon path start)))
            (if next
                (let ((part (substring path start next)))
                  (loop (+ next 1)
                        (cons (if (string=? part "") "." part) acc)))
                (reverse
                 (cons (let ((part (substring path start (string-length path))))
                         (if (string=? part "") "." part))
                       acc)))))))
  (define (join-path dir leaf)
    (cond
     ((or (string=? dir "") (string=? dir ".")) leaf)
     ((char=? (string-ref dir (- (string-length dir) 1)) #\/)
      (string-append dir leaf))
     (else
      (string-append dir "/" leaf))))
  (and (string? cmd)
       (> (string-length cmd) 0)
       (if (string-scan cmd #\/)
           (and (file-exists? cmd)
                (gauche-file-is-executable? cmd))
           (let ((path (or (sys-getenv "PATH") "")))
             (let loop ((dirs (split-path path)))
               (and (pair? dirs)
                    (or (let ((candidate (join-path (car dirs) cmd)))
                          (and (file-exists? candidate)
                               (gauche-file-is-executable? candidate)))
                        (loop (cdr dirs)))))))))

(define (parse-octal-string s)
  (let loop ((i 0) (n 0))
    (if (= i (string-length s))
        n
        (let ((ch (string-ref s i)))
          (if (or (char<? ch #\0) (char>? ch #\7))
              (error "invalid octal string" s)
              (loop (+ i 1)
                    (+ (* n 8)
                       (- (char->integer ch) (char->integer #\0)))))))))

(define (http-date-format)
  "~a, ~d ~b ~Y ~H:~M:~S GMT")

(define (http-date->utc-seconds s)
  (let* ((gmt-suffix " GMT")
         (len (string-length s))
         (suffix-len (string-length gmt-suffix))
         (normalized
          (if (and (>= len suffix-len)
                   (string=? (substring s (- len suffix-len) len) gmt-suffix))
              (string-append (substring s 0 (- len suffix-len)) " +0000")
              s))
         (parsed (gauche-string->date normalized "~a, ~d ~b ~Y ~H:~M:~S ~z")))
    (gauche-time-second (gauche-date->time-utc parsed))))

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
  ;; Use ASCII Record Separator (0x1e) so text fields can contain newlines.
  (let* ((argv (list "psql" "-X" "-A" "-t"
                     "-R" (string #\x1e)
                     "-F" (string #\x1f)
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

(define (sql-output->rows out separator . maybe-row-separator)
  (let* ((row-separator (if (null? maybe-row-separator) #\newline (car maybe-row-separator)))
         (lines (string-split-char out row-separator)))
    (let loop ((rest lines) (acc '()))
      (if (null? rest)
          (reverse acc)
          (let ((line (car rest)))
            (if (string=? line "")
                (loop (cdr rest) acc)
                (loop (cdr rest) (cons (string-split-char line separator) acc))))))))

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
  (and (vector? obj) (= (vector-length obj) 5) (eq? (vector-ref obj 0) 'method)))

(define (make-method qualifier specializers params proc)
  (vector 'method qualifier specializers params proc))

(define (method-qualifier m) (vector-ref m 1))
(define (method-specializers m) (vector-ref m 2))
(define (method-params m) (vector-ref m 3))
(define (method-proc m) (vector-ref m 4))

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
  (cond
   ((symbol? params)
    ;; Variadic shorthand: (defun f args ...)
    (list '() '() params))
   ((list? params)
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
   ((pair? params)
    ;; Dotted variadic form: (a b . rest)
    (let dotted-loop ((xs params) (required '()))
      (cond
       ((pair? xs)
        (let ((x (car xs)))
          (unless (symbol? x)
            (error "Required parameter must be a symbol" x))
          (when (memq x '(&optional &rest &body))
            (error "Lambda-list keywords are not allowed in dotted parameter prefix" params))
          (dotted-loop (cdr xs) (cons x required))))
       ((symbol? xs)
        (list (reverse required) '() xs))
       (else
        (error "Invalid dotted parameter list tail" xs)))))
   (else
    (error "Parameter list must be a list or symbol" params))))

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

(define (call-with-stack-entry get-stack set-stack entry thunk)
  (dynamic-wind
    (lambda ()
      (set-stack (cons entry (get-stack))))
    thunk
    (lambda ()
      (set-stack (cdr (get-stack))))))
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

;; Returns the isl class object for a native (non-instance) Gauche value.
;; Returns #f if not found (class table not yet initialized).
(define (native-value->class-obj val)
  (define (ctref name)
    ;; Try resolved name first, then bare name as fallback
    (or (class-table-ref (resolve-binding-symbol name))
        (class-table-ref name)))
  (cond
   ((and (integer? val) (exact? val)) (ctref '<integer>))
   ((and (number? val) (inexact? val)) (ctref '<float>))
   ((number? val)   (ctref '<number>))
   ((string? val)   (ctref '<string>))
   ((symbol? val)   (ctref '<symbol>))
   ((char? val)     (ctref '<character>))
   ((pair? val)     (ctref '<cons>))
   ((null? val)     (ctref '<null>))
   ((isl-array? val) (ctref '<general-array*>))
   ((vector? val)   (ctref '<general-vector>))
   ((or (closure? val) (primitive? val)) (ctref '<function>))
   ((or (input-port? val) (output-port? val)) (ctref '<stream>))
   (else            (ctref '<object>))))

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
                    (let ((arg-class (if (instance? arg)
                                         (instance-class arg)
                                         (native-value->class-obj arg))))
                      (if arg-class
                          (let ((d (class-distance arg-class spec)))
                            (if d
                                (loop (cdr ss) (cdr as) (cons d acc))
                                #f))
                          #f))
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
  (let* ((all-methods (generic-methods generic))
         (app-around  (sort-applicable
                       (filter (lambda (m) (eq? (method-qualifier m) ':around)) all-methods) args))
         (app-before  (sort-applicable
                       (filter (lambda (m) (eq? (method-qualifier m) ':before)) all-methods) args))
         (app-primary (sort-applicable
                       (filter (lambda (m) (not (method-qualifier m))) all-methods) args))
         (app-after   (reverse (sort-applicable
                                (filter (lambda (m) (eq? (method-qualifier m) ':after)) all-methods) args))))
    (when (null? app-primary)
      ;; ISLISP §21: no applicable method signals <program-error>
      (isl-signal-condition
       (make-isl-condition (resolve-binding-symbol '<program-error>)
         (string-append "No applicable primary method for generic function: "
                        (symbol->string (generic-name generic)))
         #f (list (generic-name generic) args))))
    (letrec ((call-effective-primary
              (lambda (eff-args)
                (for-each (lambda (m)
                            (parameterize ((*next-methods* '()) (*current-method-args* eff-args))
                              (apply-islisp (method-proc m) eff-args)))
                          app-before)
                (let ((result (invoke-chain (map method-proc app-primary) #f eff-args)))
                  (for-each (lambda (m)
                              (parameterize ((*next-methods* '()) (*current-method-args* eff-args))
                                (apply-islisp (method-proc m) eff-args)))
                            app-after)
                  result))))
      (if (null? app-around)
          (call-effective-primary args)
          (invoke-chain (map method-proc app-around) call-effective-primary args)))))

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
                         ;; ~nR: print argument in radix n (e.g. ~2R, ~16R)
                         ((char-numeric? d)
                          (let scan ((j (+ i 1)) (s ""))
                            (if (and (< j (string-length fmt))
                                     (char-numeric? (string-ref fmt j)))
                                (scan (+ j 1) (string-append s (string (string-ref fmt j))))
                                (if (and (< j (string-length fmt))
                                         (char=? (char-upcase (string-ref fmt j)) #\R))
                                    (let* ((radix (string->number s))
                                           (_ (unless (and radix (>= radix 2) (<= radix 36))
                                                (error "format ~nR: invalid radix" s))))
                                      (display (num-arg->string rest radix
                                                (string-append "format ~" s "R")) p)
                                      (loop (+ j 1) (cdr rest)))
                                    (error "unsupported format directive with numeric prefix" s)))))
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
                         ((char=? u #\I)
                          ;; ~i — ignore (consume) one argument
                          (when (null? rest)
                            (error "too few arguments for format ~I" fmt))
                          (loop (+ i 2) (cdr rest)))
                         ((char=? u #\N)
                          ;; ~n or ~N — fresh line (same as ~% for simplicity)
                          (newline p)
                          (loop (+ i 2) rest))
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
    ;; ISLISP §17.4: out-of-range start returns nil (not error)
    (if (> start n)
        '()
        (if (= m 0)
            start
            (let loop ((i start))
              (cond
               ((> (+ i m) n) '())
               ((string=? (substring haystack i (+ i m)) needle) i)
               (else (loop (+ i 1)))))))))

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

(define (eval-loop-body body env)
  (eval-sequence body env))

(define (eval-while args env who)
  (if (>= (length args) 1)
      (let ((test-form (car args))
            (body (cdr args)))
        (let loop ()
          (if (truthy? (eval-islisp test-form env))
              (begin
                (eval-loop-body body env)
                (loop))
              '())))
      (error who "needs test expression and optional body" args)))

(define (parse-loop-for-clause xs args)
  (unless (and (pair? (cdr xs))
               (pair? (cddr xs))
               (pair? (cdddr xs))
               (pair? (cddddr xs))
               (pair? (cddddr (cdr xs)))
               (symbol? (cadr xs))
               (eq? (caddr xs) 'from)
               (eq? (car (cddddr xs)) 'to))
    (error "invalid loop for clause" xs))
  (list (cadr xs)
        (cadddr xs)
        (cadr (cddddr xs))
        (cddr (cddddr xs))))

(define (parse-loop-until-clause xs args)
  (unless (pair? (cdr xs))
    (error "loop until clause needs a test form" xs))
  (list (cadr xs) (cddr xs)))

;; ISLISP standard (loop body*) — infinite loop; escaped by return-from/block.
;; Extended CL-style: (loop for ... from ... to ... [until ...] [do ...])
(define (eval-loop args env)
  (if (or (null? args)
          (not (memq (car args) '(for until do))))
      ;; ISLISP standard: infinite loop form
      (let loop-inf ()
        (eval-sequence args env)
        (loop-inf))
      ;; Extended CL-style loop
      (eval-loop-extended args env)))

(define (eval-loop-extended args env)
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
        (let ((parsed (parse-loop-for-clause xs args)))
          (set! for-var (list-ref parsed 0))
          (set! start-form (list-ref parsed 1))
          (set! end-form (list-ref parsed 2))
          (parse (list-ref parsed 3))))
       ((eq? (car xs) 'until)
        (when until-form
          (error "loop until clause appears multiple times" args))
        (let ((parsed (parse-loop-until-clause xs args)))
          (set! until-form (car parsed))
          (parse (cadr parsed))))
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
                          (eval-loop-body body loop-env)
                          (frame-set! loop-env for-var (+ i 1))
                          (loop-iter)))))))
          (let loop-iter ()
            (if (and until-form (truthy? (eval-islisp until-form loop-env)))
                '()
                (begin
                  (eval-loop-body body loop-env)
                  (loop-iter))))))))

(define (tagbody-label? item)
  (or (symbol? item) (integer? item)))

(define (collect-tagbody-labels items)
  (let collect ((rest items) (idx 0) (acc '()))
    (if (null? rest)
        (reverse acc)
        (let ((item (car rest)))
          (if (tagbody-label? item)
              (if (assoc item acc)
                  (error "duplicate tagbody label" item)
                  (collect (cdr rest) (+ idx 1) (cons (cons item idx) acc)))
              (collect (cdr rest) (+ idx 1) acc))))))

(define (eval-tagbody-step item labels env next-index)
  (if (tagbody-label? item)
      next-index
      (guard (e
              ((go-signal? e)
               (let ((entry (assoc (go-signal-tag e) labels)))
                 (if entry
                     (cdr entry)
                     (raise e))))
              (else
               (raise e)))
        (force-value (eval-islisp* item env #f))
        next-index)))

(define (eval-tagbody args env)
  (let ((items args)
        (labels (collect-tagbody-labels args))
        (len (length args)))
    (let loop ((i 0))
      (if (>= i len)
          '()
          (loop (eval-tagbody-step (list-ref items i)
                                   labels
                                   env
                                   (+ i 1)))))))

(define (eval-block args env tail?)
  (if (>= (length args) 1)
      (let ((name (resolve-binding-symbol (car args)))
            (body (cdr args)))
        (unless (symbol? name)
          (error "block name must be a symbol" name))
        (call/cc
         (lambda (escape)
           (call-with-stack-entry
            (lambda () *block-stack*)
            (lambda (v) (set! *block-stack* v))
            (cons name escape)
            (lambda ()
              (eval-sequence* body env tail?))))))
      (error "block needs name and optional body" args)))

(define (eval-catch args env tail?)
  (if (>= (length args) 1)
      (let ((tag (eval-islisp* (car args) env #f))
            (body (cdr args)))
        (call/cc
         (lambda (escape)
           (call-with-stack-entry
            (lambda () *catch-stack*)
            (lambda (v) (set! *catch-stack* v))
            (cons tag escape)
            (lambda ()
              ;; Keep non-local exits inside catch dynamic extent even for tail calls.
              (force-value (eval-sequence* body env tail?)))))))
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

(define (eval-optional-result-form result-form env tail?)
  (if result-form
      (if tail?
          (eval-islisp* result-form env #t)
          (eval-islisp result-form env))
      '()))

(define (eval-optional-result-forms result-forms env tail?)
  (if (null? result-forms)
      '()
      (if tail?
          (eval-sequence* result-forms env #t)
          (eval-sequence result-forms env))))

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
                (eval-optional-result-forms result-forms do-env tail?)
                (begin
                  (eval-loop-body body do-env)
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
             (eval-loop-body body loop-env))
           values)
          (frame-set! loop-env var '())
          (eval-optional-result-form result-form loop-env tail?)))
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
                  (eval-loop-body body loop-env)
                  (loop (+ i 1)))
                (begin
                  (frame-set! loop-env var count)
                  (eval-optional-result-form result-form loop-env tail?))))))
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
           ;; Phase 7-D: if no explicit supers, default to <standard-object>
           (supers
            (if (and (null? supers)
                     (not (eq? name (resolve-binding-symbol '<standard-object>)))
                     (not (eq? name (resolve-binding-symbol '<built-in-class>))))
                (let ((so (class-table-ref (resolve-binding-symbol '<standard-object>))))
                  (if so (list so) supers))
                supers))
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
                     (method (make-method #f (list class-obj) '(obj) method-proc)))
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
                     (method (make-method #f (list class-obj #f) '(obj value) method-proc)))
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

;; ---- Phase 7-C: (class <name>) special form ----
(define (eval-class-form args env)
  (unless (= (length args) 1)
    (error "class needs exactly one class-name" args))
  (let ((name (resolve-binding-symbol (car args))))
    (let ((entry (class-table-ref name)))
      (unless entry (error "class: undefined class" name))
      entry)))

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
         (has-qualifier? (and (>= (length args) 4)
                              (keyword-symbol? (cadr args))
                              (memq (cadr args) '(:before :after :around))))
         (qualifier (if has-qualifier? (cadr args) #f))
         (rest (if has-qualifier? (cddr args) (cdr args)))
         (raw-params (car rest))
         (body (cdr rest)))
    (unless (symbol? name)
      (error "defmethod name must be symbol" name))
    (let* ((parsed (parse-method-params raw-params env))
           (plain-params (car parsed))
           (specializers (cadr parsed))
           (method-proc (make-closure plain-params body env name))
           (generic (ensure-generic-function! name env plain-params))
           (method (make-method qualifier specializers plain-params method-proc)))
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
  (let ((raw-tag (car clause))
        (rest (cdr clause))
        (var #f))
    (unless (symbol? raw-tag)
      (error "handler-case clause tag must be symbol" raw-tag))
    (when (and (pair? rest) (list? (car rest)))
      (let ((vs (car rest)))
        (unless (or (null? vs)
                    (and (= (length vs) 1) (symbol? (car vs))))
          (error "handler-case variable list must be () or (var)" vs))
        (set! var (and (pair? vs) (car vs)))
        (set! rest (cdr rest))))
    ;; Resolve tag to a fully-qualified symbol so class hierarchy lookup works.
    (let ((tag (resolve-binding-symbol raw-tag)))
      (list tag var rest))))

;; ---- 5-F: handler-case tag matching with class hierarchy ----
;; tag-sym is a fully-resolved symbol (or raw bare name for legacy catches).
;; Normalise a handler-case tag symbol to all plausible class-name forms so
;; isl-subclassp? can match regardless of package prefix or angle-bracket style.
(define (handler-case-tag->class-syms tag-sym)
  (let* ((bare   (symbol-base-name tag-sym))  ; strip package prefix
         (angled (if (and (> (string-length bare) 0)
                          (char=? #\< (string-ref bare 0)))
                     bare
                     (string-append "<" bare ">")))  ; add <> if missing
         (plain  (if (and (> (string-length bare) 0)
                          (char=? #\< (string-ref bare 0)))
                     (substring bare 1 (- (string-length bare) 1))
                     bare)))
    (list (string->symbol angled)
          (resolve-binding-symbol (string->symbol angled))
          tag-sym)))

(define (handler-case-tag-match? tag-sym cond-obj)
  (cond
   ;; Catch-all forms
   ((or (eq? tag-sym 't) (eq? tag-sym 'otherwise)) #t)
   ;; isl-condition: check class hierarchy using isl-subclassp?
   ;; Try multiple normalised forms of the tag to handle bare `error` vs `<error>` etc.
   ((isl-condition? cond-obj)
    (let ((cond-class (isl-condition-class cond-obj))
          (candidates (handler-case-tag->class-syms tag-sym)))
      (any (lambda (super) (isl-subclassp? cond-class super)) candidates)))
   ;; Legacy Gauche native conditions (backward compat)
   (else
    (let ((bare (symbol-base-name tag-sym)))
      (or (string=? bare "error")
          (string=? bare "<error>")
          (string=? bare "condition")
          (string=? bare "<condition>")
          (string=? bare "serious-condition")
          (string=? bare "<serious-condition>"))))))

(define (eval-handler-case args env tail?)
  (unless (>= (length args) 1)
    (error "handler-case needs a protected form and clauses" args))
  (let ((protected (car args))
        (clauses (map parse-handler-case-clause (cdr args))))
    (guard (e
            ;; Re-raise go-signals (used by tagbody/go).
            ((go-signal? e) (raise e))
            (else
             (let loop ((cs clauses))
               (if (null? cs)
                   (raise e)
                   (let* ((c (car cs))
                          (tag (car c))
                          (var (cadr c))
                          (body (caddr c)))
                     (if (handler-case-tag-match? tag e)
                         (let ((henv (make-frame env)))
                           (when var
                             (frame-define! henv var e))
                           (eval-sequence* body henv tail?))
                         (loop (cdr cs))))))))
      ;; Keep exceptions from tail calls within handler-case dynamic extent.
      (force-value (eval-islisp* protected env tail?)))))

;; ---- 5-E: with-handler special form ----
(define (eval-with-handler args env tail?)
  (unless (= (length args) 2)
    (error "with-handler needs handler-form and protected-form" args))
  (let* ((handler-form   (car args))
         (protected-form (cadr args))
         (handler-fn     (eval-islisp* handler-form env #f)))
    (parameterize ((*isl-handler-stack*
                    (cons (lambda (c) (apply-islisp handler-fn (list c)))
                          (*isl-handler-stack*))))
      ;; force-value keeps execution inside the parameterize scope so that
      ;; any isl-signal-condition calls during tail evaluation stay within the handler.
      (force-value (eval-islisp* protected-form env tail?)))))

(define (parse-let-binding binding who)
  (unless (and (list? binding) (= (length binding) 2))
    (error who binding))
  (let ((name (resolve-binding-symbol (car binding))))
    (list name (cadr binding))))

(define (eval-let-bindings bindings env sequential?)
  (unless (list? bindings)
    (error (if sequential?
               "let* bindings must be a list"
               "let bindings must be a list")
           bindings))
  (let ((let-env (make-frame env)))
    (for-each
     (lambda (binding)
       (let* ((parsed (parse-let-binding binding
                                         (if sequential?
                                             "invalid let* binding"
                                             "invalid let binding")))
              (name (car parsed))
              (init-form (cadr parsed))
              (value (eval-islisp* init-form (if sequential? let-env env) #f)))
         (frame-define! let-env name value)))
     bindings)
    let-env))

(define (eval-pair-place-target place env who)
  (let ((target (eval-islisp* (cadr place) env #f)))
    (unless (pair? target)
      (error who "target must be pair" target))
    target))

(define (eval-setf-pair-place place env val)
  (let ((target (eval-pair-place-target place
                                        env
                                        (if (eq? (car place) 'car)
                                            "setf car"
                                            "setf cdr"))))
    (if (eq? (car place) 'car)
        (set-car! target val)
        (set-cdr! target val))
    val))

(define (eval-incf-pair-place place env delta)
  (let* ((target (eval-pair-place-target place
                                         env
                                         (if (eq? (car place) 'car)
                                             "incf car"
                                             "incf cdr")))
         (current (if (eq? (car place) 'car)
                      (car target)
                      (cdr target)))
         (new (+ current delta)))
    (if (eq? (car place) 'car)
        (set-car! target new)
        (set-cdr! target new))
    new))

(define (setq-entry-symbol raw env)
  (if (frame-bound? env raw)
      raw
      (resolve-binding-symbol raw)))

(define (resolve-required-binding-symbol raw who)
  (let ((sym (resolve-binding-symbol raw)))
    (unless (symbol? sym)
      (error who sym))
    sym))

(define (define-global-binding! env raw value who)
  (let ((sym (resolve-required-binding-symbol raw who)))
    (frame-define! (global-frame env) sym value)
    sym))

(define (define-global-binding-if-missing! env raw value-thunk who)
  (let* ((sym (resolve-required-binding-symbol raw who))
         (global (global-frame env)))
    (unless (frame-bound? global sym)
      (frame-define! global sym (value-thunk)))
    sym))

(define (eval-setf-symbol-place place env val)
  (frame-set! env
              (if (frame-bound? env place)
                  place
                  (resolve-binding-symbol place))
              val)
  val)

(define (eval-setf-vector-place place env val)
  (let ((target (eval-islisp* (cadr place) env #f))
        (index  (eval-islisp* (caddr place) env #f)))
    (cond
     ((vector? target)
      (unless (and (integer? index) (>= index 0) (< index (vector-length target)))
        (error "setf aref/vector-ref: index out of range" index))
      (vector-set! target index val)
      val)
     ((string? target)
      ;; (setf (aref string index) char)
      (unless (and (integer? index) (>= index 0) (< index (string-length target)))
        (error "setf aref: string index out of range" index))
      (unless (char? val)
        (error "setf aref: value for string element must be a character" val))
      (string-set! target index val)
      val)
     (else
      (error "setf aref/vector-ref: target must be a vector or string" target)))))

(define (eval-setf-string-ref-place place env val)
  (let ((target (eval-islisp* (cadr place) env #f))
        (index  (eval-islisp* (caddr place) env #f)))
    (unless (string? target)
      (error "setf string-ref: target must be a string" target))
    (unless (and (integer? index) (>= index 0) (< index (string-length target)))
      (error "setf string-ref: index out of range" index))
    (unless (char? val)
      (error "setf string-ref: value must be a character" val))
    (string-set! target index val)
    val))

(define (eval-setf-gethash-place place env val)
  (let ((key (eval-islisp* (cadr place) env #f))
        (table (eval-islisp* (caddr place) env #f)))
    (unless (hash-table? table)
      (error "setf gethash table must be hash-table" table))
    (hash-table-put! table key val)
    val))

(define (eval-setf-slot-place place env val)
  (let ((target (eval-islisp* (cadr place) env #f))
        (slot (eval-islisp* (caddr place) env #f)))
    (instance-slot-set! target slot val)))

(define (eval-setf-accessor-place place env val)
  (let* ((raw-op (car place))
         (op (if (frame-bound? env raw-op)
                 raw-op
                 (resolve-binding-symbol raw-op)))
         (slot (accessor-slot-ref op)))
    (if slot
        (let ((target (eval-islisp* (cadr place) env #f)))
          (instance-slot-set! target slot val))
        (error "Unsupported setf place" place))))

(define (eval-incf-symbol-place place env delta)
  (let* ((sym (setq-entry-symbol place env))
         (new (+ (frame-ref env sym) delta)))
    (frame-set! env sym new)
    new))

(define (setf-place-handler place)
  (cond
   ((symbol? place) eval-setf-symbol-place)
   ((and (pair? place) (eq? (car place) 'car) (= (length place) 2))
    eval-setf-pair-place)
   ((and (pair? place) (eq? (car place) 'cdr) (= (length place) 2))
    eval-setf-pair-place)
   ((and (pair? place)
         (or (eq? (car place) 'vector-ref) (eq? (car place) 'aref))
         (= (length place) 3))
    eval-setf-vector-place)
   ((and (pair? place) (eq? (car place) 'gethash) (or (= (length place) 3) (= (length place) 4)))
    eval-setf-gethash-place)
   ((and (pair? place) (eq? (car place) 'slot-value) (= (length place) 3))
    eval-setf-slot-place)
   ;; (setf (string-ref s i) ch)
   ((and (pair? place) (eq? (car place) 'string-ref) (= (length place) 3))
    eval-setf-string-ref-place)
   ;; (setf (general-vector-ref v i) x)
   ((and (pair? place) (eq? (car place) 'general-vector-ref) (= (length place) 3))
    eval-setf-vector-place)
   ;; (setf (dynamic var) val)
   ((and (pair? place) (eq? (car place) 'dynamic) (= (length place) 2))
    (lambda (place env val)
      (let ((name (resolve-binding-symbol (cadr place))))
        (hash-table-put! (*dynamic-var-table*) name val)
        val)))
   ((and (pair? place) (symbol? (car place)) (= (length place) 2))
    eval-setf-accessor-place)
   (else #f)))

(define (eval-setf args env form)
  (if (= (length args) 2)
      (let* ((place (car args))
             (val (eval-islisp* (cadr args) env #f))
             (handler (setf-place-handler place)))
        (if handler
            (handler place env val)
            (error "Unsupported setf place" place)))
      (error "setf takes place and expression" form)))

(define (incf-place-handler place)
  (cond
   ((symbol? place) eval-incf-symbol-place)
   ((and (pair? place) (eq? (car place) 'car) (= (length place) 2))
    eval-incf-pair-place)
   ((and (pair? place) (eq? (car place) 'cdr) (= (length place) 2))
    eval-incf-pair-place)
   (else #f)))

(define (eval-incf args env form)
  (if (or (= (length args) 1) (= (length args) 2))
      (let* ((place (car args))
             (delta (if (= (length args) 2)
                        (eval-islisp* (cadr args) env #f)
                        1))
             (handler (incf-place-handler place)))
        (if handler
            (handler place env delta)
            (error "Unsupported incf place" place)))
      (error "incf takes place and optional delta" form)))

(define (resolve-trace-symbols args who)
  (map (lambda (s)
         (unless (symbol? s)
           (error who s))
         (resolve-binding-symbol s))
       args))

(define (eval-trace-op args env trace?)
  (if (null? args)
      (if trace?
          (delete-duplicates (append (map car *trace-table*) *trace-macro-set*) eq?)
          (untrace-all! env))
      (map (lambda (sym)
             ((if trace? trace-one! untrace-one!) env sym))
           (resolve-trace-symbols args
                                  (if trace?
                                      "trace arguments must be symbols"
                                      "untrace arguments must be symbols")))))

(define (eval-return-from args env form)
  (unless (or (= (length args) 1) (= (length args) 2))
    (error "return-from takes block-name and optional value" form))
  (let* ((name (resolve-binding-symbol (car args)))
         (value (if (= (length args) 2)
                    (eval-islisp* (cadr args) env #f)
                    '())))
    (unless (symbol? name)
      (error "return-from name must be a symbol" name))
    (let ((entry (assoc name *block-stack*)))
      (unless entry
        (error "No enclosing block for return-from" name))
      ((cdr entry) value))))

(define (eval-throw args env form)
  (unless (= (length args) 2)
    (error "throw takes tag and value" form))
  (throw-to-catch (eval-islisp* (car args) env #f)
                  (eval-islisp* (cadr args) env #f)))

(define (eval-go args form)
  (unless (= (length args) 1)
    (error "go takes one tag argument" form))
  (let ((tag (car args)))
    (unless (or (symbol? tag) (integer? tag))
      (error "go tag must be a symbol or integer" tag))
    (raise (make-go-signal tag))))

;; ---- Phase 3: eval helpers ----

;; 3-A: flet / labels
(define (eval-macrolet args env tail?)
  ;; (macrolet ((name params body...) ...) body...)
  (unless (and (list? args) (>= (length args) 2) (list? (car args)))
    (error "macrolet" "needs bindings and body" args))
  (let* ((bindings (car args))
         (body     (cdr args))
         (menv     (make-frame env)))
    (for-each
     (lambda (binding)
       (unless (and (list? binding) (>= (length binding) 3) (symbol? (car binding)))
         (error "macrolet" "invalid binding" binding))
       (let* ((mname  (resolve-binding-symbol (car binding)))
              (params (cadr binding))
              (mbody  (cddr binding))
              (m      (make-macro params mbody menv mname)))
         (frame-define! menv mname m)))
     bindings)
    (eval-sequence* body menv tail?)))

(define (eval-flet-like args env tail? recursive?)
  (unless (and (list? args) (>= (length args) 2) (list? (car args)))
    (error (if recursive? "labels" "flet") "needs bindings and body" args))
  (let* ((bindings (car args))
         (body     (cdr args))
         (fenv     (make-frame env)))
    (for-each
     (lambda (binding)
       (unless (and (list? binding) (>= (length binding) 3) (symbol? (car binding)))
         (error (if recursive? "labels" "flet") "invalid binding" binding))
       (let* ((fname  (resolve-binding-symbol (car binding)))
              (params (cadr binding))
              (fbody  (cddr binding))
              (scope  (if recursive? fenv env))
              (fn     (make-callable-definition params fbody scope fname #f)))
         (frame-define! fenv fname fn)))
     bindings)
    (eval-sequence* body fenv tail?)))

;; 3-B: the — type declaration
(define (isl-instance-of? val class-name)
  ;; Check built-in types by ISLISP class name, then user-defined ISL instances.
  ;; Strip package prefix (e.g. ISLISP::<integer> → <integer>) before comparison.
  (let* ((cn (if (symbol? class-name)
                 (string->symbol (symbol-base-name class-name))
                 class-name))
         (s (symbol->string (if (symbol? cn) cn 'unknown)))
         (bare (cond
                ((and (> (string-length s) 0) (char=? (string-ref s 0) #\<)
                      (char=? (string-ref s (- (string-length s) 1)) #\>))
                 (substring s 1 (- (string-length s) 1)))
                (else s))))
    (cond
     ((string=? bare "object")    #t)
     ((string=? bare "t")         #t)
     ((string=? bare "integer")   (and (integer? val) (exact? val)))
     ((string=? bare "float")     (and (number? val) (inexact? val)))
     ((string=? bare "number")    (number? val))
     ((string=? bare "string")    (string? val))
     ((string=? bare "symbol")    (symbol? val))
     ((string=? bare "character") (char? val))
     ((string=? bare "list")      (or (null? val) (pair? val)))
     ((string=? bare "cons")      (pair? val))
     ((string=? bare "null")      (null? val))
     ((string=? bare "vector")    (isl-plain-vector? val))
     ((string=? bare "general-vector") (isl-plain-vector? val))
     ((string=? bare "general-array*") (isl-array? val))
     ((string=? bare "basic-array*")   (isl-array? val))
     ((string=? bare "basic-vector")   (or (isl-plain-vector? val) (string? val)))
     ((string=? bare "basic-array")
      (or (isl-plain-vector? val) (string? val) (isl-array? val)))
     ((string=? bare "function")  (or (closure? val) (primitive? val)))
     ;; Condition objects: check ISL condition class hierarchy via isl-subclassp?
     ((isl-condition? val)
      (let* ((cond-class-sym (isl-condition-class val))
             ;; Build target sym with angle brackets, then try both bare and resolved
             (target-bare (string->symbol (string-append "<" bare ">")))
             (target-resolved (resolve-binding-symbol target-bare)))
        (or (isl-subclassp? cond-class-sym target-bare)
            (isl-subclassp? cond-class-sym target-resolved))))
     (else
      ;; Try user-defined class: lookup by resolved name and by original symbol.
      (and (instance? val)
           (let ((class-obj (or (class-table-ref (resolve-binding-symbol class-name))
                                (class-table-ref class-name))))
             (and class-obj
                  (let ((d (class-distance (instance-class val) class-obj)))
                    (if d #t #f)))))))))

(define (eval-the args env tail?)
  (unless (= (length args) 2)
    (error "the needs class-name and form" args))
  (let* ((class-name (car args))
         (form       (cadr args))
         (val        (eval-islisp* form env tail?)))
    (unless (isl-instance-of? val class-name)
      (error "the: value is not of expected class" val class-name))
    val))

;; 3-C: unwind-protect
(define (eval-unwind-protect args env tail?)
  (unless (>= (length args) 1)
    (error "unwind-protect needs at least a protected form" args))
  (let ((protected (car args))
        (cleanups  (cdr args)))
    (dynamic-wind
      (lambda () #f)
      (lambda () (eval-islisp* protected env #f))
      (lambda ()
        (for-each (lambda (f) (eval-islisp* f env #f)) cleanups)))))

;; 3-E: defconstant
(define (eval-defconstant args env form)
  (eval-defglobal args env form))

;; 3-F: function special form
(define (eval-function-form args env)
  (unless (= (length args) 1)
    (error "function needs exactly one argument" args))
  (let ((name (car args)))
    (cond
     ((and (pair? name) (eq? (car name) 'lambda))
      (eval-islisp* name env #f))
     ((symbol? name)
      (let* ((sym (if (frame-bound? env name)
                      name
                      (resolve-symbol-in-package name)))
             (val (frame-ref env sym)))
        (unless (or (closure? val) (primitive? val) (generic? val))
          (error "function: not a function" name))
        val))
     (else
      (error "function needs symbol or lambda form" name)))))

(define (make-callable-definition params body env name macro?)
  (validate-params params)
  (if macro?
      (make-macro params body env name)
      (make-closure params body env name)))

(define (eval-lambda args env form)
  (unless (>= (length args) 2)
    (error "lambda needs params and body" form))
  (make-callable-definition (car args) (cdr args) env #f #f))

(define (eval-defglobal args env form)
  (if (= (length args) 2)
      (define-global-binding! env
                              (car args)
                              (eval-islisp* (cadr args) env #f)
                              "defglobal needs a symbol")
      (error "defglobal takes symbol and expression" form)))

;; ---- Phase 6-B: dynamic / dynamic-let ----
(define (eval-dynamic args env)
  (unless (= (length args) 1)
    (error "dynamic needs exactly one variable name" args))
  (let ((name (resolve-binding-symbol (car args))))
    (if (hash-table-exists? (*dynamic-var-table*) name)
        (hash-table-ref (*dynamic-var-table*) name)
        (error "dynamic variable not bound" name))))

(define (eval-dynamic-let args env tail?)
  (unless (and (list? args) (>= (length args) 2) (list? (car args)))
    (error "dynamic-let needs bindings and body" args))
  (let* ((bindings (car args))
         (body     (cdr args))
         (tbl      (*dynamic-var-table*))
         ;; Evaluate new values first (before any mutation).
         (new-vals (map (lambda (b)
                          (unless (and (list? b) (= (length b) 2) (symbol? (car b)))
                            (error "dynamic-let binding must be (var form)" b))
                          (cons (resolve-binding-symbol (car b))
                                (eval-islisp* (cadr b) env #f)))
                        bindings))
         ;; Save current values.
         (saved    (map (lambda (pair)
                          (let ((sym (car pair)))
                            (cons sym
                                  (if (hash-table-exists? tbl sym)
                                      (cons #t (hash-table-ref tbl sym))
                                      (cons #f #f)))))
                        new-vals)))
    ;; Install new values.
    (for-each (lambda (pair) (hash-table-put! tbl (car pair) (cdr pair))) new-vals)
    (dynamic-wind
      (lambda () #f)
      (lambda () (eval-sequence* body env tail?))
      (lambda ()
        ;; Restore old values.
        (for-each (lambda (entry)
                    (let ((sym (car entry))
                          (had? (cadr entry))
                          (old  (cddr entry)))
                      (if had?
                          (hash-table-put! tbl sym old)
                          (hash-table-delete! tbl sym))))
                  saved)))))

(define (eval-defvar args env form)
  (if (= (length args) 2)
      (let ((sym (define-global-binding-if-missing!
                   env (car args)
                   (lambda () (eval-islisp* (cadr args) env #f))
                   "defvar needs a symbol")))
        ;; Also register in dynamic variable table (only if not already there).
        (unless (hash-table-exists? (*dynamic-var-table*) sym)
          (hash-table-put! (*dynamic-var-table*) sym (frame-ref env sym)))
        sym)
      (error "defvar takes symbol and expression" form)))

;; ISLISP §23.1 defdynamic — standard ISLISP name for defining dynamic variables
(define (eval-defdynamic args env form)
  (unless (= (length args) 2)
    (error "defdynamic takes symbol and initial-value" form))
  (let* ((raw (car args))
         (val (eval-islisp* (cadr args) env #f)))
    (unless (symbol? raw)
      (error "defdynamic: first argument must be a symbol" raw))
    (let ((sym (define-global-binding! env raw val "defdynamic")))
      ;; Always set (or update) the dynamic binding.
      (hash-table-put! (*dynamic-var-table*) sym val)
      sym)))

(define (eval-progn args env tail?)
  (eval-sequence* args env tail?))

(define (eval-global-function-definition args env form macro?)
  (unless (>= (length args) 3)
    (error (if macro?
               "defmacro needs name, params and body"
               "defun needs name, params and body")
           form))
  (let ((name (resolve-binding-symbol (car args)))
        (params (cadr args))
        (body (cddr args)))
    (unless (symbol? name)
      (error (if macro?
                 "defmacro name must be symbol"
                 "defun name must be symbol")
             name))
    (frame-define! (global-frame env)
                   name
                   (make-callable-definition params body env name macro?))
    name))

(define (eval-let-form args env tail? sequential? form)
  (if (>= (length args) 2)
      (let ((bindings (car args))
            (body (cdr args)))
        (eval-sequence* body
                        (eval-let-bindings bindings env sequential?)
                        tail?))
      (error (if sequential?
                 "let* needs bindings and body"
                 "let needs bindings and body")
             form)))

(define (eval-setq args env form)
  (unless (and (>= (length args) 2) (even? (length args)))
    (error "setq takes symbol/expression pairs" form))
  (let loop ((rest args) (last '()))
    (if (null? rest)
        last
        (let* ((raw (car rest))
               (sym (setq-entry-symbol raw env))
               (val (eval-islisp* (cadr rest) env #f)))
          (unless (symbol? sym)
            (error "setq needs a symbol" sym))
          (frame-set! env sym val)
          (loop (cddr rest) val)))))

(define (require-symbol-list vals who)
  (for-each
   (lambda (x)
     (unless (symbol? x)
       (error who "expects symbols" x)))
   vals))

(define (eval-defpackage-import-option pkg vals opt shadowing?)
  (unless (>= (length vals) 2)
    (error (if shadowing?
               "defpackage :shadowing-import-from expects package and at least one symbol"
               "defpackage :import-from expects package and at least one symbol")
           opt))
  (let ((from (ensure-package! (car vals)))
        (syms (cdr vals)))
    (require-symbol-list syms
                         (if shadowing?
                             "defpackage :shadowing-import-from"
                             "defpackage :import-from"))
    (for-each
     (lambda (x)
       (let ((s (package-find-external-symbol-by-name from x)))
         (unless s
           (error (if shadowing?
                      "defpackage :shadowing-import-from symbol is not exported"
                      "defpackage :import-from symbol is not exported")
                  (car vals)
                  x))
         ((if shadowing? package-shadowing-import-symbol! package-import-symbol!)
          pkg
          s)))
     syms)))

(define (eval-defpackage-export-option pkg vals)
  (for-each
   (lambda (x)
     (unless (symbol? x)
       (error "defpackage :export expects symbols" x))
     (package-export! pkg x))
   vals))

(define (eval-defpackage-nicknames-option pkg vals)
  (for-each
   (lambda (x)
     (unless (or (symbol? x) (string? x))
       (error "defpackage :nicknames expects symbol/string designators" x))
     (register-package-name! x pkg))
   vals))

(define (eval-defpackage-use-option pkg vals)
  (for-each
   (lambda (x)
     (package-use! pkg (ensure-package! x)))
   vals))

(define (eval-defpackage-intern-option pkg vals)
  (require-symbol-list vals "defpackage :intern")
  (for-each
   (lambda (x)
     (package-intern! pkg (symbol-base-name x)))
   vals))

(define (eval-defpackage-shadow-option pkg vals)
  (require-symbol-list vals "defpackage :shadow")
  (for-each
   (lambda (x)
     (package-shadow-symbol! pkg x))
   vals))

(define (eval-defpackage-size-option vals opt)
  (unless (= (length vals) 1)
    (error "defpackage :size expects one integer value" opt))
  (unless (and (integer? (car vals)) (>= (car vals) 0))
    (error "defpackage :size expects non-negative integer" (car vals))))

(define (eval-defpackage-documentation-option vals opt)
  (unless (= (length vals) 1)
    (error "defpackage :documentation expects one string value" opt))
  (unless (string? (car vals))
    (error "defpackage :documentation expects string" (car vals))))

(define (eval-defpackage args)
  (unless (>= (length args) 1)
    (error "defpackage needs package name" args))
  (let* ((name (car args))
         (pkg (ensure-package! name)))
    (for-each
     (lambda (opt)
       (unless (and (list? opt) (not (null? opt)))
         (error "invalid defpackage option" opt))
       (let ((tag (car opt))
             (vals (cdr opt)))
         (cond
          ((eq? tag ':nicknames)
           (eval-defpackage-nicknames-option pkg vals))
          ((eq? tag ':use)
           (eval-defpackage-use-option pkg vals))
          ((eq? tag ':intern)
           (eval-defpackage-intern-option pkg vals))
          ((eq? tag ':shadow)
           (eval-defpackage-shadow-option pkg vals))
          ((eq? tag ':import-from)
           (eval-defpackage-import-option pkg vals opt #f))
          ((eq? tag ':shadowing-import-from)
           (eval-defpackage-import-option pkg vals opt #t))
          ((eq? tag ':export)
           (eval-defpackage-export-option pkg vals))
          ((eq? tag ':size)
           (eval-defpackage-size-option vals opt))
          ((eq? tag ':documentation)
           (eval-defpackage-documentation-option vals opt))
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

(define (bind-required-params! frame required args)
  (let bind ((params required) (rest args))
    (if (null? params)
        rest
        (begin
          (frame-define! frame (car params) (car rest))
          (bind (cdr params) (cdr rest))))))

(define (bind-optional-params! frame optional args)
  (let bind ((params optional) (rest args))
    (if (null? params)
        rest
        (let* ((entry (car params))
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
                (bind (cdr params) rest))
              (begin
                (frame-define! frame sym (car rest))
                (bind (cdr params) (cdr rest))))))))

(define (bind-rest-param! frame rest-sym args)
  (if rest-sym
      (begin
        (frame-define! frame rest-sym args)
        frame)
      (if (null? args)
          frame
          (error "Too many arguments" args))))

(define (bind-params! frame param-spec args)
  (let ((required (car param-spec))
        (optional (cadr param-spec))
        (rest-sym (caddr param-spec)))
    (when (< (length args) (length required))
      (error "Too few arguments" args))
    (bind-rest-param! frame
                      rest-sym
                      (bind-optional-params! frame
                                             optional
                                             (bind-required-params! frame required args)))))

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

(define (call-with-block-entry name thunk)
  (if name
      (call/cc
       (lambda (escape)
         (call-with-stack-entry
          (lambda () *block-stack*)
          (lambda (v) (set! *block-stack* v))
          (cons name escape)
          thunk)))
      (thunk)))

(define (make-closure-call-env closure args)
  (let* ((param-spec (parse-params (closure-params closure)))
         (call-env (make-frame (closure-env closure))))
    (bind-params! call-env param-spec args)
    call-env))

(define (eval-closure-call closure args)
  (call-with-block-entry
   (closure-name closure)
   (lambda ()
     (eval-sequence* (closure-body closure)
                     (make-closure-call-env closure args)
                     #t))))

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
      (let ((result (eval-closure-call current-fn current-args)))
        (if (tail-call? result)
            (loop (tail-call-fn result) (tail-call-args result))
            result)))
     (else
      (error "Attempt to call non-function" current-fn)))))

(define (eval-cond clauses env tail?)
  (let loop ((rest clauses))
    (if (null? rest)
        '()
        (let* ((clause (car rest))
               (test-form (and (pair? clause) (car clause)))
               (body (and (pair? clause) (cdr clause))))
          (unless (and (list? clause) (not (null? clause)))
            (error "invalid cond clause" clause))
          (let ((test-val (eval-islisp* test-form env #f)))
            (if (truthy? test-val)
                (if (null? body)
                    test-val
                    (eval-sequence* body env tail?))
                (loop (cdr rest))))))))

(define (eval-case args env tail? form)
  (unless (>= (length args) 1)
    (error "case needs key and clauses" form))
  (let ((key (eval-islisp* (car args) env #f))
        (clauses (cdr args)))
    (let loop ((rest clauses))
      (if (null? rest)
          '()
          (let* ((clause (car rest))
                 (keys (and (pair? clause) (car clause)))
                 (body (and (pair? clause) (cdr clause))))
            (unless (and (list? clause) (>= (length clause) 1))
              (error "invalid case clause" clause))
            (if (memq keys '(otherwise t else))
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
                      (loop (cdr rest))))))))))

(define (eval-and args env)
  (let loop ((rest args) (last #t))
    (if (null? rest)
        last
        (let ((value (eval-islisp* (car rest) env #f)))
          (if (truthy? value)
              (loop (cdr rest) value)
              value)))))

(define (eval-or args env)
  (let loop ((rest args))
    (if (null? rest)
        '()
        (let ((value (eval-islisp* (car rest) env #f)))
          (if (truthy? value)
              value
              (loop (cdr rest)))))))

(define (eval-quote args form)
  (if (= (length args) 1)
      (car args)
      (error "quote takes one argument" form)))

(define (eval-quasiquote args env form)
  (if (= (length args) 1)
      (quasiquote-eval (car args) env 1)
      (error "quasiquote takes one argument" form)))

(define (eval-if args env tail? form)
  (if (or (= (length args) 2) (= (length args) 3))
      (let ((test (eval-islisp* (car args) env #f)))
        (if (truthy? test)
            (eval-islisp* (cadr args) env tail?)
            (if (= (length args) 3)
                (eval-islisp* (caddr args) env tail?)
                '())))
      (error "if takes 2 or 3 arguments" form)))

(define (special-form? sym)
  (memq sym '(quote quasiquote if cond case and or loop while do for dolist dotimes return-from catch throw go tagbody trace untrace lambda defpackage in-package defglobal defvar defdynamic setq setf incf defun defmacro defgeneric defmethod progn block let let* with-open-file handler-case defclass flet labels macrolet the unwind-protect defconstant function with-handler
             dynamic dynamic-let class)))

(define (extended-special-form? sym)
  (memq sym '(trace untrace)))

(define (eval-special form env tail?)
  (let ((op (car form))
        (args (cdr form)))
    (when (and (strict-profile?) (extended-special-form? op))
      (error "Special form is unavailable in strict profile" op))
    (case op
      ((quote)
       (eval-quote args form))
      ((quasiquote)
       (eval-quasiquote args env form))
      ((if)
       (eval-if args env tail? form))
      ((cond)
       (eval-cond args env tail?))
      ((case)
       (eval-case args env tail? form))
      ((and)
       (eval-and args env))
      ((or)
       (eval-or args env))
      ((loop)
       (eval-loop args env))
      ((while)
       (eval-while args env "while"))
      ((do)
       (eval-do args env tail?))
      ;; ISLISP §10.8 for: same structure as do (parallel binding updates)
      ((for)
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
       (eval-return-from args env form))
      ((catch)
       (eval-catch args env tail?))
      ((throw)
       (eval-throw args env form))
      ((go)
       (eval-go args form))
      ((tagbody)
       (eval-tagbody args env))
      ((trace)
       (eval-trace-op args env #t))
      ((untrace)
       (eval-trace-op args env #f))
      ((lambda)
       (eval-lambda args env form))
      ((defglobal)
       (eval-defglobal args env form))
      ((defvar)
       (eval-defvar args env form))
      ;; ISLISP §23.1 standard name (defvar is the non-standard alias kept for compat)
      ((defdynamic)
       (eval-defdynamic args env form))
      ((setq)
       (eval-setq args env form))
      ((setf)
       (eval-setf args env form))
      ((incf)
       (eval-incf args env form))
      ((defun)
       (eval-global-function-definition args env form #f))
      ((defmacro)
       (eval-global-function-definition args env form #t))
      ((defgeneric)
       (eval-defgeneric args env))
      ((defmethod)
       (eval-defmethod args env))
      ((defpackage)
       (eval-defpackage args))
      ((in-package)
       (eval-in-package args))
      ((progn)
       (eval-progn args env tail?))
      ((block)
       (eval-block args env tail?))
      ((let)
       (eval-let-form args env tail? #f form))
      ((let*)
       (eval-let-form args env tail? #t form))
      ;; ---- Phase 3 ----
      ((flet)
       (eval-flet-like args env tail? #f))
      ((labels)
       (eval-flet-like args env tail? #t))
      ((macrolet)
       (eval-macrolet args env tail?))
      ((the)
       (eval-the args env tail?))
      ((unwind-protect)
       (eval-unwind-protect args env tail?))
      ((defconstant)
       (eval-defconstant args env form))
      ((function)
       (eval-function-form args env))
      ((with-handler)
       (eval-with-handler args env tail?))
      ;; ---- Phase 6-B ----
      ((dynamic)
       (eval-dynamic args env))
      ((dynamic-let)
       (eval-dynamic-let args env tail?))
      ;; ---- Phase 7-C ----
      ((class)
       (eval-class-form args env))
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

(define (install-primitives! env)
  (define (def name proc)
    (let ((sym (resolve-binding-symbol name)))
      (frame-define! env sym (make-primitive name proc))
      (when *current-package*
        (package-export! *current-package* name))))
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
  (def 'set-car
    (lambda (pair val)
      (unless (pair? pair) (error "set-car: first argument must be a pair" pair))
      (set-car! pair val)
      val))
  (def 'set-cdr
    (lambda (pair val)
      (unless (pair? pair) (error "set-cdr: first argument must be a pair" pair))
      (set-cdr! pair val)
      val))
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
  ;; ---- 5-B: signal-condition, condition-message, condition-continuable ----
  (def 'signal-condition
    (lambda (cond-obj . rest)
      ;; ISLISP: (signal-condition condition continuable)
      ;; continuable arg is accepted but we use isl-condition-continuable? instead
      (unless (isl-condition? cond-obj)
        (error "signal-condition needs an isl condition object" cond-obj))
      (isl-signal-condition cond-obj)))
  (def 'condition-message
    (lambda (cond-obj)
      (cond
       ((isl-condition? cond-obj)
        (isl-condition-message cond-obj))
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
        (apply-islisp thunk '())))))

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
      ;; ---- Phase 7-E: initialize-object generic function ----
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
      ;; ---- ISLISP §16.3/§16.4: when/unless macros ----
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

;;; ----------------------------------------------------------------
;;; ISLISP reader layer
;;;
;;; Gauche's native reader does not understand the ISLISP/Common-Lisp
;;; function-quote syntax  #'foo  ==  (function foo).  We provide a thin
;;; wrapper reader that handles list structure, quote sugar and #', and
;;; delegates every other datum (numbers, strings, characters, vectors,
;;; #t/#f, charsets, ...) to Gauche's `read`, so the well-tested lexer
;;; for those is reused verbatim.
;;;
;;; Implementation: each source port is wrapped (once, cached as a port
;;; attribute) in a <virtual-input-port> backed by a pushback buffer.
;;; All character reads go through this virtual port so Gauche's own
;;; one-char unget composes correctly with our multi-char pushback.
;;; ----------------------------------------------------------------

(define (isl-delimiter? c)
  (or (eof-object? c)
      (char-whitespace? c)
      (memv c '(#\( #\) #\[ #\] #\" #\; #\'))))

(define (make-isl-vport src)
  ;; Returns (vport . unread!) where unread! pushes a char so it is read next.
  (let ((buf '()))
    (let* ((getc (lambda ()
                   (if (pair? buf)
                       (let ((c (car buf))) (set! buf (cdr buf)) c)
                       (read-char src))))
           (vp (make <virtual-input-port> :getc getc))
           (un! (lambda (c) (set! buf (cons c buf)))))
      (cons vp un!))))

(define (isl-port-for src)
  ;; One persistent wrapper per source port (kept as a port attribute so
  ;; its lifetime follows the port and leftover lookahead survives across
  ;; successive single-datum reads, e.g. at the REPL).
  (if (port? src)
      (or (port-attribute-ref src 'isl-reader #f)
          (let ((p (make-isl-vport src)))
            (port-attribute-set! src 'isl-reader p)
            p))
      (make-isl-vport src)))

(define (isl-peek-char vp un!)
  (let ((c (read-char vp)))
    (if (eof-object? c) c (begin (un! c) c))))

(define (isl-skip-line vp)
  (let ((c (read-char vp)))
    (cond ((eof-object? c) #t)
          ((char=? c #\newline) #t)
          (else (isl-skip-line vp)))))

(define (isl-skip-block vp un!)
  ;; Opening #| already consumed; handle nested #| ... |#.
  (let loop ((depth 1))
    (let ((c (read-char vp)))
      (cond
       ((eof-object? c) (error "unterminated #| block comment"))
       ((char=? c #\#)
        (if (eqv? (isl-peek-char vp un!) #\|)
            (begin (read-char vp) (loop (+ depth 1)))
            (loop depth)))
       ((char=? c #\|)
        (if (eqv? (isl-peek-char vp un!) #\#)
            (begin (read-char vp) (if (= depth 1) #t (loop (- depth 1))))
            (loop depth)))
       (else (loop depth))))))

(define (isl-skip-ws vp un!)
  (let ((c (read-char vp)))
    (cond
     ((eof-object? c) #t)
     ((char-whitespace? c) (isl-skip-ws vp un!))
     ((char=? c #\;) (isl-skip-line vp) (isl-skip-ws vp un!))
     ((char=? c #\#)
      (let ((c2 (read-char vp)))
        (cond
         ((eof-object? c2) (un! #\#) #t)
         ((char=? c2 #\|) (isl-skip-block vp un!) (isl-skip-ws vp un!))
         ((char=? c2 #\;) (isl-read-1 vp un!) (isl-skip-ws vp un!))
         (else (un! c2) (un! #\#) #t))))
     (else (un! c) #t))))

(define (isl-read-list vp un! close)
  (let loop ((acc '()))
    (isl-skip-ws vp un!)
    (let ((c (read-char vp)))
      (cond
       ((eof-object? c) (error "unexpected EOF inside list"))
       ((char=? c close) (reverse acc))
       ((or (char=? c #\)) (char=? c #\])) (error "mismatched close paren" c))
       ((and (char=? c #\.)
             (isl-delimiter? (isl-peek-char vp un!)))
        ;; dotted tail:  (a b . c)
        (let ((tail (isl-read-1 vp un!)))
          (isl-skip-ws vp un!)
          (let ((cc (read-char vp)))
            (if (and (char? cc) (char=? cc close))
                (append-reverse acc tail)
                (error "malformed dotted list")))))
       (else (un! c)
             (loop (cons (isl-read-1 vp un!) acc)))))))

(define (isl-read-1 vp un!)
  (isl-skip-ws vp un!)
  (let ((c (read-char vp)))
    (cond
     ((eof-object? c) c)
     ((char=? c #\() (isl-read-list vp un! #\)))
     ((char=? c #\[) (isl-read-list vp un! #\]))
     ((or (char=? c #\)) (char=? c #\])) (error "unexpected close paren" c))
     ((char=? c #\') (list 'quote (isl-read-1 vp un!)))
     ((char=? c #\`) (list 'quasiquote (isl-read-1 vp un!)))
     ((char=? c #\,)
      (let ((c2 (read-char vp)))
        (if (and (char? c2) (char=? c2 #\@))
            (list 'unquote-splicing (isl-read-1 vp un!))
            (begin (when (char? c2) (un! c2))
                   (list 'unquote (isl-read-1 vp un!))))))
     ((char=? c #\#)
      (let ((c2 (read-char vp)))
        (cond
         ((eof-object? c2) (un! #\#) (read vp))
         ((char=? c2 #\') (list 'function (isl-read-1 vp un!)))
         (else (un! c2) (un! #\#) (read vp)))))
     (else (un! c) (read vp)))))

(define (isl-read src)
  (let ((p (isl-port-for src)))
    (isl-read-1 (car p) (cdr p))))

(define (read-all port)
  (let loop ((acc '()))
    (let ((x (isl-read port)))
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
  (let ((x (isl-read (current-input-port))))
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
            (let ((result (eval-islisp (if (strict-profile?)
                                           (sanitize-for-strict x)
                                           x)
                                       env)))
              (write-result result)
              (newline)))
          (repl env)))))
