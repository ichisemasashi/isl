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
          make-frame frame-define! make-macro macroexpand*
          ;; R7: core/runtime 共通ユーティリティ（compiler runtime と共有）
          make-go-signal go-signal? go-signal-tag
          score<? score=?
          gauche-make-tls gauche-tls-bind gauche-tls-accept gauche-tls-close
          gauche-tls-load-certificate gauche-tls-load-private-key
          gauche-connection-input-port gauche-connection-output-port
          gauche-connection-close
          gauche-make-thread gauche-thread-start! gauche-thread-join! gauche-thread?
          gauche-make-mutex gauche-mutex-lock! gauche-mutex-unlock! gauche-mutex?))

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

;; ISLISP §22: case-using — like case but compares the key against each datum
;; with a user-supplied two-argument predicate instead of eql.
(define (eval-case-using args env tail? form)
  (unless (>= (length args) 2)
    (error "case-using needs a predicate, key and clauses" form))
  (let ((pred (eval-islisp* (car args) env #f))
        (key  (eval-islisp* (cadr args) env #f))
        (clauses (cddr args)))
    (let loop ((rest clauses))
      (if (null? rest)
          '()
          (let* ((clause (car rest))
                 (keys (and (pair? clause) (car clause)))
                 (body (and (pair? clause) (cdr clause))))
            (unless (and (list? clause) (>= (length clause) 1))
              (error "invalid case-using clause" clause))
            (if (memq keys '(otherwise t else))
                (if (null? body) '() (eval-sequence* body env tail?))
                (let ((matched?
                       (if (list? keys)
                           (any (lambda (k)
                                  (truthy? (apply-islisp pred (list key k))))
                                keys)
                           (truthy? (apply-islisp pred (list key keys))))))
                  (if matched?
                      (if (null? body) '() (eval-sequence* body env tail?))
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
  (memq sym '(quote quasiquote if cond case case-using and or loop while do for dolist dotimes return-from catch throw go tagbody trace untrace lambda defpackage in-package defglobal defvar defdynamic setq setf incf defun defmacro defgeneric defmethod progn block let let* with-open-file handler-case defclass flet labels macrolet the assure unwind-protect defconstant function with-handler
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
      ((case-using)
       (eval-case-using args env tail? form))
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
      ((assure)
       ;; ISLISP §26: assure is `the` with the same class-assertion semantics.
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
              (let* ((rsym (resolve-symbol-in-package form))
                     (rpair (frame-find-pair env rsym)))
                (if rpair
                    (cdr rpair)
                    (isl-signal-unbound-variable form)))))))
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
            (cond
             ;; A symbol in operator position that is bound nowhere is an
             ;; undefined function (not a plain unbound variable).
             ((and (symbol? op) (not op-pair))
              (isl-signal-undefined-function op))
             ((and op-pair (macro? op-val))
              (eval-islisp* (if (macro-traced? op-sym)
                                (trace-apply-macro op-sym op-val args)
                                (apply-macro op-val args))
                           env
                           tail?))
             (else
              (let ((fn (eval-islisp* op env #f))
                    (vals (eval-list args env)))
                (if tail?
                    (make-tail-call fn vals)
                    (apply-islisp fn vals)))))))))
   ((null? form) '())
   (else
    form)))

(define (eval-islisp form env)
  (force-value (eval-islisp* form env #t)))

(include "core/conditions.scm")
(include "core/primitives.scm")
(include "core/bootstrap.scm")
(include "core/reader.scm")
