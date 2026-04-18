;; echospace: Discord-like chat sample app for ISL.

(defun echospace-env-or-empty (name)
  (let ((v (getenv name)))
    (if (null v) "" v)))

(defun echospace-root ()
  (let ((v (getenv "ISL_ROOT")))
    (if (or (null v) (= (length v) 0))
        "/Volumes/SSD-PLU3/work/LISP/islisp/isl"
        v)))

(defun echospace-path (suffix)
  (string-append (echospace-root) suffix))

(load (echospace-path "/lib/os-utils.lsp"))
(load (echospace-path "/examples/dbms/app/repr.lsp"))
(load (echospace-path "/examples/dbms/app/profile.lsp"))
(load (echospace-path "/examples/dbms/app/engine.lsp"))

(defglobal *echospace-current-user* '())
(defglobal *echospace-response-extra-headers* '())
(defglobal *echospace-request-body-override* '())
(defglobal *echospace-request-id* "")
(defglobal *echospace-db-bootstrap-root* "")
(defglobal *echospace-db-bootstrap-ready* nil)
(defglobal *echospace-db-table-cache* '())
(defglobal *echospace-embed-mutex* (mutex-open))

(defun echospace-safe-text (v)
  (if (null v) "" v))

(defun echospace-starts-with (s prefix)
  (let ((n (length s))
        (m (length prefix)))
    (if (< n m)
        nil
        (string= (substring s 0 m) prefix))))

(defun echospace-app-base ()
  (let ((script-name (echospace-env-or-empty "SCRIPT_NAME")))
    (if (= (length script-name) 0)
        "/echospace"
        script-name)))

(defun echospace-request-method ()
  (let ((m (echospace-env-or-empty "REQUEST_METHOD")))
    (if (= (length m) 0) "GET" m)))

(defun echospace-reverse (xs)
  (let ((rest xs)
        (out '()))
    (while (not (null rest))
      (setq out (cons (car rest) out))
      (setq rest (cdr rest)))
    out))

(defun echospace-split-on (s delim)
  (if (= (length s) 0)
      '()
      (let ((p (string-index delim s)))
        (if (null p)
            (list s)
            (cons (substring s 0 p)
                  (echospace-split-on (substring s (+ p 1)) delim))))))

(defun echospace-split-once (s delim)
  (let ((p (string-index delim s)))
    (if (null p)
        (list s "")
        (list (substring s 0 p)
              (substring s (+ p 1))))))

(defun echospace-trim-ws (s)
  (let ((n (length s))
        (start 0)
        (stop (- (length s) 1)))
    (while (and (< start n)
                (not (null (string-index (substring s start (+ start 1)) " \t\r\n"))))
      (setq start (+ start 1)))
    (while (and (>= stop start)
                (not (null (string-index (substring s stop (+ stop 1)) " \t\r\n"))))
      (setq stop (- stop 1)))
    (if (> start stop)
        ""
        (substring s start (+ stop 1)))))

(defun echospace-blank-text-p (s)
  (= (length (echospace-trim-ws (echospace-safe-text s))) 0))

(defun echospace-trim-leading-slashes (s)
  (if (> (length s) 0)
      (if (string= (substring s 0 1) "/")
          (echospace-trim-leading-slashes (substring s 1))
          s)
      s))

(defun echospace-trim-trailing-slashes (s)
  (let ((n (length s)))
    (if (> n 1)
        (if (string= (substring s (- n 1) n) "/")
            (echospace-trim-trailing-slashes (substring s 0 (- n 1)))
            s)
        s)))

(defun echospace-canonical-path-info (raw)
  (if (= (length raw) 0)
      ""
      (let ((t1 (echospace-trim-trailing-slashes raw)))
        (if (= (length t1) 0)
            ""
            (if (string= (substring t1 0 1) "/")
                t1
                (string-append "/" t1))))))

(defun echospace-needs-canonical-redirect-p (raw)
  (if (= (length raw) 0)
      nil
      (if (string= raw "/")
          nil
          (not (string= raw (echospace-canonical-path-info raw))))))

(defun echospace-split-path (s)
  (if (= (length s) 0)
      '()
      (let ((sep (string-index "/" s)))
        (if (null sep)
            (list s)
            (cons (substring s 0 sep)
                  (echospace-split-path (substring s (+ sep 1))))))))

(defun echospace-path-segments ()
  (let* ((raw (echospace-env-or-empty "PATH_INFO"))
         (trimmed (echospace-trim-leading-slashes raw)))
    (if (= (length trimmed) 0)
        '()
        (echospace-split-path trimmed))))

(defun echospace-ascii-downcase (s)
  (let ((i 0)
        (out ""))
    (while (< i (length s))
      (let* ((ch (substring s i (+ i 1)))
             (p (string-index ch "ABCDEFGHIJKLMNOPQRSTUVWXYZ")))
        (if (null p)
            (setq out (string-append out ch))
            (setq out (string-append out (substring "abcdefghijklmnopqrstuvwxyz" p (+ p 1))))))
      (setq i (+ i 1)))
    out))

(defun echospace-html-escape (s)
  (let ((text (echospace-safe-text s))
        (i 0)
        (out ""))
    (while (< i (length text))
      (let ((ch (substring text i (+ i 1))))
        (cond
         ((string= ch "&") (setq out (string-append out "&amp;")))
         ((string= ch "<") (setq out (string-append out "&lt;")))
         ((string= ch ">") (setq out (string-append out "&gt;")))
         ((string= ch "\"") (setq out (string-append out "&quot;")))
         (t (setq out (string-append out ch)))))
      (setq i (+ i 1)))
    out))

(defun echospace-js-escape (s)
  (let ((i 0)
        (out ""))
    (while (< i (length s))
      (let ((ch (substring s i (+ i 1))))
        (setq out
              (string-append out
                             (cond
                              ((string= ch "\\") "\\\\")
                              ((string= ch "\"") "\\\"")
                              ((string= ch "\n") "\\n")
                              ((string= ch "\r") "\\r")
                              ((string= ch "\t") "\\t")
                              (t ch)))))
      (setq i (+ i 1)))
    out))

(defun echospace-json-escape (s)
  (let ((i 0)
        (out ""))
    (while (< i (length s))
      (let ((ch (substring s i (+ i 1))))
        (setq out
              (string-append out
                             (cond
                              ((string= ch "\\") "\\\\")
                              ((string= ch "\"") "\\\"")
                              ((string= ch "\n") "\\n")
                              ((string= ch "\r") "\\r")
                              ((string= ch "\t") "\\t")
                              (t ch)))))
      (setq i (+ i 1)))
    out))

(defun echospace-list-nth-or-empty (xs idx)
  (if (or (null xs) (< idx 0))
      '()
      (if (= idx 0)
          (car xs)
          (echospace-list-nth-or-empty (cdr xs) (- idx 1)))))

(defun echospace-hex-digit-value (ch)
  (let ((p (string-index ch "0123456789")))
    (if (null p)
        (let ((pa (string-index ch "ABCDEF")))
          (if (null pa)
              (let ((pl (string-index ch "abcdef")))
                (if (null pl)
                    -1
                    (+ 10 pl)))
              (+ 10 pa)))
        p)))

(defglobal *echospace-ascii-visible*
  " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~")

(defun echospace-ascii-byte (ch)
  (cond
   ((string= ch "\t") 9)
   ((string= ch "\n") 10)
   ((string= ch "\r") 13)
   (t
    (let ((p (string-index ch *echospace-ascii-visible*)))
      (if (null p)
          (error "non-ascii character must be percent-encoded" ch)
          (+ p 32))))))

(defun echospace-utf8-cont-byte-p (b)
  (and (>= b 128) (<= b 191)))

(defun echospace-rest2 (xs)
  (cdr (cdr xs)))

(defun echospace-rest3 (xs)
  (cdr (echospace-rest2 xs)))

(defun echospace-decode-utf8-bytes (bytes)
  (let ((rest bytes)
        (out ""))
    (while (not (null rest))
      (let ((b1 (first rest)))
        (cond
         ((< b1 128)
          (setq out (string-append out (string (integer->char b1))))
          (setq rest (cdr rest)))
         ((and (>= b1 194) (<= b1 223))
          (let ((b2 (second rest)))
            (if (not (echospace-utf8-cont-byte-p b2))
                (error "invalid UTF-8 continuation byte")
                (progn
                  (setq out (string-append out (string (integer->char (+ (* (- b1 192) 64) (- b2 128))))))
                  (setq rest (echospace-rest2 rest))))))
         ((and (>= b1 224) (<= b1 239))
          (let ((b2 (second rest))
                (b3 (third rest)))
            (if (or (not (echospace-utf8-cont-byte-p b2))
                    (not (echospace-utf8-cont-byte-p b3)))
                (error "invalid UTF-8 continuation byte")
                (progn
                  (setq out (string-append out (string (integer->char (+ (* (- b1 224) 4096)
                                                                          (* (- b2 128) 64)
                                                                          (- b3 128))))))
                  (setq rest (echospace-rest3 rest))))))
         (t
          (error "unsupported UTF-8 byte sequence")))))
    out))

(defun echospace-url-decode (s)
  (let ((i 0)
        (n (length s))
        (bytes '()))
    (while (< i n)
      (let ((ch (substring s i (+ i 1))))
        (if (string= ch "+")
            (progn
              (setq bytes (append bytes (list 32)))
              (setq i (+ i 1)))
            (if (string= ch "%")
                (let* ((h1 (substring s (+ i 1) (+ i 2)))
                       (h2 (substring s (+ i 2) (+ i 3)))
                       (v1 (echospace-hex-digit-value h1))
                       (v2 (echospace-hex-digit-value h2)))
                  (if (or (< v1 0) (< v2 0))
                      (error "invalid percent-encoding")
                      (progn
                        (setq bytes (append bytes (list (+ (* v1 16) v2))))
                        (setq i (+ i 3)))))
                (progn
                  (setq bytes (append bytes (list (echospace-ascii-byte ch))))
                  (setq i (+ i 1)))))))
    (echospace-decode-utf8-bytes bytes)))

(defun echospace-url-encode-lite (s)
  (let ((i 0)
        (out ""))
    (while (< i (length s))
      (let ((ch (substring s i (+ i 1))))
        (setq out
              (string-append out
                             (cond
                              ((string= ch " ") "%20")
                              ((string= ch "&") "%26")
                              ((string= ch "?") "%3F")
                              ((string= ch "#") "%23")
                              ((string= ch "+") "%2B")
                              ((string= ch "%") "%25")
                              ((string= ch "=") "%3D")
                              ((string= ch "\"") "%22")
                              ((string= ch "'") "%27")
                              ((string= ch "/") "%2F")
                              (t ch)))))
      (setq i (+ i 1)))
    out))

(defun echospace-parse-form-field (raw-body field-name)
  (let ((pairs (echospace-split-on raw-body "&"))
        (value ""))
    (while (and (string= value "") (not (null pairs)))
      (let* ((kv (echospace-split-once (first pairs) "="))
             (k (echospace-url-decode (first kv)))
             (v (echospace-url-decode (second kv))))
        (if (string= k field-name)
            (setq value v)
            nil))
      (setq pairs (cdr pairs)))
    value))

(defun echospace-query-param (name)
  (echospace-parse-form-field (echospace-env-or-empty "QUERY_STRING") name))

(defun echospace-parse-int-safe (s)
  (let ((digits (echospace-trim-ws (echospace-safe-text s)))
        (i 0)
        (n 0))
    (if (= (length digits) 0)
        '()
        (progn
          (while (< i (length digits))
            (let ((p (string-index (substring digits i (+ i 1)) "0123456789")))
              (if (null p)
                  (setq n 'invalid)
                  (if (not (eq n 'invalid))
                      (setq n (+ (* n 10) p))
                      nil)))
            (setq i (+ i 1)))
          (if (eq n 'invalid) '() n)))))

(defun echospace-find-first-row (rows pred)
  (if (null rows)
      '()
      (if (funcall pred (car rows))
          (car rows)
          (echospace-find-first-row (cdr rows) pred))))

(defun echospace-filter-rows (rows pred)
  (if (null rows)
      '()
      (if (funcall pred (car rows))
          (cons (car rows) (echospace-filter-rows (cdr rows) pred))
          (echospace-filter-rows (cdr rows) pred))))

(defun echospace-take-last (xs n)
  (let ((len (length xs)))
    (if (<= len n)
        xs
        (nthcdr (- len n) xs))))

(defun echospace-sort-split-halves (rows)
  (let ((slow rows)
        (fast rows)
        (left '()))
    (while (and (not (null fast)) (not (null (cdr fast))))
      (setq left (cons (car slow) left))
      (setq slow (cdr slow))
      (setq fast (cdr (cdr fast))))
    (list (echospace-reverse left) slow)))

(defun echospace-sort-merge (left right key-fn desc)
  (cond
   ((null left) right)
   ((null right) left)
   (t
    (let ((lk (funcall key-fn (car left)))
          (rk (funcall key-fn (car right))))
      (if (if desc (> lk rk) (<= lk rk))
          (cons (car left) (echospace-sort-merge (cdr left) right key-fn desc))
          (cons (car right) (echospace-sort-merge left (cdr right) key-fn desc)))))))

(defun echospace-sort-by (rows key-fn desc)
  (if (or (null rows) (null (cdr rows)))
      rows
      (let* ((halves (echospace-sort-split-halves rows))
             (left (echospace-sort-by (first halves) key-fn desc))
             (right (echospace-sort-by (second halves) key-fn desc)))
        (echospace-sort-merge left right key-fn desc))))

(defun echospace-log-root-dir ()
  (let ((v (getenv "ISL_ECHOSPACE_LOG_DIR")))
    (if (or (null v) (= (length v) 0))
        "/tmp/isl-echospace-logs"
        v)))

(defun echospace-ensure-log-dir ()
  (os-mkdir-p (echospace-log-root-dir))
  (os-chmod "755" (echospace-log-root-dir)))

(defun echospace-request-id ()
  *echospace-request-id*)

(defun echospace-init-request-id ()
  (setq *echospace-request-id*
        (string-append "req-" (format nil "~A" (get-universal-time)) "-" (os-generate-token)))
  (echospace-add-response-header "X-Request-Id" *echospace-request-id*))

(defun echospace-log-line-path ()
  (string-append (echospace-log-root-dir) "/echospace-app.log"))

(defun echospace-append-log-line (line)
  (echospace-ensure-log-dir)
  (with-open-file (s (echospace-log-line-path)
                     :direction :output
                     :if-exists :append
                     :if-does-not-exist :create)
    (format s "~A~%" line)))

(defun echospace-write-log (level event fields-json)
  (echospace-append-log-line
   (string-append
    "{\"ts\":\"" (echospace-json-escape (os-date-utc-iso8601))
    "\",\"level\":\"" (echospace-json-escape level)
    "\",\"event\":\"" (echospace-json-escape event)
    "\",\"request_id\":\"" (echospace-json-escape (echospace-request-id))
    "\",\"method\":\"" (echospace-json-escape (echospace-request-method))
    "\",\"path\":\"" (echospace-json-escape (echospace-env-or-empty "PATH_INFO"))
    "\",\"user\":\"" (echospace-json-escape (echospace-current-username))
    "\",\"fields\":" fields-json
    "}")))

(defun echospace-make-log-fields-2 (k1 v1 k2 v2)
  (string-append
   "{\"" k1 "\":\"" (echospace-json-escape v1)
   "\",\"" k2 "\":\"" (echospace-json-escape v2) "\"}"))

(defun echospace-request-cookie-header ()
  (echospace-env-or-empty "HTTP_COOKIE"))

(defun echospace-parse-cookie-value (cookie-name)
  (let ((parts (echospace-split-on (echospace-request-cookie-header) ";"))
        (found ""))
    (dolist (part parts)
      (let* ((trimmed (echospace-trim-ws part))
             (kv (echospace-split-once trimmed "="))
             (name (echospace-trim-ws (first kv)))
             (value (second kv)))
        (if (and (string= found "") (string= name cookie-name))
            (setq found value)
            nil)))
    found))

(defun echospace-cookie-path ()
  (echospace-app-base))

(defun echospace-session-cookie-name ()
  "isl_echospace_session")

(defun echospace-session-cookie-value ()
  (echospace-parse-cookie-value (echospace-session-cookie-name)))

(defun echospace-clear-session-cookie-header ()
  (echospace-add-response-header
   "Set-Cookie"
   (string-append (echospace-session-cookie-name)
                  "=; Path=" (echospace-cookie-path)
                  "; HttpOnly; SameSite=Lax; Max-Age=0")))

(defun echospace-set-session-cookie-header (token)
  (echospace-add-response-header
   "Set-Cookie"
   (string-append (echospace-session-cookie-name)
                  "=" token
                  "; Path=" (echospace-cookie-path)
                  "; HttpOnly; SameSite=Lax")))

(defun echospace-safe-redirect-target (target)
  (let ((value (echospace-safe-text target)))
    (if (or (echospace-blank-text-p value)
            (not (echospace-starts-with value (echospace-app-base)))
            (not (null (string-index "\r" value)))
            (not (null (string-index "\n" value))))
        (echospace-app-base)
        value)))

(defun echospace-current-request-target ()
  (let ((path (echospace-env-or-empty "PATH_INFO"))
        (query (echospace-env-or-empty "QUERY_STRING")))
    (if (string= query "")
        (string-append (echospace-app-base) (if (string= path "") "" path))
        (string-append (echospace-app-base) (if (string= path "") "" path) "?" query))))

(defun echospace-clear-response-extra-headers ()
  (setq *echospace-response-extra-headers* '()))

(defun echospace-add-response-header (name value)
  (setq *echospace-response-extra-headers*
        (cons (list name value) *echospace-response-extra-headers*)))

(defun echospace-print-extra-headers ()
  (dolist (header (echospace-reverse *echospace-response-extra-headers*))
    (format t "~A: ~A~%" (first header) (second header))))

(defun echospace-ensure-security-headers ()
  (echospace-add-response-header "X-Frame-Options" "DENY")
  (echospace-add-response-header "X-Content-Type-Options" "nosniff")
  (echospace-add-response-header "Referrer-Policy" "same-origin")
  nil)

(defun echospace-current-user ()
  *echospace-current-user*)

(defun echospace-current-username ()
  (if (null *echospace-current-user*) "" (first *echospace-current-user*)))

(defun echospace-current-display-name ()
  (if (null *echospace-current-user*) "" (second *echospace-current-user*)))

(defun echospace-current-role ()
  (if (null *echospace-current-user*) "" (third *echospace-current-user*)))

(defun echospace-current-session-token ()
  (if (null *echospace-current-user*) "" (fourth *echospace-current-user*)))

(defun echospace-current-csrf-token ()
  (if (null *echospace-current-user*) "" (fifth *echospace-current-user*)))

(defun echospace-logged-in-p ()
  (not (null *echospace-current-user*)))

(defun echospace-role-rank (role-name)
  (cond
   ((string= role-name "admin") 20)
   ((string= role-name "member") 10)
   (t 0)))

(defun echospace-role-allowed-p (actual required)
  (>= (echospace-role-rank actual) (echospace-role-rank required)))

(defun echospace-admin-p ()
  (echospace-role-allowed-p (echospace-current-role) "admin"))

(defun echospace-db-root ()
  (let ((v (getenv "ISL_ECHOSPACE_DB_ROOT")))
    (if (or (null v) (= (length v) 0))
        "./examples/dbms/storage/echospace"
        v)))

(defun echospace-configure-dbms-root ()
  (setenv "DBMS_STORAGE_ROOT" (echospace-db-root))
  (setenv "DBMS_ALLOW_IMPLICIT_ADMIN" "1")
  (dbms-set-active-profile *dbms-profile-wiki-compat-v1*))

(defun echospace-db-bootstrap-cache-valid-p ()
  (and *echospace-db-bootstrap-ready*
       (string= *echospace-db-bootstrap-root* (echospace-db-root))))

(defun echospace-mark-db-bootstrap-ready! ()
  (setq *echospace-db-bootstrap-root* (echospace-db-root))
  (setq *echospace-db-bootstrap-ready* t))

(defun echospace-db-result-error-message (result fallback)
  (if (and (dbms-result-p result)
           (eq (second result) 'error)
           (dbms-error-p (third result)))
      (third (third result))
      fallback))

(defun echospace-db-exec! (catalog sql)
  (echospace-configure-dbms-root)
  (let ((result (dbms-exec-sql catalog sql)))
    (if (and (dbms-result-p result)
             (not (eq (second result) 'error)))
        result
        (error (echospace-db-result-error-message result "dbms exec failed") sql))))

(defun echospace-db-query-rows (catalog sql)
  (echospace-configure-dbms-root)
  (let ((result (dbms-exec-sql catalog sql)))
    (if (and (dbms-result-p result)
             (eq (second result) 'rows))
        (second (third result))
        (error (echospace-db-result-error-message result "dbms query failed") sql))))

(defun echospace-db-table-cache-get (table-name)
  (let ((rest *echospace-db-table-cache*)
        (found '()))
    (while (and (null found) (not (null rest)))
      (let ((entry (car rest)))
        (if (string= (first entry) table-name)
            (setq found (second entry))
            nil))
      (setq rest (cdr rest)))
    found))

(defun echospace-db-table-cache-put (table-name rows)
  (setq *echospace-db-table-cache*
        (cons (list table-name rows)
              (echospace-filter-rows
               *echospace-db-table-cache*
               (lambda (entry) (not (string= (first entry) table-name))))))
  rows)

(defun echospace-clear-db-table-cache ()
  (setq *echospace-db-table-cache* '()))

(defun echospace-db-table-rows (table-name)
  (echospace-configure-dbms-root)
  (let ((cached (echospace-db-table-cache-get table-name)))
    (if (null cached)
        (let ((state (dbms-engine-load-table-rows table-name)))
          (if (dbms-error-p state)
              '()
              (echospace-db-table-cache-put table-name (third state))))
        cached)))

(defun echospace-db-update-row-fields (row updates)
  (let ((vals (dbms-row-values row)))
    (dolist (pair updates)
      (setq vals (dbms-row-values-set vals (first pair) (second pair))))
    (dbms-make-row (dbms-row-id row) vals)))

(defun echospace-db-save-table-rows! (table-name rows)
  (echospace-configure-dbms-root)
  (let ((saved (dbms-engine-save-table-rows table-name rows)))
    (if (dbms-error-p saved)
        (error "dbms save failed" table-name)
        (progn
          (echospace-db-table-cache-put table-name rows)
          saved))))

(defun echospace-db-update-table-rows! (table-name pred update-fn)
  (let ((rows (echospace-db-table-rows table-name))
        (out '())
        (count 0))
    (dolist (row rows)
      (if (funcall pred row)
          (progn
            (setq out (cons (funcall update-fn row) out))
            (setq count (+ count 1)))
          (setq out (cons row out))))
    (echospace-db-save-table-rows! table-name (echospace-reverse out))
    count))

(defun echospace-db-row-value (row column-name)
  (let ((v (dbms-row-get-value row column-name)))
    (if (eq v 'NULL)
        ""
        (if (stringp v) v (format nil "~A" v)))))

(defun echospace-db-row-nullable (row column-name)
  (dbms-row-get-value row column-name))

(defun echospace-db-now-text ()
  (format nil "~A" (get-universal-time)))

(defun echospace-db-text-sql (value)
  (string-append "'" (let ((text (echospace-safe-text value))
                           (i 0)
                           (out ""))
                       (while (< i (length text))
                         (let ((ch (substring text i (+ i 1))))
                           (if (string= ch "'")
                               (setq out (string-append out "''"))
                               (setq out (string-append out ch))))
                         (setq i (+ i 1)))
                       out) "'"))

(defun echospace-ensure-schema! (catalog)
  (dolist (sql
           (list
            "CREATE TABLE IF NOT EXISTS users (id BIGSERIAL PRIMARY KEY, username TEXT NOT NULL, display_name TEXT NOT NULL, password_hash TEXT NOT NULL, role_name TEXT NOT NULL, enabled BOOL NOT NULL, created_at TIMESTAMPTZ NOT NULL, updated_at TIMESTAMPTZ NOT NULL, last_login_at TIMESTAMPTZ);"
            "CREATE TABLE IF NOT EXISTS workspaces (id BIGSERIAL PRIMARY KEY, slug TEXT NOT NULL, name TEXT NOT NULL, created_at TIMESTAMPTZ NOT NULL);"
            "CREATE TABLE IF NOT EXISTS workspace_members (id BIGSERIAL PRIMARY KEY, workspace_id BIGINT NOT NULL, user_id BIGINT NOT NULL, created_at TIMESTAMPTZ NOT NULL);"
            "CREATE TABLE IF NOT EXISTS channels (id BIGSERIAL PRIMARY KEY, workspace_id BIGINT NOT NULL, slug TEXT NOT NULL, name TEXT NOT NULL, topic TEXT, display_order INTEGER NOT NULL, created_at TIMESTAMPTZ NOT NULL);"
            "CREATE TABLE IF NOT EXISTS messages (id BIGSERIAL PRIMARY KEY, workspace_id BIGINT NOT NULL, channel_id BIGINT NOT NULL, author_user_id BIGINT NOT NULL, body TEXT NOT NULL, created_at TIMESTAMPTZ NOT NULL);"
            "CREATE TABLE IF NOT EXISTS user_sessions (id BIGSERIAL PRIMARY KEY, user_id BIGINT NOT NULL, session_token TEXT NOT NULL, csrf_token TEXT NOT NULL, created_at TIMESTAMPTZ NOT NULL, last_seen_at TIMESTAMPTZ NOT NULL, expires_at TIMESTAMPTZ NOT NULL, revoked_at TIMESTAMPTZ);"))
    (echospace-db-exec! catalog sql)))

(defun echospace-password-rounds ()
  32)

(defun echospace-password-prefix ()
  "kdfv1$")

(defun echospace-password-encoded-p (value)
  (echospace-starts-with (echospace-safe-text value) (echospace-password-prefix)))

(defun echospace-password-hash (password)
  (let* ((salt (string-append "echospace:" (os-generate-token)))
         (rounds (echospace-password-rounds))
         (digest (dbms-security-hash-password-with-salt salt password rounds)))
    (string-append (echospace-password-prefix)
                   salt
                   "$"
                   (format nil "~A" rounds)
                   "$"
                   digest)))

(defun echospace-password-valid-p (stored password)
  (let ((current (echospace-safe-text stored)))
    (if (echospace-password-encoded-p current)
        (let ((parts (echospace-split-on current "$")))
          (and (= (length parts) 4)
               (string= (first parts) "kdfv1")
               (string= (fourth parts)
                        (dbms-security-hash-password-with-salt
                         (second parts)
                         password
                         (echospace-parse-int-safe (third parts))))))
        (string= current password))))

(defun echospace-initial-admin-password ()
  (let ((v (getenv "ECHOSPACE_INITIAL_ADMIN_PASSWORD")))
    (if (or (null v) (= (length v) 0))
        "admin"
        v)))

(defun echospace-find-user-row (username)
  (echospace-find-first-row
   (echospace-db-table-rows "users")
   (lambda (row) (string= (echospace-db-row-value row "username") username))))

(defun echospace-user-id-by-username (username)
  (let ((row (echospace-find-user-row username)))
    (if (null row) '() (echospace-db-row-nullable row "id"))))

(defun echospace-find-workspace-row (workspace-slug)
  (echospace-find-first-row
   (echospace-db-table-rows "workspaces")
   (lambda (row) (string= (echospace-db-row-value row "slug") workspace-slug))))

(defun echospace-workspace-id-by-slug (workspace-slug)
  (let ((row (echospace-find-workspace-row workspace-slug)))
    (if (null row) '() (echospace-db-row-nullable row "id"))))

(defun echospace-find-channel-row-by-workspace-id (workspace-id channel-slug)
  (echospace-find-first-row
   (echospace-db-table-rows "channels")
   (lambda (row)
     (and (= (echospace-db-row-nullable row "workspace_id") workspace-id)
          (string= (echospace-db-row-value row "slug") channel-slug)))))

(defun echospace-channel-id-by-workspace-id (workspace-id channel-slug)
  (let ((row (echospace-find-channel-row-by-workspace-id workspace-id channel-slug)))
    (if (null row) '() (echospace-db-row-nullable row "id"))))

(defun echospace-membership-exists-p (workspace-id user-id)
  (not
   (null
    (echospace-find-first-row
     (echospace-db-table-rows "workspace_members")
     (lambda (row)
       (and (= (echospace-db-row-nullable row "workspace_id") workspace-id)
            (= (echospace-db-row-nullable row "user_id") user-id)))))))

(defun echospace-user-can-access-workspace-p (username workspace-slug)
  (let ((workspace-id (echospace-workspace-id-by-slug workspace-slug))
        (user-id (echospace-user-id-by-username username)))
    (if (or (null workspace-id) (null user-id))
        nil
        (if (string= (echospace-current-role) "admin")
            t
            (echospace-membership-exists-p workspace-id user-id)))))

(defun echospace-fetch-workspaces-for-user (username)
  (let ((rows '())
        (user-id (echospace-user-id-by-username username)))
    (dolist (workspace (echospace-db-table-rows "workspaces"))
      (if (or (string= (echospace-current-role) "admin")
              (echospace-membership-exists-p (echospace-db-row-nullable workspace "id") user-id))
          (setq rows
                (cons (list (echospace-db-row-value workspace "slug")
                            (echospace-db-row-value workspace "name")
                            (echospace-db-row-nullable workspace "id"))
                      rows))
          nil))
    (echospace-sort-by (echospace-reverse rows) (lambda (row) (second row)) nil)))

(defun echospace-fetch-channels-for-workspace-id (workspace-id)
  (let ((rows '()))
    (dolist (channel (echospace-db-table-rows "channels"))
      (if (= (echospace-db-row-nullable channel "workspace_id") workspace-id)
          (setq rows
                (cons (list (echospace-db-row-value channel "slug")
                            (echospace-db-row-value channel "name")
                            (echospace-db-row-value channel "topic")
                            (echospace-db-row-nullable channel "display_order")
                            (echospace-db-row-nullable channel "id"))
                      rows))
          nil))
    (echospace-sort-by (echospace-reverse rows) (lambda (row) (fourth row)) nil)))

(defun echospace-find-user-row-by-id (user-id)
  (echospace-find-first-row
   (echospace-db-table-rows "users")
   (lambda (row) (= (echospace-db-row-nullable row "id") user-id))))

(defun echospace-fetch-members-for-workspace-id (workspace-id)
  (let ((rows '()))
    (dolist (membership (echospace-db-table-rows "workspace_members"))
      (if (= (echospace-db-row-nullable membership "workspace_id") workspace-id)
          (let ((user-row (echospace-find-user-row-by-id (echospace-db-row-nullable membership "user_id"))))
            (if (null user-row)
                nil
                (setq rows
                      (cons (list (echospace-db-row-value user-row "username")
                                  (echospace-db-row-value user-row "display_name")
                                  (echospace-db-row-value user-row "role_name")
                                  "online")
                            rows))))
          nil))
    (echospace-sort-by (echospace-reverse rows) (lambda (row) (second row)) nil)))

(defun echospace-fetch-message-records-for-channel-id (workspace-id channel-id)
  (let ((rows '()))
    (dolist (message-row (echospace-db-table-rows "messages"))
      (if (and (= (echospace-db-row-nullable message-row "workspace_id") workspace-id)
               (= (echospace-db-row-nullable message-row "channel_id") channel-id))
          (let ((user-row (echospace-find-user-row-by-id (echospace-db-row-nullable message-row "author_user_id"))))
            (setq rows
                  (cons (list (echospace-db-row-nullable message-row "id")
                              (if (null user-row) "unknown" (echospace-db-row-value user-row "username"))
                              (if (null user-row) "Unknown" (echospace-db-row-value user-row "display_name"))
                              (echospace-db-row-value message-row "body")
                              (echospace-db-row-value message-row "created_at"))
                        rows)))
          nil))
    (echospace-sort-by (echospace-reverse rows) (lambda (row) (first row)) nil)))

(defun echospace-fetch-recent-messages-for-channel-id (workspace-id channel-id limit)
  (echospace-take-last
   (echospace-fetch-message-records-for-channel-id workspace-id channel-id)
   limit))

(defun echospace-fetch-messages-after-id (workspace-id channel-id after-id)
  (echospace-filter-rows
   (echospace-fetch-message-records-for-channel-id workspace-id channel-id)
   (lambda (row) (> (first row) after-id))))

(defun echospace-default-channel-path-for-user (username)
  (let ((workspaces (echospace-fetch-workspaces-for-user username)))
    (if (null workspaces)
        ""
        (let* ((workspace-slug (first (first workspaces)))
               (workspace-id (third (first workspaces)))
               (channels (echospace-fetch-channels-for-workspace-id workspace-id)))
          (if (null channels)
              ""
              (string-append (echospace-app-base)
                             "/workspaces/" workspace-slug
                             "/channels/" (first (first channels))))))))

(defun echospace-complete-seed-data! (catalog)
  (let ((now (echospace-db-now-text)))
    (if (null (echospace-find-user-row "admin"))
        (echospace-db-exec!
         catalog
         (string-append
          "INSERT INTO users (username, display_name, password_hash, role_name, enabled, created_at, updated_at) VALUES ("
          "'admin', 'Administrator', "
          (echospace-db-text-sql (echospace-password-hash (echospace-initial-admin-password)))
          ", 'admin', TRUE, "
          (echospace-db-text-sql now) ", "
          (echospace-db-text-sql now) ");"))
        nil)
    (echospace-clear-db-table-cache)
    (if (null (echospace-find-user-row "demo"))
        (echospace-db-exec!
         catalog
         (string-append
          "INSERT INTO users (username, display_name, password_hash, role_name, enabled, created_at, updated_at) VALUES ("
          "'demo', 'Demo User', "
          (echospace-db-text-sql (echospace-password-hash "demo"))
          ", 'member', TRUE, "
          (echospace-db-text-sql now) ", "
          (echospace-db-text-sql now) ");"))
        nil)
    (echospace-clear-db-table-cache)
    (if (null (echospace-find-workspace-row "welcome"))
        (echospace-db-exec!
         catalog
         (string-append
          "INSERT INTO workspaces (slug, name, created_at) VALUES ('welcome', 'Welcome Space', "
          (echospace-db-text-sql now) ");"))
        nil)
    (echospace-clear-db-table-cache)
    (let ((workspace-id (echospace-workspace-id-by-slug "welcome"))
          (admin-id (echospace-user-id-by-username "admin"))
          (demo-id (echospace-user-id-by-username "demo")))
      (if (and (not (null workspace-id)) (not (null admin-id))
               (not (echospace-membership-exists-p workspace-id admin-id)))
          (echospace-db-exec!
           catalog
           (string-append
            "INSERT INTO workspace_members (workspace_id, user_id, created_at) VALUES ("
            (format nil "~A" workspace-id) ", "
            (format nil "~A" admin-id) ", "
            (echospace-db-text-sql now) ");"))
          nil)
      (echospace-clear-db-table-cache)
      (if (and (not (null workspace-id)) (not (null demo-id))
               (not (echospace-membership-exists-p workspace-id demo-id)))
          (echospace-db-exec!
           catalog
           (string-append
            "INSERT INTO workspace_members (workspace_id, user_id, created_at) VALUES ("
            (format nil "~A" workspace-id) ", "
            (format nil "~A" demo-id) ", "
            (echospace-db-text-sql now) ");"))
          nil)
      (echospace-clear-db-table-cache)
      (if (and (not (null workspace-id))
               (null (echospace-find-channel-row-by-workspace-id workspace-id "general")))
          (echospace-db-exec!
           catalog
           (string-append
            "INSERT INTO channels (workspace_id, slug, name, topic, display_order, created_at) VALUES ("
            (format nil "~A" workspace-id) ", 'general', 'general', 'Start here', 10, "
            (echospace-db-text-sql now) ");"))
          nil)
      (echospace-clear-db-table-cache)
      (if (and (not (null workspace-id))
               (null (echospace-find-channel-row-by-workspace-id workspace-id "random")))
          (echospace-db-exec!
           catalog
           (string-append
            "INSERT INTO channels (workspace_id, slug, name, topic, display_order, created_at) VALUES ("
            (format nil "~A" workspace-id) ", 'random', 'random', 'Loose conversation', 20, "
            (echospace-db-text-sql now) ");"))
          nil)
      (echospace-clear-db-table-cache)
      (let ((channel-id (echospace-channel-id-by-workspace-id workspace-id "general")))
        (if (and (not (null channel-id))
                 (= (length (echospace-fetch-message-records-for-channel-id workspace-id channel-id)) 0))
            (progn
              (echospace-db-exec!
               catalog
               (string-append
                "INSERT INTO messages (workspace_id, channel_id, author_user_id, body, created_at) VALUES ("
                (format nil "~A" workspace-id) ", "
                (format nil "~A" channel-id) ", "
                (format nil "~A" admin-id) ", "
                (echospace-db-text-sql "Welcome to echospace. This is the first channel.") ", "
                (echospace-db-text-sql now) ");"))
              (echospace-db-exec!
               catalog
               (string-append
                "INSERT INTO messages (workspace_id, channel_id, author_user_id, body, created_at) VALUES ("
                (format nil "~A" workspace-id) ", "
                (format nil "~A" channel-id) ", "
                (format nil "~A" demo-id) ", "
                (echospace-db-text-sql "Try posting from the message box below.") ", "
                (echospace-db-text-sql now) ");")))
            nil)))))

(defun echospace-db-open ()
  (echospace-configure-dbms-root)
  (let ((catalog (dbms-engine-init)))
    (if (echospace-db-bootstrap-cache-valid-p)
        t
        (progn
          (echospace-ensure-schema! catalog)
          (echospace-complete-seed-data! catalog)
          (echospace-mark-db-bootstrap-ready!)))
    t))

(defun echospace-session-duration-days ()
  7)

(defun echospace-issue-session-token ()
  (os-generate-token))

(defun echospace-issue-csrf-token ()
  (os-generate-token))

(defun echospace-fetch-user-for-login (username password)
  (let ((row (echospace-find-user-row username)))
    (if (or (null row)
            (not (eq (echospace-db-row-nullable row "enabled") t))
            (not (echospace-password-valid-p (echospace-db-row-value row "password_hash") password)))
        '()
        (list (echospace-db-row-value row "username")
              (echospace-db-row-value row "display_name")
              (echospace-db-row-value row "role_name")))))

(defun echospace-fetch-user-by-session-token (token)
  (let ((session-row
         (echospace-find-first-row
          (echospace-db-table-rows "user_sessions")
          (lambda (row)
            (and (string= (echospace-db-row-value row "session_token") token)
                 (string= (echospace-db-row-value row "revoked_at") "")
                 (> (echospace-parse-int-safe (echospace-db-row-value row "expires_at"))
                    (get-universal-time)))))))
    (if (null session-row)
        '()
        (let ((user-row (echospace-find-user-row-by-id (echospace-db-row-nullable session-row "user_id"))))
          (if (or (null user-row)
                  (not (eq (echospace-db-row-nullable user-row "enabled") t)))
              '()
              (list (echospace-db-row-value user-row "username")
                    (echospace-db-row-value user-row "display_name")
                    (echospace-db-row-value user-row "role_name")
                    (echospace-db-row-value session-row "session_token")
                    (echospace-db-row-value session-row "csrf_token")))))))

(defun echospace-create-user-session! (username token csrf-token)
  (let ((user-id (echospace-user-id-by-username username))
        (now (echospace-db-now-text))
        (expires-at (format nil "~A" (+ (get-universal-time) (* 86400 (echospace-session-duration-days))))))
    (if (null user-id)
        nil
        (echospace-db-exec!
         nil
         (string-append
          "INSERT INTO user_sessions (user_id, session_token, csrf_token, created_at, last_seen_at, expires_at, revoked_at) VALUES ("
          (format nil "~A" user-id) ", "
          (echospace-db-text-sql token) ", "
          (echospace-db-text-sql csrf-token) ", "
          (echospace-db-text-sql now) ", "
          (echospace-db-text-sql now) ", "
          (echospace-db-text-sql expires-at) ", "
          "NULL);")))))

(defun echospace-revoke-session-token! (token)
  (if (echospace-blank-text-p token)
      nil
      (echospace-db-update-table-rows!
       "user_sessions"
       (lambda (row) (string= (echospace-db-row-value row "session_token") token))
       (lambda (row)
         (echospace-db-update-row-fields
          row
          (list (list "revoked_at" (echospace-db-now-text))))))))

(defun echospace-touch-session-token! (token)
  (if (echospace-blank-text-p token)
      nil
      (echospace-db-update-table-rows!
       "user_sessions"
       (lambda (row) (string= (echospace-db-row-value row "session_token") token))
       (lambda (row)
         (echospace-db-update-row-fields
          row
          (list (list "last_seen_at" (echospace-db-now-text))))))))

(defun echospace-update-last-login-at! (username)
  (echospace-db-update-table-rows!
   "users"
   (lambda (row) (string= (echospace-db-row-value row "username") username))
   (lambda (row)
     (echospace-db-update-row-fields
      row
      (list (list "last_login_at" (echospace-db-now-text))
            (list "updated_at" (echospace-db-now-text)))))))

(defun echospace-load-current-user ()
  (let ((token (echospace-session-cookie-value)))
    (if (echospace-blank-text-p token)
        (setq *echospace-current-user* '())
        (let ((user (echospace-fetch-user-by-session-token token)))
          (if (null user)
              (progn
                (setq *echospace-current-user* '())
                (echospace-clear-session-cookie-header))
              (progn
                (setq *echospace-current-user* user)
                (echospace-touch-session-token! token)))))))

(defun echospace-read-request-body ()
  (if (stringp *echospace-request-body-override*)
      *echospace-request-body-override*
      (with-open-file (s "/dev/stdin" :direction :input)
        (let ((line (read-line s #f))
              (acc "")
              (first-line t))
          (while (not (null line))
            (if first-line
                (progn
                  (setq acc line)
                  (setq first-line nil))
                (setq acc (string-append acc "\n" line)))
            (setq line (read-line s #f)))
          acc))))

(defun echospace-render-csrf-hidden-input ()
  (format t "<input type=\"hidden\" name=\"csrf_token\" value=\"~A\">~%"
          (echospace-html-escape (echospace-current-csrf-token))))

(defun echospace-valid-csrf-token-p (raw-body)
  (let ((token (echospace-parse-form-field raw-body "csrf_token")))
    (and (not (echospace-blank-text-p token))
         (not (echospace-blank-text-p (echospace-current-csrf-token)))
         (string= token (echospace-current-csrf-token)))))

(defun echospace-read-form-body-or-render (require-csrf)
  (if (not (string= (echospace-request-method) "POST"))
      '()
      (let ((raw-body (echospace-read-request-body)))
        (if (and require-csrf (not (echospace-valid-csrf-token-p raw-body)))
            (progn
              (echospace-write-log "warn" "csrf.invalid"
                                   (echospace-make-log-fields-2 "target" (echospace-current-request-target)
                                                                "username" (echospace-current-username)))
              (echospace-render-forbidden "invalid csrf token")
              '())
            raw-body))))

(defun echospace-login-target-url ()
  (string-append (echospace-app-base) "/login?next="
                 (echospace-url-encode-lite (echospace-current-request-target))))

(defun echospace-route-required-role (method segments)
  (cond
   ((null segments) "member")
   ((and (= (length segments) 1)
         (string= (first segments) "login"))
    "")
   ((and (= (length segments) 1)
         (string= (first segments) "healthz"))
    "")
   ((and (= (length segments) 1)
         (string= (first segments) "logout"))
    "member")
   ((string= (first segments) "admin")
    "admin")
   ((string= (first segments) "api")
    "member")
   ((string= (first segments) "workspaces")
    "member")
   (t "member")))

(defun echospace-ensure-authorized (method segments)
  (let ((required (echospace-route-required-role method segments)))
    (if (string= required "")
        t
        (if (not (echospace-logged-in-p))
            (progn
              (echospace-render-redirect (echospace-login-target-url))
              nil)
            (if (echospace-role-allowed-p (echospace-current-role) required)
                t
                (progn
                  (echospace-render-forbidden "insufficient role")
                  nil))))))

(defun echospace-render-headers (status content-type)
  (echospace-ensure-security-headers)
  (format t "Status: ~A~%" status)
  (format t "Content-Type: ~A~%" content-type)
  (echospace-print-extra-headers)
  (format t "~%"))

(defun echospace-render-redirect (location)
  (echospace-render-headers "303 See Other" "text/html; charset=UTF-8")
  (format t "<!doctype html><meta http-equiv=\"refresh\" content=\"0;url=~A\"><a href=\"~A\">Redirect</a>"
          (echospace-html-escape location)
          (echospace-html-escape location)))

(defun echospace-render-forbidden (message)
  (echospace-render-headers "403 Forbidden" "text/html; charset=UTF-8")
  (format t "<!doctype html><title>Forbidden</title><main style=\"font-family:system-ui;margin:2rem\"><h1>Forbidden</h1><p>~A</p></main>"
          (echospace-html-escape message)))

(defun echospace-render-not-found ()
  (echospace-render-headers "404 Not Found" "text/html; charset=UTF-8")
  (format t "<!doctype html><title>Not Found</title><main style=\"font-family:system-ui;margin:2rem\"><h1>Not Found</h1></main>"))

(defun echospace-render-error (message)
  (echospace-render-headers "500 Internal Server Error" "text/html; charset=UTF-8")
  (format t "<!doctype html><title>Error</title><main style=\"font-family:system-ui;margin:2rem\"><h1>Application Error</h1><pre>~A</pre></main>"
          (echospace-html-escape message)))

(defun echospace-render-healthz (db-ok)
  (echospace-render-headers "200 OK" "application/json; charset=UTF-8")
  (format t "{\"status\":\"~A\",\"db\":\"~A\",\"request_id\":\"~A\"}"
          (if db-ok "ok" "error")
          (if db-ok "ok" "error")
          (echospace-json-escape (echospace-request-id))))

(defun echospace-render-json-members (members)
  (echospace-render-headers "200 OK" "application/json; charset=UTF-8")
  (format t "{\"members\":[")
  (let ((first-item t))
    (dolist (member members)
      (if first-item
          (setq first-item nil)
          (format t ","))
      (format t "{\"username\":\"~A\",\"display_name\":\"~A\",\"role\":\"~A\",\"status\":\"~A\"}"
              (echospace-json-escape (first member))
              (echospace-json-escape (second member))
              (echospace-json-escape (third member))
              (echospace-json-escape (fourth member)))))
  (format t "],\"request_id\":\"~A\"}" (echospace-json-escape (echospace-request-id))))

(defun echospace-render-json-messages (messages)
  (echospace-render-headers "200 OK" "application/json; charset=UTF-8")
  (format t "{\"messages\":[")
  (let ((first-item t))
    (dolist (message-row messages)
      (if first-item
          (setq first-item nil)
          (format t ","))
      (format t "{\"id\":~A,\"username\":\"~A\",\"display_name\":\"~A\",\"body\":\"~A\",\"created_at\":\"~A\"}"
              (first message-row)
              (echospace-json-escape (second message-row))
              (echospace-json-escape (third message-row))
              (echospace-json-escape (fourth message-row))
              (echospace-json-escape (fifth message-row)))))
  (format t "],\"request_id\":\"~A\"}" (echospace-json-escape (echospace-request-id))))

(defun echospace-render-layout-start (title)
  (echospace-render-headers "200 OK" "text/html; charset=UTF-8")
  (format t "<!doctype html>~%")
  (format t "<html><head><meta charset=\"utf-8\"><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">~%")
  (format t "<title>~A</title>~%" (echospace-html-escape title))
  (format t "<style>:root{--bg:#1f232b;--bg2:#171a21;--bg3:#2a2f3a;--bg4:#11151c;--panel:#222733;--text:#eef3ff;--muted:#9aa7bf;--accent:#57c7ff;--accent2:#83f2c2;--danger:#ff7a7a;--border:#394155;--chip:#31384a}*{box-sizing:border-box}body{margin:0;font-family:ui-sans-serif,system-ui,-apple-system,BlinkMacSystemFont,\"Segoe UI\",sans-serif;background:radial-gradient(circle at top left,#283140 0,#1b1f28 45%,#12151c 100%);color:var(--text)}a{color:var(--accent);text-decoration:none}a:hover{text-decoration:underline}.topbar{display:flex;justify-content:space-between;align-items:center;padding:.85rem 1rem;background:rgba(17,21,28,.75);backdrop-filter:blur(10px);border-bottom:1px solid var(--border);position:sticky;top:0;z-index:10}.brand{font-weight:800;letter-spacing:.04em}.shell{display:grid;grid-template-columns:84px 240px minmax(0,1fr) 240px;min-height:calc(100vh - 56px)}.workspace-nav{background:var(--bg4);padding:1rem .75rem;border-right:1px solid var(--border)}.workspace-pill{display:flex;align-items:center;justify-content:center;width:52px;height:52px;border-radius:18px;margin:0 auto .8rem;background:var(--chip);color:var(--text);font-weight:700;transition:.18s}.workspace-pill.active,.workspace-pill:hover{background:linear-gradient(135deg,var(--accent),#7be2ff);color:#071018;border-radius:14px;text-decoration:none}.sidebar{background:var(--bg2);border-right:1px solid var(--border);padding:1rem}.sidebar h2,.member-panel h2{margin:0 0 .8rem;font-size:.8rem;text-transform:uppercase;letter-spacing:.08em;color:var(--muted)}.channel-link{display:block;padding:.55rem .75rem;border-radius:10px;color:var(--text);margin-bottom:.35rem}.channel-link small{display:block;color:var(--muted);margin-top:.2rem}.channel-link.active,.channel-link:hover{background:var(--chip);text-decoration:none}.main{display:grid;grid-template-rows:auto minmax(0,1fr) auto;background:rgba(34,39,51,.7)}.channel-header{padding:1rem 1.2rem;border-bottom:1px solid var(--border);background:rgba(34,39,51,.85)}.channel-header h1{margin:0;font-size:1.05rem}.channel-header p{margin:.35rem 0 0;color:var(--muted)}.timeline{padding:1rem 1.2rem;overflow:auto}.message{padding:.8rem .9rem;border:1px solid rgba(87,199,255,.08);background:rgba(18,21,28,.42);border-radius:14px;margin-bottom:.8rem}.message-head{display:flex;gap:.65rem;align-items:baseline}.message-author{font-weight:700}.message-meta{font-size:.8rem;color:var(--muted)}.message-body{margin-top:.45rem;white-space:pre-wrap;line-height:1.5}.composer{padding:1rem 1.2rem;border-top:1px solid var(--border);background:rgba(17,21,28,.7)}.composer textarea,.login-card input,.admin-panel input,.admin-panel select{width:100%;border:1px solid var(--border);background:#0e1218;color:var(--text);border-radius:12px;padding:.8rem .9rem}.composer textarea{min-height:88px;resize:vertical}.btn{border:none;border-radius:999px;padding:.75rem 1rem;background:linear-gradient(135deg,var(--accent),var(--accent2));color:#04131a;font-weight:800;cursor:pointer}.btn.secondary{background:var(--chip);color:var(--text)}.member-panel{background:var(--bg2);border-left:1px solid var(--border);padding:1rem}.member{display:flex;justify-content:space-between;gap:.75rem;padding:.55rem .7rem;background:rgba(255,255,255,.03);border-radius:10px;margin-bottom:.45rem}.member span:last-child{color:var(--accent2);font-size:.8rem}.flash{margin:1rem 1.2rem 0;padding:.8rem .95rem;border-radius:12px}.flash.notice{background:rgba(131,242,194,.12);border:1px solid rgba(131,242,194,.28)}.flash.error{background:rgba(255,122,122,.12);border:1px solid rgba(255,122,122,.28)}.empty{color:var(--muted);padding:1.2rem;border:1px dashed var(--border);border-radius:14px}.login-wrap{min-height:100vh;display:grid;place-items:center;padding:2rem}.login-card{width:min(460px,100%);background:rgba(22,26,33,.92);border:1px solid var(--border);border-radius:24px;padding:1.4rem;box-shadow:0 18px 56px rgba(0,0,0,.35)}.login-card h1{margin-top:0}.admin-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(280px,1fr));gap:1rem}.admin-panel{background:rgba(22,26,33,.92);border:1px solid var(--border);border-radius:18px;padding:1rem}.admin-panel h2{margin-top:0}.stack>*+*{margin-top:.7rem}.table{width:100%;border-collapse:collapse}.table th,.table td{padding:.55rem .4rem;border-bottom:1px solid rgba(255,255,255,.08);text-align:left;font-size:.92rem}.muted{color:var(--muted)}@media (max-width: 980px){.shell{grid-template-columns:72px 220px minmax(0,1fr)}.member-panel{display:none}}@media (max-width: 720px){.shell{grid-template-columns:1fr}.workspace-nav,.sidebar,.member-panel{display:none}.topbar{flex-wrap:wrap;gap:.75rem}}</style>~%")
  (format t "</head><body>~%"))

(defun echospace-render-layout-end ()
  (format t "</body></html>~%"))

(defun echospace-render-topbar ()
  (format t "<header class=\"topbar\">~%")
  (format t "<div class=\"brand\"><a href=\"~A\" style=\"color:inherit;text-decoration:none\">ECHOSPACE</a></div>~%"
          (echospace-html-escape (echospace-app-base)))
  (format t "<div style=\"display:flex;gap:.75rem;align-items:center\">~%")
  (if (echospace-logged-in-p)
      (progn
        (format t "<span class=\"muted\">~A (@~A, ~A)</span>~%"
                (echospace-html-escape (echospace-current-display-name))
                (echospace-html-escape (echospace-current-username))
                (echospace-html-escape (echospace-current-role)))
        (if (echospace-admin-p)
            (format t "<a href=\"~A/admin\">Admin</a>~%" (echospace-html-escape (echospace-app-base)))
            nil)
        (format t "<form method=\"post\" action=\"~A/logout\" style=\"margin:0\">~%" (echospace-html-escape (echospace-app-base)))
        (echospace-render-csrf-hidden-input)
        (format t "<button class=\"btn secondary\" type=\"submit\">Logout</button></form>~%"))
      (format t "<a href=\"~A/login\">Login</a>~%" (echospace-html-escape (echospace-app-base))))
  (format t "</div></header>~%"))

(defun echospace-flash-notice ()
  (echospace-query-param "notice"))

(defun echospace-flash-error ()
  (echospace-query-param "error"))

(defun echospace-render-flash ()
  (if (not (echospace-blank-text-p (echospace-flash-notice)))
      (format t "<div class=\"flash notice\">~A</div>~%" (echospace-html-escape (echospace-flash-notice)))
      nil)
  (if (not (echospace-blank-text-p (echospace-flash-error)))
      (format t "<div class=\"flash error\">~A</div>~%" (echospace-html-escape (echospace-flash-error)))
      nil))

(defun echospace-channel-url (workspace-slug channel-slug)
  (string-append (echospace-app-base) "/workspaces/" workspace-slug "/channels/" channel-slug))

(defun echospace-render-login-page ()
  (let ((next (echospace-safe-redirect-target (echospace-query-param "next"))))
    (echospace-render-layout-start "Echospace Login")
    (format t "<main class=\"login-wrap\"><section class=\"login-card stack\">~%")
    (format t "<h1>Sign in to Echospace</h1><p class=\"muted\">Default accounts: <code>admin/admin</code> and <code>demo/demo</code>.</p>~%")
    (echospace-render-flash)
    (format t "<form method=\"post\" action=\"~A/login\" class=\"stack\">~%" (echospace-html-escape (echospace-app-base)))
    (format t "<input type=\"hidden\" name=\"next\" value=\"~A\">~%" (echospace-html-escape next))
    (format t "<label>Username<br><input type=\"text\" name=\"username\" autofocus></label>~%")
    (format t "<label>Password<br><input type=\"password\" name=\"password\"></label>~%")
    (format t "<button class=\"btn\" type=\"submit\">Login</button>~%")
    (format t "</form></section></main>~%")
    (echospace-render-layout-end)))

(defun echospace-render-workspace-nav (workspaces active-workspace-slug)
  (format t "<aside class=\"workspace-nav\">~%")
  (dolist (workspace workspaces)
    (let* ((slug (first workspace))
           (name (second workspace))
           (dest
            (if (null (third workspace))
                (echospace-app-base)
                (let ((channels (echospace-fetch-channels-for-workspace-id (third workspace))))
                  (if (null channels)
                      (echospace-app-base)
                      (echospace-channel-url slug (first (first channels))))))))
      (format t "<a class=\"workspace-pill~A\" href=\"~A\" title=\"~A\">~A</a>~%"
              (if (string= slug active-workspace-slug) " active" "")
              (echospace-html-escape dest)
              (echospace-html-escape name)
              (echospace-html-escape (substring name 0 (min 2 (length name)))))))
  (format t "</aside>~%"))

(defun echospace-render-channel-sidebar (workspace-name workspace-slug channels active-channel-slug)
  (format t "<aside class=\"sidebar\"><h2>~A</h2>~%" (echospace-html-escape workspace-name))
  (dolist (channel channels)
    (format t "<a class=\"channel-link~A\" href=\"~A\"><strong># ~A</strong><small>~A</small></a>~%"
            (if (string= (first channel) active-channel-slug) " active" "")
            (echospace-html-escape (echospace-channel-url workspace-slug (first channel)))
            (echospace-html-escape (second channel))
            (echospace-html-escape (third channel))))
  (format t "</aside>~%"))

(defun echospace-render-members-panel (members)
  (format t "<aside class=\"member-panel\"><h2>Members</h2><div id=\"members-panel\">~%")
  (dolist (member members)
    (format t "<div class=\"member\"><div><strong>~A</strong><div class=\"muted\">@~A · ~A</div></div><span>~A</span></div>~%"
            (echospace-html-escape (second member))
            (echospace-html-escape (first member))
            (echospace-html-escape (third member))
            (echospace-html-escape (fourth member))))
  (format t "</div></aside>~%"))

(defun echospace-render-message-item (message-row)
  (format t "<article class=\"message\" data-message-id=\"~A\">~%"
          (first message-row))
  (format t "<div class=\"message-head\"><span class=\"message-author\">~A</span><span class=\"message-meta\">@~A · ~A</span></div>~%"
          (echospace-html-escape (third message-row))
          (echospace-html-escape (second message-row))
          (echospace-html-escape (fifth message-row)))
  (format t "<div class=\"message-body\">~A</div></article>~%"
          (echospace-html-escape (fourth message-row))))

(defun echospace-render-channel-page (workspace-slug channel-slug)
  (let* ((workspace-row (echospace-find-workspace-row workspace-slug))
         (workspace-id (if (null workspace-row) '() (echospace-db-row-nullable workspace-row "id")))
         (channel-row (if (null workspace-row) '()
                          (echospace-find-channel-row-by-workspace-id workspace-id channel-slug))))
    (if (or (null workspace-row)
            (null channel-row)
            (not (echospace-user-can-access-workspace-p (echospace-current-username) workspace-slug)))
        (echospace-render-not-found)
        (let* ((workspaces (echospace-fetch-workspaces-for-user (echospace-current-username)))
               (channels (echospace-fetch-channels-for-workspace-id workspace-id))
               (members (echospace-fetch-members-for-workspace-id workspace-id))
               (messages (echospace-fetch-recent-messages-for-channel-id workspace-id (echospace-db-row-nullable channel-row "id") 50))
               (messages-api (string-append (echospace-app-base) "/api/workspaces/" workspace-slug "/channels/" channel-slug "/messages"))
               (members-api (string-append (echospace-app-base) "/api/workspaces/" workspace-slug "/members")))
          (echospace-render-layout-start
           (string-append (echospace-db-row-value workspace-row "name") " · #" (echospace-db-row-value channel-row "name")))
          (echospace-render-topbar)
          (echospace-render-flash)
          (format t "<div class=\"shell\">~%")
          (echospace-render-workspace-nav workspaces workspace-slug)
          (echospace-render-channel-sidebar
           (echospace-db-row-value workspace-row "name")
           workspace-slug
           channels
           channel-slug)
          (format t "<main class=\"main\">~%")
          (format t "<header class=\"channel-header\"><h1># ~A</h1><p>~A</p></header>~%"
                  (echospace-html-escape (echospace-db-row-value channel-row "name"))
                  (echospace-html-escape (echospace-db-row-value channel-row "topic")))
          (format t "<section class=\"timeline\" id=\"timeline\" data-api=\"~A\" data-members-api=\"~A\">~%"
                  (echospace-html-escape messages-api)
                  (echospace-html-escape members-api))
          (if (null messages)
              (format t "<div class=\"empty\">No messages yet. Say hello.</div>~%")
              (dolist (message-row messages)
                (echospace-render-message-item message-row)))
          (format t "</section>~%")
          (format t "<section class=\"composer\"><form method=\"post\" action=\"~A/messages\" class=\"stack\">~%"
                  (echospace-html-escape (echospace-channel-url workspace-slug channel-slug)))
          (echospace-render-csrf-hidden-input)
          (format t "<textarea name=\"body\" placeholder=\"Message #~A\"></textarea>~%"
                  (echospace-html-escape (echospace-db-row-value channel-row "name")))
          (format t "<div style=\"display:flex;justify-content:space-between;align-items:center;gap:1rem\"><span class=\"muted\">Polling every ~A seconds for new messages.</span><button class=\"btn\" type=\"submit\">Send Message</button></div>~%"
                  (echospace-html-escape (let ((v (getenv "ISL_ECHOSPACE_POLL_INTERVAL_SEC"))) (if (or (null v) (= (length v) 0)) "5" v))))
          (format t "</form></section></main>~%")
          (echospace-render-members-panel members)
          (format t "</div>~%")
          (format t "<script>(function(){const timeline=document.getElementById('timeline');if(!timeline)return;const messagesApi=timeline.dataset.api;const membersApi=timeline.dataset.membersApi;const pollMs=(parseInt(\"~A\",10)||5)*1000;const escapeHtml=(s)=>String(s).replace(/[&<>\\\"]/g,(c)=>({\"&\":\"&amp;\",\"<\":\"&lt;\",\">\":\"&gt;\",\"\\\"\":\"&quot;\"}[c]));const renderMessage=(m)=>`<article class=\\\"message\\\" data-message-id=\\\"${m.id}\\\"><div class=\\\"message-head\\\"><span class=\\\"message-author\\\">${escapeHtml(m.display_name)}</span><span class=\\\"message-meta\\\">@${escapeHtml(m.username)} · ${escapeHtml(m.created_at)}</span></div><div class=\\\"message-body\\\">${escapeHtml(m.body)}</div></article>`;const syncMembers=(items)=>{const panel=document.getElementById('members-panel');if(!panel)return;panel.innerHTML=items.map((m)=>`<div class=\\\"member\\\"><div><strong>${escapeHtml(m.display_name)}</strong><div class=\\\"muted\\\">@${escapeHtml(m.username)} · ${escapeHtml(m.role)}</div></div><span>${escapeHtml(m.status)}</span></div>`).join('');};const latestId=()=>{const nodes=timeline.querySelectorAll('[data-message-id]');if(!nodes.length)return 0;return parseInt(nodes[nodes.length-1].getAttribute('data-message-id'),10)||0;};const poll=()=>{fetch(messagesApi+'?after_id='+latestId(),{credentials:'same-origin'}).then((r)=>r.ok?r.json():null).then((data)=>{if(!data||!data.messages)return;data.messages.forEach((m)=>{timeline.insertAdjacentHTML('beforeend',renderMessage(m));});if(data.messages.length){timeline.scrollTop=timeline.scrollHeight;}}).catch(()=>{});fetch(membersApi,{credentials:'same-origin'}).then((r)=>r.ok?r.json():null).then((data)=>{if(data&&data.members)syncMembers(data.members);}).catch(()=>{});};setInterval(poll,pollMs);})();</script>~%"
                  (echospace-html-escape (let ((v (getenv "ISL_ECHOSPACE_POLL_INTERVAL_SEC"))) (if (or (null v) (= (length v) 0)) "5" v))))
          (echospace-render-layout-end)))))

(defun echospace-render-admin-page ()
  (let ((workspaces (echospace-sort-by
                     (mapcar (lambda (row) (list (echospace-db-row-value row "slug")
                                                 (echospace-db-row-value row "name")))
                             (echospace-db-table-rows "workspaces"))
                     (lambda (row) (first row))
                     nil))
        (users (echospace-sort-by
                (mapcar (lambda (row) (list (echospace-db-row-value row "username")
                                            (echospace-db-row-value row "display_name")
                                            (echospace-db-row-value row "role_name")))
                        (echospace-db-table-rows "users"))
                (lambda (row) (first row))
                nil)))
    (echospace-render-layout-start "Echospace Admin")
    (echospace-render-topbar)
    (echospace-render-flash)
    (format t "<main style=\"padding:1.25rem\" class=\"stack\">~%")
    (format t "<section><h1 style=\"margin-top:0\">Admin</h1><p class=\"muted\">Create users, workspaces, channels, and memberships for the chat app.</p></section>~%")
    (format t "<section class=\"admin-grid\">~%")
    (format t "<article class=\"admin-panel stack\"><h2>Create User</h2><form method=\"post\" action=\"~A/admin/users\" class=\"stack\">~%"
            (echospace-html-escape (echospace-app-base)))
    (echospace-render-csrf-hidden-input)
    (format t "<input type=\"text\" name=\"username\" placeholder=\"username\">~%")
    (format t "<input type=\"text\" name=\"display_name\" placeholder=\"Display name\">~%")
    (format t "<input type=\"password\" name=\"password\" placeholder=\"Password\">~%")
    (format t "<select name=\"role_name\"><option value=\"member\">member</option><option value=\"admin\">admin</option></select>~%")
    (format t "<button class=\"btn\" type=\"submit\">Create User</button></form></article>~%")
    (format t "<article class=\"admin-panel stack\"><h2>Create Workspace</h2><form method=\"post\" action=\"~A/admin/workspaces\" class=\"stack\">~%"
            (echospace-html-escape (echospace-app-base)))
    (echospace-render-csrf-hidden-input)
    (format t "<input type=\"text\" name=\"slug\" placeholder=\"workspace-slug\">~%")
    (format t "<input type=\"text\" name=\"name\" placeholder=\"Workspace name\">~%")
    (format t "<button class=\"btn\" type=\"submit\">Create Workspace</button></form></article>~%")
    (format t "<article class=\"admin-panel stack\"><h2>Create Channel</h2><form method=\"post\" action=\"~A/admin/channels\" class=\"stack\">~%"
            (echospace-html-escape (echospace-app-base)))
    (echospace-render-csrf-hidden-input)
    (format t "<input type=\"text\" name=\"workspace_slug\" placeholder=\"workspace-slug\">~%")
    (format t "<input type=\"text\" name=\"slug\" placeholder=\"channel-slug\">~%")
    (format t "<input type=\"text\" name=\"name\" placeholder=\"Channel name\">~%")
    (format t "<input type=\"text\" name=\"topic\" placeholder=\"Topic\">~%")
    (format t "<input type=\"text\" name=\"display_order\" placeholder=\"10\">~%")
    (format t "<button class=\"btn\" type=\"submit\">Create Channel</button></form></article>~%")
    (format t "<article class=\"admin-panel stack\"><h2>Add Membership</h2><form method=\"post\" action=\"~A/admin/memberships\" class=\"stack\">~%"
            (echospace-html-escape (echospace-app-base)))
    (echospace-render-csrf-hidden-input)
    (format t "<input type=\"text\" name=\"username\" placeholder=\"username\">~%")
    (format t "<input type=\"text\" name=\"workspace_slug\" placeholder=\"workspace-slug\">~%")
    (format t "<button class=\"btn\" type=\"submit\">Add Membership</button></form></article>~%")
    (format t "</section>~%")
    (format t "<section class=\"admin-panel stack\"><h2>Current Users</h2><table class=\"table\"><thead><tr><th>Username</th><th>Display Name</th><th>Role</th></tr></thead><tbody>~%")
    (dolist (user users)
      (format t "<tr><td>@~A</td><td>~A</td><td>~A</td></tr>~%"
              (echospace-html-escape (first user))
              (echospace-html-escape (second user))
              (echospace-html-escape (third user))))
    (format t "</tbody></table></section>~%")
    (format t "<section class=\"admin-panel stack\"><h2>Workspaces</h2><table class=\"table\"><thead><tr><th>Slug</th><th>Name</th></tr></thead><tbody>~%")
    (dolist (workspace workspaces)
      (format t "<tr><td>~A</td><td>~A</td></tr>~%"
              (echospace-html-escape (first workspace))
              (echospace-html-escape (second workspace))))
    (format t "</tbody></table></section></main>~%")
    (echospace-render-layout-end)))

(defun echospace-handle-root ()
  (if (not (echospace-logged-in-p))
      (echospace-render-redirect (echospace-login-target-url))
      (let ((dest (echospace-default-channel-path-for-user (echospace-current-username))))
        (if (echospace-blank-text-p dest)
            (progn
              (echospace-render-layout-start "Echospace")
              (echospace-render-topbar)
              (format t "<main style=\"padding:2rem\"><div class=\"empty\">You do not belong to any workspace yet.</div></main>")
              (echospace-render-layout-end))
            (echospace-render-redirect dest)))))

(defun echospace-handle-login-post ()
  (let* ((raw-body (echospace-read-request-body))
         (username (echospace-parse-form-field raw-body "username"))
         (password (echospace-parse-form-field raw-body "password"))
         (next (echospace-safe-redirect-target (echospace-parse-form-field raw-body "next")))
         (user (echospace-fetch-user-for-login username password)))
    (if (null user)
        (echospace-render-redirect
         (string-append (echospace-app-base) "/login?error="
                        (echospace-url-encode-lite "Invalid username or password")
                        "&next=" (echospace-url-encode-lite next)))
        (let ((token (echospace-issue-session-token))
              (csrf-token (echospace-issue-csrf-token))
              (dest (if (echospace-blank-text-p next)
                        (echospace-default-channel-path-for-user username)
                        next)))
          (echospace-create-user-session! (first user) token csrf-token)
          (echospace-update-last-login-at! (first user))
          (setq *echospace-current-user* (list (first user) (second user) (third user) token csrf-token))
          (echospace-set-session-cookie-header token)
          (echospace-write-log "info" "auth.login" (echospace-make-log-fields-2 "username" username "dest" dest))
          (echospace-render-redirect (if (echospace-blank-text-p dest) (echospace-app-base) dest))))))

(defun echospace-handle-logout-post ()
  (let ((raw-body (echospace-read-form-body-or-render t)))
    (if (stringp raw-body)
        (progn
          (echospace-revoke-session-token! (echospace-current-session-token))
          (setq *echospace-current-user* '())
          (echospace-clear-session-cookie-header)
          (echospace-render-redirect
           (string-append (echospace-app-base) "/login?notice="
                          (echospace-url-encode-lite "Logged out"))))
        nil)))

(defun echospace-handle-message-post (workspace-slug channel-slug)
  (let ((raw-body (echospace-read-form-body-or-render t)))
    (if (not (stringp raw-body))
        nil
        (let* ((body (echospace-parse-form-field raw-body "body"))
               (workspace-id (echospace-workspace-id-by-slug workspace-slug))
               (channel-id (if (null workspace-id) '()
                               (echospace-channel-id-by-workspace-id workspace-id channel-slug)))
               (author-id (echospace-user-id-by-username (echospace-current-username))))
          (if (or (echospace-blank-text-p body)
                  (null workspace-id)
                  (null channel-id)
                  (null author-id)
                  (not (echospace-user-can-access-workspace-p (echospace-current-username) workspace-slug)))
              (echospace-render-redirect
               (string-append (echospace-channel-url workspace-slug channel-slug)
                              "?error=" (echospace-url-encode-lite "Unable to post message")))
              (progn
                (echospace-db-exec!
                 nil
                 (string-append
                  "INSERT INTO messages (workspace_id, channel_id, author_user_id, body, created_at) VALUES ("
                  (format nil "~A" workspace-id) ", "
                  (format nil "~A" channel-id) ", "
                  (format nil "~A" author-id) ", "
                  (echospace-db-text-sql body) ", "
                  (echospace-db-text-sql (echospace-db-now-text)) ");"))
                (echospace-write-log "info" "message.post"
                                     (echospace-make-log-fields-2 "workspace" workspace-slug "channel" channel-slug))
                (echospace-render-redirect (echospace-channel-url workspace-slug channel-slug))))))))

(defun echospace-handle-api-messages (workspace-slug channel-slug)
  (let ((workspace-id (echospace-workspace-id-by-slug workspace-slug)))
    (if (or (null workspace-id)
            (not (echospace-user-can-access-workspace-p (echospace-current-username) workspace-slug)))
        (echospace-render-forbidden "workspace access denied")
        (let ((channel-id (echospace-channel-id-by-workspace-id workspace-id channel-slug))
              (after-id (echospace-parse-int-safe (echospace-query-param "after_id"))))
          (if (null channel-id)
              (echospace-render-not-found)
              (echospace-render-json-messages
               (echospace-fetch-messages-after-id workspace-id channel-id (if (null after-id) 0 after-id))))))))

(defun echospace-handle-api-members (workspace-slug)
  (let ((workspace-id (echospace-workspace-id-by-slug workspace-slug)))
    (if (or (null workspace-id)
            (not (echospace-user-can-access-workspace-p (echospace-current-username) workspace-slug)))
        (echospace-render-forbidden "workspace access denied")
        (echospace-render-json-members (echospace-fetch-members-for-workspace-id workspace-id)))))

(defun echospace-handle-admin-create-user ()
  (let ((raw-body (echospace-read-form-body-or-render t)))
    (if (stringp raw-body)
        (let ((username (echospace-trim-ws (echospace-parse-form-field raw-body "username")))
              (display-name (echospace-trim-ws (echospace-parse-form-field raw-body "display_name")))
              (password (echospace-parse-form-field raw-body "password"))
              (role-name (echospace-parse-form-field raw-body "role_name"))
              (now (echospace-db-now-text)))
          (if (or (echospace-blank-text-p username)
                  (echospace-blank-text-p display-name)
                  (echospace-blank-text-p password)
                  (not (or (string= role-name "member") (string= role-name "admin")))
                  (not (null (echospace-find-user-row username))))
              (echospace-render-redirect
               (string-append (echospace-app-base) "/admin?error="
                              (echospace-url-encode-lite "Invalid or duplicate user")))
              (progn
                (echospace-db-exec!
                 nil
                 (string-append
                  "INSERT INTO users (username, display_name, password_hash, role_name, enabled, created_at, updated_at) VALUES ("
                  (echospace-db-text-sql username) ", "
                  (echospace-db-text-sql display-name) ", "
                  (echospace-db-text-sql (echospace-password-hash password)) ", "
                  (echospace-db-text-sql role-name) ", TRUE, "
                  (echospace-db-text-sql now) ", "
                  (echospace-db-text-sql now) ");"))
                (echospace-render-redirect
                 (string-append (echospace-app-base) "/admin?notice="
                                (echospace-url-encode-lite "User created"))))))
        nil)))

(defun echospace-handle-admin-create-workspace ()
  (let ((raw-body (echospace-read-form-body-or-render t)))
    (if (stringp raw-body)
        (let ((slug (echospace-trim-ws (echospace-parse-form-field raw-body "slug")))
              (name (echospace-trim-ws (echospace-parse-form-field raw-body "name"))))
          (if (or (echospace-blank-text-p slug)
                  (echospace-blank-text-p name)
                  (not (null (echospace-find-workspace-row slug))))
              (echospace-render-redirect
               (string-append (echospace-app-base) "/admin?error="
                              (echospace-url-encode-lite "Invalid or duplicate workspace")))
              (progn
                (echospace-db-exec!
                 nil
                 (string-append
                  "INSERT INTO workspaces (slug, name, created_at) VALUES ("
                  (echospace-db-text-sql slug) ", "
                  (echospace-db-text-sql name) ", "
                  (echospace-db-text-sql (echospace-db-now-text)) ");"))
                (echospace-render-redirect
                 (string-append (echospace-app-base) "/admin?notice="
                                (echospace-url-encode-lite "Workspace created"))))))
        nil)))

(defun echospace-handle-admin-create-channel ()
  (let ((raw-body (echospace-read-form-body-or-render t)))
    (if (stringp raw-body)
        (let* ((workspace-slug (echospace-trim-ws (echospace-parse-form-field raw-body "workspace_slug")))
               (slug (echospace-trim-ws (echospace-parse-form-field raw-body "slug")))
               (name (echospace-trim-ws (echospace-parse-form-field raw-body "name")))
               (topic (echospace-trim-ws (echospace-parse-form-field raw-body "topic")))
               (display-order-raw (echospace-parse-form-field raw-body "display_order"))
               (display-order (echospace-parse-int-safe display-order-raw))
               (workspace-id (echospace-workspace-id-by-slug workspace-slug)))
          (if (or (null workspace-id)
                  (echospace-blank-text-p slug)
                  (echospace-blank-text-p name)
                  (null display-order)
                  (not (null (echospace-find-channel-row-by-workspace-id workspace-id slug))))
              (echospace-render-redirect
               (string-append (echospace-app-base) "/admin?error="
                              (echospace-url-encode-lite "Invalid or duplicate channel")))
              (progn
                (echospace-db-exec!
                 nil
                 (string-append
                  "INSERT INTO channels (workspace_id, slug, name, topic, display_order, created_at) VALUES ("
                  (format nil "~A" workspace-id) ", "
                  (echospace-db-text-sql slug) ", "
                  (echospace-db-text-sql name) ", "
                  (echospace-db-text-sql topic) ", "
                  (format nil "~A" display-order) ", "
                  (echospace-db-text-sql (echospace-db-now-text)) ");"))
                (echospace-render-redirect
                 (string-append (echospace-app-base) "/admin?notice="
                                (echospace-url-encode-lite "Channel created"))))))
        nil)))

(defun echospace-handle-admin-create-membership ()
  (let ((raw-body (echospace-read-form-body-or-render t)))
    (if (stringp raw-body)
        (let* ((username (echospace-trim-ws (echospace-parse-form-field raw-body "username")))
               (workspace-slug (echospace-trim-ws (echospace-parse-form-field raw-body "workspace_slug")))
               (user-id (echospace-user-id-by-username username))
               (workspace-id (echospace-workspace-id-by-slug workspace-slug)))
          (if (or (null user-id)
                  (null workspace-id)
                  (echospace-membership-exists-p workspace-id user-id))
              (echospace-render-redirect
               (string-append (echospace-app-base) "/admin?error="
                              (echospace-url-encode-lite "Invalid or duplicate membership")))
              (progn
                (echospace-db-exec!
                 nil
                 (string-append
                  "INSERT INTO workspace_members (workspace_id, user_id, created_at) VALUES ("
                  (format nil "~A" workspace-id) ", "
                  (format nil "~A" user-id) ", "
                  (echospace-db-text-sql (echospace-db-now-text)) ");"))
                (echospace-render-redirect
                 (string-append (echospace-app-base) "/admin?notice="
                                (echospace-url-encode-lite "Membership added"))))))
        nil)))

(defun echospace-dispatch-route (method segments)
  (cond
   ((and (null segments) (string= method "GET"))
    (echospace-handle-root))
   ((and (= (length segments) 1) (string= (first segments) "healthz"))
    (echospace-render-healthz t))
   ((and (= (length segments) 1) (string= (first segments) "login") (string= method "GET"))
    (echospace-render-login-page))
   ((and (= (length segments) 1) (string= (first segments) "login") (string= method "POST"))
    (echospace-handle-login-post))
   ((and (= (length segments) 1) (string= (first segments) "logout") (string= method "POST"))
    (echospace-handle-logout-post))
   ((and (= (length segments) 1) (string= (first segments) "admin") (string= method "GET"))
    (echospace-render-admin-page))
   ((and (= (length segments) 2) (string= (first segments) "admin") (string= (second segments) "users") (string= method "POST"))
    (echospace-handle-admin-create-user))
   ((and (= (length segments) 2) (string= (first segments) "admin") (string= (second segments) "workspaces") (string= method "POST"))
    (echospace-handle-admin-create-workspace))
   ((and (= (length segments) 2) (string= (first segments) "admin") (string= (second segments) "channels") (string= method "POST"))
    (echospace-handle-admin-create-channel))
   ((and (= (length segments) 2) (string= (first segments) "admin") (string= (second segments) "memberships") (string= method "POST"))
    (echospace-handle-admin-create-membership))
   ((and (= (length segments) 4)
         (string= (first segments) "api")
         (string= (second segments) "workspaces")
         (string= (fourth segments) "members")
         (string= method "GET"))
    (echospace-handle-api-members (third segments)))
   ((and (= (length segments) 6)
         (string= (first segments) "api")
         (string= (second segments) "workspaces")
         (string= (fourth segments) "channels")
         (string= (sixth segments) "messages")
         (string= method "GET"))
    (echospace-handle-api-messages (third segments) (fifth segments)))
   ((and (= (length segments) 4)
         (string= (first segments) "workspaces")
         (string= (third segments) "channels")
         (string= method "GET"))
    (echospace-render-channel-page (second segments) (fourth segments)))
   ((and (= (length segments) 5)
         (string= (first segments) "workspaces")
         (string= (third segments) "channels")
         (string= (fifth segments) "messages")
         (string= method "POST"))
    (echospace-handle-message-post (second segments) (fourth segments)))
   (t
    (echospace-render-not-found))))

(defun echospace-header-value (headers name)
  (let ((xs headers)
        (found '()))
    (while (and (null found) (not (null xs)))
      (let ((entry (car xs)))
        (if (string= (echospace-ascii-downcase (first entry))
                     (echospace-ascii-downcase name))
            (setq found (second entry))
            nil))
      (setq xs (cdr xs)))
    (if (null found) "" found)))

(defun echospace-target-query-string (target)
  (let ((p (string-index "?" target)))
    (if (null p)
        ""
        (substring target (+ p 1) (length target)))))

(defun echospace-restore-env-vars (saved)
  (dolist (entry saved)
    (setenv (first entry) (second entry))))

(defun echospace-run-embedded-request (thunk)
  (handler-case
    (capture-output-string thunk)
    (error (e)
      (capture-output-string
       (lambda ()
         (if (string= (echospace-request-id) "")
             (echospace-init-request-id)
             nil)
         (echospace-render-error (format nil "~A" e)))))))

(defun echospace-handle-request-embedded (method target version headers body script-name path-info server-port)
  (mutex-lock *echospace-embed-mutex*)
  (let* ((vars (list "REQUEST_METHOD" "QUERY_STRING" "CONTENT_LENGTH" "CONTENT_TYPE"
                     "SCRIPT_NAME" "PATH_INFO" "SERVER_PROTOCOL" "SERVER_PORT"
                     "GATEWAY_INTERFACE" "HTTP_COOKIE" "HTTP_HOST" "HTTP_USER_AGENT"))
         (saved '())
         (out ""))
    (dolist (name vars)
      (setq saved (cons (list name (echospace-env-or-empty name)) saved)))
    (setq *echospace-request-body-override* body)
    (setenv "REQUEST_METHOD" method)
    (setenv "QUERY_STRING" (echospace-target-query-string target))
    (setenv "CONTENT_LENGTH" (format nil "~A" (length body)))
    (setenv "CONTENT_TYPE" (echospace-header-value headers "content-type"))
    (setenv "SCRIPT_NAME" script-name)
    (setenv "PATH_INFO" path-info)
    (setenv "SERVER_PROTOCOL" version)
    (setenv "SERVER_PORT" server-port)
    (setenv "GATEWAY_INTERFACE" "CGI/1.1")
    (setenv "HTTP_COOKIE" (echospace-header-value headers "cookie"))
    (setenv "HTTP_HOST" (echospace-header-value headers "host"))
    (setenv "HTTP_USER_AGENT" (echospace-header-value headers "user-agent"))
    (setq out (echospace-run-embedded-request (lambda () (echospace-render-app))))
    (setq *echospace-request-body-override* '())
    (echospace-restore-env-vars saved)
    (mutex-unlock *echospace-embed-mutex*)
    out))

(defun echospace-render-app ()
  (let ((method (echospace-request-method))
        (segments (echospace-path-segments))
        (raw-path (echospace-env-or-empty "PATH_INFO"))
        (db '()))
    (echospace-clear-response-extra-headers)
    (echospace-clear-db-table-cache)
    (echospace-init-request-id)
    (setq db (handler-case (echospace-db-open) (error (e) '())))
    (if (null db)
        (if (and (= (length segments) 1) (string= (first segments) "healthz"))
            (echospace-render-healthz '())
            (error "database unavailable"))
        (progn
          (echospace-load-current-user)
          (if (and (string= method "GET") (echospace-needs-canonical-redirect-p raw-path))
              (echospace-render-redirect (string-append (echospace-app-base) (echospace-canonical-path-info raw-path)))
              (if (echospace-ensure-authorized method segments)
                  (echospace-dispatch-route method segments)
                  nil))
          nil))))

(if (string= (echospace-env-or-empty "ECHOSPACE_EMBED_MODE") "1")
    nil
    (handler-case
      (echospace-render-app)
      (error (e)
        (if (string= (echospace-request-id) "")
            (echospace-init-request-id)
            nil)
        (echospace-write-log "error" "request.error"
                             (echospace-make-log-fields-2 "target" (echospace-current-request-target)
                                                          "message" (format nil "~A" e)))
        (echospace-render-error (format nil "~A" e)))))
