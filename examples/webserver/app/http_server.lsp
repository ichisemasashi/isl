(defun ws-ascii-downcase (s)
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

(defun ws-trim-left (s)
  (let ((i 0)
        (n (length s)))
    (while (and (< i n)
                (not (null (string-index (substring s i (+ i 1)) " \t"))))
      (setq i (+ i 1)))
    (substring s i n)))

(defun ws-trim-right (s)
  (let ((n (length s))
        (i (- (length s) 1)))
    (while (and (>= i 0)
                (not (null (string-index (substring s i (+ i 1)) " \t"))))
      (setq i (- i 1)))
    (substring s 0 (+ i 1))))

(defun ws-trim (s)
  (ws-trim-right (ws-trim-left s)))

(defun ws-strip-cr (s)
  (let ((n (length s)))
    (if (and (> n 0) (string= (substring s (- n 1) n) "\r"))
        (substring s 0 (- n 1))
        s)))

(defun ws-split-once (s needle)
  (let ((p (string-index needle s)))
    (if (null p)
        '()
        (list (substring s 0 p)
              (substring s (+ p (length needle)) (length s))))))

(defun ws-header-get (headers key)
  (let ((k (ws-ascii-downcase key))
        (xs headers)
        (found '()))
    (while (and (null found) (not (null xs)))
      (let ((entry (car xs)))
        (if (string= (car entry) k)
            (setq found (second entry))
            nil))
      (setq xs (cdr xs)))
    found))

(defun ws-parse-int (s)
  (let ((digits (ws-trim s))
        (i 0)
        (n 0))
    (if (= (length digits) 0)
        '()
        (progn
          (while (< i (length digits))
            (let ((ch (substring digits i (+ i 1)))
                  (p (string-index (substring digits i (+ i 1)) "0123456789")))
              (if (null p)
                  (setq n 'invalid)
                  (if (not (eq n 'invalid))
                      (setq n (+ (* n 10) p))
                      nil)))
            (setq i (+ i 1)))
          (if (eq n 'invalid) '() n)))))

(defun ws-read-fixed-body (conn n)
  (let ((i 0)
        (out ""))
    (while (< i n)
      (let ((ch (ws-conn-receive-char conn #f)))
        (if (null ch)
            (ws-protocol-error 400 "unexpected EOF while reading request body")
            (setq out (string-append out (string ch)))))
      (setq i (+ i 1)))
    out))

(defun ws-read-request-line (conn)
  (let ((line (ws-conn-receive-line conn #f)))
    (if (null line)
        'eof
        (let ((normalized (ws-strip-cr line)))
          (if (> (length normalized) (ws-max-request-line-bytes))
              (ws-protocol-error 431 "request line too large")
              normalized)))))

(defun ws-parse-request-line (line)
  (let ((a (ws-split-once line " ")))
    (if (null a)
        (ws-protocol-error 400 "invalid request-line")
        (let* ((method (car a))
               (b (ws-split-once (second a) " ")))
          (if (null b)
              (ws-protocol-error 400 "invalid request-line")
              (let ((target (car b))
                    (version (second b)))
                (if (or (string= version "HTTP/1.1")
                        (string= version "HTTP/1.0"))
                    (list method target version)
                    (ws-protocol-error 400 "unsupported HTTP version"))))))))

(defun ws-read-headers (conn)
  (let ((headers '())
        (done nil)
        (total-bytes 0))
    (while (not done)
      (let ((raw (ws-conn-receive-line conn #f)))
        (if (null raw)
            (ws-protocol-error 400 "unexpected EOF in headers")
            (let ((line (ws-strip-cr raw)))
              (setq total-bytes (+ total-bytes (length line) 2))
              (if (> (length line) (ws-max-header-line-bytes))
                  (ws-protocol-error 431 "header line too large")
                  nil)
              (if (> total-bytes (ws-max-header-bytes))
                  (ws-protocol-error 431 "request headers too large")
                  nil)
              (if (string= line "")
                  (setq done t)
                  (let ((p (string-index ":" line)))
                    (if (null p)
                        (ws-protocol-error 400 "invalid header line")
                        (let ((name (ws-ascii-downcase (ws-trim (substring line 0 p))))
                              (value (ws-trim (substring line (+ p 1) (length line)))))
                          (setq headers (cons (list name value) headers))))))))))
    (ws-reverse headers)))

(defun ws-connection-keep-alive-p (version headers)
  (let ((conn-value (ws-header-get headers "connection")))
    (if (string= version "HTTP/1.1")
        (if (null conn-value)
            #t
            (not (string= (ws-ascii-downcase conn-value) "close")))
        (if (null conn-value)
            #f
            (string= (ws-ascii-downcase conn-value) "keep-alive")))))

(defun ws-read-http-request (conn)
  (let ((line (ws-read-request-line conn)))
    (if (eq line 'eof)
        'eof
        (catch 'ws-bad-request
          (let* ((parts (ws-parse-request-line line))
                 (method (car parts))
                 (target (second parts))
                 (version (third parts))
                 (headers (ws-read-headers conn))
                 (len-header (ws-header-get headers "content-length"))
                 (content-length (if (null len-header) 0 (ws-parse-int len-header))))
            (if (null content-length)
                (ws-protocol-error 400 "invalid Content-Length")
                (if (> content-length (ws-max-body-bytes))
                    (ws-protocol-error 413 "request body too large")
                    nil))
            (if (> content-length (ws-max-body-bytes))
                (ws-protocol-error 413 "request body too large")
                (let ((body (if (> content-length 0)
                                (ws-read-fixed-body conn content-length)
                                "")))
                  (list 'ok
                        (list 'method method)
                        (list 'target target)
                        (list 'version version)
                        (list 'headers headers)
                        (list 'body body)
                        (list 'keep-alive (ws-connection-keep-alive-p version headers))))))))))

(defun ws-request-get (req key)
  (ws-config-get (cdr req) key))

(defun ws-now-epoch ()
  (format nil "~A" (get-universal-time)))

(defun ws-access-log-path ()
  (ws-env-or "WEBSERVER_ACCESS_LOG" "/tmp/webserver-access.log"))

(defun ws-cgi-timeout-seconds ()
  (let ((v (getenv "WEBSERVER_CGI_TIMEOUT_SEC")))
    (if (null v)
        5
        (let ((n (ws-parse-int v)))
          (if (or (null n) (< n 1))
              5
              n)))))

(defun ws-max-request-line-bytes ()
  4096)

(defun ws-max-header-line-bytes ()
  8192)

(defun ws-max-header-bytes ()
  32768)

(defun ws-max-body-bytes ()
  1048576)

(defun ws-protocol-error (status message)
  (throw 'ws-bad-request (list 'error status message)))

(defun ws-access-log (method path status duration-ms)
  (handler-case
    (ws-append-line
     (ws-access-log-path)
     (format nil "~A method=~A path=~A status=~A duration_ms=~A"
             (ws-now-epoch) method path status duration-ms))
    (error (e) #f)))

(defun ws-reason (status)
  (if (= status 200) "OK"
      (if (= status 206) "Partial Content"
          (if (= status 304) "Not Modified"
      (if (= status 400) "Bad Request"
          (if (= status 403) "Forbidden"
              (if (= status 404) "Not Found"
                  (if (= status 405) "Method Not Allowed"
                      (if (= status 413) "Payload Too Large"
                          (if (= status 431) "Request Header Fields Too Large"
                              (if (= status 416) "Range Not Satisfiable"
                              (if (= status 503) "Service Unavailable"
                                  (if (= status 504) "Gateway Timeout"
                                      "Internal Server Error")))))))))))))

(defun ws-target-path-only (target)
  (let ((p (string-index "?" target)))
    (if (null p) target (substring target 0 p))))

(defun ws-target-query (target)
  (let ((p (string-index "?" target)))
    (if (null p) "" (substring target (+ p 1) (length target)))))

(defun ws-send-response (conn version status headers body send-body)
  (let* ((status-line (string-append version " "
                                     (format nil "~A" status)
                                     " "
                                     (ws-reason status)
                                     "\r\n"))
         (out status-line))
    (while (not (null headers))
      (let ((h (car headers)))
        (setq out (string-append out (car h) ": " (second h) "\r\n")))
      (setq headers (cdr headers)))
    (setq out (string-append out "\r\n"))
    (if send-body
        (setq out (string-append out body))
        nil)
    (ws-conn-send conn out)))

(defun ws-contains (hay needle)
  (not (null (string-index needle hay))))

(defun ws-ends-with (s suffix)
  (let ((n (length s))
        (m (length suffix)))
    (and (>= n m)
         (string= (substring s (- n m) n) suffix))))

(defun ws-static-mime-type (path)
  (let ((p (ws-ascii-downcase path)))
    (if (ws-ends-with p ".html") "text/html; charset=UTF-8"
        (if (ws-ends-with p ".htm") "text/html; charset=UTF-8"
            (if (ws-ends-with p ".css") "text/css; charset=UTF-8"
                (if (ws-ends-with p ".js") "application/javascript; charset=UTF-8"
                    (if (ws-ends-with p ".json") "application/json; charset=UTF-8"
                        (if (ws-ends-with p ".txt") "text/plain; charset=UTF-8"
                            (if (ws-ends-with p ".png") "image/png"
                                (if (ws-ends-with p ".jpg") "image/jpeg"
                                    (if (ws-ends-with p ".jpeg") "image/jpeg"
                                        "application/octet-stream")))))))))))

(defun ws-read-file-text (path)
  (with-open-file (s path :direction :input)
    (let ((line (read-line s #f))
          (acc "")
          (first t))
      (while (not (null line))
        (if first
            (progn
              (setq acc line)
              (setq first nil))
            (setq acc (string-append acc "\n" line)))
        (setq line (read-line s #f)))
      acc)))

(defun ws-read-file-raw (path)
  (ws-read-file-text path))

(defun ws-command-output (cmd)
  (let ((tmp (ws-temp-file "webserver-cmd" ".tmp"))
        (status 1)
        (out ""))
    (setq status (system (string-append cmd " > " (ws-shell-quote tmp) " 2>/dev/null")))
    (if (null (probe-file tmp))
        (setq out "")
        (progn
          (setq out (ws-read-file-text tmp))
          (delete-file tmp)))
    (list status (ws-trim out))))

(defun ws-file-mtime-epoch (cfg path)
  (let* ((os-family (ws-config-get cfg 'os_family))
         (cmd (if (string= os-family "linux")
                  (string-append "stat -c %Y " (ws-shell-quote path))
                  (string-append "stat -f %m " (ws-shell-quote path))))
         (result (ws-command-output cmd))
         (status (car result))
         (out (second result))
         (n (if (= status 0) (ws-parse-int out) '())))
    (if (null n) 0 n)))

(defun ws-http-date-from-epoch (cfg epoch-sec)
  (let* ((os-family (ws-config-get cfg 'os_family))
         (fmt "'%a, %d %b %Y %H:%M:%S GMT'")
         (cmd (if (string= os-family "linux")
                  (string-append "date -u -d @" (format nil "~A" epoch-sec) " +" fmt)
                  (string-append "date -u -r " (format nil "~A" epoch-sec) " +" fmt)))
         (result (ws-command-output cmd)))
    (if (= (car result) 0)
        (second result)
        "")))

(defun ws-epoch-from-http-date (cfg http-date)
  (let* ((os-family (ws-config-get cfg 'os_family))
         (cmd (if (string= os-family "linux")
                  (string-append "date -u -d " (ws-shell-quote http-date) " +%s")
                  (string-append "date -ju -f '%a, %d %b %Y %H:%M:%S GMT' "
                                 (ws-shell-quote http-date)
                                 " +%s")))
         (result (ws-command-output cmd))
         (n (if (= (car result) 0) (ws-parse-int (second result)) '())))
    (if (null n) '() n)))

(defun ws-digits-only-p (s)
  (let ((i 0)
        (ok t))
    (if (= (length s) 0)
        nil
        (progn
          (while (and ok (< i (length s)))
            (if (null (string-index (substring s i (+ i 1)) "0123456789"))
                (setq ok nil)
                nil)
            (setq i (+ i 1)))
          ok))))

(defun ws-parse-range-header (range-header total-size)
  (if (null range-header)
      (list 'none)
      (if (not (ws-starts-with range-header "bytes="))
          (list 'none)
          (let ((spec (substring range-header (length "bytes=") (length range-header))))
            (if (ws-contains spec ",")
                (list 'error)
                (let ((parts (ws-split-once spec "-")))
                  (if (null parts)
                      (list 'error)
                      (let ((start-s (car parts))
                            (end-s (second parts)))
                        (if (and (= (length start-s) 0) (= (length end-s) 0))
                            (list 'error)
                            (if (= total-size 0)
                                (list 'unsat)
                                (if (= (length start-s) 0)
                                    (if (not (ws-digits-only-p end-s))
                                        (list 'error)
                                        (let* ((suffix (ws-parse-int end-s))
                                               (take (if (> suffix total-size) total-size suffix))
                                               (start (- total-size take))
                                               (end (- total-size 1)))
                                          (if (<= suffix 0)
                                              (list 'unsat)
                                              (list 'ok start end))))
                                    (if (not (ws-digits-only-p start-s))
                                        (list 'error)
                                        (let ((start (ws-parse-int start-s)))
                                          (if (>= start total-size)
                                              (list 'unsat)
                                              (if (= (length end-s) 0)
                                                  (list 'ok start (- total-size 1))
                                                  (if (not (ws-digits-only-p end-s))
                                                      (list 'error)
                                                      (let ((end (ws-parse-int end-s)))
                                                        (if (< end start)
                                                            (list 'error)
                                                            (if (>= end total-size)
                                                                (list 'ok start (- total-size 1))
                                                                (list 'ok start end))))))))))))))))))))

(defun ws-write-file-raw (path text)
  (with-open-file (s path :direction :output :if-exists :overwrite)
    (format s "~A" text)))

(defun ws-delete-file-if-exists (path)
  (if (null (probe-file path))
      nil
      (delete-file path)))

(defun ws-temp-file (prefix suffix)
  (string-append "/tmp/" prefix "-"
                 (format nil "~A" (get-universal-time))
                 "-"
                 (format nil "~A" (get-internal-run-time))
                 suffix))

(defun ws-byte-length (text)
  (let* ((tmp (ws-temp-file "webserver-bytes" ".tmp"))
         (result '())
         (status 1)
         (out "")
         (n '()))
    (ws-write-file-raw tmp text)
    (setq result (ws-command-output (string-append "wc -c < " (ws-shell-quote tmp))))
    (setq status (car result))
    (setq out (second result))
    (ws-delete-file-if-exists tmp)
    (setq n (if (= status 0) (ws-parse-int out) '()))
    (if (null n)
        (length text)
        n)))

(defun ws-split-lines (s)
  (let ((i 0)
        (start 0)
        (acc '()))
    (while (< i (length s))
      (if (string= (substring s i (+ i 1)) "\n")
          (progn
            (setq acc (cons (ws-strip-cr (substring s start i)) acc))
            (setq start (+ i 1)))
          nil)
      (setq i (+ i 1)))
    (if (< start (length s))
        (setq acc (cons (ws-strip-cr (substring s start (length s))) acc))
        nil)
    (ws-reverse acc)))

(defun ws-cgi-split-header-body (raw)
  (let ((p-crlf (string-index "\r\n\r\n" raw)))
    (if (null p-crlf)
        (let ((p-lf (string-index "\n\n" raw)))
          (if (null p-lf)
              (list raw "")
              (list (substring raw 0 p-lf)
                    (substring raw (+ p-lf 2) (length raw)))))
        (list (substring raw 0 p-crlf)
              (substring raw (+ p-crlf 4) (length raw))))))

(defun ws-cgi-header-get (headers key)
  (let ((k (ws-ascii-downcase key))
        (xs headers)
        (found '()))
    (while (and (null found) (not (null xs)))
      (if (string= (car (car xs)) k)
          (setq found (second (car xs)))
          nil)
      (setq xs (cdr xs)))
    found))

(defun ws-cgi-parse-status (status-line)
  (if (null status-line)
      200
      (let ((p (string-index " " status-line)))
        (if (null p)
            (let ((n (ws-parse-int status-line)))
              (if (null n) 500 n))
            (let ((n (ws-parse-int (substring status-line 0 p))))
              (if (null n) 500 n))))))

(defun ws-http-error-response (conn version status keep-alive body)
  (ws-send-response
   conn
   version
   status
   (list
    (list "Content-Type" "text/plain; charset=UTF-8")
    (list "Content-Length" (format nil "~A" (ws-byte-length body)))
    (list "Connection" (if keep-alive "keep-alive" "close")))
   body
   #t))

(defun ws-with-mutex (m thunk)
  (mutex-lock m)
  (handler-case
    (let ((result (funcall thunk)))
      (mutex-unlock m)
      result)
    (error (e)
      (handler-case
        (mutex-unlock m)
        (error (e2) #f))
      (error e))))

(defun ws-static-public-path (target-path)
  (if (or (string= target-path "/")
          (string= target-path "/public")
          (string= target-path "/public/"))
      "/public/index.html"
      target-path))

(defun ws-static-file-within-public-p (public-root file-path)
  (if (not (ws-file-readable-p file-path))
      #f
      (let ((cmd (string-append "test -L " (ws-shell-quote file-path))))
        (if (= (system cmd) 0)
            #f
            (let ((rel (substring file-path (+ (length public-root) 1) (length file-path)))
                  (parts '())
                  (cur public-root)
                  (ok #t))
              (setq parts (ws-split-path rel))
              (while (and ok (not (null parts)))
                (setq cur (string-append cur "/" (car parts)))
                (if (= (system (string-append "test -L " (ws-shell-quote cur))) 0)
                    (setq ok #f)
                    nil)
                (setq parts (cdr parts)))
              ok)))))

(defun ws-resolve-static-file (cfg target-path)
  (let* ((public-root (ws-normalize-path
                       (string-append (ws-config-get cfg 'document_root) "/public")))
         (mapped (ws-static-public-path target-path)))
    (if (or (not (ws-starts-with mapped "/"))
            (ws-contains mapped "..")
            (ws-contains mapped "\\"))
        (list 'error 403 "forbidden\n")
        (if (not (or (string= mapped "/public")
                     (ws-starts-with mapped "/public/")))
            (list 'error 403 "forbidden\n")
            (let* ((candidate (ws-normalize-path
                               (string-append (ws-config-get cfg 'document_root) mapped))))
              (if (not (ws-starts-with candidate (string-append public-root "/")))
                  (list 'error 403 "forbidden\n")
                  (if (not (ws-static-file-within-public-p public-root candidate))
                      (list 'error 404 "not found\n")
                      (let ((mime (ws-static-mime-type candidate)))
                        (list 'ok candidate mime)))))))))

(defun ws-cgi-request-path-p (path)
  (ws-starts-with path "/cgi-bin/"))

(defun ws-cgi-script-resolvable-p (cgi-bin-dir rel-path abs-script)
  (if (or (not (ws-starts-with abs-script (string-append cgi-bin-dir "/")))
          (ws-contains rel-path "..")
          (ws-contains rel-path "\\")
          (ws-contains rel-path "'"))
      #f
      (let ((parts (ws-split-path rel-path))
            (cur cgi-bin-dir)
            (ok #t))
        (while (and ok (not (null parts)))
          (setq cur (string-append cur "/" (car parts)))
          (if (= (system (string-append "test -L " (ws-shell-quote cur))) 0)
              (setq ok #f)
              nil)
          (setq parts (cdr parts)))
        (if (not ok)
            #f
            (= (system (string-append "test -f " (ws-shell-quote abs-script)
                                      " && test -x " (ws-shell-quote abs-script)
                                      " && test ! -L " (ws-shell-quote abs-script)))
               0)))))

(defun ws-resolve-cgi-script-and-path-info (cfg rel-path)
  (let* ((cgi-bin-dir (ws-config-get cfg 'cgi_bin_dir))
         (candidate rel-path)
         (resolved nil))
    (while (and (null resolved) (> (length candidate) 0))
      (let ((abs-script (ws-normalize-path (string-append cgi-bin-dir "/" candidate))))
        (if (ws-cgi-script-resolvable-p cgi-bin-dir candidate abs-script)
            (let ((path-info (if (= (length candidate) (length rel-path))
                                 ""
                                 (substring rel-path (length candidate) (length rel-path)))))
              (if (and (> (length path-info) 0)
                       (not (ws-starts-with path-info "/")))
                  (setq path-info (string-append "/" path-info))
                  nil)
              (setq resolved (list abs-script candidate path-info)))
            (let ((slash (ws-last-index-of-char candidate "/")))
              (if (null slash)
                  (setq candidate "")
                  (setq candidate (substring candidate 0 slash)))))))
    resolved))

(defun ws-resolve-cgi-script (cfg target-path)
  (if (not (ws-cgi-request-path-p target-path))
      (list 'not-cgi)
      (let* ((cgi-bin-dir (ws-config-get cfg 'cgi_bin_dir))
             (prefix-len (length "/cgi-bin/"))
             (rel-path (substring target-path prefix-len (length target-path))))
        (if (= (length rel-path) 0)
            (list 'error 404 "not found\n")
            (let ((resolved (ws-resolve-cgi-script-and-path-info cfg rel-path)))
              (if (null resolved)
                  (list 'error 404 "not found\n")
                  (list 'ok
                        (first resolved)
                        (string-append "/cgi-bin/" (second resolved))
                        (third resolved))))))))

(defun ws-parse-cgi-headers (header-text)
  (let ((lines (ws-split-lines header-text))
        (headers '()))
    (while (not (null lines))
      (let ((line (car lines)))
        (if (string= line "")
            nil
            (let ((p (string-index ":" line)))
              (if (null p)
                  nil
                  (let ((name (ws-ascii-downcase (ws-trim (substring line 0 p))))
                        (value (ws-trim (substring line (+ p 1) (length line)))))
                    (setq headers (cons (list name value) headers))))))
        (setq lines (cdr lines))))
    (ws-reverse headers)))

(defun ws-remove-hop-by-hop-cgi-headers (headers)
  (let ((out '())
        (xs headers))
    (while (not (null xs))
      (let* ((h (car xs))
             (k (car h)))
        (if (or (string= k "status")
                (string= k "content-length")
                (string= k "connection"))
            nil
            (setq out (cons h out))))
      (setq xs (cdr xs)))
    (ws-reverse out)))

(defun ws-run-cgi-script (cfg req script-path script-name path-info)
  (let* ((target (ws-request-get req 'target))
         (method (ws-request-get req 'method))
         (version (ws-request-get req 'version))
         (headers (ws-request-get req 'headers))
         (body (ws-request-get req 'body))
         (query (ws-target-query target))
         (content-type (ws-header-get headers "content-type"))
         (stdin-path (ws-temp-file "webserver-cgi-in" ".tmp"))
         (stdout-path (ws-temp-file "webserver-cgi-out" ".tmp"))
         (timeout-sec (ws-cgi-timeout-seconds))
         (cmd (string-append
               (ws-shell-quote script-path)
               " < " (ws-shell-quote stdin-path)
               " > " (ws-shell-quote stdout-path))))
    (ws-write-file-raw stdin-path body)
    (setenv "REQUEST_METHOD" method)
    (setenv "QUERY_STRING" query)
    (setenv "CONTENT_LENGTH" (format nil "~A" (ws-byte-length body)))
    (setenv "CONTENT_TYPE" (if (null content-type) "" content-type))
    (setenv "SCRIPT_NAME" script-name)
    (setenv "PATH_INFO" path-info)
    (setenv "SERVER_PROTOCOL" version)
    (setenv "SERVER_PORT" (format nil "~A" (ws-config-get cfg 'listen_port)))
    (setenv "GATEWAY_INTERFACE" "CGI/1.1")
    (let* ((exec-result (system-timeout cmd timeout-sec))
           (status (car exec-result))
           (timedout (second exec-result)))
      (if timedout
          (progn
            (ws-log-error (format nil "cgi timeout method=~A script=~A timeout_sec=~A"
                                  method script-path timeout-sec))
            (ws-delete-file-if-exists stdin-path)
            (ws-delete-file-if-exists stdout-path)
            (list 'error 504 "cgi timeout\n"))
          (if (not (= status 0))
              (progn
                (ws-log-error (format nil "cgi failed method=~A script=~A exit_status=~A"
                                      method script-path status))
                (ws-delete-file-if-exists stdin-path)
                (ws-delete-file-if-exists stdout-path)
                (list 'error 500 "cgi execution failed\n"))
              (let* ((raw (ws-read-file-text stdout-path))
                     (parts (ws-cgi-split-header-body raw))
                     (hdr-text (car parts))
                     (resp-body (second parts))
                     (resp-headers (ws-parse-cgi-headers hdr-text))
                     (resp-status (ws-cgi-parse-status (ws-cgi-header-get resp-headers "status"))))
                (ws-delete-file-if-exists stdin-path)
                (ws-delete-file-if-exists stdout-path)
                (list 'ok resp-status resp-headers resp-body)))))))

(defun ws-handle-cgi-request (conn cfg req)
  (let* ((method (ws-request-get req 'method))
         (version (ws-request-get req 'version))
         (keep-alive (ws-request-get req 'keep-alive))
         (path (ws-target-path-only (ws-request-get req 'target)))
         (resolved (ws-resolve-cgi-script cfg path)))
    (if (eq (car resolved) 'ok)
        (let ((run (ws-run-cgi-script cfg
                                      req
                                      (second resolved)
                                      (third resolved)
                                      (fourth resolved))))
          (if (eq (car run) 'ok)
              (let* ((status (second run))
                     (raw-headers (third run))
                     (body (fourth run))
                     (send-body (not (string= method "HEAD")))
                     (filtered (ws-remove-hop-by-hop-cgi-headers raw-headers))
                     (content-type (ws-cgi-header-get filtered "content-type"))
                     (headers (append
                               (if (null content-type)
                                   (list (list "Content-Type" "text/plain; charset=UTF-8"))
                                   '())
                               filtered
                               (list
                                (list "Content-Length" (format nil "~A" (ws-byte-length body)))
                                (list "Connection" (if keep-alive "keep-alive" "close"))))))
                (ws-send-response conn version status headers body send-body)
                (list keep-alive status))
              (progn
                (ws-http-error-response conn version (second run) keep-alive (third run))
                (list keep-alive (second run)))))
        (progn
          (ws-http-error-response conn version (second resolved) keep-alive (third resolved))
          (list keep-alive (second resolved))))))

(defun ws-handle-static-request (conn cfg req)
  (let* ((method (ws-request-get req 'method))
         (target (ws-request-get req 'target))
         (version (ws-request-get req 'version))
         (keep-alive (ws-request-get req 'keep-alive))
         (headers (ws-request-get req 'headers))
         (path (ws-target-path-only target))
         (resolved (ws-resolve-static-file cfg path)))
    (if (eq (car resolved) 'ok)
        (let* ((file-path (second resolved))
               (mime (third resolved))
               (full-body (ws-read-file-raw file-path))
               (total-size (length full-body))
               (mtime-epoch (ws-file-mtime-epoch cfg file-path))
               (last-modified (ws-http-date-from-epoch cfg mtime-epoch))
               (if-modified-since (ws-header-get headers "if-modified-since"))
               (ims-epoch (if (null if-modified-since) '() (ws-epoch-from-http-date cfg if-modified-since)))
               (not-modified (if (null ims-epoch) #f (<= mtime-epoch ims-epoch)))
               (send-body (not (string= method "HEAD"))))
          (if not-modified
              (progn
                (ws-send-response
                 conn
                 version
                 304
                 (list
                  (list "Last-Modified" last-modified)
                  (list "Accept-Ranges" "bytes")
                  (list "Content-Length" "0")
                  (list "Connection" (if keep-alive "keep-alive" "close")))
                 ""
                 #f)
                (list keep-alive 304))
              (let* ((range-header (ws-header-get headers "range"))
                     (range-result (ws-parse-range-header range-header total-size)))
                (if (or (eq (car range-result) 'error)
                        (eq (car range-result) 'unsat))
                    (progn
                      (ws-send-response
                       conn
                       version
                       416
                       (list
                        (list "Content-Type" "text/plain; charset=UTF-8")
                        (list "Content-Range" (format nil "bytes */~A" total-size))
                        (list "Content-Length" "22")
                        (list "Connection" (if keep-alive "keep-alive" "close")))
                       "range not satisfiable\n"
                       send-body)
                      (list keep-alive 416))
                    (if (eq (car range-result) 'ok)
                        (let* ((start (second range-result))
                               (end (third range-result))
                               (part-body (substring full-body start (+ end 1))))
                          (ws-send-response
                           conn
                           version
                           206
                           (list
                            (list "Content-Type" mime)
                            (list "Last-Modified" last-modified)
                            (list "Accept-Ranges" "bytes")
                            (list "Content-Range"
                                  (format nil "bytes ~A-~A/~A" start end total-size))
                            (list "Content-Length" (format nil "~A" (length part-body)))
                            (list "Connection" (if keep-alive "keep-alive" "close")))
                           part-body
                           send-body)
                          (list keep-alive 206))
                        (progn
                          (ws-send-response
                           conn
                           version
                           200
                           (list
                            (list "Content-Type" mime)
                            (list "Last-Modified" last-modified)
                            (list "Accept-Ranges" "bytes")
                            (list "Content-Length" (format nil "~A" total-size))
                            (list "Connection" (if keep-alive "keep-alive" "close")))
                           full-body
                           send-body)
                          (list keep-alive 200)))))))
        (progn
          (ws-http-error-response conn version (second resolved) keep-alive (third resolved))
          (list keep-alive (second resolved))))))

(defun ws-handle-post-request (conn req)
  (let* ((method (ws-request-get req 'method))
         (target (ws-request-get req 'target))
         (version (ws-request-get req 'version))
         (body (ws-request-get req 'body))
         (keep-alive (ws-request-get req 'keep-alive))
         (path (ws-target-path-only target))
         (content (if (string= method "POST")
                      (format nil "webserver t2 post path=~A bytes=~A~%" path (length body))
                      (format nil "webserver t2 ~A path=~A~%" method path)))
         (send-body (not (string= method "HEAD")))
         (base-headers
          (list
           (list "Content-Type" "text/plain; charset=UTF-8")
           (list "Content-Length" (format nil "~A" (ws-byte-length content)))
           (list "Connection" (if keep-alive "keep-alive" "close")))))
    (ws-send-response conn version 200 base-headers content send-body)
    (list keep-alive 200)))

(defun ws-handle-http-request (conn cfg req)
  (let* ((method (ws-request-get req 'method))
         (version (ws-request-get req 'version))
         (keep-alive (ws-request-get req 'keep-alive))
         (path (ws-target-path-only (ws-request-get req 'target)))
         (cgi-enabled (ws-config-get cfg 'cgi_enabled)))
    (if (or (string= method "GET") (string= method "HEAD") (string= method "POST"))
        (if (ws-cgi-request-path-p path)
            (if cgi-enabled
                (ws-handle-cgi-request conn cfg req)
                (progn
                  (ws-http-error-response conn version 404 keep-alive "not found\n")
                  (list keep-alive 404)))
            (if (or (string= method "GET") (string= method "HEAD"))
                (ws-handle-static-request conn cfg req)
                (ws-handle-post-request conn req)))
        (progn
          (ws-send-response
           conn
           version
           405
           (list
            (list "Content-Type" "text/plain; charset=UTF-8")
            (list "Content-Length" "19")
            (list "Connection" (if keep-alive "keep-alive" "close"))
           (list "Allow" "GET, HEAD, POST"))
           "method not allowed\n"
           #t)
          (list keep-alive 405)))))

(defun ws-handle-connection (cfg conn)
  (let ((alive #t))
    (while alive
      (let ((started (get-internal-run-time))
            (req (ws-read-http-request conn)))
        (if (eq req 'eof)
            (setq alive #f)
            (if (and (listp req) (eq (car req) 'ok))
                (let* ((handled (ws-handle-http-request conn cfg req))
                      (next-alive (car handled))
                      (status (second handled))
                      (duration-ms (truncate (* 1000 (/ (- (get-internal-run-time) started)
                                                         (internal-time-units-per-second))))))
                  (ws-access-log (ws-request-get req 'method)
                                 (ws-target-path-only (ws-request-get req 'target))
                                 status
                                 duration-ms)
                  (setq alive next-alive))
                (progn
                  (let* ((status (if (and (listp req) (eq (car req) 'error))
                                     (second req)
                                     400))
                         (message (if (and (listp req) (eq (car req) 'error))
                                      (third req)
                                      "bad request\n"))
                         (duration-ms (truncate (* 1000 (/ (- (get-internal-run-time) started)
                                                            (internal-time-units-per-second))))))
                    (ws-http-error-response conn "HTTP/1.1" status #f message)
                    (ws-access-log "INVALID" "-" status duration-ms)
                    (if (and (listp req) (eq (car req) 'error))
                        (ws-log-error (format nil "request rejected status=~A reason=~A"
                                              status message))
                        nil))
                  (setq alive #f))))))))

(defun ws-conn-send (conn text)
  (if (tls-connection-p conn)
      (tls-send conn text)
      (tcp-send conn text)))

(defun ws-conn-receive-line (conn eof-error-p)
  (if (tls-connection-p conn)
      (tls-receive-line conn eof-error-p)
      (tcp-receive-line conn eof-error-p)))

(defun ws-conn-receive-char (conn eof-error-p)
  (if (tls-connection-p conn)
      (tls-receive-char conn eof-error-p)
      (tcp-receive-char conn eof-error-p)))

(defun ws-conn-close (conn)
  (if (tls-connection-p conn)
      (tls-close conn)
      (tcp-close conn)))

(defun ws-make-listener (tls-mode port cert-file key-file)
  (if tls-mode
      (tls-listen port cert-file key-file)
      (tcp-listen port)))

(defun ws-accept-connection (listener tls-mode)
  (if tls-mode
      (tls-accept listener)
      (tcp-accept listener)))

(defun ws-close-listener (listener tls-mode)
  (if tls-mode
      (tls-listener-close listener)
      (tcp-listener-close listener)))

(defun ws-handle-over-capacity (conn)
  (ws-log-error "connection limit exceeded: rejecting with 503")
  (handler-case
    (ws-send-response
     conn
     "HTTP/1.1"
     503
     (list
      (list "Content-Type" "text/plain; charset=UTF-8")
      (list "Content-Length" "20")
      (list "Connection" "close"))
     "service unavailable\n"
     #t)
    (error (e) #f))
  (handler-case
    (ws-conn-close conn)
      (error (e) #f)))

(defun ws-spawn-connection-worker (cfg conn active-mutex active-box)
  (thread-spawn
   (lambda ()
     (handler-case
       (ws-handle-connection cfg conn)
       (error (e)
         (ws-log (format nil "connection error: ~A" e))))
     (handler-case
       (ws-conn-close conn)
       (error (e) #f))
     (ws-with-mutex
      active-mutex
      (lambda ()
        (setf (car active-box) (- (car active-box) 1))
        (if (< (car active-box) 0)
            (setf (car active-box) 0)
            nil))))))

(defun ws-start-server (cfg tls-mode)
  (let* ((port (ws-config-get cfg 'listen_port))
         (cert-file (ws-config-get cfg 'tls_cert_file))
         (key-file (ws-config-get cfg 'tls_key_file))
         (max-connections (ws-config-get cfg 'max_connections))
         (max-accepts-env (getenv "WEBSERVER_MAX_ACCEPTS"))
         (max-accepts (if (null max-accepts-env) 0 (ws-parse-int max-accepts-env)))
         (active-mutex (mutex-open))
         (active-box (list 0))
         (listener
          (handler-case
            (ws-make-listener tls-mode port cert-file key-file)
            (error (e)
              (ws-log-error (format nil "~A listener startup failed port=~A reason=~A"
                                    (if tls-mode "https" "http")
                                    port
                                    e))
              (throw 'ws-error e))))
         (accepted 0)
         (workers '()))
    (ws-log (format nil "~A server listening on port ~A"
                    (if tls-mode "https" "http")
                    port))
    (while (or (null max-accepts) (= max-accepts 0) (< accepted max-accepts))
      (let ((conn
             (handler-case
               (ws-accept-connection listener tls-mode)
               (error (e)
                 (ws-log-error (format nil "accept failed: ~A" e))
                 (throw 'ws-error e)))))
        (setq accepted (+ accepted 1))
        (let ((allow nil))
          (ws-with-mutex
           active-mutex
           (lambda ()
             (if (< (car active-box) max-connections)
                 (progn
                   (setf (car active-box) (+ (car active-box) 1))
                   (setq allow #t))
                 (setq allow nil))))
          (if allow
              (setq workers (cons (ws-spawn-connection-worker cfg conn active-mutex active-box) workers))
              (ws-handle-over-capacity conn)))))
    (handler-case
      (ws-close-listener listener tls-mode)
      (error (e) #f))
    (while (not (null workers))
      (handler-case
        (thread-join (car workers))
        (error (e) #f))
      (setq workers (cdr workers)))
    (ws-log (format nil "~A server stopped"
                    (if tls-mode "https" "http")))))

(defun ws-start-http-server (cfg)
  (ws-start-server cfg #f))

(defun ws-start-https-server (cfg)
  (ws-start-server cfg #t))
