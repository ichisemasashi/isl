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
            (throw 'ws-bad-request "unexpected EOF while reading request body")
            (setq out (string-append out (string ch)))))
      (setq i (+ i 1)))
    out))

(defun ws-read-request-line (conn)
  (let ((line (ws-conn-receive-line conn #f)))
    (if (null line)
        'eof
        (ws-strip-cr line))))

(defun ws-parse-request-line (line)
  (let ((a (ws-split-once line " ")))
    (if (null a)
        (throw 'ws-bad-request "invalid request-line")
        (let* ((method (car a))
               (b (ws-split-once (second a) " ")))
          (if (null b)
              (throw 'ws-bad-request "invalid request-line")
              (let ((target (car b))
                    (version (second b)))
                (if (or (string= version "HTTP/1.1")
                        (string= version "HTTP/1.0"))
                    (list method target version)
                    (throw 'ws-bad-request "unsupported HTTP version"))))))))

(defun ws-read-headers (conn)
  (let ((headers '())
        (done nil))
    (while (not done)
      (let ((raw (ws-conn-receive-line conn #f)))
        (if (null raw)
            (throw 'ws-bad-request "unexpected EOF in headers")
            (let ((line (ws-strip-cr raw)))
              (if (string= line "")
                  (setq done t)
                  (let ((p (string-index ":" line)))
                    (if (null p)
                        (throw 'ws-bad-request "invalid header line")
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
                (throw 'ws-bad-request "invalid Content-Length")
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

(defun ws-reason (status)
  (if (= status 200) "OK"
      (if (= status 400) "Bad Request"
          (if (= status 403) "Forbidden"
              (if (= status 404) "Not Found"
                  (if (= status 405) "Method Not Allowed"
                      "Internal Server Error"))))))

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
  (with-open-file (s path :direction :input)
    (let ((ch (read-char s #f))
          (acc ""))
      (while (not (null ch))
        (setq acc (string-append acc (string ch)))
        (setq ch (read-char s #f)))
      acc)))

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
    (list "Content-Length" (format nil "~A" (length body)))
    (list "Connection" (if keep-alive "keep-alive" "close")))
   body
   #t))

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
                      (let ((body (ws-read-file-text candidate))
                            (mime (ws-static-mime-type candidate)))
                        (list 'ok candidate mime body)))))))))

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

(defun ws-resolve-cgi-script (cfg target-path)
  (if (not (ws-cgi-request-path-p target-path))
      (list 'not-cgi)
      (let* ((cgi-bin-dir (ws-config-get cfg 'cgi_bin_dir))
             (prefix-len (length "/cgi-bin/"))
             (rel-path (substring target-path prefix-len (length target-path)))
             (abs-script (ws-normalize-path (string-append cgi-bin-dir "/" rel-path))))
        (if (= (length rel-path) 0)
            (list 'error 404 "not found\n")
            (if (ws-cgi-script-resolvable-p cgi-bin-dir rel-path abs-script)
                (list 'ok abs-script)
                (list 'error 404 "not found\n"))))))

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

(defun ws-run-cgi-script (cfg req script-path)
  (let* ((target (ws-request-get req 'target))
         (method (ws-request-get req 'method))
         (version (ws-request-get req 'version))
         (headers (ws-request-get req 'headers))
         (body (ws-request-get req 'body))
         (query (ws-target-query target))
         (content-type (ws-header-get headers "content-type"))
         (stdin-path (ws-temp-file "webserver-cgi-in" ".tmp"))
         (stdout-path (ws-temp-file "webserver-cgi-out" ".tmp"))
         (cmd (string-append
               (ws-shell-quote script-path)
               " < " (ws-shell-quote stdin-path)
               " > " (ws-shell-quote stdout-path))))
    (ws-write-file-raw stdin-path body)
    (setenv "REQUEST_METHOD" method)
    (setenv "QUERY_STRING" query)
    (setenv "CONTENT_LENGTH" (format nil "~A" (length body)))
    (setenv "CONTENT_TYPE" (if (null content-type) "" content-type))
    (setenv "SCRIPT_NAME" (ws-target-path-only target))
    (setenv "SERVER_PROTOCOL" version)
    (setenv "SERVER_PORT" (format nil "~A" (ws-config-get cfg 'listen_port)))
    (setenv "GATEWAY_INTERFACE" "CGI/1.1")
    (let ((status (system cmd)))
      (if (not (= status 0))
          (progn
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
            (list 'ok resp-status resp-headers resp-body))))))

(defun ws-handle-cgi-request (conn cfg req)
  (let* ((method (ws-request-get req 'method))
         (version (ws-request-get req 'version))
         (keep-alive (ws-request-get req 'keep-alive))
         (path (ws-target-path-only (ws-request-get req 'target)))
         (resolved (ws-resolve-cgi-script cfg path)))
    (if (eq (car resolved) 'ok)
        (let ((run (ws-run-cgi-script cfg req (second resolved))))
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
                                (list "Content-Length" (format nil "~A" (length body)))
                                (list "Connection" (if keep-alive "keep-alive" "close"))))))
                (ws-send-response conn version status headers body send-body)
                keep-alive)
              (progn
                (ws-http-error-response conn version 500 keep-alive "cgi execution failed\n")
                keep-alive)))
        (progn
          (ws-http-error-response conn version (second resolved) keep-alive (third resolved))
          keep-alive))))

(defun ws-handle-static-request (conn cfg req)
  (let* ((method (ws-request-get req 'method))
         (target (ws-request-get req 'target))
         (version (ws-request-get req 'version))
         (keep-alive (ws-request-get req 'keep-alive))
         (path (ws-target-path-only target))
         (resolved (ws-resolve-static-file cfg path)))
    (if (eq (car resolved) 'ok)
        (let* ((mime (third resolved))
               (body (fourth resolved))
               (send-body (not (string= method "HEAD"))))
          (ws-send-response
           conn
           version
           200
           (list
            (list "Content-Type" mime)
            (list "Content-Length" (format nil "~A" (length body)))
            (list "Connection" (if keep-alive "keep-alive" "close")))
           body
           send-body)
          keep-alive)
        (progn
          (ws-http-error-response conn version (second resolved) keep-alive (third resolved))
          keep-alive))))

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
           (list "Content-Length" (format nil "~A" (length content)))
           (list "Connection" (if keep-alive "keep-alive" "close")))))
    (ws-send-response conn version 200 base-headers content send-body)
    keep-alive))

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
                  keep-alive))
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
          keep-alive))))

(defun ws-handle-connection (cfg conn)
  (let ((alive #t))
    (while alive
      (let ((req (ws-read-http-request conn)))
        (if (eq req 'eof)
            (setq alive #f)
            (if (and (listp req) (eq (car req) 'ok))
                (setq alive (ws-handle-http-request conn cfg req))
                (progn
                  (ws-send-response
                   conn
                   "HTTP/1.1"
                   400
                   (list
                    (list "Content-Type" "text/plain; charset=UTF-8")
                    (list "Content-Length" "12")
                    (list "Connection" "close"))
                   "bad request\n"
                   #t)
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

(defun ws-start-server (cfg tls-mode)
  (let* ((port (ws-config-get cfg 'listen_port))
         (cert-file (ws-config-get cfg 'tls_cert_file))
         (key-file (ws-config-get cfg 'tls_key_file))
         (max-accepts-env (getenv "WEBSERVER_MAX_ACCEPTS"))
         (max-accepts (if (null max-accepts-env) 0 (ws-parse-int max-accepts-env)))
         (listener (ws-make-listener tls-mode port cert-file key-file))
         (accepted 0))
    (ws-log (format nil "~A server listening on port ~A"
                    (if tls-mode "https" "http")
                    port))
    (while (or (null max-accepts) (= max-accepts 0) (< accepted max-accepts))
      (let ((conn (ws-accept-connection listener tls-mode)))
        (setq accepted (+ accepted 1))
        (handler-case
          (ws-handle-connection cfg conn)
          (error (e)
            (ws-log (format nil "connection error: ~A" e))))
        (handler-case
          (ws-conn-close conn)
          (error (e) #f))))
    (handler-case
      (ws-close-listener listener tls-mode)
      (error (e) #f))
    (ws-log (format nil "~A server stopped"
                    (if tls-mode "https" "http")))))

(defun ws-start-http-server (cfg)
  (ws-start-server cfg #f))

(defun ws-start-https-server (cfg)
  (ws-start-server cfg #t))
