;; ISL Wiki MVP routes:
;;   /wiki              -> index
;;   /wiki/<slug>       -> view
;;   /wiki/<slug>/edit  -> edit form (save is not implemented yet)

(defun env-or-empty (name)
  (let ((v (getenv name)))
    (if (null v) "" v)))

(defun wiki-root ()
  (let ((v (getenv "ISL_ROOT")))
    (if (or (null v) (= (length v) 0))
        "/Volumes/SSD-PLU3/work/LISP/islisp/isl"
        v)))

(defun wiki-path (suffix)
  (string-append (wiki-root) suffix))

(load (wiki-path "/lib/os-utils.lsp"))
(load (wiki-path "/examples/dbms/app/repr.lsp"))
(load (wiki-path "/examples/dbms/app/profile.lsp"))
(load (wiki-path "/examples/dbms/app/engine.lsp"))

(defun request-method ()
  (let ((m (env-or-empty "REQUEST_METHOD")))
    (if (= (length m) 0) "GET" m)))

(defun starts-with (s prefix)
  (let ((n (length s))
        (m (length prefix)))
    (if (< n m)
        nil
        (string= (substring s 0 m) prefix))))

(defun app-base ()
  (let ((script-name (env-or-empty "SCRIPT_NAME")))
    (if (= (length script-name) 0)
        "/wiki"
        script-name)))

(defun wiki-config-path ()
  (wiki-path "/examples/wiki/conf/wiki.conf"))

(defun conf-trim-left (s)
  (let ((i 0)
        (n (length s)))
    (while (and (< i n)
                (not (null (string-index (substring s i (+ i 1)) " \t"))))
      (setq i (+ i 1)))
    (substring s i n)))

(defun conf-trim-right (s)
  (let ((n (length s))
        (i (- (length s) 1)))
    (while (and (>= i 0)
                (not (null (string-index (substring s i (+ i 1)) " \t"))))
      (setq i (- i 1)))
    (substring s 0 (+ i 1))))

(defun conf-trim (s)
  (conf-trim-right (conf-trim-left s)))

(defun conf-split-kv (line)
  (let ((p (string-index "=" line)))
    (if (null p)
        '()
        (list (conf-trim (substring line 0 p))
              (conf-trim (substring line (+ p 1) (length line)))))))

(defun conf-comment-or-blank-p (line)
  (let ((tline (conf-trim line)))
    (or (= (length tline) 0)
        (starts-with tline "#")
        (starts-with tline ";"))))

(defun wiki-reverse (xs)
  (let ((cur xs)
        (acc '()))
    (while (not (null cur))
      (setq acc (cons (car cur) acc))
      (setq cur (cdr cur)))
    acc))

(defun read-wiki-config-file (path)
  (if (null (probe-file path))
      '()
      (with-open-file (s path :direction :input)
        (let ((line (read-line s #f))
              (acc '()))
          (while (not (null line))
            (if (conf-comment-or-blank-p line)
                nil
                (let ((kv (conf-split-kv line)))
                  (if (or (null kv) (= (length (first kv)) 0))
                      nil
                      (setq acc (cons kv acc)))))
            (setq line (read-line s #f)))
          (wiki-reverse acc)))))

(defglobal *wiki-config-cache* '())

(defun wiki-config ()
  (if (null *wiki-config-cache*)
      (setq *wiki-config-cache* (read-wiki-config-file (wiki-config-path)))
      nil)
  *wiki-config-cache*)

(defun wiki-config-get (key fallback)
  (let ((xs (wiki-config))
        (found '()))
    (while (and (null found) (not (null xs)))
      (let ((kv (car xs)))
        (if (string= (first kv) key)
            (setq found (second kv))
            nil))
      (setq xs (cdr xs)))
    (if (null found) fallback found)))

(defun trim-leading-slashes (s)
  (if (> (length s) 0)
      (if (string= (substring s 0 1) "/")
          (trim-leading-slashes (substring s 1))
          s)
      s))

(defun split-path (s)
  (if (= (length s) 0)
      '()
      (let ((sep (string-index "/" s)))
        (if (null sep)
            (list s)
            (cons (substring s 0 sep)
                  (split-path (substring s (+ sep 1))))))))

(defun path-segments ()
  (let* ((raw (env-or-empty "PATH_INFO"))
         (trimmed (trim-leading-slashes raw)))
    (if (= (length trimmed) 0)
        '()
        (split-path trimmed))))

(defun trim-trailing-slashes (s)
  (let ((n (length s)))
    (if (> n 1)
        (if (string= (substring s (- n 1) n) "/")
            (trim-trailing-slashes (substring s 0 (- n 1)))
            s)
        s)))

(defun canonical-path-info (raw)
  (if (= (length raw) 0)
      ""
      (let ((t1 (trim-trailing-slashes raw)))
        (if (= (length t1) 0)
            ""
            (if (string= (substring t1 0 1) "/")
                t1
                (string-append "/" t1))))))

(defun needs-canonical-redirect-p (raw)
  (if (= (length raw) 0)
      nil
      (if (string= raw "/")
          nil
          (if (string= raw (canonical-path-info raw))
              nil
              t))))

(defun safe-text (v)
  (if (null v) "" v))

(defun html-escape (s)
  (let ((text (safe-text s))
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

(defun sql-escape (s)
  (let ((text (safe-text s))
        (i 0)
        (out ""))
    (while (< i (length text))
      (let ((ch (substring text i (+ i 1))))
        (if (string= ch "'")
            (setq out (string-append out "''"))
            (setq out (string-append out ch))))
      (setq i (+ i 1)))
    out))

(defun json-escape (s)
  (let ((text (safe-text s))
        (i 0)
        (out ""))
    (while (< i (length text))
      (let ((ch (substring text i (+ i 1))))
        (setq out
              (string-append
               out
               (cond
                ((string= ch "\\") "\\\\")
                ((string= ch "\"") "\\\"")
                ((string= ch "\n") "\\n")
                ((string= ch "\r") "\\r")
                ((string= ch "\t") "\\t")
                (t ch)))))
      (setq i (+ i 1)))
    out))

(defun slug-safe-char-p (ch)
  (not (null (string-index ch "abcdefghijklmnopqrstuvwxyz0123456789-"))))

(defun slug-safe-p (slug)
  (let ((n (length slug))
        (i 0)
        (ok t))
    (if (= n 0)
        nil
        (if (> n 128)
            nil
            (progn
          (if (string= (substring slug 0 1) "-")
              (setq ok nil)
              (if (string= (substring slug (- n 1) n) "-")
                  (setq ok nil)
                  nil))
          (while (if ok (< i n) nil)
            (if (slug-safe-char-p (substring slug i (+ i 1)))
                nil
              (setq ok nil))
            (setq i (+ i 1)))
          ok)))))

(defun reserved-slug-p (slug)
  (or (string= slug "new")
      (string= slug "media")
      (string= slug "search")
      (string= slug "admin")
      (string= slug "login")
      (string= slug "logout")))

(defun media-root-dir ()
  (wiki-config-get "media_dir" (wiki-path "/examples/webserver/runtime/docroot/public/wiki-files")))

(defun backup-root-dir ()
  (wiki-config-get "backup_dir" "/tmp/isl-wiki-backups"))

(defun backup-keep-count ()
  (let ((raw (wiki-config-get "backup_keep_count" "7")))
    (let ((n (parse-int-safe raw)))
      (if (null n) 7 n))))

(defun session-cookie-name ()
  (wiki-config-get "session_cookie_name" "isl_wiki_session"))

(defun session-duration-days ()
  (let ((raw (wiki-config-get "session_days" "7")))
    (if (string= raw "") "7" raw)))

(defglobal *wiki-current-user* '())
(defglobal *response-extra-headers* '())
(defglobal *wiki-request-id* "")

(defun clear-response-extra-headers ()
  (setq *response-extra-headers* '()))

(defun add-response-header (name value)
  (setq *response-extra-headers* (cons (list name value) *response-extra-headers*)))

(defun current-user ()
  *wiki-current-user*)

(defun current-username ()
  (if (null *wiki-current-user*) "" (first *wiki-current-user*)))

(defun current-display-name ()
  (if (null *wiki-current-user*) "" (second *wiki-current-user*)))

(defun current-role ()
  (if (null *wiki-current-user*) "" (third *wiki-current-user*)))

(defun current-session-token ()
  (if (null *wiki-current-user*) "" (fourth *wiki-current-user*)))

(defun current-csrf-token ()
  (if (null *wiki-current-user*) "" (fifth *wiki-current-user*)))

(defun logged-in-p ()
  (not (null *wiki-current-user*)))

(defun role-rank (role-name)
  (cond
   ((string= role-name "admin") 30)
   ((string= role-name "editor") 20)
   ((string= role-name "viewer") 10)
   (t 0)))

(defun role-allowed-p (actual required)
  (>= (role-rank actual) (role-rank required)))

(defun editor-or-admin-p ()
  (role-allowed-p (current-role) "editor"))

(defun admin-p ()
  (role-allowed-p (current-role) "admin"))

(defun valid-page-status-p (status)
  (or (string= status "draft")
      (string= status "published")
      (string= status "private")))

(defun visible-page-status-sql (alias)
  (if (admin-p)
      "1=1"
      (if (editor-or-admin-p)
          (string-append alias ".status in ('draft', 'published')")
          (string-append alias ".status = 'published'"))))

(defun request-cookie-header ()
  (env-or-empty "HTTP_COOKIE"))

(defun parse-cookie-value (cookie-name)
  (let ((parts (split-on (request-cookie-header) ";"))
        (found ""))
    (dolist (part parts)
      (let* ((trimmed (trim-ws part))
             (kv (split-once trimmed "="))
             (name (trim-ws (first kv)))
             (value (second kv)))
        (if (and (string= found "") (string= name cookie-name))
            (setq found value)
            nil)))
    found))

(defun cookie-path ()
  (app-base))

(defun session-cookie-value ()
  (parse-cookie-value (session-cookie-name)))

(defun clear-session-cookie-header ()
  (add-response-header
   "Set-Cookie"
   (string-append (session-cookie-name)
                  "=; Path="
                  (cookie-path)
                  "; HttpOnly; SameSite=Lax; Max-Age=0")))

(defun set-session-cookie-header (token)
  (add-response-header
   "Set-Cookie"
   (string-append (session-cookie-name)
                  "="
                  token
                  "; Path="
                  (cookie-path)
                  "; HttpOnly; SameSite=Lax")))

(defun current-request-target ()
  (let ((path (env-or-empty "PATH_INFO"))
        (query (env-or-empty "QUERY_STRING")))
    (if (string= query "")
        (string-append (app-base) (if (string= path "") "" path))
        (string-append (app-base) (if (string= path "") "" path) "?" query))))

(defun log-root-dir ()
  (wiki-config-get "log_dir" "/tmp/isl-wiki-logs"))

(defun ensure-log-dir ()
  (os-mkdir-p (log-root-dir))
  (os-chmod "755" (log-root-dir)))

(defun request-id ()
  *wiki-request-id*)

(defun generate-request-id ()
  (string-append "req-" (format nil "~A" (get-universal-time)) "-" (format nil "~A" *markdown-temp-counter*)))

(defun init-request-id ()
  (setq *wiki-request-id* (generate-request-id))
  (add-response-header "X-Request-Id" *wiki-request-id*))

(defun iso-now ()
  (os-date-utc-iso8601))

(defun log-line-path ()
  (string-append (log-root-dir) "/wiki-app.log"))

(defun append-log-line (line)
  (ensure-log-dir)
  (with-open-file (s (log-line-path)
                     :direction :output
                     :if-exists :append
                     :if-does-not-exist :create)
    (format s "~A~%" line)))

(defun debug-stderr (msg)
  (with-open-file (s "/dev/stderr" :direction :output :if-exists :append)
    (format s "wiki-debug: ~A~%" msg)))

(defun write-structured-log (level event fields-json)
  (append-log-line
   (string-append
    "{\"ts\":\"" (json-escape (iso-now))
    "\",\"level\":\"" (json-escape level)
    "\",\"event\":\"" (json-escape event)
    "\",\"request_id\":\"" (json-escape (request-id))
    "\",\"method\":\"" (json-escape (request-method))
    "\",\"path\":\"" (json-escape (env-or-empty "PATH_INFO"))
    "\",\"user\":\"" (json-escape (current-username))
    "\",\"role\":\"" (json-escape (current-role))
    "\",\"fields\":" fields-json
    "}")))

(defun make-log-fields-1 (k1 v1)
  (string-append "{\"" k1 "\":\"" (json-escape v1) "\"}"))

(defun make-log-fields-2 (k1 v1 k2 v2)
  (string-append
   "{\"" k1 "\":\"" (json-escape v1)
   "\",\"" k2 "\":\"" (json-escape v2) "\"}"))

(defun make-log-fields-3 (k1 v1 k2 v2 k3 v3)
  (string-append
   "{\"" k1 "\":\"" (json-escape v1)
   "\",\"" k2 "\":\"" (json-escape v2)
   "\",\"" k3 "\":\"" (json-escape v3) "\"}"))

(defun write-error-log (message)
  (write-structured-log
   "error"
   "request.error"
   (make-log-fields-2
    "target" (current-request-target)
    "message" message)))

(defun login-target-url ()
  (string-append (app-base) "/login?next=" (url-encode-lite (current-request-target))))

(defun issue-session-token ()
  (os-generate-token))

(defun issue-csrf-token ()
  (issue-session-token))

(defun media-public-base ()
  (wiki-config-get "media_base_url" (string-append (app-base) "/files")))

(defun media-delivery-url (stored-filename)
  (string-append (media-public-base) "/" stored-filename))

(defglobal *command-temp-counter* 0)

(defun command-output (cmd)
  (setq *command-temp-counter* (+ *command-temp-counter* 1))
  (os-command-output cmd))

(defun inline-cache-dir ()
  "/tmp/isl-wiki-inline-cache")

(defun ensure-inline-cache-dir ()
  (os-mkdir-p (inline-cache-dir)))

(defun inline-cache-path (stored-filename)
  (string-append (inline-cache-dir) "/" (sanitize-filename stored-filename) ".b64"))

(defun inline-thumb-cache-path (stored-filename)
  (string-append (inline-cache-dir) "/" (sanitize-filename stored-filename) ".thumb.b64"))

(defun inline-media-url (db stored-filename mime-type)
  (let ((meta (fetch-media-file-meta db stored-filename)))
    (if (null meta)
        ""
        (let ((storage-path (first meta))
              (mtype (if (blank-text-p mime-type) "application/octet-stream" mime-type)))
          (if (null (probe-file storage-path))
              ""
              (let* ((cache-path (inline-cache-path stored-filename))
                     (cached (if (null (probe-file cache-path))
                                 ""
                                 (trim-ws (read-file-text cache-path)))))
                (if (not (blank-text-p cached))
                    (string-append "data:" mtype ";base64," cached)
                    (progn
                      (ensure-inline-cache-dir)
                      (let* ((thumb-path (string-append "/tmp/isl-wiki-thumb-"
                                                        (format nil "~A" (get-universal-time))
                                                        "-"
                                                        (format nil "~A" *command-temp-counter*)
                                                        ".jpg"))
                             (use-thumb (os-image-thumb-jpeg storage-path thumb-path 256))
                             (source-path (if use-thumb thumb-path storage-path))
                             (source-mime (if use-thumb "image/jpeg" mtype))
                             (b64 (os-base64-file-no-newline source-path)))
                        (if use-thumb
                            (if (null (probe-file thumb-path)) nil (delete-file thumb-path))
                            nil)
                        (if (blank-text-p b64)
                            ""
                            (progn
                              (write-file-text cache-path b64)
                              (string-append "data:" source-mime ";base64," b64))))))))))))

(defun inline-media-thumb-url (db stored-filename mime-type)
  (let ((meta (fetch-media-file-meta db stored-filename)))
    (if (null meta)
        ""
        (let ((storage-path (first meta))
              (mtype (if (blank-text-p mime-type) "application/octet-stream" mime-type)))
          (if (null (probe-file storage-path))
              ""
              (let* ((cache-path (inline-thumb-cache-path stored-filename))
                     (cached (if (null (probe-file cache-path))
                                 ""
                                 (trim-ws (read-file-text cache-path)))))
                (if (not (blank-text-p cached))
                    (string-append "data:image/jpeg;base64," cached)
                    (progn
                      (ensure-inline-cache-dir)
                      (let* ((thumb-path (string-append "/tmp/isl-wiki-thumb-"
                                                        (format nil "~A" (get-universal-time))
                                                        "-"
                                                        (format nil "~A" *command-temp-counter*)
                                                        ".jpg"))
                             (thumb-ok (os-image-thumb-jpeg storage-path thumb-path 220))
                             (b64 (if thumb-ok
                                      (os-base64-file-no-newline thumb-path)
                                      "")))
                        (if thumb-ok
                            (if (null (probe-file thumb-path)) nil (delete-file thumb-path))
                            nil)
                        (if (blank-text-p b64)
                            ""
                            (progn
                              (write-file-text cache-path b64)
                              (string-append "data:image/jpeg;base64," b64))))))))))))

(defun ensure-media-dir ()
  (os-mkdir-p (media-root-dir))
  (os-chmod "755" (media-root-dir)))

(defun ensure-backup-dir ()
  (os-mkdir-p (backup-root-dir))
  (os-chmod "755" (backup-root-dir)))

(defun contains-char-p (s ch)
  (not (null (string-index ch s))))

(defun shell-quote (s)
  (os-shell-quote s))

(defun last-index-of (s needle)
  (let ((i 0)
        (last '()))
    (while (< i (length s))
      (if (string= (substring s i (+ i 1)) needle)
          (setq last i)
          nil)
      (setq i (+ i 1)))
    last))

(defun basename (path)
  (let ((p (last-index-of path "/")))
    (if (null p)
        path
        (substring path (+ p 1)))))

(defun ascii-downcase-string (s)
  (let ((i 0)
        (out ""))
    (while (< i (length s))
      (let* ((ch (substring s i (+ i 1)))
             (code (char->integer ch)))
        (if (and (>= code 65) (<= code 90))
            (setq out (string-append out (string (integer->char (+ code 32)))))
            (setq out (string-append out ch))))
      (setq i (+ i 1)))
    out))

(defun file-ext-lower (name)
  (let ((p (last-index-of name ".")))
    (if (null p)
        ""
        (ascii-downcase-string (substring name (+ p 1))))))

(defun safe-file-char-p (ch)
  (or (not (null (string-index ch "abcdefghijklmnopqrstuvwxyz")))
      (not (null (string-index ch "ABCDEFGHIJKLMNOPQRSTUVWXYZ")))
      (not (null (string-index ch "0123456789")))
      (string= ch ".")
      (string= ch "-")
      (string= ch "_")))

(defun sanitize-filename (name)
  (let ((i 0)
        (out ""))
    (while (< i (length name))
      (let ((ch (substring name i (+ i 1))))
        (if (safe-file-char-p ch)
            (setq out (string-append out ch))
            (setq out (string-append out "_"))))
      (setq i (+ i 1)))
    (if (= (length out) 0) "file.bin" out)))

(defun media-max-bytes ()
  (let ((raw (wiki-config-get "media_max_bytes" "10485760")))
    (let ((n (parse-int-safe raw)))
      (if (null n) 10485760 n))))

(defun dangerous-media-extension-p (ext)
  (or (string= ext "html")
      (string= ext "htm")
      (string= ext "xhtml")
      (string= ext "js")
      (string= ext "mjs")
      (string= ext "cjs")
      (string= ext "svg")
      (string= ext "svgz")
      (string= ext "exe")
      (string= ext "dll")
      (string= ext "com")
      (string= ext "bat")
      (string= ext "cmd")
      (string= ext "msi")
      (string= ext "ps1")
      (string= ext "sh")
      (string= ext "bash")
      (string= ext "zsh")
      (string= ext "php")
      (string= ext "pl")
      (string= ext "py")
      (string= ext "rb")
      (string= ext "jar")))

(defun allowed-media-extension-p (ext)
  (or (string= ext "png")
      (string= ext "jpg")
      (string= ext "jpeg")
      (string= ext "gif")
      (string= ext "webp")
      (string= ext "mp4")
      (string= ext "webm")
      (string= ext "mov")
      (string= ext "mkv")
      (string= ext "mp3")
      (string= ext "wav")
      (string= ext "ogg")
      (string= ext "m4a")
      (string= ext "flac")
      (string= ext "pdf")
      (string= ext "zip")
      (string= ext "gz")
      (string= ext "tgz")
      (string= ext "tar")
      (string= ext "7z")
      (string= ext "txt")
      (string= ext "md")
      (string= ext "csv")
      (string= ext "json")))

(defun allowed-mime-for-ext-p (ext mime-type)
  (cond
   ((string= ext "png") (string= mime-type "image/png"))
   ((or (string= ext "jpg") (string= ext "jpeg")) (string= mime-type "image/jpeg"))
   ((string= ext "gif") (string= mime-type "image/gif"))
   ((string= ext "webp") (string= mime-type "image/webp"))
   ((string= ext "mp4") (string= mime-type "video/mp4"))
   ((string= ext "webm") (string= mime-type "video/webm"))
   ((string= ext "mov") (string= mime-type "video/quicktime"))
   ((string= ext "mkv") (string= mime-type "video/x-matroska"))
   ((string= ext "mp3") (string= mime-type "audio/mpeg"))
   ((string= ext "wav") (or (string= mime-type "audio/wav")
                            (string= mime-type "audio/x-wav")))
   ((string= ext "ogg") (string= mime-type "audio/ogg"))
   ((string= ext "m4a") (string= mime-type "audio/mp4"))
   ((string= ext "flac") (or (string= mime-type "audio/flac")
                             (string= mime-type "audio/x-flac")))
   ((string= ext "pdf") (string= mime-type "application/pdf"))
   ((string= ext "zip") (or (string= mime-type "application/zip")
                            (string= mime-type "application/x-zip-compressed")))
   ((or (string= ext "gz") (string= ext "tgz")) (or (string= mime-type "application/gzip")
                                                    (string= mime-type "application/x-gzip")))
   ((string= ext "tar") (string= mime-type "application/x-tar"))
   ((string= ext "7z") (string= mime-type "application/x-7z-compressed"))
   ((string= ext "txt") (string= mime-type "text/plain"))
   ((string= ext "md") (or (string= mime-type "text/plain")
                           (string= mime-type "text/markdown")))
   ((string= ext "csv") (or (string= mime-type "text/csv")
                            (string= mime-type "text/plain")))
   ((string= ext "json") (or (string= mime-type "application/json")
                             (string= mime-type "text/plain")))
   (t nil)))

(defun media-mime-allowed-p (mime-type)
  (or (string= mime-type "image/png")
      (string= mime-type "image/jpeg")
      (string= mime-type "image/gif")
      (string= mime-type "image/webp")
      (string= mime-type "video/mp4")
      (string= mime-type "video/webm")
      (string= mime-type "video/quicktime")
      (string= mime-type "video/x-matroska")
      (string= mime-type "audio/mpeg")
      (string= mime-type "audio/wav")
      (string= mime-type "audio/x-wav")
      (string= mime-type "audio/ogg")
      (string= mime-type "audio/mp4")
      (string= mime-type "audio/flac")
      (string= mime-type "audio/x-flac")
      (string= mime-type "application/pdf")
      (string= mime-type "application/zip")
      (string= mime-type "application/x-zip-compressed")
      (string= mime-type "application/gzip")
      (string= mime-type "application/x-gzip")
      (string= mime-type "application/x-tar")
      (string= mime-type "application/x-7z-compressed")
      (string= mime-type "text/plain")
      (string= mime-type "text/markdown")
      (string= mime-type "text/csv")
      (string= mime-type "application/json")))

(defun valid-media-type-p (media-type)
  (or (string= media-type "image")
      (string= media-type "video")
      (string= media-type "audio")
      (string= media-type "file")))

(defun infer-media-type (filename)
  (let ((ext (file-ext-lower filename)))
    (if (or (string= ext "png") (string= ext "jpg") (string= ext "jpeg") (string= ext "gif") (string= ext "webp"))
        "image"
        (if (or (string= ext "mp4") (string= ext "webm") (string= ext "mov") (string= ext "mkv"))
            "video"
            (if (or (string= ext "mp3") (string= ext "wav") (string= ext "ogg") (string= ext "m4a") (string= ext "flac"))
                "audio"
                "file")))))

(defun infer-mime-type (filename media-type)
  (let ((ext (file-ext-lower filename)))
    (cond
     ((string= ext "png") "image/png")
     ((or (string= ext "jpg") (string= ext "jpeg")) "image/jpeg")
     ((string= ext "gif") "image/gif")
     ((string= ext "webp") "image/webp")
     ((string= ext "mp4") "video/mp4")
     ((string= ext "webm") "video/webm")
     ((string= ext "mov") "video/quicktime")
     ((string= ext "mkv") "video/x-matroska")
     ((string= ext "mp3") "audio/mpeg")
     ((string= ext "wav") "audio/wav")
     ((string= ext "ogg") "audio/ogg")
     ((string= ext "m4a") "audio/mp4")
     ((string= ext "flac") "audio/flac")
     ((string= ext "pdf") "application/pdf")
     ((string= ext "zip") "application/zip")
     ((string= ext "gz") "application/gzip")
     ((string= ext "tgz") "application/gzip")
     ((string= ext "tar") "application/x-tar")
     ((string= ext "7z") "application/x-7z-compressed")
     ((string= ext "txt") "text/plain")
     ((string= ext "md") "text/markdown")
     ((string= ext "csv") "text/csv")
     ((string= ext "json") "application/json")
     (t
     (if (string= media-type "video")
          "video/mp4"
          (if (string= media-type "audio")
              "audio/mpeg"
              (if (string= media-type "image")
                  "image/png"
                  "application/octet-stream")))))))

(defun media-display-type (media-type mime-type)
  (cond
   ((string= media-type "image") "image")
   ((string= media-type "video") "video")
   ((string= media-type "audio") "audio")
   ((starts-with mime-type "image/") "image")
   ((starts-with mime-type "video/") "video")
   ((starts-with mime-type "audio/") "audio")
   (t "file")))

(defun render-media-embed (media-url media-title media-type media-mime)
  (let ((dtype (media-display-type media-type media-mime)))
    (cond
     ((string= dtype "image")
      (format t "<img src=\"~A\" alt=\"~A\" style=\"max-width:100%;height:auto;border:1px solid #ddd;border-radius:6px\">~%"
              (html-escape media-url)
              (html-escape media-title)))
     ((string= dtype "video")
      (format t "<video controls style=\"max-width:100%\" src=\"~A\"></video>~%"
              (html-escape media-url)))
     ((string= dtype "audio")
      (format t "<audio controls src=\"~A\"></audio>~%"
              (html-escape media-url)))
     (t
      (format t "<p><a href=\"~A\" download>Download file</a></p>~%"
              (html-escape media-url))))))

(defun db-root ()
  (let ((v (getenv "ISL_WIKI_DB_ROOT")))
    (if (or (null v) (= (length v) 0))
        (wiki-config-get "db_root" "./examples/dbms/storage/wiki")
        v)))

(defun db-display-target ()
  (db-root))

(defun configure-dbms-root ()
  (setenv "DBMS_STORAGE_ROOT" (db-root))
  (dbms-set-active-profile *dbms-profile-wiki-compat-v1*))

(defun db-result-error-message (result fallback)
  (if (and (dbms-result-p result)
           (eq (second result) 'error)
           (dbms-error-p (third result)))
      (third (third result))
      fallback))

(defun db-exec! (catalog sql)
  (configure-dbms-root)
  (debug-stderr (format nil "db-exec begin ~A"
                        (if (> (length sql) 80)
                            (substring sql 0 80)
                            sql)))
  (let ((result (dbms-exec-sql (dbms-engine-init) sql)))
    (if (and (dbms-result-p result)
             (not (eq (second result) 'error)))
        (progn
          (debug-stderr "db-exec ok")
          result)
        (error (db-result-error-message result "dbms exec failed") sql))))

(defun db-table-rows (table-name)
  (configure-dbms-root)
  (let ((state (dbms-storage-load-table-rows table-name)))
    (if (dbms-error-p state)
        '()
        (third state))))

(defun db-row-value (row column-name)
  (let ((v (dbms-row-get-value row column-name)))
    (if (eq v 'NULL)
        ""
        (if (stringp v) v (format nil "~A" v)))))

(defun db-row-nullable (row column-name)
  (dbms-row-get-value row column-name))

(defun db-now-text ()
  (format nil "~A" (get-universal-time)))

(defun db-bool-sql (flag)
  (if flag "TRUE" "FALSE"))

(defun db-text-sql (value)
  (string-append "'" (sql-escape (safe-text value)) "'"))

(defun db-nullable-text-sql (value)
  (if (blank-text-p value)
      "NULL"
      (db-text-sql value)))

(defun db-nullable-number-sql (value)
  (if (null value)
      "NULL"
      (format nil "~A" value)))

(defun visible-page-p (status)
  (if (admin-p)
      t
      (if (editor-or-admin-p)
          (or (string= status "draft") (string= status "published"))
          (string= status "published"))))

(defun value-string< (a b)
  (string< (safe-text (format nil "~A" a)) (safe-text (format nil "~A" b))))

(defun value< (a b)
  (if (and (numberp a) (numberp b))
      (< a b)
      (value-string< a b)))

(defun value> (a b)
  (if (and (numberp a) (numberp b))
      (> a b)
      (value-string< b a)))

(defun sort-rows-by-insert (row rows key-fn desc)
  (if (null rows)
      (list row)
      (let ((left (funcall key-fn row))
            (right (funcall key-fn (car rows))))
        (if (if desc (value> left right) (value< left right))
            (cons row rows)
            (cons (car rows) (sort-rows-by-insert row (cdr rows) key-fn desc))))))

(defun sort-rows-by (rows key-fn desc)
  (if (null rows)
      '()
      (sort-rows-by-insert (car rows)
                           (sort-rows-by (cdr rows) key-fn desc)
                           key-fn
                           desc)))

(defun find-first-row (rows pred)
  (if (null rows)
      '()
      (if (funcall pred (car rows))
          (car rows)
          (find-first-row (cdr rows) pred))))

(defun filter-rows (rows pred)
  (if (null rows)
      '()
      (if (funcall pred (car rows))
          (cons (car rows) (filter-rows (cdr rows) pred))
          (filter-rows (cdr rows) pred))))

(defun table-row-by-id (table-name id)
  (find-first-row
   (db-table-rows table-name)
   (lambda (row) (= (db-row-nullable row "id") id))))

(defun find-page-row-any (slug)
  (find-first-row
   (db-table-rows "pages")
   (lambda (row) (string= (db-row-value row "slug") slug))))

(defun find-page-row-visible (slug)
  (find-first-row
   (db-table-rows "pages")
   (lambda (row)
     (and (string= (db-row-value row "slug") slug)
          (string= (db-row-value row "deleted_at") "")
          (visible-page-p (db-row-value row "status"))))))

(defun find-user-row (username)
  (find-first-row
   (db-table-rows "users")
   (lambda (row) (string= (db-row-value row "username") username))))

(defun user-id-by-username (username)
  (let ((row (find-user-row username)))
    (if (null row) '() (db-row-nullable row "id"))))

(defun page-id-by-slug (slug)
  (let ((row (find-page-row-any slug)))
    (if (null row) '() (db-row-nullable row "id"))))

(defun string-contains-ci-p (needle haystack)
  (not (null (string-index (ascii-downcase-string needle)
                           (ascii-downcase-string haystack)))))

(defun ensure-wiki-dbms-schema (catalog)
  (dolist (sql
           (list
            "CREATE TABLE IF NOT EXISTS pages (id BIGSERIAL PRIMARY KEY, slug TEXT NOT NULL, title TEXT NOT NULL, body_md TEXT NOT NULL, created_at TIMESTAMPTZ DEFAULT now(), updated_at TIMESTAMPTZ DEFAULT now(), last_edited_by TEXT, current_rev_no INTEGER NOT NULL, deleted_at TIMESTAMPTZ, deleted_by TEXT, status TEXT NOT NULL);"
            "CREATE TABLE IF NOT EXISTS page_revisions (id BIGSERIAL PRIMARY KEY, page_id BIGINT NOT NULL, rev_no INTEGER NOT NULL, title TEXT NOT NULL, body_md TEXT NOT NULL, edited_by TEXT, edit_summary TEXT, created_at TIMESTAMPTZ DEFAULT now());"
            "CREATE TABLE IF NOT EXISTS media_assets (id BIGSERIAL PRIMARY KEY, page_id BIGINT, media_type TEXT NOT NULL, title TEXT, original_filename TEXT NOT NULL, stored_filename TEXT NOT NULL, mime_type TEXT NOT NULL, storage_path TEXT NOT NULL, public_url TEXT NOT NULL, created_by TEXT, created_at TIMESTAMPTZ DEFAULT now(), deleted_at TIMESTAMPTZ, deleted_by TEXT);"
            "CREATE TABLE IF NOT EXISTS roles (name TEXT PRIMARY KEY, description TEXT NOT NULL);"
            "CREATE TABLE IF NOT EXISTS users (id BIGSERIAL PRIMARY KEY, username TEXT NOT NULL, display_name TEXT NOT NULL, password_hash TEXT NOT NULL, role_name TEXT NOT NULL, enabled BOOL NOT NULL, created_at TIMESTAMPTZ DEFAULT now(), updated_at TIMESTAMPTZ DEFAULT now(), last_login_at TIMESTAMPTZ);"
            "CREATE TABLE IF NOT EXISTS user_sessions (id BIGSERIAL PRIMARY KEY, user_id BIGINT NOT NULL, session_token TEXT NOT NULL, csrf_token TEXT NOT NULL, created_at TIMESTAMPTZ DEFAULT now(), last_seen_at TIMESTAMPTZ DEFAULT now(), expires_at TIMESTAMPTZ NOT NULL, revoked_at TIMESTAMPTZ);"
            "CREATE TABLE IF NOT EXISTS audit_logs (id BIGSERIAL PRIMARY KEY, actor_user_id BIGINT, action TEXT NOT NULL, target_type TEXT NOT NULL, target_id TEXT, meta_json TEXT NOT NULL, created_at TIMESTAMPTZ DEFAULT now());"
            "CREATE TABLE IF NOT EXISTS page_tags (id BIGSERIAL PRIMARY KEY, page_id BIGINT NOT NULL, tag TEXT NOT NULL, created_at TIMESTAMPTZ DEFAULT now());"
            "CREATE TABLE IF NOT EXISTS backup_runs (id BIGSERIAL PRIMARY KEY, triggered_by_user_id BIGINT, mode TEXT NOT NULL, status TEXT NOT NULL, backup_dir TEXT, keep_count INTEGER, dry_run BOOL NOT NULL, sql_path TEXT, media_path TEXT, message TEXT, created_at TIMESTAMPTZ DEFAULT now(), completed_at TIMESTAMPTZ);"))
    (db-exec! catalog sql))
  (if (null (find-first-row (db-table-rows "roles") (lambda (row) (string= (db-row-value row "name") "viewer"))))
      (db-exec! catalog "INSERT INTO roles (name, description) VALUES ('viewer', 'Read-only wiki access');")
      nil)
  (if (null (find-first-row (db-table-rows "roles") (lambda (row) (string= (db-row-value row "name") "editor"))))
      (db-exec! catalog "INSERT INTO roles (name, description) VALUES ('editor', 'Can create and edit wiki content');")
      nil)
  (if (null (find-first-row (db-table-rows "roles") (lambda (row) (string= (db-row-value row "name") "admin"))))
      (db-exec! catalog "INSERT INTO roles (name, description) VALUES ('admin', 'Can manage wiki administration features');")
      nil)
  (if (null (find-user-row "admin"))
      (db-exec! catalog
                "INSERT INTO users (username, display_name, password_hash, role_name, enabled) VALUES ('admin', 'Administrator', 'admin', 'admin', TRUE);")
      nil)
  (if (null (find-page-row-any "home"))
      (progn
        (db-exec! catalog
                  "INSERT INTO pages (slug, title, body_md, last_edited_by, current_rev_no, status) VALUES ('home', 'Home', '# Welcome\\n\\nThis is the first wiki page.', 'system', 1, 'published');")
        (let ((page-id (page-id-by-slug "home")))
          (if (not (null page-id))
              (db-exec! catalog
                        (string-append
                         "INSERT INTO page_revisions (page_id, rev_no, title, body_md, edited_by, edit_summary) VALUES ("
                         (format nil "~A" page-id)
                         ", 1, 'Home', '# Welcome\\n\\nThis is the first wiki page.', 'system', 'Initial page');"))
              nil)))
      nil))

(defun db-open ()
  (configure-dbms-root)
  (debug-stderr "db-open begin")
  (handler-case
    (let ((catalog '()))
      (debug-stderr "dbms-engine-init begin")
      (setq catalog (dbms-engine-init))
      (debug-stderr "dbms-engine-init ok")
      (debug-stderr "ensure-wiki-dbms-schema begin")
      (ensure-wiki-dbms-schema catalog)
      (debug-stderr "ensure-wiki-dbms-schema ok")
      (debug-stderr "db-open ok")
      t)
    (error (e)
      (debug-stderr (format nil "db-open error ~A" e))
      t)))

(defglobal *markdown-temp-counter* 0)

(defun md2html-bin ()
  (wiki-config-get "md2html_bin" (wiki-path "/examples/md2html/md2html")))

(defun next-temp-base ()
  (setq *markdown-temp-counter* (+ *markdown-temp-counter* 1))
  (string-append "/tmp/isl-wiki-"
                 (format nil "~A" (get-universal-time))
                 "-"
                 (format nil "~A" *markdown-temp-counter*)))

(defun write-file-text (path text)
  (os-write-file-text path (safe-text text)))

(defun read-file-text (path)
  (os-read-file-text path))

(defun safe-delete-file (path)
  (os-safe-delete-file path))

(defun read-request-body ()
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
      acc)))

(defun capture-request-body-to-file (path)
  (if (os-cat-stdin-to-file path) 0 1))

(defun request-content-type ()
  (env-or-empty "CONTENT_TYPE"))

(defun multipart-boundary (content-type)
  (let ((marker "boundary="))
    (let ((p (string-index marker content-type)))
      (if (null p)
          ""
          (trim-ws (substring content-type (+ p (length marker)) (length content-type)))))))

(defun multipart-helper-path ()
  (wiki-path "/examples/wiki/app/multipart_extract.lsp"))

(defun multipart-field-path (dir field-name)
  (string-append dir "/field-" field-name ".txt"))

(defun read-optional-file-text (path)
  (if (null (probe-file path))
      ""
      (read-file-text path)))

(defun hex-digit-value (ch)
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

(defglobal *ascii-visible*
  " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~")

(defun ascii-byte (ch)
  (cond
   ((string= ch "\t") 9)
   ((string= ch "\n") 10)
   ((string= ch "\r") 13)
   (t
    (let ((p (string-index ch *ascii-visible*)))
      (if (null p)
          (error "non-ascii character must be percent-encoded" ch)
          (+ p 32))))))

(defun split-on (s delim)
  (if (= (length s) 0)
      '()
      (let ((p (string-index delim s)))
        (if (null p)
            (list s)
            (cons (substring s 0 p)
                  (split-on (substring s (+ p 1)) delim))))))

(defun split-once (s delim)
  (let ((p (string-index delim s)))
    (if (null p)
        (list s "")
        (list (substring s 0 p)
              (substring s (+ p 1))))))

(defun trim-ws (s)
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

(defun index-of-from (s needle start)
  (if (>= start (length s))
      '()
      (let ((p (string-index needle (substring s start))))
        (if (null p) '() (+ start p)))))

(defun wiki-link->markdown (inner)
  (let* ((parts (split-once inner "|"))
         (slug (trim-ws (first parts)))
         (label-raw (if (null (second parts)) "" (trim-ws (second parts))))
         (label (if (blank-text-p label-raw) slug label-raw)))
    (if (slug-safe-p slug)
        (string-append "[" label "](" (app-base) "/" slug ")")
        (string-append "[[" inner "]]"))))

(defun expand-wiki-links (body-md)
  (let ((n (length body-md))
        (i 0)
        (out ""))
    (while (< i n)
      (let ((open (index-of-from body-md "[[" i)))
        (if (null open)
            (progn
              (setq out (string-append out (substring body-md i n)))
              (setq i n))
            (let ((close (index-of-from body-md "]]" (+ open 2))))
              (if (null close)
                  (progn
                    (setq out (string-append out (substring body-md i n)))
                    (setq i n))
                  (progn
                    (setq out (string-append out (substring body-md i open)))
                    (setq out (string-append out (wiki-link->markdown (substring body-md (+ open 2) close))))
                    (setq i (+ close 2))))))))
    out))

(defun url-encode-lite (s)
  (let ((i 0)
        (out ""))
    (while (< i (length s))
      (let ((ch (substring s i (+ i 1))))
        (setq out
              (string-append
               out
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
                (t ch)))))
      (setq i (+ i 1)))
    out))

(defun js-string-escape (s)
  (let ((i 0)
        (out ""))
    (while (< i (length s))
      (let ((ch (substring s i (+ i 1))))
        (setq out
              (string-append
               out
               (cond
                ((string= ch "\\") "\\\\")
                ((string= ch "\"") "\\\"")
                ((string= ch "\n") "\\n")
                ((string= ch "\r") "\\r")
                ((string= ch "\t") "\\t")
                (t ch)))))
      (setq i (+ i 1)))
    out))

(defun utf8-cont-byte-p (b)
  (and (>= b 128) (<= b 191)))

(defun rest2 (xs)
  (cdr (cdr xs)))

(defun rest3 (xs)
  (cdr (rest2 xs)))

(defun decode-utf8-bytes (bytes)
  (let ((rest bytes)
        (out ""))
    (while (not (null rest))
      (let ((b1 (first rest)))
         (cond
         ((< b1 128)
          (setq out (string-append out (string (integer->char b1))))
          (setq rest (cdr rest)))
         ((and (>= b1 194) (<= b1 223))
          (if (null (cdr rest))
              (error "invalid UTF-8 sequence")
              (let ((b2 (second rest)))
                (if (not (utf8-cont-byte-p b2))
                    (error "invalid UTF-8 continuation byte" b2)
                    (progn
                      (setq out (string-append out (string (integer->char (+ (* (- b1 192) 64) (- b2 128))))))
                      (setq rest (rest2 rest)))))))
         ((and (>= b1 224) (<= b1 239))
          (if (or (null (cdr rest)) (null (rest2 rest)))
              (error "invalid UTF-8 sequence")
              (let ((b2 (second rest))
                    (b3 (third rest)))
                (if (or (not (utf8-cont-byte-p b2))
                        (not (utf8-cont-byte-p b3)))
                    (error "invalid UTF-8 continuation byte")
                    (if (or (and (= b1 224) (< b2 160))
                            (and (= b1 237) (> b2 159)))
                        (error "invalid UTF-8 code point")
                        (progn
                          (setq out (string-append out (string (integer->char (+ (* (- b1 224) 4096)
                                                                                  (* (- b2 128) 64)
                                                                                  (- b3 128))))))
                          (setq rest (rest3 rest))))))))
         ((and (>= b1 240) (<= b1 244))
          (if (or (null (cdr rest)) (null (rest2 rest)) (null (rest3 rest)))
              (error "invalid UTF-8 sequence")
              (let ((b2 (second rest))
                    (b3 (third rest))
                    (b4 (fourth rest)))
                (if (or (not (utf8-cont-byte-p b2))
                        (not (utf8-cont-byte-p b3))
                        (not (utf8-cont-byte-p b4)))
                    (error "invalid UTF-8 continuation byte")
                    (if (or (and (= b1 240) (< b2 144))
                            (and (= b1 244) (> b2 143)))
                        (error "invalid UTF-8 code point")
                        (progn
                          (setq out (string-append out (string (integer->char (+ (* (- b1 240) 262144)
                                                                                  (* (- b2 128) 4096)
                                                                                  (* (- b3 128) 64)
                                                                                  (- b4 128))))))
                          (setq rest (cdr (rest3 rest)))))))))
         (t
          (error "invalid UTF-8 leading byte" b1)))))
    out))

(defun url-decode (s)
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
                (if (< (+ i 2) n)
                    (let* ((h1 (substring s (+ i 1) (+ i 2)))
                           (h2 (substring s (+ i 2) (+ i 3)))
                           (v1 (hex-digit-value h1))
                           (v2 (hex-digit-value h2)))
                      (if (or (< v1 0) (< v2 0))
                          (error "invalid percent-encoding" s)
                          (progn
                            (setq bytes (append bytes (list (+ (* v1 16) v2))))
                            (setq i (+ i 3)))))
                    (error "truncated percent-encoding" s))
                (progn
                  (setq bytes (append bytes (list (ascii-byte ch))))
                  (setq i (+ i 1)))))))
    (decode-utf8-bytes bytes)))

(defun parse-form-field (raw-body field-name)
  (let ((pairs (split-on raw-body "&")))
    (let ((rest pairs)
          (value ""))
      (while (and (string= value "") (not (null rest)))
        (let* ((kv (split-once (first rest) "="))
               (k (url-decode (first kv)))
               (v (url-decode (second kv))))
          (if (string= k field-name)
              (setq value v)
              nil))
        (setq rest (cdr rest)))
      value)))

(defun query-param (name)
  (parse-form-field (env-or-empty "QUERY_STRING") name))

(defun blank-text-p (s)
  (let ((text (safe-text s))
        (i 0)
        (all-space t))
    (while (if all-space (< i (length text)) nil)
      (if (not (null (string-index (substring text i (+ i 1)) " \t\r\n")))
          nil
          (setq all-space nil))
      (setq i (+ i 1)))
    all-space))

(defun markdown->html (body-md)
  (let* ((normalized-md (expand-wiki-links body-md))
         (base (next-temp-base))
         (md-path (string-append base ".md"))
         (html-path (string-append base ".html"))
         (cmd (string-append "ISL_ROOT=" (shell-quote (wiki-root)) " "
                             (shell-quote (md2html-bin))
                             " -o " (shell-quote html-path)
                             " " (shell-quote md-path))))
    (write-file-text md-path normalized-md)
    (let ((status (system cmd)))
      (if (= status 0)
          (let ((html (read-file-text html-path)))
            (safe-delete-file md-path)
            (safe-delete-file html-path)
            html)
          (progn
            (safe-delete-file md-path)
            (safe-delete-file html-path)
            (string-append "<pre>" (html-escape normalized-md) "</pre>"))))))

(defun fetch-pages (db)
  (mapcar
   (lambda (row)
     (list (db-row-value row "slug")
           (db-row-value row "title")
           (db-row-value row "updated_at")
           (db-row-value row "status")))
   (sort-rows-by
    (filter-rows
     (db-table-rows "pages")
     (lambda (row)
       (and (string= (db-row-value row "deleted_at") "")
            (visible-page-p (db-row-value row "status")))))
    (lambda (row) (db-row-value row "slug"))
    nil)))

(defun fetch-pages-by-query (db q)
  (fetch-pages-by-query-filtered db q "" ""))

(defun fetch-pages-by-query-filtered (db q tag status)
  (let ((matched '())
        (needle (ascii-downcase-string q)))
    (dolist (row (db-table-rows "pages"))
      (let ((page-id (db-row-nullable row "id"))
            (page-status (db-row-value row "status")))
        (if (and (string= (db-row-value row "deleted_at") "")
                 (visible-page-p page-status)
                 (or (blank-text-p status) (string= page-status status))
                 (or (blank-text-p tag)
                     (not (null (find-first-row
                                 (db-table-rows "page_tags")
                                 (lambda (tag-row)
                                   (and (= (db-row-nullable tag-row "page_id") page-id)
                                        (string= (db-row-value tag-row "tag") tag))))))
                 )
                 (or (blank-text-p needle)
                     (string-contains-ci-p needle (db-row-value row "title"))
                     (string-contains-ci-p needle (db-row-value row "body_md"))))
            (setq matched (cons row matched))
            nil)))
    (setq matched
          (sort-rows-by matched
                        (lambda (row)
                          (if (string-contains-ci-p needle (db-row-value row "title")) 0 1))
                        nil))
    (setq matched (sort-rows-by matched (lambda (row) (db-row-value row "updated_at")) t))
    (mapcar
     (lambda (row)
       (list (db-row-value row "slug")
             (db-row-value row "title")
             (db-row-value row "updated_at")
             ""
             (fetch-page-tags-text db (db-row-value row "slug"))
             (db-row-value row "status")))
     matched)))

(defun fetch-search-tags (db)
  (let ((seen '())
        (out '()))
    (dolist (row (db-table-rows "page_tags"))
      (let ((tag (db-row-value row "tag")))
        (if (null (find-first-row seen (lambda (v) (string= v tag))))
            (progn
              (setq seen (cons tag seen))
              (setq out (cons (list tag) out)))
            nil)))
    (sort-rows-by out (lambda (row) (first row)) nil)))

(defun fetch-page (db slug)
  (let ((row (find-page-row-visible slug)))
    (if (null row)
        '()
        (list (db-row-value row "slug")
              (db-row-value row "title")
              (db-row-value row "body_md")
              (db-row-value row "current_rev_no")
              (db-row-value row "status")))))

(defun fetch-page-any (db slug)
  (let ((row (find-page-row-any slug)))
    (if (null row)
        '()
        (list (db-row-value row "slug")
              (db-row-value row "title")
              (db-row-value row "body_md")
              (db-row-value row "current_rev_no")
              (db-row-value row "deleted_at")
              (db-row-value row "deleted_by")
              (db-row-value row "status")))))

(defun fetch-page-tags (db slug)
  (let ((page-id (page-id-by-slug slug))
        (out '()))
    (if (null page-id)
        '()
        (progn
          (dolist (row (db-table-rows "page_tags"))
            (if (= (db-row-nullable row "page_id") page-id)
                (setq out (cons (list (db-row-value row "tag")) out))
                nil))
          (sort-rows-by out (lambda (row) (first row)) nil)))))

(defun fetch-page-tags-text (db slug)
  (let ((rows (fetch-page-tags db slug))
        (out "")
        (first-tag t))
    (dolist (row rows)
      (if first-tag
          (progn
            (setq out (first row))
            (setq first-tag nil))
          (setq out (string-append out ", " (first row)))))
    out))

(defun fetch-page-id-any (db slug)
  (let ((row (find-page-row-any slug)))
    (if (null row) "" (format nil "~A" (db-row-nullable row "id")))))

(defun fetch-page-updated-at (db slug)
  (let ((row (find-page-row-any slug)))
    (if (null row) "" (db-row-value row "updated_at"))))

(defun fetch-latest-edit-summary (db slug)
  (let ((page-id (page-id-by-slug slug))
        (best '()))
    (if (null page-id)
        ""
        (progn
          (dolist (row (db-table-rows "page_revisions"))
            (if (and (= (db-row-nullable row "page_id") page-id)
                     (or (null best)
                         (> (db-row-nullable row "rev_no") (db-row-nullable best "rev_no"))))
                (setq best row)
                nil))
          (if (null best) "" (db-row-value best "edit_summary"))))))

(defun page-current-rev-no (page-row)
  (if (null page-row) "" (fourth page-row)))

(defun page-status (page-row)
  (if (null page-row) "" (fifth page-row)))

(defun split-tags-input (raw)
  (let ((parts (split-on raw ","))
        (out '()))
    (dolist (part parts)
      (let ((tag (ascii-downcase-string (trim-ws part))))
        (if (blank-text-p tag)
            nil
            (let ((exists nil))
              (dolist (seen out)
                (if (string= seen tag)
                    (setq exists t)
                    nil))
              (if exists
                  nil
                  (setq out (append out (list tag))))))))
    out))

(defun sync-page-tags (db slug tags)
  (let ((page-id (fetch-page-id-any db slug)))
    (if (blank-text-p page-id)
        nil
        (progn
          (db-exec! db (string-append "DELETE FROM page_tags WHERE page_id=" page-id ";"))
          (dolist (tag tags)
            (db-exec!
             db
             (string-append
              "INSERT INTO page_tags (page_id, tag) VALUES ("
              page-id ", '" (sql-escape tag) "');")))))))

(defun fetch-page-history (db slug)
  (let ((page-id (page-id-by-slug slug))
        (out '()))
    (if (null page-id)
        '()
        (progn
          (dolist (row (db-table-rows "page_revisions"))
            (if (= (db-row-nullable row "page_id") page-id)
                (setq out (cons (list (db-row-value row "rev_no")
                                      (db-row-value row "edited_by")
                                      (db-row-value row "edit_summary")
                                      (db-row-value row "created_at"))
                                out))
                nil))
          (sort-rows-by out (lambda (row) (first row)) t)))))

(defun fetch-page-revision (db slug rev-no)
  (let ((n (parse-int-safe rev-no)))
    (if (null n)
        '()
        (let ((page-id (page-id-by-slug slug))
              (row '()))
          (if (null page-id)
              '()
              (progn
                (setq row
                      (find-first-row
                       (db-table-rows "page_revisions")
                       (lambda (rev-row)
                         (and (= (db-row-nullable rev-row "page_id") page-id)
                              (= (db-row-nullable rev-row "rev_no") n)))))
                (if (null row)
                    '()
                    (list (db-row-value row "rev_no")
                          (db-row-value row "title")
                          (db-row-value row "body_md")
                          (db-row-value row "edited_by")
                          (db-row-value row "edit_summary")
                          (db-row-value row "created_at")))))))))

(defun latest-revision-no (db slug)
  (let ((page (fetch-page db slug)))
    (if (null page) "" (page-current-rev-no page))))

(defun fetch-media-list (db)
  (let ((out '()))
    (dolist (row (db-table-rows "media_assets"))
      (if (string= (db-row-value row "deleted_at") "")
          (let ((page-row (table-row-by-id "pages" (db-row-nullable row "page_id"))))
            (setq out (cons (list (db-row-value row "id")
                                  (db-row-value row "media_type")
                                  (db-row-value row "title")
                                  (db-row-value row "stored_filename")
                                  (db-row-value row "mime_type")
                                  (if (null page-row) "" (db-row-value page-row "slug")))
                            out)))
          nil))
    (sort-rows-by out (lambda (row) (first row)) t)))

(defun fetch-media-for-page (db slug)
  (let ((page-id (page-id-by-slug slug))
        (out '()))
    (if (null page-id)
        '()
        (progn
          (dolist (row (db-table-rows "media_assets"))
            (if (and (= (db-row-nullable row "page_id") page-id)
                     (string= (db-row-value row "deleted_at") ""))
                (setq out (cons (list (db-row-value row "media_type")
                                      (db-row-value row "title")
                                      (db-row-value row "stored_filename")
                                      (db-row-value row "mime_type"))
                                out))
                nil))
          (sort-rows-by out (lambda (row) (third row)) t)))))

(defun fetch-media-file-meta (db stored-filename)
  (let ((row
         (find-first-row
          (db-table-rows "media_assets")
          (lambda (media-row)
            (and (string= (db-row-value media-row "stored_filename") stored-filename)
                 (string= (db-row-value media-row "deleted_at") ""))))))
    (if (null row)
        '()
        (list (db-row-value row "storage_path")
              (db-row-value row "mime_type")
              (db-row-value row "original_filename")))))

(defun fetch-media-by-id (db media-id)
  (let* ((mid (parse-int-safe media-id))
         (row (if (null mid)
                  '()
                  (find-first-row
                   (db-table-rows "media_assets")
                   (lambda (media-row)
                     (and (= (db-row-nullable media-row "id") mid)
                          (string= (db-row-value media-row "deleted_at") "")))))))
    (if (null mid)
        '()
        (if (null row)
            '()
            (let ((page-row (table-row-by-id "pages" (db-row-nullable row "page_id"))))
              (list (db-row-value row "id")
                    (db-row-value row "media_type")
                    (db-row-value row "title")
                    (db-row-value row "stored_filename")
                    (db-row-value row "mime_type")
                    (db-row-value row "storage_path")
                    (if (null page-row) "" (db-row-value page-row "slug"))))))))

(defun fetch-media-by-id-any (db media-id)
  (let* ((mid (parse-int-safe media-id))
         (row (if (null mid)
                  '()
                  (find-first-row
                   (db-table-rows "media_assets")
                   (lambda (media-row)
                     (= (db-row-nullable media-row "id") mid))))))
    (if (null mid)
        '()
        (if (null row)
            '()
            (let ((page-row (table-row-by-id "pages" (db-row-nullable row "page_id"))))
              (list (db-row-value row "id")
                    (db-row-value row "media_type")
                    (db-row-value row "title")
                    (db-row-value row "stored_filename")
                    (db-row-value row "mime_type")
                    (db-row-value row "storage_path")
                    (if (null page-row) "" (db-row-value page-row "slug"))
                    (db-row-value row "deleted_at")
                    (db-row-value row "deleted_by")))))))

(defun fetch-deleted-pages (db)
  (let ((out '()))
    (dolist (row (db-table-rows "pages"))
      (if (not (string= (db-row-value row "deleted_at") ""))
          (setq out (cons (list (db-row-value row "slug")
                                (db-row-value row "title")
                                (db-row-value row "deleted_at")
                                (db-row-value row "deleted_by"))
                          out))
          nil))
    (sort-rows-by out (lambda (row) (third row)) t)))

(defun fetch-deleted-media (db)
  (let ((out '()))
    (dolist (row (db-table-rows "media_assets"))
      (if (not (string= (db-row-value row "deleted_at") ""))
          (let ((page-row (table-row-by-id "pages" (db-row-nullable row "page_id"))))
            (setq out (cons (list (db-row-value row "id")
                                  (db-row-value row "media_type")
                                  (db-row-value row "title")
                                  (db-row-value row "stored_filename")
                                  (db-row-value row "mime_type")
                                  (if (null page-row) "" (db-row-value page-row "slug"))
                                  (db-row-value row "deleted_at")
                                  (db-row-value row "deleted_by"))
                            out)))
          nil))
    (sort-rows-by out (lambda (row) (seventh row)) t)))

(defun fetch-audit-logs (db)
  (let ((out '()))
    (dolist (row (db-table-rows "audit_logs"))
      (let ((user-row (table-row-by-id "users" (db-row-nullable row "actor_user_id"))))
        (setq out (cons (list (db-row-value row "id")
                              (db-row-value row "created_at")
                              (if (null user-row) "[deleted-user]" (db-row-value user-row "username"))
                              (db-row-value row "action")
                              (db-row-value row "target_type")
                              (db-row-value row "target_id")
                              (db-row-value row "meta_json"))
                        out))))
    (sort-rows-by out (lambda (row) (second row)) t)))

(defun fetch-backup-runs (db)
  (let ((out '()))
    (dolist (row (db-table-rows "backup_runs"))
      (let ((user-row (table-row-by-id "users" (db-row-nullable row "triggered_by_user_id"))))
        (setq out (cons (list (db-row-value row "id")
                              (db-row-value row "mode")
                              (db-row-value row "status")
                              (db-row-value row "backup_dir")
                              (db-row-value row "keep_count")
                              (if (eq (db-row-nullable row "dry_run") t) "true" "false")
                              (db-row-value row "sql_path")
                              (db-row-value row "media_path")
                              (db-row-value row "message")
                              (db-row-value row "created_at")
                              (db-row-value row "completed_at")
                              (if (null user-row) "[system]" (db-row-value user-row "username")))
                        out))))
    (sort-rows-by out (lambda (row) (tenth row)) t)))

(defun fetch-backup-stats (db)
  (let ((pages-active 0)
        (pages-deleted 0)
        (media-active 0)
        (media-deleted 0)
        (backup-runs 0)
        (last-backup ""))
    (dolist (row (db-table-rows "pages"))
      (if (string= (db-row-value row "deleted_at") "")
          (setq pages-active (+ pages-active 1))
          (setq pages-deleted (+ pages-deleted 1))))
    (dolist (row (db-table-rows "media_assets"))
      (if (string= (db-row-value row "deleted_at") "")
          (setq media-active (+ media-active 1))
          (setq media-deleted (+ media-deleted 1))))
    (dolist (row (db-table-rows "backup_runs"))
      (setq backup-runs (+ backup-runs 1))
      (if (and (string= (db-row-value row "mode") "backup")
               (or (string= (db-row-value row "status") "ok")
                   (string= (db-row-value row "status") "dry-run"))
               (or (string= last-backup "")
                   (value> (db-row-value row "created_at") last-backup)))
          (setq last-backup (db-row-value row "created_at"))
          nil))
    (list (format nil "~A" pages-active)
          (format nil "~A" pages-deleted)
          (format nil "~A" media-active)
          (format nil "~A" media-deleted)
          (format nil "~A" backup-runs)
          last-backup)))

(defun fetch-user-for-login (db username password)
  (let ((row (find-user-row username)))
    (if (or (null row)
            (not (eq (db-row-nullable row "enabled") t))
            (not (string= (db-row-value row "password_hash") password)))
        '()
        (list (db-row-value row "username")
              (db-row-value row "display_name")
              (db-row-value row "role_name")))))

(defun fetch-user-by-session-token (db token)
  (let ((session-row
         (find-first-row
          (db-table-rows "user_sessions")
          (lambda (row)
            (and (string= (db-row-value row "session_token") token)
                 (string= (db-row-value row "revoked_at") "")
                 (> (parse-int-safe (db-row-value row "expires_at")) (get-universal-time)))))))
    (if (null session-row)
        '()
        (let ((user-row (table-row-by-id "users" (db-row-nullable session-row "user_id"))))
          (if (or (null user-row) (not (eq (db-row-nullable user-row "enabled") t)))
              '()
              (list (db-row-value user-row "username")
                    (db-row-value user-row "display_name")
                    (db-row-value user-row "role_name")
                    (db-row-value session-row "session_token")
                    (db-row-value session-row "csrf_token")))))))

(defun create-user-session (db username token csrf-token)
  (let ((user-id (user-id-by-username username))
        (days (parse-int-safe (session-duration-days))))
    (if (null user-id)
        nil
        (db-exec!
         db
         (string-append
          "INSERT INTO user_sessions (user_id, session_token, csrf_token, expires_at) VALUES ("
          (format nil "~A" user-id) ", "
          (db-text-sql token) ", "
          (db-text-sql csrf-token) ", "
          (db-text-sql (format nil "~A" (+ (get-universal-time) (* 86400 days))))
          ");")))))

(defun revoke-session-token (db token)
  (if (blank-text-p token)
      nil
      (db-exec!
       db
       (string-append
        "UPDATE user_sessions SET revoked_at = "
        (db-text-sql (db-now-text))
        " WHERE session_token=" (db-text-sql token) ";"))))

(defun touch-session-token (db token)
  (if (blank-text-p token)
      nil
      (db-exec!
       db
       (string-append
        "UPDATE user_sessions SET last_seen_at = "
        (db-text-sql (db-now-text))
        " WHERE session_token=" (db-text-sql token) ";"))))

(defun update-last-login-at (db username)
  (db-exec!
   db
   (string-append
    "UPDATE users SET last_login_at = "
    (db-text-sql (db-now-text))
    " WHERE username=" (db-text-sql username) ";")))

(defun load-current-user (db)
  (let ((token (session-cookie-value)))
    (if (blank-text-p token)
        (setq *wiki-current-user* '())
        (let ((user (fetch-user-by-session-token db token)))
          (if (null user)
              (progn
                (setq *wiki-current-user* '())
                (clear-session-cookie-header))
              (progn
                (setq *wiki-current-user* user)
                (touch-session-token db token)))))))

(defun render-csrf-hidden-input ()
  (format t "  <input type=\"hidden\" name=\"csrf_token\" value=\"~A\">~%" (html-escape (current-csrf-token))))

(defun valid-csrf-token-p (raw-body)
  (let ((token (parse-form-field raw-body "csrf_token")))
    (and (not (blank-text-p token))
         (not (blank-text-p (current-csrf-token)))
         (string= token (current-csrf-token)))))

(defun read-form-body-or-render (require-csrf)
  (let ((content-type (env-or-empty "CONTENT_TYPE")))
    (if (not (starts-with content-type "application/x-www-form-urlencoded"))
        (progn
          (render-bad-request "CONTENT_TYPE must be application/x-www-form-urlencoded")
          '())
        (let ((raw-body (read-request-body)))
          (if (and require-csrf (not (valid-csrf-token-p raw-body)))
              (progn
                (render-forbidden "invalid csrf_token")
                '())
              raw-body)))))

(defun backup-confirm-phrase ()
  "RUN BACKUP")

(defun restore-confirm-phrase ()
  "RESTORE WIKI")

(defun audit-actor-id-sql ()
  (if (blank-text-p (current-username))
      "NULL"
      (let ((user-id (user-id-by-username (current-username))))
        (if (null user-id) "NULL" (format nil "~A" user-id)))))

(defun make-audit-meta-1 (k1 v1)
  (string-append
   "{\""
   k1
   "\":\""
   (json-escape v1)
   "\"}"))

(defun make-audit-meta-2 (k1 v1 k2 v2)
  (string-append
   "{\""
   k1
   "\":\""
   (json-escape v1)
   "\",\""
   k2
   "\":\""
   (json-escape v2)
   "\"}"))

(defun make-audit-meta-3 (k1 v1 k2 v2 k3 v3)
  (string-append
   "{\""
   k1
   "\":\""
   (json-escape v1)
   "\",\""
   k2
   "\":\""
   (json-escape v2)
   "\",\""
   k3
   "\":\""
   (json-escape v3)
   "\"}"))

(defun make-audit-meta-4 (k1 v1 k2 v2 k3 v3 k4 v4)
  (string-append
   "{\""
   k1
   "\":\""
   (json-escape v1)
   "\",\""
   k2
   "\":\""
   (json-escape v2)
   "\",\""
   k3
   "\":\""
   (json-escape v3)
   "\",\""
   k4
   "\":\""
   (json-escape v4)
   "\"}"))

(defun write-audit-log (db action target-type target-id meta-json)
  (db-exec!
   db
   (string-append
    "INSERT INTO audit_logs (actor_user_id, action, target_type, target_id, meta_json) VALUES ("
    (audit-actor-id-sql) ", '"
    (sql-escape action) "', '"
    (sql-escape target-type) "', "
    (if (blank-text-p target-id)
        "NULL"
        (string-append "'" (sql-escape target-id) "'"))
    ", '"
    (sql-escape meta-json)
    "');")))

(defun print-extra-headers ()
  (dolist (header (wiki-reverse *response-extra-headers*))
    (format t "~A: ~A~%" (first header) (second header)))
  (clear-response-extra-headers))

(defun print-headers-ok ()
  (format t "Content-Type: text/html; charset=UTF-8~%")
  (print-extra-headers)
  (format t "~%"))

(defun print-headers-404 ()
  (format t "Status: 404 Not Found~%")
  (format t "Content-Type: text/html; charset=UTF-8~%")
  (print-extra-headers)
  (format t "~%"))

(defun print-headers-400 ()
  (format t "Status: 400 Bad Request~%")
  (format t "Content-Type: text/html; charset=UTF-8~%")
  (print-extra-headers)
  (format t "~%"))

(defun print-headers-403 ()
  (format t "Status: 403 Forbidden~%")
  (format t "Content-Type: text/html; charset=UTF-8~%")
  (print-extra-headers)
  (format t "~%"))

(defun print-headers-409 ()
  (format t "Status: 409 Conflict~%")
  (format t "Content-Type: text/html; charset=UTF-8~%")
  (print-extra-headers)
  (format t "~%"))

(defun print-headers-500 ()
  (format t "Status: 500 Internal Server Error~%")
  (format t "Content-Type: text/html; charset=UTF-8~%")
  (print-extra-headers)
  (format t "~%"))

(defun print-headers-json-ok ()
  (format t "Content-Type: application/json; charset=UTF-8~%")
  (print-extra-headers)
  (format t "~%"))

(defun print-headers-301 (location)
  (format t "Status: 301 Moved Permanently~%")
  (format t "Location: ~A~%" location)
  (format t "Content-Type: text/html; charset=UTF-8~%")
  (print-extra-headers)
  (format t "~%"))

(defun print-headers-303 (location)
  (format t "Status: 303 See Other~%")
  (format t "Location: ~A~%" location)
  (format t "Content-Type: text/html; charset=UTF-8~%")
  (print-extra-headers)
  (format t "~%"))

(defun print-layout-head (title)
  (format t "<!doctype html>~%")
  (format t "<html lang=\"ja\">~%")
  (format t "<head>~%")
  (format t "  <meta charset=\"UTF-8\">~%")
  (format t "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">~%")
  (format t "  <title>~A</title>~%" (html-escape title))
  (format t "  <style>body{font-family:system-ui,-apple-system,sans-serif;margin:2rem;line-height:1.6}main{max-width:840px}textarea{width:100%;min-height:18rem;font-family:ui-monospace,SFMono-Regular,monospace}code{background:#f4f4f4;padding:.1rem .3rem;border-radius:4px}pre{background:#f7f7f7;padding:1rem;border-radius:6px;overflow:auto}a{color:#0b5394}.wiki-body{border:1px solid #ddd;border-radius:8px;padding:1rem;background:#fff}.wiki-body :first-child{margin-top:0}.wiki-body :last-child{margin-bottom:0}</style>~%")
  (format t "</head>~%")
  (format t "<body><main>~%")
  (format t "<p><a href=\"~A\">Wiki Index</a> | <a href=\"~A/media\">Media</a>"
          (app-base) (app-base))
  (if (editor-or-admin-p)
      (format t " | <a href=\"~A/new\">New Page</a>" (app-base))
      nil)
  (if (admin-p)
      (format t " | <a href=\"~A/admin\">Admin</a>" (app-base))
      nil)
  (if (logged-in-p)
      (format t " | signed in as <strong>~A</strong> (<code>~A</code>) | <a href=\"~A/logout\">Logout</a></p>~%"
              (html-escape (current-display-name))
              (html-escape (current-role))
              (app-base))
      (format t " | <a href=\"~A/login\">Login</a></p>~%" (app-base)))
  (format t "<form method=\"get\" action=\"~A/search\" style=\"margin:0 0 1rem 0\">~%" (app-base))
  (format t "<input type=\"text\" name=\"q\" value=\"~A\" placeholder=\"Search wiki text\" style=\"width:70%;max-width:28rem\">~%"
          (html-escape (query-param "q")))
  (format t "<button type=\"submit\">Search</button>~%")
  (format t "</form>~%"))

(defun print-layout-foot ()
  (format t "</main></body>~%</html>~%"))

(defun render-not-found ()
  (print-headers-404)
  (print-layout-head "Not Found")
  (format t "<h1>404 Not Found</h1>~%")
  (format t "<p>指定されたページは見つかりませんでした。</p>~%")
  (print-layout-foot))

(defun render-bad-request (message)
  (print-headers-400)
  (print-layout-head "Bad Request")
  (format t "<h1>400 Bad Request</h1>~%")
  (format t "<p>リクエスト内容が不正です。</p>~%")
  (format t "<pre>~A</pre>~%" (html-escape message))
  (print-layout-foot))

(defun render-forbidden (message)
  (print-headers-403)
  (print-layout-head "Forbidden")
  (format t "<h1>403 Forbidden</h1>~%")
  (format t "<p>この操作を実行する権限がありません。</p>~%")
  (format t "<pre>~A</pre>~%" (html-escape message))
  (print-layout-foot))

(defun render-error (message)
  (print-headers-500)
  (print-layout-head "Internal Server Error")
  (format t "<h1>500 Internal Server Error</h1>~%")
  (format t "<p>アプリケーション内部でエラーが発生しました。</p>~%")
  (format t "<p>request id: <code>~A</code></p>~%" (html-escape (request-id)))
  (format t "<p>詳細はサーバーログを確認してください。</p>~%")
  (print-layout-foot))

(defun render-edit-conflict (db slug submitted-title submitted-body-md submitted-summary expected-rev actual-rev)
  (let ((page (fetch-page db slug)))
    (if (null page)
        (render-not-found)
        (let ((current-title (second page))
              (current-body-md (third page)))
          (print-headers-409)
          (print-layout-head "Edit Conflict")
          (format t "<h1>Edit Conflict</h1>~%")
          (format t "<p>このページは別の編集で更新されました。保存は行っていません。</p>~%")
          (format t "<p><small>your base rev: <code>~A</code> / current rev: <code>~A</code></small></p>~%"
                  (html-escape expected-rev)
                  (html-escape actual-rev))
          (format t "<p><a href=\"~A/~A/edit\">Reload editor</a> | <a href=\"~A/~A\">View page</a></p>~%"
                  (app-base) (html-escape slug) (app-base) (html-escape slug))
          (format t "<h2>Current Page</h2>~%")
          (format t "<p><strong>Title</strong></p><pre>~A</pre>~%" (html-escape current-title))
          (format t "<p><strong>Body</strong></p><pre>~A</pre>~%" (html-escape current-body-md))
          (format t "<h2>Your Unsaved Draft</h2>~%")
          (format t "<p><strong>Title</strong></p><pre>~A</pre>~%" (html-escape submitted-title))
          (format t "<p><strong>Body</strong></p><pre>~A</pre>~%" (html-escape submitted-body-md))
          (if (blank-text-p submitted-summary)
              nil
              (format t "<p><strong>Edit Summary</strong></p><pre>~A</pre>~%" (html-escape submitted-summary)))
          (print-layout-foot)))))

(defun split-lines-keep-empty (text)
  (let ((s (safe-text text))
        (i 0)
        (start 0)
        (out '()))
    (while (< i (length s))
      (if (string= (substring s i (+ i 1)) "\n")
          (progn
            (setq out (cons (substring s start i) out))
            (setq start (+ i 1)))
          nil)
      (setq i (+ i 1)))
    (wiki-reverse (cons (substring s start (length s)) out))))

(defun list-nth-or-empty (xs idx)
  (let ((cur xs)
        (i 0))
    (while (and (< i idx) (not (null cur)))
      (setq cur (cdr cur))
      (setq i (+ i 1)))
    (if (null cur) "" (car cur))))

(defun max2 (a b)
  (if (> a b) a b))

(defun render-diff-table (left-text right-text)
  (let* ((left-lines (split-lines-keep-empty left-text))
         (right-lines (split-lines-keep-empty right-text))
         (n (max2 (length left-lines) (length right-lines)))
         (i 0))
    (format t "<style>.diff-table{width:100%;border-collapse:collapse}.diff-table th,.diff-table td{border-bottom:1px solid #eee;padding:.35rem;vertical-align:top}.diff-same{background:#fff}.diff-change{background:#fff6d6}.diff-add{background:#e8f7e8}.diff-del{background:#fdeaea}.diff-code{white-space:pre-wrap;font-family:ui-monospace,SFMono-Regular,monospace}</style>~%")
    (format t "<table class=\"diff-table\">~%")
    (format t "<thead><tr><th>Line</th><th>Left</th><th>Right</th></tr></thead><tbody>~%")
    (while (< i n)
      (let* ((line-no (format nil "~A" (+ i 1)))
             (left-line (list-nth-or-empty left-lines i))
             (right-line (list-nth-or-empty right-lines i))
             (klass (cond
                     ((and (string= left-line "") (not (string= right-line ""))) "diff-add")
                     ((and (not (string= left-line "")) (string= right-line "")) "diff-del")
                     ((string= left-line right-line) "diff-same")
                     (t "diff-change"))))
        (format t "<tr class=\"~A\"><td>~A</td><td class=\"diff-code\">~A</td><td class=\"diff-code\">~A</td></tr>~%"
                klass
                (html-escape line-no)
                (html-escape left-line)
                (html-escape right-line)))
      (setq i (+ i 1)))
    (format t "</tbody></table>~%")))

(defun render-redirect (location)
  (print-headers-301 location)
  (print-layout-head "Moved")
  (format t "<h1>301 Moved Permanently</h1>~%")
  (format t "<p>Canonical URL: <a href=\"~A\">~A</a></p>~%"
          (html-escape location)
          (html-escape location))
  (print-layout-foot))

(defun render-see-other (location)
  (print-headers-303 location)
  (print-layout-head "Saved")
  (format t "<h1>Saved</h1>~%")
  (format t "<p><a href=\"~A\">continue</a></p>~%" (html-escape location))
  (print-layout-foot))

(defun redirect-to-login ()
  (render-see-other (login-target-url)))

(defun next-after-login ()
  (let ((next (query-param "next")))
    (if (blank-text-p next) (app-base) next)))

(defun render-login-form (message next-value)
  (let ((next (if (blank-text-p next-value) (next-after-login) next-value)))
    (print-headers-ok)
    (print-layout-head "Login")
    (format t "<h1>Login</h1>~%")
    (if (blank-text-p message)
        nil
        (format t "<p><strong>~A</strong></p>~%" (html-escape message)))
    (format t "<form method=\"post\" action=\"~A/login\">~%" (app-base))
    (format t "  <input type=\"hidden\" name=\"next\" value=\"~A\">~%" (html-escape next))
    (format t "  <p><label>Username<br><input type=\"text\" name=\"username\" value=\"\" style=\"width:100%\"></label></p>~%")
    (format t "  <p><label>Password<br><input type=\"password\" name=\"password\" value=\"\" style=\"width:100%\"></label></p>~%")
    (format t "  <p><button type=\"submit\">Login</button></p>~%")
    (format t "</form>~%")
    (format t "<p><small>initial admin: <code>admin</code> / <code>admin</code> (change immediately after setup)</small></p>~%")
    (print-layout-foot)))

(defun handle-login (db)
  (let ((content-type (env-or-empty "CONTENT_TYPE")))
    (if (not (starts-with content-type "application/x-www-form-urlencoded"))
        (render-bad-request "CONTENT_TYPE must be application/x-www-form-urlencoded")
        (let* ((raw-body (read-request-body))
               (username (parse-form-field raw-body "username"))
               (password (parse-form-field raw-body "password"))
               (next (parse-form-field raw-body "next")))
          (if (or (blank-text-p username) (blank-text-p password))
              (render-login-form "username and password are required" next)
              (let ((user (fetch-user-for-login db username password)))
                (if (null user)
                    (render-login-form "invalid username or password" next)
                    (let ((token (issue-session-token))
                          (csrf-token (issue-csrf-token))
                          (dest (if (blank-text-p next) (app-base) next)))
                      (create-user-session db (first user) token csrf-token)
                      (update-last-login-at db (first user))
                      (setq *wiki-current-user* (list (first user) (second user) (third user) token csrf-token))
                      (set-session-cookie-header token)
                      (render-see-other dest)))))))))

(defun handle-logout (db)
  (let ((token (current-session-token)))
    (revoke-session-token db token)
    (setq *wiki-current-user* '())
    (clear-session-cookie-header)
    (render-see-other (app-base))))

(defun route-required-role (method segments)
  (let ((n (length segments)))
    (cond
     ((= n 0) "")
     ((string= (first segments) "healthz") "")
     ((string= (first segments) "login") "")
     ((string= (first segments) "logout") "viewer")
     ((string= (first segments) "admin") "admin")
     ((and (= n 4) (string= (second segments) "revisions") (string= (fourth segments) "rollback")) "admin")
     ((and (= n 3) (string= (first segments) "media") (string= (third segments) "delete")) "admin")
     ((and (= n 2) (string= (second segments) "delete")) "editor")
     ((string= (first segments) "new") "editor")
     ((and (= n 2) (string= (second segments) "edit")) "editor")
     ((and (= n 2) (string= (first segments) "media") (string= (second segments) "new")) "editor")
     ((string= method "POST") "viewer")
     (t ""))))

(defun ensure-authorized (method segments)
  (let ((required (route-required-role method segments)))
    (if (or (null required) (string= required ""))
        t
        (if (not (logged-in-p))
            (progn
              (redirect-to-login)
              nil)
            (if (role-allowed-p (current-role) required)
                t
                (progn
                  (render-forbidden (string-append "required role: " required))
                  nil))))))

(defun render-index (db)
  (let ((rows (fetch-pages db))
        (base (app-base)))
    (print-headers-ok)
    (print-layout-head "Wiki Index")
    (format t "<h1>Wiki Pages</h1>~%")
    (if (editor-or-admin-p)
        (format t "<p><a href=\"~A/new\">Create New Page</a> | <a href=\"~A/media/new\">Add Media</a></p>~%" base base)
        nil)
    (if (null rows)
        (format t "<p>ページがありません。</p>~%")
        (progn
          (format t "<ul>~%")
          (dolist (row rows)
            (let ((slug (first row))
                  (title (second row))
                  (updated-at (third row))
                  (page-status (fourth row)))
              (format t "<li><a href=\"~A/~A\">~A</a> <small>(~A / ~A)</small></li>~%"
                      base
                      (html-escape slug)
                      (html-escape title)
                      (html-escape updated-at)
                      (html-escape page-status))))
          (format t "</ul>~%")))
    (print-layout-foot)))

(defun render-search (db)
  (let ((q (query-param "q"))
        (tag (query-param "tag"))
        (status (query-param "status"))
        (base (app-base))
        (tags (fetch-search-tags db)))
    (print-headers-ok)
    (print-layout-head "Search")
    (format t "<h1>Search</h1>~%")
    (format t "<form method=\"get\" action=\"~A/search\" style=\"margin:0 0 1rem 0;padding:1rem;border:1px solid #ddd;border-radius:8px\">~%" base)
    (format t "<p><label>Query<br><input type=\"text\" name=\"q\" value=\"~A\" placeholder=\"Search wiki text\" style=\"width:100%\"></label></p>~%"
            (html-escape q))
    (format t "<p><label>Status<br><select name=\"status\"><option value=\"\">all</option><option value=\"published\"~A>published</option><option value=\"draft\"~A>draft</option><option value=\"private\"~A>private</option></select></label></p>~%"
            (if (string= status "published") " selected" "")
            (if (string= status "draft") " selected" "")
            (if (string= status "private") " selected" ""))
    (format t "<p><label>Tag<br><input type=\"text\" name=\"tag\" value=\"~A\" list=\"search-tags\" placeholder=\"tag name\"></label></p>~%"
            (html-escape tag))
    (format t "<datalist id=\"search-tags\">~%")
    (dolist (row tags)
      (format t "<option value=\"~A\"></option>~%" (html-escape (first row))))
    (format t "</datalist>~%")
    (format t "<p><button type=\"submit\">Search</button></p>~%")
    (format t "</form>~%")
    (if (blank-text-p q)
        (format t "<p>検索文字列を入力してください。</p>~%")
        (let ((rows (fetch-pages-by-query-filtered db q tag status)))
          (format t "<p>query: <code>~A</code>" (html-escape q))
          (if (blank-text-p tag)
              nil
              (format t " / tag: <code>~A</code>" (html-escape tag)))
          (if (blank-text-p status)
              nil
              (format t " / status: <code>~A</code>" (html-escape status)))
          (format t "</p>~%")
          (if (null rows)
              (format t "<p>一致するページはありません。</p>~%")
              (progn
                (format t "<ul>~%")
                (dolist (row rows)
                  (let ((slug (first row))
                        (title (second row))
                        (updated-at (third row))
                        (snippet (fourth row))
                        (tags-text (fifth row))
                        (page-status (sixth row)))
                    (format t "<li><a href=\"~A/~A?q=~A\">~A</a> <small>(~A / ~A)</small>"
                            base
                            (html-escape slug)
                            (html-escape (url-encode-lite q))
                            (html-escape title)
                            (html-escape updated-at)
                            (html-escape page-status))
                    (if (blank-text-p tags-text)
                        nil
                        (format t "<br><small>tags: ~A</small>" (html-escape tags-text)))
                    (if (blank-text-p snippet)
                        nil
                        (format t "<br><small>~A</small>" snippet))
                    (format t "</li>~%")))
                (format t "</ul>~%")))))
    (print-layout-foot)))

(defun print-highlight-script (q)
  (format t "<style>.wiki-hit{background:#ffeb3b;color:#111;padding:0 .08em;border-radius:2px}</style>~%")
  (format t "<script>(function(){~%")
  (format t "const q=\"~A\";~%" (js-string-escape q))
  (format t "if(!q){return;}~%")
  (format t "const esc=(s)=>s.replace(/[.*+?^${}()|[\\]\\\\]/g,'\\\\$&');~%")
  (format t "const re=new RegExp(esc(q),'gi');~%")
  (format t "const walk=(root)=>{const w=document.createTreeWalker(root,NodeFilter.SHOW_TEXT);const nodes=[];while(w.nextNode())nodes.push(w.currentNode);for(const n of nodes){if(!n.nodeValue||!re.test(n.nodeValue))continue;re.lastIndex=0;const frag=document.createDocumentFragment();let last=0;n.nodeValue.replace(re,(m,idx)=>{if(idx>last)frag.appendChild(document.createTextNode(n.nodeValue.slice(last,idx)));const mark=document.createElement('mark');mark.className='wiki-hit';mark.textContent=m;frag.appendChild(mark);last=idx+m.length;return m;});if(last<n.nodeValue.length)frag.appendChild(document.createTextNode(n.nodeValue.slice(last)));n.parentNode.replaceChild(frag,n);}};~%")
  (format t "document.querySelectorAll('h1,.wiki-body,pre').forEach((el)=>walk(el));~%")
  (format t "})();</script>~%"))

(defun render-view (db slug)
  (if (not (slug-safe-p slug))
      (render-not-found)
      (let ((row (fetch-page db slug))
            (base (app-base)))
        (if (null row)
            (render-not-found)
	            (let ((title (second row))
	                  (body-md (third row))
	                  (current-rev-no (page-current-rev-no row))
                      (page-status (page-status row))
                      (tags-text (fetch-page-tags-text db slug))
	                  (updated-at (fetch-page-updated-at db slug))
	                  (latest-summary (fetch-latest-edit-summary db slug))
	                  (media-rows (fetch-media-for-page db slug))
                  (search-q (query-param "q")))
              (let ((body-html (markdown->html body-md)))
              (print-headers-ok)
              (print-layout-head title)
	              (format t "<h1>~A</h1>~%" (html-escape title))
	              (if (editor-or-admin-p)
	                  (format t "<p><a href=\"~A/~A/edit\">Edit this page</a> | <a href=\"~A/~A/history\">History</a></p>~%"
	                          base
	                          (html-escape slug)
	                          base
	                          (html-escape slug))
	                  nil)
	              (if (not (editor-or-admin-p))
	                  (format t "<p><a href=\"~A/~A/history\">History</a></p>~%" base (html-escape slug))
	                  nil)
	              (if (editor-or-admin-p)
	                  (format t "<p><a href=\"~A/new\">Create New Page</a> | <a href=\"~A/media/new?page_slug=~A\">Attach Media To This Page</a></p>~%"
                                  base
                                  base
                                  (html-escape (url-encode-lite slug)))
	                  nil)
                  (if (editor-or-admin-p)
                      (progn
                        (format t "<form method=\"post\" action=\"~A/~A/delete\" style=\"margin:0 0 1rem 0\">~%"
                                base
                                (html-escape slug))
                        (render-csrf-hidden-input)
                        (format t "<button type=\"submit\" onclick=\"return confirm('Delete page ~A?');\">Delete Page</button>~%"
                                (html-escape slug))
                        (format t "</form>~%"))
                      nil)
	              (format t "<p><small>updated: ~A / slug: <code>~A</code> / rev: <code>~A</code></small></p>~%"
	                      (html-escape updated-at)
	                      (html-escape slug)
	                      (html-escape current-rev-no))
              (format t "<p><small>status: <code>~A</code></small></p>~%" (html-escape page-status))
              (if (blank-text-p tags-text)
                  nil
                  (format t "<p><small>tags: <code>~A</code></small></p>~%" (html-escape tags-text)))
              (if (string= latest-summary "")
                  nil
                  (format t "<p><small>latest summary: ~A</small></p>~%" (html-escape latest-summary)))
              (format t "<h2>Preview (HTML)</h2>~%")
              (format t "<article class=\"wiki-body\">~A</article>~%" body-html)
              (format t "<h2>Markdown Source</h2>~%")
              (format t "<pre>~A</pre>~%" (html-escape body-md))
              (if (blank-text-p search-q)
                  nil
                  (format t "<p><small>search highlight: <code>~A</code></small></p>~%"
                          (html-escape search-q)))
              (if (null media-rows)
                  nil
                  (progn
                    (format t "<h2>Attached Media</h2>~%")
                    (dolist (m media-rows)
                      (let ((media-type (first m))
                            (media-title (second m))
                            (stored-filename (third m))
                            (media-mime (fourth m)))
                        (let ((media-url (media-delivery-url stored-filename)))
                        (format t "<div style=\"margin:0 0 1rem 0\">~%")
                        (if (string= media-title "")
                            nil
                            (format t "<p><strong>~A</strong></p>~%" (html-escape media-title)))
                        (render-media-embed media-url media-title media-type media-mime)
                        (format t "<p><small><a href=\"~A\">~A</a> (~A)</small></p>~%"
                                (html-escape media-url)
                                (html-escape media-url)
                                (html-escape media-mime))
                        (format t "</div>~%"))))))
              (if (blank-text-p search-q)
                  nil
	                  (print-highlight-script search-q))
	              (print-layout-foot)))))))

(defun render-history (db slug)
  (if (not (slug-safe-p slug))
      (render-not-found)
      (let ((page (fetch-page db slug)))
        (if (null page)
            (render-not-found)
            (let ((rows (fetch-page-history db slug))
                  (title (second page))
                  (base (app-base))
                  (latest-rev (latest-revision-no db slug)))
              (print-headers-ok)
              (print-layout-head (string-append "History: " title))
              (format t "<h1>History: ~A</h1>~%" (html-escape title))
              (format t "<p><a href=\"~A/~A\">View current page</a>" base (html-escape slug))
              (if (editor-or-admin-p)
                  (format t " | <a href=\"~A/~A/edit\">Edit</a>" base (html-escape slug))
                  nil)
              (format t "</p>~%")
              (if (null rows)
                  (format t "<p>履歴がありません。</p>~%")
                  (progn
                    (format t "<table style=\"width:100%;border-collapse:collapse\">~%")
                    (format t "<thead><tr><th style=\"text-align:left;border-bottom:1px solid #ddd;padding:.4rem\">Rev</th><th style=\"text-align:left;border-bottom:1px solid #ddd;padding:.4rem\">Edited By</th><th style=\"text-align:left;border-bottom:1px solid #ddd;padding:.4rem\">Summary</th><th style=\"text-align:left;border-bottom:1px solid #ddd;padding:.4rem\">Created At</th><th style=\"text-align:left;border-bottom:1px solid #ddd;padding:.4rem\">Diff</th></tr></thead>~%")
                    (format t "<tbody>~%")
                    (dolist (row rows)
                      (let ((rev-no (first row))
                            (edited-by (second row))
                            (edit-summary (third row))
                            (created-at (fourth row)))
                        (format t "<tr>~%")
                        (format t "<td style=\"vertical-align:top;border-bottom:1px solid #eee;padding:.4rem\"><a href=\"~A/~A/revisions/~A\">~A</a></td>~%"
                                base
                                (html-escape slug)
                                (html-escape rev-no)
                                (html-escape rev-no))
                        (format t "<td style=\"vertical-align:top;border-bottom:1px solid #eee;padding:.4rem\">~A</td>~%" (html-escape edited-by))
                        (format t "<td style=\"vertical-align:top;border-bottom:1px solid #eee;padding:.4rem\">~A</td>~%" (html-escape edit-summary))
                        (format t "<td style=\"vertical-align:top;border-bottom:1px solid #eee;padding:.4rem\">~A</td>~%" (html-escape created-at))
                        (format t "<td style=\"vertical-align:top;border-bottom:1px solid #eee;padding:.4rem\">")
                        (if (string= rev-no latest-rev)
                            (format t "<small>latest</small>")
                            (format t "<a href=\"~A/~A/compare/~A/~A\">vs latest</a>"
                                    base
                                    (html-escape slug)
                                    (html-escape rev-no)
                                    (html-escape latest-rev)))
                        (format t "</td>~%")
                        (format t "</tr>~%")))
                    (format t "</tbody></table>~%")))
              (print-layout-foot))))))

(defun render-revision-view (db slug rev-no)
  (if (not (slug-safe-p slug))
      (render-not-found)
      (let ((page (fetch-page db slug))
            (rev (fetch-page-revision db slug rev-no))
            (base (app-base)))
        (if (or (null page) (null rev))
            (render-not-found)
            (let ((found-rev-no (first rev))
                  (title (second rev))
                  (body-md (third rev))
                  (edited-by (fourth rev))
                  (edit-summary (fifth rev))
                  (created-at (sixth rev)))
              (let ((body-html (markdown->html body-md)))
                (print-headers-ok)
                (print-layout-head (string-append "Revision " found-rev-no ": " title))
                (format t "<h1>Revision ~A: ~A</h1>~%" (html-escape found-rev-no) (html-escape title))
                (format t "<p><a href=\"~A/~A\">Current page</a> | <a href=\"~A/~A/history\">History</a></p>~%"
                        base (html-escape slug) base (html-escape slug))
                (format t "<p><small>edited_by: ~A / created_at: ~A</small></p>~%"
                        (html-escape edited-by)
                        (html-escape created-at))
                (if (string= edit-summary "")
                    nil
                    (format t "<p><small>edit_summary: ~A</small></p>~%" (html-escape edit-summary)))
                (format t "<h2>Preview (HTML)</h2>~%")
                (format t "<article class=\"wiki-body\">~A</article>~%" body-html)
                (format t "<h2>Markdown Source</h2>~%")
                (format t "<pre>~A</pre>~%" (html-escape body-md))
                (if (admin-p)
                    (progn
                      (format t "<form method=\"post\" action=\"~A/~A/revisions/~A/rollback\" style=\"margin-top:1rem\">~%"
                              base
                              (html-escape slug)
                              (html-escape found-rev-no))
                      (render-csrf-hidden-input)
                      (format t "<button type=\"submit\" onclick=\"return confirm('Rollback page to revision ~A?');\">Rollback To This Revision</button>~%" (html-escape found-rev-no))
                      (format t "</form>~%"))
                    nil)
                (print-layout-foot)))))))

(defun render-compare-view (db slug left-rev-no right-rev-no)
  (if (not (slug-safe-p slug))
      (render-not-found)
      (let ((page (fetch-page db slug))
            (left-rev (fetch-page-revision db slug left-rev-no))
            (right-rev (fetch-page-revision db slug right-rev-no))
            (base (app-base)))
        (if (or (null page) (null left-rev) (null right-rev))
            (render-not-found)
            (let ((left-title (second left-rev))
                  (left-body (third left-rev))
                  (right-title (second right-rev))
                  (right-body (third right-rev)))
              (print-headers-ok)
              (print-layout-head (string-append "Compare Revisions: " slug))
              (format t "<h1>Compare Revisions</h1>~%")
              (format t "<p><a href=\"~A/~A/history\">History</a> | <a href=\"~A/~A\">Current page</a></p>~%"
                      base (html-escape slug) base (html-escape slug))
              (format t "<p><small>left rev: <code>~A</code> / right rev: <code>~A</code></small></p>~%"
                      (html-escape left-rev-no)
                      (html-escape right-rev-no))
              (format t "<h2>Title</h2>~%")
              (render-diff-table left-title right-title)
              (format t "<h2>Body</h2>~%")
              (render-diff-table left-body right-body)
              (print-layout-foot))))))

(defun rollback-page-to-revision (db slug rev-no)
  (let ((raw-body (read-form-body-or-render t)))
    (if (null raw-body)
        nil
        (let ((rev (fetch-page-revision db slug rev-no)))
          (if (null rev)
              (render-not-found)
              (let* ((title (second rev))
                     (body-md (third rev))
                     (target-rev-no (first rev))
                     (page-id (page-id-by-slug slug))
                     (next-rev (+ (parse-int-safe (page-current-rev-no (fetch-page-any db slug))) 1)))
                (db-exec!
                 db
                 (string-append
                  "UPDATE pages SET title=" (db-text-sql title)
                  ", body_md=" (db-text-sql body-md)
                  ", last_edited_by=" (db-text-sql (current-username))
                  ", current_rev_no=" (format nil "~A" next-rev)
                  " WHERE slug=" (db-text-sql slug) ";"))
                (db-exec!
                 db
                 (string-append
                  "INSERT INTO page_revisions (page_id, rev_no, title, body_md, edited_by, edit_summary) VALUES ("
                  (format nil "~A" page-id) ", "
                  (format nil "~A" next-rev) ", "
                  (db-text-sql title) ", "
                  (db-text-sql body-md) ", "
                  (db-text-sql (current-username)) ", "
                  (db-text-sql (string-append "rollback to rev " target-rev-no))
                  ");"))
                (write-audit-log
                 db
                 "page.rollback"
                 "page"
                 slug
                 (make-audit-meta-4
                  "rollback_to_rev" target-rev-no
                  "title" title
                  "edited_by" (current-username)
                  "edit_summary" (string-append "rollback to rev " target-rev-no)))
                (render-see-other (string-append (app-base) "/" slug))))))))

(defun save-page (db slug)
  (if (or (not (slug-safe-p slug)) (reserved-slug-p slug))
      (render-not-found)
      (let ((page (fetch-page db slug)))
        (if (null page)
            (render-not-found)
            (let ((raw-body (read-form-body-or-render t)))
              (if (null raw-body)
                  nil
                  (let* ((title (parse-form-field raw-body "title"))
                         (body-md (parse-form-field raw-body "body_md"))
                         (status (parse-form-field raw-body "status"))
                         (tags-raw (parse-form-field raw-body "tags"))
                         (edit-summary (parse-form-field raw-body "edit_summary"))
                         (expected-rev (parse-form-field raw-body "current_rev_no"))
                         (page-now (fetch-page db slug))
                         (actual-rev (page-current-rev-no page-now)))
                    (if (blank-text-p title)
                        (render-bad-request "title must not be blank")
                        (if (not (valid-page-status-p status))
                            (render-bad-request "status must be draft/published/private")
                            (if (or (blank-text-p expected-rev)
                                    (not (string= expected-rev actual-rev)))
                                (render-edit-conflict db slug title body-md edit-summary expected-rev actual-rev)
                                (let* ((page-id (page-id-by-slug slug))
                                       (next-rev (+ (parse-int-safe actual-rev) 1)))
                                  (db-exec!
                                   db
                                   (string-append
                                    "UPDATE pages SET title=" (db-text-sql title)
                                    ", body_md=" (db-text-sql body-md)
                                    ", status=" (db-text-sql status)
                                    ", last_edited_by=" (db-text-sql (current-username))
                                    ", current_rev_no=" (format nil "~A" next-rev)
                                    " WHERE slug=" (db-text-sql slug)
                                    " AND current_rev_no=" expected-rev ";"))
                                  (db-exec!
                                   db
                                   (string-append
                                    "INSERT INTO page_revisions (page_id, rev_no, title, body_md, edited_by, edit_summary) VALUES ("
                                    (format nil "~A" page-id) ", "
                                    (format nil "~A" next-rev) ", "
                                    (db-text-sql title) ", "
                                    (db-text-sql body-md) ", "
                                    (db-text-sql (current-username)) ", "
                                    (db-text-sql edit-summary)
                                    ");"))
                                  (sync-page-tags db slug (split-tags-input tags-raw))
                                  (write-audit-log
                                   db
                                   "page.update"
                                   "page"
                                   slug
                                   (make-audit-meta-2 "title" title "edit_summary" edit-summary))
                                  (write-structured-log
                                   "info"
                                   "page.update"
                                   (make-log-fields-3
                                    "slug" slug
                                    "title" title
                                    "edit_summary" edit-summary))
                                  (render-see-other (string-append (app-base) "/" slug)))))))))))))

(defun create-page (db)
  (let ((raw-body (read-form-body-or-render t)))
    (if (null raw-body)
        nil
        (let* ((slug (parse-form-field raw-body "slug"))
               (title (parse-form-field raw-body "title"))
               (body-md (parse-form-field raw-body "body_md"))
               (status (parse-form-field raw-body "status"))
               (tags-raw (parse-form-field raw-body "tags"))
               (edit-summary (parse-form-field raw-body "edit_summary")))
          (if (or (not (slug-safe-p slug)) (reserved-slug-p slug))
              (render-bad-request "invalid slug")
              (if (blank-text-p title)
                  (render-bad-request "title must not be blank")
                  (if (not (valid-page-status-p status))
                      (render-bad-request "status must be draft/published/private")
                      (if (null (fetch-page-any db slug))
                          (let ((page-id '()))
                            (db-exec!
                             db
                             (string-append
                              "INSERT INTO pages (slug, title, body_md, status, last_edited_by, current_rev_no) VALUES ("
                              (db-text-sql slug) ", "
                              (db-text-sql title) ", "
                              (db-text-sql body-md) ", "
                              (db-text-sql status) ", "
                              (db-text-sql (current-username)) ", 1);"))
                            (setq page-id (page-id-by-slug slug))
                            (db-exec!
                             db
                             (string-append
                              "INSERT INTO page_revisions (page_id, rev_no, title, body_md, edited_by, edit_summary) VALUES ("
                              (format nil "~A" page-id) ", 1, "
                              (db-text-sql title) ", "
                              (db-text-sql body-md) ", "
                              (db-text-sql (current-username)) ", "
                              (db-text-sql edit-summary)
                              ");"))
                            (sync-page-tags db slug (split-tags-input tags-raw))
                            (write-audit-log
                             db
                             "page.create"
                             "page"
                             slug
                             (make-audit-meta-2 "title" title "edit_summary" edit-summary))
                            (write-structured-log
                             "info"
                             "page.create"
                             (make-log-fields-3
                              "slug" slug
                              "title" title
                              "edit_summary" edit-summary))
                            (render-see-other (string-append (app-base) "/" slug)))
                          (render-bad-request "slug already exists (or is soft-deleted; restore it from admin)")))))))))

(defun render-media-index (db)
  (let ((rows (fetch-media-list db))
        (base (app-base)))
    (print-headers-ok)
    (print-layout-head "Media Library")
    (format t "<h1>Media Library</h1>~%")
    (if (editor-or-admin-p)
        (format t "<p><a href=\"~A/media/new\">Add Media</a></p>~%" base)
        nil)
    (if (null rows)
        (format t "<p>メディアがありません。</p>~%")
        (dolist (row rows)
          (let ((media-id (first row))
                (media-type (second row))
                (media-title (third row))
                (stored-filename (fourth row))
                (media-mime (fifth row))
                (page-slug (sixth row)))
            (let ((media-url (media-delivery-url stored-filename)))
            (format t "<section style=\"margin:0 0 1.5rem 0;padding:1rem;border:1px solid #ddd;border-radius:8px\">~%")
            (format t "<p><strong>#~A</strong> [~A]</p>~%" (html-escape media-id) (html-escape media-type))
            (if (string= media-title "")
                nil
                (format t "<p>~A</p>~%" (html-escape media-title)))
            (render-media-embed media-url media-title media-type media-mime)
            (format t "<p><small><a href=\"~A\">~A</a> (~A)</small></p>~%"
                    (html-escape media-url)
                    (html-escape media-url)
                    (html-escape media-mime))
            (if (string= page-slug "")
                nil
                (format t "<p><small>page: <a href=\"~A/~A\">~A</a></small></p>~%"
                        base
                        (html-escape page-slug)
                        (html-escape page-slug)))
            (if (admin-p)
                (progn
                  (format t "<form method=\"post\" action=\"~A/media/~A/delete\" style=\"margin-top:.75rem\">~%" base (html-escape media-id))
                  (render-csrf-hidden-input)
                  (format t "<button type=\"submit\" onclick=\"return confirm('Delete media #~A?');\">Delete Media</button>~%" (html-escape media-id))
                  (format t "</form>~%"))
                nil)
            (format t "</section>~%")))))
    (print-layout-foot)))

(defun render-admin-deleted-pages (db)
  (let ((rows (fetch-deleted-pages db))
        (base (app-base)))
    (print-headers-ok)
    (print-layout-head "Deleted Pages")
    (format t "<h1>Deleted Pages</h1>~%")
    (format t "<p><a href=\"~A/admin\">Admin</a></p>~%" base)
    (if (null rows)
        (format t "<p>削除済みページはありません。</p>~%")
        (progn
          (format t "<table style=\"width:100%;border-collapse:collapse\">~%")
          (format t "<thead><tr><th style=\"text-align:left;border-bottom:1px solid #ddd;padding:.4rem\">Slug</th><th style=\"text-align:left;border-bottom:1px solid #ddd;padding:.4rem\">Title</th><th style=\"text-align:left;border-bottom:1px solid #ddd;padding:.4rem\">Deleted At</th><th style=\"text-align:left;border-bottom:1px solid #ddd;padding:.4rem\">Deleted By</th><th style=\"text-align:left;border-bottom:1px solid #ddd;padding:.4rem\">Action</th></tr></thead>~%")
          (format t "<tbody>~%")
          (dolist (row rows)
            (let ((slug (first row))
                  (title (second row))
                  (deleted-at (third row))
                  (deleted-by (fourth row)))
              (format t "<tr>~%")
              (format t "<td style=\"vertical-align:top;border-bottom:1px solid #eee;padding:.4rem\"><code>~A</code></td>~%" (html-escape slug))
              (format t "<td style=\"vertical-align:top;border-bottom:1px solid #eee;padding:.4rem\">~A</td>~%" (html-escape title))
              (format t "<td style=\"vertical-align:top;border-bottom:1px solid #eee;padding:.4rem\">~A</td>~%" (html-escape deleted-at))
              (format t "<td style=\"vertical-align:top;border-bottom:1px solid #eee;padding:.4rem\">~A</td>~%" (html-escape deleted-by))
              (format t "<td style=\"vertical-align:top;border-bottom:1px solid #eee;padding:.4rem\">~%")
              (format t "<form method=\"post\" action=\"~A/admin/deleted-pages/~A/restore\">~%" base (html-escape slug))
              (render-csrf-hidden-input)
              (format t "<button type=\"submit\" onclick=\"return confirm('Restore page ~A?');\">Restore</button>~%" (html-escape slug))
              (format t "</form>~%")
              (format t "</td>~%")
              (format t "</tr>~%")))
          (format t "</tbody></table>~%")))
    (print-layout-foot)))

(defun render-admin-deleted-media (db)
  (let ((rows (fetch-deleted-media db))
        (base (app-base)))
    (print-headers-ok)
    (print-layout-head "Deleted Media")
    (format t "<h1>Deleted Media</h1>~%")
    (format t "<p><a href=\"~A/admin\">Admin</a></p>~%" base)
    (if (null rows)
        (format t "<p>削除済みメディアはありません。</p>~%")
        (progn
          (format t "<table style=\"width:100%;border-collapse:collapse\">~%")
          (format t "<thead><tr><th style=\"text-align:left;border-bottom:1px solid #ddd;padding:.4rem\">ID</th><th style=\"text-align:left;border-bottom:1px solid #ddd;padding:.4rem\">Type</th><th style=\"text-align:left;border-bottom:1px solid #ddd;padding:.4rem\">Title</th><th style=\"text-align:left;border-bottom:1px solid #ddd;padding:.4rem\">Deleted At</th><th style=\"text-align:left;border-bottom:1px solid #ddd;padding:.4rem\">Deleted By</th><th style=\"text-align:left;border-bottom:1px solid #ddd;padding:.4rem\">Action</th></tr></thead>~%")
          (format t "<tbody>~%")
          (dolist (row rows)
            (let ((media-id (first row))
                  (media-type (second row))
                  (media-title (third row))
                  (deleted-at (seventh row))
                  (deleted-by (eighth row)))
              (format t "<tr>~%")
              (format t "<td style=\"vertical-align:top;border-bottom:1px solid #eee;padding:.4rem\"><code>~A</code></td>~%" (html-escape media-id))
              (format t "<td style=\"vertical-align:top;border-bottom:1px solid #eee;padding:.4rem\">~A</td>~%" (html-escape media-type))
              (format t "<td style=\"vertical-align:top;border-bottom:1px solid #eee;padding:.4rem\">~A</td>~%" (html-escape media-title))
              (format t "<td style=\"vertical-align:top;border-bottom:1px solid #eee;padding:.4rem\">~A</td>~%" (html-escape deleted-at))
              (format t "<td style=\"vertical-align:top;border-bottom:1px solid #eee;padding:.4rem\">~A</td>~%" (html-escape deleted-by))
              (format t "<td style=\"vertical-align:top;border-bottom:1px solid #eee;padding:.4rem\">~%")
              (format t "<form method=\"post\" action=\"~A/admin/deleted-media/~A/restore\">~%" base (html-escape media-id))
              (render-csrf-hidden-input)
              (format t "<button type=\"submit\" onclick=\"return confirm('Restore media #~A?');\">Restore</button>~%" (html-escape media-id))
              (format t "</form>~%")
              (format t "</td>~%")
              (format t "</tr>~%")))
          (format t "</tbody></table>~%")))
    (print-layout-foot)))

(defun render-file-not-found ()
  (format t "Status: 404 Not Found~%")
  (format t "Content-Type: text/plain; charset=UTF-8~%")
  (print-extra-headers)
  (format t "~%")
  (format t "Not Found~%"))

(defun file-size-bytes (path)
  (os-wc-c path))

(defun detect-mime-type (path)
  (os-file-mime-type path))

(defun safe-download-filename (name)
  (sanitize-filename (basename name)))

(defun inline-media-mime-p (mime-type)
  (or (starts-with mime-type "image/")
      (starts-with mime-type "video/")
      (starts-with mime-type "audio/")))

(defun validate-media-upload (path original-filename desired-mime)
  (let* ((size (file-size-bytes path))
         (ext (file-ext-lower original-filename))
         (actual-mime (detect-mime-type path))
         (expected-mime (infer-mime-type original-filename (infer-media-type original-filename))))
    (cond
     ((blank-text-p original-filename)
      (list "error" "filename is required"))
     ((dangerous-media-extension-p ext)
      (list "error" "dangerous file extension is not allowed"))
     ((not (allowed-media-extension-p ext))
      (list "error" "file extension is not allowed"))
     ((<= size 0)
      (list "error" "empty file is not allowed"))
     ((> size (media-max-bytes))
      (list "error" (string-append "file exceeds max size of " (format nil "~A" (media-max-bytes)) " bytes")))
     ((blank-text-p actual-mime)
      (list "error" "failed to detect actual mime type"))
     ((not (media-mime-allowed-p actual-mime))
      (list "error" "detected mime type is not allowed"))
     ((not (allowed-mime-for-ext-p ext actual-mime))
      (list "error" "filename extension does not match detected mime type"))
     ((and (not (blank-text-p expected-mime))
           (not (string= expected-mime actual-mime))
           (not (allowed-mime-for-ext-p ext actual-mime)))
      (list "error" "inferred mime type does not match detected mime type"))
     ((and (not (blank-text-p desired-mime))
           (not (allowed-mime-for-ext-p ext desired-mime)))
      (list "error" "declared mime type is not allowed for this extension"))
     ((and (not (blank-text-p desired-mime))
           (not (string= desired-mime actual-mime)))
      (list "error" "declared mime type does not match detected mime type"))
     (t
      (list "ok" actual-mime)))))

(defun parse-int-safe (s)
  (let ((i 0)
        (n 0))
    (if (= (length s) 0)
        '()
        (progn
          (while (< i (length s))
            (let ((p (string-index (substring s i (+ i 1)) "0123456789")))
              (if (null p)
                  (setq n 'invalid)
                  (if (not (eq n 'invalid))
                      (setq n (+ (* n 10) p))
                      nil)))
            (setq i (+ i 1)))
          (if (eq n 'invalid) '() n)))))

(defun render-media-file-stream (storage-path mime-type download-name)
  (let ((size (file-size-bytes storage-path)))
    (format t "Status: 200 OK~%")
    (format t "Content-Type: ~A~%" (if (blank-text-p mime-type) "application/octet-stream" mime-type))
    (format t "Content-Length: ~A~%" size)
    (format t "X-Content-Type-Options: nosniff~%")
    (format t "Content-Disposition: ~A; filename=\"~A\"~%"
            (if (inline-media-mime-p mime-type) "inline" "attachment")
            (html-escape (safe-download-filename download-name)))
    (print-extra-headers)
    (format t "~%")
    (os-cat-to-stdout storage-path)))

(defun render-media-file (db stored-filename)
  (let ((meta (fetch-media-file-meta db stored-filename)))
    (if (null meta)
        (render-file-not-found)
        (let ((storage-path (first meta))
              (mime-type (second meta))
              (download-name (third meta)))
          (if (null (probe-file storage-path))
              (render-file-not-found)
              (render-media-file-stream storage-path mime-type download-name))))))

(defun render-page-slug-datalist (rows)
  (format t "<datalist id=\"page-slugs\">~%")
  (dolist (row rows)
    (format t "<option value=\"~A\">~A</option>~%"
            (html-escape (first row))
            (html-escape (second row))))
  (format t "</datalist>~%"))

(defun render-media-new (db)
  (let ((base (app-base))
        (rows (fetch-pages db))
        (prefill-page-slug (query-param "page_slug")))
    (print-headers-ok)
    (print-layout-head "Add Media")
    (format t "<h1>Add Media</h1>~%")
    (format t "<p>ブラウザからファイルをアップロードして、メディアライブラリへ登録します。</p>~%")
    (format t "<p><small>max size: <code>~A</code> bytes / allowed: png, jpg, jpeg, gif, webp, mp4, webm, mov, mkv, mp3, wav, ogg, m4a, flac, pdf, zip, gz, tgz, tar, 7z, txt, md, csv, json</small></p>~%"
            (html-escape (format nil "~A" (media-max-bytes))))
    (render-page-slug-datalist rows)
    (format t "<form method=\"post\" action=\"~A/media/new\" enctype=\"multipart/form-data\">~%" base)
    (render-csrf-hidden-input)
    (format t "  <p><label>Upload File<br><input type=\"file\" name=\"upload_file\" accept=\".png,.jpg,.jpeg,.gif,.webp,.mp4,.webm,.mov,.mkv,.mp3,.wav,.ogg,.m4a,.flac,.pdf,.zip,.gz,.tgz,.tar,.7z,.txt,.md,.csv,.json\" required></label></p>~%")
    (format t "  <p><label>Title<br><input type=\"text\" name=\"title\" value=\"\" style=\"width:100%\"></label></p>~%")
    (format t "  <p><label>Attach To Page (optional)<br><input type=\"text\" name=\"page_slug\" value=\"~A\" style=\"width:100%\" list=\"page-slugs\" placeholder=\"home\"></label></p>~%"
            (html-escape prefill-page-slug))
    (format t "  <p><small>既存ページに紐付けると、そのページの添付一覧に表示されます。</small></p>~%")
    (format t "  <p><label>Media Type<br><select name=\"media_type\"><option value=\"\">auto</option><option value=\"image\">image</option><option value=\"video\">video</option><option value=\"audio\">audio</option><option value=\"file\">file</option></select></label></p>~%")
    (format t "  <p><label>MIME Type (optional)<br><input type=\"text\" name=\"mime_type\" value=\"\" style=\"width:100%\" placeholder=\"application/pdf\"></label></p>~%")
    (format t "  <p><label>Edit Summary<br><input type=\"text\" name=\"edit_summary\" value=\"add media\" style=\"width:100%\"></label></p>~%")
    (format t "  <p><button type=\"submit\">Add Media</button></p>~%")
    (format t "</form>~%")
    (if (admin-p)
        (progn
          (format t "<hr>~%")
          (format t "<h2>Admin Only: Register From Server Path</h2>~%")
          (format t "<form method=\"post\" action=\"~A/media/new\">~%" base)
          (render-csrf-hidden-input)
          (format t "  <p><label>Source Path (server local)<br><input type=\"text\" name=\"source_path\" value=\"\" style=\"width:100%\" placeholder=\"/path/to/file.png\"></label></p>~%")
          (format t "  <p><label>Title<br><input type=\"text\" name=\"title\" value=\"\" style=\"width:100%\"></label></p>~%")
          (format t "  <p><label>Attach To Page (optional)<br><input type=\"text\" name=\"page_slug\" value=\"~A\" style=\"width:100%\" list=\"page-slugs\" placeholder=\"home\"></label></p>~%"
                  (html-escape prefill-page-slug))
          (format t "  <p><label>Media Type<br><select name=\"media_type\"><option value=\"\">auto</option><option value=\"image\">image</option><option value=\"video\">video</option><option value=\"audio\">audio</option><option value=\"file\">file</option></select></label></p>~%")
          (format t "  <p><label>MIME Type (optional)<br><input type=\"text\" name=\"mime_type\" value=\"\" style=\"width:100%\" placeholder=\"application/pdf\"></label></p>~%")
          (format t "  <p><label>Edit Summary<br><input type=\"text\" name=\"edit_summary\" value=\"add media\" style=\"width:100%\"></label></p>~%")
          (format t "  <p><button type=\"submit\">Register From Source Path</button></p>~%")
          (format t "</form>~%"))
        nil)
    (print-layout-foot)))

(defun path-under-dir-p (path dir)
  (let ((base (trim-trailing-slashes dir)))
    (and (starts-with path base)
         (> (length path) (length base))
         (string= (substring path (length base) (+ (length base) 1)) "/"))))

(defun write-backup-run-record (db mode status backup-dir keep-count dry-run sql-path media-path message)
  (db-exec!
   db
   (string-append
    "INSERT INTO backup_runs (triggered_by_user_id, mode, status, backup_dir, keep_count, dry_run, sql_path, media_path, message, completed_at) VALUES ("
    (audit-actor-id-sql) ", '"
    (sql-escape mode) "', '"
    (sql-escape status) "', "
    (db-nullable-text-sql backup-dir) ", "
    (db-nullable-number-sql keep-count) ", "
    (db-bool-sql dry-run) ", "
    (db-nullable-text-sql sql-path) ", "
    (db-nullable-text-sql media-path) ", "
    (db-nullable-text-sql message) ", "
    (db-text-sql (db-now-text))
    ");")))

(defun cleanup-old-backups (dir keep-count)
  (if (or (null keep-count) (<= keep-count 0))
      nil
      (let ((entries (os-ls-newest-first-glob (string-append dir "/wiki-backup-*")))
            (idx 0))
        (dolist (line entries)
          (setq idx (+ idx 1))
          (if (> idx keep-count)
              (safe-delete-file (trim-ws line))
              nil)))))

(defun run-backup (target-dir keep-count dry-run)
  (let ((backup-dir (if (blank-text-p target-dir) (backup-root-dir) target-dir)))
    (ensure-backup-dir)
    (ensure-media-dir)
    (if (not (path-under-dir-p backup-dir "/tmp"))
        (list "error" "" "" "backup_dir must be under /tmp")
	        (let* ((ts (format nil "~A" (get-universal-time)))
	               (base (string-append backup-dir "/wiki-backup-" ts))
	               (sql-path (string-append base ".sql"))
	               (media-path (string-append base "-media.tar.gz")))
          (if dry-run
              (list "dry-run" sql-path media-path "backup dry-run only")
              (progn
                (os-mkdir-p backup-dir)
                (let ((db-result (dbms-wiki-backup sql-path)))
                  (if (string= (first db-result) "ok")
                      (let ((media-status (if (os-tar-create-gz media-path (media-root-dir)) 0 1)))
                        (if (= media-status 0)
                            (progn
                              (cleanup-old-backups backup-dir keep-count)
                              (list "ok" sql-path media-path "backup completed"))
                            (list "error" sql-path media-path "media backup failed")))
                      (list "error" sql-path media-path (second db-result))))))))))

(defun run-backup-with-audit (db target-dir keep-count dry-run)
  (let ((result (run-backup target-dir keep-count dry-run)))
    (write-backup-run-record
     db
     "backup"
     (first result)
     (if (blank-text-p target-dir) (backup-root-dir) target-dir)
     keep-count
     dry-run
     (second result)
     (third result)
     (fourth result))
    (if (or (string= (first result) "ok") (string= (first result) "dry-run"))
        (write-audit-log
         db
         "backup.run"
         "backup"
         (second result)
         (make-audit-meta-3
          "sql_path" (second result)
          "media_path" (third result)
          "status" (first result)))
        nil)
    result))

(defun run-restore (sql-path media-path restore-media)
  (if (or (blank-text-p sql-path)
          (not (path-under-dir-p sql-path (backup-root-dir)))
          (null (probe-file sql-path)))
      (list "error" "invalid sql_path")
      (let ((db-result (dbms-wiki-restore sql-path)))
        (if (not (string= (first db-result) "ok"))
            (list "error" (second db-result))
            (if (or (blank-text-p media-path) (not restore-media))
                (list "ok" "database restored")
                (if (or (not (path-under-dir-p media-path (backup-root-dir)))
                        (null (probe-file media-path)))
                    (list "error" "invalid media_archive_path")
                    (progn
                      (ensure-media-dir)
	                      (let ((media-status (if (os-tar-extract-gz media-path (media-root-dir)) 0 1)))
	                        (if (= media-status 0)
	                            (list "ok" "database and media restored")
	                            (list "error" "media restore failed"))))))))))

(defun run-restore-with-audit (db sql-path media-path restore-media)
  (let ((result (run-restore sql-path media-path restore-media)))
    (write-backup-run-record
     db
     "restore"
     (first result)
     (backup-root-dir)
     (backup-keep-count)
     nil
     sql-path
     media-path
     (second result))
    (if (string= (first result) "ok")
        (write-audit-log
         db
         "restore.run"
         "backup"
         sql-path
         (make-audit-meta-3
          "sql_path" sql-path
          "media_path" media-path
          "restore_media" (if restore-media "true" "false")))
        nil)
    result))

(defun render-backup-runs-table (rows)
  (if (null rows)
      (format t "<p>バックアップ履歴はまだありません。</p>~%")
      (progn
        (format t "<table style=\"width:100%;border-collapse:collapse\">~%")
        (format t "<thead><tr><th style=\"text-align:left;border-bottom:1px solid #ddd;padding:.4rem\">When</th><th style=\"text-align:left;border-bottom:1px solid #ddd;padding:.4rem\">Mode</th><th style=\"text-align:left;border-bottom:1px solid #ddd;padding:.4rem\">Status</th><th style=\"text-align:left;border-bottom:1px solid #ddd;padding:.4rem\">Dir</th><th style=\"text-align:left;border-bottom:1px solid #ddd;padding:.4rem\">Keep</th><th style=\"text-align:left;border-bottom:1px solid #ddd;padding:.4rem\">Actor</th><th style=\"text-align:left;border-bottom:1px solid #ddd;padding:.4rem\">Message</th></tr></thead>~%")
        (format t "<tbody>~%")
        (dolist (row rows)
          (let ((mode (list-nth-or-empty row 1))
                (status (list-nth-or-empty row 2))
                (backup-dir (list-nth-or-empty row 3))
                (keep-count (list-nth-or-empty row 4))
                (message (list-nth-or-empty row 8))
                (created-at (list-nth-or-empty row 9))
                (actor (list-nth-or-empty row 11)))
            (format t "<tr>~%")
            (format t "<td style=\"vertical-align:top;border-bottom:1px solid #eee;padding:.4rem\">~A</td>~%" (html-escape created-at))
            (format t "<td style=\"vertical-align:top;border-bottom:1px solid #eee;padding:.4rem\"><code>~A</code></td>~%" (html-escape mode))
            (format t "<td style=\"vertical-align:top;border-bottom:1px solid #eee;padding:.4rem\"><code>~A</code></td>~%" (html-escape status))
            (format t "<td style=\"vertical-align:top;border-bottom:1px solid #eee;padding:.4rem\">~A</td>~%" (html-escape backup-dir))
            (format t "<td style=\"vertical-align:top;border-bottom:1px solid #eee;padding:.4rem\">~A</td>~%" (html-escape keep-count))
            (format t "<td style=\"vertical-align:top;border-bottom:1px solid #eee;padding:.4rem\">~A</td>~%" (html-escape actor))
            (format t "<td style=\"vertical-align:top;border-bottom:1px solid #eee;padding:.4rem\">~A</td>~%" (html-escape message))
            (format t "</tr>~%")))
        (format t "</tbody></table>~%"))))

(defun persist-media-record (db page-slug media-type title original-filename stored-name mime-type storage-path public-url edit-summary)
  (let ((page-id (if (blank-text-p page-slug) '() (page-id-by-slug page-slug))))
    (if (and (not (blank-text-p page-slug)) (null page-id))
        "page_slug not found"
        (progn
          (db-exec!
           db
           (string-append
            "INSERT INTO media_assets (page_id, media_type, title, original_filename, stored_filename, mime_type, storage_path, public_url, created_by) VALUES ("
            (if (null page-id) "NULL" (format nil "~A" page-id)) ", "
            (db-text-sql media-type) ", "
            (db-text-sql title) ", "
            (db-text-sql original-filename) ", "
            (db-text-sql stored-name) ", "
            (db-text-sql mime-type) ", "
            (db-text-sql storage-path) ", "
            (db-text-sql public-url) ", "
            (db-text-sql (current-username))
            ");"))
          (if (or (blank-text-p edit-summary) (blank-text-p page-slug))
              nil
              (let ((page (fetch-page-any db page-slug))
                    (next-rev (+ (parse-int-safe (page-current-rev-no (fetch-page-any db page-slug))) 1)))
                (db-exec!
               db
               (string-append
                "UPDATE pages SET current_rev_no=" (format nil "~A" next-rev)
                ", last_edited_by=" (db-text-sql (current-username))
                " WHERE slug=" (db-text-sql page-slug) ";"))
                (db-exec!
                 db
                 (string-append
                  "INSERT INTO page_revisions (page_id, rev_no, title, body_md, edited_by, edit_summary) VALUES ("
                  (format nil "~A" page-id) ", "
                  (format nil "~A" next-rev) ", "
                  (db-text-sql (second page)) ", "
                  (db-text-sql (third page)) ", "
                  (db-text-sql (current-username)) ", "
                  (db-text-sql edit-summary)
                  ");"))))
          (write-audit-log
           db
           "media.create"
           "media"
           stored-name
           (make-audit-meta-3
            "title" title
            "page_slug" page-slug
            "stored_filename" stored-name))
          (write-structured-log
           "info"
           "media.create"
           (make-log-fields-3
            "page_slug" page-slug
            "stored_filename" stored-name
            "mime_type" mime-type))
          ""))))

(defun move-upload-into-media-dir (temp-path stored-name)
    (let ((dest-path (string-append (media-root-dir) "/" stored-name)))
    (ensure-media-dir)
    (let ((mv-status (if (os-mv temp-path dest-path) 0 1)))
      (if (= mv-status 0)
          (progn
            (os-chmod "644" dest-path)
            dest-path)
          ""))))

(defun create-media-from-source-path (db raw-body)
  (let* ((source-path (parse-form-field raw-body "source_path"))
         (title (parse-form-field raw-body "title"))
         (page-slug (parse-form-field raw-body "page_slug"))
         (media-type-input (parse-form-field raw-body "media_type"))
         (mime-type-input (parse-form-field raw-body "mime_type"))
         (edit-summary (parse-form-field raw-body "edit_summary")))
    (if (not (admin-p))
        (render-forbidden "source_path mode is admin only")
        (if (blank-text-p source-path)
            (render-bad-request "source_path must not be blank")
            (if (null (probe-file source-path))
                (render-bad-request "source file not found")
                (let* ((base-name (basename source-path))
                       (safe-name (sanitize-filename base-name))
                       (media-type (if (blank-text-p media-type-input)
                                       (infer-media-type base-name)
                                       media-type-input))
                       (validation (validate-media-upload source-path base-name mime-type-input))
                       (stored-name (string-append
                                     (format nil "~A" (get-universal-time))
                                     "-"
                                     (format nil "~A" *markdown-temp-counter*)
                                     "-"
                                     safe-name))
                       (storage-path (string-append (media-root-dir) "/" stored-name))
                       (public-url (media-delivery-url stored-name)))
                  (if (not (valid-media-type-p media-type))
                      (render-bad-request "media_type must be image/video/audio/file")
                      (if (not (string= (first validation) "ok"))
                          (render-bad-request (second validation))
                          (let ((mime-type (second validation)))
                            (ensure-media-dir)
                            (let ((copy-status (if (os-cp source-path storage-path) 0 1)))
                              (if (= copy-status 0)
                                  (progn
                                    (os-chmod "644" storage-path)
                                    (let ((err (persist-media-record db page-slug media-type title base-name stored-name mime-type storage-path public-url edit-summary)))
                                      (if (string= err "")
                                          (render-see-other (string-append (app-base) "/media"))
                                          (progn
                                            (safe-delete-file storage-path)
                                            (render-bad-request err)))))
                                  (render-bad-request "file copy failed"))))))))))))

(defun create-media-from-multipart (db)
  (let* ((content-type (request-content-type))
         (boundary (multipart-boundary content-type))
         (temp-base (next-temp-base))
         (body-path (string-append temp-base ".body"))
         (work-dir (string-append temp-base "-upload")))
    (if (blank-text-p boundary)
        (render-bad-request "multipart boundary not found")
        (let ((capture-status (capture-request-body-to-file body-path)))
          (if (not (= capture-status 0))
              (render-bad-request "failed to receive upload body")
              (progn
                (os-mkdir-p work-dir)
                (setenv "ISL_ROOT" (wiki-root))
                (setenv "WIKI_MULTIPART_INPUT" body-path)
                (setenv "WIKI_MULTIPART_OUTDIR" work-dir)
                (setenv "WIKI_MULTIPART_BOUNDARY" boundary)
                (let ((parse-status
                       (system
                        (string-append
                         (shell-quote (wiki-path "/bin/isl"))
                         " "
                         (shell-quote (multipart-helper-path))))))
                  (if (not (= parse-status 0))
                      (render-bad-request "failed to parse multipart upload")
                      (let* ((csrf-token (read-optional-file-text (multipart-field-path work-dir "csrf_token")))
                             (title (read-optional-file-text (multipart-field-path work-dir "title")))
                             (page-slug (read-optional-file-text (multipart-field-path work-dir "page_slug")))
                             (media-type-input (read-optional-file-text (multipart-field-path work-dir "media_type")))
                             (mime-type-input (read-optional-file-text (multipart-field-path work-dir "mime_type")))
                             (edit-summary (read-optional-file-text (multipart-field-path work-dir "edit_summary")))
                             (upload-temp-path (string-append work-dir "/upload.bin"))
                             (upload-filename (read-optional-file-text (string-append work-dir "/upload_filename.txt")))
                             (upload-content-type (read-optional-file-text (string-append work-dir "/upload_content_type.txt"))))
                        (if (or (blank-text-p csrf-token)
                                (blank-text-p (current-csrf-token))
                                (not (string= csrf-token (current-csrf-token))))
                            (render-forbidden "invalid csrf_token")
                            (if (or (blank-text-p upload-filename)
                                    (null (probe-file upload-temp-path)))
                                (render-bad-request "upload_file must be provided")
                                (let* ((safe-name (sanitize-filename (basename upload-filename)))
                                       (media-type
                                        (if (blank-text-p media-type-input)
                                            (infer-media-type safe-name)
                                            media-type-input))
                                       (declared-mime
                                        (if (blank-text-p mime-type-input)
                                            upload-content-type
                                            mime-type-input))
                                       (validation
                                        (validate-media-upload upload-temp-path upload-filename declared-mime))
                                       (stored-name
                                        (string-append
                                         "upload-"
                                         (format nil "~A" (get-universal-time))
                                         "-"
                                         (format nil "~A" *markdown-temp-counter*)
                                         "-"
                                         safe-name)))
                                  (if (not (valid-media-type-p media-type))
                                      (render-bad-request "media_type must be image/video/audio/file")
                                      (if (not (string= (first validation) "ok"))
                                          (render-bad-request (second validation))
                                          (let* ((mime-type (second validation))
                                                 (storage-path (move-upload-into-media-dir upload-temp-path stored-name))
                                                 (public-url (media-delivery-url stored-name))
                                                 (err
                                                  (if (blank-text-p storage-path)
                                                      "failed to store uploaded file"
                                                      (persist-media-record
                                                       db
                                                       page-slug
                                                       media-type
                                                       title
                                                       upload-filename
                                                       stored-name
                                                       mime-type
                                                       storage-path
                                                       public-url
                                                       edit-summary))))
                                            (if (string= err "")
                                                (render-see-other (string-append (app-base) "/media"))
                                                (progn
                                                  (if (blank-text-p storage-path)
                                                      nil
                                                      (safe-delete-file storage-path))
                                                  (render-bad-request err))))))))))))))))))

(defun render-admin-home ()
  (let ((base (app-base)))
    (print-headers-ok)
    (print-layout-head "Admin")
    (format t "<h1>Admin</h1>~%")
    (format t "<ul>~%")
    (format t "<li><a href=\"~A/admin/backup\">Backup</a></li>~%" base)
    (format t "<li><a href=\"~A/admin/restore\">Restore</a></li>~%" base)
    (format t "<li><a href=\"~A/admin/stats\">Stats</a></li>~%" base)
    (format t "<li><a href=\"~A/admin/audit\">Audit Log</a></li>~%" base)
    (format t "<li><a href=\"~A/admin/deleted-pages\">Deleted Pages</a></li>~%" base)
    (format t "<li><a href=\"~A/admin/deleted-media\">Deleted Media</a></li>~%" base)
    (format t "</ul>~%")
    (print-layout-foot)))

(defun render-healthz (db)
  (let ((db-ok (not (null db))))
    (print-headers-json-ok)
    (format t "{\"status\":\"~A\",\"request_id\":\"~A\",\"db\":\"~A\",\"media_dir\":\"~A\",\"backup_dir\":\"~A\"}~%"
            (if db-ok "ok" "error")
            (json-escape (request-id))
            (if db-ok "ok" "error")
            (json-escape (media-root-dir))
            (json-escape (backup-root-dir)))))

(defun render-admin-stats (db)
  (let ((stats (fetch-backup-stats db)))
    (print-headers-json-ok)
    (format t "{\"request_id\":\"~A\",\"pages_active\":\"~A\",\"pages_deleted\":\"~A\",\"media_active\":\"~A\",\"media_deleted\":\"~A\",\"backup_runs\":\"~A\",\"last_backup_at\":\"~A\"}~%"
            (json-escape (request-id))
            (json-escape (first stats))
            (json-escape (second stats))
            (json-escape (third stats))
            (json-escape (fourth stats))
            (json-escape (fifth stats))
            (json-escape (sixth stats)))))

(defun render-admin-audit (db)
  (let ((rows (fetch-audit-logs db)))
    (print-headers-ok)
    (print-layout-head "Audit Log")
    (format t "<h1>Audit Log</h1>~%")
    (format t "<p><small>Latest 100 events</small></p>~%")
    (if (null rows)
        (format t "<p>監査ログはまだありません。</p>~%")
        (progn
          (format t "<table style=\"width:100%;border-collapse:collapse\">~%")
          (format t "<thead><tr><th style=\"text-align:left;border-bottom:1px solid #ddd;padding:.4rem\">When</th><th style=\"text-align:left;border-bottom:1px solid #ddd;padding:.4rem\">Actor</th><th style=\"text-align:left;border-bottom:1px solid #ddd;padding:.4rem\">Action</th><th style=\"text-align:left;border-bottom:1px solid #ddd;padding:.4rem\">Target</th><th style=\"text-align:left;border-bottom:1px solid #ddd;padding:.4rem\">Meta</th></tr></thead>~%")
          (format t "<tbody>~%")
          (dolist (row rows)
            (let ((created-at (second row))
                  (actor (third row))
                  (action (fourth row))
                  (target-type (fifth row))
                  (target-id (sixth row))
                  (meta-json (seventh row)))
              (format t "<tr>~%")
              (format t "<td style=\"vertical-align:top;border-bottom:1px solid #eee;padding:.4rem\">~A</td>~%" (html-escape created-at))
              (format t "<td style=\"vertical-align:top;border-bottom:1px solid #eee;padding:.4rem\">~A</td>~%" (html-escape actor))
              (format t "<td style=\"vertical-align:top;border-bottom:1px solid #eee;padding:.4rem\"><code>~A</code></td>~%" (html-escape action))
              (format t "<td style=\"vertical-align:top;border-bottom:1px solid #eee;padding:.4rem\"><code>~A</code><br><small>~A</small></td>~%" (html-escape target-type) (html-escape target-id))
              (format t "<td style=\"vertical-align:top;border-bottom:1px solid #eee;padding:.4rem\"><pre style=\"margin:0\">~A</pre></td>~%" (html-escape meta-json))
              (format t "</tr>~%")))
          (format t "</tbody></table>~%")))
    (print-layout-foot)))

(defun render-admin-backup-form (db)
  (let ((base (app-base))
        (rows (fetch-backup-runs db)))
    (print-headers-ok)
    (print-layout-head "Backup")
    (format t "<h1>Backup</h1>~%")
    (format t "<p>DB(SQL) とメディア(tar.gz)をバックアップします。</p>~%")
    (format t "<form method=\"post\" action=\"~A/admin/backup\">~%" base)
    (render-csrf-hidden-input)
    (format t "<p><label>Backup Dir<br><input type=\"text\" name=\"backup_dir\" value=\"~A\" style=\"width:100%\"></label></p>~%" (html-escape (backup-root-dir)))
    (format t "<p><label>Keep Count<br><input type=\"number\" name=\"keep_count\" value=\"~A\" min=\"1\" style=\"width:10rem\"></label></p>~%" (html-escape (format nil "~A" (backup-keep-count))))
    (format t "<p><label><input type=\"checkbox\" name=\"dry_run\" value=\"1\"> Dry run only (do not create files)</label></p>~%")
    (format t "<p><label>Confirmation Phrase<br><input type=\"text\" name=\"confirm_phrase\" value=\"\" style=\"width:100%\" placeholder=\"~A\"></label></p>~%" (html-escape (backup-confirm-phrase)))
    (format t "<p><small>Type <code>~A</code> to confirm this backup.</small></p>~%" (html-escape (backup-confirm-phrase)))
    (format t "<p><button type=\"submit\">Run Backup</button></p>~%")
    (format t "</form>~%")
    (format t "<p><small>default backup dir: <code>~A</code> / keep count: <code>~A</code></small></p>~%"
            (html-escape (backup-root-dir))
            (html-escape (format nil "~A" (backup-keep-count))))
    (format t "<h2>Recent Backup History</h2>~%")
    (render-backup-runs-table rows)
    (print-layout-foot)))

(defun render-admin-backup-run ()
  (let ((raw-body (read-form-body-or-render t)))
    (if (null raw-body)
        nil
        (let* ((confirm-phrase (parse-form-field raw-body "confirm_phrase"))
               (backup-dir (parse-form-field raw-body "backup_dir"))
               (keep-count-raw (parse-form-field raw-body "keep_count"))
               (dry-run (string= (parse-form-field raw-body "dry_run") "1"))
               (keep-count (if (blank-text-p keep-count-raw) (backup-keep-count) (parse-int-safe keep-count-raw))))
          (if (not (string= confirm-phrase (backup-confirm-phrase)))
              (render-bad-request (string-append "confirm_phrase must be " (backup-confirm-phrase)))
              (if (or (null keep-count) (<= keep-count 0))
                  (render-bad-request "keep_count must be a positive integer")
                  (let ((result (run-backup-with-audit db backup-dir keep-count dry-run)))
                    (write-structured-log
                     (if (or (string= (first result) "ok") (string= (first result) "dry-run")) "info" "error")
                     "backup.run"
                     (make-log-fields-3
                      "status" (first result)
                      "detail" (if (null (fourth result)) "" (fourth result))
                      "backup_dir" (if (blank-text-p backup-dir) (backup-root-dir) backup-dir)))
                    (print-headers-ok)
                    (print-layout-head "Backup Result")
                    (if (or (string= (first result) "ok") (string= (first result) "dry-run"))
                        (progn
                          (format t "<h1>~A</h1>~%" (if (string= (first result) "dry-run") "Backup dry-run" "Backup completed"))
                          (format t "<p>SQL: <code>~A</code></p>~%" (html-escape (second result)))
                          (format t "<p>Media: <code>~A</code></p>~%" (html-escape (third result)))
                          (format t "<p><small>keep_count: <code>~A</code> / dry_run: <code>~A</code></small></p>~%"
                                  (html-escape (format nil "~A" keep-count))
                                  (html-escape (if dry-run "true" "false"))))
                        (progn
                          (format t "<h1>Backup failed</h1>~%")
                          (format t "<p><code>~A</code></p>~%" (html-escape (fourth result)))))
                    (print-layout-foot))))))))

(defun render-admin-restore-form ()
  (let ((base (app-base)))
    (print-headers-ok)
    (print-layout-head "Restore")
    (format t "<h1>Restore</h1>~%")
    (format t "<p>バックアップファイルの絶対パスを指定します。</p>~%")
    (format t "<form method=\"post\" action=\"~A/admin/restore\">~%" base)
    (render-csrf-hidden-input)
    (format t "  <p><label>SQL Path<br><input type=\"text\" name=\"sql_path\" value=\"\" style=\"width:100%\" placeholder=\"~A/wiki-backup-XXXX.sql\"></label></p>~%" (html-escape (backup-root-dir)))
    (format t "  <p><label>Media Archive Path (optional)<br><input type=\"text\" name=\"media_archive_path\" value=\"\" style=\"width:100%\" placeholder=\"~A/wiki-backup-XXXX-media.tar.gz\"></label></p>~%" (html-escape (backup-root-dir)))
    (format t "  <p><label><input type=\"checkbox\" name=\"restore_media\" value=\"1\"> Restore media archive too</label></p>~%")
    (format t "  <p><label>Confirmation Phrase<br><input type=\"text\" name=\"confirm_phrase\" value=\"\" style=\"width:100%\" placeholder=\"~A\"></label></p>~%" (html-escape (restore-confirm-phrase)))
    (format t "  <p><small>Type <code>~A</code> to confirm restore.</small></p>~%" (html-escape (restore-confirm-phrase)))
    (format t "  <p><button type=\"submit\">Run Restore</button></p>~%")
    (format t "</form>~%")
    (format t "<h2>Validation Commands</h2>~%")
    (format t "<pre>ls -lh ~A\nls -lh ~A\ncat ~A/catalog.lspdata</pre>~%"
            (html-escape (backup-root-dir))
            (html-escape (db-root))
            (html-escape (db-root)))
    (format t "<p><small>See <code>examples/wiki/docs/restore-runbook.md</code> for the full restore checklist.</small></p>~%")
    (print-layout-foot)))

(defun render-admin-restore-run ()
  (let ((raw-body (read-form-body-or-render t)))
    (if (null raw-body)
        nil
        (let* ((sql-path (parse-form-field raw-body "sql_path"))
               (media-path (parse-form-field raw-body "media_archive_path"))
               (restore-media (string= (parse-form-field raw-body "restore_media") "1"))
               (confirm-phrase (parse-form-field raw-body "confirm_phrase")))
          (if (not (string= confirm-phrase (restore-confirm-phrase)))
              (render-bad-request (string-append "confirm_phrase must be " (restore-confirm-phrase)))
              (let ((result (run-restore-with-audit db sql-path media-path restore-media)))
                (write-structured-log
                 (if (string= (first result) "ok") "info" "error")
                 "restore.run"
                 (make-log-fields-3
                  "status" (first result)
                  "sql_path" sql-path
                  "restore_media" (if restore-media "true" "false")))
                (print-headers-ok)
                (print-layout-head "Restore Result")
                (if (string= (first result) "ok")
                    (progn
                      (format t "<h1>Restore completed</h1>~%")
                      (format t "<p><code>~A</code></p>~%" (html-escape (second result))))
                    (progn
                      (format t "<h1>Restore failed</h1>~%")
                      (format t "<p><code>~A</code></p>~%" (html-escape (second result)))))
                (print-layout-foot)))))))

(defun create-media (db)
  (let ((content-type (request-content-type)))
    (if (starts-with content-type "multipart/form-data")
        (create-media-from-multipart db)
        (if (starts-with content-type "application/x-www-form-urlencoded")
            (let ((raw-body (read-form-body-or-render t)))
              (if (null raw-body)
                  nil
                  (create-media-from-source-path db raw-body)))
            (render-bad-request "CONTENT_TYPE must be multipart/form-data")))))

(defun delete-media (db media-id)
  (let ((raw-body (read-form-body-or-render t)))
    (if (null raw-body)
        nil
        (let ((mid (parse-int-safe media-id)))
          (if (null mid)
              (render-bad-request "invalid media id")
              (let ((media (fetch-media-by-id db media-id)))
          (if (null media)
              (render-not-found)
              (let ((stored-filename (fourth media))
                    (storage-path (sixth media))
                    (page-slug (seventh media)))
                (db-exec!
                 db
                 (string-append
                  "UPDATE media_assets SET deleted_at=" (db-text-sql (db-now-text))
                  ", deleted_by=" (db-text-sql (current-username))
                  " WHERE id=" (format nil "~A" mid) ";"))
                (write-audit-log
                 db
                 "media.delete"
                 "media"
                 media-id
                 (make-audit-meta-3
                  "stored_filename" stored-filename
                  "storage_path" storage-path
                  "page_slug" page-slug))
                (render-see-other (string-append (app-base) "/media"))))))))))

(defun delete-page (db slug)
  (let ((raw-body (read-form-body-or-render t)))
    (if (null raw-body)
        nil
        (let ((page (fetch-page db slug)))
          (if (null page)
              (render-not-found)
              (let ((title (second page)))
                (db-exec!
                 db
                 (string-append
                  "UPDATE pages SET deleted_at=" (db-text-sql (db-now-text))
                  ", deleted_by=" (db-text-sql (current-username))
                  " WHERE slug=" (db-text-sql slug) ";"))
                (write-audit-log
                 db
                 "page.delete"
                 "page"
                 slug
                 (make-audit-meta-2
                  "title" title
                  "deleted_by" (current-username)))
                (render-see-other (app-base))))))))

(defun restore-page (db slug)
  (let ((raw-body (read-form-body-or-render t)))
    (if (null raw-body)
        nil
        (let ((page (fetch-page-any db slug)))
          (if (or (null page) (string= (fifth page) ""))
              (render-not-found)
              (let ((title (second page)))
                (db-exec!
                 db
                 (string-append
                  "UPDATE pages SET deleted_at = NULL, deleted_by = NULL WHERE slug="
                  (db-text-sql slug) ";"))
                (write-audit-log
                 db
                 "page.restore"
                 "page"
                 slug
                 (make-audit-meta-2
                  "title" title
                  "restored_by" (current-username)))
                (render-see-other (string-append (app-base) "/admin/deleted-pages"))))))))

(defun restore-media (db media-id)
  (let ((raw-body (read-form-body-or-render t)))
    (if (null raw-body)
        nil
        (let ((mid (parse-int-safe media-id))
              (media (fetch-media-by-id-any db media-id)))
          (if (or (null mid) (null media) (string= (eighth media) ""))
              (render-not-found)
              (let ((stored-filename (fourth media)))
                (db-exec!
                 db
                 (string-append
                  "UPDATE media_assets SET deleted_at = NULL, deleted_by = NULL WHERE id="
                  (format nil "~A" mid) ";"))
                (write-audit-log
                 db
                 "media.restore"
                 "media"
                 media-id
                 (make-audit-meta-2
                  "stored_filename" stored-filename
                  "restored_by" (current-username)))
                (render-see-other (string-append (app-base) "/admin/deleted-media"))))))))

(defun render-new ()
  (let ((base (app-base)))
    (print-headers-ok)
    (print-layout-head "Create New Page")
    (format t "<h1>Create New Page</h1>~%")
    (format t "<form method=\"post\" action=\"~A/new\">~%" base)
    (render-csrf-hidden-input)
    (format t "  <p><label>Slug<br><input type=\"text\" name=\"slug\" value=\"\" style=\"width:100%\" placeholder=\"new-page\"></label></p>~%")
    (format t "  <p><small>slug: a-z, 0-9, '-' のみ。先頭末尾 '-' 不可。</small></p>~%")
    (format t "  <p><label>Title<br><input type=\"text\" name=\"title\" value=\"\" style=\"width:100%\"></label></p>~%")
    (format t "  <p><label>Body (Markdown)<br><textarea name=\"body_md\"></textarea></label></p>~%")
    (format t "  <p><label>Status<br><select name=\"status\"><option value=\"published\">published</option><option value=\"draft\">draft</option><option value=\"private\">private</option></select></label></p>~%")
    (format t "  <p><label>Tags (comma separated)<br><input type=\"text\" name=\"tags\" value=\"\" style=\"width:100%\" placeholder=\"guide, onboarding\"></label></p>~%")
    (format t "  <p><label>Edit Summary<br><input type=\"text\" name=\"edit_summary\" value=\"create page\" style=\"width:100%\"></label></p>~%")
    (format t "  <p><button type=\"submit\">Create</button></p>~%")
    (format t "</form>~%")
    (print-layout-foot)))

(defun render-edit (db slug)
  (if (not (slug-safe-p slug))
      (render-not-found)
      (let ((row (fetch-page db slug)))
        (if (null row)
            (render-not-found)
	            (let ((title (second row))
	                  (body-md (third row))
                      (page-status (page-status row))
                      (tags-text (fetch-page-tags-text db slug)))
	              (print-headers-ok)
	              (print-layout-head (string-append "Edit: " title))
              (format t "<h1>Edit: ~A</h1>~%" (html-escape title))
	              (format t "<form method=\"post\" action=\"~A/~A/edit\">~%"
	                      (app-base)
	                      (html-escape slug))
	              (render-csrf-hidden-input)
	              (format t "  <input type=\"hidden\" name=\"current_rev_no\" value=\"~A\">~%" (html-escape (page-current-rev-no row)))
	              (format t "  <p><label>Title<br><input type=\"text\" name=\"title\" value=\"~A\" style=\"width:100%\"></label></p>~%"
	                      (html-escape title))
              (format t "  <p><label>Body (Markdown)<br><textarea name=\"body_md\">~A</textarea></label></p>~%"
                      (html-escape body-md))
              (format t "  <p><label>Status<br><select name=\"status\"><option value=\"published\"~A>published</option><option value=\"draft\"~A>draft</option><option value=\"private\"~A>private</option></select></label></p>~%"
                      (if (string= page-status "published") " selected" "")
                      (if (string= page-status "draft") " selected" "")
                      (if (string= page-status "private") " selected" ""))
              (format t "  <p><label>Tags (comma separated)<br><input type=\"text\" name=\"tags\" value=\"~A\" style=\"width:100%\"></label></p>~%"
                      (html-escape tags-text))
              (format t "  <p><label>Edit Summary<br><input type=\"text\" name=\"edit_summary\" value=\"\" style=\"width:100%\"></label></p>~%")
              (format t "  <p><button type=\"submit\">Save</button></p>~%")
              (format t "</form>~%")
              (format t "<form method=\"post\" action=\"~A/~A/delete\" style=\"margin-top:1rem\">~%"
                      (app-base)
                      (html-escape slug))
              (render-csrf-hidden-input)
              (format t "<button type=\"submit\" onclick=\"return confirm('Delete page ~A?');\">Delete Page</button>~%"
                      (html-escape slug))
              (format t "</form>~%")
              (print-layout-foot))))))

(defun render-app ()
  (let* ((method (request-method))
         (raw-path (env-or-empty "PATH_INFO"))
         (segments (path-segments))
         (n (length segments))
         (db '()))
    (debug-stderr (format nil "render-app begin method=~A path=~A" method raw-path))
    (clear-response-extra-headers)
    (init-request-id)
    (setq db (handler-case
               (db-open)
               (error (e) '())))
    (debug-stderr (format nil "render-app db=~A" (if (null db) "null" "ok")))
    (if (null db)
        (if (and (= n 1) (string= (first segments) "healthz"))
            (render-healthz '())
            (error "database unavailable"))
        (progn
          (debug-stderr "load-current-user begin")
          (load-current-user db)
          (debug-stderr "load-current-user ok")
          (if (and (string= method "GET") (needs-canonical-redirect-p raw-path))
              (render-redirect (string-append (app-base) (canonical-path-info raw-path)))
              (if (ensure-authorized method segments)
                  (cond
                   ((= n 0)
                    (debug-stderr "render-index begin")
                    (render-index db))
                   ((= n 1)
                    (cond
                     ((string= (first segments) "new")
                      (if (string= method "POST")
                          (create-page db)
                          (render-new)))
                     ((string= (first segments) "media")
                      (render-media-index db))
                     ((string= (first segments) "search")
                      (render-search db))
                     ((string= (first segments) "healthz")
                      (render-healthz db))
                     ((string= (first segments) "admin")
                      (render-admin-home))
                     ((string= (first segments) "login")
                      (if (string= method "POST")
                          (handle-login db)
                          (render-login-form "" "")))
                     ((string= (first segments) "logout")
                      (handle-logout db))
                     (t
                      (render-view db (first segments)))))
                   ((= n 2)
                    (cond
                     ((string= (first segments) "admin")
                      (cond
                       ((string= (second segments) "audit")
                        (render-admin-audit db))
                       ((string= (second segments) "stats")
                        (render-admin-stats db))
                       ((string= (second segments) "deleted-pages")
                        (render-admin-deleted-pages db))
                       ((string= (second segments) "deleted-media")
                        (render-admin-deleted-media db))
                       ((string= (second segments) "backup")
                        (if (string= method "POST")
                            (render-admin-backup-run)
                            (render-admin-backup-form db)))
                       ((string= (second segments) "restore")
                        (if (string= method "POST")
                            (render-admin-restore-run)
                            (render-admin-restore-form)))
                       (t
                        (render-not-found))))
                     ((string= (first segments) "media")
                      (if (string= (second segments) "new")
                          (if (string= method "POST")
                              (create-media db)
                              (render-media-new db))
                          (render-not-found)))
                     ((string= (first segments) "files")
                      (render-media-file db (second segments)))
                     ((string= (second segments) "history")
                      (render-history db (first segments)))
                     ((string= (second segments) "delete")
                      (if (string= method "POST")
                          (delete-page db (first segments))
                          (render-not-found)))
                     ((string= (second segments) "edit")
                      (if (string= method "POST")
                          (save-page db (first segments))
                          (render-edit db (first segments))))
                     (t
                      (render-not-found))))
                   ((= n 3)
                    (cond
                     ((and (string= (first segments) "media")
                           (string= (third segments) "delete"))
                      (if (string= method "POST")
                          (delete-media db (second segments))
                          (render-not-found)))
                     ((and (string= (second segments) "revisions")
                           (not (blank-text-p (third segments))))
                      (render-revision-view db (first segments) (third segments)))
                     (t
                      (render-not-found))))
                   ((= n 4)
                    (cond
                     ((and (string= (first segments) "admin")
                           (string= (second segments) "deleted-pages")
                           (string= (fourth segments) "restore"))
                      (if (string= method "POST")
                          (restore-page db (third segments))
                          (render-not-found)))
                     ((and (string= (first segments) "admin")
                           (string= (second segments) "deleted-media")
                           (string= (fourth segments) "restore"))
                      (if (string= method "POST")
                          (restore-media db (third segments))
                          (render-not-found)))
                     ((and (string= (second segments) "compare")
                           (not (blank-text-p (third segments)))
                           (not (blank-text-p (fourth segments))))
                      (render-compare-view db (first segments) (third segments) (fourth segments)))
                     ((and (string= (second segments) "revisions")
                           (string= (fourth segments) "rollback")
                           (not (blank-text-p (third segments))))
                      (if (string= method "POST")
                          (rollback-page-to-revision db (first segments) (third segments))
                          (render-not-found)))
                     (t
                      (render-not-found))))
                   (t
                    (render-not-found)))
                  nil))
          nil))))

(handler-case
  (render-app)
  (error (e)
    (if (string= (request-id) "")
        (init-request-id)
        nil)
    (write-error-log (format nil "~A" e))
    (render-error (format nil "~A" e))))
