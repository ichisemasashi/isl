;; ISL Wiki MVP routes:
;;   /wiki              -> index
;;   /wiki/<slug>       -> view
;;   /wiki/<slug>/edit  -> edit form (save is not implemented yet)

(defun env-or-empty (name)
  (let ((v (getenv name)))
    (if (null v) "" v)))

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
      (string= slug "media")))

(defun media-root-dir ()
  (let ((v (getenv "ISL_WIKI_MEDIA_DIR")))
    (if (null v)
        "/Volumes/SSD-PLU3/work/LISP/islisp/isl/examples/wiki/storage/media"
        v)))

(defun media-public-base ()
  (let ((v (getenv "ISL_WIKI_MEDIA_BASE_URL")))
    (if (null v)
        "/cgi-bin/wiki.cgi/files"
        v)))

(defun media-delivery-url (stored-filename)
  (string-append (media-public-base) "/" stored-filename))

(defun ensure-media-dir ()
  (system (string-append "mkdir -p " (shell-quote (media-root-dir))))
  (system (string-append "chmod 755 " (shell-quote (media-root-dir)))))

(defun contains-char-p (s ch)
  (not (null (string-index ch s))))

(defun shell-quote (s)
  (if (contains-char-p s "'")
      (error "single quote is not supported in file path" s)
      (string-append "'" s "'")))

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

(defun valid-media-type-p (media-type)
  (or (string= media-type "image")
      (string= media-type "video")
      (string= media-type "audio")))

(defun infer-media-type (filename)
  (let ((ext (file-ext-lower filename)))
    (if (or (string= ext "png") (string= ext "jpg") (string= ext "jpeg") (string= ext "gif") (string= ext "webp") (string= ext "svg"))
        "image"
        (if (or (string= ext "mp4") (string= ext "webm") (string= ext "mov") (string= ext "mkv"))
            "video"
            (if (or (string= ext "mp3") (string= ext "wav") (string= ext "ogg") (string= ext "m4a") (string= ext "flac"))
                "audio"
                "image")))))

(defun infer-mime-type (filename media-type)
  (let ((ext (file-ext-lower filename)))
    (cond
     ((string= ext "png") "image/png")
     ((or (string= ext "jpg") (string= ext "jpeg")) "image/jpeg")
     ((string= ext "gif") "image/gif")
     ((string= ext "webp") "image/webp")
     ((string= ext "svg") "image/svg+xml")
     ((string= ext "mp4") "video/mp4")
     ((string= ext "webm") "video/webm")
     ((string= ext "mov") "video/quicktime")
     ((string= ext "mkv") "video/x-matroska")
     ((string= ext "mp3") "audio/mpeg")
     ((string= ext "wav") "audio/wav")
     ((string= ext "ogg") "audio/ogg")
     ((string= ext "m4a") "audio/mp4")
     ((string= ext "flac") "audio/flac")
     (t
      (if (string= media-type "video")
          "video/mp4"
          (if (string= media-type "audio")
              "audio/mpeg"
              "image/png"))))))

(defun db-url ()
  (let ((v (getenv "ISL_WIKI_DB_URL")))
    (if (null v)
        "postgresql://127.0.0.1:5432/isl_wiki"
        v)))

(defglobal *markdown-temp-counter* 0)

(defun pandoc-bin ()
  (let ((v (getenv "ISL_WIKI_PANDOC")))
    (if (null v)
        "/opt/homebrew/bin/pandoc"
        v)))

(defun next-temp-base ()
  (setq *markdown-temp-counter* (+ *markdown-temp-counter* 1))
  (string-append "/tmp/isl-wiki-"
                 (format nil "~A" (get-universal-time))
                 "-"
                 (format nil "~A" *markdown-temp-counter*)))

(defun write-file-text (path text)
  (with-open-file (s path
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists :supersede)
    (format s "~A" (safe-text text))))

(defun read-file-text (path)
  (with-open-file (s path :direction :input)
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

(defun safe-delete-file (path)
  (handler-case
    (delete-file path)
    (error (e) '())))

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
  (let* ((base (next-temp-base))
         (md-path (string-append base ".md"))
         (html-path (string-append base ".html"))
         (cmd (string-append (pandoc-bin)
                             " --from=gfm-raw_html --to=html5"
                             " --output " html-path
                             " " md-path)))
    (write-file-text md-path body-md)
    (let ((status (system cmd)))
      (if (= status 0)
          (let ((html (read-file-text html-path)))
            (safe-delete-file md-path)
            (safe-delete-file html-path)
            html)
          (progn
            (safe-delete-file md-path)
            (safe-delete-file html-path)
            (error "pandoc conversion failed" cmd status))))))

(defun fetch-pages (db)
  (postgres-query db
    "select slug, title, to_char(updated_at, 'YYYY-MM-DD HH24:MI:SS') from pages order by slug"))

(defun fetch-page (db slug)
  (let* ((sql (string-append
                "select slug, title, body_md "
                "from pages where slug='" (sql-escape slug) "' limit 1"))
         (rows (postgres-query db sql)))
    (if (null rows)
        '()
        (first rows))))

(defun fetch-page-updated-at (db slug)
  (postgres-query-one
   db
   (string-append
    "select to_char(updated_at, 'YYYY-MM-DD HH24:MI:SS') "
    "from pages where slug='" (sql-escape slug) "' limit 1")))

(defun fetch-latest-edit-summary (db slug)
  (postgres-query-one
   db
   (string-append
    "select coalesce(edit_summary, '') "
    "from page_revisions r "
    "join pages p on p.id = r.page_id "
    "where p.slug='" (sql-escape slug) "' "
    "order by r.rev_no desc limit 1")))

(defun fetch-media-list (db)
  (postgres-query
   db
   (string-append
    "select m.id, m.media_type, coalesce(m.title, ''), m.stored_filename, m.mime_type, "
    "coalesce((select p.slug from pages p where p.id = m.page_id), '') "
    "from media_assets m order by m.id desc")))

(defun fetch-media-for-page (db slug)
  (postgres-query
   db
   (string-append
    "select m.media_type, coalesce(m.title, ''), m.stored_filename, m.mime_type "
    "from media_assets m "
    "join pages p on p.id = m.page_id "
    "where p.slug='" (sql-escape slug) "' "
    "order by m.id desc")))

(defun fetch-media-file-meta (db stored-filename)
  (let ((rows (postgres-query
               db
               (string-append
                "select storage_path, mime_type from media_assets "
                "where stored_filename='" (sql-escape stored-filename) "' limit 1"))))
    (if (null rows) '() (first rows))))

(defun print-headers-ok ()
  (format t "Content-Type: text/html; charset=UTF-8~%~%"))

(defun print-headers-404 ()
  (format t "Status: 404 Not Found~%")
  (format t "Content-Type: text/html; charset=UTF-8~%~%"))

(defun print-headers-400 ()
  (format t "Status: 400 Bad Request~%")
  (format t "Content-Type: text/html; charset=UTF-8~%~%"))

(defun print-headers-500 ()
  (format t "Status: 500 Internal Server Error~%")
  (format t "Content-Type: text/html; charset=UTF-8~%~%"))

(defun print-headers-301 (location)
  (format t "Status: 301 Moved Permanently~%")
  (format t "Location: ~A~%" location)
  (format t "Content-Type: text/html; charset=UTF-8~%~%"))

(defun print-headers-303 (location)
  (format t "Status: 303 See Other~%")
  (format t "Location: ~A~%" location)
  (format t "Content-Type: text/html; charset=UTF-8~%~%"))

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
  (format t "<p><a href=\"~A\">Wiki Index</a> | <a href=\"~A/new\">New Page</a> | <a href=\"~A/media\">Media</a></p>~%"
          (app-base) (app-base) (app-base)))

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

(defun render-error (message)
  (print-headers-500)
  (print-layout-head "Internal Server Error")
  (format t "<h1>500 Internal Server Error</h1>~%")
  (format t "<p>アプリケーション内部でエラーが発生しました。</p>~%")
  (format t "<pre>~A</pre>~%" (html-escape message))
  (print-layout-foot))

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

(defun render-index (db)
  (let ((rows (fetch-pages db))
        (base (app-base)))
    (print-headers-ok)
    (print-layout-head "Wiki Index")
    (format t "<h1>Wiki Pages</h1>~%")
    (format t "<p><a href=\"~A/new\">Create New Page</a> | <a href=\"~A/media/new\">Add Media</a></p>~%" base base)
    (if (null rows)
        (format t "<p>ページがありません。</p>~%")
        (progn
          (format t "<ul>~%")
          (dolist (row rows)
            (let ((slug (first row))
                  (title (second row))
                  (updated-at (third row)))
              (format t "<li><a href=\"~A/~A\">~A</a> <small>(~A)</small></li>~%"
                      base
                      (html-escape slug)
                      (html-escape title)
                      (html-escape updated-at))))
          (format t "</ul>~%")))
    (print-layout-foot)))

(defun render-view (db slug)
  (if (not (slug-safe-p slug))
      (render-not-found)
      (let ((row (fetch-page db slug))
            (base (app-base)))
        (if (null row)
            (render-not-found)
            (let ((title (second row))
                  (body-md (third row))
                  (updated-at (fetch-page-updated-at db slug))
                  (latest-summary (fetch-latest-edit-summary db slug))
                  (media-rows (fetch-media-for-page db slug)))
              (let ((body-html (markdown->html body-md)))
              (print-headers-ok)
              (print-layout-head title)
              (format t "<h1>~A</h1>~%" (html-escape title))
              (format t "<p><a href=\"~A/~A/edit\">Edit this page</a></p>~%"
                      base
                      (html-escape slug))
              (format t "<p><a href=\"~A/new\">Create New Page</a> | <a href=\"~A/media/new\">Add Media</a></p>~%" base base)
              (format t "<p><small>updated: ~A / slug: <code>~A</code></small></p>~%"
                      (html-escape updated-at)
                      (html-escape slug))
              (if (string= latest-summary "")
                  nil
                  (format t "<p><small>latest summary: ~A</small></p>~%" (html-escape latest-summary)))
              (format t "<h2>Preview (HTML)</h2>~%")
              (format t "<article class=\"wiki-body\">~A</article>~%" body-html)
              (format t "<h2>Markdown Source</h2>~%")
              (format t "<pre>~A</pre>~%" (html-escape body-md))
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
                        (if (string= media-type "image")
                            (format t "<img src=\"~A\" alt=\"~A\" style=\"max-width:100%;height:auto;border:1px solid #ddd;border-radius:6px\">~%"
                                    (html-escape media-url)
                                    (html-escape media-title))
                            (if (string= media-type "video")
                                (format t "<video controls style=\"max-width:100%\" src=\"~A\"></video>~%"
                                        (html-escape media-url))
                                (format t "<audio controls src=\"~A\"></audio>~%"
                                        (html-escape media-url))))
                        (format t "<p><small><a href=\"~A\">~A</a> (~A)</small></p>~%"
                                (html-escape media-url)
                                (html-escape media-url)
                                (html-escape media-mime))
                        (format t "</div>~%"))))))
              (print-layout-foot)))))))

(defun save-page (db slug)
  (if (or (not (slug-safe-p slug)) (reserved-slug-p slug))
      (render-not-found)
      (let ((page (fetch-page db slug)))
        (if (null page)
            (render-not-found)
            (let ((content-type (env-or-empty "CONTENT_TYPE")))
              (if (not (starts-with content-type "application/x-www-form-urlencoded"))
                  (render-bad-request "CONTENT_TYPE must be application/x-www-form-urlencoded")
                  (let* ((raw-body (read-request-body))
                         (title (parse-form-field raw-body "title"))
                         (body-md (parse-form-field raw-body "body_md"))
                         (edit-summary (parse-form-field raw-body "edit_summary")))
                    (if (blank-text-p title)
                        (render-bad-request "title must not be blank")
                        (let ((sql
                               (string-append
                                "with target as ("
                                "  select p.id as page_id,"
                                "         coalesce((select max(r.rev_no) from page_revisions r where r.page_id = p.id), 0) + 1 as next_rev"
                                "    from pages p"
                                "   where p.slug = '" (sql-escape slug) "'"
                                "), updated as ("
                                "  update pages p"
                                "     set title = '" (sql-escape title) "',"
                                "         body_md = '" (sql-escape body-md) "',"
                                "         last_edited_by = 'web'"
                                "    from target t"
                                "   where p.id = t.page_id"
                                "  returning p.id, t.next_rev"
                                ") "
                                "insert into page_revisions (page_id, rev_no, title, body_md, edited_by, edit_summary) "
                                "select u.id, u.next_rev, '"
                                (sql-escape title)
                                "', '"
                                (sql-escape body-md)
                                "', 'web', '"
                                (sql-escape edit-summary)
                                "' "
                                "from updated u")))
                          (postgres-exec db sql)
                          (render-see-other (string-append (app-base) "/" slug)))))))))))

(defun create-page (db)
  (let ((content-type (env-or-empty "CONTENT_TYPE")))
    (if (not (starts-with content-type "application/x-www-form-urlencoded"))
        (render-bad-request "CONTENT_TYPE must be application/x-www-form-urlencoded")
        (let* ((raw-body (read-request-body))
               (slug (parse-form-field raw-body "slug"))
               (title (parse-form-field raw-body "title"))
               (body-md (parse-form-field raw-body "body_md"))
               (edit-summary (parse-form-field raw-body "edit_summary")))
          (if (or (not (slug-safe-p slug)) (reserved-slug-p slug))
              (render-bad-request "invalid slug")
              (if (blank-text-p title)
                  (render-bad-request "title must not be blank")
                  (if (null (fetch-page db slug))
                      (let ((sql
                             (string-append
                              "with inserted as ("
                              "  insert into pages (slug, title, body_md, last_edited_by) "
                              "  values ('" (sql-escape slug) "', '"
                              (sql-escape title) "', '"
                              (sql-escape body-md) "', 'web') "
                              "  returning id, title, body_md"
                              ") "
                              "insert into page_revisions (page_id, rev_no, title, body_md, edited_by, edit_summary) "
                              "select i.id, 1, i.title, i.body_md, 'web', '"
                              (sql-escape edit-summary)
                              "' from inserted i")))
                        (postgres-exec db sql)
                        (render-see-other (string-append (app-base) "/" slug)))
                      (render-bad-request "slug already exists"))))))))

(defun render-media-index (db)
  (let ((rows (fetch-media-list db))
        (base (app-base)))
    (print-headers-ok)
    (print-layout-head "Media Library")
    (format t "<h1>Media Library</h1>~%")
    (format t "<p><a href=\"~A/media/new\">Add Media</a></p>~%" base)
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
            (if (string= media-type "image")
                (format t "<img src=\"~A\" alt=\"~A\" style=\"max-width:100%;height:auto;border:1px solid #ddd;border-radius:6px\">~%"
                        (html-escape media-url)
                        (html-escape media-title))
                (if (string= media-type "video")
                    (format t "<video controls style=\"max-width:100%\" src=\"~A\"></video>~%"
                            (html-escape media-url))
                    (format t "<audio controls src=\"~A\"></audio>~%"
                            (html-escape media-url))))
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
            (format t "</section>~%")))))
    (print-layout-foot)))

(defun render-file-not-found ()
  (format t "Status: 404 Not Found~%")
  (format t "Content-Type: text/plain; charset=UTF-8~%~%")
  (format t "Not Found~%"))

(defun print-media-headers-ok (mime-type)
  (format t "Status: 200 OK~%")
  (if (blank-text-p mime-type)
      (format t "Content-Type: application/octet-stream~%~%")
      (format t "Content-Type: ~A~%~%" mime-type)))

(defun render-media-file (db stored-filename)
  (let ((meta (fetch-media-file-meta db stored-filename)))
    (if (null meta)
        (render-file-not-found)
        (let ((storage-path (first meta))
              (mime-type (second meta)))
          (if (null (probe-file storage-path))
              (render-file-not-found)
              (progn
                (print-media-headers-ok mime-type)
                (system (string-append "cat " (shell-quote storage-path)))))))))

(defun render-media-new ()
  (let ((base (app-base)))
    (print-headers-ok)
    (print-layout-head "Add Media")
    (format t "<h1>Add Media</h1>~%")
    (format t "<p>サーバー上のファイルをストレージへコピーし、メタ情報をDBに保存します。</p>~%")
    (format t "<form method=\"post\" action=\"~A/media/new\">~%" base)
    (format t "  <p><label>Source Path (server local)<br><input type=\"text\" name=\"source_path\" value=\"\" style=\"width:100%\" placeholder=\"/path/to/file.png\"></label></p>~%")
    (format t "  <p><label>Title<br><input type=\"text\" name=\"title\" value=\"\" style=\"width:100%\"></label></p>~%")
    (format t "  <p><label>Page Slug (optional)<br><input type=\"text\" name=\"page_slug\" value=\"\" style=\"width:100%\" placeholder=\"home\"></label></p>~%")
    (format t "  <p><label>Media Type<br><select name=\"media_type\"><option value=\"\">auto</option><option value=\"image\">image</option><option value=\"video\">video</option><option value=\"audio\">audio</option></select></label></p>~%")
    (format t "  <p><label>MIME Type (optional)<br><input type=\"text\" name=\"mime_type\" value=\"\" style=\"width:100%\" placeholder=\"image/png\"></label></p>~%")
    (format t "  <p><label>Edit Summary<br><input type=\"text\" name=\"edit_summary\" value=\"add media\" style=\"width:100%\"></label></p>~%")
    (format t "  <p><button type=\"submit\">Add Media</button></p>~%")
    (format t "</form>~%")
    (print-layout-foot)))

(defun create-media (db)
  (let ((content-type (env-or-empty "CONTENT_TYPE")))
    (if (not (starts-with content-type "application/x-www-form-urlencoded"))
        (render-bad-request "CONTENT_TYPE must be application/x-www-form-urlencoded")
        (let* ((raw-body (read-request-body))
               (source-path (parse-form-field raw-body "source_path"))
               (title (parse-form-field raw-body "title"))
               (page-slug (parse-form-field raw-body "page_slug"))
               (media-type-input (parse-form-field raw-body "media_type"))
               (mime-type-input (parse-form-field raw-body "mime_type"))
               (edit-summary (parse-form-field raw-body "edit_summary")))
          (if (blank-text-p source-path)
              (render-bad-request "source_path must not be blank")
              (if (null (probe-file source-path))
                  (render-bad-request "source file not found")
                  (let* ((base-name (basename source-path))
                         (safe-name (sanitize-filename base-name))
                         (media-type (if (blank-text-p media-type-input)
                                         (infer-media-type base-name)
                                         media-type-input))
                         (mime-type (if (blank-text-p mime-type-input)
                                        (infer-mime-type base-name media-type)
                                        mime-type-input))
                         (stored-name (string-append
                                       (format nil "~A" (get-universal-time))
                                       "-"
                                       (format nil "~A" *markdown-temp-counter*)
                                       "-"
                                       safe-name))
                         (storage-path (string-append (media-root-dir) "/" stored-name))
                         (public-url (media-delivery-url stored-name))
                         (page-id-sql
                          (if (blank-text-p page-slug)
                              "NULL"
                              (if (null (fetch-page db page-slug))
                                  ""
                                  (string-append "(select id from pages where slug='" (sql-escape page-slug) "' limit 1)")))))
                    (if (string= page-id-sql "")
                        (render-bad-request "page_slug not found")
                        (if (not (valid-media-type-p media-type))
                            (render-bad-request "media_type must be image/video/audio")
                            (progn
                              (ensure-media-dir)
                              (let ((copy-status (system (string-append "cp " (shell-quote source-path) " " (shell-quote storage-path)))))
                                (if (= copy-status 0)
                                    (progn
                                      (system (string-append "chmod 644 " (shell-quote storage-path)))
                                      (let ((sql (string-append
                                                  "insert into media_assets (page_id, media_type, title, original_filename, stored_filename, mime_type, storage_path, public_url, created_by) values ("
                                                  page-id-sql ", '"
                                                  (sql-escape media-type) "', '"
                                                  (sql-escape title) "', '"
                                                  (sql-escape base-name) "', '"
                                                  (sql-escape stored-name) "', '"
                                                  (sql-escape mime-type) "', '"
                                                  (sql-escape storage-path) "', '"
                                                  (sql-escape public-url) "', 'web')")))
                                        (postgres-exec db sql)
                                        (if (blank-text-p edit-summary)
                                            nil
                                            (postgres-exec
                                             db
                                             (string-append
                                              "insert into page_revisions (page_id, rev_no, title, body_md, edited_by, edit_summary) "
                                              "select p.id, coalesce((select max(r.rev_no) from page_revisions r where r.page_id=p.id),0)+1, p.title, p.body_md, 'web', '"
                                              (sql-escape edit-summary)
                                              "' from pages p where p.slug='" (sql-escape page-slug) "'")))
                                        (render-see-other (string-append (app-base) "/media"))))
                                    (render-bad-request "file copy failed")))))))))))))

(defun render-new ()
  (let ((base (app-base)))
    (print-headers-ok)
    (print-layout-head "Create New Page")
    (format t "<h1>Create New Page</h1>~%")
    (format t "<form method=\"post\" action=\"~A/new\">~%" base)
    (format t "  <p><label>Slug<br><input type=\"text\" name=\"slug\" value=\"\" style=\"width:100%\" placeholder=\"new-page\"></label></p>~%")
    (format t "  <p><small>slug: a-z, 0-9, '-' のみ。先頭末尾 '-' 不可。</small></p>~%")
    (format t "  <p><label>Title<br><input type=\"text\" name=\"title\" value=\"\" style=\"width:100%\"></label></p>~%")
    (format t "  <p><label>Body (Markdown)<br><textarea name=\"body_md\"></textarea></label></p>~%")
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
                  (body-md (third row)))
              (print-headers-ok)
              (print-layout-head (string-append "Edit: " title))
              (format t "<h1>Edit: ~A</h1>~%" (html-escape title))
              (format t "<form method=\"post\" action=\"~A/~A/edit\">~%"
                      (app-base)
                      (html-escape slug))
              (format t "  <p><label>Title<br><input type=\"text\" name=\"title\" value=\"~A\" style=\"width:100%\"></label></p>~%"
                      (html-escape title))
              (format t "  <p><label>Body (Markdown)<br><textarea name=\"body_md\">~A</textarea></label></p>~%"
                      (html-escape body-md))
              (format t "  <p><label>Edit Summary<br><input type=\"text\" name=\"edit_summary\" value=\"\" style=\"width:100%\"></label></p>~%")
              (format t "  <p><button type=\"submit\">Save</button></p>~%")
              (format t "</form>~%")
              (print-layout-foot))))))

(defun render-app ()
  (let* ((method (request-method))
         (raw-path (env-or-empty "PATH_INFO"))
         (segments (path-segments))
         (n (length segments))
         (db (postgres-open (db-url))))
    (if (if (string= method "GET") (needs-canonical-redirect-p raw-path) nil)
        (render-redirect (string-append (app-base) (canonical-path-info raw-path)))
        (if (= n 0)
            (render-index db)
                (if (= n 1)
                    (if (string= (first segments) "new")
                        (if (string= method "POST")
                            (create-page db)
                            (render-new))
                        (if (string= (first segments) "media")
                            (render-media-index db)
                            (render-view db (first segments))))
                (if (= n 2)
                    (if (string= (first segments) "media")
                        (if (string= (second segments) "new")
                            (if (string= method "POST")
                                (create-media db)
                                (render-media-new))
                            (render-not-found))
                        (if (string= (first segments) "files")
                            (render-media-file db (second segments))
                        (if (string= (second segments) "edit")
                            (if (string= method "POST")
                                (save-page db (first segments))
                                (render-edit db (first segments)))
                            (render-not-found))))
                    (render-not-found)))))
    (postgres-close db)))

(handler-case
  (render-app)
  (error (e)
    (render-error (format nil "~A" e))))
