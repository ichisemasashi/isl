;; ISL Wiki MVP routes:
;;   /wiki              -> index
;;   /wiki/<slug>       -> view
;;   /wiki/<slug>/edit  -> edit form (save is not implemented yet)

(defun env-or-empty (name)
  (let ((v (getenv name)))
    (if (null v) "" v)))

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
                "select slug, title, body_md, to_char(updated_at, 'YYYY-MM-DD HH24:MI:SS') "
                "from pages where slug='" (sql-escape slug) "' limit 1"))
         (rows (postgres-query db sql)))
    (if (null rows)
        '()
        (first rows))))

(defun print-headers-ok ()
  (format t "Content-Type: text/html; charset=UTF-8~%~%"))

(defun print-headers-404 ()
  (format t "Status: 404 Not Found~%")
  (format t "Content-Type: text/html; charset=UTF-8~%~%"))

(defun print-headers-500 ()
  (format t "Status: 500 Internal Server Error~%")
  (format t "Content-Type: text/html; charset=UTF-8~%~%"))

(defun print-headers-301 (location)
  (format t "Status: 301 Moved Permanently~%")
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
  (format t "<p><a href=\"~A\">Wiki Index</a></p>~%" (app-base)))

(defun print-layout-foot ()
  (format t "</main></body>~%</html>~%"))

(defun render-not-found ()
  (print-headers-404)
  (print-layout-head "Not Found")
  (format t "<h1>404 Not Found</h1>~%")
  (format t "<p>指定されたページは見つかりませんでした。</p>~%")
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

(defun render-index (db)
  (let ((rows (fetch-pages db))
        (base (app-base)))
    (print-headers-ok)
    (print-layout-head "Wiki Index")
    (format t "<h1>Wiki Pages</h1>~%")
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
                  (updated-at (fourth row)))
              (let ((body-html (markdown->html body-md)))
              (print-headers-ok)
              (print-layout-head title)
              (format t "<h1>~A</h1>~%" (html-escape title))
              (format t "<p><a href=\"~A/~A/edit\">Edit this page</a></p>~%"
                      base
                      (html-escape slug))
              (format t "<p><small>updated: ~A / slug: <code>~A</code></small></p>~%"
                      (html-escape updated-at)
                      (html-escape slug))
              (format t "<h2>Preview (HTML)</h2>~%")
              (format t "<article class=\"wiki-body\">~A</article>~%" body-html)
              (format t "<h2>Markdown Source</h2>~%")
              (format t "<pre>~A</pre>~%" (html-escape body-md))
              (print-layout-foot)))))))

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
              (format t "<p>この画面はMVP表示のみです。保存処理は次ステップで実装します。</p>~%")
              (format t "<form method=\"post\" action=\"#\">~%")
              (format t "  <p><label>Title<br><input type=\"text\" name=\"title\" value=\"~A\" style=\"width:100%\"></label></p>~%"
                      (html-escape title))
              (format t "  <p><label>Body (Markdown)<br><textarea name=\"body_md\">~A</textarea></label></p>~%"
                      (html-escape body-md))
              (format t "  <p><button type=\"submit\" disabled>Save (not implemented)</button></p>~%")
              (format t "</form>~%")
              (print-layout-foot))))))

(defun render-app ()
  (let* ((raw-path (env-or-empty "PATH_INFO"))
         (segments (path-segments))
         (n (length segments))
         (db (postgres-open (db-url))))
    (if (needs-canonical-redirect-p raw-path)
        (render-redirect (string-append (app-base) (canonical-path-info raw-path)))
        (if (= n 0)
            (render-index db)
                (if (= n 1)
                    (render-view db (first segments))
                (if (= n 2)
                    (if (string= (second segments) "edit")
                        (render-edit db (first segments))
                        (render-not-found))
                    (render-not-found)))))
    (postgres-close db)))

(handler-case
  (render-app)
  (error (e)
    (render-error (format nil "~A" e))))
