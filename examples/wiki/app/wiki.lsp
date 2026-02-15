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

(defun print-layout-head (title)
  (format t "<!doctype html>~%")
  (format t "<html lang=\"ja\">~%")
  (format t "<head>~%")
  (format t "  <meta charset=\"UTF-8\">~%")
  (format t "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">~%")
  (format t "  <title>~A</title>~%" (html-escape title))
  (format t "  <style>body{font-family:system-ui,-apple-system,sans-serif;margin:2rem;line-height:1.6}main{max-width:840px}textarea{width:100%;min-height:18rem;font-family:ui-monospace,SFMono-Regular,monospace}code{background:#f4f4f4;padding:.1rem .3rem;border-radius:4px}pre{background:#f7f7f7;padding:1rem;border-radius:6px;overflow:auto}a{color:#0b5394}</style>~%")
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
              (print-headers-ok)
              (print-layout-head title)
              (format t "<h1>~A</h1>~%" (html-escape title))
              (format t "<p><a href=\"~A/~A/edit\">Edit this page</a></p>~%"
                      base
                      (html-escape slug))
              (format t "<p><small>updated: ~A / slug: <code>~A</code></small></p>~%"
                      (html-escape updated-at)
                      (html-escape slug))
              (format t "<h2>Markdown</h2>~%")
              (format t "<pre>~A</pre>~%" (html-escape body-md))
              (print-layout-foot))))))

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
  (let* ((segments (path-segments))
         (n (length segments))
         (db (postgres-open (db-url))))
    (if (= n 0)
        (render-index db)
            (if (= n 1)
                (render-view db (first segments))
            (if (= n 2)
                (if (string= (second segments) "edit")
                    (render-edit db (first segments))
                    (render-not-found))
                (render-not-found))))
    (postgres-close db)))

(handler-case
  (render-app)
  (error (e)
    (render-error (format nil "~A" e))))
