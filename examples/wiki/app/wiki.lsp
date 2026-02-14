;; Minimal ISL CGI app: return a fixed HTML page.
(format t
        "Content-Type: text/html; charset=UTF-8~%~%")
(format t
        "<!doctype html>~%")
(format t
        "<html lang=\"ja\">~%")
(format t
        "<head>~%")
(format t
        "  <meta charset=\"UTF-8\">~%")
(format t
        "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">~%")
(format t
        "  <title>ISL Wiki</title>~%")
(format t
        "  <style>body{font-family:system-ui,-apple-system,sans-serif;margin:2rem;line-height:1.6}main{max-width:720px}code{background:#f4f4f4;padding:.1rem .3rem;border-radius:4px}</style>~%")
(format t
        "</head>~%")
(format t
        "<body>~%")
(format t
        "  <main>~%")
(format t
        "    <h1>ISL Wiki (fixed page)</h1>~%")
(format t
        "    <p>Apache CGI + ISL で配信しています。</p>~%")
(format t
        "    <p>次の段階で PostgreSQL と接続し、記事をDBから表示できるようにします。</p>~%")
(format t
        "    <p>Script: <code>examples/wiki/app/wiki.lsp</code></p>~%")
(format t
        "  </main>~%")
(format t
        "</body>~%")
(format t
        "</html>~%")
