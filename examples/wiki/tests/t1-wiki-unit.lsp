(defun wiki-test-root ()
  (let ((v (getenv "ISL_ROOT")))
    (if (null v)
        "/Volumes/SSD-PLU3/work/LISP/islisp/isl"
        v)))

(setenv "ISL_ROOT" (wiki-test-root))
(setenv "WIKI_EMBED_MODE" "1")

(load (string-append (wiki-test-root) "/examples/wiki/app/wiki.lsp"))

(defun assert-true (label pred)
  (if pred
      t
      (error (string-append "assert failed: " label))))

(defun assert-equal (label expected actual)
  (assert-true label (equal expected actual)))

(defun run-tests ()
  (setq *wiki-media-name-counter* 0)

  (let ((tags (split-tags-input " Foo,bar, foo ,, BAZ ,bar ")))
    (assert-equal "split-tags-input normalizes and de-duplicates"
                  '("foo" "bar" "baz")
                  tags))

  (let ((name-a (build-stored-media-name "upload" "../My File.PNG"))
        (name-b (build-stored-media-name "upload" "../My File.PNG")))
    (assert-true "stored media name keeps prefix" (starts-with name-a "upload-"))
    (assert-true "stored media name sanitizes filename" (not (null (string-index "My_File.PNG" name-a))))
    (assert-true "stored media name stays unique" (not (string= name-a name-b))))

  (assert-equal "public route requires no role"
                ""
                (route-required-role "GET" '("search")))
  (assert-equal "editor route requires editor"
                "editor"
                (route-required-role "POST" '("guide" "edit")))
  (assert-equal "admin rollback route requires admin"
                "admin"
                (route-required-role "POST" '("guide" "revisions" "3" "rollback")))
  (assert-equal "generic post requires viewer"
                "viewer"
                (route-required-role "POST" '("logout")))

  (format t "t1-wiki-unit: ok~%"))

(run-tests)
