#!/Volumes/SD_ONE/work/dev/isl/bin/isl

(defun env-or-empty (name)
  (let ((v (getenv name)))
    (if (null v) "" v)))

(defun wiki-root ()
  (let ((v (env-or-empty "ISL_ROOT")))
    (if (= (length v) 0)
        "/Volumes/SSD-PLU3/work/LISP/islisp/isl"
        v)))

(setenv "ISL_ROOT" (wiki-root))
(load (string-append (wiki-root) "/examples/wiki/app/wiki.lsp"))
