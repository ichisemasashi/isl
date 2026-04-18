#!/opt/homebrew/bin/gosh

(define root
  (or (sys-getenv "ISL_ROOT")
      "/Volumes/SSD-PLU3/work/LISP/islisp/isl"))

(sys-setenv "ISL_ROOT" root #t)
(add-load-path "/Volumes/SSD-PLU3/work/LISP/islisp/isl/src")
(use isl.main)
(exit
 (main
  (list (car (command-line))
        (string-append root "/examples/echospace/app/echospace.lsp"))))
