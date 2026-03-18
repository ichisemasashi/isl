#!/opt/homebrew/bin/gosh

(define root
  (or (sys-getenv "ISL_ROOT")
      "/Volumes/SD_ONE/work/dev/isl"))

(sys-setenv "ISL_ROOT" root #t)
(add-load-path "/Volumes/SD_ONE/work/dev/isl/src")
(use isl.main)
(exit (main (list (car (command-line))
                  (string-append root "/examples/wiki/app/wiki.lsp"))))
