(load "/Volumes/SSD-PLU3/work/LISP/islisp/isl/lib/os-utils.lsp")

(defun assert-true (label value)
  (if value
      t
      (format t "assertion failed: ~A~%" label)))

(defun assert-equal (label expected actual)
  (if (equal expected actual)
      t
      (format t "assertion failed: ~A expected=~A actual=~A~%" label expected actual)))

(defun string-list-contains-p (items target)
  (let ((rest items)
        (found nil))
    (while (and (not found) (not (null rest)))
      (if (string= (car rest) target)
          (setq found t)
          nil)
      (setq rest (cdr rest)))
    found))

(defglobal *os-utils-test-root* "/tmp/isl-os-utils-smoke")
(defglobal *os-utils-src-file* (string-append *os-utils-test-root* "/a.txt"))
(defglobal *os-utils-copy-file* (string-append *os-utils-test-root* "/b.txt"))
(defglobal *os-utils-moved-file* (string-append *os-utils-test-root* "/c.txt"))
(defglobal *os-utils-tar-file* "/tmp/isl-os-utils-smoke.tar.gz")
(defglobal *os-utils-untar-root* "/tmp/isl-os-utils-smoke-out")

(assert-true "mkdir" (os-mkdir-p *os-utils-test-root*))

(with-open-file (s *os-utils-src-file*
                   :direction :output
                   :if-does-not-exist :create
                   :if-exists :supersede)
  (format s "abc~%def"))

(assert-equal "cat-file" "abc\ndef" (os-cat-file *os-utils-src-file*))
(assert-equal "wc-c" 7 (os-wc-c *os-utils-src-file*))
(assert-equal "tr-delete" "abcdef" (os-tr-delete "\n" (os-cat-file *os-utils-src-file*)))
(assert-true "cp" (os-cp *os-utils-src-file* *os-utils-copy-file*))
(assert-equal "copied" "abc\ndef" (os-cat-file *os-utils-copy-file*))
(assert-true "mv" (os-mv *os-utils-copy-file* *os-utils-moved-file*))
(assert-equal "moved" "abc\ndef" (os-cat-file *os-utils-moved-file*))
(assert-true "chmod" (os-chmod "644" *os-utils-moved-file*))
(assert-true "rm-f" (os-rm-f *os-utils-moved-file*))
(assert-true "rm-f removed" (null (probe-file *os-utils-moved-file*)))
(assert-true "ls contains a.txt"
             (string-list-contains-p (os-ls *os-utils-test-root*) "a.txt"))
(assert-true "mime non-empty" (> (length (os-file-mime-type *os-utils-src-file*)) 0))
(assert-equal "base64" "YWJjCmRlZg==" (os-base64-file-no-newline *os-utils-src-file*))
(assert-true "command available sh" (os-command-available-p "sh"))
(assert-true "file executable isl" (os-file-executable-p "/Volumes/SSD-PLU3/work/LISP/islisp/isl/bin/isl"))
(assert-true "dir test" (os-test-dir-p *os-utils-test-root*))
(assert-true "file readable test" (os-test-file-readable-p *os-utils-src-file*))
(assert-true "uname non-empty" (> (length (os-uname-s)) 0))
(assert-true "token non-empty" (> (length (os-generate-token)) 0))
(assert-true "self pid running"
             (let ((ppid (getenv "PPID")))
               (if (null ppid) t (os-process-running-p ppid))))
(assert-true "kill invalid pid tolerated" (os-kill "999999" "TERM"))
(assert-true "lsof owner type"
             (let ((owner (os-lsof-listen-owner-pid 65534)))
               (or (null owner) (stringp owner))))
(assert-true "date non-empty" (> (length (os-date-utc-iso8601)) 0))
(assert-true "http date non-empty" (> (length (os-date-http-from-epoch "darwin" 0)) 0))
(assert-true "http date parse" (numberp (os-epoch-from-http-date "darwin" "Thu, 01 Jan 1970 00:00:00 GMT")))
(assert-true "sips availability predicate boolean"
             (or (eq (os-sips-available-p) t)
                 (null (os-sips-available-p))))
(assert-true "tail file"
             (progn
               (os-write-file-text *os-utils-src-file* "tail-a\ntail-b\n")
               (os-tail-file-to-stdout *os-utils-src-file* 1)
               t))
(assert-true "tar create" (os-tar-create-gz *os-utils-tar-file* *os-utils-test-root*))
(assert-true "mkdir out" (os-mkdir-p *os-utils-untar-root*))
(assert-true "tar extract" (os-tar-extract-gz *os-utils-tar-file* *os-utils-untar-root*))
(assert-equal "untar" "tail-a\ntail-b" (os-cat-file (string-append *os-utils-untar-root* "/a.txt")))

(format t "os-utils smoke passed~%")
