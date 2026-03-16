(defun osu-contains-char-p (s ch)
  (not (null (string-index ch s))))

(defun osu-shell-quote (s)
  (if (osu-contains-char-p s "'")
      (error "single quote is not supported in path" s)
      (string-append "'" s "'")))

(defun osu-trim-left (s)
  (let ((i 0)
        (n (length s)))
    (while (and (< i n)
                (not (null (string-index (substring s i (+ i 1)) " \t\r\n"))))
      (setq i (+ i 1)))
    (substring s i n)))

(defun osu-trim-right (s)
  (let ((i (- (length s) 1)))
    (while (and (>= i 0)
                (not (null (string-index (substring s i (+ i 1)) " \t\r\n"))))
      (setq i (- i 1)))
    (substring s 0 (+ i 1))))

(defun osu-trim (s)
  (osu-trim-right (osu-trim-left s)))

(defun osu-date-command-format->srfi (fmt)
  (let ((i 0)
        (n (length fmt))
        (acc ""))
    (while (< i n)
      (if (and (< (+ i 1) n)
               (string= (substring fmt i (+ i 1)) "%"))
          (let ((code (substring fmt (+ i 1) (+ i 2))))
            (setq acc
                  (string-append
                   acc
                   (cond
                    ((string= code "Y") "~Y")
                    ((string= code "m") "~m")
                    ((string= code "d") "~d")
                    ((string= code "H") "~H")
                    ((string= code "M") "~M")
                    ((string= code "S") "~S")
                    ((string= code "a") "~a")
                    ((string= code "b") "~b")
                    ((string= code "%") "%")
                    (t (string-append "%" code)))))
            (setq i (+ i 2)))
          (progn
            (setq acc (string-append acc (substring fmt i (+ i 1))))
            (setq i (+ i 1)))))
    (if (and (> (length acc) 0)
             (string= (substring acc 0 1) "+"))
        (substring acc 1 (length acc))
        acc)))

(defun osu-read-file-text (path)
  (if (null (probe-file path))
      ""
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
          acc))))

(defun osu-write-file-text (path text)
  (with-open-file (s path
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists :supersede)
    (format s "~A" text)))

(defun osu-safe-delete-file (path)
  (handler-case
    (delete-file path)
    (error (e) nil)))

(defglobal *os-utils-temp-counter* 0)
(defglobal *os-utils-token-counter* 0)

(defun osu-temp-path (prefix)
  (setq *os-utils-temp-counter* (+ *os-utils-temp-counter* 1))
  (string-append "/tmp/"
                 prefix
                 "-"
                 (format nil "~A" (get-universal-time))
                 "-"
                 (format nil "~A" *os-utils-temp-counter*)
                 ".tmp"))

(defun osu-split-on (s delim)
  (if (= (length s) 0)
      '()
      (let ((p (string-index delim s)))
        (if (null p)
            (list s)
            (cons (substring s 0 p)
                  (osu-split-on
                   (substring s (+ p (length delim)) (length s))
                   delim))))))

(defun osu-parse-int-safe (s)
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

(defun osu-command-output (cmd)
  (let ((tmp (osu-temp-path "os-utils-cmd")))
    (let ((status (system (string-append cmd " > " (osu-shell-quote tmp) " 2>/dev/null"))))
      (if (not (= status 0))
          (progn
            (osu-safe-delete-file tmp)
            "")
          (let ((out (osu-read-file-text tmp)))
            (osu-safe-delete-file tmp)
            out)))))

(defun os-shell-quote (s)
  (osu-shell-quote s))

(defun os-read-file-text (path)
  (osu-read-file-text path))

(defun os-write-file-text (path text)
  (osu-write-file-text path text))

(defun os-safe-delete-file (path)
  (osu-safe-delete-file path))

(defun os-command-output (cmd)
  (osu-command-output cmd))

(defun os-command-available-p (cmd)
  (handler-case
    (command-available-p cmd)
    (error (e) nil)))

(defun os-file-executable-p (path)
  (handler-case
    (file-executable-p path)
    (error (e) nil)))

(defun os-test-dir-p (path)
  (handler-case
    (directory-path-p path)
    (error (e) nil)))

(defun os-test-file-readable-p (path)
  (handler-case
    (file-readable-p path)
    (error (e) nil)))

(defun os-test-symlink-p (path)
  (handler-case
    (file-symlink-p path)
    (error (e) nil)))

(defun os-uname-s ()
  (handler-case
    (let ((v (uname)))
      (if (or (null v) (null (car v)))
          "unknown"
          (car v)))
    (error (e) "unknown")))

(defun os-process-running-p (pid)
  (= (system (string-append "sh -c 'kill -0 "
                            pid
                            " >/dev/null 2>&1'"))
     0))

(defun os-mkdir-p (path)
  (handler-case
    (make-directory* path)
    (error (e) nil)))

(defun os-chmod (mode path)
  (handler-case
    (chmod-file path mode)
    (error (e) nil)))

(defun os-tr-delete (chars text)
  (let ((i 0)
        (n (length text))
        (acc ""))
    (while (< i n)
      (let ((ch (substring text i (+ i 1))))
        (if (null (string-index ch chars))
            (setq acc (string-append acc ch))
            nil))
      (setq i (+ i 1)))
    acc))

(defun os-cat-file (path)
  (osu-read-file-text path))

(defun os-cat-to-stdout (path)
  (handler-case
    (copy-file-to-stdout path)
    (error (e) nil)))

(defun os-cat-stdin-to-file (path)
  (handler-case
    (copy-stdin-to-file path)
    (error (e) nil)))

(defun os-cat-files (paths)
  (let ((rest paths)
        (acc ""))
    (while (not (null rest))
      (setq acc (string-append acc (os-cat-file (car rest))))
      (setq rest (cdr rest)))
    acc))

(defun os-wc-c (path)
  (handler-case
    (file-size path)
    (error (e) 0)))

(defun os-file-mime-type (path)
  (osu-trim
   (osu-command-output
    (string-append "file --brief --mime-type " (osu-shell-quote path)))))

(defun os-base64-file-no-newline (path)
  (handler-case
    (base64-encode-file path)
    (error (e)
      (os-tr-delete "\r\n"
                    (osu-command-output
                     (string-append "base64 < " (osu-shell-quote path)))))))

(defun os-sips-available-p ()
  (or (os-command-available-p "sips")
      (os-file-executable-p "/usr/bin/sips")))

(defun os-image-thumb-jpeg (source-path dest-path max-size)
  (if (not (os-sips-available-p))
      nil
      (= (system (string-append "sips -Z "
                                (format nil "~A" max-size)
                                " "
                                (osu-shell-quote source-path)
                                " --out "
                                (osu-shell-quote dest-path)
                                " >/dev/null 2>&1"))
         0)))

(defun os-tar-create-gz (archive-path source-dir)
  (= (system (string-append "tar -czf "
                            (osu-shell-quote archive-path)
                            " -C "
                            (osu-shell-quote source-dir)
                            " ."))
     0))

(defun os-tar-extract-gz (archive-path target-dir)
  (= (system (string-append "tar -xzf "
                            (osu-shell-quote archive-path)
                            " -C "
                            (osu-shell-quote target-dir)))
     0))

(defun os-ls (path)
  (handler-case
    (directory-list path)
    (error (e) '())))

(defun os-ls-newest-first-glob (pattern)
  (handler-case
    (glob-newest-first pattern)
    (error (e) '())))

(defun os-mv (source-path dest-path)
  (handler-case
    (rename-file source-path dest-path)
    (error (e) nil)))

(defun os-cp (source-path dest-path)
  (handler-case
    (copy-file source-path dest-path)
    (error (e) nil)))

(defun os-rm-f (path)
  (if (null (probe-file path))
      t
      (not (null (os-safe-delete-file path)))))

(defun os-date-format (fmt)
  (osu-trim
   (osu-command-output
    (string-append "date " fmt))))

(defun os-date-utc-format (fmt)
  (handler-case
    (date-utc-format (osu-date-command-format->srfi fmt))
    (error (e)
      (osu-trim
       (osu-command-output
        (string-append "date -u " fmt))))))

(defun os-date-utc-iso8601 ()
  (handler-case
    (date-utc-iso8601)
    (error (e)
      (os-date-utc-format "+%Y-%m-%dT%H:%M:%SZ"))))

(defun os-date-http-from-epoch (os-family epoch-sec)
  (handler-case
    (date-http-from-epoch epoch-sec)
    (error (e)
      (let* ((fmt "'%a, %d %b %Y %H:%M:%S GMT'")
             (cmd (string-append "date -u -r " (format nil "~A" epoch-sec) " +" fmt)))
        (osu-trim (osu-command-output cmd))))))

(defun os-epoch-from-http-date (os-family http-date)
  (handler-case
    (epoch-from-http-date http-date)
    (error (e)
      (let* ((cmd (string-append "date -ju -f '%a, %d %b %Y %H:%M:%S GMT' "
                                 (osu-shell-quote http-date)
                                 " +%s"))
             (out (osu-trim (osu-command-output cmd)))
             (n (osu-parse-int-safe out)))
        (if (null n) '() n)))))

(defun os-generate-token ()
  (handler-case
    (generate-token)
    (error (e)
      (setq *os-utils-token-counter* (+ *os-utils-token-counter* 1))
      (let ((pid (getenv "PPID")))
        (string-append
         "tok-"
         (format nil "~A" (get-universal-time))
         "-"
         (format nil "~A" (get-internal-run-time))
         "-"
         (if (null pid) "0" pid)
         "-"
         (format nil "~A" *os-utils-token-counter*))))))

(defun os-sleep-sec (seconds)
  (if (and (numberp seconds) (>= seconds 0))
      (handler-case
        (sleep-seconds seconds)
        (error (e)
          (= (system (string-append "sleep " (format nil "~A" seconds))) 0)))
      nil))

(defun os-kill (pid sig)
  (= (system (string-append "sh -c 'kill -"
                            sig
                            " "
                            pid
                            " >/dev/null 2>&1 || true'"))
     0))

(defun os-lsof-listen-owner-pid (port)
  (if (not (os-command-available-p "lsof"))
      '()
      (let* ((cmd (string-append
                   "lsof -nP -iTCP:"
                   (format nil "~A" port)
                   " -sTCP:LISTEN -Fp | sed -n \"s/^p//p\" | head -n 1"))
             (out (osu-trim (osu-command-output cmd)))
             (n (osu-parse-int-safe out)))
        (if (null n) '() out))))

(defun os-tail-file-to-stdout (path lines)
  (if (null (probe-file path))
      nil
      (= (system (string-append "tail -n "
                                (format nil "~A" lines)
                                " "
                                (osu-shell-quote path)))
         0)))
