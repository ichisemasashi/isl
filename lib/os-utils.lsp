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
  (= (system (string-append "sh -c 'command -v " cmd " >/dev/null 2>&1'")) 0))

(defun os-file-executable-p (path)
  (= (system (string-append "sh -c 'test -x " (osu-shell-quote path) " >/dev/null 2>&1'")) 0))

(defun os-mkdir-p (path)
  (= (system (string-append "mkdir -p " (osu-shell-quote path))) 0))

(defun os-chmod (mode path)
  (= (system (string-append "chmod " mode " " (osu-shell-quote path))) 0))

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
  (= (system (string-append "cat " (osu-shell-quote path))) 0))

(defun os-cat-stdin-to-file (path)
  (= (system (string-append "cat > " (osu-shell-quote path))) 0))

(defun os-cat-files (paths)
  (let ((rest paths)
        (acc ""))
    (while (not (null rest))
      (setq acc (string-append acc (os-cat-file (car rest))))
      (setq rest (cdr rest)))
    acc))

(defun os-wc-c (path)
  (let* ((tmp (osu-temp-path "os-utils-wc"))
         (cmd (string-append "wc -c < " (osu-shell-quote path) " > " (osu-shell-quote tmp)))
         (status (system cmd)))
    (if (not (= status 0))
        (progn
          (osu-safe-delete-file tmp)
          0)
        (let ((n (osu-parse-int-safe (osu-trim (osu-read-file-text tmp)))))
          (osu-safe-delete-file tmp)
          (if (null n) 0 n)))))

(defun os-file-mime-type (path)
  (osu-trim
   (osu-command-output
    (string-append "file --brief --mime-type " (osu-shell-quote path)))))

(defun os-base64-file-no-newline (path)
  (os-tr-delete "\r\n"
                (osu-command-output
                 (string-append "base64 < " (osu-shell-quote path)))))

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
  (let ((raw (osu-command-output (string-append "ls -1A " (osu-shell-quote path)))))
    (if (= (length raw) 0)
        '()
        (osu-split-on (osu-trim raw) "\n"))))

(defun os-ls-newest-first-glob (pattern)
  (let ((raw (osu-command-output (string-append "ls -1t " pattern " 2>/dev/null"))))
    (if (= (length raw) 0)
        '()
        (osu-split-on (osu-trim raw) "\n"))))

(defun os-mv (source-path dest-path)
  (= (system (string-append "mv "
                            (osu-shell-quote source-path)
                            " "
                            (osu-shell-quote dest-path)))
     0))

(defun os-cp (source-path dest-path)
  (= (system (string-append "cp "
                            (osu-shell-quote source-path)
                            " "
                            (osu-shell-quote dest-path)))
     0))

(defun os-date-format (fmt)
  (osu-trim
   (osu-command-output
    (string-append "date " fmt))))

(defun os-date-utc-format (fmt)
  (osu-trim
   (osu-command-output
    (string-append "date -u " fmt))))

(defun os-date-utc-iso8601 ()
  (os-date-utc-format "+%Y-%m-%dT%H:%M:%SZ"))

(defun os-generate-token ()
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
     (format nil "~A" *os-utils-token-counter*))))
