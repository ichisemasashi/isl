#!/Volumes/SSD-PLU3/work/LISP/islisp/isl/bin/isl

(defun env-or-empty (name)
  (let ((v (getenv name)))
    (if (null v) "" v)))

(defun starts-with (s prefix)
  (let ((n (length s))
        (m (length prefix)))
    (if (< n m)
        nil
        (string= (substring s 0 m) prefix))))

(defun ends-with (s suffix)
  (let ((n (length s))
        (m (length suffix)))
    (if (< n m)
        nil
        (string= (substring s (- n m) n) suffix))))

(defun trim-leading-crlf (s)
  (if (starts-with s "\r\n")
      (trim-leading-crlf (substring s 2 (length s)))
      (if (starts-with s "\n")
          (trim-leading-crlf (substring s 1 (length s)))
          s)))

(defun trim-trailing-crlf (s)
  (if (ends-with s "\r\n")
      (trim-trailing-crlf (substring s 0 (- (length s) 2)))
      (if (ends-with s "\n")
          (trim-trailing-crlf (substring s 0 (- (length s) 1)))
          s)))

(defun read-file-text (path)
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

(defun write-file-text (path text)
  (with-open-file (s path
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists :supersede)
    (format s "~A" text)))

(defun split-once (s delim)
  (let ((p (string-index delim s)))
    (if (null p)
        (list s "")
        (list (substring s 0 p)
              (substring s (+ p (length delim)) (length s))))))

(defun split-on (s delim)
  (if (= (length s) 0)
      '()
      (let ((kv (split-once s delim)))
        (if (= (length (second kv)) 0)
            (list (first kv))
            (cons (first kv) (split-on (second kv) delim))))))

(defun string-downcase-ascii (s)
  (let ((upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
        (lower "abcdefghijklmnopqrstuvwxyz")
        (i 0)
        (n (length s))
        (acc ""))
    (while (< i n)
      (let* ((ch (substring s i (+ i 1)))
             (p (string-index ch upper)))
        (setq acc
              (string-append acc
                             (if (null p)
                                 ch
                                 (substring lower p (+ p 1))))))
      (setq i (+ i 1)))
    acc))

(defun trim-ws-left (s)
  (let ((i 0)
        (n (length s)))
    (while (and (< i n)
                (not (null (string-index (substring s i (+ i 1)) " \t\r\n"))))
      (setq i (+ i 1)))
    (substring s i n)))

(defun trim-ws-right (s)
  (let ((i (- (length s) 1)))
    (while (and (>= i 0)
                (not (null (string-index (substring s i (+ i 1)) " \t\r\n"))))
      (setq i (- i 1)))
    (substring s 0 (+ i 1))))

(defun trim-ws (s)
  (trim-ws-right (trim-ws-left s)))

(defun find-quoted-param (source key)
  (let ((marker (string-append key "=\"")))
    (let ((p (string-index marker source)))
      (if (null p)
          ""
          (let* ((start (+ p (length marker)))
                 (rest (substring source start (length source)))
                 (end (string-index "\"" rest)))
            (if (null end)
                ""
                (substring rest 0 end)))))))

(defun safe-field-name (name)
  (let ((allowed "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_")
        (i 0)
        (n (length name))
        (acc ""))
    (while (< i n)
      (let ((ch (substring name i (+ i 1))))
        (setq acc
              (string-append
               acc
               (if (null (string-index ch allowed))
                   "_"
                   ch))))
      (setq i (+ i 1)))
    acc))

(defun parse-header-lines (headers-raw)
  (let ((lines (if (null (string-index "\r\n" headers-raw))
                   (split-on headers-raw "\n")
                   (split-on headers-raw "\r\n")))
        (acc '()))
    (dolist (line lines)
      (let ((p (string-index ":" line)))
        (if (null p)
            nil
            (setq acc
                  (cons (list (string-downcase-ascii (trim-ws (substring line 0 p)))
                              (trim-ws (substring line (+ p 1) (length line))))
                        acc)))))
    acc))

(defun header-value (headers key)
  (let ((cur headers)
        (found ""))
    (while (and (string= found "") (not (null cur)))
      (let ((kv (car cur)))
        (if (string= (first kv) key)
            (setq found (second kv))
            nil))
      (setq cur (cdr cur)))
    found))

(defun write-field-file (out-dir name body)
  (write-file-text
   (string-append out-dir "/field-" (safe-field-name name) ".txt")
   body))

(defun write-upload-files (out-dir filename content-type body)
  (write-file-text (string-append out-dir "/upload.bin") body)
  (write-file-text (string-append out-dir "/upload_filename.txt") filename)
  (write-file-text (string-append out-dir "/upload_content_type.txt") content-type))

(defun parse-part (out-dir part saw-file)
  (let* ((trimmed (trim-trailing-crlf (trim-leading-crlf part))))
    (if (or (= (length trimmed) 0) (string= trimmed "--"))
        saw-file
        (let* ((sep-crlf (string-index "\r\n\r\n" trimmed))
               (sep-lf (string-index "\n\n" trimmed))
               (sep (if (null sep-crlf) sep-lf sep-crlf))
               (sep-len (if (null sep-crlf) 2 4)))
          (if (null sep)
              saw-file
              (let* ((headers-raw (substring trimmed 0 sep))
                     (body (trim-trailing-crlf (substring trimmed (+ sep sep-len) (length trimmed))))
                     (headers (parse-header-lines headers-raw))
                     (cd (header-value headers "content-disposition"))
                     (name (find-quoted-param cd "name"))
                     (filename (find-quoted-param cd "filename")))
                (if (or (= (length cd) 0)
                        (null (string-index "form-data" (string-downcase-ascii cd)))
                        (= (length name) 0))
                    saw-file
                    (if (> (length filename) 0)
                        (if saw-file
                            saw-file
                            (progn
                              (write-upload-files out-dir filename (header-value headers "content-type") body)
                              t))
                        (progn
                          (write-field-file out-dir name body)
                          saw-file)))))))))

(defun parse-multipart (input-path out-dir boundary)
  (let* ((raw (read-file-text input-path))
         (delimiter (string-append "--" boundary))
         (parts (split-on raw delimiter))
         (saw-file nil))
    (dolist (part parts)
      (setq saw-file (parse-part out-dir part saw-file)))
    0))

(defun main ()
  (let ((input-path (env-or-empty "WIKI_MULTIPART_INPUT"))
        (out-dir (env-or-empty "WIKI_MULTIPART_OUTDIR"))
        (boundary (env-or-empty "WIKI_MULTIPART_BOUNDARY")))
    (if (or (= (length input-path) 0)
            (= (length out-dir) 0)
            (= (length boundary) 0))
        (progn
          (format t "usage: set WIKI_MULTIPART_INPUT/WIKI_MULTIPART_OUTDIR/WIKI_MULTIPART_BOUNDARY~%")
          (exit 1))
        (if (null (probe-file input-path))
            (progn
              (format t "multipart input file not found~%")
              (exit 1))
            (exit (parse-multipart input-path out-dir boundary))))))

(main)
