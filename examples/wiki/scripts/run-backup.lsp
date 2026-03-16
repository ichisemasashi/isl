#!/Volumes/SSD-PLU3/work/LISP/islisp/isl/bin/isl

(defun env-or-empty (name)
  (let ((v (getenv name)))
    (if (null v) "" v)))

(defun shell-quote (s)
  (string-append "'" (string-append-replace s "'" "'\\''") "'"))

(defun string-append-replace (s old new)
  (let ((p (string-index old s)))
    (if (null p)
        s
        (string-append
         (substring s 0 p)
         new
         (string-append-replace
          (substring s (+ p (length old)) (length s))
          old
          new)))))

(defun split-on (s delim)
  (if (= (length s) 0)
      '()
      (let ((p (string-index delim s)))
        (if (null p)
            (list s)
            (cons (substring s 0 p)
                  (split-on (substring s (+ p (length delim)) (length s)) delim))))))

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

(defun parse-int-safe (s fallback)
  (handler-case
    (convert s <integer>)
    (error (e) fallback)))

(defun temp-path ()
  (string-append "/tmp/isl-wiki-backup-" (format nil "~A" (get-universal-time)) ".txt"))

(defun read-file-text (path)
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
      acc)))

(defun safe-delete-file (path)
  (handler-case
    (delete-file path)
    (error (e) nil)))

(defun capture-command-output (cmd)
  (let ((tmp (temp-path)))
    (let ((status (system (string-append cmd " > " (shell-quote tmp) " 2>/dev/null"))))
      (if (not (= status 0))
          (progn
            (safe-delete-file tmp)
            "")
          (let ((value (trim-ws (read-file-text tmp))))
            (safe-delete-file tmp)
            value)))))

(defun iso-stamp ()
  (capture-command-output "date -u +%Y%m%dT%H%M%SZ"))

(defun cleanup-old-backups (backup-dir keep-count)
  (let ((tmp (temp-path)))
    (let ((cmd (string-append
                "ls -1t "
                (shell-quote (string-append backup-dir "/wiki-backup-*"))
                " 2>/dev/null | awk 'NR>"
                (format nil "~A" keep-count)
                " {print}' > "
                (shell-quote tmp))))
      (system cmd)
      (if (null (probe-file tmp))
          nil
          (let ((raw (read-file-text tmp)))
            (safe-delete-file tmp)
            (dolist (line (split-on raw "\n"))
              (if (= (length (trim-ws line)) 0)
                  nil
                  (safe-delete-file (trim-ws line)))))))))

(defun main ()
  (let* ((db-url (let ((v (getenv "ISL_WIKI_DB_URL")))
                   (if (null v) "postgresql://127.0.0.1:5432/isl_wiki" v)))
         (media-dir (let ((v (getenv "ISL_WIKI_MEDIA_DIR")))
                      (if (null v) "./examples/webserver/runtime/docroot/public/wiki-files" v)))
         (backup-dir (let ((v (getenv "ISL_WIKI_BACKUP_DIR")))
                       (if (null v) "/tmp/isl-wiki-backups" v)))
         (keep-count (parse-int-safe (env-or-empty "ISL_WIKI_BACKUP_KEEP_COUNT") 7))
         (dry-run (string= (env-or-empty "ISL_WIKI_DRY_RUN") "1"))
         (ts (iso-stamp))
         (base (string-append backup-dir "/wiki-backup-" ts))
         (sql-path (string-append base ".sql"))
         (media-path (string-append base "-media.tar.gz")))
    (system (string-append "mkdir -p " (shell-quote backup-dir)))
    (format t "backup_dir=~A~%" backup-dir)
    (format t "keep_count=~A~%" keep-count)
    (format t "sql_path=~A~%" sql-path)
    (format t "media_path=~A~%" media-path)
    (if dry-run
        (progn
          (format t "dry_run=true~%")
          (exit 0))
        (let ((db-status (system (string-append "pg_dump --clean --if-exists "
                                                (shell-quote db-url)
                                                " > "
                                                (shell-quote sql-path)))))
          (if (not (= db-status 0))
              (exit 1)
              (let ((media-status (system (string-append "tar -czf "
                                                         (shell-quote media-path)
                                                         " -C "
                                                         (shell-quote media-dir)
                                                         " ."))))
                (if (not (= media-status 0))
                    (exit 1)
                    (progn
                      (cleanup-old-backups backup-dir keep-count)
                      (format t "backup_complete=true~%")
                      (exit 0)))))))))

(main)
