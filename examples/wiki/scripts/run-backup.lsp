#!/Volumes/SSD-PLU3/work/LISP/islisp/isl/bin/isl

(load "/Volumes/SSD-PLU3/work/LISP/islisp/isl/lib/os-utils.lsp")

(defun env-or-empty (name)
  (let ((v (getenv name)))
    (if (null v) "" v)))

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

(defun capture-command-output (cmd)
  (trim-ws (os-command-output cmd)))

(defun iso-stamp ()
  (os-date-utc-format "+%Y%m%dT%H%M%SZ"))

(defun cleanup-old-backups (backup-dir keep-count)
  (let ((entries (os-ls-newest-first-glob (string-append backup-dir "/wiki-backup-*")))
        (idx 0))
    (dolist (line entries)
      (setq idx (+ idx 1))
      (if (> idx keep-count)
          (os-safe-delete-file (trim-ws line))
          nil))))

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
    (os-mkdir-p backup-dir)
    (format t "backup_dir=~A~%" backup-dir)
    (format t "keep_count=~A~%" keep-count)
    (format t "sql_path=~A~%" sql-path)
    (format t "media_path=~A~%" media-path)
    (if dry-run
        (progn
          (format t "dry_run=true~%")
          (exit 0))
        (let ((db-status (system (string-append "pg_dump --clean --if-exists "
                                                (os-shell-quote db-url)
                                                " > "
                                                (os-shell-quote sql-path)))))
          (if (not (= db-status 0))
              (exit 1)
              (let ((media-status (if (os-tar-create-gz media-path media-dir) 0 1)))
                (if (not (= media-status 0))
                    (exit 1)
                    (progn
                      (cleanup-old-backups backup-dir keep-count)
                      (format t "backup_complete=true~%")
                      (exit 0)))))))))

(main)
