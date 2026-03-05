;; dbms/app/storage.lsp
;; 永続化層（MVP Core）。

(load "./examples/dbms/app/repr.lsp")

(defglobal *dbms-storage-tmp-seq* 0)

(defun dbms-storage-root ()
  (let ((v (getenv "DBMS_STORAGE_ROOT")))
    (if (or (null v) (string= v ""))
        "./examples/dbms/storage"
        v)))

(defun dbms-storage-catalog-path ()
  (string-append (dbms-storage-root) "/catalog.lspdata"))

(defun dbms-storage-table-path (table-name)
  (string-append (dbms-storage-root) "/table_" table-name ".lspdata"))

(defun dbms-storage-wal-path ()
  (string-append (dbms-storage-root) "/wal.log"))

(defun dbms-storage-shell-quote (s)
  (let ((i 0)
        (n (length s))
        (out "'"))
    (while (< i n)
      (let ((ch (substring s i (+ i 1))))
        (if (string= ch "'")
            (setq out (string-append out "'\\''"))
            (setq out (string-append out ch))))
      (setq i (+ i 1)))
    (string-append out "'")))

(defun dbms-storage-ensure-root ()
  (= (system (string-append "mkdir -p " (dbms-storage-shell-quote (dbms-storage-root)))) 0))

(defun dbms-storage-next-tmp-path (target-path)
  (setq *dbms-storage-tmp-seq* (+ *dbms-storage-tmp-seq* 1))
  (string-append target-path ".tmp." (format nil "~A" (get-universal-time)) "." (format nil "~A" *dbms-storage-tmp-seq*)))

(defun dbms-storage-write-sexpr-file (path value)
  (with-open-file (s path :direction :output :if-exists :overwrite :if-does-not-exist :create)
    (format s "~S~%" value))
  value)

(defun dbms-storage-read-sexpr-file (path)
  (if (null (probe-file path))
      '()
      (with-open-file (s path :direction :input)
        (read s))))

(defun dbms-storage-read-all-sexpr-file (path)
  (if (null (probe-file path))
      '()
      (with-open-file (s path :direction :input)
        (let ((out '())
              (v '())
              (done nil))
          (setq v (read s))
          (while (not done)
            (if (or (null v)
                    (string= (format nil "~A" v) "#<eof>"))
                (setq done t)
                (progn
                  (setq out (append out (list v)))
                  (setq v (read s)))))
          out))))

(defun dbms-wal-record-p (v)
  (and (listp v)
       (= (length v) 6)
       (eq (first v) 'dbms-wal-record)
       (numberp (second v))
       (numberp (third v))
       (not (null (fourth v)))
       (not (null (fifth v)))))

(defun dbms-wal-record-lsn (r)
  (if (dbms-wal-record-p r) (second r) 0))

(defun dbms-storage-load-wal-records ()
  (let ((raw (dbms-storage-read-sexpr-file (dbms-storage-wal-path)))
        (out '())
        (done nil))
    (if (or (null raw) (not (listp raw)))
        (setq raw '())
        nil)
    ;; P1-006 contract: first invalid record and after are treated as invalid tail.
    (while (and (not done) (not (null raw)))
      (if (dbms-wal-record-p (car raw))
          (progn
            (setq out (append out (list (car raw))))
            (setq raw (cdr raw)))
          (setq done t)))
    out))

(defun dbms-storage-wal-next-lsn ()
  (let ((records (dbms-storage-load-wal-records))
        (mx 0))
    (dolist (r records)
      (if (> (dbms-wal-record-lsn r) mx)
          (setq mx (dbms-wal-record-lsn r))
          nil))
    (+ mx 1)))

(defun dbms-storage-wal-append-record (txid kind op payload)
  (if (not (dbms-storage-ensure-root))
      (dbms-make-error 'dbms/not-implemented "failed to create storage root for WAL" (dbms-storage-root))
      (let ((record (list 'dbms-wal-record
                          (dbms-storage-wal-next-lsn)
                          txid
                          kind
                          op
                          payload))
            (records (dbms-storage-load-wal-records)))
        (if (or (not (numberp txid))
                (null kind)
                (null op))
            (dbms-make-error 'dbms/invalid-representation "invalid wal record fields" record)
            (progn
              (setq records (append records (list record)))
              (let ((saved (dbms-storage-atomic-replace (dbms-storage-wal-path) records)))
                (if (dbms-error-p saved)
                    saved
                    record)))))))

(defun dbms-storage-atomic-replace (target-path value)
  (if (not (dbms-storage-ensure-root))
      (dbms-make-error 'dbms/not-implemented "failed to create storage root" target-path)
      (let* ((tmp-path (dbms-storage-next-tmp-path target-path))
             (tmp-write (dbms-storage-write-sexpr-file tmp-path value)))
        (if (dbms-error-p tmp-write)
            tmp-write
            (let ((mv-status (system (string-append "mv "
                                                    (dbms-storage-shell-quote tmp-path)
                                                    " "
                                                    (dbms-storage-shell-quote target-path)))))
              (if (= mv-status 0)
                  value
                  ;; mv 失敗時フォールバック: 明示警告のうえ直接上書き。
                  (progn
                    (format t "[dbms-storage-warning] atomic replace failed, fallback overwrite: ~A~%" target-path)
                    (dbms-storage-write-sexpr-file target-path value)
                    (if (not (null (probe-file tmp-path)))
                        (delete-file tmp-path)
                        nil)
                    value)))))))

(defun dbms-storage-load-catalog ()
  (let* ((path (dbms-storage-catalog-path))
         (raw (dbms-storage-read-sexpr-file path)))
    (if (null raw)
        (dbms-empty-catalog)
        (if (dbms-catalog-p raw)
            raw
            (dbms-empty-catalog)))))

(defun dbms-storage-save-catalog (catalog)
  (if (dbms-catalog-p catalog)
      (dbms-storage-atomic-replace (dbms-storage-catalog-path) catalog)
      (dbms-make-error 'dbms/invalid-representation "catalog must be dbms-catalog" catalog)))

(defun dbms-storage-load-table-rows (table-name)
  (if (or (null table-name) (string= table-name ""))
      (dbms-make-error 'dbms/invalid-representation "table-name must be non-empty string" table-name)
      (let* ((path (dbms-storage-table-path table-name))
             (raw (dbms-storage-read-sexpr-file path)))
        (if (null raw)
            (dbms-make-table-state table-name '() 1)
            (if (dbms-table-state-p raw)
                raw
                (dbms-make-table-state table-name '() 1))))))

(defun dbms-storage-save-table-rows (table-name rows)
  (if (or (null table-name) (string= table-name ""))
      (dbms-make-error 'dbms/invalid-representation "table-name must be non-empty string" table-name)
      (let ((state (dbms-make-table-state table-name rows (+ (length rows) 1))))
        (dbms-storage-atomic-replace (dbms-storage-table-path table-name) state))))
