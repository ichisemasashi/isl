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

(defun dbms-number-in-list-p (n xs)
  (if (null xs)
      nil
      (if (= n (car xs))
          t
          (dbms-number-in-list-p n (cdr xs)))))

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

(defun dbms-storage-wal-max-txid ()
  (let ((records (dbms-storage-load-wal-records))
        (mx 0))
    (dolist (r records)
      (if (> (third r) mx)
          (setq mx (third r))
          nil))
    mx))

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

(defun dbms-storage-save-table-state (table-name table-state)
  (if (or (null table-name) (string= table-name ""))
      (dbms-make-error 'dbms/invalid-representation "table-name must be non-empty string" table-name)
      (if (not (dbms-table-state-p table-state))
          (dbms-make-error 'dbms/invalid-representation "table-state must be dbms-table-state" table-state)
          (if (not (string= (second table-state) table-name))
              (dbms-make-error 'dbms/invalid-representation "table-state name mismatch" (list table-name table-state))
              (dbms-storage-atomic-replace (dbms-storage-table-path table-name) table-state)))))

(defun dbms-storage-wal-committed-txids (records)
  (let ((begun '())
        (committed '()))
    (dolist (r records)
      (let ((txid (third r))
            (kind (fourth r)))
        (if (eq kind 'begin)
            (if (dbms-number-in-list-p txid begun)
                nil
                (setq begun (append begun (list txid))))
            nil)
        (if (and (eq kind 'commit)
                 (dbms-number-in-list-p txid begun)
                 (not (dbms-number-in-list-p txid committed)))
            (setq committed (append committed (list txid)))
            nil)))
    committed))

(defun dbms-storage-wal-apply-data-record (r)
  (let ((op (fifth r))
        (payload (first (cdr (cdr (cdr (cdr (cdr r))))))))
    (if (eq op 'catalog-replace)
        (if (and (listp payload)
                 (not (null payload))
                 (dbms-catalog-p (first payload)))
            (dbms-storage-save-catalog (first payload))
            (dbms-make-error 'dbms/invalid-representation "invalid WAL catalog payload" payload))
        (if (eq op 'table-replace)
            (if (and (listp payload)
                     (>= (length payload) 2)
                     (stringp (first payload))
                     (dbms-table-state-p (second payload)))
                (dbms-storage-save-table-state (first payload) (second payload))
                (dbms-make-error 'dbms/invalid-representation "invalid WAL table payload" payload))
            (dbms-make-error 'dbms/invalid-representation "unsupported WAL data op" op)))))

(defun dbms-storage-recover-from-wal ()
  (let* ((records (dbms-storage-load-wal-records))
         (committed (dbms-storage-wal-committed-txids records))
         (applied 0)
         (failed '()))
    (dolist (r records)
      (if (dbms-error-p failed)
          nil
          (if (and (eq (fourth r) 'data)
                   (dbms-number-in-list-p (third r) committed))
              (let ((saved (dbms-storage-wal-apply-data-record r)))
                (if (dbms-error-p saved)
                    (setq failed saved)
                    (setq applied (+ applied 1))))
              nil)))
    (if (dbms-error-p failed)
        failed
        (list 'dbms-recovery-report
              (list "committed-tx-count" (length committed))
              (list "applied-data-record-count" applied)))))

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
