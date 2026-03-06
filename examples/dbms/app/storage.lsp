;; dbms/app/storage.lsp
;; 永続化層（MVP Core）。

(load "./examples/dbms/app/repr.lsp")

(defglobal *dbms-storage-tmp-seq* 0)
(defglobal *dbms-audit-archive-seq* 0)

(defun dbms-storage-root ()
  (let ((v (getenv "DBMS_STORAGE_ROOT")))
    (if (or (null v) (string= v ""))
        "./examples/dbms/storage"
        v)))

(defun dbms-storage-catalog-path ()
  (string-append (dbms-storage-root) "/catalog.lspdata"))

(defun dbms-storage-table-path (table-name)
  (string-append (dbms-storage-root) "/table_" table-name ".lspdata"))

(defun dbms-storage-table-pages-path (table-name)
  (string-append (dbms-storage-root) "/table_" table-name ".pages.lspdata"))

(defun dbms-storage-wal-path ()
  (string-append (dbms-storage-root) "/wal.log"))

(defun dbms-storage-auth-path ()
  (string-append (dbms-storage-root) "/auth.lspdata"))

(defun dbms-storage-audit-path ()
  (string-append (dbms-storage-root) "/audit.lspdata"))

(defun dbms-storage-audit-archive-path (seq)
  (string-append (dbms-storage-root) "/audit.archive." (format nil "~A" seq) ".lspdata"))

(defun dbms-storage-next-audit-archive-path ()
  (setq *dbms-audit-archive-seq* (+ *dbms-audit-archive-seq* 1))
  (dbms-storage-audit-archive-path *dbms-audit-archive-seq*))

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

(defun dbms-storage-digits-only-p (s)
  (let ((i 0)
        (n (length s))
        (ok t))
    (if (= n 0)
        nil
        (progn
          (while (and ok (< i n))
            (let ((ch (substring s i (+ i 1))))
              (if (null (string-index ch "0123456789"))
                  (setq ok nil)
                  nil))
            (setq i (+ i 1)))
          ok))))

(defun dbms-storage-parse-nonnegative-int (s)
  (let ((i 0)
        (n (length s))
        (acc 0))
    (while (< i n)
      (let ((p (string-index (substring s i (+ i 1)) "0123456789")))
        (setq acc (+ (* acc 10) p)))
      (setq i (+ i 1)))
    acc))

(defun dbms-storage-audit-max-entries ()
  (let ((v (getenv "DBMS_AUDIT_MAX_ENTRIES")))
    (if (and (stringp v)
             (not (string= v ""))
             (dbms-storage-digits-only-p v))
        (let ((n (dbms-storage-parse-nonnegative-int v)))
          (if (<= n 0) 1 n))
        10000)))

(defun dbms-storage-list-take (xs n)
  (if (or (<= n 0) (null xs))
      '()
      (cons (car xs) (dbms-storage-list-take (cdr xs) (- n 1)))))

(defun dbms-storage-list-drop (xs n)
  (if (or (<= n 0) (null xs))
      xs
      (dbms-storage-list-drop (cdr xs) (- n 1))))

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
              (let ((saved (dbms-storage-atomic-replace (dbms-storage-table-path table-name) table-state)))
                (if (dbms-error-p saved)
                    saved
                    (let ((psaved (dbms-storage-save-table-pages table-name (third table-state))))
                      (if (dbms-error-p psaved) psaved saved))))))))

(defun dbms-storage-load-auth-meta ()
  (let ((raw (dbms-storage-read-sexpr-file (dbms-storage-auth-path))))
    (if (dbms-auth-meta-p raw)
        raw
        (dbms-auth-empty))))

(defun dbms-storage-save-auth-meta (auth)
  (if (dbms-auth-meta-p auth)
      (dbms-storage-atomic-replace (dbms-storage-auth-path) auth)
      (dbms-make-error 'dbms/invalid-representation "auth must be dbms-auth-meta" auth)))

(defun dbms-storage-load-audit-log ()
  (let ((raw (dbms-storage-read-sexpr-file (dbms-storage-audit-path))))
    (if (listp raw) raw '())))

(defun dbms-storage-append-audit-record (record)
  (let* ((entries (dbms-storage-load-audit-log))
         (merged (append entries (list record)))
         (max-entries (dbms-storage-audit-max-entries)))
    (if (> (length merged) max-entries)
        (let* ((overflow (- (length merged) max-entries))
               (archived (dbms-storage-list-take merged overflow))
               (kept (dbms-storage-list-drop merged overflow))
               (archive-path (dbms-storage-next-audit-archive-path))
               (saved-archive (dbms-storage-atomic-replace archive-path archived)))
          (if (dbms-error-p saved-archive)
              saved-archive
              (dbms-storage-atomic-replace (dbms-storage-audit-path) kept)))
        (dbms-storage-atomic-replace (dbms-storage-audit-path) merged))))

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
            (if (eq op 'auth-replace)
                (if (and (listp payload)
                         (not (null payload))
                         (dbms-auth-meta-p (first payload)))
                    (dbms-storage-save-auth-meta (first payload))
                    (dbms-make-error 'dbms/invalid-representation "invalid WAL auth payload" payload))
                (dbms-make-error 'dbms/invalid-representation "unsupported WAL data op" op))))))

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

;; ------------------------------------------------------------------
;; P4-001: data page storage
;; ------------------------------------------------------------------

(defglobal *dbms-data-page-size* 4096)
(defglobal *dbms-data-page-slot-capacity* 64)

(defun dbms-data-page-header-p (v)
  ;; (dbms-data-page-header <page-id> <lsn> <checksum> <free-space> <flags>)
  (and (listp v)
       (= (length v) 6)
       (eq (first v) 'dbms-data-page-header)
       (numberp (second v))
       (> (second v) 0)
       (numberp (third v))
       (>= (third v) 0)
       (numberp (fourth v))
       (>= (fourth v) 0)
       (numberp (fifth v))
       (>= (fifth v) 0)
       (listp (dbms-sixth v))))

(defun dbms-make-data-page-header (page-id lsn checksum free-space flags)
  (list 'dbms-data-page-header page-id lsn checksum free-space flags))

(defun dbms-data-page-p (v)
  ;; (dbms-data-page <header> <slots>)
  (and (listp v)
       (= (length v) 3)
       (eq (first v) 'dbms-data-page)
       (dbms-data-page-header-p (second v))
       (listp (third v))))

(defun dbms-make-data-page (header slots)
  (list 'dbms-data-page header slots))

(defun dbms-data-page-file-p (v)
  ;; (dbms-data-page-file <version> <page-size> <slot-capacity> <next-page-id> <free-pages> <pages>)
  (and (listp v)
       (= (length v) 7)
       (eq (first v) 'dbms-data-page-file)
       (numberp (second v))
       (numberp (third v))
       (numberp (fourth v))
       (numberp (fifth v))
       (> (fifth v) 0)
       (listp (dbms-sixth v))
       (listp (dbms-seventh v))))

(defun dbms-make-data-page-file (version page-size slot-capacity next-page-id free-pages pages)
  (list 'dbms-data-page-file version page-size slot-capacity next-page-id free-pages pages))

(defun dbms-data-page-file-next-page-id (pf) (fifth pf))
(defun dbms-data-page-file-free-pages (pf) (dbms-sixth pf))
(defun dbms-data-page-file-pages (pf) (dbms-seventh pf))

(defun dbms-empty-slot-list (n)
  (if (<= n 0)
      '()
      (cons '() (dbms-empty-slot-list (- n 1)))))

(defun dbms-data-page-empty (page-id)
  (dbms-make-data-page (dbms-make-data-page-header page-id 0 0 *dbms-data-page-slot-capacity* '(free))
                       (dbms-empty-slot-list *dbms-data-page-slot-capacity*)))

(defun dbms-storage-new-page-file ()
  (dbms-make-data-page-file 1
                            *dbms-data-page-size*
                            *dbms-data-page-slot-capacity*
                            1
                            '()
                            '()))

(defun dbms-page-pair-page-id (p) (first p))
(defun dbms-page-pair-page (p) (second p))

(defun dbms-find-page-pair (pages page-id)
  (if (null pages)
      '()
      (if (= (dbms-page-pair-page-id (car pages)) page-id)
          (car pages)
          (dbms-find-page-pair (cdr pages) page-id))))

(defun dbms-put-page-pair (pages page-id page)
  (if (null pages)
      (list (list page-id page))
      (let ((h (car pages)))
        (if (= (dbms-page-pair-page-id h) page-id)
            (cons (list page-id page) (cdr pages))
            (cons h (dbms-put-page-pair (cdr pages) page-id page))))))

(defun dbms-list-contains-number-p (xs n)
  (if (null xs)
      nil
      (if (= (car xs) n)
          t
          (dbms-list-contains-number-p (cdr xs) n))))

(defun dbms-list-add-number-unique (xs n)
  (if (dbms-list-contains-number-p xs n)
      xs
      (append xs (list n))))

(defun dbms-list-remove-number (xs n)
  (if (null xs)
      '()
      (if (= (car xs) n)
          (dbms-list-remove-number (cdr xs) n)
          (cons (car xs) (dbms-list-remove-number (cdr xs) n)))))

(defun dbms-slot-free-p (slot)
  (null slot))

(defun dbms-count-free-slots (slots)
  (if (null slots)
      0
      (+ (if (dbms-slot-free-p (car slots)) 1 0)
         (dbms-count-free-slots (cdr slots)))))

(defun dbms-count-used-slots (slots)
  (- (length slots) (dbms-count-free-slots slots)))

(defun dbms-page-checksum (slots)
  ;; Lightweight deterministic checksum for corruption detection smoke checks.
  (+ (* 31 (length slots))
     (dbms-count-used-slots slots)))

(defun dbms-list-set-nth (xs idx v)
  (if (null xs)
      '()
      (if (= idx 0)
          (cons v (cdr xs))
          (cons (car xs) (dbms-list-set-nth (cdr xs) (- idx 1) v)))))

(defun dbms-page-find-free-slot-index (slots idx)
  (if (null slots)
      -1
      (if (dbms-slot-free-p (car slots))
          idx
          (dbms-page-find-free-slot-index (cdr slots) (+ idx 1)))))

(defun dbms-page-put-row (page row)
  (let* ((header (second page))
         (slots (third page))
         (free-idx (dbms-page-find-free-slot-index slots 0)))
    (if (< free-idx 0)
        '()
        (let* ((new-slots (dbms-list-set-nth slots free-idx row))
               (free-count (dbms-count-free-slots new-slots))
               (flags (if (= free-count *dbms-data-page-slot-capacity*) '(free) '()))
               (new-header (dbms-make-data-page-header (second header)
                                                       (+ (third header) 1)
                                                       (dbms-page-checksum new-slots)
                                                       free-count
                                                       flags)))
          (dbms-make-data-page new-header new-slots)))))

(defun dbms-page-row-list-from-slots (slots)
  (if (null slots)
      '()
      (let ((rest (dbms-page-row-list-from-slots (cdr slots))))
        (if (dbms-slot-free-p (car slots))
            rest
            (cons (car slots) rest)))))

(defun dbms-page-file-row-list (pages)
  (if (null pages)
      '()
      (append (dbms-page-row-list-from-slots (third (dbms-page-pair-page (car pages))))
              (dbms-page-file-row-list (cdr pages)))))

(defun dbms-page-empty-p (page)
  (= (dbms-count-used-slots (third page)) 0))

(defun dbms-page-file-empty-page-count (pages)
  (if (null pages)
      0
      (+ (if (dbms-page-empty-p (dbms-page-pair-page (car pages))) 1 0)
         (dbms-page-file-empty-page-count (cdr pages)))))

(defun dbms-max-number-list (xs cur)
  (if (null xs)
      cur
      (dbms-max-number-list (cdr xs) (if (> (car xs) cur) (car xs) cur))))

(defun dbms-page-file-page-id-list (pages)
  (if (null pages)
      '()
      (cons (dbms-page-pair-page-id (car pages))
            (dbms-page-file-page-id-list (cdr pages)))))

(defun dbms-data-page-file-repack-with-rows (page-file rows)
  (let ((pages (dbms-data-page-file-pages page-file))
        (free-pages '())
        (next-id (dbms-data-page-file-next-page-id page-file))
        (rest rows))
    ;; Clear existing pages.
    (dolist (pair pages)
      (let* ((page-id (dbms-page-pair-page-id pair))
             (cleared (dbms-data-page-empty page-id)))
        (setq pages (dbms-put-page-pair pages page-id cleared))
        (setq free-pages (dbms-list-add-number-unique free-pages page-id))))
    ;; Place rows, reusing free pages before allocating new pages.
    (while (not (null rest))
      (let ((placed nil)
            (candidates free-pages))
        (while (and (not placed) (not (null candidates)))
          (let* ((pid (car candidates))
                 (pair (dbms-find-page-pair pages pid))
                 (page (if (null pair) '() (dbms-page-pair-page pair)))
                 (next-page (if (null page) '() (dbms-page-put-row page (car rest)))))
            (if (null next-page)
                nil
                (progn
                  (setq pages (dbms-put-page-pair pages pid next-page))
                  (setq placed t)
                  (if (= (dbms-count-free-slots (third next-page)) 0)
                      (setq free-pages (dbms-list-remove-number free-pages pid))
                      (setq free-pages (dbms-list-add-number-unique free-pages pid))))))
          (setq candidates (cdr candidates)))
        (if (not placed)
            (let* ((pid next-id)
                   (fresh (dbms-data-page-empty pid))
                   (used (dbms-page-put-row fresh (car rest))))
              (setq pages (dbms-put-page-pair pages pid used))
              (setq next-id (+ next-id 1))
              (if (= (dbms-count-free-slots (third used)) 0)
                  nil
                  (setq free-pages (dbms-list-add-number-unique free-pages pid))))
            nil))
      (setq rest (cdr rest)))
    ;; Recompute free page set from actual pages.
    (setq free-pages '())
    (dolist (pair pages)
      (let* ((pid (dbms-page-pair-page-id pair))
             (page (dbms-page-pair-page pair))
             (free-count (dbms-count-free-slots (third page))))
        (if (> free-count 0)
            (setq free-pages (dbms-list-add-number-unique free-pages pid))
            nil)))
    (dbms-make-data-page-file 1
                              *dbms-data-page-size*
                              *dbms-data-page-slot-capacity*
                              next-id
                              free-pages
                              pages)))

(defun dbms-storage-load-table-pages-file (table-name)
  (let ((raw (dbms-storage-read-sexpr-file (dbms-storage-table-pages-path table-name))))
    (if (dbms-data-page-file-p raw)
        raw
        (dbms-storage-new-page-file))))

(defun dbms-storage-save-table-pages-file (table-name page-file)
  (if (dbms-data-page-file-p page-file)
      (dbms-storage-atomic-replace (dbms-storage-table-pages-path table-name) page-file)
      (dbms-make-error 'dbms/invalid-representation "page-file must be dbms-data-page-file" page-file)))

(defun dbms-storage-save-table-pages (table-name rows)
  (let* ((loaded (dbms-storage-load-table-pages-file table-name))
         (repacked (dbms-data-page-file-repack-with-rows loaded rows)))
    (dbms-storage-save-table-pages-file table-name repacked)))

(defun dbms-storage-load-table-rows-from-pages (table-name)
  (let* ((pf (dbms-storage-load-table-pages-file table-name))
         (rows (dbms-page-file-row-list (dbms-data-page-file-pages pf)))
         (max-rid 0)
         (rest rows))
    (while (not (null rest))
      (let ((rid (second (car rest))))
        (if (and (numberp rid) (> rid max-rid))
            (setq max-rid rid)
            nil))
      (setq rest (cdr rest)))
    (dbms-make-table-state table-name rows (+ max-rid 1))))

(defun dbms-storage-table-pages-stats-from-file (pf)
  (let* ((pages (dbms-data-page-file-pages pf))
         (free-pages (dbms-data-page-file-free-pages pf))
         (page-count (length pages))
         (row-count (length (dbms-page-file-row-list pages)))
         (empty-page-count (dbms-page-file-empty-page-count pages))
         (slot-capacity *dbms-data-page-slot-capacity*)
         (total-slot-count (* page-count slot-capacity))
         (used-slot-count row-count)
         (free-slot-count (- total-slot-count used-slot-count))
         (dead-tuple-count (* empty-page-count slot-capacity))
         (fragmentation-ratio (if (= page-count 0) 0 (/ empty-page-count page-count))))
    (list (list "page-count" page-count)
          (list "free-page-count" (length free-pages))
          (list "empty-page-count" empty-page-count)
          (list "next-page-id" (dbms-data-page-file-next-page-id pf))
          (list "row-count" row-count)
          (list "slot-capacity" slot-capacity)
          (list "total-slot-count" total-slot-count)
          (list "used-slot-count" used-slot-count)
          (list "free-slot-count" free-slot-count)
          (list "dead-tuple-count" dead-tuple-count)
          (list "fragmentation-ratio" fragmentation-ratio))))

(defun dbms-storage-table-pages-stats (table-name)
  (dbms-storage-table-pages-stats-from-file (dbms-storage-load-table-pages-file table-name)))

(defun dbms-storage-vacuum-table-pages (table-name)
  (let* ((loaded (dbms-storage-load-table-pages-file table-name))
         (before (dbms-storage-table-pages-stats-from-file loaded))
         (rows (dbms-page-file-row-list (dbms-data-page-file-pages loaded)))
         ;; Compaction builds from a fresh file so empty historical pages are dropped.
         (compacted (dbms-data-page-file-repack-with-rows (dbms-storage-new-page-file) rows))
         (saved (dbms-storage-save-table-pages-file table-name compacted))
         (after (dbms-storage-table-pages-stats-from-file compacted)))
    (if (dbms-error-p saved)
        saved
        (list (list "table" table-name)
              (list "before" before)
              (list "after" after)))))

(defun dbms-storage-load-table-rows (table-name)
  (if (or (null table-name) (string= table-name ""))
      (dbms-make-error 'dbms/invalid-representation "table-name must be non-empty string" table-name)
      (let* ((pages-path (dbms-storage-table-pages-path table-name))
             (legacy-path (dbms-storage-table-path table-name))
             (has-pages (not (null (probe-file pages-path))))
             (raw (if has-pages
                      (dbms-storage-load-table-rows-from-pages table-name)
                      (dbms-storage-read-sexpr-file legacy-path))))
        (if (dbms-table-state-p raw)
            raw
            (if (null raw)
                (dbms-make-table-state table-name '() 1)
                (if (dbms-table-state-p raw)
                    raw
                    (dbms-make-table-state table-name '() 1)))))))

(defun dbms-storage-save-table-rows (table-name rows)
  (if (or (null table-name) (string= table-name ""))
      (dbms-make-error 'dbms/invalid-representation "table-name must be non-empty string" table-name)
      (let ((state (dbms-make-table-state table-name rows (+ (length rows) 1))))
        (let ((saved (dbms-storage-atomic-replace (dbms-storage-table-path table-name) state)))
          (if (dbms-error-p saved)
              saved
              (let ((psaved (dbms-storage-save-table-pages table-name rows)))
                (if (dbms-error-p psaved) psaved saved)))))))

;; ------------------------------------------------------------------
;; B+Tree index storage (P3-002)
;; ------------------------------------------------------------------

(defun dbms-storage-index-path (table-name index-name)
  (string-append (dbms-storage-root) "/index_" table-name "__" index-name ".lspdata"))

(defun dbms-sixth (xs)
  (first (cdr (cdr (cdr (cdr (cdr xs)))))))

(defun dbms-seventh (xs)
  (first (cdr (cdr (cdr (cdr (cdr (cdr xs))))))))

(defun dbms-btree-index-p (v)
  ;; (dbms-btree-index <table> <index> <column> <root-page-id> <next-page-id> <pages>)
  (and (listp v)
       (= (length v) 7)
       (eq (first v) 'dbms-btree-index)
       (stringp (second v))
       (stringp (third v))
       (stringp (fourth v))
       (dbms-positive-int-p (fifth v))
       (dbms-positive-int-p (dbms-sixth v))
       (listp (dbms-seventh v))))

(defun dbms-btree-index-table-name (idx) (second idx))
(defun dbms-btree-index-name (idx) (third idx))
(defun dbms-btree-index-column (idx) (fourth idx))
(defun dbms-btree-index-root-page-id (idx) (fifth idx))
(defun dbms-btree-index-next-page-id (idx) (dbms-sixth idx))
(defun dbms-btree-index-pages (idx) (dbms-seventh idx))

(defun dbms-make-btree-index (table-name index-name column-name root-page-id next-page-id pages)
  (list 'dbms-btree-index table-name index-name column-name root-page-id next-page-id pages))

(defun dbms-storage-load-index (table-name index-name)
  (let ((raw (dbms-storage-read-sexpr-file (dbms-storage-index-path table-name index-name))))
    (if (dbms-btree-index-p raw)
        raw
        '())))

(defun dbms-storage-save-index (table-name index-name index-data)
  (if (not (dbms-btree-index-p index-data))
      (dbms-make-error 'dbms/invalid-representation "index-data must be dbms-btree-index" index-data)
      (dbms-storage-atomic-replace (dbms-storage-index-path table-name index-name) index-data)))

(defun dbms-btree-page-id-eq-p (page-id page-pair)
  (and (listp page-pair)
       (>= (length page-pair) 2)
       (= page-id (first page-pair))))

(defun dbms-btree-find-page-pair (page-id pages)
  (if (null pages)
      '()
      (if (dbms-btree-page-id-eq-p page-id (car pages))
          (car pages)
          (dbms-btree-find-page-pair page-id (cdr pages)))))

(defun dbms-btree-get-page (index-data page-id)
  (let ((pair (dbms-btree-find-page-pair page-id (dbms-btree-index-pages index-data))))
    (if (null pair) '() (second pair))))

(defun dbms-btree-put-page-pair (page-id page pages)
  (if (null pages)
      (list (list page-id page))
      (let ((h (car pages)))
        (if (dbms-btree-page-id-eq-p page-id h)
            (cons (list page-id page) (cdr pages))
            (cons h (dbms-btree-put-page-pair page-id page (cdr pages)))))))

(defun dbms-btree-put-page (index-data page-id page)
  (dbms-make-btree-index
   (dbms-btree-index-table-name index-data)
   (dbms-btree-index-name index-data)
   (dbms-btree-index-column index-data)
   (dbms-btree-index-root-page-id index-data)
   (dbms-btree-index-next-page-id index-data)
   (dbms-btree-put-page-pair page-id page (dbms-btree-index-pages index-data))))

(defun dbms-btree-alloc-page-id (index-data)
  (let ((new-id (dbms-btree-index-next-page-id index-data)))
    (list new-id
          (dbms-make-btree-index
           (dbms-btree-index-table-name index-data)
           (dbms-btree-index-name index-data)
           (dbms-btree-index-column index-data)
           (dbms-btree-index-root-page-id index-data)
           (+ new-id 1)
           (dbms-btree-index-pages index-data)))))

(defun dbms-btree-index-set-root (index-data root-page-id)
  (dbms-make-btree-index
   (dbms-btree-index-table-name index-data)
   (dbms-btree-index-name index-data)
   (dbms-btree-index-column index-data)
   root-page-id
   (dbms-btree-index-next-page-id index-data)
   (dbms-btree-index-pages index-data)))

(defun dbms-btree-header-with-flags (page-id flags)
  (dbms-make-btree-page-header page-id 0 0 *dbms-btree-page-header-size* *dbms-btree-page-size* flags))

(defun dbms-btree-key-compare (a b)
  (if (and (eq a 'NULL) (eq b 'NULL))
      0
      (if (eq a 'NULL)
          1
          (if (eq b 'NULL)
              -1
              (if (and (numberp a) (numberp b))
                  (if (< a b) -1 (if (> a b) 1 0))
                  (if (and (stringp a) (stringp b))
                      (if (string< a b) -1 (if (string> a b) 1 0))
                      (if (and (or (eq a t) (null a))
                               (or (eq b t) (null b)))
                          (let ((ia (if (eq a t) 1 0))
                                (ib (if (eq b t) 1 0)))
                            (if (< ia ib) -1 (if (> ia ib) 1 0)))
                          0)))))))

(defun dbms-btree-entry-key (entry)
  (second entry))

(defun dbms-btree-leaf-entry-rid (entry)
  (third entry))

(defun dbms-btree-internal-entry-child (entry)
  (third entry))

(defun dbms-btree-leaf-entry-before-p (a b)
  (let ((cmp (dbms-btree-key-compare (dbms-btree-entry-key a) (dbms-btree-entry-key b))))
    (if (= cmp 0)
        (< (dbms-btree-leaf-entry-rid a) (dbms-btree-leaf-entry-rid b))
        (< cmp 0))))

(defun dbms-btree-internal-entry-before-p (a b)
  (< (dbms-btree-key-compare (dbms-btree-entry-key a) (dbms-btree-entry-key b)) 0))

(defun dbms-btree-sorted-insert-leaf-entry (entry entries)
  (if (null entries)
      (list entry)
      (if (dbms-btree-leaf-entry-before-p entry (car entries))
          (cons entry entries)
          (cons (car entries) (dbms-btree-sorted-insert-leaf-entry entry (cdr entries))))))

(defun dbms-btree-sorted-insert-internal-entry (entry entries)
  (if (null entries)
      (list entry)
      (if (dbms-btree-internal-entry-before-p entry (car entries))
          (cons entry entries)
          (cons (car entries) (dbms-btree-sorted-insert-internal-entry entry (cdr entries))))))

(defun dbms-list-take (xs n)
  (if (or (= n 0) (null xs))
      '()
      (cons (car xs) (dbms-list-take (cdr xs) (- n 1)))))

(defun dbms-list-drop (xs n)
  (if (or (= n 0) (null xs))
      xs
      (dbms-list-drop (cdr xs) (- n 1))))

(defun dbms-btree-nth (xs n)
  (if (null xs)
      '()
      (if (= n 0)
          (car xs)
          (dbms-btree-nth (cdr xs) (- n 1)))))

(defun dbms-btree-choose-child-id (internal-page key)
  (let ((child (fifth internal-page))
        (entries (fourth internal-page)))
    (while (not (null entries))
      (let ((e (car entries)))
        (if (>= (dbms-btree-key-compare key (dbms-btree-entry-key e)) 0)
            (setq child (dbms-btree-internal-entry-child e))
            nil))
      (setq entries (cdr entries)))
    child))

(defun dbms-btree-leaf-split (index-data page-id leaf-page entries)
  (let* ((n (length entries))
         (mid (truncate (/ n 2)))
         (left-entries (dbms-list-take entries mid))
         (right-entries (dbms-list-drop entries mid))
         (alloc (dbms-btree-alloc-page-id index-data))
         (right-id (first alloc))
         (idx1 (second alloc))
         (old-right (dbms-sixth leaf-page))
         (left-page (dbms-make-btree-page
                     (dbms-btree-header-with-flags page-id '())
                     'leaf
                     left-entries
                     '()
                     right-id))
         (right-page (dbms-make-btree-page
                      (dbms-btree-header-with-flags right-id '())
                      'leaf
                      right-entries
                      '()
                      old-right))
         (idx2 (dbms-btree-put-page idx1 page-id left-page))
         (idx3 (dbms-btree-put-page idx2 right-id right-page))
         (promote (dbms-btree-entry-key (car right-entries))))
    (list idx3 promote right-id)))

(defun dbms-btree-internal-split (index-data page-id page entries)
  (let* ((n (length entries))
         (mid (truncate (/ n 2)))
         (promote-entry (dbms-btree-nth entries mid))
         (promote-key (dbms-btree-entry-key promote-entry))
         (right-leftmost (dbms-btree-internal-entry-child promote-entry))
         (left-entries (dbms-list-take entries mid))
         (right-entries (dbms-list-drop entries (+ mid 1)))
         (alloc (dbms-btree-alloc-page-id index-data))
         (right-id (first alloc))
         (idx1 (second alloc))
         (left-page (dbms-make-btree-page
                     (dbms-btree-header-with-flags page-id '())
                     'internal
                     left-entries
                     (fifth page)
                     (dbms-sixth page)))
         (right-page (dbms-make-btree-page
                      (dbms-btree-header-with-flags right-id '())
                      'internal
                      right-entries
                      right-leftmost
                      '()))
         (idx2 (dbms-btree-put-page idx1 page-id left-page))
         (idx3 (dbms-btree-put-page idx2 right-id right-page)))
    (list idx3 promote-key right-id)))

(defun dbms-btree-insert-rec (index-data page-id key rid)
  (let ((page (dbms-btree-get-page index-data page-id)))
    (if (null page)
        (dbms-make-error 'dbms/invalid-representation "missing btree page" page-id)
        (if (eq (third page) 'leaf)
            (let* ((new-entry (dbms-make-btree-leaf-entry key rid))
                   (entries (dbms-btree-sorted-insert-leaf-entry new-entry (fourth page)))
                   (cap (dbms-btree-page-capacity-estimate 'leaf)))
              (if (<= (length entries) cap)
                  (let ((new-page (dbms-make-btree-page
                                   (dbms-btree-header-with-flags page-id (if (= page-id (dbms-btree-index-root-page-id index-data))
                                                                              '(root)
                                                                              '()))
                                   'leaf
                                   entries
                                   '()
                                   (dbms-sixth page))))
                    (list 'ok (dbms-btree-put-page index-data page-id new-page)))
                  (let ((split (dbms-btree-leaf-split index-data page-id page entries)))
                    (list 'split (first split) (second split) (third split)))))
            (let* ((child-id (dbms-btree-choose-child-id page key))
                   (ins (dbms-btree-insert-rec index-data child-id key rid)))
              (if (dbms-error-p ins)
                  ins
                  (if (eq (first ins) 'ok)
                      (list 'ok (second ins))
                      (let* ((idx1 (second ins))
                             (promote-key (third ins))
                             (right-id (fourth ins))
                             (new-entry (dbms-make-btree-internal-entry promote-key right-id))
                             (entries (dbms-btree-sorted-insert-internal-entry new-entry (fourth page)))
                             (cap (dbms-btree-page-capacity-estimate 'internal)))
                        (if (<= (length entries) cap)
                            (let ((new-page (dbms-make-btree-page
                                             (dbms-btree-header-with-flags page-id (if (= page-id (dbms-btree-index-root-page-id idx1))
                                                                                        '(root)
                                                                                        '()))
                                             'internal
                                             entries
                                             (fifth page)
                                             (dbms-sixth page))))
                              (list 'ok (dbms-btree-put-page idx1 page-id new-page)))
                            (let ((split (dbms-btree-internal-split idx1 page-id page entries)))
                              (list 'split (first split) (second split) (third split))))))))))))

(defun dbms-storage-btree-new-index (table-name index-name column-name)
  (let* ((root-id 1)
         (root-page (dbms-make-btree-page
                     (dbms-btree-header-with-flags root-id '(root))
                     'leaf
                     '()
                     '()
                     '())))
    (dbms-make-btree-index table-name index-name column-name root-id 2 (list (list root-id root-page)))))

(defun dbms-storage-btree-insert (index-data key rid)
  (let ((ins (dbms-btree-insert-rec index-data (dbms-btree-index-root-page-id index-data) key rid)))
    (if (dbms-error-p ins)
        ins
        (if (eq (first ins) 'ok)
            (second ins)
            (let* ((idx1 (second ins))
                   (promote-key (third ins))
                   (right-id (fourth ins))
                   (alloc (dbms-btree-alloc-page-id idx1))
                   (new-root-id (first alloc))
                   (idx2 (second alloc))
                   (new-root (dbms-make-btree-page
                              (dbms-btree-header-with-flags new-root-id '(root))
                              'internal
                              (list (dbms-make-btree-internal-entry promote-key right-id))
                              (dbms-btree-index-root-page-id idx1)
                              '()))
                   (idx3 (dbms-btree-put-page idx2 new-root-id new-root)))
              (dbms-btree-index-set-root idx3 new-root-id))))))

(defun dbms-btree-leaf-entry-match-key-p (entry key)
  (= (dbms-btree-key-compare (dbms-btree-entry-key entry) key) 0))

(defun dbms-btree-find-leaf-page-id (index-data key)
  (let ((page-id (dbms-btree-index-root-page-id index-data))
        (done nil))
    (while (not done)
      (let ((page (dbms-btree-get-page index-data page-id)))
        (if (or (null page) (eq (third page) 'leaf))
            (setq done t)
            (setq page-id (dbms-btree-choose-child-id page key)))))
    page-id))

(defun dbms-btree-collect-leaf-rids-by-key (entries key)
  (if (null entries)
      '()
      (let ((e (car entries))
            (rest (cdr entries)))
        (if (dbms-btree-leaf-entry-match-key-p e key)
            (cons (dbms-btree-leaf-entry-rid e)
                  (dbms-btree-collect-leaf-rids-by-key rest key))
            (dbms-btree-collect-leaf-rids-by-key rest key)))))

(defun dbms-storage-btree-search (index-data key)
  (if (not (dbms-btree-index-p index-data))
      (dbms-make-error 'dbms/invalid-representation "index-data must be dbms-btree-index" index-data)
      (let ((leaf-id (dbms-btree-find-leaf-page-id index-data key))
            (out '())
            (done nil))
        (if (null leaf-id)
            '()
            (progn
              (while (not done)
                (let ((page (dbms-btree-get-page index-data leaf-id)))
                  (if (or (null page) (not (eq (third page) 'leaf)))
                      (setq done t)
                      (let ((entries (fourth page))
                            (collected (dbms-btree-collect-leaf-rids-by-key (fourth page) key))
                            (right-id (dbms-sixth page)))
                        (setq out (append out collected))
                        (if (or (null right-id) (null entries))
                            (setq done t)
                            (let ((last-key (dbms-btree-entry-key (car (dbms-list-drop entries (- (length entries) 1))))))
                              (if (> (dbms-btree-key-compare last-key key) 0)
                                  (setq done t)
                                  (setq leaf-id right-id))))))))
              out)))))

(defun dbms-btree-leftmost-leaf-page-id (index-data)
  (let ((page-id (dbms-btree-index-root-page-id index-data))
        (done nil))
    (while (not done)
      (let ((page (dbms-btree-get-page index-data page-id)))
        (if (or (null page) (eq (third page) 'leaf))
            (setq done t)
            (setq page-id (fifth page)))))
    page-id))

(defun dbms-btree-key-meets-lower-p (entry-key lower-key lower-inclusive)
  (if (null lower-key)
      t
      (let ((cmp (dbms-btree-key-compare entry-key lower-key)))
        (if lower-inclusive
            (>= cmp 0)
            (> cmp 0)))))

(defun dbms-btree-key-meets-upper-p (entry-key upper-key upper-inclusive)
  (if (null upper-key)
      t
      (let ((cmp (dbms-btree-key-compare entry-key upper-key)))
        (if upper-inclusive
            (<= cmp 0)
            (< cmp 0)))))

(defun dbms-btree-key-beyond-upper-p (entry-key upper-key upper-inclusive)
  (if (null upper-key)
      nil
      (let ((cmp (dbms-btree-key-compare entry-key upper-key)))
        (if upper-inclusive
            (> cmp 0)
            (>= cmp 0)))))

(defun dbms-btree-entry-rids-in-range (entries lower-key lower-inclusive upper-key upper-inclusive)
  (if (null entries)
      '()
      (let* ((e (car entries))
             (k (dbms-btree-entry-key e))
             (rest (dbms-btree-entry-rids-in-range (cdr entries) lower-key lower-inclusive upper-key upper-inclusive)))
        (if (and (dbms-btree-key-meets-lower-p k lower-key lower-inclusive)
                 (dbms-btree-key-meets-upper-p k upper-key upper-inclusive))
            (cons (dbms-btree-leaf-entry-rid e) rest)
            rest))))

(defun dbms-storage-btree-range-search (index-data lower-key lower-inclusive upper-key upper-inclusive limit)
  ;; Returns row-id list in key ascending order.
  (if (not (dbms-btree-index-p index-data))
      (dbms-make-error 'dbms/invalid-representation "index-data must be dbms-btree-index" index-data)
      (let ((leaf-id (if (null lower-key)
                         (dbms-btree-leftmost-leaf-page-id index-data)
                         (dbms-btree-find-leaf-page-id index-data lower-key)))
            (out '())
            (done nil))
        (while (and (not done) (not (null leaf-id)))
          (let ((page (dbms-btree-get-page index-data leaf-id)))
            (if (or (null page) (not (eq (third page) 'leaf)))
                (setq done t)
                (let ((entries (fourth page))
                      (right-id (dbms-sixth page)))
                  (setq out (append out (dbms-btree-entry-rids-in-range entries
                                                                         lower-key
                                                                         lower-inclusive
                                                                         upper-key
                                                                         upper-inclusive)))
                  (if (and (numberp limit) (> limit 0) (>= (length out) limit))
                      (setq done t)
                      (if (or (null right-id) (null entries))
                          (setq done t)
                          (let ((last-key (dbms-btree-entry-key (car (dbms-list-drop entries (- (length entries) 1))))))
                            (if (dbms-btree-key-beyond-upper-p last-key upper-key upper-inclusive)
                                (setq done t)
                                (setq leaf-id right-id)))))))))
        (if (and (numberp limit) (> limit 0) (> (length out) limit))
            (dbms-list-take out limit)
            out))))

(defun dbms-row-id (row)
  (second row))

(defun dbms-row-values (row)
  (third row))

(defun dbms-row-value-from-pairs (pairs col-name)
  (if (null pairs)
      'NULL
      (if (string= (first (car pairs)) col-name)
          (second (car pairs))
          (dbms-row-value-from-pairs (cdr pairs) col-name))))

(defun dbms-row-get-value (row col-name)
  (dbms-row-value-from-pairs (dbms-row-values row) col-name))

(defun dbms-storage-build-btree-index (table-name index-name column-name rows)
  (let ((idx (dbms-storage-btree-new-index table-name index-name column-name))
        (rest rows)
        (failed '()))
    (while (and (null failed) (not (null rest)))
      (let* ((row (car rest))
             (rid (dbms-row-id row))
             (key (dbms-row-get-value row column-name))
             (next (dbms-storage-btree-insert idx key rid)))
        (if (dbms-error-p next)
            (setq failed next)
            (setq idx next)))
      (setq rest (cdr rest)))
    (if (dbms-error-p failed) failed idx)))
