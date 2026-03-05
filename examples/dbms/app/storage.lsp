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
