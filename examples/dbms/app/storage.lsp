;; dbms/app/storage.lsp
;; 永続化層（MVP Core）。

(load
 (let ((v (getenv "ISL_ROOT")))
   (string-append
    (if (or (null v) (string= v ""))
        "/Volumes/SSD-PLU3/work/LISP/islisp/isl"
        v)
    "/lib/os-utils.lsp")))
(load "./examples/dbms/app/repr.lsp")

(defglobal *dbms-storage-tmp-seq* 0)
(defglobal *dbms-audit-archive-seq* 0)
(defglobal *dbms-backup-generation-seq* 0)

(defun dbms-storage-reverse (xs)
  (let ((rest xs)
        (out '()))
    (while (not (null rest))
      (setq out (cons (car rest) out))
      (setq rest (cdr rest)))
    out))

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

(defun dbms-storage-checkpoint-path ()
  (string-append (dbms-storage-root) "/checkpoint.lspdata"))

(defun dbms-storage-auth-path ()
  (string-append (dbms-storage-root) "/auth.lspdata"))

(defun dbms-storage-security-path ()
  (string-append (dbms-storage-root) "/security.lspdata"))

(defun dbms-storage-audit-path ()
  (string-append (dbms-storage-root) "/audit.lspdata"))

(defun dbms-storage-backup-root ()
  (string-append (dbms-storage-root) "/backups"))

(defun dbms-storage-backup-index-path ()
  (string-append (dbms-storage-backup-root) "/index.lspdata"))

(defun dbms-storage-backup-dump-path (generation-id)
  (string-append (dbms-storage-backup-root) "/backup." generation-id ".dump.lspdata"))

(defun dbms-storage-backup-wal-path (generation-id)
  (string-append (dbms-storage-backup-root) "/backup." generation-id ".wal.lspdata"))

(defun dbms-storage-replication-path ()
  (string-append (dbms-storage-root) "/replication.lspdata"))

(defun dbms-storage-failover-path ()
  (string-append (dbms-storage-root) "/failover.lspdata"))

(defun dbms-storage-failover-path-for-root (root)
  (string-append root "/failover.lspdata"))

(defun dbms-storage-maintenance-path ()
  (string-append (dbms-storage-root) "/maintenance.lspdata"))

(defun dbms-storage-wal-path-for-root (root)
  (string-append root "/wal.log"))

(defun dbms-storage-checkpoint-path-for-root (root)
  (string-append root "/checkpoint.lspdata"))

(defun dbms-storage-catalog-path-for-root (root)
  (string-append root "/catalog.lspdata"))

(defun dbms-storage-table-path-for-root (root table-name)
  (string-append root "/table_" table-name ".lspdata"))

(defun dbms-storage-table-pages-path-for-root (root table-name)
  (string-append root "/table_" table-name ".pages.lspdata"))

(defun dbms-storage-auth-path-for-root (root)
  (string-append root "/auth.lspdata"))

(defun dbms-storage-audit-path-for-root (root)
  (string-append root "/audit.lspdata"))

(defun dbms-storage-audit-archive-path (seq)
  (string-append (dbms-storage-root) "/audit.archive." (format nil "~A" seq) ".lspdata"))

(defun dbms-storage-next-audit-archive-path ()
  (setq *dbms-audit-archive-seq* (+ *dbms-audit-archive-seq* 1))
  (dbms-storage-audit-archive-path *dbms-audit-archive-seq*))

(defun dbms-storage-shell-quote (s)
  (os-shell-quote s))

(defun dbms-storage-ensure-root ()
  (os-mkdir-p (dbms-storage-root)))

(defun dbms-storage-ensure-backup-root ()
  (os-mkdir-p (dbms-storage-backup-root)))

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
                  (setq out (cons v out))
                  (setq v (read s)))))
          (dbms-storage-reverse out)))))

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

(defun dbms-storage-list-append1 (xs x)
  (dbms-storage-reverse (cons x (dbms-storage-reverse xs))))

;; ------------------------------------------------------------------
;; LT-001: replication metadata + WAL shipping helpers
;; ------------------------------------------------------------------

(defun dbms-repl-follower-p (v)
  ;; (dbms-repl-follower <id> <root> <last-shipped-lsn> <last-applied-lsn> <sync-state>)
  (and (listp v)
       (= (length v) 6)
       (eq (first v) 'dbms-repl-follower)
       (stringp (second v))
       (stringp (third v))
       (numberp (fourth v))
       (numberp (fifth v))
       (stringp (dbms-sixth v))))

(defun dbms-make-repl-follower (follower-id follower-root last-shipped-lsn last-applied-lsn sync-state)
  (list 'dbms-repl-follower follower-id follower-root last-shipped-lsn last-applied-lsn sync-state))

(defun dbms-repl-follower-id (f)
  (if (dbms-repl-follower-p f) (second f) ""))

(defun dbms-repl-follower-root (f)
  (if (dbms-repl-follower-p f) (third f) ""))

(defun dbms-repl-meta-p (v)
  ;; (dbms-repl-meta <version> <mode> <followers>)
  (and (listp v)
       (= (length v) 4)
       (eq (first v) 'dbms-repl-meta)
       (numberp (second v))
       (stringp (third v))
       (listp (fourth v))))

(defun dbms-storage-empty-repl-meta ()
  (list 'dbms-repl-meta 1 "ASYNC" '()))

(defun dbms-storage-repl-mode (meta)
  (if (dbms-repl-meta-p meta)
      (third meta)
      "ASYNC"))

(defun dbms-storage-repl-followers (meta)
  (if (dbms-repl-meta-p meta)
      (fourth meta)
      '()))

(defun dbms-storage-load-repl-meta ()
  (let ((raw (dbms-storage-read-sexpr-file (dbms-storage-replication-path))))
    (if (dbms-repl-meta-p raw)
        raw
        (dbms-storage-empty-repl-meta))))

(defun dbms-storage-save-repl-meta (meta)
  (if (not (dbms-repl-meta-p meta))
      (dbms-make-error 'dbms/invalid-representation "replication meta must be dbms-repl-meta" meta)
      (dbms-storage-atomic-replace (dbms-storage-replication-path) meta)))

(defun dbms-storage-repl-find-follower (followers follower-id)
  (if (null followers)
      '()
      (if (and (dbms-repl-follower-p (car followers))
               (string= (dbms-repl-follower-id (car followers)) follower-id))
          (car followers)
          (dbms-storage-repl-find-follower (cdr followers) follower-id))))

(defun dbms-storage-repl-put-follower (followers follower)
  (if (null followers)
      (list follower)
      (if (and (dbms-repl-follower-p (car followers))
               (string= (dbms-repl-follower-id (car followers)) (dbms-repl-follower-id follower)))
          (cons follower (cdr followers))
          (cons (car followers) (dbms-storage-repl-put-follower (cdr followers) follower)))))

(defun dbms-storage-repl-set-mode (mode)
  (if (or (not (stringp mode))
          (and (not (string= mode "ASYNC"))
               (not (string= mode "SYNC"))))
      (dbms-make-error 'dbms/invalid-representation "replication mode must be ASYNC or SYNC" mode)
      (let* ((meta (dbms-storage-load-repl-meta))
             (next (list 'dbms-repl-meta
                         (second meta)
                         mode
                         (dbms-storage-repl-followers meta)))
             (saved (dbms-storage-save-repl-meta next)))
        (if (dbms-error-p saved) saved next))))

(defun dbms-storage-repl-register-follower (follower-id follower-root)
  (if (or (not (stringp follower-id)) (string= follower-id "")
          (not (stringp follower-root)) (string= follower-root ""))
      (dbms-make-error 'dbms/invalid-representation "follower-id/root must be non-empty string" (list follower-id follower-root))
      (let* ((meta (dbms-storage-load-repl-meta))
             (followers (dbms-storage-repl-followers meta))
             (current (dbms-storage-repl-find-follower followers follower-id))
             (nextf (if (dbms-repl-follower-p current)
                        (dbms-make-repl-follower follower-id follower-root (fourth current) (fifth current) (dbms-sixth current))
                        (dbms-make-repl-follower follower-id follower-root 0 0 "INIT")))
             (next-followers (dbms-storage-repl-put-follower followers nextf))
             (next (list 'dbms-repl-meta (second meta) (dbms-storage-repl-mode meta) next-followers))
             (saved (dbms-storage-save-repl-meta next)))
        (if (dbms-error-p saved) saved nextf))))

(defun dbms-storage-repl-update-follower-progress (follower-id shipped-lsn applied-lsn sync-state)
  (let* ((meta (dbms-storage-load-repl-meta))
         (followers (dbms-storage-repl-followers meta))
         (cur (dbms-storage-repl-find-follower followers follower-id)))
    (if (null cur)
        (dbms-make-error 'dbms/invalid-representation "follower not found" follower-id)
        (let* ((nextf (dbms-make-repl-follower follower-id
                                               (dbms-repl-follower-root cur)
                                               shipped-lsn
                                               applied-lsn
                                               sync-state))
               (next-followers (dbms-storage-repl-put-follower followers nextf))
               (next (list 'dbms-repl-meta (second meta) (dbms-storage-repl-mode meta) next-followers))
               (saved (dbms-storage-save-repl-meta next)))
          (if (dbms-error-p saved) saved nextf)))))

(defun dbms-storage-repl-ship-wal-to-root (target-root)
  (if (or (not (stringp target-root)) (string= target-root ""))
      (dbms-make-error 'dbms/invalid-representation "target-root must be non-empty string" target-root)
      (let* ((mk-status (if (os-mkdir-p target-root) 0 1))
             (records (dbms-storage-load-wal-records))
             (checkpoint (dbms-storage-load-checkpoint-meta))
             (saved-wal '())
             (saved-checkpoint '())
             (max-lsn (dbms-storage-wal-max-lsn)))
        (if (not (= mk-status 0))
            (dbms-make-error 'dbms/not-implemented "failed to create follower root" target-root)
            (progn
              (setq saved-wal (dbms-storage-atomic-replace (dbms-storage-wal-path-for-root target-root) records))
              (if (dbms-error-p saved-wal)
                  saved-wal
                  (progn
                    (setq saved-checkpoint
                          (dbms-storage-atomic-replace (dbms-storage-checkpoint-path-for-root target-root) checkpoint))
                    (if (dbms-error-p saved-checkpoint)
                        saved-checkpoint
                        (list 'dbms-repl-ship-report
                              (list "target-root" target-root)
                              (list "record-count" (length records))
                              (list "max-lsn" max-lsn))))))))))

;; ------------------------------------------------------------------
;; LT-002: auto failover metadata helpers
;; ------------------------------------------------------------------

(defun dbms-failover-meta-p (v)
  ;; (dbms-failover-meta <version> <enabled> <ttl-sec> <last-heartbeat-at> <lease-owner> <lease-until> <fence-epoch>)
  (and (listp v)
       (= (length v) 8)
       (eq (first v) 'dbms-failover-meta)
       (numberp (second v))
       (or (eq (third v) t) (null (third v)))
       (numberp (fourth v))
       (numberp (fifth v))
       (stringp (dbms-sixth v))
       (numberp (dbms-seventh v))
       (numberp (dbms-eighth v))))

(defun dbms-storage-empty-failover-meta ()
  (list 'dbms-failover-meta 1 nil 30 0 "primary" 0 0))

(defun dbms-storage-load-failover-meta ()
  (let ((raw (dbms-storage-read-sexpr-file (dbms-storage-failover-path))))
    (if (dbms-failover-meta-p raw)
        raw
        (dbms-storage-empty-failover-meta))))

(defun dbms-storage-save-failover-meta (meta)
  (if (not (dbms-failover-meta-p meta))
      (dbms-make-error 'dbms/invalid-representation "failover meta must be dbms-failover-meta" meta)
      (dbms-storage-atomic-replace (dbms-storage-failover-path) meta)))

(defun dbms-storage-failover-enabled-p (meta)
  (if (dbms-failover-meta-p meta)
      (third meta)
      nil))

(defun dbms-storage-failover-ttl-sec (meta)
  (if (dbms-failover-meta-p meta)
      (fourth meta)
      30))

(defun dbms-storage-failover-last-heartbeat-at (meta)
  (if (dbms-failover-meta-p meta)
      (fifth meta)
      0))

(defun dbms-storage-failover-lease-owner (meta)
  (if (dbms-failover-meta-p meta)
      (dbms-sixth meta)
      ""))

(defun dbms-storage-failover-lease-until (meta)
  (if (dbms-failover-meta-p meta)
      (dbms-seventh meta)
      0))

(defun dbms-storage-failover-fence-epoch (meta)
  (if (dbms-failover-meta-p meta)
      (dbms-eighth meta)
      0))

(defun dbms-storage-failover-upsert (enabled ttl-sec heartbeat-at lease-owner lease-until fence-epoch)
  (let* ((cur (dbms-storage-load-failover-meta))
         (next (list 'dbms-failover-meta
                     (second cur)
                     (if (or (eq enabled t) (null enabled)) enabled nil)
                     (if (and (numberp ttl-sec) (> ttl-sec 0)) ttl-sec (dbms-storage-failover-ttl-sec cur))
                     (if (and (numberp heartbeat-at) (>= heartbeat-at 0)) heartbeat-at (dbms-storage-failover-last-heartbeat-at cur))
                     (if (and (stringp lease-owner) (not (string= lease-owner "")))
                         lease-owner
                         (dbms-storage-failover-lease-owner cur))
                     (if (and (numberp lease-until) (>= lease-until 0)) lease-until (dbms-storage-failover-lease-until cur))
                     (if (and (numberp fence-epoch) (>= fence-epoch 0)) fence-epoch (dbms-storage-failover-fence-epoch cur))))
         (saved (dbms-storage-save-failover-meta next)))
    (if (dbms-error-p saved) saved next)))

;; ------------------------------------------------------------------
;; LT-003: storage maintenance metadata + corruption check helpers
;; ------------------------------------------------------------------

(defun dbms-maint-table-state-p (v)
  ;; (dbms-maint-table <name> <last-analyze-at> <last-vacuum-at> <last-frag-ratio> <last-row-count>)
  (and (listp v)
       (= (length v) 6)
       (eq (first v) 'dbms-maint-table)
       (stringp (second v))
       (numberp (third v))
       (numberp (fourth v))
       (numberp (fifth v))
       (numberp (dbms-sixth v))))

(defun dbms-make-maint-table-state (table-name last-analyze-at last-vacuum-at last-frag-ratio last-row-count)
  (list 'dbms-maint-table table-name last-analyze-at last-vacuum-at last-frag-ratio last-row-count))

(defun dbms-maint-table-name (ts)
  (if (dbms-maint-table-state-p ts) (second ts) ""))

(defun dbms-maint-meta-p (v)
  ;; (dbms-maint-meta <version> <enabled> <vacuum-frag-threshold> <analyze-interval-sec> <vacuum-interval-sec>
  ;;                 <corruption-check-interval-sec> <last-corruption-check-at> <max-vacuum-tables-per-tick> <table-states>)
  (and (listp v)
       (= (length v) 10)
       (eq (first v) 'dbms-maint-meta)
       (numberp (second v))
       (or (eq (third v) t) (null (third v)))
       (numberp (fourth v))
       (numberp (fifth v))
       (numberp (dbms-sixth v))
       (numberp (dbms-seventh v))
       (numberp (dbms-eighth v))
       (numberp (dbms-ninth v))
       (listp (dbms-tenth v))))

(defun dbms-storage-empty-maint-meta ()
  (list 'dbms-maint-meta 1 nil 0.25 3600 1800 900 0 2 '()))

(defun dbms-storage-load-maint-meta ()
  (let ((raw (dbms-storage-read-sexpr-file (dbms-storage-maintenance-path))))
    (if (dbms-maint-meta-p raw)
        raw
        (dbms-storage-empty-maint-meta))))

(defun dbms-storage-save-maint-meta (meta)
  (if (not (dbms-maint-meta-p meta))
      (dbms-make-error 'dbms/invalid-representation "maintenance meta must be dbms-maint-meta" meta)
      (dbms-storage-atomic-replace (dbms-storage-maintenance-path) meta)))

(defun dbms-storage-maint-enabled-p (meta)
  (if (dbms-maint-meta-p meta) (third meta) nil))

(defun dbms-storage-maint-vacuum-frag-threshold (meta)
  (if (dbms-maint-meta-p meta) (fourth meta) 0.25))

(defun dbms-storage-maint-analyze-interval-sec (meta)
  (if (dbms-maint-meta-p meta) (fifth meta) 3600))

(defun dbms-storage-maint-vacuum-interval-sec (meta)
  (if (dbms-maint-meta-p meta) (dbms-sixth meta) 1800))

(defun dbms-storage-maint-corruption-check-interval-sec (meta)
  (if (dbms-maint-meta-p meta) (dbms-seventh meta) 900))

(defun dbms-storage-maint-last-corruption-check-at (meta)
  (if (dbms-maint-meta-p meta) (dbms-eighth meta) 0))

(defun dbms-storage-maint-max-vacuum-tables-per-tick (meta)
  (if (dbms-maint-meta-p meta) (dbms-ninth meta) 2))

(defun dbms-storage-maint-table-states (meta)
  (if (dbms-maint-meta-p meta) (dbms-tenth meta) '()))

(defun dbms-storage-maint-find-table-state (table-states table-name)
  (if (null table-states)
      '()
      (if (and (dbms-maint-table-state-p (car table-states))
               (string= (dbms-maint-table-name (car table-states)) table-name))
          (car table-states)
          (dbms-storage-maint-find-table-state (cdr table-states) table-name))))

(defun dbms-storage-maint-put-table-state (table-states state)
  (if (null table-states)
      (list state)
      (if (and (dbms-maint-table-state-p (car table-states))
               (string= (dbms-maint-table-name (car table-states))
                        (dbms-maint-table-name state)))
          (cons state (cdr table-states))
          (cons (car table-states)
                (dbms-storage-maint-put-table-state (cdr table-states) state)))))

(defun dbms-storage-maint-configure (enabled vacuum-frag-threshold analyze-interval-sec vacuum-interval-sec corruption-check-interval-sec max-vacuum-tables-per-tick)
  (let* ((cur (dbms-storage-load-maint-meta))
         (next (list 'dbms-maint-meta
                     (second cur)
                     (if (or (eq enabled t) (null enabled)) enabled nil)
                     (if (and (numberp vacuum-frag-threshold)
                              (>= vacuum-frag-threshold 0)
                              (<= vacuum-frag-threshold 1))
                         vacuum-frag-threshold
                         (dbms-storage-maint-vacuum-frag-threshold cur))
                     (if (and (numberp analyze-interval-sec) (> analyze-interval-sec 0))
                         analyze-interval-sec
                         (dbms-storage-maint-analyze-interval-sec cur))
                     (if (and (numberp vacuum-interval-sec) (> vacuum-interval-sec 0))
                         vacuum-interval-sec
                         (dbms-storage-maint-vacuum-interval-sec cur))
                     (if (and (numberp corruption-check-interval-sec) (> corruption-check-interval-sec 0))
                         corruption-check-interval-sec
                         (dbms-storage-maint-corruption-check-interval-sec cur))
                     (dbms-storage-maint-last-corruption-check-at cur)
                     (if (and (numberp max-vacuum-tables-per-tick) (> max-vacuum-tables-per-tick 0))
                         max-vacuum-tables-per-tick
                         (dbms-storage-maint-max-vacuum-tables-per-tick cur))
                     (dbms-storage-maint-table-states cur)))
         (saved (dbms-storage-save-maint-meta next)))
    (if (dbms-error-p saved) saved next)))

(defun dbms-storage-maint-touch-table-state (table-name now-sec last-analyze-at last-vacuum-at last-frag-ratio last-row-count)
  (let* ((meta (dbms-storage-load-maint-meta))
         (states (dbms-storage-maint-table-states meta))
         (cur (dbms-storage-maint-find-table-state states table-name))
         (next-state (dbms-make-maint-table-state table-name
                                                  (if (and (numberp last-analyze-at) (>= last-analyze-at 0))
                                                      last-analyze-at
                                                      (if (dbms-maint-table-state-p cur) (third cur) 0))
                                                  (if (and (numberp last-vacuum-at) (>= last-vacuum-at 0))
                                                      last-vacuum-at
                                                      (if (dbms-maint-table-state-p cur) (fourth cur) 0))
                                                  (if (and (numberp last-frag-ratio) (>= last-frag-ratio 0))
                                                      last-frag-ratio
                                                      (if (dbms-maint-table-state-p cur) (fifth cur) 0))
                                                  (if (and (numberp last-row-count) (>= last-row-count 0))
                                                      last-row-count
                                                      (if (dbms-maint-table-state-p cur) (dbms-sixth cur) 0))))
         (next-states (dbms-storage-maint-put-table-state states next-state))
         (next (list 'dbms-maint-meta
                     (second meta)
                     (dbms-storage-maint-enabled-p meta)
                     (dbms-storage-maint-vacuum-frag-threshold meta)
                     (dbms-storage-maint-analyze-interval-sec meta)
                     (dbms-storage-maint-vacuum-interval-sec meta)
                     (dbms-storage-maint-corruption-check-interval-sec meta)
                     (if (and (numberp now-sec) (>= now-sec 0))
                         now-sec
                         (dbms-storage-maint-last-corruption-check-at meta))
                     (dbms-storage-maint-max-vacuum-tables-per-tick meta)
                     next-states))
         (saved (dbms-storage-save-maint-meta next)))
    (if (dbms-error-p saved) saved next)))

(defun dbms-storage-maint-set-last-corruption-check-at (now-sec)
  (let* ((meta (dbms-storage-load-maint-meta))
         (next (list 'dbms-maint-meta
                     (second meta)
                     (dbms-storage-maint-enabled-p meta)
                     (dbms-storage-maint-vacuum-frag-threshold meta)
                     (dbms-storage-maint-analyze-interval-sec meta)
                     (dbms-storage-maint-vacuum-interval-sec meta)
                     (dbms-storage-maint-corruption-check-interval-sec meta)
                     (if (and (numberp now-sec) (>= now-sec 0)) now-sec (dbms-storage-maint-last-corruption-check-at meta))
                     (dbms-storage-maint-max-vacuum-tables-per-tick meta)
                     (dbms-storage-maint-table-states meta)))
         (saved (dbms-storage-save-maint-meta next)))
    (if (dbms-error-p saved) saved next)))

(defun dbms-storage-page-corruption-errors (table-name page-id page slot-capacity)
  (let* ((header (second page))
         (slots (third page))
         (expected-checksum (dbms-page-checksum slots))
         (actual-checksum (fourth header))
         (expected-free (dbms-count-free-slots slots))
         (actual-free (fifth header))
         (errors '()))
    (if (not (= (length slots) slot-capacity))
        (setq errors (append errors (list (list "table" table-name)
                                          (list "page-id" page-id)
                                          (list "kind" "SLOT_CAPACITY_MISMATCH")
                                          (list "expected" slot-capacity)
                                          (list "actual" (length slots))))))
    (if (not (= expected-checksum actual-checksum))
        (setq errors (append errors (list (list "table" table-name)
                                          (list "page-id" page-id)
                                          (list "kind" "CHECKSUM_MISMATCH")
                                          (list "expected" expected-checksum)
                                          (list "actual" actual-checksum)))))
    (if (not (= expected-free actual-free))
        (setq errors (append errors (list (list "table" table-name)
                                          (list "page-id" page-id)
                                          (list "kind" "FREE_SPACE_MISMATCH")
                                          (list "expected" expected-free)
                                          (list "actual" actual-free)))))
    errors))

(defun dbms-storage-check-table-page-integrity (table-name)
  (let ((pf (dbms-storage-load-table-pages-file table-name)))
    (if (not (dbms-data-page-file-p pf))
        (list (list "table" table-name)
              (list "kind" "PAGE_FILE_INVALID"))
        (let* ((slot-capacity (fourth pf))
               (errors '()))
          (dolist (pair (dbms-data-page-file-pages pf))
            (if (and (listp pair) (= (length pair) 2) (dbms-data-page-p (second pair)))
                (setq errors (append errors
                                     (dbms-storage-page-corruption-errors table-name
                                                                          (first pair)
                                                                          (second pair)
                                                                          slot-capacity)))
                (setq errors (append errors
                                     (list (list (list "table" table-name)
                                                 (list "page-id" (if (and (listp pair) (not (null pair))) (first pair) -1))
                                                 (list "kind" "PAGE_PAIR_INVALID")))))))
          errors))))

(defun dbms-storage-check-catalog-integrity (catalog)
  (let ((tables (dbms-catalog-table-names catalog))
        (errors '()))
    (dolist (name tables)
      (setq errors (append errors (dbms-storage-check-table-page-integrity name))))
    errors))

(defun dbms-checkpoint-meta-p (v)
  ;; (dbms-checkpoint <version> <last-lsn> <created-at> <max-txid>)
  (and (listp v)
       (= (length v) 5)
       (eq (first v) 'dbms-checkpoint)
       (numberp (second v))
       (numberp (third v))
       (>= (third v) 0)
       (numberp (fourth v))
       (numberp (fifth v))
       (>= (fifth v) 0)))

(defun dbms-make-checkpoint-meta (version last-lsn created-at max-txid)
  (list 'dbms-checkpoint version last-lsn created-at max-txid))

(defun dbms-storage-empty-checkpoint-meta ()
  (dbms-make-checkpoint-meta 1 0 0 0))

(defun dbms-storage-load-checkpoint-meta ()
  (let ((raw (dbms-storage-read-sexpr-file (dbms-storage-checkpoint-path))))
    (if (dbms-checkpoint-meta-p raw)
        raw
        (dbms-storage-empty-checkpoint-meta))))

(defun dbms-storage-save-checkpoint-meta (checkpoint)
  (if (dbms-checkpoint-meta-p checkpoint)
      (dbms-storage-atomic-replace (dbms-storage-checkpoint-path) checkpoint)
      (dbms-make-error 'dbms/invalid-representation "checkpoint must be dbms-checkpoint" checkpoint)))

(defun dbms-storage-checkpoint-last-lsn (checkpoint)
  (if (dbms-checkpoint-meta-p checkpoint)
      (third checkpoint)
      0))

(defun dbms-storage-checkpoint-max-txid (checkpoint)
  (if (dbms-checkpoint-meta-p checkpoint)
      (fifth checkpoint)
      0))

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
            (setq out (cons (car raw) out))
            (setq raw (cdr raw)))
          (setq done t)))
    (dbms-storage-reverse out)))

(defun dbms-storage-wal-max-txid ()
  (let ((records (dbms-storage-load-wal-records))
        (mx 0)
        (checkpoint (dbms-storage-load-checkpoint-meta)))
    (if (> (dbms-storage-checkpoint-max-txid checkpoint) mx)
        (setq mx (dbms-storage-checkpoint-max-txid checkpoint))
        nil)
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

(defun dbms-storage-wal-max-lsn ()
  (- (dbms-storage-wal-next-lsn) 1))

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
              (setq records (dbms-storage-list-append1 records record))
              (let ((saved (dbms-storage-atomic-replace (dbms-storage-wal-path) records)))
                (if (dbms-error-p saved)
                    saved
                    record)))))))

(defun dbms-backup-generation-p (v)
  ;; (dbms-backup-generation <id> <created-at> <base-lsn> <max-lsn> <dump-path> <wal-path>)
  (and (listp v)
       (= (length v) 7)
       (eq (first v) 'dbms-backup-generation)
       (stringp (second v))
       (numberp (third v))
       (numberp (fourth v))
       (numberp (fifth v))
       (stringp (dbms-sixth v))
       (stringp (dbms-seventh v))
       (<= (fourth v) (fifth v))))

(defun dbms-backup-generation-id (entry)
  (if (dbms-backup-generation-p entry) (second entry) ""))

(defun dbms-backup-generation-created-at (entry)
  (if (dbms-backup-generation-p entry) (third entry) 0))

(defun dbms-backup-generation-base-lsn (entry)
  (if (dbms-backup-generation-p entry) (fourth entry) 0))

(defun dbms-backup-generation-max-lsn (entry)
  (if (dbms-backup-generation-p entry) (fifth entry) 0))

(defun dbms-backup-generation-dump-path (entry)
  (if (dbms-backup-generation-p entry) (dbms-sixth entry) ""))

(defun dbms-backup-generation-wal-path (entry)
  (if (dbms-backup-generation-p entry) (dbms-seventh entry) ""))

(defun dbms-make-backup-generation (generation-id created-at base-lsn max-lsn dump-path wal-path)
  (list 'dbms-backup-generation generation-id created-at base-lsn max-lsn dump-path wal-path))

(defun dbms-backup-index-p (v)
  ;; (dbms-backup-index <version> <generations>)
  (and (listp v)
       (= (length v) 3)
       (eq (first v) 'dbms-backup-index)
       (numberp (second v))
       (listp (third v))))

(defun dbms-storage-empty-backup-index ()
  (list 'dbms-backup-index 1 '()))

(defun dbms-storage-backup-index-generations (index)
  (if (dbms-backup-index-p index)
      (third index)
      '()))

(defun dbms-storage-load-backup-index ()
  (let ((raw (dbms-storage-read-sexpr-file (dbms-storage-backup-index-path))))
    (if (dbms-backup-index-p raw)
        raw
        (dbms-storage-empty-backup-index))))

(defun dbms-storage-save-backup-index (index)
  (if (not (dbms-backup-index-p index))
      (dbms-make-error 'dbms/invalid-representation "backup index must be dbms-backup-index" index)
      (if (not (dbms-storage-ensure-backup-root))
          (dbms-make-error 'dbms/not-implemented "failed to create backup root" (dbms-storage-backup-root))
          (dbms-storage-atomic-replace (dbms-storage-backup-index-path) index))))

(defun dbms-storage-next-backup-generation-id ()
  (setq *dbms-backup-generation-seq* (+ *dbms-backup-generation-seq* 1))
  (string-append (format nil "~A" (get-universal-time))
                 "-"
                 (format nil "~A" *dbms-backup-generation-seq*)))

(defun dbms-storage-backup-retention-keep-count ()
  (let ((v (getenv "DBMS_BACKUP_KEEP_COUNT")))
    (if (and (stringp v)
             (not (string= v ""))
             (dbms-storage-digits-only-p v))
        (dbms-storage-parse-nonnegative-int v)
        0)))

(defun dbms-storage-backup-retention-keep-days ()
  (let ((v (getenv "DBMS_BACKUP_KEEP_DAYS")))
    (if (and (stringp v)
             (not (string= v ""))
             (dbms-storage-digits-only-p v))
        (dbms-storage-parse-nonnegative-int v)
        0)))

(defun dbms-storage-string-in-list-p (s xs)
  (if (null xs)
      nil
      (if (string= s (car xs))
          t
          (dbms-storage-string-in-list-p s (cdr xs)))))

(defun dbms-storage-string-list-add-unique (xs s)
  (if (dbms-storage-string-in-list-p s xs)
      xs
      (append xs (list s))))

(defun dbms-storage-backup-delete-generation-files (entry)
  (let ((dump-path (dbms-backup-generation-dump-path entry))
        (wal-path (dbms-backup-generation-wal-path entry)))
    (if (not (null (probe-file dump-path)))
        (os-rm-f dump-path)
        nil)
    (if (not (null (probe-file wal-path)))
        (os-rm-f wal-path)
        nil)
    'ok))

(defun dbms-storage-prune-backup-generations (keep-count keep-days)
  (if (or (not (numberp keep-count))
          (not (numberp keep-days))
          (< keep-count 0)
          (< keep-days 0))
      (dbms-make-error 'dbms/invalid-representation "keep-count/keep-days must be non-negative numbers" (list keep-count keep-days))
      (let* ((index (dbms-storage-load-backup-index))
             (entries (dbms-storage-backup-index-generations index))
             (total (length entries))
             (overflow (if (and (> keep-count 0) (> total keep-count))
                           (- total keep-count)
                           0))
             (cutoff (if (> keep-days 0)
                         (- (get-universal-time) (* keep-days 86400))
                         0))
             (rest entries)
             (i 0)
             (delete-ids '())
             (kept '())
             (deleted 0))
        (while (not (null rest))
          (let* ((entry (car rest))
                 (gid (dbms-backup-generation-id entry))
                 (created-at (dbms-backup-generation-created-at entry))
                 (delete-by-count (< i overflow))
                 (delete-by-days (if (> keep-days 0)
                                     (< created-at cutoff)
                                     nil)))
            (if (or delete-by-count delete-by-days)
                (setq delete-ids (dbms-storage-string-list-add-unique delete-ids gid))
                nil))
          (setq i (+ i 1))
          (setq rest (cdr rest)))
        (setq rest entries)
        (while (not (null rest))
          (let* ((entry (car rest))
                 (gid (dbms-backup-generation-id entry)))
            (if (dbms-storage-string-in-list-p gid delete-ids)
                (progn
                  (dbms-storage-backup-delete-generation-files entry)
                  (setq deleted (+ deleted 1)))
                (setq kept (append kept (list entry)))))
          (setq rest (cdr rest)))
        (let* ((next-index (list 'dbms-backup-index (second index) kept))
               (saved (dbms-storage-save-backup-index next-index)))
          (if (dbms-error-p saved)
              saved
              (list 'dbms-backup-prune-report
                    (list "before-count" total)
                    (list "after-count" (length kept))
                    (list "deleted-count" deleted)
                    (list "deleted-ids" delete-ids)))))))

(defun dbms-storage-create-backup-generation (dump-payload)
  (if (not (dbms-storage-ensure-backup-root))
      (dbms-make-error 'dbms/not-implemented "failed to create backup root" (dbms-storage-backup-root))
      (let* ((generation-id (dbms-storage-next-backup-generation-id))
             (created-at (get-universal-time))
             (wal-records (dbms-storage-load-wal-records))
             (max-lsn (dbms-storage-wal-max-lsn))
             (dump-path (dbms-storage-backup-dump-path generation-id))
             (wal-path (dbms-storage-backup-wal-path generation-id))
             (entry (dbms-make-backup-generation generation-id
                                                created-at
                                                max-lsn
                                                max-lsn
                                                dump-path
                                                wal-path))
             (saved-dump (dbms-storage-atomic-replace dump-path dump-payload)))
        (if (dbms-error-p saved-dump)
            saved-dump
            (let ((saved-wal (dbms-storage-atomic-replace wal-path wal-records)))
              (if (dbms-error-p saved-wal)
                  saved-wal
                  (let* ((index (dbms-storage-load-backup-index))
                         (next-index (list 'dbms-backup-index
                                           (second index)
                                           (append (dbms-storage-backup-index-generations index)
                                                   (list entry))))
                         (saved-index (dbms-storage-save-backup-index next-index)))
                    (if (dbms-error-p saved-index)
                        saved-index
                        (let ((pruned (dbms-storage-prune-backup-generations
                                       (dbms-storage-backup-retention-keep-count)
                                       (dbms-storage-backup-retention-keep-days))))
                          (if (dbms-error-p pruned)
                              pruned
                              entry))))))))))

(defun dbms-storage-list-backup-generations ()
  (dbms-storage-backup-index-generations (dbms-storage-load-backup-index)))

(defun dbms-storage-find-backup-generation (generation-id)
  (let ((rest (dbms-storage-list-backup-generations))
        (found '()))
    (while (and (null found) (not (null rest)))
      (if (string= (dbms-backup-generation-id (car rest)) generation-id)
          (setq found (car rest))
          nil)
      (setq rest (cdr rest)))
    found))

(defun dbms-storage-load-backup-wal-records (generation-id)
  (let ((entry (dbms-storage-find-backup-generation generation-id)))
    (if (null entry)
        (dbms-make-error 'dbms/invalid-representation "backup generation not found" generation-id)
        (let ((raw (dbms-storage-read-sexpr-file (dbms-backup-generation-wal-path entry)))
              (out '())
              (done nil))
          (if (or (null raw) (not (listp raw)))
              (setq raw '())
              nil)
          (while (and (not done) (not (null raw)))
            (if (dbms-wal-record-p (car raw))
                (progn
                  (setq out (cons (car raw) out))
                  (setq raw (cdr raw)))
                (setq done t)))
          (dbms-storage-reverse out)))))

(defun dbms-storage-save-wal-records (records)
  (if (not (listp records))
      (dbms-make-error 'dbms/invalid-representation "records must be list" records)
      (let ((rest records)
            (ok t))
        (while (and ok (not (null rest)))
          (if (dbms-wal-record-p (car rest))
              (setq rest (cdr rest))
              (setq ok nil)))
        (if (not ok)
            (dbms-make-error 'dbms/invalid-representation "records must contain dbms-wal-record only" records)
            (dbms-storage-atomic-replace (dbms-storage-wal-path) records)))))

(defun dbms-storage-wal-records-after-lsn (records lsn)
  (let ((rest records)
        (out '()))
    (while (not (null rest))
      (if (> (dbms-wal-record-lsn (car rest)) lsn)
          (setq out (cons (car rest) out))
          nil)
      (setq rest (cdr rest)))
    (dbms-storage-reverse out)))

(defun dbms-storage-create-checkpoint ()
  (let* ((last-lsn (dbms-storage-wal-max-lsn))
         (max-txid (dbms-storage-wal-max-txid))
         (checkpoint (dbms-make-checkpoint-meta 1
                                                last-lsn
                                                (get-universal-time)
                                                max-txid))
         (saved (dbms-storage-save-checkpoint-meta checkpoint)))
    (if (dbms-error-p saved)
        saved
        checkpoint)))

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

(defun dbms-security-credential-p (v)
  ;; (dbms-credential <user> <salt> <rounds> <digest>)
  (and (listp v)
       (= (length v) 5)
       (eq (first v) 'dbms-credential)
       (stringp (second v))
       (stringp (third v))
       (numberp (fourth v))
       (stringp (fifth v))))

(defun dbms-make-security-credential (user salt rounds digest)
  (list 'dbms-credential user salt rounds digest))

(defun dbms-security-meta-p (v)
  ;; (dbms-security-meta <version> <kdf-spec> <credentials> <tls-mode> <cert-path> <key-path> <ca-path> <audit-key>)
  (and (listp v)
       (= (length v) 9)
       (eq (first v) 'dbms-security-meta)
       (numberp (second v))
       (stringp (third v))
       (listp (fourth v))
       (stringp (fifth v))
       (stringp (dbms-sixth v))
       (stringp (dbms-seventh v))
       (stringp (dbms-eighth v))
       (stringp (dbms-ninth v))))

(defun dbms-storage-empty-security-meta ()
  (list 'dbms-security-meta 1 "KDF-V1-32" '() "DISABLED" "" "" "" "dbms-audit-bootstrap-key"))

(defun dbms-storage-load-security-meta ()
  (let ((raw (dbms-storage-read-sexpr-file (dbms-storage-security-path))))
    (if (dbms-security-meta-p raw)
        raw
        (dbms-storage-empty-security-meta))))

(defun dbms-storage-save-security-meta (meta)
  (if (dbms-security-meta-p meta)
      (dbms-storage-atomic-replace (dbms-storage-security-path) meta)
      (dbms-make-error 'dbms/invalid-representation "security meta must be dbms-security-meta" meta)))

(defun dbms-security-meta-kdf-spec (meta)
  (if (dbms-security-meta-p meta) (third meta) "KDF-V1-32"))

(defun dbms-security-meta-credentials (meta)
  (if (dbms-security-meta-p meta) (fourth meta) '()))

(defun dbms-security-meta-tls-mode (meta)
  (if (dbms-security-meta-p meta) (fifth meta) "DISABLED"))

(defun dbms-security-meta-cert-path (meta)
  (if (dbms-security-meta-p meta) (dbms-sixth meta) ""))

(defun dbms-security-meta-key-path (meta)
  (if (dbms-security-meta-p meta) (dbms-seventh meta) ""))

(defun dbms-security-meta-ca-path (meta)
  (if (dbms-security-meta-p meta) (dbms-eighth meta) ""))

(defun dbms-security-meta-audit-key (meta)
  (if (dbms-security-meta-p meta) (dbms-ninth meta) "dbms-audit-bootstrap-key"))

(defun dbms-storage-security-find-credential (credentials user)
  (if (null credentials)
      '()
      (if (and (dbms-security-credential-p (car credentials))
               (string= (second (car credentials)) user))
          (car credentials)
          (dbms-storage-security-find-credential (cdr credentials) user))))

(defun dbms-storage-security-put-credential (credentials cred)
  (if (null credentials)
      (list cred)
      (if (and (dbms-security-credential-p (car credentials))
               (string= (second (car credentials)) (second cred)))
          (cons cred (cdr credentials))
          (cons (car credentials) (dbms-storage-security-put-credential (cdr credentials) cred)))))

(defun dbms-storage-audit-record-p (v)
  ;; legacy: (dbms-audit-record ts user action object result detail)
  ;; signed : (dbms-audit-record ts user action object result detail prev-hash entry-hash)
  (and (listp v)
       (or (= (length v) 7) (= (length v) 9))
       (eq (first v) 'dbms-audit-record)
       (numberp (second v))
       (stringp (third v))))

(defun dbms-storage-audit-record-body (record)
  (list (second record)
        (third record)
        (fourth record)
        (fifth record)
        (dbms-sixth record)
        (dbms-seventh record)))

(defun dbms-storage-audit-record-prev-hash (record)
  (if (and (dbms-storage-audit-record-p record) (= (length record) 9))
      (dbms-eighth record)
      ""))

(defun dbms-storage-audit-record-entry-hash (record)
  (if (and (dbms-storage-audit-record-p record) (= (length record) 9))
      (dbms-ninth record)
      ""))

(defun dbms-storage-string-last (xs)
  (if (null (cdr xs))
      (car xs)
      (dbms-storage-string-last (cdr xs))))

(defun dbms-storage-audit-char-code (ch)
  (let ((idx (string-index ch " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~")))
    (if (null idx) 0 (+ idx 32))))

(defun dbms-storage-audit-mod (n m)
  (- n (* m (truncate (/ n m)))))

(defun dbms-storage-audit-hash-string (s)
  (let ((i 0)
        (n (length s))
        (acc 216613626))
    (while (< i n)
      (setq acc (+ (* acc 16777619)
                   (dbms-storage-audit-char-code (substring s i (+ i 1)))))
      (setq acc (dbms-storage-audit-mod acc 2147483647))
      (if (< acc 0)
          (setq acc (- 0 acc))
          nil)
      (setq i (+ i 1)))
    (format nil "~X" acc)))

(defun dbms-storage-audit-entry-hash (audit-key prev-hash record)
  (dbms-storage-audit-hash-string
   (string-append audit-key "|" prev-hash "|" (format nil "~S" (dbms-storage-audit-record-body record)))))

(defun dbms-storage-audit-sign-record (audit-key prev-hash record)
  (let ((entry-hash (dbms-storage-audit-entry-hash audit-key prev-hash record)))
    (list 'dbms-audit-record
          (second record)
          (third record)
          (fourth record)
          (fifth record)
          (dbms-sixth record)
          (dbms-seventh record)
          prev-hash
          entry-hash)))

(defun dbms-storage-audit-normalize-log-with-key (entries audit-key)
  (let ((rest entries)
        (prev-hash "")
        (out '())
        (tampered '()))
    (while (and (null tampered) (not (null rest)))
      (let* ((entry (car rest))
             (signed (if (= (length entry) 9)
                         entry
                         (dbms-storage-audit-sign-record audit-key prev-hash entry)))
             (expected (dbms-storage-audit-entry-hash audit-key
                                                     (dbms-storage-audit-record-prev-hash signed)
                                                     signed)))
        (if (or (not (dbms-storage-audit-record-p signed))
                (not (string= (dbms-storage-audit-record-prev-hash signed) prev-hash))
                (not (string= (dbms-storage-audit-record-entry-hash signed) expected)))
            (setq tampered (dbms-make-error 'dbms/audit-tamper-detected "audit log tamper detected" signed))
            (progn
              (setq out (append out (list signed)))
              (setq prev-hash (dbms-storage-audit-record-entry-hash signed)))))
      (setq rest (cdr rest)))
    (if (dbms-error-p tampered) tampered out)))

(defun dbms-storage-audit-archive-list-from-seq (seq)
  (let ((path (dbms-storage-audit-archive-path seq)))
    (if (null (probe-file path))
        '()
        (cons path (dbms-storage-audit-archive-list-from-seq (+ seq 1))))))

(defun dbms-storage-audit-archive-paths ()
  (dbms-storage-audit-archive-list-from-seq 1))

(defun dbms-storage-audit-verify-chain ()
  (let* ((security (dbms-storage-load-security-meta))
         (audit-key (dbms-security-meta-audit-key security))
         (paths (append (dbms-storage-audit-archive-paths)
                        (list (dbms-storage-audit-path))))
         (last-hash "")
         (checked 0)
         (failed '()))
    (dolist (path paths)
      (if (dbms-error-p failed)
          nil
          (let* ((entries (dbms-storage-read-sexpr-file path))
                 (norm (if (listp entries)
                           (dbms-storage-audit-normalize-log-with-key entries audit-key)
                           '())))
            (if (dbms-error-p norm)
                (setq failed norm)
                (progn
                  (if (not (null norm))
                      (progn
                        (if (not (string= (dbms-storage-audit-record-prev-hash (car norm)) last-hash))
                            (setq failed (dbms-make-error 'dbms/audit-tamper-detected "audit chain boundary mismatch" path))
                            (setq last-hash (dbms-storage-audit-record-entry-hash (dbms-storage-string-last norm))))
                        (setq checked (+ checked (length norm))))
                      nil))))))
    (if (dbms-error-p failed)
        failed
        (list 'dbms-audit-verify-report
              (list "ok" t)
              (list "record-count" checked)
              (list "tail-hash" last-hash)))))

(defun dbms-storage-load-audit-log ()
  (let ((raw (dbms-storage-read-sexpr-file (dbms-storage-audit-path))))
    (if (listp raw) raw '())))

(defun dbms-storage-append-audit-record (record)
  (let* ((security (dbms-storage-load-security-meta))
         (audit-key (dbms-security-meta-audit-key security))
         (entries (dbms-storage-load-audit-log))
         (normalized (dbms-storage-audit-normalize-log-with-key entries audit-key)))
    (if (dbms-error-p normalized)
        normalized
        (let* ((prev-hash (if (null normalized)
                              ""
                              (dbms-storage-audit-record-entry-hash (dbms-storage-string-last normalized))))
               (signed (dbms-storage-audit-sign-record audit-key prev-hash record))
               (merged (append normalized (list signed)))
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
              (dbms-storage-atomic-replace (dbms-storage-audit-path) merged))))))

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
  (let* ((all-records (dbms-storage-load-wal-records))
         (checkpoint (dbms-storage-load-checkpoint-meta))
         (checkpoint-lsn (dbms-storage-checkpoint-last-lsn checkpoint))
         (records (dbms-storage-wal-records-after-lsn all-records checkpoint-lsn))
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
              (list "checkpoint-lsn" checkpoint-lsn)
              (list "wal-total-record-count" (length all-records))
              (list "wal-scanned-record-count" (length records))
              (list "committed-tx-count" (length committed))
              (list "applied-data-record-count" applied)))))

(defun dbms-storage-replay-wal-records-up-to-lsn (records target-lsn)
  (if (or (not (numberp target-lsn)) (< target-lsn 0))
      (dbms-make-error 'dbms/invalid-representation "target-lsn must be non-negative number" target-lsn)
      (let ((rest records)
            (selected '())
            (applied 0)
            (failed '())
            (committed '()))
        (while (not (null rest))
          (if (<= (dbms-wal-record-lsn (car rest)) target-lsn)
              (setq selected (cons (car rest) selected))
              nil)
          (setq rest (cdr rest)))
        (setq selected (dbms-storage-reverse selected))
        (setq committed (dbms-storage-wal-committed-txids selected))
        (dolist (r selected)
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
                  (list "target-lsn" target-lsn)
                  (list "committed-tx-count" (length committed))
                  (list "applied-data-record-count" applied))))))

(defun dbms-storage-atomic-replace (target-path value)
  (if (not (dbms-storage-ensure-root))
      (dbms-make-error 'dbms/not-implemented "failed to create storage root" target-path)
      (let* ((tmp-path (dbms-storage-next-tmp-path target-path))
             (tmp-write (dbms-storage-write-sexpr-file tmp-path value)))
        (if (dbms-error-p tmp-write)
            tmp-write
            (let ((mv-status (if (os-mv tmp-path target-path) 0 1)))
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
  (let ((slots (dbms-empty-slot-list *dbms-data-page-slot-capacity*)))
    (dbms-make-data-page (dbms-make-data-page-header page-id
                                                     0
                                                     (dbms-page-checksum slots)
                                                     *dbms-data-page-slot-capacity*
                                                     '(free))
                         slots)))

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

(defun dbms-eighth (xs)
  (first (cdr (cdr (cdr (cdr (cdr (cdr (cdr xs)))))))))

(defun dbms-ninth (xs)
  (first (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr xs))))))))))

(defun dbms-tenth (xs)
  (first (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr xs)))))))))))

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
