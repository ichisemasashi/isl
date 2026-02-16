;; dbms/app/storage.lsp
;; 永続化層の雛形（MVP Core）。

(defun dbms-storage-root ()
  (let ((v (getenv "DBMS_STORAGE_ROOT")))
    (if (or (null v) (string= v ""))
        "./examples/dbms/storage"
        v)))

(defun dbms-storage-catalog-path ()
  (string-append (dbms-storage-root) "/catalog.lspdata"))

(defun dbms-storage-table-path (table-name)
  (string-append (dbms-storage-root) "/table_" table-name ".lspdata"))

(defun dbms-storage-load-catalog ()
  ;; 以後のステップで read ベース実装に差し替える。
  '())

(defun dbms-storage-save-catalog (catalog)
  ;; 以後のステップで tmp -> rename 実装に差し替える。
  catalog)

(defun dbms-storage-load-table-rows (table-name)
  (if (null table-name)
      '()
      '())
  '())

(defun dbms-storage-save-table-rows (table-name rows)
  (if (null table-name)
      rows
      rows)
  rows)
