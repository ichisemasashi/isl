;; dbms/app/catalog.lsp
;; スキーマ管理層の雛形（MVP Core）。

(load "./examples/dbms/app/repr.lsp")
(load "./examples/dbms/app/storage.lsp")

(defun dbms-catalog-new ()
  (dbms-empty-catalog))

(defun dbms-catalog-load ()
  (let ((raw (dbms-storage-load-catalog)))
    (if (dbms-catalog-p raw)
        raw
        (dbms-catalog-new))))

(defun dbms-catalog-save (catalog)
  (dbms-storage-save-catalog catalog))

(defun dbms-catalog-find-table (catalog table-name)
  (if (or (null table-name) (string= table-name ""))
      '()
      (dbms-catalog-find-table-def catalog table-name)))

(defun dbms-catalog-create-table (catalog table-def)
  (if (not (dbms-table-def-p table-def))
      (dbms-make-error 'dbms/invalid-representation "table-def must be dbms-table-def" table-def)
      (let ((name (dbms-table-def-name table-def)))
        (if (null (dbms-catalog-find-table-def catalog name))
            (dbms-catalog-add-table-def catalog table-def)
            (dbms-make-error 'dbms/duplicate-table "table already exists" name)))))
