;; dbms/app/catalog.lsp
;; スキーマ管理層の雛形（MVP Core）。

(load "./examples/dbms/app/storage.lsp")

(defun dbms-catalog-new ()
  '())

(defun dbms-catalog-load ()
  (let ((raw (dbms-storage-load-catalog)))
    (if (null raw) (dbms-catalog-new) raw)))

(defun dbms-catalog-save (catalog)
  (dbms-storage-save-catalog catalog))

(defun dbms-catalog-find-table (catalog table-name)
  (if (null table-name)
      catalog
      catalog)
  ;; 以後のステップでテーブル探索を実装する。
  '())

(defun dbms-catalog-create-table (catalog table-def)
  (if (null table-def)
      catalog
      catalog)
  ;; 以後のステップで CREATE TABLE の制約検証を実装する。
  catalog)
