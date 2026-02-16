;; dbms/app/profile.lsp
;; 実装プロファイルと機能ゲート管理。

(defglobal *dbms-profile-mvp-core* 'mvp-core)
(defglobal *dbms-profile-wiki-compat-v1* 'wiki-compat-v1)
(defglobal *dbms-active-profile* *dbms-profile-mvp-core*)

(defun dbms-profile-valid-p (profile)
  (or (eq profile *dbms-profile-mvp-core*)
      (eq profile *dbms-profile-wiki-compat-v1*)))

(defun dbms-set-active-profile (profile)
  (if (dbms-profile-valid-p profile)
      (setq *dbms-active-profile* profile)
      (error "dbms/profile-invalid" profile)))

(defun dbms-active-profile ()
  *dbms-active-profile*)

(defun dbms-feature-enabled-p (feature)
  (if (eq *dbms-active-profile* *dbms-profile-wiki-compat-v1*)
      t
      ;; MVP Core で許可する機能のみ列挙する。
      (or (eq feature 'create-table)
          (eq feature 'insert)
          (eq feature 'select)
          (eq feature 'update)
          (eq feature 'delete)
          (eq feature 'where-basic)
          (eq feature 'order-by-single)
          (eq feature 'constraints-pk-not-null)
          (eq feature 'types-int-text-bool)
          (eq feature 'storage-basic))))

(defun dbms-profile-name ()
  (if (eq *dbms-active-profile* *dbms-profile-mvp-core*)
      "MVP Core"
      "Wiki Compatibility v1"))
