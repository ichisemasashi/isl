;;; isl-calc 2周目: オブジェクトシステムによる AST 評価器
;;; 教科書 part1-core/03-intermediate/05-redesign-with-object-system-ast.md 本文の完成版
;;;
;;; 実行: ./bin/isl docs/textBook/part1-core/03-intermediate/code/isl-calc-v2.lsp
;;;
;;; 1周目 (v1) の cond 分岐を、AST ノード型ごとの defmethod に置き換えた。
;;; さらに変数環境を導入し、変数参照 (var-node) と let 束縛 (let-node) を追加した。

;;; --- AST ノードの定義 -------------------------------------------------
(defclass num-node   () ((value :initarg value :accessor node-value)))
(defclass var-node   () ((name  :initarg name  :accessor node-name)))
(defclass binop-node () ((op    :initarg op    :accessor node-op)
                         (lhs   :initarg lhs   :accessor node-lhs)
                         (rhs   :initarg rhs   :accessor node-rhs)))
(defclass let-node   () ((name  :initarg name  :accessor node-name)
                         (init  :initarg init  :accessor node-init)
                         (body  :initarg body  :accessor node-body)))

;;; --- 演算子シンボル -> 関数 -------------------------------------------
(defun op->fn (op)
  (cond ((eq op '+) #'+) ((eq op '-) #'-)
        ((eq op '*) #'*) ((eq op '/) #'/)
        (t (error "unknown operator" op))))

;;; --- S式 -> AST への簡易パーサ ----------------------------------------
(defun parse (e)
  (cond ((numberp e) (create (class num-node) 'value e))
        ((symbolp e) (create (class var-node) 'name e))
        ((eq (car e) 'let)
         (let ((b (car (cadr e))))                 ; b = (x init)
           (create (class let-node)
                   'name (car b)
                   'init (parse (cadr b))
                   'body (parse (car (cddr e))))))
        (t (create (class binop-node)
                   'op  (car e)
                   'lhs (parse (cadr e))
                   'rhs (parse (car (cddr e)))))))

;;; --- 環境: (name . value) の連想リスト -------------------------------
(defun env-lookup (name env)
  (let ((pair (assoc name env)))
    (if pair (cdr pair) (error "unbound variable" name))))

;;; --- 評価: 型ごとにディスパッチ --------------------------------------
(defgeneric ast-eval (node env))
(defmethod ast-eval ((n num-node) env) (node-value n))
(defmethod ast-eval ((n var-node) env) (env-lookup (node-name n) env))
(defmethod ast-eval ((n binop-node) env)
  (funcall (op->fn (node-op n))
           (ast-eval (node-lhs n) env)
           (ast-eval (node-rhs n) env)))
(defmethod ast-eval ((n let-node) env)
  (let ((v (ast-eval (node-init n) env)))
    (ast-eval (node-body n)
              (cons (cons (node-name n) v) env))))

;;; --- 入口: S式を parse して空環境で評価 ------------------------------
(defun calc (e) (ast-eval (parse e) '()))

;;; --- 動作確認 --------------------------------------------------------
(format (standard-output) "~S => ~S~%" '(+ 1 (* 2 3))
        (calc '(+ 1 (* 2 3))))
(format (standard-output) "~S => ~S~%" '(let ((x 10)) (+ x 5))
        (calc '(let ((x 10)) (+ x 5))))
(format (standard-output) "~S => ~S~%" '(let ((x 2)) (let ((y 3)) (* x y)))
        (calc '(let ((x 2)) (let ((y 3)) (* x y)))))
