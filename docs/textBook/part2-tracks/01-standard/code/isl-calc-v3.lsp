;;; isl-calc 3周目: strict 準拠版（標準コンディションで型付きに失敗を返す）
;;; 教科書 part2-tracks/01-standard/04-calc-strict-edition.md 本文の完成版
;;;
;;; 実行（strict プロファイル）:
;;;   ./bin/isl --profile strict docs/textBook/part2-tracks/01-standard/code/isl-calc-v3.lsp
;;;
;;; 2周目 (v2) のオブジェクトシステム評価器を土台に、
;;;   - 拡張機能に依存しない（strict でそのまま動く）
;;;   - ゼロ除算は処理系が通知する標準コンディション <division-by-zero> に委ねる
;;;   - with-handler + instancep で失敗を「型付きの結果」として呼び出し側へ返す
;;; という3点を加えた。公開 API は接頭辞 calc- で名前空間を整理している。

;;; --- AST ノード（v2 と同じ）----------------------------------------
(defclass num-node   () ((value :initarg value :accessor node-value)))
(defclass var-node   () ((name  :initarg name  :accessor node-name)))
(defclass binop-node () ((op    :initarg op    :accessor node-op)
                         (lhs   :initarg lhs   :accessor node-lhs)
                         (rhs   :initarg rhs   :accessor node-rhs)))
(defclass let-node   () ((name  :initarg name  :accessor node-name)
                         (init  :initarg init  :accessor node-init)
                         (body  :initarg body  :accessor node-body)))

;;; --- 演算子シンボル -> 関数 ----------------------------------------
(defun calc-op->fn (op)
  (cond ((eq op '+) #'+) ((eq op '-) #'-)
        ((eq op '*) #'*) ((eq op '/) #'/)
        (t (error "unknown operator: ~A" op))))

;;; --- S式 -> AST ----------------------------------------------------
(defun calc-parse (e)
  (cond ((numberp e) (create (class num-node) 'value e))
        ((symbolp e) (create (class var-node) 'name e))
        ((eq (car e) 'let)
         (let ((b (car (cadr e))))
           (create (class let-node)
                   'name (car b)
                   'init (calc-parse (cadr b))
                   'body (calc-parse (car (cddr e))))))
        (t (create (class binop-node)
                   'op  (car e)
                   'lhs (calc-parse (cadr e))
                   'rhs (calc-parse (car (cddr e)))))))

;;; --- 環境 ----------------------------------------------------------
(defun calc-env-lookup (name env)
  (let ((pair (assoc name env)))
    (if pair (cdr pair) (error "unbound variable: ~A" name))))

;;; --- 評価 ----------------------------------------------------------
(defgeneric calc-eval-node (node env))
(defmethod calc-eval-node ((n num-node) env) (node-value n))
(defmethod calc-eval-node ((n var-node) env) (calc-env-lookup (node-name n) env))
(defmethod calc-eval-node ((n binop-node) env)
  ;; (/ x 0) はここで処理系が <division-by-zero> を通知する（自前チェック不要）
  (funcall (calc-op->fn (node-op n))
           (calc-eval-node (node-lhs n) env)
           (calc-eval-node (node-rhs n) env)))
(defmethod calc-eval-node ((n let-node) env)
  (let ((v (calc-eval-node (node-init n) env)))
    (calc-eval-node (node-body n)
                    (cons (cons (node-name n) v) env))))

;;; --- 公開 API ------------------------------------------------------
;; 例外を投げる素朴版
(defun calc-eval (e) (calc-eval-node (calc-parse e) '()))

;; 失敗を「型付きの結果」として返す版:
;;   成功 → (ok 値)
;;   ゼロ除算 → (error division-by-zero オペランド)
;;   その他 → (error other メッセージ)
(defun calc-try (e)
  (catch 'calc-done
    (with-handler
      (lambda (c)
        (throw 'calc-done
          (cond ((instancep c (class <division-by-zero>))
                 (list 'error 'division-by-zero (arithmetic-error-operands c)))
                ((instancep c (class <error>))
                 (list 'error 'other (condition-message c)))
                (t (list 'error 'unknown)))))
      (list 'ok (calc-eval e)))))

;;; --- ミニ準拠テスト ------------------------------------------------
;; テストケース: (value 式 期待値) / (error 式 期待種別)
(defun calc-run-test (tc)
  (let ((kind (car tc)) (expr (cadr tc)) (expected (car (cddr tc))))
    (cond ((eq kind 'value)
           (let ((got (calc-try expr)))
             (if (and (eq (car got) 'ok) (= (cadr got) expected))
                 (progn (format (standard-output) "  ok    ~S => ~S~%" expr expected) t)
                 (progn (format (standard-output) "  FAIL  ~S : ~S (expected ~S)~%"
                                expr got expected) nil))))
          ((eq kind 'error)
           (let ((got (calc-try expr)))
             (if (and (eq (car got) 'error) (eq (cadr got) expected))
                 (progn (format (standard-output) "  ok    ~S => error/~S~%" expr expected) t)
                 (progn (format (standard-output) "  FAIL  ~S : ~S (expected error/~S)~%"
                                expr got expected) nil)))))))

(defun calc-run-tests (tests)
  (let ((pass 0) (total 0))
    (mapcar (lambda (tc)
              (setq total (+ total 1))
              (when (calc-run-test tc) (setq pass (+ pass 1))))
            tests)
    (format (standard-output) "~%~D / ~D passed~%" pass total)))

;;; --- 実行 ----------------------------------------------------------
(calc-run-tests
 '((value (+ 1 (* 2 3))            7)
   (value (let ((x 10)) (+ x 5))   15)
   (value (let ((x 2)) (let ((y 3)) (* x y))) 6)
   (error (/ 1 0)                  division-by-zero)
   (error (+ y 1)                  other)))
