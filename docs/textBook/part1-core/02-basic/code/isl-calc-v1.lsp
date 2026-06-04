;;; isl-calc 1周目: 四則電卓（関数参照 #' ディスパッチ版）
;;; 教科書 part1-core/02-basic/04-build-calculator.md の同梱コード
;;;
;;; 実行: ./bin/isl docs/textBook/part1-core/02-basic/code/isl-calc-v1.lsp
;;;
;;; アイデア: 「演算子は関数である」。#'+ で + 関数そのものを値として取り出し、
;;; オペランドを評価した結果に apply する。この設計なら可変長引数も単項マイナスも
;;; 自然に処理できる。

;; 演算子シンボル -> 関数オブジェクト
(defun op->fn (op)
  (cond ((eq op '+) #'+)
        ((eq op '-) #'-)
        ((eq op '*) #'*)
        ((eq op '/) #'/)
        (t (error "unknown operator" op))))

;; 数式（木）を評価する
(defun calc-eval (e)
  (if (consp e)
      ;; リスト（式）なら: 先頭の演算子に対応する関数を、評価済みオペランドへ適用
      (let ((fn   (op->fn (car e)))
            (args (mapcar (lambda (x) (calc-eval x)) (cdr e))))
        (apply fn args))
      ;; アトム（数）なら、それ自身が値
      e))

;; --- 動作確認 ---
(format (standard-output) "~S => ~S~%" '(+ 1 2)             (calc-eval '(+ 1 2)))
(format (standard-output) "~S => ~S~%" '(+ 1 (* 2 3))       (calc-eval '(+ 1 (* 2 3))))
(format (standard-output) "~S => ~S~%" '(* (+ 1 2) (- 9 4)) (calc-eval '(* (+ 1 2) (- 9 4))))
(format (standard-output) "~S => ~S~%" '(/ 7 2)             (calc-eval '(/ 7 2)))
(format (standard-output) "~S => ~S~%" '(+ 1 2 3 4)         (calc-eval '(+ 1 2 3 4)))  ; 可変長
(format (standard-output) "~S => ~S~%" '(- 5)               (calc-eval '(- 5)))        ; 単項マイナス
