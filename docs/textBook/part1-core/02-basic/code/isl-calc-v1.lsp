;;; isl-calc 1周目: 四則電卓（基本2項版）
;;; 教科書 part1-core/02-basic/04-build-calculator.md の同梱コード
;;;
;;; 実行: ./bin/isl docs/textBook/part1-core/02-basic/code/isl-calc-v1.lsp

(defun calc-eval (e)
  (if (consp e)
      ;; リスト（式）なら: 先頭が演算子、オペランドを再帰評価してから計算
      (let ((op (car e))
            (a  (calc-eval (car (cdr e))))
            (b  (calc-eval (car (cdr (cdr e))))))
        (cond ((eq op '+) (+ a b))
              ((eq op '-) (- a b))
              ((eq op '*) (* a b))
              ((eq op '/) (/ a b))
              (t (error "unknown operator" op))))
      ;; アトム（数）なら、それ自身が値
      e))

;; --- 動作確認 ---
(format (standard-output) "~S => ~S~%" '(+ 1 2)             (calc-eval '(+ 1 2)))
(format (standard-output) "~S => ~S~%" '(+ 1 (* 2 3))       (calc-eval '(+ 1 (* 2 3))))
(format (standard-output) "~S => ~S~%" '(* (+ 1 2) (- 9 4)) (calc-eval '(* (+ 1 2) (- 9 4))))
(format (standard-output) "~S => ~S~%" '(/ 7 2)             (calc-eval '(/ 7 2)))
