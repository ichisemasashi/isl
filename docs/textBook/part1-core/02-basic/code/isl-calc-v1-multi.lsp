;;; isl-calc 1周目: 四則電卓（演習版: 単項マイナス + 可変長引数）
;;; 教科書 part1-core/02-basic/04-build-calculator.md 演習1・2 の解答例
;;;
;;; 実行: ./bin/isl docs/textBook/part1-core/02-basic/code/isl-calc-v1-multi.lsp

(defun calc-eval (e)
  (if (consp e)
      (let ((op   (car e))
            ;; オペランド列を一括で再帰評価する
            (args (mapcar (lambda (x) (calc-eval x)) (cdr e))))
        (cond ((eq op '+) (apply (function +) args))
              ((eq op '-) (if (= (length args) 1)
                              (- (car args))          ; 単項マイナス
                              (apply (function -) args)))
              ((eq op '*) (apply (function *) args))
              ((eq op '/) (apply (function /) args))
              (t (error "unknown operator" op))))
      e))

;; --- 動作確認 ---
(format (standard-output) "~S => ~S~%" '(+ 1 2 3 4)            (calc-eval '(+ 1 2 3 4)))
(format (standard-output) "~S => ~S~%" '(- 5)                 (calc-eval '(- 5)))
(format (standard-output) "~S => ~S~%" '(* (+ 1 2) (- 10 3 2)) (calc-eval '(* (+ 1 2) (- 10 3 2))))
(format (standard-output) "~S => ~S~%" '(/ 12 2 3)            (calc-eval '(/ 12 2 3)))
