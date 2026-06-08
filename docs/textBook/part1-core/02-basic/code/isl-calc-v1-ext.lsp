;;; isl-calc 1周目: 演習解答例（演算子テーブルの拡張）
;;; 教科書 part1-core/02-basic/05-build-calculator.md 演習1・2 の解答例
;;;
;;; 実行: ./bin/isl docs/textBook/part1-core/02-basic/code/isl-calc-v1-ext.lsp
;;;
;;; #' ディスパッチ版の利点: 新しい演算子は op->fn に1行足すだけで増やせる。
;;; ここでは比較演算 (> <) と max/min を追加した。calc-eval 本体は不変。

(defun op->fn (op)
  (cond ((eq op '+)   #'+)
        ((eq op '-)   #'-)
        ((eq op '*)   #'*)
        ((eq op '/)   #'/)
        ((eq op '>)   #'>)      ; 追加: 大小比較（真なら t、偽なら nil）
        ((eq op '<)   #'<)
        ((eq op 'max) #'max)    ; 追加: 可変長の最大・最小
        ((eq op 'min) #'min)
        (t (error "unknown operator" op))))

(defun calc-eval (e)
  (if (consp e)
      (apply (op->fn (car e))
             (mapcar (lambda (x) (calc-eval x)) (cdr e)))
      e))

;; --- 動作確認 ---
(format (standard-output) "~S => ~S~%" '(> 5 3)              (calc-eval '(> 5 3)))
(format (standard-output) "~S => ~S~%" '(< 5 3)              (calc-eval '(< 5 3)))
(format (standard-output) "~S => ~S~%" '(max 1 9 4)          (calc-eval '(max 1 9 4)))
(format (standard-output) "~S => ~S~%" '(min (+ 2 3) 9 4)    (calc-eval '(min (+ 2 3) 9 4)))
