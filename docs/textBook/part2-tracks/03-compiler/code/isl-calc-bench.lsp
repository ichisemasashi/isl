;;; isl-calc 5周目: ネイティブ化ベンチマーク（コンパイル可能なサブセット版）
;;; 教科書 part2-tracks/03-compiler/05-calc-native-binary.md 本文の完成版
;;;
;;; インタプリタ実行:
;;;   ./bin/isl        docs/textBook/part2-tracks/03-compiler/code/isl-calc-bench.lsp
;;; ネイティブ実行（即時コンパイル）:
;;;   ./bin/islc --run docs/textBook/part2-tracks/03-compiler/code/isl-calc-bench.lsp
;;; AOT ビルド:
;;;   ./bin/islc-aot -o isl-calc-bench docs/textBook/part2-tracks/03-compiler/code/isl-calc-bench.lsp
;;;   ./isl-calc-bench
;;;
;;; ★ なぜ v3（オブジェクトシステム版）ではないのか
;;; native backend は defclass/create（class 特殊形式）や with-handler を未サポート。
;;; そこで評価器は 1周目のリスト版（cond + apply + mapcar + 再帰）に戻す。これは
;;; コンパイラの「守備範囲（コンパイル可能なサブセット）」にちょうど収まる。

;;; --- 演算子シンボル -> 関数（1周目と同じ）------------------------
(defun op->fn (op)
  (cond ((eq op '+) #'+) ((eq op '-) #'-)
        ((eq op '*) #'*) ((eq op '/) #'/)
        (t (error "unknown operator"))))

;;; --- リスト評価器（1周目と同じ）---------------------------------
(defun calc-eval (e)
  (if (consp e)
      (apply (op->fn (car e))
             (mapcar (lambda (x) (calc-eval x)) (cdr e)))
      e))

;;; --- ベンチマーク: 固定式を大量に評価 ---------------------------
;; コンパイル版は標準入力からの read を持たないため、式はプログラムに埋め込む。
;; 反復数は控えめ（ネイティブランタイムの都合で極端な値は避ける）。
(defglobal expr '(+ 1 (* 2 (- 100 (/ 40 4)))))
(defglobal acc 0)
(dotimes (i 3000)
  (setq acc (calc-eval expr)))
(print acc)   ; => 181
