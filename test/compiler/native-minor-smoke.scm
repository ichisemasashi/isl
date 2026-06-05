;;; native-minor-smoke.scm
;;;
;;; ネイティブ AOT バックエンドの「残りの軽微な制限」を解消した機能の回帰テスト:
;;;   1. ベクタリテラル #(...) の定数構築（ネスト・空ベクタ含む）
;;;   2. call-next-method / next-method-p（多段継承チェーン）
;;;   3. 多次元配列（create-array rank>=2 / aref / set-aref / array-dimensions / 印字）
;;;   4. メソッド修飾子 :before / :after / :around（標準メソッド結合）
;;;
;;; いずれもインタプリタをオラクルに出力一致を確認する。
;;; 実行:  gosh test/compiler/native-minor-smoke.scm

(use gauche.process)
(use srfi-13)

(define *tmp* "test/tmp-native-minor.lsp")
(define *fail* 0)

(define (out cmd src)
  (call-with-output-file *tmp*
    (lambda (p) (display src p) (newline p)))
  (string-trim-both
   (process-output->string `(,@cmd ,*tmp*) :error :null :on-abnormal-exit :ignore)))

(define (check label src)
  (let ((n (out '("./bin/islc" "--run") src))
        (i (out '("./bin/isl") src)))
    (if (and (string=? n i) (> (string-length i) 0))
        (format #t "ok ~a  (~a)~%" label i)
        (begin (set! *fail* (+ *fail* 1))
               (format #t "FAIL ~a: native=~s interp=~s~%" label n i)))))

;; 1. ベクタリテラル
(check "vec-literal"        "(print '#(1 2 3))")
(check "vec-literal-quote2" "(print (quote #(10 20)))")
(check "vec-nested"         "(print '#(1 #(2 3) 4))")
(check "vec-empty"          "(print '#())")
(check "vec-aref"           "(print (aref '#(7 8 9) 1))")

;; 2. call-next-method / next-method-p
(check "cnm-chain"
  "(defclass <a> () ()) (defclass <b> (<a>) ()) (defclass <c> (<b>) ()) (defgeneric g (o)) (defmethod g ((o <a>)) (list 'a)) (defmethod g ((o <b>)) (cons 'b (call-next-method))) (defmethod g ((o <c>)) (cons 'c (call-next-method))) (print (g (make-instance '<c>)))")
(check "next-method-p"
  "(defclass <a> () ()) (defclass <b> (<a>) ()) (defgeneric g (o)) (defmethod g ((o <a>)) 'base) (defmethod g ((o <b>)) (if (next-method-p) (call-next-method) 'none)) (print (g (make-instance '<b>)))")

;; 3. 多次元配列
(check "array-2d-aref"      "(print (aref (create-array '(2 2) 7) 1 1))")
(check "array-2d-set"       "(let ((a (create-array '(2 3) 0))) (set-aref 5 a 1 2) (print (aref a 1 2)))")
(check "array-dimensions"   "(print (array-dimensions (create-array '(2 3) 0)))")
(check "array-print"        "(print (create-array '(2 2) 7))")
(check "array-rank1-vec"    "(print (create-array '(3) 9))")
(check "array-loop-fill"
  "(let ((a (create-array '(2 2) 0))) (dotimes (i 2) (dotimes (j 2) (set-aref (+ i j) a i j))) (print (aref a 1 1)))")

;; 4. メソッド修飾子（before/after/around）
(check "qual-around-before-after"
  "(defclass <a> () ()) (defgeneric g (o)) (defmethod g ((o <a>)) (format (standard-output) \"primary \")) (defmethod g :before ((o <a>)) (format (standard-output) \"before \")) (defmethod g :after ((o <a>)) (format (standard-output) \"after \")) (defmethod g :around ((o <a>)) (format (standard-output) \"around[ \") (call-next-method) (format (standard-output) \"]around\")) (g (make-instance '<a>))")
(check "qual-before-after-only"
  "(defclass <a> () ()) (defgeneric g (o)) (defmethod g ((o <a>)) (format (standard-output) \"P\")) (defmethod g :before ((o <a>)) (format (standard-output) \"B\")) (defmethod g :after ((o <a>)) (format (standard-output) \"A\")) (g (make-instance '<a>))")
(check "qual-inherit-order"
  "(defclass <a> () ()) (defclass <b> (<a>) ()) (defgeneric g (o)) (defmethod g ((o <a>)) (format (standard-output) \"P\")) (defmethod g :before ((o <a>)) (format (standard-output) \"ba\")) (defmethod g :before ((o <b>)) (format (standard-output) \"bb\")) (defmethod g :after ((o <a>)) (format (standard-output) \"aa\")) (defmethod g :after ((o <b>)) (format (standard-output) \"ab\")) (g (make-instance '<b>))")
(check "qual-around-value"
  "(defclass <a> () ()) (defgeneric g (o)) (defmethod g ((o <a>)) 10) (defmethod g :around ((o <a>)) (+ 1 (call-next-method))) (print (g (make-instance '<a>)))")

(when (file-exists? *tmp*) (sys-unlink *tmp*))

(if (zero? *fail*)
    (begin (format #t "native-minor-smoke: all passed~%") (exit 0))
    (begin (format #t "native-minor-smoke: ~a failed~%" *fail*) (exit 1)))
