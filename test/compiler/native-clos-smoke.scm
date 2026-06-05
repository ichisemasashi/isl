;;; native-clos-smoke.scm
;;;
;;; ネイティブ AOT バックエンドの CLOS（defclass/defgeneric/defmethod/
;;; make-instance/アクセサ/メソッドディスパッチ/継承）の回帰テスト。
;;; かつて native では CLOS が完全に未対応だった
;;; （docs/remediation-plan.md の P2/P3 CLOS を参照）。
;;;
;;; インタプリタをオラクルに出力一致を確認する。
;;; 実行:  gosh test/compiler/native-clos-smoke.scm

(use gauche.process)
(use srfi-13)

(define *tmp* "test/tmp-native-clos.lsp")
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

;; defclass + accessor + make-instance
(check "slot-accessor"
  "(defclass <pt> () ((x :initarg :x :accessor px))) (print (px (make-instance '<pt> :x 42)))")
(check "create-alias"
  "(defclass <pt2> () ((x :initarg :x :accessor gx))) (print (gx (create '<pt2> :x 7)))")
(check "two-slots"
  "(defclass <p3> () ((a :initarg :a :accessor ga) (b :initarg :b :accessor gb))) (let ((o (make-instance '<p3> :a 1 :b 2))) (print (+ (ga o) (gb o))))")

;; defgeneric + defmethod + dispatch
(check "single-dispatch"
  "(defclass <a> () ()) (defgeneric g (o)) (defmethod g ((o <a>)) 'ok) (print (g (make-instance '<a>)))")
(check "method-uses-slot"
  "(defclass <pt> () ((x :initarg :x :accessor px))) (defgeneric area (p)) (defmethod area ((p <pt>)) (* (px p) (px p))) (print (area (make-instance '<pt> :x 5)))")

;; 継承（スロット継承・サブクラスへのメソッド適用・最特定選択）
(check "inherit-slot"
  "(defclass <base> () ((v :initarg :v :accessor gv))) (defclass <sub> (<base>) ()) (print (gv (make-instance '<sub> :v 99)))")
(check "method-on-subclass"
  "(defclass <an> () ()) (defclass <dog> (<an>) ()) (defgeneric spk (a)) (defmethod spk ((a <an>)) 'animal) (print (spk (make-instance '<dog>)))")
(check "most-specific"
  "(defclass <an> () ()) (defclass <dog> (<an>) ()) (defgeneric s (a)) (defmethod s ((a <an>)) 'animal) (defmethod s ((a <dog>)) 'dog) (print (s (make-instance '<dog>)))")

;; ループ・条件と組み合わせ
(check "clos-in-loop"
  "(defclass <c> () ((n :initarg :n :accessor gn))) (let ((tot 0)) (dolist (k '(1 2 3)) (setq tot (+ tot (gn (make-instance '<c> :n k))))) (print tot))")

(when (file-exists? *tmp*) (sys-unlink *tmp*))

(if (zero? *fail*)
    (begin (format #t "native-clos-smoke: all passed~%") (exit 0))
    (begin (format #t "native-clos-smoke: ~a failed~%" *fail*) (exit 1)))
