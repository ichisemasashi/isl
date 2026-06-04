;;; native-loop-smoke.scm
;;;
;;; ネイティブ AOT バックエンドの反復・代入・unwind-protect 回帰テスト:
;;;   setq、while、dotimes、dolist、for、tagbody/go、unwind-protect。
;;; かつてこれらは native では未対応（while/dotimes/dolist は黙って 0 を返していた）。
;;; docs/remediation-plan.md の P1「制御構造の完成」を参照。
;;;
;;; インタプリタをオラクルに出力一致を確認する。
;;; 実行:  gosh test/compiler/native-loop-smoke.scm

(use gauche.process)
(use srfi-13)

(define *tmp* "test/tmp-native-loop.lsp")
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

;; setq（変数の破壊的更新）
(check "setq-let"     "(print (let ((x 0)) (setq x 5) x))")
(check "setq-accum"   "(print (let ((x 1)) (setq x (+ x 10)) (setq x (+ x 100)) x))")
(check "setq-param"   "(defun f (a) (setq a (* a 2)) a) (print (f 21))")

;; while
(check "while-accum"
  "(let ((i 0) (s 0)) (while (< i 4) (setq s (+ s i)) (setq i (+ i 1))) (print s))")

;; dotimes
(check "dotimes"      "(let ((s 0)) (dotimes (i 4) (setq s (+ s i))) (print s))")
(check "dotimes-res"  "(print (dotimes (i 5 i) nil))")

;; dolist
(check "dolist"       "(let ((s 0)) (dolist (x '(1 2 3 4)) (setq s (+ s x))) (print s))")

;; for（並列ステップ更新）
(check "for"          "(print (for ((i 0 (+ i 1)) (s 0 (+ s i))) ((>= i 4) s)))")

;; tagbody / go
(check "tagbody-back" "(let ((x 0)) (tagbody a (setq x (+ x 1)) (if (< x 3) (go a))) (print x))")
(check "tagbody-fwd"  "(let ((x 0)) (tagbody (go skip) (setq x 99) skip (setq x 7)) (print x))")

;; ネスト・複合
(check "nested-loop"  "(let ((n 0)) (dotimes (i 3) (dotimes (j 3) (setq n (+ n 1)))) (print n))")
(check "loop+block"
  "(print (block done (let ((i 0)) (while (< i 10) (if (= i 5) (return-from done i)) (setq i (+ i 1))) -1)))")

;; unwind-protect（cleanup は常に実行される）
(check "uwp-normal"   "(let ((s 0)) (unwind-protect (setq s 1) (setq s (+ s 10))) (print s))")
(check "uwp-return"   "(let ((s 0)) (block b (unwind-protect (return-from b 1) (setq s 9))) (print s))")
(check "uwp-throw"    "(let ((s 0)) (catch 'k (unwind-protect (throw 'k 1) (setq s 5))) (print s))")
(check "uwp-nested"
  "(let ((g 0)) (block b (unwind-protect (unwind-protect (return-from b 1) (setq g (+ g 1))) (setq g (+ g 10)))) (print g))")

(when (file-exists? *tmp*) (sys-unlink *tmp*))

(if (zero? *fail*)
    (begin (format #t "native-loop-smoke: all passed~%") (exit 0))
    (begin (format #t "native-loop-smoke: ~a failed~%" *fail*) (exit 1)))
