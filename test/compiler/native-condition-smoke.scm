;;; native-condition-smoke.scm
;;;
;;; ネイティブ AOT バックエンドの例外（handler-case / error / ゼロ除算捕捉）の
;;; 回帰テスト。かつて native では handler-case が未対応で、ゼロ除算も
;;; <division-by-zero> として捕捉できなかった
;;; （docs/remediation-plan.md の P2 例外を参照）。
;;;
;;; インタプリタをオラクルに出力一致を確認する。
;;; 実行:  gosh test/compiler/native-condition-smoke.scm

(use gauche.process)
(use srfi-13)

(define *tmp* "test/tmp-native-cond.lsp")
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

;; error を handler-case で捕捉
(check "catch-error"  "(print (handler-case (error \"boom\") (<error> (c) 'caught)))")
(check "no-error"     "(print (handler-case (+ 1 2) (<error> (c) 'x)))")
(check "cond-message" "(print (handler-case (error \"oops\") (<error> (c) (condition-message c))))")

;; ゼロ除算が <division-by-zero> として捕捉できる（クラス階層も）
(check "div0-specific" "(print (handler-case (/ 1 0) (<division-by-zero> (c) 'dz)))")
(check "div0-via-error" "(print (handler-case (/ 1 0) (<error> (c) 'e)))")
(check "div0-arith"    "(print (handler-case (/ 1 0) (<arithmetic-error> (c) 'a)))")

;; 複数節（最初に一致した節が選ばれる／非一致は外側へ再シグナル）
(check "two-clauses"   "(print (handler-case (error \"x\") (<division-by-zero> (c) 1) (<error> (c) 2)))")
(check "nested-reraise"
  "(print (handler-case (handler-case (error \"x\") (<division-by-zero> (c) 1)) (<error> (c) 2)))")

;; （注: t/otherwise の包括節は JIT/native では捕捉するがインタプリタは
;;  クラス名として扱うため非可搬。標準クラス節のみをテストする。）

;; unwind-protect を貫通する例外でも cleanup が実行される
(check "uwp-through-error"
  "(let ((s 0)) (handler-case (unwind-protect (error \"e\") (setq s 9)) (<error> (c) nil)) (print s))")

;; ループや setq と組み合わせ
(check "loop-handler"
  "(let ((sum 0)) (dolist (x '(1 0 2)) (handler-case (setq sum (+ sum (/ 10 x))) (<division-by-zero> (c) nil))) (print sum))")

(when (file-exists? *tmp*) (sys-unlink *tmp*))

(if (zero? *fail*)
    (begin (format #t "native-condition-smoke: all passed~%") (exit 0))
    (begin (format #t "native-condition-smoke: ~a failed~%" *fail*) (exit 1)))
