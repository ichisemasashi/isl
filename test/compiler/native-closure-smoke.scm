;;; native-closure-smoke.scm
;;;
;;; ネイティブ AOT バックエンドの局所関数 flet / labels 回帰テスト。
;;; かつて native では "unsupported special form: flet" だった
;;; （docs/remediation-plan.md の P1 制御構造を参照）。
;;;
;;; インタプリタをオラクルに出力一致を確認する。
;;; 実行:  gosh test/compiler/native-closure-smoke.scm

(use gauche.process)
(use srfi-13)

(define *tmp* "test/tmp-native-closure.lsp")
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

;; flet（非再帰の局所関数）
(check "flet-single"   "(print (flet ((g (x) (* x 2))) (g 21)))")
(check "flet-multi"    "(print (flet ((f (x) (+ x 1)) (g (y) (* y 3))) (+ (f 10) (g 4))))")
(check "flet-capture"  "(print (let ((x 100)) (flet ((f () x)) (f))))")
(check "flet-mutate"   "(print (let ((c 0)) (flet ((inc () (setq c (+ c 1)))) (inc) (inc) (inc) c)))")

;; labels（相互再帰・自己再帰）
(check "labels-mutual"
  "(print (labels ((ev (n) (if (= n 0) t (od (- n 1)))) (od (n) (if (= n 0) nil (ev (- n 1))))) (ev 6)))")
(check "labels-self"
  "(print (labels ((fact (n) (if (= n 0) 1 (* n (fact (- n 1)))))) (fact 5)))")
(check "labels-loop-acc"
  "(print (labels ((sum (n acc) (if (= n 0) acc (sum (- n 1) (+ acc n))))) (sum 100 0)))")

(when (file-exists? *tmp*) (sys-unlink *tmp*))

(if (zero? *fail*)
    (begin (format #t "native-closure-smoke: all passed~%") (exit 0))
    (begin (format #t "native-closure-smoke: ~a failed~%" *fail*) (exit 1)))
