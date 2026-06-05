;;; native-float-smoke.scm
;;;
;;; ネイティブ AOT バックエンドの浮動小数点（<float>）回帰テスト。
;;; かつて native は float を扱えず (+ 1.5 2.5) が "expected number" だった
;;; （docs/remediation-plan.md の P1 数値・型を参照）。
;;;
;;; インタプリタをオラクルとして出力一致を確認する。
;;; 実行:  gosh test/compiler/native-float-smoke.scm

(use gauche.process)
(use srfi-13)

(define *tmp* "test/tmp-native-float.lsp")
(define *fail* 0)

(define (out cmd src)
  (call-with-output-file *tmp*
    (lambda (p) (display src p) (newline p)))
  (string-trim-both
   (process-output->string `(,@cmd ,*tmp*) :error :null :on-abnormal-exit :ignore)))

;; native 出力が interpreter（オラクル）と一致することを要求する
(define (check label src)
  (let ((n (out '("./bin/islc" "--run") src))
        (i (out '("./bin/isl") src)))
    (if (and (string=? n i) (> (string-length i) 0))
        (format #t "ok ~a  (~a)~%" label i)
        (begin (set! *fail* (+ *fail* 1))
               (format #t "FAIL ~a: native=~s interp=~s~%" label n i)))))

;; float contagion（混合演算は float になる）
(check "add-float"   "(print (+ 1.5 2.5))")        ; 4.0
(check "add-mixed"   "(print (+ 1 2.0))")          ; 3.0
(check "sub-mixed"   "(print (- 5.0 1))")          ; 4.0
(check "mul-mixed"   "(print (* 3 2.0))")          ; 6.0
(check "div-mixed"   "(print (/ 1 2.0))")          ; 0.5
(check "div-float"   "(print (/ 1.0 4))")          ; 0.25
;; 出力の最短往復表現
(check "lit-half"    "(print 0.5)")
(check "lit-tenth"   "(print 0.1)")
(check "lit-whole"   "(print 2.0)")
;; 比較（float 混在）
(check "cmp-lt"      "(print (< 1.5 2))")          ; #t
(check "cmp-eq"      "(print (= 2.0 2))")          ; #t
;; 述語・変換・冪
(check "floatp-yes"  "(print (floatp 1.5))")       ; #t
(check "float-conv"  "(print (float 3))")          ; 3.0
(check "expt-float"  "(print (expt 2.0 3))")       ; 8.0
;; 整数/有理数は依然 exact のまま（float 化しない）
(check "int-exact"   "(print (+ 1 2))")            ; 3
(check "ratio-exact" "(print (/ 7 2))")            ; 7/2

(when (file-exists? *tmp*) (sys-unlink *tmp*))

(if (zero? *fail*)
    (begin (format #t "native-float-smoke: all passed~%") (exit 0))
    (begin (format #t "native-float-smoke: ~a failed~%" *fail*) (exit 1)))
