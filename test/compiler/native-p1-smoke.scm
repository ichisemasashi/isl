;;; native-p1-smoke.scm
;;;
;;; P1 回帰テスト（native AOT バックエンド）:
;;;  - 制御構造 and / or / case を lowering 経由で実行（短絡・多データ・default）。
;;;  - 数値比較 /= と数値ライブラリ expt / mod / abs / max / min（有理数対応）。
;;;
;;; かつてこれらは native では "unsupported operation" だった
;;; （docs/remediation-plan.md の P1 参照）。
;;;
;;; 実行:  gosh test/compiler/native-p1-smoke.scm

(use gauche.process)
(use srfi-13)

(define *tmp* "test/tmp-native-p1.lsp")
(define *fail* 0)

(define (native-out src)
  (call-with-output-file *tmp*
    (lambda (p) (display src p) (newline p)))
  (string-trim-both
   (process-output->string `("./bin/islc" "--run" ,*tmp*)
                           :error :null :on-abnormal-exit :ignore)))

(define (check label src expected)
  (let ((got (native-out src)))
    (if (string=? got expected)
        (format #t "ok ~a~%" label)
        (begin (set! *fail* (+ *fail* 1))
               (format #t "FAIL ~a: expected ~s got ~s~%" label expected got)))))

;; --- and / or（短絡評価） ---
(check "and-truthy"   "(print (and 1 2 3))" "3")
(check "and-short"    "(print (and 1 nil 3))" "nil")
(check "and-empty"    "(print (and))" "#t")
(check "or-first"     "(print (or nil 7 8))" "7")
(check "or-allnil"    "(print (or nil nil))" "nil")
(check "and-or-nest"  "(print (and 1 (or nil 7)))" "7")

;; --- case（多データ・default） ---
(check "case-hit"     "(print (case 2 ((1) (quote a)) ((2) (quote b)) (t (quote c))))" "b")
(check "case-miss"    "(print (case 9 ((1) (quote a)) (t (quote z))))" "z")
(check "case-multi"   "(print (case 3 ((1 2) (quote lo)) ((3 4) (quote hi))))" "hi")

;; --- 数値比較 ---
(check "ne-true"      "(print (/= 1 2))" "#t")
(check "ne-false"     "(print (/= 2 2))" "#f")

;; --- 数値ライブラリ ---
(check "abs-int"      "(print (abs -5))" "5")
(check "abs-ratio"    "(print (abs (/ -3 4)))" "3/4")
(check "max"          "(print (max 1 3 2))" "3")
(check "min"          "(print (min 3 1 2))" "1")
(check "mod-pos"      "(print (mod 7 3))" "1")
(check "mod-neg-div"  "(print (mod -7 3))" "2")     ; 結果は除数の符号
(check "mod-neg-dvr"  "(print (mod 7 -3))" "-2")
(check "expt-pos"     "(print (expt 2 10))" "1024")
(check "expt-neg"     "(print (expt 2 -1))" "1/2")  ; 有理数（native は float 非対応）
(check "expt-ratio"   "(print (expt (/ 2 3) 2))" "4/9")

(when (file-exists? *tmp*) (sys-unlink *tmp*))

(if (zero? *fail*)
    (begin (format #t "native-p1-smoke: all passed~%") (exit 0))
    (begin (format #t "native-p1-smoke: ~a failed~%" *fail*) (exit 1)))
