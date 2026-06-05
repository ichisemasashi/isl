;;; native-data-smoke.scm
;;;
;;; ネイティブ AOT バックエンドの文字・文字列・ベクタ回帰テスト。
;;; かつて native はこれらを扱えず（char リテラルや string-append/vector が未対応）
;;; "unsupported" だった。docs/remediation-plan.md の P2「データ型の拡張」を参照。
;;;
;;; インタプリタをオラクルに出力一致を確認する。
;;; 実行:  gosh test/compiler/native-data-smoke.scm

(use gauche.process)
(use srfi-13)

(define *tmp* "test/tmp-native-data.lsp")
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

;; 文字
(check "char-literal"  "(print #\\A)")
(check "char-cmp-lt"   "(print (char< #\\a #\\b))")
(check "char-cmp-eq"   "(print (char= #\\a #\\a))")
(check "char->integer" "(print (char->integer #\\A))")
(check "integer->char" "(print (integer->char 66))")

;; 文字列
(check "string-append" "(print (string-append \"ab\" \"cd\"))")
(check "string=-t"     "(print (string= \"x\" \"x\"))")
(check "length-string" "(print (length \"hello\"))")
(check "elt-string"    "(print (elt \"abc\" 1))")
(check "char-index"    "(print (char-index #\\l \"hello\"))")
(check "create-string" "(print (create-string 3 #\\x))")

;; ベクタ / 1 次元配列
(check "vector"        "(print (vector 1 2 3))")
(check "elt-vector"    "(print (elt (vector 10 20 30) 1))")
(check "aref-vector"   "(print (aref (vector 7 8 9) 2))")
(check "create-vector" "(print (create-vector 3 0))")
(check "create-array"  "(print (aref (create-array '(3) 7) 0))")
(check "length-vector" "(print (length (vector 1 2 3 4)))")

;; ループ・他構文との複合
(check "vector-mutate-loop"
  "(let ((v (create-vector 3 0))) (dotimes (i 3) (set-elt (* i i) v i)) (print v))")
(check "string-build-loop"
  "(let ((s \"\")) (dolist (w '(\"a\" \"b\" \"c\")) (setq s (string-append s w))) (print s))")

(when (file-exists? *tmp*) (sys-unlink *tmp*))

(if (zero? *fail*)
    (begin (format #t "native-data-smoke: all passed~%") (exit 0))
    (begin (format #t "native-data-smoke: ~a failed~%" *fail*) (exit 1)))
