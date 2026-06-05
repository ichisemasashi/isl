;;; native-typeconv-smoke.scm
;;;
;;; ネイティブ AOT バックエンドの the / convert / 動的変数 回帰テスト。
;;; かつて native はこれらを扱えず "unsupported" だった
;;; （docs/remediation-plan.md の P2/P3 を参照）。
;;;
;;; インタプリタをオラクルに出力一致を確認する。
;;; 実行:  gosh test/compiler/native-typeconv-smoke.scm

(use gauche.process)
(use srfi-13)

(define *tmp* "test/tmp-native-typeconv.lsp")
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

;; the / assure（型表明、正しい型）
(check "the-int"     "(print (the <integer> 42))")
(check "the-expr"    "(print (the <integer> (+ 1 2)))")
(check "assure"      "(print (assure <integer> (* 6 7)))")

;; convert（型変換）
(check "convert-char"  "(print (convert 65 <character>))")
(check "convert-int"   "(print (convert 3.7 <integer>))")
(check "convert-float" "(print (convert 3 <float>))")
(check "convert-str"   "(print (convert 42 <string>))")
(check "convert-sym"   "(print (convert \"hi\" <symbol>))")
(check "convert-list"  "(print (convert \"ab\" <list>))")

;; dynamic / dynamic-let / defdynamic
(check "dynamic-read"   "(defdynamic *d* 1) (print (dynamic *d*))")
(check "dynamic-let"    "(defdynamic *d* 1) (print (dynamic-let ((*d* 9)) (dynamic *d*)))")
(check "dynamic-restore" "(defdynamic *d* 1) (dynamic-let ((*d* 9)) (dynamic *d*)) (print (dynamic *d*))")
(check "dynamic-via-fn"  "(defdynamic *d* 1) (defun rd () (dynamic *d*)) (print (dynamic-let ((*d* 7)) (rd)))")
;; 非局所脱出でも動的束縛は復元される（unwind-protect 経由）
(check "dynamic-unwind"
  "(defdynamic *d* 1) (block b (dynamic-let ((*d* 9)) (return-from b 0))) (print (dynamic *d*))")

(when (file-exists? *tmp*) (sys-unlink *tmp*))

(if (zero? *fail*)
    (begin (format #t "native-typeconv-smoke: all passed~%") (exit 0))
    (begin (format #t "native-typeconv-smoke: ~a failed~%" *fail*) (exit 1)))
