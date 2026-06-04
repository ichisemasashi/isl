;;; native-nonlocal-smoke.scm
;;;
;;; ネイティブ AOT バックエンドの非局所制御の回帰テスト:
;;;   block / return-from と catch / throw（setjmp/longjmp ベース）。
;;;
;;; かつてこれらは native では "unsupported special form" だった
;;; （docs/remediation-plan.md の P1 制御構造を参照）。
;;;
;;; 実行:  gosh test/compiler/native-nonlocal-smoke.scm

(use gauche.process)
(use srfi-13)

(define *tmp* "test/tmp-native-nonlocal.lsp")
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

;; block / return-from
(check "block-return"   "(print (block b (return-from b 7) 0))" "7")
(check "block-novalue"  "(print (block b 5))" "5")
(check "block-compute"  "(print (block b (+ 1 (return-from b 10)) 0))" "10")
(check "block-nest-in"  "(print (block a (block b (return-from b 1)) 2))" "2")
(check "block-nest-out" "(print (block a (block b (return-from a 99)) 2))" "99")

;; catch / throw
(check "catch-throw"    "(print (catch (quote k) (throw (quote k) 8) 0))" "8")
(check "catch-novalue"  "(print (catch (quote k) 9))" "9")
(check "throw-past-blk" "(print (catch (quote k) (block b (throw (quote k) 42)) 0))" "42")
(check "catch-outer"    "(print (catch (quote a) (catch (quote b) (throw (quote a) 5))))" "5")

;; mixed with already-supported forms
(check "block+cond"     "(print (block b (if (= 1 1) (return-from b 'yes) 'no)))" "yes")

(when (file-exists? *tmp*) (sys-unlink *tmp*))

(if (zero? *fail*)
    (begin (format #t "native-nonlocal-smoke: all passed~%") (exit 0))
    (begin (format #t "native-nonlocal-smoke: ~a failed~%" *fail*) (exit 1)))
