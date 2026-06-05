;;; native-p0-smoke.scm
;;;
;;; P0 回帰テスト（native AOT バックエンド）:
;;;  1. format の基数指示子 ~X/~O/~B/~& が正しく出力される。
;;;  2. 未対応の特殊形式（while/dotimes/dolist/tagbody/unwind-protect 等）は
;;;     *黙って誤った値* を返さず、非 0 終了で失敗する。
;;;
;;; かつて while/dotimes/dolist は初期値（0）を黙って印字し、~X は "~X" を
;;; そのまま出力していた（docs/remediation-plan.md の P0 参照）。
;;;
;;; 実行:  gosh test/compiler/native-p0-smoke.scm

(use gauche.process)
(use srfi-13)

(define *tmp* "test/tmp-native-p0.lsp")

(define (write-src! src)
  (call-with-output-file *tmp*
    (lambda (p) (display src p) (newline p))))

(define (native-run src)
  ;; (values stdout exit-status)
  (write-src! src)
  (let* ((out (string-trim-both
               (process-output->string `("./bin/islc" "--run" ,*tmp*)
                                       :error :null
                                       :on-abnormal-exit :ignore)))
         ;; もう一度走らせて終了コードを取る（process-output->string は
         ;; 終了コードを返さないため）
         (st (process-exit-status
              (run-process `("./bin/islc" "--run" ,*tmp*)
                           :output :null :error :null :wait #t))))
    (values out st)))

(define *fail* 0)

(define (check-output label src expected)
  (receive (out st) (native-run src)
    (cond
     ((not (string=? out expected))
      (set! *fail* (+ *fail* 1))
      (format #t "FAIL ~a: expected ~s got ~s~%" label expected out))
     (else (format #t "ok ~a~%" label)))))

(define (check-fails-loudly label src)
  ;; 成功（終了コード 0）してはならない。値も印字してはならない。
  (receive (out st) (native-run src)
    (cond
     ((and (not (zero? (sys-wait-exit-status st)))
           (string=? out ""))
      (format #t "ok ~a (fails loudly, exit nonzero)~%" label))
     (else
      (set! *fail* (+ *fail* 1))
      (format #t "FAIL ~a: expected loud failure, got out=~s status=~a~%"
              label out st)))))

;; 1. format 基数指示子
(check-output "format-~X" "(format (standard-output) \"~X\" 255)" "ff")
(check-output "format-~O" "(format (standard-output) \"~O\" 8)" "10")
(check-output "format-~B" "(format (standard-output) \"~B\" 5)" "101")
(check-output "format-~X-neg" "(format (standard-output) \"~X\" -255)" "-ff")
(check-output "format-mix" "(format (standard-output) \"~A=~X~%\" 255 255)"
              "255=ff")

;; 2. まだネイティブ未対応の構文は「黙って誤った値」を返さず、必ず非 0 終了で
;;    失敗する（誤出力を error へ格下げする P0 の原則を維持）。
;;    実装済みの構文は native-loop/nonlocal/data/closure-smoke 等で検証する。
;;    ここでは未対応が安定している CLOS / dynamic を使う。
(check-fails-loudly "defclass-not-silent"
  "(defclass <p> () ((x :initarg :x :accessor px))) (print (px (make-instance '<p> :x 1)))")
(check-fails-loudly "handler-case-not-silent"
  "(print (handler-case (error \"boom\") (<error> (c) 'caught)))")

(when (file-exists? *tmp*) (sys-unlink *tmp*))

(if (zero? *fail*)
    (begin (format #t "native-p0-smoke: all passed~%") (exit 0))
    (begin (format #t "native-p0-smoke: ~a failed~%" *fail*) (exit 1)))
