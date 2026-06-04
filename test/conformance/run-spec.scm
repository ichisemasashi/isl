;;; run-spec.scm — ISLISP 標準準拠テスト（ギャップ検出用）
;;;
;;;   gosh test/conformance/run-spec.scm           ; extended プロファイル
;;;   gosh test/conformance/run-spec.scm strict     ; strict プロファイル
;;;
;;; spec-milestones.scm のケースは「標準が要求する正しい振る舞い」を表す。
;;; 実装に欠落/逸脱があるグループは FAIL し、終了コードは非 0 になる。
;;; 既存の run.scm（緑を維持する回帰テスト）とは独立に運用する。

(add-load-path "./test/conformance")
(load "common.scm")
(load "spec-milestones.scm")

(define (parse-profile args)
  (if (and (pair? (cdr args))
           (string-ci=? (cadr args) "strict"))
      'strict 'extended))

(exit (run-all spec-conformance-milestones (parse-profile (command-line))))
