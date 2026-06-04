;;; native-gap-probe.scm
;;;
;;; ネイティブ AOT バックエンド (`./bin/islc --run`) の対応範囲を、
;;; インタプリタ (`./bin/isl`) をオラクルとして実証的に測定する。
;;;
;;; 各プローブ src を両経路で実行し、標準出力を比較して分類する:
;;;   SAME       … ネイティブ出力 = インタプリタ出力（コンパイラが対応）
;;;   DIFF       … 双方値を返すが食い違う（コンパイラのバグ/逸脱）
;;;   NATIVE-NG  … ネイティブが空/エラー（未対応の機能）
;;;   INTERP-NG  … インタプリタ側が失敗（プローブ対象外・参考）
;;;
;;; 使い方:  gosh test/compiler/native-gap-probe.scm
;;; 終了コード = DIFF + NATIVE-NG の件数。

(add-load-path "./src")
(use gauche.process)
(use srfi-1)
(use srfi-13)

(define *tmp* "test/tmp-native-gap.lsp")

(define (write-src! src)
  (call-with-output-file *tmp*
    (lambda (p) (display src p) (newline p))))

(define (run-cmd cmd)
  ;; stdout を文字列で返す。stderr は捨てる（clang の warning 等）。
  ;; 異常終了時は空文字列扱い。
  (guard (e (else ""))
    (string-trim-both (process-output->string cmd :error :null))))

(define (run-interp src)
  (write-src! src)
  (run-cmd `("./bin/isl" ,*tmp*)))

(define (run-native src)
  (write-src! src)
  (run-cmd `("./bin/islc" "--run" ,*tmp*)))

;; ネイティブ出力に紛れるエラー痕跡（ISL_V_ERROR など）を検知
(define (looks-like-error? s)
  (or (string=? s "")
      (string-scan s "error")
      (string-scan s "Error")
      (string-scan s "unsupported")
      (string-scan s "Unbound")
      (string-scan s "lowering pending")))

(define *probes*
  ;; (label . src)  — src は標準出力に結果を 1 行で書くこと
  '(;; --- 算術・比較 ---
    ("int-arith"      . "(print (+ (* 2 3) (- 10 4)))")
    ("rational"       . "(print (/ 7 2))")
    ("float-arith"    . "(print (+ 1.5 2.5))")
    ("compare"        . "(print (< 1 2))")
    ("expt"           . "(print (expt 2 10))")
    ("mod"            . "(print (mod 7 3))")
    ("abs-max"        . "(print (max (abs -5) 3))")
    ;; --- 制御構造 ---
    ("if"             . "(print (if (= 1 1) 'yes 'no))")
    ("cond"           . "(print (cond ((= 1 2) 'a) (t 'b)))")
    ("case"           . "(print (case 2 ((1) 'one) ((2) 'two) (t 'x)))")
    ("and-or"         . "(print (and 1 (or nil 7)))")
    ("let"            . "(print (let ((x 5)) (* x x)))")
    ("let*"           . "(print (let* ((x 1) (y (+ x 1))) (+ x y)))")
    ("while"          . "(let ((i 0) (s 0)) (while (< i 4) (setq s (+ s i)) (setq i (+ i 1))) (print s))")
    ("for"            . "(print (for ((i 0 (+ i 1)) (s 0 (+ s i))) ((>= i 4) s)))")
    ("dotimes"        . "(let ((s 0)) (dotimes (i 4) (setq s (+ s i))) (print s))")
    ("dolist"         . "(let ((s 0)) (dolist (x '(1 2 3)) (setq s (+ s x))) (print s))")
    ;; --- 関数・クロージャ・再帰 ---
    ("defun-rec"      . "(defun f (n) (if (= n 0) 1 (* n (f (- n 1))))) (print (f 5))")
    ("closure"        . "(print ((lambda (x) ((lambda (y) (+ x y)) 5)) 10))")
    ("flet"           . "(print (flet ((g (x) (* x 2))) (g 21)))")
    ("labels"         . "(print (labels ((ev (n) (if (= n 0) t (od (- n 1)))) (od (n) (if (= n 0) nil (ev (- n 1))))) (ev 6)))")
    ("funcall"        . "(print (funcall (function +) 1 2 3))")
    ("apply"          . "(print (apply (function +) '(1 2 3 4)))")
    ;; --- リスト ---
    ("cons-list"      . "(print (cons 1 (list 2 3)))")
    ("mapcar"         . "(print (mapcar (function +) '(1 2 3) '(10 20 30)))")
    ("append-rev"     . "(print (append (reverse '(1 2)) '(3 4)))")
    ("assoc-member"   . "(print (list (assoc 'b '((a . 1) (b . 2))) (member 2 '(1 2 3))))")
    ;; --- 非局所制御 ---
    ("block-return"   . "(print (block b (return-from b 7) 0))")
    ("catch-throw"    . "(print (catch 'k (throw 'k 8) 0))")
    ("tagbody-go"     . "(let ((x 0)) (tagbody a (setq x (+ x 1)) (if (< x 3) (go a))) (print x))")
    ("unwind-protect" . "(let ((s 0)) (block b (unwind-protect (return-from b 1) (setq s 9))) (print s))")
    ;; --- 例外 ---
    ("handler-case"   . "(print (handler-case (error \"boom\") (<error> (c) 'caught)))")
    ("div-zero"       . "(print (handler-case (/ 1 0) (<error> (c) 'caught)))")
    ;; --- 文字・文字列 ---
    ("string-ops"     . "(print (string-append \"ab\" \"cd\"))")
    ("string-length"  . "(print (length \"hello\"))")
    ("char-cmp"       . "(print (char< #\\a #\\b))")
    ("char-literal"   . "(print #\\A)")
    ;; --- ベクタ・配列 ---
    ("vector"         . "(print (vector 1 2 3))")
    ("vector-ref"     . "(print (elt (vector 10 20 30) 1))")
    ("array"          . "(print (aref (create-array '(3) 7) 0))")
    ;; --- CLOS ---
    ("defclass"       . "(defclass <pt> () ((x :initarg :x :accessor px))) (print (px (make-instance '<pt> :x 42)))")
    ("defmethod"      . "(defclass <a> () ()) (defgeneric g (o)) (defmethod g ((o <a>)) 'ok) (print (g (make-instance '<a>)))")
    ;; --- 動的変数・型 ---
    ("dynamic"        . "(defdynamic *d* 1) (print (dynamic-let ((*d* 9)) (dynamic *d*)))")
    ("the"            . "(print (the <integer> 42))")
    ("convert"        . "(print (convert 65 <character>))")
    ;; --- format ---
    ("format-a"       . "(format (standard-output) \"~A\" 42)")
    ("format-s"       . "(format (standard-output) \"~S\" '(1 2))")
    ("format-d"       . "(format (standard-output) \"~D\" 255)")
    ("format-x"       . "(format (standard-output) \"~X\" 255)")
    ))

(define (classify interp native)
  (cond
   ((string=? interp "") 'INTERP-NG)
   ((looks-like-error? interp) 'INTERP-NG)
   ((looks-like-error? native) 'NATIVE-NG)
   ((string=? interp native) 'SAME)
   (else 'DIFF)))

(define (main args)
  (let ((same 0) (diff 0) (nng 0) (ing 0) (rows '()))
    (for-each
     (lambda (pr)
       (let* ((label (car pr))
              (src   (cdr pr))
              (i (run-interp src))
              (n (run-native src))
              (st (classify i n)))
         (case st
           ((SAME) (set! same (+ same 1)))
           ((DIFF) (set! diff (+ diff 1)))
           ((NATIVE-NG) (set! nng (+ nng 1)))
           ((INTERP-NG) (set! ing (+ ing 1))))
         (set! rows (cons (list st label i n) rows))))
     *probes*)
    (when (file-exists? *tmp*) (sys-unlink *tmp*))
    (set! rows (reverse rows))
    ;; 詳細
    (format #t "=== Native AOT backend gap probe ===~%~%")
    (for-each
     (lambda (r)
       (format #t "[~a] ~a~%" (list-ref r 0) (list-ref r 1))
       (unless (eq? (list-ref r 0) 'SAME)
         (format #t "    interp: ~s~%    native: ~s~%"
                 (list-ref r 2) (list-ref r 3))))
     rows)
    (newline)
    (format #t "--- NATIVE-NG (ネイティブ未対応) ---~%")
    (for-each (lambda (r) (when (eq? (list-ref r 0) 'NATIVE-NG)
                            (format #t "  ~a~%" (list-ref r 1))))
              rows)
    (format #t "--- DIFF (出力相違) ---~%")
    (for-each (lambda (r) (when (eq? (list-ref r 0) 'DIFF)
                            (format #t "  ~a~%" (list-ref r 1))))
              rows)
    (newline)
    (format #t "=== TOTAL: ~a SAME / ~a NATIVE-NG / ~a DIFF / ~a INTERP-NG (全 ~a) ===~%"
            same nng diff ing (length rows))
    (exit (+ diff nng))))
