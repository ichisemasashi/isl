;;; spec-milestones.scm
;;;
;;; ISLISP 標準 (ISO/IEC 13816) 準拠テストの「正典」ケース集。
;;; common.scm の (kind form expected) 形式に従う:
;;;   kind = value  : (eval form) が expected と equal? であること
;;;   kind = error  : 評価がエラー（コンディション送出）になること
;;;
;;; ここに書かれているのは「ISLISP 標準が要求する正しい振る舞い」である。
;;; 実装が標準から逸脱している項目は意図的に FAIL し、欠落を可視化する。
;;; （現状の欠落一覧は docs/spec-gap-report.md と spec-probe.scm を参照）
;;;
;;; 実行:
;;;   gosh test/conformance/run-spec.scm          ; extended
;;;   gosh test/conformance/run-spec.scm strict    ; strict
;;;
;;; 各グループ末尾のコメント [GAP] は、調査時点で未実装/逸脱だった項目。

;; ==========================================================================
;; §11 Numbers — 算術と比較
;; ==========================================================================

(define spec-numbers
  (list
   ;; 数値比較述語（ISLISP では 2 引数）
   (list 'value '(= 1 1) #t)
   (list 'value '(= 1 2) '())
   (list 'value '(< 1 2) #t)
   (list 'value '(<= 1 1) #t)
   (list 'value '(> 3 2) #t)
   (list 'value '(>= 3 3) #t)
   (list 'value '(/= 1 2) #t)
   ;; 基本算術
   (list 'value '(+ 1 2 3 4) 10)
   (list 'value '(- 10 1 2) 7)
   (list 'value '(* 2 3 4) 24)
   ;; 整数除算: ISLISP は div(切り捨て/床関数) と mod を定義する
   (list 'value '(div 7 2) 3)        ; [GAP] div 未実装
   (list 'value '(div -7 2) -4)      ; [GAP] div は床関数（負で -4）
   (list 'value '(mod -7 2) 1)
   (list 'value '(mod 7 -2) -1)
   ;; quotient は 0 方向への切り捨て
   (list 'value '(quotient 7 2) 3)
   (list 'value '(quotient -7 2) -3)
   ;; gcd / lcm / isqrt
   (list 'value '(gcd 12 8) 4)
   (list 'value '(lcm 4 6) 12)
   (list 'value '(isqrt 17) 4)
   ;; abs / max / min / expt
   (list 'value '(abs -5) 5)
   (list 'value '(max 1 3 2) 3)
   (list 'value '(min 3 1 2) 1)
   (list 'value '(expt 2 10) 1024)
   ;; floor/ceiling/round/truncate は ISLISP では 1 引数（最近接整数を返す）
   (list 'value '(floor 7) 7)
   (list 'value '(ceiling 7) 7)
   (list 'value '(truncate 7) 7)
   (list 'value '(round 7) 7)
   ;; 超越関数（atan2 / 双曲線関数）
   (list 'value '(atan2 1 1) 0.7853981633974483)   ; [GAP] atan2 未実装
   ;; 標準定数（浮動小数点の限界値）
   (list 'value '(floatp *most-negative-float*) #t)  ; [GAP] 定数未定義
   ;; ゼロ除算は <division-by-zero> として捕捉可能でなければならない
   (list 'value '(handler-case (quotient 1 0)
                   (<division-by-zero> (c) 'caught))
         'caught)))                          ; [GAP] quotient のゼロ除算が捕捉不可

;; ==========================================================================
;; §10/§11 Predicates — 型述語
;; ==========================================================================

(define spec-predicates
  (list
   (list 'value '(integerp 1) #t)
   (list 'value '(integerp 1.0) '())
   (list 'value '(floatp 1.0) #t)
   (list 'value '(numberp 1) #t)
   (list 'value '(characterp #\a) #t)
   (list 'value '(stringp "x") #t)
   (list 'value '(symbolp 'a) #t)
   (list 'value '(consp '(1)) #t)
   (list 'value '(consp '()) '())
   (list 'value '(listp '()) #t)
   (list 'value '(null '()) #t)
   (list 'value '(functionp (function car)) #t)
   ;; generic-function-p
   (list 'value '(progn (defgeneric gfp-g (x))
                        (generic-function-p (function gfp-g)))
         #t)                                  ; [GAP] generic-function-p 未実装
   ;; instancep / subclassp（class 形式）
   (list 'value '(instancep 1 (class <integer>)) #t)
   (list 'value '(subclassp (class <integer>) (class <number>)) #t)
   (list 'value '(subclassp (class <number>) (class <integer>)) '())))

;; ==========================================================================
;; §14/§15 Cons & List
;; ==========================================================================

(define spec-cons-list
  (list
   (list 'value '(cons 1 2) '(1 . 2))
   (list 'value '(car '(1 2 3)) 1)
   (list 'value '(cdr '(1 2 3)) '(2 3))
   (list 'value '(create-list 3 'a) '(a a a))
   (list 'value '(reverse '(1 2 3)) '(3 2 1))
   (list 'value '(append '(1 2) '(3 4)) '(1 2 3 4))
   (list 'value '(member 2 '(1 2 3)) '(2 3))
   (list 'value '(member 9 '(1 2 3)) '())
   (list 'value '(assoc 'b '((a . 1) (b . 2))) '(b . 2))
   (list 'value '(mapcar (function -) '(1 2 3)) '(-1 -2 -3))
   (list 'value '(mapcar (function +) '(1 2) '(10 20)) '(11 22))
   (list 'value '(mapcan (lambda (x) (list x x)) '(1 2)) '(1 1 2 2))
   (list 'value '(maplist (function car) '(1 2 3)) '(1 2 3))
   ;; set-car/set-cdr: ISLISP の引数順序は (set-car obj cons)
   (list 'value '(let ((c (list 1 2))) (set-car 9 c) c) '(9 2))   ; [GAP] 実装は引数逆順
   (list 'value '(let ((c (list 1 2))) (set-cdr (list 9) c) c) '(1 9))) ; [GAP] 実装は引数逆順
  )

;; ==========================================================================
;; §15 Sequences
;; ==========================================================================

(define spec-sequence
  (list
   (list 'value '(length '(1 2 3)) 3)
   (list 'value '(length "abc") 3)
   (list 'value '(length (vector 1 2)) 2)
   (list 'value '(elt '(a b c) 1) 'b)
   (list 'value '(elt "abc" 1) #\b)
   (list 'value '(elt (vector 'x 'y 'z) 2) 'z)
   (list 'value '(subseq "hello" 1 3) "el")
   (list 'value '(subseq '(a b c d) 1 3) '(b c))
   ;; set-elt: (set-elt obj seq z)
   (list 'value '(let ((v (vector 0 0 0))) (set-elt 9 v 1) (elt v 1)) 9)
   ;; map-into
   (list 'value '(let ((v (vector 0 0))) (map-into v (function -) '(1 2)) v) #(-1 -2))))

;; ==========================================================================
;; §12 Characters
;; ==========================================================================

(define spec-character
  (list
   (list 'value '(char= #\a #\a) #t)
   (list 'value '(char/= #\a #\b) #t)
   (list 'value '(char< #\a #\b) #t)
   (list 'value '(char<= #\a #\a) #t)
   (list 'value '(char> #\b #\a) #t)
   (list 'value '(char>= #\b #\b) #t)))

;; ==========================================================================
;; §16 Strings
;; ==========================================================================

(define spec-string
  (list
   (list 'value '(create-string 3 #\a) "aaa")
   (list 'value '(string= "abc" "abc") #t)
   (list 'value '(string/= "abc" "abd") #t)
   (list 'value '(string< "abc" "abd") #t)
   (list 'value '(string-append "ab" "cd") "abcd")
   (list 'value '(char-index #\l "hello") 2)
   (list 'value '(char-index #\z "hello") '())
   (list 'value '(string-index "ll" "hello") 2)))

;; ==========================================================================
;; §13 Arrays & Vectors（多次元配列を含む）
;; ==========================================================================

(define spec-array
  (list
   ;; 一般ベクタ
   (list 'value '(create-vector 3 0) #(0 0 0))
   (list 'value '(vector 1 2 3) #(1 2 3))
   (list 'value '(general-vector-p (vector 1 2)) #t)
   ;; 一次元配列
   (list 'value '(array-dimensions (create-array '(3) 0)) '(3))
   ;; 多次元配列（ISLISP 標準では必須）
   (list 'value '(array-dimensions (create-array '(2 3) 0)) '(2 3))  ; [GAP] 多次元未対応
   (list 'value '(aref (create-array '(2 2) 7) 1 1) 7)               ; [GAP] 多次元 aref
   (list 'value '(garef (create-array '(2 2) 5) 0 1) 5)              ; [GAP] garef 未実装
   (list 'value '(let ((a (create-array '(2 2) 0)))
                   (set-aref 9 a 1 1) (aref a 1 1)) 9)               ; [GAP] 多次元 set-aref
   ;; 配列述語
   (list 'value '(basic-array-p (create-array '(2 2) 0)) #t)        ; [GAP]
   (list 'value '(general-array*-p (create-array '(2 2) 0)) #t)))   ; [GAP]

;; ==========================================================================
;; §10 Symbols — プロパティリスト
;; ==========================================================================

(define spec-symbol
  (list
   (list 'value '(symbolp (gensym)) #t)
   ;; property / set-property / remove-property（ISLISP のプロパティリスト API）
   (list 'value '(progn (set-property 7 'psym 'pkey) (property 'psym 'pkey)) 7) ; [GAP]
   (list 'value '(property 'psym-none 'pkey) '())                                ; [GAP]
   (list 'value '(property 'psym-none 'pkey 'def) 'def)                          ; [GAP]
   (list 'value '(progn (set-property 1 'rsym 'rkey)
                        (remove-property 'rsym 'rkey)
                        (property 'rsym 'rkey))
         '())))                                                                  ; [GAP]

;; ==========================================================================
;; §18/§19 Streams & I/O — 文字列ストリームと粒度の細かい出力 API
;; ==========================================================================

(define spec-stream
  (list
   ;; 標準ストリームはアクセサ関数（変数ではない）
   (list 'value '(streamp (standard-output)) #t)
   ;; 文字列出力ストリーム
   (list 'value '(let ((s (create-string-output-stream)))
                   (format-object s "hi" nil)
                   (get-output-stream-string s))
         "hi")                                ; [GAP] string-output-stream 未実装
   ;; 文字列入力ストリーム
   (list 'value '(let ((s (create-string-input-stream "abc")))
                   (read-char s))
         #\a)                                 ; [GAP] string-input-stream 未実装
   (list 'value '(read (create-string-input-stream "(1 2 3)")) '(1 2 3)) ; [GAP]
   ;; preview-char は ISLISP の先読み標準名
   (list 'value '(preview-char (create-string-input-stream "xy")) #\x)    ; [GAP]
   ;; 粒度の細かい出力関数
   (list 'value '(let ((s (create-string-output-stream)))
                   (format-integer s 255 16)
                   (get-output-stream-string s))
         "ff")                                ; [GAP] format-integer 未実装
   (list 'value '(let ((s (create-string-output-stream)))
                   (format-char s #\a)
                   (get-output-stream-string s))
         "a")))                               ; [GAP] format-char 未実装

;; ==========================================================================
;; §22 Conditions — クラス階層・アクセサ・report
;; ==========================================================================

(define spec-condition
  (list
   ;; error はフォーマット文字列＋引数からコンディションを作る
   (list 'value '(handler-case (error "n=~a" 5)
                   (<simple-error> (c)
                     (let ((s (create-string-output-stream)))
                       (report-condition c s)
                       (get-output-stream-string s))))
         "n=5")                               ; [GAP] report-condition / 文字列ストリーム
   ;; simple-error アクセサ
   (list 'value '(handler-case (error "n=~a" 5)
                   (<simple-error> (c) (simple-error-format-string c)))
         "n=~a")                              ; [GAP] simple-error-format-string 未実装
   ;; domain-error アクセサ
   (list 'value '(handler-case (car 5)
                   (<domain-error> (c) (domain-error-object c)))
         5)                                   ; [GAP] domain-error-object 未実装
   ;; arithmetic-error アクセサ
   (list 'value '(handler-case (quotient 1 0)
                   (<arithmetic-error> (c) (instancep (arithmetic-error-operands c)
                                                      (class <list>))))
         #t)                                  ; [GAP] arithmetic-error 捕捉/アクセサ
   ;; continue-condition による継続可能コンディションからの復帰
   (list 'value '(with-handler
                     (lambda (c) (continue-condition c 99))
                   (+ 1 (cerror "use 99" "need a value")))
         100)                                 ; [GAP] continue-condition の戻り値
   ;; クラス階層
   (list 'value '(subclassp (class <simple-error>) (class <error>)) #t)
   (list 'value '(subclassp (class <error>) (class <serious-condition>)) #t)
   (list 'value '(subclassp (class <division-by-zero>) (class <arithmetic-error>)) #t)
   (list 'value '(subclassp (class <end-of-stream>) (class <stream-error>)) #t)))

;; ==========================================================================
;; §21 Object System — create / initialize-object（CLOS）
;; ==========================================================================

(define spec-object
  (list
   (list 'value '(instancep 1 (class-of 1)) #t)
   ;; ISLISP のインスタンス生成は generic function `create`
   (list 'value '(progn
                   (defclass <so-pt> () ((x :initarg x :accessor so-x)))
                   (so-x (create (class <so-pt>) 'x 42)))
         42)                                  ; [GAP] create / initialize-object 未実装
   ;; defclass + defgeneric + defmethod（メソッドディスパッチ）
   (list 'value '(progn
                   (defclass <so-a> () ())
                   (defclass <so-b> (<so-a>) ())
                   (defgeneric so-g (o))
                   (defmethod so-g ((o <so-a>)) 'a)
                   (defmethod so-g ((o <so-b>)) (cons 'b (call-next-method)))
                   (so-g (create (class <so-b>))))
         '(b . a))))                          ; [GAP] create 経由

;; ==========================================================================
;; §23/§24 Control & Special forms
;; ==========================================================================

(define spec-control
  (list
   (list 'value '(case 2 ((1) 'one) ((2) 'two) (t 'other)) 'two)
   (list 'value '(case 9 ((1) 'one) (t 'other)) 'other)
   ;; case-using（述語指定の case）
   (list 'value '(case-using (function =) 2.0 ((1 2) 'lo) (t 'hi)) 'lo)  ; [GAP] case-using
   ;; the / assure（型表明）
   (list 'value '(the <integer> 5) 5)
   (list 'value '(assure <integer> 5) 5)                                ; [GAP] assure
   ;; for ループ
   (list 'value '(for ((i 0 (+ i 1)) (s 0 (+ s i))) ((>= i 4) s)) 6)
   ;; block / return-from
   (list 'value '(block b (return-from b 7) 0) 7)
   ;; catch / throw
   (list 'value '(catch 'k (throw 'k 8) 0) 8)
   ;; tagbody / go
   (list 'value '(let ((x 0))
                   (tagbody a (setq x (+ x 1)) (if (< x 3) (go a)))
                   x)
         3)
   ;; unwind-protect（クリーンアップ実行を確認）
   (list 'value '(let ((log '()))
                   (block b
                     (unwind-protect (return-from b 'done)
                       (setq log (cons 'cleanup log))))
                   log)
         '(cleanup))
   ;; flet / labels
   (list 'value '(flet ((f (x) (* x 2))) (f 5)) 10)
   (list 'value '(labels ((ev? (n) (if (= n 0) t (od? (- n 1))))
                          (od? (n) (if (= n 0) '() (ev? (- n 1)))))
                   (ev? 6))
         #t)
   ;; dynamic / dynamic-let
   (list 'value '(progn (defdynamic *dv* 1)
                        (dynamic-let ((*dv* 9)) (dynamic *dv*)))
         9)
   ;; convert（型変換）
   (list 'value '(convert 65 <character>) #\A)                          ; convert 数値→文字
   (list 'value '(convert '(#\a #\b) <string>) "ab")
   ;; apply / funcall
   (list 'value '(apply (function +) 1 2 '(3 4)) 10)
   (list 'value '(funcall (function +) 1 2 3) 6)))

;; ==========================================================================
;; マイルストーン束ね
;; ==========================================================================

(define spec-conformance-milestones
  (list
   (list "Spec-Numbers"     spec-numbers)
   (list "Spec-Predicates"  spec-predicates)
   (list "Spec-ConsList"    spec-cons-list)
   (list "Spec-Sequence"    spec-sequence)
   (list "Spec-Character"   spec-character)
   (list "Spec-String"      spec-string)
   (list "Spec-Array"       spec-array)
   (list "Spec-Symbol"      spec-symbol)
   (list "Spec-Stream"      spec-stream)
   (list "Spec-Condition"   spec-condition)
   (list "Spec-Object"      spec-object)
   (list "Spec-Control"     spec-control)))
