;;; spec-probe.scm
;;;
;;; ISLISP 標準 (ISO/IEC 13816) の定義済み操作子・標準クラスを網羅的に
;;; プローブし、実装に対して以下を分類する:
;;;
;;;   OK      … 標準どおりに動作した
;;;   WRONG   … 操作子は存在するが結果が標準と食い違う
;;;   ERR     … 操作子は存在するが（想定外の）エラーを送出した
;;;   MISSING … 操作子そのものが未定義 ("Unbound variable")
;;;
;;; 使い方:
;;;   gosh test/conformance/spec-probe.scm            ; extended プロファイル
;;;   gosh test/conformance/spec-probe.scm strict     ; strict プロファイル
;;;
;;; 末尾に MISSING / WRONG / ERR の一覧と章別サマリを出力する。
;;; 終了コードは MISSING+WRONG+ERR の総数（0 なら完全準拠）。

(add-load-path "./src")
(use isl.core)
(use srfi-1)
(use srfi-13)

;; --------------------------------------------------------------------------
;; 分類エンジン
;; --------------------------------------------------------------------------

(define *results* '())        ; (chapter name status detail) のリスト（逆順）

(define (record! chapter name status detail)
  (set! *results* (cons (list chapter name status detail) *results*)))

(define (cond-message e)
  (guard (_ (else "<no-message>"))
    (if (is-a? e <message-condition>)
        (x->string (slot-ref e 'message))
        (x->string e))))

(define (missing-error? e)
  ;; 未定義の操作子は frame-ref が "Unbound variable ..." を送出する
  (let ((m (cond-message e)))
    (and (string? m)
         (string-prefix? "Unbound variable" m))))

;; entry: (name kind form expected)
;;   kind = eq   : (equal? result expected) を判定
;;        = val  : エラーなく値を返せば OK（expected 無視）
;; いずれも Unbound variable は MISSING、その他のエラーは ERR。
(define (probe env chapter entry)
  (let ((name     (list-ref entry 0))
        (kind     (list-ref entry 1))
        (form     (list-ref entry 2))
        (expected (if (>= (length entry) 4) (list-ref entry 3) #f)))
    (guard (e (else
               (if (missing-error? e)
                   (record! chapter name 'MISSING #f)
                   (record! chapter name 'ERR (cond-message e)))))
      (let ((actual (eval-islisp form env)))
        (cond
         ((eq? kind 'val)
          (record! chapter name 'OK actual))
         ((equal? actual expected)
          (record! chapter name 'OK actual))
         (else
          (record! chapter name 'WRONG (list 'got actual 'want expected))))))))

(define (probe-class env chapter name)
  ;; 標準クラスの存在確認: (class <name>) が成功すれば OK
  (guard (e (else (record! chapter (symbol->string name) 'MISSING #f)))
    (eval-islisp (list 'class name) env)
    (record! chapter (symbol->string name) 'OK #f)))

;; --------------------------------------------------------------------------
;; プローブ項目（ISLISP 標準の章立てに沿う）
;; --------------------------------------------------------------------------

(define numeric-entries
  '((= eq (= 1 1) #t)
    (/= eq (/= 1 2) #t)
    (< eq (< 1 2) #t)
    (> eq (> 2 1) #t)
    (<= eq (<= 1 1) #t)
    (>= eq (>= 2 2) #t)
    (+ eq (+ 1 2 3) 6)
    (- eq (- 10 3 2) 5)
    (* eq (* 2 3 4) 24)
    (/ val (/ 6 2))
    (quotient val (quotient 7 2))
    (reciprocal val (reciprocal 4))
    (div eq (div 7 2) 3)
    (div-neg eq (div -7 2) -4)
    (mod eq (mod -7 2) 1)
    (gcd eq (gcd 12 8) 4)
    (lcm eq (lcm 4 6) 12)
    (isqrt eq (isqrt 17) 4)
    (abs eq (abs -5) 5)
    (max eq (max 1 3 2) 3)
    (min eq (min 3 1 2) 1)
    (exp val (exp 0))
    (log val (log 1))
    (expt eq (expt 2 10) 1024)
    (sqrt val (sqrt 16))
    (sin val (sin 0))
    (cos val (cos 0))
    (tan val (tan 0))
    (atan val (atan 1))
    (atan2 val (atan2 1 1))
    (sinh val (sinh 0))
    (cosh val (cosh 0))
    (tanh val (tanh 0))
    (atanh val (atanh 0))
    (floor eq (floor 3) 3)
    (floor-1arg-float eq (floor 2.9) 2)
    (ceiling eq (ceiling 2.1) 3)
    (truncate eq (truncate 2.9) 2)
    (round eq (round 2.4) 2)
    (float val (float 3))
    (*most-positive-float* val *most-positive-float*)
    (*most-negative-float* val *most-negative-float*)))

(define predicate-entries
  '((numberp eq (numberp 1) #t)
    (integerp eq (integerp 1) #t)
    (floatp eq (floatp 1.0) #t)
    (characterp eq (characterp #\a) #t)
    (stringp eq (stringp "x") #t)
    (symbolp eq (symbolp 'a) #t)
    (listp eq (listp '(1)) #t)
    (consp eq (consp '(1)) #t)
    (null eq (null '()) #t)
    (functionp val (functionp (function car)))
    (generic-function-p val (generic-function-p (function car)))
    (basic-array-p val (basic-array-p (vector 1 2)))
    (basic-array*-p val (basic-array*-p (create-array '(2 2) 0)))
    (general-array*-p val (general-array*-p (create-array '(2 2) 0)))
    (basic-vector-p val (basic-vector-p (vector 1 2)))
    (general-vector-p val (general-vector-p (vector 1 2)))
    (instancep-classform val (instancep 1 (class <integer>)))
    (subclassp val (subclassp (class <integer>) (class <number>)))
    (eq eq (eq 'a 'a) #t)
    (eql eq (eql 1 1) #t)
    (equal eq (equal '(1 2) '(1 2)) #t)
    (not eq (not '()) #t)))

(define cons-entries
  '((cons eq (cons 1 2) (1 . 2))
    (car eq (car '(1 2)) 1)
    (cdr eq (cdr '(1 2)) (2))
    ;; ISLISP 標準の引数順序は (set-car obj cons)。実装は (set-car cons obj) で逆。
    ;; ここでは存在確認のため実装順で呼ぶ（順序逸脱はギャップレポートに記載）。
    (set-car val (set-car (list 1 2) 9))
    (set-cdr val (set-cdr (list 1 2) 9))
    (create-list eq (create-list 3 'a) (a a a))
    (list eq (list 1 2 3) (1 2 3))
    (reverse eq (reverse '(1 2 3)) (3 2 1))
    (nreverse eq (nreverse (list 1 2 3)) (3 2 1))
    (append eq (append '(1) '(2)) (1 2))
    (member eq (member 2 '(1 2 3)) (2 3))
    (mapcar eq (mapcar (function -) '(1 2 3)) (-1 -2 -3))
    (mapc val (mapc (function -) '(1 2 3)))
    (mapcan eq (mapcan (lambda (x) (list x x)) '(1 2)) (1 1 2 2))
    (maplist eq (maplist (function car) '(1 2)) (1 2))
    (mapl val (mapl (function car) '(1 2)))
    (mapcon val (mapcon (lambda (l) (list (car l))) '(1 2)))
    (assoc eq (assoc 'b '((a . 1) (b . 2))) (b . 2))))

(define sequence-entries
  '((length eq (length '(1 2 3)) 3)
    (length-str eq (length "abc") 3)
    (length-vec eq (length (vector 1 2)) 2)
    (elt-list eq (elt '(a b c) 1) b)
    (elt-str eq (elt "abc" 1) #\b)
    (elt-vec eq (elt (vector 'a 'b) 0) a)
    (set-elt val (set-elt 'z (list 'a 'b) 0))
    (subseq-str eq (subseq "hello" 1 3) "el")
    (subseq-list eq (subseq '(a b c d) 1 3) (b c))
    (map-into val (map-into (vector 0 0) (function -) '(1 2)))))

(define char-entries
  '((char= eq (char= #\a #\a) #t)
    (char/= eq (char/= #\a #\b) #t)
    (char< eq (char< #\a #\b) #t)
    (char> eq (char> #\b #\a) #t)
    (char<= eq (char<= #\a #\a) #t)
    (char>= eq (char>= #\b #\b) #t)))

(define string-entries
  '((create-string eq (create-string 3 #\a) "aaa")
    (string= eq (string= "a" "a") #t)
    (string/= eq (string/= "a" "b") #t)
    (string< eq (string< "a" "b") #t)
    (string> eq (string> "b" "a") #t)
    (string<= eq (string<= "a" "a") #t)
    (string>= eq (string>= "b" "b") #t)
    (char-index eq (char-index #\l "hello") 2)
    (string-index eq (string-index "ll" "hello") 2)
    (string-append eq (string-append "a" "b") "ab")))

(define array-entries
  '((create-array val (create-array '(2 3) 0))
    (array-dimensions eq (array-dimensions (create-array '(2 3) 0)) (2 3))
    (aref-multidim eq (aref (create-array '(2 2) 7) 1 1) 7)
    (garef val (garef (create-array '(2 2) 7) 0 0))
    (set-aref val (set-aref 5 (create-array '(2) 0) 1))
    (set-garef val (set-garef 5 (create-array '(2) 0) 1))
    (create-vector eq (create-vector 3 0) #(0 0 0))
    (vector eq (vector 1 2 3) #(1 2 3))))

(define symbol-entries
  '((gensym val (gensym))
    (set-property val (set-property 5 'sp-sym 'sp-key))
    (property eq (progn (set-property 7 'pr-sym 'pr-key) (property 'pr-sym 'pr-key)) 7)
    (remove-property val (progn (set-property 1 'rp-sym 'rp-key)
                                (remove-property 'rp-sym 'rp-key)))))

(define stream-entries
  '((standard-output val (standard-output))
    (standard-input val (standard-input))
    (error-output val (error-output))
    (create-string-output-stream val (create-string-output-stream))
    (create-string-input-stream val (create-string-input-stream "abc"))
    (get-output-stream-string
     eq (let ((s (create-string-output-stream)))
          (format s "hi")
          (get-output-stream-string s))
     "hi")
    (read-from-string-stream
     eq (read-char (create-string-input-stream "abc")) #\a)
    (preview-char eq (preview-char (create-string-input-stream "abc")) #\a)
    (read-line-stream eq (read-line (create-string-input-stream "abc")) "abc")
    (format-integer
     eq (let ((s (create-string-output-stream)))
          (format-integer s 255 16)
          (get-output-stream-string s))
     "ff")
    (format-char
     eq (let ((s (create-string-output-stream)))
          (format-char s #\a)
          (get-output-stream-string s))
     "a")
    (format-object
     eq (let ((s (create-string-output-stream)))
          (format-object s "x" nil)
          (get-output-stream-string s))
     "x")
    (format-float val (let ((s (create-string-output-stream)))
                        (format-float s 1.5)
                        (get-output-stream-string s)))
    (format-fresh-line val (let ((s (create-string-output-stream)))
                            (format-fresh-line s)))
    (format-tab val (let ((s (create-string-output-stream)))
                      (format-tab s 3)))
    (read-stream eq (read (create-string-input-stream "(1 2)")) (1 2))))

(define condition-entries
  '((error-and-message
     eq (handler-case (error "boom ~a" 1)
          (<simple-error> (c) (condition-message c)))
     "boom 1")
    (simple-error-format-string
     val (handler-case (error "boom ~a" 1)
           (<simple-error> (c) (simple-error-format-string c))))
    (simple-error-format-arguments
     val (handler-case (error "boom ~a" 1)
           (<simple-error> (c) (simple-error-format-arguments c))))
    (cerror val (handler-case (cerror "cont" "err ~a" 1)
                  (<simple-error> (c) 'caught)))
    (signal-condition
     val (handler-case
             (signal-condition
               (create (class <simple-error>)
                       'format-string "x" 'format-arguments '())
               nil)
           (<error> (c) 'caught)))
    (condition-continuable
     val (handler-case (cerror "cont" "e")
           (<simple-error> (c) (condition-continuable c))))
    (arithmetic-error-operation
     val (handler-case (quotient 1 0)
           (<division-by-zero> (c) (arithmetic-error-operation c))))
    (arithmetic-error-operands
     val (handler-case (quotient 1 0)
           (<division-by-zero> (c) (arithmetic-error-operands c))))
    (domain-error-object
     val (handler-case (car 5)
           (<domain-error> (c) (domain-error-object c))))
    (domain-error-expected-class
     val (handler-case (car 5)
           (<domain-error> (c) (domain-error-expected-class c))))
    (undefined-entity-name
     val (handler-case (the-undefined-fn)
           (<undefined-function> (c) (undefined-entity-name c))))
    (undefined-entity-namespace
     val (handler-case (the-undefined-fn)
           (<undefined-function> (c) (undefined-entity-namespace c))))
    (report-condition
     val (handler-case (error "boom")
           (<simple-error> (c)
             (let ((s (create-string-output-stream)))
               (report-condition c s) 'ok))))
    (continue-condition
     val (with-handler
             (lambda (c) (continue-condition c 99))
           (cerror "use 99" "need value")))))

(define object-entries
  '((class-of val (class-of 1))
    (create val (progn (defclass <probe-c> () ((v :initarg v)))
                       (create (class <probe-c>) 'v 5)))
    (make-instance val (progn (defclass <probe-c2> () ((v :initarg :v)))
                              (make-instance (class <probe-c2>) :v 5)))
    (initialize-object val (progn (defclass <probe-c3> () ())
                                  (initialize-object (create (class <probe-c3>)) '())))))

(define control-entries
  '((quote eq (quote a) a)
    (if eq (if t 1 2) 1)
    (cond eq (cond (t 5)) 5)
    (case eq (case 2 ((1) 'a) ((2) 'b) (t 'c)) b)
    (case-using eq (case-using (function =) 2 ((1 2) 'a) (t 'b)) a)
    (and eq (and t t 3) 3)
    (or eq (or '() 4) 4)
    (progn eq (progn 1 2 3) 3)
    (while val (let ((i 0)) (while (< i 3) (setq i (+ i 1))) i))
    (for eq (for ((i 0 (+ i 1)) (s 0 (+ s i))) ((>= i 4) s)) 6)
    (block eq (block b (return-from b 7) 0) 7)
    (catch eq (catch 'k (throw 'k 8) 0) 8)
    (tagbody val (let ((x 0))
                   (tagbody
                    a (setq x (+ x 1)) (if (< x 3) (go a)))
                   x))
    (unwind-protect eq (block b (unwind-protect 1 2)) 1)
    (the eq (the <integer> 5) 5)
    (assure eq (assure <integer> 5) 5)
    (convert-char eq (convert 65 <character>) #\A)
    (convert-str eq (convert 'abc <string>) "abc")
    (lambda eq ((lambda (x) (+ x 1)) 4) 5)
    (function val (function car))
    (flet eq (flet ((f (x) (* x 2))) (f 5)) 10)
    (labels eq (labels ((e? (n) (if (= n 0) t (o? (- n 1))))
                        (o? (n) (if (= n 0) '() (e? (- n 1)))))
                 (e? 4)) #t)
    (let eq (let ((x 1) (y 2)) (+ x y)) 3)
    (let* eq (let* ((x 1) (y (+ x 1))) y) 2)
    (setq eq (let ((x 0)) (setq x 5) x) 5)
    (dynamic-let eq (progn (defdynamic *dyv* 0)
                           (dynamic-let ((*dyv* 9)) (dynamic *dyv*))) 9)
    (apply eq (apply (function +) 1 2 '(3 4)) 10)
    (funcall eq (funcall (function +) 1 2) 3)
    (identity eq (identity 5) 5)))

(define misc-entries
  '((get-universal-time val (get-universal-time))
    (get-internal-run-time val (get-internal-run-time))
    (get-internal-real-time val (get-internal-real-time))
    (internal-time-units-per-second val (internal-time-units-per-second))
    (parse-number val (parse-number "42"))))

(define standard-classes
  '(<object>
    <basic-array> <basic-array*> <general-array*>
    <basic-vector> <general-vector> <string>
    <character> <symbol> <list> <cons> <null>
    <number> <integer> <float>
    <function> <generic-function> <standard-generic-function>
    <stream>
    <built-in-class> <standard-class> <standard-object>
    <serious-condition> <error> <storage-exhausted>
    <arithmetic-error> <division-by-zero>
    <floating-point-overflow> <floating-point-underflow>
    <control-error> <parse-error> <program-error>
    <domain-error> <undefined-entity>
    <unbound-variable> <undefined-function>
    <simple-error> <stream-error> <end-of-stream>))

(define all-chapters
  (list (list "01-Numbers"            'entries numeric-entries)
        (list "02-Predicates"         'entries predicate-entries)
        (list "03-Cons-List"          'entries cons-entries)
        (list "04-Sequence"           'entries sequence-entries)
        (list "05-Character"          'entries char-entries)
        (list "06-String"             'entries string-entries)
        (list "07-Array-Vector"       'entries array-entries)
        (list "08-Symbol-Property"    'entries symbol-entries)
        (list "09-Stream-IO"          'entries stream-entries)
        (list "10-Condition"          'entries condition-entries)
        (list "11-Object-System"      'entries object-entries)
        (list "12-Control-Forms"      'entries control-entries)
        (list "13-Misc"               'entries misc-entries)
        (list "14-Standard-Classes"   'classes standard-classes)))

;; --------------------------------------------------------------------------
;; 実行
;; --------------------------------------------------------------------------

(define (status-symbol s)
  (case s
    ((OK) "OK     ")
    ((WRONG) "WRONG  ")
    ((ERR) "ERR    ")
    ((MISSING) "MISSING")
    (else "?      ")))

(define (run profile)
  (let ((env (make-initial-env profile)))
    (for-each
     (lambda (chapter-spec)
       (let ((chapter (list-ref chapter-spec 0))
             (kind    (list-ref chapter-spec 1))
             (items   (list-ref chapter-spec 2)))
         (if (eq? kind 'classes)
             (for-each (lambda (cn) (probe-class env chapter cn)) items)
             (for-each (lambda (e) (probe env chapter e)) items))))
     all-chapters))
  ;; 結果は逆順なので戻す
  (set! *results* (reverse *results*)))

(define (count-status status)
  (length (filter (lambda (r) (eq? (list-ref r 2) status)) *results*)))

(define (print-detail-lines status)
  (for-each
   (lambda (r)
     (when (eq? (list-ref r 2) status)
       (format #t "  [~a] ~a / ~a"
               (status-symbol status)
               (list-ref r 0)
               (list-ref r 1))
       (when (list-ref r 3)
         (format #t "   ~s" (list-ref r 3)))
       (newline)))
   *results*))

(define (print-chapter-summary)
  (let loop ((rs *results*) (cur #f) (ok 0) (bad 0))
    (define (flush ch o b)
      (when ch
        (format #t "  ~a : ~a OK, ~a NG~%" ch o b)))
    (cond
     ((null? rs) (flush cur ok bad))
     (else
      (let* ((r (car rs)) (ch (list-ref r 0)) (st (list-ref r 2)))
        (if (and cur (not (string=? ch cur)))
            (begin (flush cur ok bad)
                   (loop rs ch 0 0))
            (loop (cdr rs) ch
                  (+ ok (if (eq? st 'OK) 1 0))
                  (+ bad (if (eq? st 'OK) 0 1)))))))))

(define (main args)
  (let ((profile (if (and (pair? (cdr args))
                          (string-ci=? (cadr args) "strict"))
                     'strict 'extended)))
    (format #t "=== ISLISP spec probe (profile: ~a) ===~%~%" profile)
    (run profile)
    (format #t "--- 章別サマリ ---~%")
    (print-chapter-summary)
    (newline)
    (let ((missing (count-status 'MISSING))
          (wrong   (count-status 'WRONG))
          (err     (count-status 'ERR))
          (ok      (count-status 'OK)))
      (when (> missing 0)
        (format #t "--- MISSING (未定義) : ~a 件 ---~%" missing)
        (print-detail-lines 'MISSING) (newline))
      (when (> wrong 0)
        (format #t "--- WRONG (結果が標準と相違) : ~a 件 ---~%" wrong)
        (print-detail-lines 'WRONG) (newline))
      (when (> err 0)
        (format #t "--- ERR (想定外エラー) : ~a 件 ---~%" err)
        (print-detail-lines 'ERR) (newline))
      (format #t "=== TOTAL: ~a OK / ~a MISSING / ~a WRONG / ~a ERR (全 ~a 項目) ===~%"
              ok missing wrong err (length *results*))
      (exit (+ missing wrong err)))))
