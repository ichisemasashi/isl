# ISLISP 標準準拠 実装手順書

基準: ISO/IEC 13816:2007 (ISLISP)  
実装対象: `src/isl/core.scm`  
前提: `docs/islisp-conformance-gaps.md` で列挙したギャップを解消する。  
作成日: 2026-05-03  
改訂日: 2026-05-03（ギャップ文書との照合によりカバー率 100% に更新）

---

## 読み方

各フェーズは「前のフェーズに依存しない範囲で独立して実装できる」ように設計した。  
ただし Phase 5（条件システム）は Phase 3・4 の完了後に着手することを推奨する。  
Phase 7（OOP）は Phase 5 の条件クラス階層が揃ってから着手するとエラー通知が標準準拠になる。

各タスクの末尾に **確認コマンド** を記す。`gosh` が使える環境で実行する。

---

## Phase 1: 算術・型述語の補完

**なぜ先か**: `install-primitives!` の末尾に `(def ...)` を追記するだけで完結する。  
既存コードへの影響はなく、テストも単独で書ける。

### 1-A. 算術関数の追加

**対象**: `abs`, `max`, `min`, `expt`, `sqrt`, `rem`, `gcd`, `lcm`, `signum`, `negate`, `float`, `isqrt`  
**場所**: `src/isl/core.scm` の `install-primitives!` 内、`(def '+ +)` の近く（行 3070 付近）

```scheme
(def 'abs
  (lambda (x)
    (unless (number? x) (error "abs needs a number" x))
    (abs x)))
(def 'max
  (lambda xs
    (unless (and (pair? xs) (every number? xs))
      (error "max needs at least one number" xs))
    (apply max xs)))
(def 'min
  (lambda xs
    (unless (and (pair? xs) (every number? xs))
      (error "min needs at least one number" xs))
    (apply min xs)))
(def 'expt
  (lambda (base exp)
    (unless (number? base) (error "expt base must be number" base))
    (unless (number? exp)  (error "expt exponent must be number" exp))
    (expt base exp)))
(def 'sqrt
  (lambda (x)
    (unless (number? x) (error "sqrt needs a number" x))
    (when (negative? x) (error "sqrt of negative number" x))
    (sqrt x)))
(def 'rem
  (lambda (n d)
    (unless (integer? n) (error "rem dividend must be integer" n))
    (unless (integer? d) (error "rem divisor must be integer" d))
    (remainder n d)))                    ; Gauche の remainder が ISO rem と同じ符号規則
(def 'gcd
  (lambda xs
    (if (null? xs) 0 (apply gcd xs))))
(def 'lcm
  (lambda xs
    (if (null? xs) 1 (apply lcm xs))))
(def 'signum
  (lambda (x)
    (unless (number? x) (error "signum needs a number" x))
    (cond ((positive? x) 1) ((negative? x) -1) (else 0))))
(def 'negate
  (lambda (x)
    (unless (number? x) (error "negate needs a number" x))
    (- x)))
(def 'float
  (lambda (x)
    (unless (number? x) (error "float needs a number" x))
    (exact->inexact x)))
(def 'isqrt
  (lambda (n)
    (unless (and (integer? n) (>= n 0))
      (error "isqrt needs a non-negative integer" n))
    (exact (floor (sqrt n)))))
```

### 1-B. 数値変換関数

**対象**: `number->string`, `string->number`

```scheme
(def 'number->string
  (lambda args
    (unless (and (pair? args) (number? (car args)))
      (error "number->string needs a number" args))
    (let ((n     (car args))
          (radix (if (and (pair? (cdr args)) (integer? (cadr args)))
                     (cadr args) 10)))
      (unless (memv radix '(2 8 10 16))
        (error "number->string radix must be 2, 8, 10 or 16" radix))
      (number->string n radix))))   ; Gauche の number->string をそのまま使える
(def 'string->number
  (lambda args
    (unless (and (pair? args) (string? (car args)))
      (error "string->number needs a string" args))
    (let ((s     (car args))
          (radix (if (and (pair? (cdr args)) (integer? (cadr args)))
                     (cadr args) 10)))
      (let ((result (string->number s radix)))
        (if result result '())))))  ; 失敗時は nil = '()
```

### 1-C. 型述語の追加

**対象**: `consp`, `characterp`, `integerp`, `floatp`, `functionp`, `general-vector-p`, `general-array*-p`

```scheme
(def 'consp pair?)
(def 'characterp char?)
(def 'integerp integer?)
(def 'floatp inexact?)            ; Gauche では inexact? が float 判定
(def 'functionp
  (lambda (x)
    (or (closure? x) (primitive? x))))  ; closure? / primitive? は既存のisl内部述語
;; ISLISP 標準名。vectorp の別名。
(def 'general-vector-p vector?)
;; 多次元配列の判定。isl では vector で 1 次元のみ実装のため vector? と同一。
;; 多次元配列を実装した後はここを更新する。
(def 'general-array*-p vector?)
```

### 1-D. 三角関数・超越関数

**対象**: `exp`, `log`, `sin`, `cos`, `tan`, `asin`, `acos`, `atan`

```scheme
(def 'exp  (lambda (x) (unless (number? x) (error "exp needs number" x))  (exp x)))
(def 'log  (lambda (x) (unless (number? x) (error "log needs number" x))  (log x)))
(def 'sin  (lambda (x) (unless (number? x) (error "sin needs number" x))  (sin x)))
(def 'cos  (lambda (x) (unless (number? x) (error "cos needs number" x))  (cos x)))
(def 'tan  (lambda (x) (unless (number? x) (error "tan needs number" x))  (tan x)))
(def 'asin (lambda (x) (unless (number? x) (error "asin needs number" x)) (asin x)))
(def 'acos (lambda (x) (unless (number? x) (error "acos needs number" x)) (acos x)))
(def 'atan
  (lambda args
    (cond
     ((= (length args) 1)
      (unless (number? (car args)) (error "atan needs number" (car args)))
      (atan (car args)))
     ((= (length args) 2)
      (unless (number? (car args))  (error "atan y must be number" (car args)))
      (unless (number? (cadr args)) (error "atan x must be number" (cadr args)))
      (atan (car args) (cadr args)))
     (else (error "atan takes 1 or 2 arguments" args)))))
```

### 1-E. `quotient` と `floor`/`ceiling`/`truncate`/`round` の2引数版

**対象**: `quotient`、および既存の `floor`/`ceiling`/`truncate`/`round` の2引数対応  
**場所**: 既存の `(def 'floor floor)` 等の定義（行 3075 付近）を以下に置き換える。

ISLISP では `(floor x divisor)` の2引数版は商（整数）のみを返す（Common Lisp とは異なり多値ではない）。

```scheme
;; 既存の (def 'floor floor) を以下に置き換える:
(def 'floor
  (lambda args
    (cond
     ((= (length args) 1)
      (floor (car args)))
     ((= (length args) 2)
      (let ((x (car args)) (d (cadr args)))
        (unless (number? x) (error "floor x must be number" x))
        (unless (number? d) (error "floor divisor must be number" d))
        (floor (/ x d))))      ; 商のみ返す（ISLISP は多値なし）
     (else (error "floor takes 1 or 2 arguments" args)))))
(def 'ceiling
  (lambda args
    (cond
     ((= (length args) 1) (ceiling (car args)))
     ((= (length args) 2)
      (let ((x (car args)) (d (cadr args)))
        (unless (number? x) (error "ceiling x must be number" x))
        (unless (number? d) (error "ceiling divisor must be number" d))
        (ceiling (/ x d))))
     (else (error "ceiling takes 1 or 2 arguments" args)))))
(def 'truncate
  (lambda args
    (cond
     ((= (length args) 1) (truncate (car args)))
     ((= (length args) 2)
      (let ((x (car args)) (d (cadr args)))
        (unless (number? x) (error "truncate x must be number" x))
        (unless (number? d) (error "truncate divisor must be number" d))
        (truncate (/ x d))))
     (else (error "truncate takes 1 or 2 arguments" args)))))
(def 'round
  (lambda args
    (cond
     ((= (length args) 1) (round (car args)))
     ((= (length args) 2)
      (let ((x (car args)) (d (cadr args)))
        (unless (number? x) (error "round x must be number" x))
        (unless (number? d) (error "round divisor must be number" d))
        (round (/ x d))))
     (else (error "round takes 1 or 2 arguments" args)))))
;; quotient = truncate の整数版
(def 'quotient
  (lambda (n d)
    (unless (integer? n) (error "quotient n must be integer" n))
    (unless (integer? d) (error "quotient d must be integer" d))
    (quotient n d)))   ; Gauche の quotient をそのまま使える
```

### 確認

```sh
gosh -I src -e '(use isl.core)' -e \
  '(let ((e (make-initial-env (quote strict))))
     (for-each (lambda (f) (display (eval-islisp f e)) (newline))
       (quote ((abs -3) (max 1 2 3) (min 1 2 3) (expt 2 10)
               (sqrt 4.0) (rem 7 3) (rem -7 3) (quotient 7 3)
               (floor 7 2) (ceiling 7 2) (truncate 7 2) (round 7 2)
               (number->string 255 16) (string->number "ff" 16)
               (consp (cons 1 2)) (integerp 3) (floatp 1.5)
               (general-vector-p (vector 1 2))))))'
```

---

## Phase 2: リスト・文字列・文字・ベクター・配列操作の補完

### 2-A. リスト関数の追加

**場所**: `install-primitives!` 内、`(def 'append ...)` の近く（行 3204 付近）

```scheme
(def 'reverse
  (lambda (xs)
    (unless (list? xs) (error "reverse needs a list" xs))
    (reverse xs)))
(def 'nreverse          ; 破壊的逆順（Gauche には reverse! がある）
  (lambda (xs)
    (unless (list? xs) (error "nreverse needs a list" xs))
    (reverse! xs)))     ; Gauche の reverse! を使用
(def 'map               ; ISLISP の map = CL の mapcar（多リスト版含む）
  (lambda (f . lists)
    (when (null? lists) (error "map needs at least one list"))
    (for-each (lambda (l) (unless (list? l) (error "map needs lists" l))) lists)
    (apply map (lambda xs (apply-islisp f xs)) lists)))
(def 'for-each
  (lambda (f . lists)
    (when (null? lists) (error "for-each needs at least one list"))
    (for-each (lambda (l) (unless (list? l) (error "for-each needs lists" l))) lists)
    (apply for-each (lambda xs (apply-islisp f xs)) lists)
    '()))
(def 'mapc              ; 副作用のみ。第1リストをそのまま返す
  (lambda (f . lists)
    (when (null? lists) (error "mapc needs at least one list"))
    (for-each (lambda (l) (unless (list? l) (error "mapc needs lists" l))) lists)
    (apply for-each (lambda xs (apply-islisp f xs)) lists)
    (car lists)))
(def 'mapcan            ; 結果リストを nconc で結合
  (lambda (f . lists)
    (when (null? lists) (error "mapcan needs at least one list"))
    (for-each (lambda (l) (unless (list? l) (error "mapcan needs lists" l))) lists)
    (apply append (apply map (lambda xs (apply-islisp f xs)) lists))))
(def 'maplist           ; cdr 部分を対象に map
  (lambda (f . lists)
    (when (null? lists) (error "maplist needs at least one list"))
    (for-each (lambda (l) (unless (list? l) (error "maplist needs lists" l))) lists)
    (let loop ((tails lists) (acc '()))
      (if (any null? tails)
          (reverse acc)
          (loop (map cdr tails)
                (cons (apply-islisp f tails) acc))))))
(def 'member
  (lambda (obj lst . opts)
    (let ((test (if (and (pair? opts) (eq? (car opts) ':test))
                    (cadr opts) #f)))
      (unless (list? lst) (error "member needs a list" lst))
      (let loop ((rest lst))
        (cond
         ((null? rest) '())
         (test
          (if (truthy? (apply-islisp test (list obj (car rest))))
              rest (loop (cdr rest))))
         (else
          (if (eqv? obj (car rest))
              rest (loop (cdr rest)))))))))
(def 'assoc
  (lambda (key alist . opts)
    (let ((test (if (and (pair? opts) (eq? (car opts) ':test))
                    (cadr opts) #f)))
      (unless (list? alist) (error "assoc needs an alist" alist))
      (let loop ((rest alist))
        (cond
         ((null? rest) '())
         ((not (pair? (car rest))) (loop (cdr rest)))
         (test
          (if (truthy? (apply-islisp test (list key (caar rest))))
              (car rest) (loop (cdr rest))))
         (else
          (if (eqv? key (caar rest))
              (car rest) (loop (cdr rest)))))))))
(def 'remove            ; 要素除去（非破壊）
  (lambda (obj lst . opts)
    (let ((test (if (and (pair? opts) (eq? (car opts) ':test))
                    (cadr opts) #f)))
      (unless (list? lst) (error "remove needs a list" lst))
      (let loop ((rest lst) (acc '()))
        (cond
         ((null? rest) (reverse acc))
         ((if test
              (truthy? (apply-islisp test (list obj (car rest))))
              (eqv? obj (car rest)))
          (loop (cdr rest) acc))
         (else
          (loop (cdr rest) (cons (car rest) acc))))))))
(def 'nconc             ; 破壊的 append
  (lambda lists
    (cond
     ((null? lists) '())
     ((null? (cdr lists)) (car lists))
     (else
      (let ((first (car lists)))
        (if (null? first)
            (apply-islisp (frame-ref env 'nconc) (cdr lists))
            (begin
              (set-cdr! (last-pair first)
                        (apply-islisp (frame-ref env 'nconc) (cdr lists)))
              first)))))))
(def 'list-ref
  (lambda (lst i)
    (unless (list? lst) (error "list-ref needs a list" lst))
    (unless (and (integer? i) (>= i 0)) (error "list-ref index must be non-negative integer" i))
    (let loop ((rest lst) (n i))
      (cond
       ((null? rest) (error "list-ref index out of range" i))
       ((= n 0) (car rest))
       (else (loop (cdr rest) (- n 1)))))))
(def 'list-tail
  (lambda (lst i)
    (unless (list? lst) (error "list-tail needs a list" lst))
    (unless (and (integer? i) (>= i 0)) (error "list-tail index must be non-negative integer" i))
    (let loop ((rest lst) (n i))
      (cond
       ((= n 0) rest)
       ((null? rest) (error "list-tail index out of range" i))
       (else (loop (cdr rest) (- n 1)))))))
(def 'last-pair
  (lambda (lst)
    (unless (pair? lst) (error "last-pair needs a non-empty list" lst))
    (let loop ((rest lst))
      (if (null? (cdr rest)) rest (loop (cdr rest))))))
(def 'create-list
  (lambda args
    (unless (and (pair? args) (integer? (car args)) (>= (car args) 0))
      (error "create-list needs a non-negative integer" args))
    (let ((n    (car args))
          (init (if (pair? (cdr args)) (cadr args) '())))
      (let loop ((i n) (acc '()))
        (if (= i 0) acc (loop (- i 1) (cons init acc)))))))
(def 'list-copy
  (lambda (lst)
    (unless (list? lst) (error "list-copy needs a list" lst))
    (map (lambda (x) x) lst)))
(def 'nthcdr
  (lambda (n lst)
    (unless (and (integer? n) (>= n 0)) (error "nthcdr n must be non-negative integer" n))
    (let loop ((rest lst) (i n))
      (if (= i 0) rest
          (begin
            (unless (pair? rest) (error "nthcdr list too short" n))
            (loop (cdr rest) (- i 1)))))))
(def 'sort
  (lambda (lst pred)
    (unless (list? lst) (error "sort needs a list" lst))
    (sort lst (lambda (a b) (truthy? (apply-islisp pred (list a b)))))))
```

**注意 (`nconc`)**: `nconc` の実装内で `last-pair` を使うため、`last-pair` より後に登録すること。  
また `frame-ref env 'nconc` での再帰呼び出しを避けるために `letrec` か名前付き `let loop` に書き直す選択肢もある。

### 2-B. 文字列関数の追加

**場所**: `install-primitives!` 内、`(def 'substring ...)` の近く

```scheme
(def 'string-length
  (lambda (s)
    (ensure-string s "string-length")
    (string-length s)))
(def 'string-ref
  (lambda (s i)
    (ensure-string s "string-ref")
    (ensure-nonnegative-integer i "string-ref")
    (let ((n (string-length s)))
      (unless (< i n) (error "string-ref index out of range" i n))
      (string-ref s i))))
(def 'string-copy
  (lambda (s)
    (ensure-string s "string-copy")
    (string-copy s)))
(def 'string-upcase
  (lambda (s)
    (ensure-string s "string-upcase")
    (string-upcase s)))
(def 'string-downcase
  (lambda (s)
    (ensure-string s "string-downcase")
    (string-downcase s)))
```

`(setf (string-ref s i) c)` のサポートは Phase 3 の `setf` 拡張で対応する。

### 2-C. 文字比較関数の追加

```scheme
(def 'char=  (lambda (a b) (ensure-char a "char=")  (ensure-char b "char=")  (char=?  a b)))
(def 'char/= (lambda (a b) (ensure-char a "char/=") (ensure-char b "char/=") (not (char=? a b))))
(def 'char<  (lambda (a b) (ensure-char a "char<")  (ensure-char b "char<")  (char<?  a b)))
(def 'char>  (lambda (a b) (ensure-char a "char>")  (ensure-char b "char>")  (char>?  a b)))
(def 'char<= (lambda (a b) (ensure-char a "char<=") (ensure-char b "char<=") (char<=? a b)))
(def 'char>= (lambda (a b) (ensure-char a "char>=") (ensure-char b "char>=") (char>=? a b)))
(def 'char-upcase
  (lambda (c) (ensure-char c "char-upcase") (char-upcase c)))
(def 'char-downcase
  (lambda (c) (ensure-char c "char-downcase") (char-downcase c)))
```

### 2-D. ベクター ISLISP 標準名の追加

ISLISP の標準関数名は Scheme と異なる。strict モードで標準名が使えるよう別名を登録する。  
**場所**: `install-primitives!` 内、`(def 'vector-ref ...)` の直後

```scheme
;; ISLISP 標準名を既存の実装の別名として登録
(def 'create-vector            ; ISLISP 標準名 = make-vector
  (lambda args
    (build-vector-from-args args "create-vector")))
(def 'general-vector-ref       ; ISLISP 標準名 = vector-ref
  (lambda (v i)
    (unless (vector? v) (error "general-vector-ref target must be vector" v))
    (unless (and (integer? i) (>= i 0) (< i (vector-length v)))
      (error "general-vector-ref index out of range" i))
    (vector-ref v i)))
;; (setf (general-vector-ref v i) val) のサポートは Phase 3 の setf 拡張で対応する。
;; setf の place として 'general-vector-ref を認識するケースを eval-setf に追加する。
```

`setf` の place 拡張（Phase 3）では以下を `eval-setf` に追加する:

```scheme
;; eval-setf 内の place ディスパッチに追加:
;;   ((general-vector-ref)
;;    (let ((v (eval-islisp* (cadr place) env #f))
;;          (i (eval-islisp* (caddr place) env #f)))
;;      (unless (vector? v) (error "setf general-vector-ref: not a vector" v))
;;      (unless (and (integer? i) (>= i 0) (< i (vector-length v)))
;;        (error "setf general-vector-ref: index out of range" i))
;;      (vector-set! v i val)
;;      val))
```

### 2-E. 多次元配列関数の追加

ISLISP は多次元配列を `<general-array*>` として規定している。  
現在の isl は `make-array` で 1 次元配列（vector）のみ実装している。  
ここでは `create-array` と `array-dimensions` を 1 次元の範囲で実装し、  
多次元対応は将来の拡張として印を付ける。

**場所**: `install-primitives!` 内、`(def 'make-array ...)` の直後

```scheme
(def 'create-array         ; ISLISP 標準名。現状は 1 次元のみ。
  (lambda args
    ;; args = (dimensions &optional initial-element)
    ;; dimensions は非負整数か 1 要素の整数リスト
    (build-vector-from-args args "create-array")))
(def 'array-dimensions     ; 配列の次元リストを返す
  (lambda (arr)
    (unless (vector? arr) (error "array-dimensions needs an array" arr))
    (list (vector-length arr))))   ; 1 次元のみ対応
```

`(setf (aref arr i) val)` は既存の `setf` が `aref` を place として認識しているか確認し、  
していない場合は `eval-setf` に以下を追加する:

```scheme
;;   ((aref)
;;    (let ((arr (eval-islisp* (cadr place) env #f))
;;          (i   (eval-islisp* (caddr place) env #f)))
;;      (unless (vector? arr) (error "setf aref: not an array" arr))
;;      (unless (and (integer? i) (>= i 0) (< i (vector-length arr)))
;;        (error "setf aref: index out of range" i))
;;      (vector-set! arr i val)
;;      val))
```

### 確認

```sh
gosh -I src -e '(use isl.core)' -e \
  '(let ((e (make-initial-env (quote strict))))
     (for-each (lambda (f) (display (eval-islisp f e)) (newline))
       (quote (
         (reverse (quote (1 2 3)))
         (nreverse (list 1 2 3))
         (map (lambda (x) (* x 2)) (quote (1 2 3)))
         (mapc (lambda (x) (display x)) (quote (a b c)))
         (mapcan (lambda (x) (list x x)) (quote (1 2)))
         (maplist (lambda (x) (car x)) (quote (1 2 3)))
         (remove 2 (quote (1 2 3 2)))
         (member 2 (quote (1 2 3)))
         (assoc (quote b) (quote ((a 1)(b 2))))
         (list-ref (quote (a b c)) 1)
         (create-list 3 0)
         (string-length "hello")
         (string-ref "hello" 1)
         (char< (quote #\a) (quote #\b))
         (general-vector-ref (vector 10 20 30) 1)
         (array-dimensions (vector 1 2 3))
       ))))'
```

---

## Phase 3: 特殊形式の追加

特殊形式の追加は `eval-special` の `case` と `special-form?` の `memq` リストの両方を変更する。

### 3-A. `flet` と `labels`

**場所**: `eval-special` (`case` ブロック、行 2883 付近) と `special-form?` の memq リスト（行 2873）

#### `special-form?` への追加

```scheme
;; 行 2873 の memq リストに 'flet 'labels を追加
(define (special-form? sym)
  (memq sym '(... flet labels ...)))   ; 既存リストに追記
```

#### `eval-flet-like` の実装

```scheme
(define (eval-flet-like args env tail? recursive?)
  ;; args = (((name params body...) ...) body-form...)
  (unless (and (list? args) (>= (length args) 1) (list? (car args)))
    (error (if recursive? "labels" "flet") "needs bindings and body" args))
  (let* ((bindings (car args))
         (body     (cdr args))
         (fenv     (make-frame env)))
    ;; recursive? (labels) の場合は先にフレームを確定してからクロージャを作る
    (for-each
     (lambda (binding)
       (unless (and (list? binding) (>= (length binding) 2) (symbol? (car binding)))
         (error (if recursive? "labels" "flet") "invalid binding" binding))
       (let* ((fname  (resolve-binding-symbol (car binding)))
              (params (cadr binding))
              (fbody  (cddr binding))
              (scope  (if recursive? fenv env))   ; labels は fenv, flet は env
              (fn     (eval-lambda (cons params fbody) scope #f)))
         (frame-define! fenv fname fn)))
     bindings)
    (eval-sequence* body fenv tail?)))

;; eval-special の case に追加:
;;   ((flet)   (eval-flet-like args env tail? #f))
;;   ((labels) (eval-flet-like args env tail? #t))
```

**実装手順**:
1. `eval-special` の `case` に `((flet) ...)` と `((labels) ...)` の節を追加する。
2. `special-form?` の `memq` リストに `'flet` と `'labels` を追記する。

### 3-B. `the` — 型宣言

```scheme
;; eval-special の case に追加:
;;   ((the) (eval-the args env tail?))

(define (eval-the args env tail?)
  (unless (= (length args) 2)
    (error "the needs class-name and form" args))
  (let* ((class-name (car args))
         (form       (cadr args))
         (val        (eval-islisp* form env tail?)))
    (unless (isl-instancep val class-name env)
      (error "the: value is not of expected class" val class-name))
    val))
```

`isl-instancep` は既存の `(def 'instancep ...)` の実装を内部関数として切り出して再利用する。

### 3-C. `unwind-protect`

```scheme
;; eval-special の case に追加:
;;   ((unwind-protect) (eval-unwind-protect args env tail?))

(define (eval-unwind-protect args env tail?)
  (unless (>= (length args) 1)
    (error "unwind-protect needs at least a protected form" args))
  (let ((protected (car args))
        (cleanups  (cdr args)))
    (dynamic-wind
      (lambda () #f)
      (lambda () (eval-islisp* protected env tail?))
      (lambda ()
        (for-each (lambda (f) (eval-islisp* f env #f)) cleanups)))))
```

Gauche の `dynamic-wind` は非局所脱出（`throw`, `return-from`）でも cleanup を実行する。

### 3-D. `with-open-input-file` / `with-open-output-file`

マクロとして実装する。`make-initial-env` の `install-primitives!` 呼び出し後に追加する。

```scheme
(eval-islisp
 '(defmacro with-open-input-file (spec &rest body)
    (let ((var      (first spec))
          (filename (second spec)))
      `(with-open-file (,var ,filename :direction :input) ,@body)))
 env)
(eval-islisp
 '(defmacro with-open-output-file (spec &rest body)
    (let ((var      (first spec))
          (filename (second spec)))
      `(with-open-file (,var ,filename :direction :output) ,@body)))
 env)
```

### 3-E. `defconstant`

`defglobal` と同じだが、再代入時にエラーを出す。

**簡易実装**（定数チェックなし。エイリアスのみ）:

```scheme
;; eval-special の case に追加:
;;   ((defconstant) (eval-defglobal args env form))
;; special-form? のリストにも 'defconstant を追加
```

**本格実装**: 定数テーブル `*constant-symbols*` をグローバル変数として導入し、  
`frame-set!` の先頭でそのシンボルが定数なら `error` を投げるよう修正する。

```scheme
(define *constant-symbols* '())

(define (eval-defconstant args env form)
  (let* ((name (resolve-binding-symbol (car args)))
         (val  (eval-islisp* (cadr args) env #f)))
    (frame-define! (global-frame env) name val)
    (set! *constant-symbols* (cons name *constant-symbols*))
    name))

;; frame-set! の先頭に追加:
;; (when (memq sym *constant-symbols*)
;;   (error "cannot rebind constant" sym))
```

### 3-F. `function` 特殊形式

ISLISP では `(function name)` は特殊形式であり、関数オブジェクトを返す。  
`#'name` 構文は ISLISP には存在しないが、`(function name)` は必須。

```scheme
;; eval-special の case に追加:
;;   ((function) (eval-function-form args env))
;; special-form? のリストにも 'function を追加

(define (eval-function-form args env)
  (unless (= (length args) 1)
    (error "function needs exactly one argument" args))
  (let ((name (car args)))
    (cond
     ((symbol? name)
      ;; 変数束縛から関数オブジェクトを取得
      (let ((val (frame-ref env (resolve-symbol-in-package name))))
        (unless (or (closure? val) (primitive? val) (macro? val))
          (error "function: not a function" name))
        val))
     ((and (pair? name) (eq? (car name) 'lambda))
      ;; (function (lambda ...)) 形式
      (eval-islisp* name env #f))
     (else
      (error "function needs symbol or lambda form" name)))))
```

### 確認

```sh
gosh -I src -e '(use isl.core)' -e \
  '(let ((e (make-initial-env (quote strict))))
     (for-each (lambda (f) (display (eval-islisp f e)) (newline))
       (quote (
         (flet ((add1 (x) (+ x 1))) (add1 9))
         (labels ((even? (n) (if (= n 0) t (odd? (- n 1))))
                  (odd?  (n) (if (= n 0) nil (even? (- n 1)))))
                 (even? 4))
         (the <integer> 42)
         (unwind-protect (+ 1 2) (display "cleanup"))
         (function +)
       ))))'
```

---

## Phase 4: I/O ストリームの整備

### 4-A. 文字 I/O 関数

**場所**: `install-primitives!` 内、`(def 'read ...)` の近く（行 3463 付近）

EOF 時は Phase 5 で `<end-of-stream>` 条件を実装した後、  
`(error "end-of-stream ...")` を `(isl-signal-condition ...)` に差し替える。

```scheme
(def 'read-char
  (lambda args
    (let ((port (if (pair? args) (car args) (current-input-port))))
      (unless (input-port? port) (error "read-char needs an input stream" port))
      (let ((ch (read-char port)))
        (if (eof-object? ch)
            (error "end-of-stream in read-char")   ; Phase 5 で <end-of-stream> に変更
            ch)))))
(def 'write-char
  (lambda args
    (unless (pair? args) (error "write-char needs a character" args))
    (let ((ch   (car args))
          (port (if (pair? (cdr args)) (cadr args) (current-output-port))))
      (unless (char? ch) (error "write-char first arg must be character" ch))
      (write-char ch port)
      ch)))
(def 'peek-char
  (lambda args
    (let ((port (if (pair? args) (car args) (current-input-port))))
      (unless (input-port? port) (error "peek-char needs an input stream" port))
      (let ((ch (peek-char port)))
        (if (eof-object? ch)
            '()        ; Phase 5 で <end-of-stream> 条件に変更
            ch)))))
```

### 4-B. 出力関数の追加・修正

```scheme
(def 'write               ; read で読み戻せる表現で出力（エスケープあり）
  (lambda args
    (unless (pair? args) (error "write needs an object" args))
    (let ((obj  (car args))
          (port (if (pair? (cdr args)) (cadr args) (current-output-port))))
      (write obj port)
      obj)))
(def 'terpri
  (lambda args
    (let ((port (if (pair? args) (car args) (current-output-port))))
      (newline port)
      '())))
```

**`print` のストリーム引数対応**: 既存の `print`（行 3444）はストリーム引数を受け取らない。  
ISLISP では `(print obj stream?)` という形式が必要なため、以下に置き換える:

```scheme
;; 既存の (def 'print ...) を以下に置き換える:
(def 'print
  (lambda args
    (unless (pair? args) (error "print needs an object" args))
    (let ((obj  (car args))
          (port (if (pair? (cdr args)) (cadr args) (current-output-port))))
      (write obj port)
      (newline port)
      obj)))
```

### 4-C. ストリーム生成・終了関数

```scheme
(def 'open-input-stream
  (lambda (filename)
    (ensure-string filename "open-input-stream")
    (open-input-file filename)))
(def 'open-output-stream       ; 常に :supersede（ISLISP 標準仕様）
  (lambda (filename)
    (ensure-string filename "open-output-stream")
    (open-output-file filename :if-exists :supersede
                               :if-does-not-exist :create)))
(def 'open-io-stream           ; 入出力兼用ストリーム
  (lambda (filename)
    (ensure-string filename "open-io-stream")
    ;; Gauche には open-file がある（入出力兼用）
    (open-file filename (logior O_RDWR O_CREAT) #o666)))
(def 'close
  (lambda (stream)
    (cond
     ((input-port? stream)  (close-input-port stream))
     ((output-port? stream) (close-output-port stream))
     (else (error "close needs a stream" stream)))
    '()))
```

**注意 (`open-io-stream`)**: Gauche の `open-file` は低レベル API であり、  
`O_RDWR` 等は `(use gauche.fcntl)` または `(use gauche.syscall)` が必要。  
代替として `call-with-port` や Gauche の `open-input-output-file`（あれば）を使う。  
ISLISP 標準に必要な機能として登録するが、実際の動作は環境依存であることを注記する。

### 4-D. ストリーム型述語

```scheme
(def 'input-stream-p  input-port?)
(def 'output-stream-p output-port?)
```

### 4-E. 標準ストリーム変数

`make-initial-env` の末尾（`install-primitives!` の後）に追加する:

```scheme
(frame-define! env (resolve-binding-symbol '*standard-input*)
               (current-input-port))
(package-export! *current-package* '*standard-input*)
(frame-define! env (resolve-binding-symbol '*standard-output*)
               (current-output-port))
(package-export! *current-package* '*standard-output*)
(frame-define! env (resolve-binding-symbol '*error-output*)
               (current-error-port))
(package-export! *current-package* '*error-output*)
```

### 4-F. `read` / `read-line` のストリーム引数確認と修正

既存の `read`（行 3463）と `read-line`（行 3472）はすでにストリーム引数を受け取る実装になっているが、  
引数なしの場合に `(current-input-port)` を使うよう統一されているか確認する。

```scheme
;; 現状の read は (read) と (read stream) に対応済み。
;; read-line は (read-line stream) と (read-line stream eof-error-p) 形式。
;; ISLISP では (read-line &optional stream) なので引数なし版も必要。
;; 既存実装を以下に置き換える:
(def 'read-line
  (lambda args
    (let* ((port (if (pair? args) (car args) (current-input-port)))
           (line (read-line port)))
      (if (eof-object? line)
          (error "end-of-stream in read-line")   ; Phase 5 で <end-of-stream> に変更
          line))))
```

### 確認

```sh
gosh -I src -e '(use isl.core)' -e \
  '(let ((e (make-initial-env (quote strict))))
     (eval-islisp
       (quote (with-open-output-file (s "/tmp/isl-test.txt")
                (write-char (quote #\H) s)
                (write "world" s)
                (terpri s)))
       e)
     (eval-islisp
       (quote (with-open-input-file (s "/tmp/isl-test.txt")
                (display (read-char s))
                (newline)))
       e))'
```

---

## Phase 5: 条件システムの再設計（最難関）

Phase 1〜4 の完了後に着手する。これは既存の `handler-case` と Gauche 例外の橋渡しを変更する大規模な修正。

### 5-A. ISLISP 条件クラス階層の定義

`core.scm` のトップレベル（`install-primitives!` の前）に isl 条件オブジェクトの Scheme 表現を定義する:

```scheme
(define (make-isl-condition class-sym message continuable? irritants)
  (vector 'isl-condition class-sym message continuable? irritants))
(define (isl-condition? x)
  (and (vector? x) (= (vector-length x) 5) (eq? (vector-ref x 0) 'isl-condition)))
(define (isl-condition-class x)        (vector-ref x 1))
(define (isl-condition-message x)      (vector-ref x 2))
(define (isl-condition-continuable? x) (vector-ref x 3))
(define (isl-condition-irritants x)    (vector-ref x 4))
```

`make-initial-env` 内の `install-primitives!` 後に条件クラスを `defclass` で登録する:

```scheme
(for-each (lambda (form) (eval-islisp form env))
  '((defclass <condition>              ()                    ())
    (defclass <serious-condition>      (<condition>)         ())
    (defclass <error>                  (<serious-condition>) ())
    (defclass <arithmetic-error>       (<error>)             ())
    (defclass <division-by-zero>       (<arithmetic-error>)  ())
    (defclass <floating-point-overflow>  (<arithmetic-error>) ())
    (defclass <floating-point-underflow> (<arithmetic-error>) ())
    (defclass <control-error>          (<error>)             ())
    (defclass <parse-error>            (<error>)             ())
    (defclass <program-error>          (<error>)             ())
    (defclass <domain-error>           (<program-error>)     ())
    (defclass <undefined-entity>       (<program-error>)     ())
    (defclass <unbound-variable>       (<undefined-entity>)  ())
    (defclass <undefined-function>     (<undefined-entity>)  ())
    (defclass <arity-error>            (<program-error>)     ())
    (defclass <index-out-of-range>     (<program-error>)     ())
    (defclass <type-error>             (<program-error>)     ())
    (defclass <simple-error>           (<error>)             ())
    (defclass <stream-error>           (<error>)             ())
    (defclass <end-of-stream>          (<stream-error>)      ())))
```

Phase 4 で暫定的に `(error "end-of-stream ...")` としていた箇所を以下に差し替える:

```scheme
;; read-char / peek-char / read-line の EOF 処理:
(isl-signal-condition
 (make-isl-condition '<end-of-stream> "end of stream" #f '()))
```

### 5-B. `signal-condition`・`error`・`condition-message` の実装

```scheme
;; ハンドラスタック（Gauche のパラメータオブジェクトで動的変数として実装）
(define *isl-handler-stack* (make-parameter '()))

(define (isl-signal-condition cond-obj)
  (let loop ((stack ((*isl-handler-stack*))))
    (if (null? stack)
        (raise cond-obj)   ; 未捕捉 → Gauche 例外として伝播
        (let ((handler (car stack)))
          (parameterize ((*isl-handler-stack* (cdr stack)))
            (handler cond-obj))))))   ; ハンドラが戻れば継続が再開される

;; install-primitives! に追加:
(def 'signal-condition
  (lambda (cond-obj continuable)
    (unless (isl-condition? cond-obj)
      (error "signal-condition needs an isl condition object" cond-obj))
    (isl-signal-condition cond-obj)))
(def 'condition-continuable
  (lambda (cond-obj)
    (unless (isl-condition? cond-obj)
      (error "condition-continuable needs an isl condition object" cond-obj))
    (isl-condition-continuable? cond-obj)))
(def 'condition-message
  (lambda (cond-obj)
    (cond
     ((isl-condition? cond-obj)
      (isl-condition-message cond-obj))
     ((condition? cond-obj)
      (condition/report-string cond-obj))   ; Gauche native 条件
     (else
      (write-to-string cond-obj)))))
;; error 関数（<simple-error> を通知）
(def 'error
  (lambda (msg . irritants)
    (unless (string? msg) (error "error first arg must be string" msg))
    (isl-signal-condition
     (make-isl-condition '<simple-error> msg #f irritants))))
```

### 5-C. `continue-condition` の実装

`continue-condition` は継続可能条件（`continuable = #t`）に対して継続を再開する。  
`with-handler` のハンドラが正常リターンしたとき、その値が `signal-condition` の戻り値となる仕組みを利用する。

```scheme
;; install-primitives! に追加:
(def 'continue-condition
  (lambda args
    (unless (and (pair? args) (isl-condition? (car args)))
      (error "continue-condition needs an isl condition object" args))
    (let ((cond-obj (car args))
          (value    (if (pair? (cdr args)) (cadr args) '())))
      (unless (isl-condition-continuable? cond-obj)
        (error "condition is not continuable" cond-obj))
      ;; 継続は with-handler の呼び出し元に戻ることで実現される。
      ;; このオブジェクトを throw するのではなく、ハンドラから value を返す。
      ;; そのため continue-condition は「ハンドラの中」から呼ぶことを前提とする。
      value)))
```

**実装上の注意**: ISLISP の `continue-condition` は `signal-condition` の呼び出し元まで値を返す  
継続渡しスタイルである。`*isl-handler-stack*` ベースの実装では、ハンドラが通常の値を返せば  
`isl-signal-condition` の内部ループが `handler` の戻り値をそのまま返すため、  
`signal-condition` → `isl-signal-condition` の呼び出し元に値が伝わる。  
`continue-condition` は上記の値を返す簡略実装で正しく機能する。

### 5-D. `cerror` の実装

```scheme
;; install-primitives! に追加:
(def 'cerror
  (lambda (continue-string error-string . irritants)
    (unless (string? continue-string)
      (error "cerror continue-string must be string" continue-string))
    (unless (string? error-string)
      (error "cerror error-string must be string" error-string))
    (isl-signal-condition
     (make-isl-condition '<simple-error> error-string #t
                         (cons continue-string irritants)))))
```

### 5-E. `with-handler` の実装

`eval-special` への追加。`special-form?` のリストにも `'with-handler` を追記する。

```scheme
;; case の節:
;;   ((with-handler) (eval-with-handler args env tail?))

(define (eval-with-handler args env tail?)
  (unless (= (length args) 2)
    (error "with-handler needs handler-form and protected-form" args))
  (let* ((handler-form   (car args))
         (protected-form (cadr args))
         (handler-fn     (eval-islisp* handler-form env #f)))
    (parameterize ((*isl-handler-stack*
                    (cons (lambda (c) (apply-islisp handler-fn (list c)))
                          ((*isl-handler-stack*)))))
      (eval-islisp* protected-form env tail?))))
```

### 5-F. `handler-case` の修正

条件オブジェクトが `isl-condition` になった後は型チェックを修正する:

```scheme
;; handler-case-tag-match? を拡張して isl-condition の class 階層も判定する
(define (handler-case-tag-match? tag cond-obj)
  (or (eq? tag 'error)
      (eq? tag 'condition)
      (eq? tag 'serious-condition)
      (eq? tag 't)
      (eq? tag 'otherwise)
      (and (isl-condition? cond-obj)
           (isl-subclassp? (isl-condition-class cond-obj) tag))))
```

`isl-subclassp?` は `*class-table*` を参照して祖先クラスを辿る既存ロジックを関数化して再利用する。

### 5-G. `ignore-errors`

マクロとして実装する。`make-initial-env` 内で登録する。

```scheme
(eval-islisp
 '(defmacro ignore-errors (&rest forms)
    `(handler-case (progn ,@forms)
       (<serious-condition> () nil)))
 env)
```

### 確認

```sh
gosh -I src -e '(use isl.core)' -e \
  '(let ((e (make-initial-env (quote strict))))
     (for-each (lambda (f) (display (eval-islisp f e)) (newline))
       (quote (
         (with-handler
           (lambda (c) (string-append "caught: " (condition-message c)))
           (error "test error"))
         (ignore-errors (error "ignored"))
         (handler-case (error "hello") (<error> (c) (condition-message c)))
         (cerror "continue" "recoverable error")
       ))))'
```

---

## Phase 6: 意味論的修正（strict モードの整合）

**注意**: 既存の動作に影響する変更。extended モードのコードが壊れる可能性があるため、  
strict モード限定の変更として実装し、extended は現状維持を推奨する。

### 6-A. `nil` / `#f` の整合と `t` / `#t` の統一

**問題点**:
1. `truthy?` が `#f` と `'()` を両方偽と判定している（行 62）。ISLISP では `nil`（= `'()`）のみが偽。
2. ISLISP では `#t` リテラルは存在しない。真値は `t` のみ。

**対応方針**: リーダーレベルで変換パスを導入する。  
`run-file` および `repl` のフォーム評価前に `sanitize-for-strict` を適用する。

```scheme
(define (sanitize-for-strict form)
  (cond
   ((eq? form #f) '())     ; #f → nil（= '()）
   ((eq? form #t) #t)      ; #t はそのまま（t = #t として統一）
   ((pair? form)
    (cons (sanitize-for-strict (car form))
          (sanitize-for-strict (cdr form))))
   ((vector? form)
    (vector-map sanitize-for-strict form))
   (else form)))
```

`main.scm` の `eval-forms` 内で strict モード時に適用する:

```scheme
(define (eval-forms forms env)
  (for-each
   (lambda (form)
     (eval-islisp (if (strict-profile?)
                      (sanitize-for-strict form)
                      form)
                  env))
   forms))
```

また、strict モードでは `#t` リテラルを `t` と等価に扱うため、  
`eval-islisp*` の自己評価節を以下のように修正する:

```scheme
;; eval-islisp* の自己評価節（行 2965 付近）:
;;   ((or (number? form) (string? form) (char? form) (boolean? form)) form)
;; → strict モードでは boolean #t を t（= #t）として、#f を '() として扱う
;;   sanitize-for-strict で前処理済みなら追加変更不要。
```

### 6-B. `defvar` の動的スコープ・`dynamic` / `dynamic-let` の実装

現在の `defvar` は `defglobal` と同一の挙動。  
ISLISP の `defvar` は **動的変数** を宣言し、`(dynamic var)` でアクセスする。

**実装方針**: 動的変数テーブルを導入する。

```scheme
;; グローバルな動的変数テーブル（Gauche パラメータオブジェクト）
(define *dynamic-var-table* (make-parameter (make-hash-table)))

;; eval-defvar の修正（既存の実装を置き換え）:
(define (eval-defvar args env form)
  (let* ((name (resolve-binding-symbol (car args)))
         (val  (eval-islisp* (cadr args) env #f)))
    (frame-define! (global-frame env) name val)
    (hash-table-put! (*dynamic-var-table*) name val)
    name))

;; special-form? に 'dynamic 'dynamic-let を追加
;; eval-special に以下を追加:
;;   ((dynamic)     (eval-dynamic args env))
;;   ((dynamic-let) (eval-dynamic-let args env tail?))

(define (eval-dynamic args env)
  (unless (= (length args) 1) (error "dynamic needs variable name" args))
  (let ((name (resolve-binding-symbol (car args))))
    (or (hash-table-ref/default (*dynamic-var-table*) name #f)
        (error "dynamic variable unbound" name))))

(define (eval-dynamic-let args env tail?)
  (unless (and (list? args) (>= (length args) 1) (list? (car args)))
    (error "dynamic-let needs bindings and body" args))
  (let* ((bindings (car args))
         (body     (cdr args))
         (saved    (map (lambda (b)
                          (let ((name (resolve-binding-symbol (car b))))
                            (cons name
                                  (hash-table-ref/default (*dynamic-var-table*) name #f))))
                        bindings)))
    (for-each
     (lambda (b)
       (let ((name (resolve-binding-symbol (car b)))
             (val  (eval-islisp* (cadr b) env #f)))
         (hash-table-put! (*dynamic-var-table*) name val)))
     bindings)
    (dynamic-wind
      (lambda () #f)
      (lambda () (eval-sequence* body env tail?))
      (lambda ()
        (for-each
         (lambda (pair)
           (if (cdr pair)
               (hash-table-put! (*dynamic-var-table*) (car pair) (cdr pair))
               (hash-table-delete! (*dynamic-var-table*) (car pair))))
         saved)))))
```

### 6-C. `vector-ref` / `vector-set!` の名称整合（strict モード）

ISLISP 標準の名称は `general-vector-ref` / `(setf general-vector-ref)`。  
`vector-ref` は Scheme 名称。Phase 2-D で `general-vector-ref` を追加済みのため、  
strict モードでは `vector-ref` を無効化するオプションを `disable-extended-primitives!` に追加する。

```scheme
;; *extended-primitive-symbols* のリストに追加:
;;   'vector-ref 'vector-set! 'vectorp 'make-vector
;; （strict では general-vector-ref / create-vector を使う）
;; ただしこれは既存コードへの破壊的変更になるため、
;; strict モードで警告を出すにとどめる選択肢もある。
```

---

## Phase 7: OOP の完全化

Phase 5 の条件クラス階層が揃ってから着手すると、エラー通知が標準準拠になる。

### 7-A. `call-next-method` と `next-method-p`

`defgeneric` / `defmethod` のディスパッチ実装（`eval-defgeneric` 付近）を確認し、  
applicable メソッドリストを構築後に `*next-methods*` に束縛してメソッド本体を呼ぶ。

```scheme
(define *next-methods* (make-parameter '()))

;; install-primitives! に追加:
(def 'call-next-method
  (lambda args
    (let ((next ((*next-methods*))))
      (if (null? next)
          (error "call-next-method: no next method available")
          (let ((method (car next)))
            (parameterize ((*next-methods* (cdr next)))
              (apply-islisp method args)))))))
(def 'next-method-p
  (lambda ()
    (not (null? ((*next-methods*))))))
```

`apply-generic-function`（または相当するディスパッチ関数）の実装を修正し、  
メソッド適用時に `parameterize` で `*next-methods*` を残りのメソッドリストに設定する:

```scheme
;; メソッド適用の擬似コード:
;; (parameterize ((*next-methods* (cdr applicable-methods)))
;;   (apply-islisp (car applicable-methods) args))
```

### 7-B. `:before` / `:after` / `:around` 補助メソッド

`eval-defmethod` を修正して `:qualifier` キーワードを認識させる。  
メソッドオブジェクトにqualifier フィールドを追加し、ディスパッチ時に分類して呼ぶ。

**呼び出し順**: `around` → `before` → primary → `after`（逆順）

```scheme
;; defmethod の構文:
;;   (defmethod name qualifier? (specializers) body...)
;;   qualifier = :before | :after | :around | なし（primary）

;; eval-defmethod の修正点:
;; 1. method-name の次が keyword かどうかチェック
;; 2. qualifier をメソッドオブジェクトに保存
;; 3. apply-generic-function を以下の順で呼び出すよう変更:
;;    a. around メソッドがあれば最初に適用（call-next-method で before/primary/after に連鎖）
;;    b. before メソッドを順番に適用
;;    c. primary メソッドを適用（戻り値を使う）
;;    d. after メソッドを逆順に適用
;;    e. primary の戻り値を返す
```

詳細実装は独立した作業として進める。`docs/compiler-compliance-spec.md` §7 も参照。

### 7-C. `subclassp` と `class` 特殊形式

```scheme
;; *class-table* を参照してサブクラス判定する関数を切り出す
(define (isl-subclassp? sub-name super-name)
  ;; *class-table* のエントリを辿って super-name が祖先に含まれるか判定
  ;; 既存の instancep の実装で使っているロジックを再利用する
  ...)

;; install-primitives! に追加:
(def 'subclassp
  (lambda (sub super)
    (isl-subclassp? sub super)))
```

`(class class-name)` は特殊形式として実装する（評価せずにクラスオブジェクトを返す）:

```scheme
;; eval-special の case に追加:
;;   ((class) (eval-class-form args env))
;; special-form? のリストにも 'class を追加

(define (eval-class-form args env)
  (unless (= (length args) 1)
    (error "class needs exactly one class-name" args))
  (let ((name (car args)))
    (unless (symbol? name) (error "class: class-name must be symbol" name))
    ;; クラスオブジェクト（*class-table* のエントリ）を返す
    (let ((entry (assoc name *class-table*)))
      (unless entry (error "class: undefined class" name))
      entry)))
```

### 7-D. `standard-object` と `built-in-class`

ISLISP ではユーザ定義クラスのデフォルトスーパークラスは `<standard-object>` である。  
また組み込み型は `<built-in-class>` に属する。

```scheme
;; make-initial-env 内で条件クラス定義の前に追加:
(for-each (lambda (form) (eval-islisp form env))
  '(;; ユーザ定義クラスの共通祖先
    (defclass <standard-object> () ())
    ;; 組み込み型の共通親クラス（実際には処理系が内部管理）
    (defclass <built-in-class>  () ())))

;; defclass の実装（eval-defclass）を修正して:
;; スーパークラスリストが空のとき <standard-object> をデフォルトとして追加する。
;; (defclass Foo () (...)) → 実際は (defclass Foo (<standard-object>) (...)) として処理
```

### 7-E. `initialize-object`

```scheme
;; install-primitives! に追加:
(def 'initialize-object
  (lambda (obj initargs)
    obj))   ; デフォルト実装：何もしない（メソッドでオーバーライド可能）

;; make-instance の実装（eval-defclass 付近）を修正して
;; make-instance 後に initialize-object を呼ぶ:
;; (let ((obj (make-raw-instance class initargs)))
;;   (apply-islisp (frame-ref env 'initialize-object) (list obj initargs))
;;   obj)
```

### 7-F. `slot-boundp` と `slot-makunbound`

```scheme
;; install-primitives! に追加:
(def 'slot-boundp
  (lambda (obj slot-name)
    (unless (isl-instance? obj) (error "slot-boundp needs an instance" obj))
    (unless (symbol? slot-name) (error "slot-boundp slot-name must be symbol" slot-name))
    ;; インスタンスのスロットテーブルで unbound マーカーを使って判定
    ;; 現在の isl では slot-value がエラーを出す = 未束縛
    (guard (e (else #f))
      (slot-value obj slot-name)
      #t)))
(def 'slot-makunbound
  (lambda (obj slot-name)
    (unless (isl-instance? obj) (error "slot-makunbound needs an instance" obj))
    (unless (symbol? slot-name) (error "slot-makunbound slot-name must be symbol" slot-name))
    ;; スロットを未束縛マーカーに設定する
    ;; 現在の isl のインスタンス表現を確認して unbound マーカーを導入する
    ;; 暫定：エラーを出す（将来実装）
    (error "slot-makunbound: not yet implemented" slot-name)))
```

**注意**: `slot-boundp` と `slot-makunbound` を完全に実装するには、  
インスタンスのスロットテーブルに「未束縛」を表すセンチネル値を導入する必要がある。  
現在の isl のインスタンス表現（`eval-defclass` 付近）を確認してから実装する。

---

## Phase 8: `format` の標準化

### 8-A. `format` のストリーム引数対応

現在の `format` 実装（行 4412 付近）を確認し、  
第1引数が `t`（標準出力）/ `nil`（文字列生成）/ ストリームオブジェクトの場合を正しく処理する。

```scheme
;; 現在の format 実装を以下の方針で修正:
;;   (format t "~a" x)       → 標準出力へ（現状の動作確認）
;;   (format nil "~a" x)     → 文字列を返す（capture-output-string 相当）
;;   (format stream "~a" x)  → stream へ出力

;; 修正例（render-format は既存の文字列生成関数）:
(def 'format
  (lambda args
    (unless (>= (length args) 2)
      (error "format needs stream/t/nil and format-string" args))
    (let ((dest   (car args))
          (fmt    (cadr args))
          (fmtargs (cddr args)))
      (unless (string? fmt) (error "format second arg must be string" fmt))
      (let ((rendered (render-format fmt fmtargs)))
        (cond
         ((eq? dest #t)
          (display rendered)
          '())
         ((or (null? dest) (eq? dest #f))
          rendered)                     ; 文字列として返す
         ((output-port? dest)
          (display rendered dest)
          '())
         (else
          (error "format first arg must be t, nil or output stream" dest)))))))
```

---

## 実装チェックリスト

各 Phase の完了基準として以下を確認する。

### Phase 1 完了基準

```sh
gosh -I src -e '(use isl.core)' -e \
  '(let ((e (make-initial-env (quote strict))))
     (for-each (lambda (f) (display (eval-islisp f e)) (display " ") )
       (quote ((abs -3) (max 1 2 3) (min 1 2 3) (expt 2 10) (sqrt 4.0)
               (rem 7 3) (rem -7 3) (quotient 7 3) (quotient -7 3)
               (floor 7 2) (ceiling 7 2) (truncate -7 2) (round 7 2)
               (number->string 255 16) (string->number "ff" 16)
               (consp (cons 1 2)) (integerp 3) (floatp 1.5)
               (general-vector-p (vector 1 2))))) (newline))'
```

### Phase 2 完了基準

```sh
gosh -I src -e '(use isl.core)' -e \
  '(let ((e (make-initial-env (quote strict))))
     (for-each (lambda (f) (display (eval-islisp f e)) (display " "))
       (quote ((reverse (quote (1 2 3)))
               (nreverse (list 1 2 3))
               (map (lambda (x) (* x 2)) (quote (1 2 3)))
               (mapc (lambda (x) x) (quote (1 2 3)))
               (mapcan (lambda (x) (list x x)) (quote (1 2)))
               (remove 2 (quote (1 2 3 2)))
               (member 2 (quote (1 2 3)))
               (assoc (quote b) (quote ((a 1)(b 2))))
               (create-list 3 0)
               (string-length "hello") (string-ref "hello" 1)
               (general-vector-ref (vector 10 20) 1)
               (array-dimensions (vector 1 2 3))))) (newline))'
```

### Phase 3 完了基準

```sh
gosh -I src -e '(use isl.core)' -e \
  '(let ((e (make-initial-env (quote strict))))
     (for-each (lambda (f) (display (eval-islisp f e)) (display " "))
       (quote (
         (flet ((f (x) (* x x))) (f 5))
         (labels ((fact (n) (if (= n 0) 1 (* n (fact (- n 1)))))) (fact 5))
         (the <integer> 42)
         (unwind-protect (+ 1 2) nil)
         (function +)
       ))) (newline))'
```

### Phase 4 完了基準

```sh
gosh -I src -e '(use isl.core)' -e \
  '(let ((e (make-initial-env (quote strict))))
     (eval-islisp
       (quote (with-open-output-file (s "/tmp/isl-phase4.txt")
                (write "hello" s) (terpri s)))
       e)
     (eval-islisp
       (quote (with-open-input-file (s "/tmp/isl-phase4.txt")
                (display (read-char s)) (newline)))
       e))'
```

### Phase 5 完了基準

```sh
gosh -I src -e '(use isl.core)' -e \
  '(let ((e (make-initial-env (quote strict))))
     (for-each (lambda (f) (display (eval-islisp f e)) (newline))
       (quote (
         (handler-case (error "test") (<error> (c) (condition-message c)))
         (ignore-errors (error "ignored"))
         (with-handler (lambda (c) (condition-message c)) (error "ok"))
         (condition-continuable
           (handler-case (cerror "cont" "err") (<simple-error> (c) c)))
       ))))'
```

### Phase 6 完了基準

```sh
gosh -I src -e '(use isl.core)' -e \
  '(let ((e (make-initial-env (quote strict))))
     (eval-islisp
       (quote (progn
         (defvar *x* 10)
         (dynamic-let ((*x* 20))
           (display (dynamic *x*)) (newline))
         (display (dynamic *x*)) (newline)))
       e))'
```

### Phase 7 完了基準

```sh
gosh -I src -e '(use isl.core)' -e \
  '(let ((e (make-initial-env (quote strict))))
     (for-each (lambda (f) (display (eval-islisp f e)) (newline))
       (quote (
         (progn
           (defclass <animal> () ((name :initarg :name :reader animal-name)))
           (defclass <dog> (<animal>) ())
           (defgeneric speak (a))
           (defmethod speak ((a <animal>)) "...")
           (defmethod speak ((d <dog>)) (string-append "Woof: " (call-next-method)))
           (speak (make-instance (quote <dog>) :name "Rex")))
         (subclassp (quote <dog>) (quote <animal>))
       ))))'
```

---

## 新規テストケースの追加方針

`test/conformance/milestones.scm` に ISO 標準章別のテストセクションを追加する。  
既存の M0〜M11 は維持し、新しい `std-*` カテゴリとして追記する。

```scheme
(define std-arith-cases
  (list
   (list 'value '(abs -5)             5)
   (list 'value '(abs 5)              5)
   (list 'value '(max 1 2 3)          3)
   (list 'value '(min 1 2 3)          1)
   (list 'value '(expt 2 10)          1024)
   (list 'value '(rem 7 3)            1)
   (list 'value '(rem -7 3)          -1)
   (list 'value '(quotient 7 3)       2)
   (list 'value '(quotient -7 3)     -2)
   (list 'value '(floor 7 2)          3)
   (list 'value '(ceiling 7 2)        4)
   (list 'value '(number->string 255 16) "ff")
   (list 'value '(string->number "ff" 16) 255)
   (list 'value '(string->number "zz" 16) nil)
   (list 'error '(abs "a")            'error)))

(define std-list-cases
  (list
   (list 'value '(reverse '(1 2 3))          '(3 2 1))
   (list 'value '(map (lambda (x) (* x 2)) '(1 2 3)) '(2 4 6))
   (list 'value '(member 2 '(1 2 3))         '(2 3))
   (list 'value '(member 9 '(1 2 3))         nil)
   (list 'value '(assoc 'b '((a 1)(b 2)))    '(b 2))
   (list 'value '(list-ref '(a b c) 1)       'b)
   (list 'value '(create-list 3 0)           '(0 0 0))
   (list 'value '(remove 2 '(1 2 3 2))       '(1 3))
   (list 'value '(mapcan (lambda (x) (list x x)) '(1 2)) '(1 1 2 2))
   (list 'error '(list-ref '(a b) 5)         'error)))

(define std-string-cases
  (list
   (list 'value '(string-length "hello")    5)
   (list 'value '(string-ref "hello" 1)     #\e)
   (list 'value '(string-upcase "hello")    "HELLO")
   (list 'value '(string-downcase "HELLO")  "hello")
   (list 'error '(string-ref "hi" 5)        'error)))

(define std-char-cases
  (list
   (list 'value '(char= #\a #\a)    #t)
   (list 'value '(char< #\a #\b)    #t)
   (list 'value '(char-upcase #\a)  #\A)
   (list 'value '(char-downcase #\A) #\a)
   (list 'error '(char= #\a "a")    'error)))

(define std-flet-cases
  (list
   (list 'value '(flet ((f (x) (+ x 1))) (f 9))  10)
   (list 'value
         '(labels ((even? (n) (if (= n 0) t (odd? (- n 1))))
                   (odd?  (n) (if (= n 0) nil (even? (- n 1)))))
                  (even? 4))
         't)
   (list 'value '(unwind-protect 42 nil)          42)
   (list 'value '(the <integer> 99)               99)
   (list 'error '(the <string> 42)                'error)))

(define std-condition-cases
  (list
   (list 'value
         '(handler-case (error "hello") (<error> (c) (condition-message c)))
         "hello")
   (list 'value
         '(ignore-errors (error "ignored"))
         nil)
   (list 'value
         '(with-handler (lambda (c) (condition-message c)) (error "test"))
         "test")))

(define std-dynamic-cases
  (list
   (list 'value
         '(progn (defvar *d* 1)
                 (dynamic-let ((*d* 2)) (dynamic *d*)))
         2)
   (list 'value
         '(progn (defvar *d2* 10) (dynamic *d2*))
         10)))

;; all-milestones への追加:
;; (list "std-arith"     std-arith-cases)
;; (list "std-list"      std-list-cases)
;; (list "std-string"    std-string-cases)
;; (list "std-char"      std-char-cases)
;; (list "std-flet"      std-flet-cases)
;; (list "std-condition" std-condition-cases)
;; (list "std-dynamic"   std-dynamic-cases)
```

---

## ギャップ文書との対応表

`docs/islisp-conformance-gaps.md` の全項目と本手順書の対応:

| ギャップ項目 | 手順書の対応箇所 |
|-------------|----------------|
| **特殊形式** | |
| `flet` | Phase 3-A |
| `labels` | Phase 3-A |
| `the` | Phase 3-B |
| `with-handler` | Phase 5-E |
| `ignore-errors` | Phase 5-G |
| `unwind-protect` | Phase 3-C |
| `with-open-input-file` / `with-open-output-file` | Phase 3-D |
| `dynamic` / `dynamic-let` | Phase 6-B |
| `defconstant` | Phase 3-E |
| `function`（特殊形式）| Phase 3-F |
| **算術関数** | |
| `abs`, `max`, `min`, `expt`, `sqrt`, `rem`, `gcd`, `lcm`, `signum`, `negate`, `float`, `isqrt` | Phase 1-A |
| `number->string`, `string->number` | Phase 1-B |
| `exp`, `log`, `sin`, `cos`, `tan`, `asin`, `acos`, `atan` | Phase 1-D |
| `quotient` | Phase 1-E |
| `floor`/`ceiling`/`truncate`/`round` 2引数版 | Phase 1-E |
| **型述語** | |
| `consp`, `characterp`, `integerp`, `floatp`, `functionp` | Phase 1-C |
| `input-stream-p`, `output-stream-p` | Phase 4-D |
| `general-vector-p`, `general-array*-p` | Phase 1-C |
| **リスト操作** | |
| `reverse` | Phase 2-A |
| `nreverse` | Phase 2-A |
| `map`, `for-each` | Phase 2-A |
| `mapc`, `mapcan`, `maplist` | Phase 2-A |
| `member`, `assoc` | Phase 2-A |
| `list-ref`, `list-tail`, `last-pair` | Phase 2-A |
| `create-list`, `list-copy`, `nthcdr`, `sort` | Phase 2-A |
| `nconc` | Phase 2-A |
| `remove` | Phase 2-A |
| **文字列操作** | |
| `string-length`, `string-ref`, `string-copy`, `string-upcase`, `string-downcase` | Phase 2-B |
| **文字操作** | |
| `char=`, `char/=`, `char<`, `char>`, `char<=`, `char>=`, `char-upcase`, `char-downcase` | Phase 2-C |
| **ベクター標準名** | |
| `create-vector`, `general-vector-ref` | Phase 2-D |
| `(setf general-vector-ref)` | Phase 2-D |
| `general-vector-p`（型述語） | Phase 1-C |
| `vector-ref`/`vector-set!` 名称整合（§3-5）| Phase 6-C |
| **配列（多次元）** | |
| `create-array`, `array-dimensions` | Phase 2-E |
| `(setf aref)` | Phase 2-E |
| **I/O・ストリーム** | |
| `read-char`, `write-char`, `peek-char` | Phase 4-A |
| `write`, `terpri` | Phase 4-B |
| `print` ストリーム引数修正 | Phase 4-B |
| `open-input-stream`, `open-output-stream`, `close` | Phase 4-C |
| `open-io-stream` | Phase 4-C |
| `input-stream-p`, `output-stream-p` | Phase 4-D |
| `*standard-input*`, `*standard-output*`, `*error-output*` | Phase 4-E |
| `read` / `read-line` ストリーム引数修正 | Phase 4-F |
| EOF → `<end-of-stream>` 条件 | Phase 5-A（差し替え手順を記載）|
| **条件システム** | |
| 条件クラス階層 | Phase 5-A |
| `signal-condition`, `condition-continuable`, `condition-message`, `error` | Phase 5-B |
| `continue-condition` | Phase 5-C |
| `cerror` | Phase 5-D |
| `handler-case` の修正 | Phase 5-F |
| **OOP** | |
| `call-next-method`, `next-method-p` | Phase 7-A |
| `:before`/`:after`/`:around` | Phase 7-B |
| `subclassp`, `class`（特殊形式）| Phase 7-C |
| `standard-object`, `built-in-class` | Phase 7-D |
| `initialize-object` | Phase 7-E |
| `slot-boundp`, `slot-makunbound` | Phase 7-F |
| **意味論的相違** | |
| `nil`/`#f` の二重性 | Phase 6-A |
| `t`/`#t` の統一 | Phase 6-A |
| `handler-case` → `with-handler` 意味論修正 | Phase 5-E / 5-F |
| `format` ストリーム引数 | Phase 8-A |
| `defvar` 動的スコープ | Phase 6-B |

---

## 参照

- `docs/islisp-conformance-gaps.md` — ギャップ一覧（本文書の元データ）
- `src/isl/core.scm` — 実装対象（`install-primitives!`: 行 3002〜, `eval-special`: 行 2878〜）
- `test/conformance/milestones.scm` — 既存テストケース
- ISO/IEC 13816:2007 — 各 Phase の §参照
