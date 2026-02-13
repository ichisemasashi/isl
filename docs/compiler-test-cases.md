# ISLISP コンパイラ: マイルストーン別テストケース一覧（入力フォームと期待値）

この文書は `docs/compiler-milestones.md` の各マイルストーンに対応する具体テストケース一覧。
原則として `strict` プロファイル向け。期待値は interpreter をオラクルにした同値判定を前提とする。

表記:

- `Form`: `eval-islisp` に与える入力フォーム
- `Expected`: 期待値（または `ERROR`）
- `Note`: 比較観点（必要な場合のみ）

---

## M0 準備（差分ハーネス確認）

1.
- Form: `(+ 1 2)`
- Expected: `3`

2.
- Form: `((lambda (x) (+ x 1)) 9)`
- Expected: `10`

3.
- Form: `(if #f 1 2)`
- Expected: `2`

4.
- Form: `(car 1)`
- Expected: `ERROR`

5.
- Form: `(progn (defglobal m0-x 7) m0-x)`
- Expected: `7`

---

## M1 フロントエンド（read/macroexpand/normalize）

1.
- Form: `(progn (defmacro m1-id (x) x) (macroexpand-1 '(m1-id (+ 1 2))))`
- Expected: `(+ 1 2)`

2.
- Form: `(progn (defmacro m1-inc (x) (list '+ x 1)) (m1-inc 4))`
- Expected: `5`

3.
- Form: `(macroexpand '(when #t 1 2))`
- Expected: `ERROR`
- Note: `when` 未定義前提（strict）

4.
- Form: `(progn (defmacro m1-nest (x) (list 'if x 1 0)) (macroexpand '(m1-nest #t)))`
- Expected: `(if #t 1 0)`

5.
- Form: `(progn (defmacro m1-q (x) (list 'quote x)) (m1-q abc))`
- Expected: `abc`

---

## M2 ランタイム最小核（値・環境・クロージャ）

1.
- Form: `((lambda (x) ((lambda (f) (funcall f 5)) (lambda (y) (+ x y)))) 10)`
- Expected: `15`

2.
- Form: `(progn (defun m2-rest (a &rest r) (list a r)) (m2-rest 1 2 3 4))`
- Expected: `(1 (2 3 4))`

3.
- Form: `(let ((x 1)) (let ((x 2)) x))`
- Expected: `2`

4.
- Form: `(let ((x 1)) ((lambda (x) x) 9) x)`
- Expected: `1`

5.
- Form: `((lambda (x y) x) 1)`
- Expected: `ERROR`

---

## M3 LLVM AOT 最小実装

1.
- Form: `(progn (defun m3-f (x) (+ x 3)) (m3-f 4))`
- Expected: `7`

2.
- Form: `(progn (defun m3-if (x) (if x 1 2)) (m3-if #f))`
- Expected: `2`

3.
- Form: `(progn (defun m3-let (x) (let ((y 5)) (+ x y))) (m3-let 7))`
- Expected: `12`

4.
- Form: `(progn (defun m3-rec (n) (if (= n 0) 1 (* n (m3-rec (- n 1))))) (m3-rec 5))`
- Expected: `120`

5.
- Form: `(progn (defun m3-bad (x) (+ x 1)) (m3-bad "a"))`
- Expected: `ERROR`

---

## M4 評価順序と副作用

1.
- Form: `(progn (defglobal m4-log '()) (defun m4-push (x) (setq m4-log (append m4-log (list x))) x) (+ (m4-push 1) (m4-push 2)) m4-log)`
- Expected: `(1 2)`

2.
- Form: `(progn (defglobal m4-x 0) (defun m4-a () (setq m4-x (+ m4-x 1)) m4-x) (defun m4-b () (setq m4-x (+ m4-x 10)) m4-x) (list (m4-a) (m4-b) m4-x))`
- Expected: `(1 11 11)`

3.
- Form: `(progn (defglobal m4-v (vector 0 0)) (defglobal m4-i 0) (defun m4-idx () (setq m4-i (+ m4-i 1)) 0) (setf (vector-ref m4-v (m4-idx)) 9) (list m4-i (vector-ref m4-v 0)))`
- Expected: `(1 9)`

4.
- Form: `(progn (defglobal m4-log2 '()) (defun m4-mark (x) (setq m4-log2 (append m4-log2 (list x))) x) (if (m4-mark #f) (m4-mark 'then) (m4-mark 'else)) m4-log2)`
- Expected: `(#f else)`

5.
- Form: `(progn (defglobal m4-c 0) (let ((x (setq m4-c (+ m4-c 1))) (y (setq m4-c (+ m4-c 10)))) m4-c))`
- Expected: `11`

---

## M5 非局所制御（block/return-from, catch/throw, tagbody/go）

1.
- Form: `(block done (return-from done 42) 0)`
- Expected: `42`

2.
- Form: `(catch 'k (throw 'k 99) 0)`
- Expected: `99`

3.
- Form: `(throw 'no-target 1)`
- Expected: `ERROR`

4.
- Form: `(block b (catch 'k (return-from b 7) 0) 1)`
- Expected: `7`

5.
- Form: `(progn (defglobal m5-x 0) (tagbody start (setq m5-x (+ m5-x 1)) (if (< m5-x 3) (go start) 'done)) m5-x)`
- Expected: `3`

---

## M6 条件処理（handler-case）

1.
- Form: `(handler-case (/ 1 0) (error (c) 'caught))`
- Expected: `caught`

2.
- Form: `(handler-case (+ 1 2) (error (c) 'caught))`
- Expected: `3`

3.
- Form: `(handler-case (progn (car 1) 'ng) (error (c) 'ok))`
- Expected: `ok`

4.
- Form: `(handler-case (handler-case (car 1) (error (c) (throw 'm6 5))) (error (c) 'outer))`
- Expected: `ERROR`
- Note: `throw` 脱出先なし

5.
- Form: `(catch 'm6 (handler-case (car 1) (error (c) (throw 'm6 'via-handler))))`
- Expected: `via-handler`

---

## M7 オブジェクトシステム（defclass/defgeneric/defmethod）

1.
- Form: `(progn (defclass m7-a () ((v :initarg :v :reader m7-v))) (defglobal m7-o (make-instance 'm7-a :v 9)) (m7-v m7-o))`
- Expected: `9`

2.
- Form: `(progn (defgeneric m7-g (x)) (defmethod m7-g ((x m7-a)) 'a) (m7-g m7-o))`
- Expected: `a`

3.
- Form: `(progn (defclass m7-b (m7-a) ()) (defglobal m7-ob (make-instance 'm7-b :v 3)) (defmethod m7-g ((x m7-b)) 'b) (m7-g m7-ob))`
- Expected: `b`

4.
- Form: `(slot-value m7-o 'v)`
- Expected: `9`

5.
- Form: `(progn (defgeneric m7-h (x y)) (defmethod m7-h ((x m7-a) y) 'left) (m7-h m7-ob 1))`
- Expected: `left`

---

## M8 数値境界・比較・型境界

1.
- Form: `(zerop 0)`
- Expected: `#t`

2.
- Form: `(list (plusp 1) (minusp -1) (oddp 3) (evenp 4))`
- Expected: `(#t #t #t #t)`

3.
- Form: `(list (floor 7 3) (ceiling 7 3) (truncate 7 3) (round 7 3))`
- Expected: interpreter と一致
- Note: 処理系の戻り値規約に依存するため oracle 比較

4.
- Form: `(+ 9007199254740991 1)`
- Expected: interpreter と一致

5.
- Form: `(< "a" 1)`
- Expected: `ERROR`

---

## M9 パッケージ/シンボル解決

1.
- Form: `(progn (defpackage :m9.p1 (:use :islisp) (:export foo)) (in-package :m9.p1) (defun foo () 10) (in-package :islisp) (m9.p1:foo))`
- Expected: `10`

2.
- Form: `(progn (defpackage :m9.p2 (:use :islisp)) (in-package :m9.p2) (import 'm9.p1:foo) (foo))`
- Expected: `10`

3.
- Form: `(progn (in-package :islisp) (find-package :m9.p1))`
- Expected: package object（`()` でない）

4.
- Form: `(progn (in-package :m9.p1) (intern "BAR") (in-package :islisp) (m9.p1:bar))`
- Expected: `ERROR`
- Note: 未 export シンボル参照

5.
- Form: `(progn (in-package :m9.p1) (export 'bar) (in-package :islisp) (m9.p1:bar))`
- Expected: `bar`

---

## M10 最適化段階導入（意味不変）

以下は `-O0` と `-O2` の両方で同じ結果になることを確認する。

1.
- Form: `(progn (defun m10-sum (n acc) (if (= n 0) acc (m10-sum (- n 1) (+ acc n)))) (m10-sum 100 0))`
- Expected: `5050`

2.
- Form: `(progn (defglobal m10-c 0) (defun m10-f () (setq m10-c (+ m10-c 1)) m10-c) (+ (m10-f) (m10-f)))`
- Expected: `3`

3.
- Form: `(progn (defun m10-g (x) (if x 1 (car 1))) (m10-g #t))`
- Expected: `1`
- Note: 到達不能分岐の誤評価がないこと

4.
- Form: `(progn (defun m10-h (x) (* (+ x 1) (- x 1))) (m10-h 9))`
- Expected: `80`

5.
- Form: `(progn (defun m10-e () (car 1)) (m10-e))`
- Expected: `ERROR`

---

## M11 ORC JIT（任意）

以下は interpreter / AOT / JIT の三者一致を確認する。

1.
- Form: `(progn (defun m11-fib (n) (if (< n 2) n (+ (m11-fib (- n 1)) (m11-fib (- n 2))))) (m11-fib 10))`
- Expected: `55`

2.
- Form: `(progn (defglobal m11-x 1) (defun m11-bump () (setq m11-x (+ m11-x 1))) (list (m11-bump) (m11-bump) m11-x))`
- Expected: `(2 3 3)`

3.
- Form: `(progn (defmacro m11-m (x) (list '+ x 1)) (m11-m 9))`
- Expected: `10`

4.
- Form: `(block b (catch 'k (throw 'k (return-from b 8))))`
- Expected: `8`

5.
- Form: `(progn (defun m11-err () (throw 'none 1)) (m11-err))`
- Expected: `ERROR`

---

## 実行上の注意

1. 期待値が「interpreter と一致」のケースは、仕様/実装依存差分を固定化しないための oracle 比較ケース。
2. `ERROR` はエラー種別まで厳密一致にする前段階では「エラーが発生すること」を最低条件とする。
3. M10 以降は同一ケースを `-O0/-O2` で必ず二重実行する。
4. M11 は任意だが、導入する場合は AOT/JIT/interpreter の三者比較を必須にする。
