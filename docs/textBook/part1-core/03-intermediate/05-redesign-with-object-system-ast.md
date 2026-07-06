# 3.5 ★ オブジェクトシステムで AST 評価器に作り直す（isl-calc 2 周目）

> スパイラル **2 周目** / 中級編 第 5 章 — 題材「isl-calc」を構造化する

1 周目（基本編）では、`(op a b ...)` というリストを `calc-eval` が再帰でたどり、`op->fn` の `cond` で演算子を関数に変えて計算しました。よくできた電卓でしたが、章末で 2 つの限界を確認しました——**変数が無い**、**構造が平たい**（`let` のような特殊な評価が扱えない）。

2 周目では、この 2 つを越えます。武器は中級編で仕込んできたものです。

- **オブジェクトシステム**（3 章）… 式の種類を**ノードのクラス**で表し、`cond` を**型ディスパッチ**に置き換える。
- **環境**（2 章）… `(名前 . 値)` の**連想リスト**で変数を持ち回り、`let` で継ぎ足す。
- **エラー処理**（4 章）… 未定義変数・ゼロ除算を止まらずに扱う。

出来上がるのは、**変数と `let` 束縛を持つ、オブジェクトシステムベースの AST 評価器**です。動く完成版は [`code/isl-calc-v2.lsp`](code/isl-calc-v2.lsp) に同梱しています。

## 学習目標

この章を終えると、次のことができるようになります。

- 数式を **AST ノード**（`num-node` / `var-node` / `binop-node` / `let-node`）として表現できる。
- S 式を AST に変換する簡易パーサ `parse` を書ける。
- `ast-eval` ジェネリック関数で、**型ごとにディスパッチ**して評価できる。
- **環境**を引数で持ち回り、変数参照と `let` 束縛を評価できる。
- 1 周目の `cond` 版と比べ、この設計が**なぜ拡張しやすいか**を説明できる。

## 前提知識

- [3.1 高階関数](01-higher-order.md) / [3.2 クロージャ](02-closures.md)（環境と `assoc`）
- [3.3 オブジェクトシステムの基礎](03-object-system-basics.md)（`defclass` / `defgeneric` / `defmethod` / `create`）
- [3.4 エラー処理の基礎](04-error-handling-intro.md)（`with-handler`）
- [2.5 ★ 四則電卓を作る](../02-basic/05-build-calculator.md)（1 周目の到達点）

---

## 1. 設計：式を「型を持つノード」に分ける

1 周目では、式が「アトムか、リストか」の 2 通りでした。2 周目は、式の**種類**をはっきり分け、それぞれをクラスにします。

| 式の例 | ノード型 | 持つスロット |
|--------|---------|-------------|
| `42` | `num-node` | `value` |
| `x` | `var-node` | `name` |
| `(+ 1 2)` | `binop-node` | `op` / `lhs` / `rhs` |
| `(let ((x 1)) (+ x 2))` | `let-node` | `name` / `init` / `body` |

`lhs`・`rhs`・`init`・`body` に入るのも、また**ノード**です。つまり式全体が、ノードを子に持つ**木**になります。1 周目の「入れ子リスト＝木」を、型のついたオブジェクトの木に置き換えるわけです。

```lisp
(defclass num-node   () ((value :initarg value :accessor node-value)))
(defclass var-node   () ((name  :initarg name  :accessor node-name)))
(defclass binop-node () ((op    :initarg op    :accessor node-op)
                         (lhs   :initarg lhs   :accessor node-lhs)
                         (rhs   :initarg rhs   :accessor node-rhs)))
(defclass let-node   () ((name  :initarg name  :accessor node-name)
                         (init  :initarg init  :accessor node-init)
                         (body  :initarg body  :accessor node-body)))
```

---

## 2. S 式 → AST：簡易パーサ `parse`

私たちが打つのは相変わらず `(+ 1 (* 2 3))` のような S 式です。それを上のノードの木に変換するのが `parse` です。1 周目の `calc-eval` の「アトムかリストか」の分岐が、ここでは「どのノードを作るか」の分岐になります。

```lisp
(defun parse (e)
  (cond ((numberp e) (create (class num-node) 'value e))     ; 数
        ((symbolp e) (create (class var-node) 'name e))      ; 変数
        ((eq (car e) 'let)                                   ; (let ((x init)) body)
         (let ((b (car (cadr e))))                           ; b = (x init)
           (create (class let-node)
                   'name (car b)
                   'init (parse (cadr b))                    ; init を再帰的に parse
                   'body (parse (car (cddr e))))))           ; body を再帰的に parse
        (t (create (class binop-node)                        ; (op lhs rhs)
                   'op  (car e)
                   'lhs (parse (cadr e))                     ; lhs を再帰的に parse
                   'rhs (parse (car (cddr e)))))))
```

肝は、子の式にも `parse` を**再帰的に**かけている点です。`(+ 1 (* 2 3))` を parse すると、`rhs` にはさらに `binop-node`（`(* 2 3)`）がぶら下がった木ができます。**評価する前に、まず木を組み立てる**——この「パース → 評価」の 2 段構えは、本物の言語処理系の基本形でもあります。

> **なぜ `numberp` を先に見るか**: `cond` は上から順に試します。数・シンボルという「葉」を先に片付け、残った「リスト」を `let` かそれ以外（二項演算）かで分ける、という順序です。

---

## 3. 環境：変数を持ち回る器

2 章で作った連想リストの環境を、そのまま使います。変数参照は `assoc` で引き、無ければ未定義変数エラーです。

```lisp
(defun env-lookup (name env)
  (let ((pair (assoc name env)))
    (if pair (cdr pair) (error "unbound variable" name))))
```

---

## 4. 評価：型ごとの `defmethod`

いよいよ 1 周目の `cond` を葬り、**ノード型ごとのメソッド**に置き換えます。`ast-eval` は評価対象のノードと**環境**の 2 引数を取ります。

```lisp
(defgeneric ast-eval (node env))

(defmethod ast-eval ((n num-node) env)      ; 数：そのまま
  (node-value n))

(defmethod ast-eval ((n var-node) env)      ; 変数：環境から引く
  (env-lookup (node-name n) env))

(defmethod ast-eval ((n binop-node) env)    ; 演算：左右を評価してから適用
  (funcall (op->fn (node-op n))
           (ast-eval (node-lhs n) env)
           (ast-eval (node-rhs n) env)))

(defmethod ast-eval ((n let-node) env)      ; let：init を評価し、環境に足して body を評価
  (let ((v (ast-eval (node-init n) env)))
    (ast-eval (node-body n)
              (cons (cons (node-name n) v) env))))
```

各メソッドを読み解きます。1 周目で 1 つの `calc-eval` に押し込めていた処理が、**種類ごとに分かれて**いることに注目してください。

- **`num-node`** … 環境は使わず、値をそのまま返す（1 周目の「アトムはそれ自身」）。
- **`var-node`** … 環境を引く。**1 周目にできなかった変数参照**が、たった 1 メソッドで実現した。
- **`binop-node`** … 左右を**それぞれ環境つきで再帰評価**し、`op->fn` の関数を `funcall`。1 周目の再帰がメソッドの再帰になった。
- **`let-node`** … これが 2 周目の目玉。`init` を今の環境で評価し、その値を `(名前 . 値)` として**環境の先頭に継ぎ足し**、その拡張環境で `body` を評価する。2 章で見た「環境の継ぎ足し」そのものです。

`op->fn` は 1 周目のものを流用します。

```lisp
(defun op->fn (op)
  (cond ((eq op '+) #'+) ((eq op '-) #'-)
        ((eq op '*) #'*) ((eq op '/) #'/)
        (t (error "unknown operator" op))))
```

最後に、入口となる `calc` を用意します。S 式を parse し、**空の環境**で評価するだけです。

```lisp
(defun calc (e) (ast-eval (parse e) '()))
```

---

## 5. 動かして確かめる

同梱の [`code/isl-calc-v2.lsp`](code/isl-calc-v2.lsp) を実行します。

```sh
./bin/isl docs/textBook/part1-core/03-intermediate/code/isl-calc-v2.lsp
```

出力:

```
(+ 1 (* 2 3)) => 7
(let ((x 10)) (+ x 5)) => 15
(let ((x 2)) (let ((y 3)) (* x y))) => 6
```

- `(+ 1 (* 2 3))` … 1 周目と同じ入れ子の式が、AST 経由でも `7`。
- `(let ((x 10)) (+ x 5))` … **変数** `x` が `10` に束縛され、`(+ x 5)` が `15`。1 周目にはできなかった芸当です。
- ネストした `let` … 内側の環境が外側を継ぎ足して見えるので、`x` と `y` の両方が引けて `6`。

1 周目の「変数が無い」「`let` が扱えない」という限界を、**両方**越えられました 🎉

---

## 6. エラーを止まらずに扱う（4 章の応用）

未定義変数やゼロ除算は、いまは `error` で止まります。4 章の `safe-eval` で包めば、既定値を返して続行できます。

```lisp
(defun safe-calc (e default)
  (catch 'k
    (with-handler (lambda (c) (throw 'k default))
      (calc e))))
```

```lisp
> (safe-calc '(+ y 1) 'undef)      ; y は未定義 → 既定値
undef
> (safe-calc '(/ 1 0) 'inf)        ; ゼロ除算 → 既定値
inf
> (safe-calc '(let ((x 3)) (* x x)) 'undef)
9
```

「木の評価」と「エラーの回収」を別々の部品（`ast-eval` と `safe-calc`）に分けて書けるのも、構造化した設計の効き目です。

---

## 7. 1 周目と何が変わったか

| | 1 周目（`cond`／リスト） | 2 周目（オブジェクトシステム／AST） |
|---|---|---|
| 式の表現 | 生の S 式リスト | 型を持つノードの木 |
| 分岐 | `calc-eval` 内の `if`/`cond` | ノード型ごとの `defmethod` |
| 変数 | **無い** | `var-node`＋環境で参照 |
| `let` 束縛 | **無い**（平たい） | `let-node`＋環境の継ぎ足し |
| 拡張の仕方 | 本体を書き換える | クラスとメソッドを**足す** |

新しい構文（たとえば `if` 式）を足したいとき、1 周目なら `calc-eval` 本体を開いて分岐を増やしますが、2 周目は `if-node` クラスと `ast-eval` メソッドを**足す**だけ——`num-node` や `binop-node` の既存コードには一切触れません。この「追加で育てられる」性質が、次の周から先で効いてきます。

---

## 演習

1. **ネスト let**: `(let ((x 1)) (let ((y (+ x 1))) (* x y)))` を `calc` で評価し、内側の `let` が外側の `x` を見られることを確かめよ。
2. **シャドーイング**: `(let ((x 1)) (let ((x 2)) x))` は `1` と `2` のどちらになるか予想し、確かめよ。環境を**先頭に足す**設計から理由を説明せよ。
3. **`if` ノード（発展）**: `if-node`（`test` / `then` / `else` の 3 スロット）を足し、`parse` に `(eq (car e) 'if)` の枝を、`ast-eval` に対応メソッドを加えよ。`(if (> x 0) x (- x))` のような式を評価できるようにせよ（比較演算子は `op->fn` に足す）。
4. **関数呼び出し（発展）**: 演習 3 まで解けたら、`call-node`（ユーザ定義関数の呼び出し）を設計してみよ。関数の**環境**をどう持たせるかで、2 章のクロージャの知識が効いてくる。

---

## まとめ

- 式を **AST ノードの木**（`num` / `var` / `binop` / `let`）で表し、S 式は `parse` で木に変換する。
- 評価は `ast-eval` ジェネリック関数の**型ディスパッチ**。1 周目の `cond` が、種類ごとの `defmethod` に分かれた。
- **環境**（連想リスト）を引数で持ち回り、`var-node` で参照、`let-node` で先頭に継ぎ足す——1 周目にできなかった**変数と束縛**が実現した。
- エラーは `safe-calc`（4 章の型）で回収し、止まらずに評価できる。
- この設計は「クラスとメソッドを**足す**」形で拡張できる。`if` や関数呼び出しへの道が開けている。

## スパイラルの次の一歩

3 周目（標準編）では、この評価器を **strict プロファイルで規格準拠**に書き直します。`error` の投げ方や `with-handler` を ISLISP 標準のコンディション体系に沿わせ、パッケージで名前空間を整理して、電卓を「規格に堪える」堅牢さへ引き上げます。

## 関連

- 前章: [3.4 エラー処理の基礎](04-error-handling-intro.md)
- 前の周: [★ 四則電卓を作る（isl-calc 1 周目）](../02-basic/05-build-calculator.md)
- 次の周: [★ strict 準拠版に書き直す（isl-calc 3 周目）](../../part2-tracks/01-standard/04-calc-strict-edition.md)
- 同梱コード: [`code/isl-calc-v2.lsp`](code/isl-calc-v2.lsp)
