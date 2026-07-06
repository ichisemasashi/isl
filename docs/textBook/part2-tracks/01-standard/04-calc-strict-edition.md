# S1.4 ★ strict 準拠版に書き直す（isl-calc 3 周目）

> スパイラル **3 周目** / 標準編 第 4 章 — isl-calc を規格準拠で堅牢化する

2 周目で、私たちは isl-calc をオブジェクトシステムの AST 評価器に作り直し、変数と `let` を手に入れました（[part1 3.5](../../part1-core/03-intermediate/05-redesign-with-object-system-ast.md)）。3 周目のゴールは、その評価器を **strict プロファイルで規格準拠に**し、失敗を**標準コンディションで型付きに扱う**版へ育てることです。

この周は、これまでと少し趣が違います。派手な新機能を足すのではなく、**「拡張に頼っていないか」を点検し**、**エラーの扱いを規格の作法に寄せ**、**自分で検証する**——地味だが実戦的な「堅牢化」の周です。完成版は [`code/isl-calc-v3.lsp`](code/isl-calc-v3.lsp) に同梱しています。

## 学習目標

この章を終えると、次のことができるようになります。

- 2 周目の評価器が strict でそのまま動くことを確認できる。
- ゼロ除算を標準コンディション `<division-by-zero>` として扱える。
- `with-handler` ＋ `instancep` で、失敗を**型付きの結果**として呼び出し側へ返す API を書ける。
- 小さな準拠テストを書き、strict ランナーの発想で回せる。

## 前提知識

- [part1 3.5 オブジェクトシステムで AST 評価器に作り直す](../../part1-core/03-intermediate/05-redesign-with-object-system-ast.md)（v2）
- 標準編 [S1.1](01-strict-profile.md)〜[S1.3](03-packages.md)

> この章のコードは strict で確認します。`./bin/isl --profile strict <file>`。

---

## 1. まず動かす——v2 は strict で動くか

意外に思うかもしれませんが、2 周目の [isl-calc v2](../../part1-core/03-intermediate/code/isl-calc-v2.lsp) は、**そのまま strict で動きます**。

```sh
./bin/isl --profile strict docs/textBook/part1-core/03-intermediate/code/isl-calc-v2.lsp
```

```
(+ 1 (* 2 3)) => 7
(let ((x 10)) (+ x 5)) => 15
(let ((x 2)) (let ((y 3)) (* x y))) => 6
```

理由は [S1.1](01-strict-profile.md) で見たとおりです。v2 が使っている道具——オブジェクトシステム（`defclass`/`defgeneric`/`defmethod`/`create`）、`assoc`、`funcall`、算術——は**すべて規格の範囲内**だからです。拡張に無自覚に依存していなかったことが、ここで報われます。つまり 3 周目の書き直しは「壊れたものを直す」のではなく、「**より規格の作法に沿わせ、より頑丈にする**」作業になります。

---

## 2. ゼロ除算を標準コンディションに委ねる

v2 の `binop-node` の評価はこうでした。

```lisp
(defmethod ast-eval ((n binop-node) env)
  (funcall (op->fn (node-op n))
           (ast-eval (node-lhs n) env)
           (ast-eval (node-rhs n) env)))
```

`(/ x 0)` のとき、`#'/` を `x` と `0` に適用します。ここで**自前のゼロ除算チェックを書く必要はありません**。[S1.2](02-standard-conditions.md) で見たとおり、処理系が自動で **`<division-by-zero>`** を通知してくれるからです。自前で `(when (= b 0) (error ...))` と書くと、かえって標準の型情報（`arithmetic-error-operands` など）を捨ててしまいます。**規格の機構に委ねる**のがここでの正解です。

未定義変数も同様に、`calc-env-lookup` が `error` で通知します。こちらは自作のデータ（連想リスト）に対する参照なので、処理系の `<unbound-variable>` ではなく、`error` が作る `<simple-error>` になります。いずれも `<error>` のサブクラスなので、次節の型付き API で一様に扱えます。

---

## 3. 失敗を「型付きの結果」で返す——`calc-try`

part1 の電卓は、エラーが起きると**その場で止まる**か、既定値に潰すだけでした。実用的な評価器は、呼び出し側に「**成功したのか、失敗なら何が起きたのか**」を返すべきです。[S1.2](02-standard-conditions.md) の `with-handler` ＋ `instancep` で、これを設計します。

```lisp
(defun calc-try (e)
  (catch 'calc-done
    (with-handler
      (lambda (c)
        (throw 'calc-done
          (cond ((instancep c (class <division-by-zero>))
                 (list 'error 'division-by-zero (arithmetic-error-operands c)))
                ((instancep c (class <error>))
                 (list 'error 'other (condition-message c)))
                (t (list 'error 'unknown)))))
      (list 'ok (calc-eval e)))))
```

戻り値の約束はこうです。

- 成功 … `(ok 値)`
- ゼロ除算 … `(error division-by-zero (被除数 除数))`
- その他のエラー … `(error other "メッセージ")`

呼び出し側は、この**タグ付きリスト**を見れば分岐できます。例外で制御が飛ぶのではなく、**普通の値**として失敗が返るので扱いやすいのが利点です。

```lisp
> (calc-try '(+ 1 (* 2 3)))
(ok 7)
> (calc-try '(/ 1 0))
(error division-by-zero (1 0))
> (calc-try '(+ y 1))
(error other "unbound variable: y")
```

`instancep` の分岐順序は [S1.2](02-standard-conditions.md) の鉄則どおり、**細かい `<division-by-zero>` を先に、粗い `<error>` を後に**しています。これで「ゼロ除算だけは構造化した情報（オペランド）を返し、他はメッセージを返す」という使い分けが成立します。

> **名前設計**: 公開 API は [S1.3](03-packages.md) の規律に沿い、`calc-parse` / `calc-eval` / `calc-try` と接頭辞 `calc-` で統一しています。パッケージ（拡張）に依存しないので strict でそのまま移植できます。

---

## 4. 自分で検証する——ミニ準拠テスト

規格準拠を名乗るなら、「本当に期待どおりか」を**自動で確かめる**べきです。isl 本体の strict ランナー（[S1.1](01-strict-profile.md) の `run-strict.scm`）の発想を、小さく自作します。テストケースを**データ**として並べ、期待どおりか照合するだけです。

```lisp
;; テストケース: (value 式 期待値) / (error 式 期待種別)
(defun calc-run-test (tc)
  (let ((kind (car tc)) (expr (cadr tc)) (expected (car (cddr tc))))
    (cond ((eq kind 'value)
           (let ((got (calc-try expr)))
             (if (and (eq (car got) 'ok) (= (cadr got) expected))
                 (progn (format (standard-output) "  ok    ~S => ~S~%" expr expected) t)
                 (progn (format (standard-output) "  FAIL  ~S : ~S (expected ~S)~%"
                                expr got expected) nil))))
          ((eq kind 'error)
           (let ((got (calc-try expr)))
             (if (and (eq (car got) 'error) (eq (cadr got) expected))
                 (progn (format (standard-output) "  ok    ~S => error/~S~%" expr expected) t)
                 (progn (format (standard-output) "  FAIL  ~S : ~S (expected error/~S)~%"
                                expr got expected) nil)))))))
```

ケースを並べて回します。

```lisp
(calc-run-tests
 '((value (+ 1 (* 2 3))            7)
   (value (let ((x 10)) (+ x 5))   15)
   (value (let ((x 2)) (let ((y 3)) (* x y))) 6)
   (error (/ 1 0)                  division-by-zero)
   (error (+ y 1)                  other)))
```

同梱コードを strict で実行すると、次のようになります。

```sh
./bin/isl --profile strict docs/textBook/part2-tracks/01-standard/code/isl-calc-v3.lsp
```

```
  ok    (+ 1 (* 2 3)) => 7
  ok    (let ((x 10)) (+ x 5)) => 15
  ok    (let ((x 2)) (let ((y 3)) (* x y))) => 6
  ok    (/ 1 0) => error/division-by-zero
  ok    (+ y 1) => error/other

5 / 5 passed
```

「**値のケース**」と「**エラーのケース**」を同じ表で扱えるのがポイントです。これは isl 本体の準拠テストが `value` / `error` / `oracle` の種別でケースを分類しているのと同じ発想です（[CLAUDE.md](../../../../CLAUDE.md) のテスト構成を参照）。エラーもまた**仕様の一部**であり、テストの対象なのです。

---

## ★ 処理系の中では（コラム）

私たちが書いた `calc-try` の「失敗を型付きの値で返す」設計は、規模の大きな処理系でも定番です。isl 本体の準拠テストランナー（`test/conformance/common.scm`）も、各ケースを評価し、`value` なら結果を `equal?` で照合、`error` ならコンディションが起きたかを確認します。私たちのミニランナーは、その骨格を数十行に凝縮したものです。「評価器を作る」ことと「評価器を検証する仕組みを作る」ことは両輪だ、という感覚を持てれば、この周の狙いは達成です。

---

## 演習

1. **未定義変数の細分化**: `calc-try` に「未定義変数」専用の枝を足したい。`calc-env-lookup` が投げる `<simple-error>` はメッセージでしか判別できない。どう設計すれば「未定義変数」を他の `error` と区別できるか、案を 2 つ挙げよ（ヒント: メッセージ規約に頼る案／自前のマーカーを irritants に載せる案）。
2. **テストを増やす**: `calc-run-tests` に、ネストした `let` のシャドーイング `(let ((x 1)) (let ((x 2)) x))`（期待値 `2`）と、未知の演算子 `(% 1 2)`（期待 `error/other`）を足し、全部パスすることを確かめよ。
3. **strict 点検**: `calc-try` の実装に、うっかり拡張関数（例: `getenv`）を混ぜて strict で実行するとどうなるか確かめよ。strict が「拡張依存の検出器」として働くことを体験せよ。

---

## スパイラルの次の一歩

4 周目（拡張編）では、この strict 版評価器を土台に、今度は**拡張機能を積極的に使い**ます。履歴をファイルへ永続化し、式の評価を **Web サービス**として公開します。3 周目で「拡張に依存しない芯」を固めたからこそ、拡張は「芯の外側に足す層」として気持ちよく載せられます。

## まとめ

- v2 は拡張非依存だったので、**strict でそのまま動く**。3 周目は「直す」より「規格の作法に寄せ、頑丈にする」周。
- ゼロ除算は自前チェックせず、処理系の **`<division-by-zero>`** に委ねる（型情報を捨てない）。
- `with-handler` ＋ `instancep` で、失敗を `(ok 値)` / `(error 種別 …)` の**型付きの値**で返す `calc-try` を設計。
- テストケースを**データ**として並べる**ミニ準拠テスト**で、値もエラーも自動検証する。
- 公開 API は `calc-` 接頭辞で統一し、パッケージ（拡張）に非依存。

## 関連

- 前章: [S1.3 シンボル解決と名前設計](03-packages.md)
- 前の周: [★ オブジェクトシステムで AST 評価器に作り直す（isl-calc 2 周目）](../../part1-core/03-intermediate/05-redesign-with-object-system-ast.md)
- 次の周: [★ Web サービス化する（isl-calc 4 周目）](../02-extended/04-calc-as-web-service.md)
- 同梱コード: [`code/isl-calc-v3.lsp`](code/isl-calc-v3.lsp)
