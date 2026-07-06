# S1.2 標準コンディション機構

> スパイラル **3 周目** / 標準編 第 2 章

part1 の [3.4 エラー処理の基礎](../../part1-core/03-intermediate/04-error-handling-intro.md) で、`error` でエラーを投げ、`with-handler` で捕まえる型を学びました。あのとき、ハンドラが受け取る `c`（コンディション）の**中身**にはあえて踏み込みませんでした。この章では、その `c` を正面から扱います。

キーアイデアは——**「コンディションは、種類（クラス）を持つオブジェクトである」**。ゼロ除算・未定義変数・その他のエラーは、それぞれ別のクラスとして区別できます。だから「ゼロ除算だけは既定値に、他は診断メッセージを出す」のように、**エラーの種類に応じた処理**が書けます。これは 3 周目の isl-calc を頑丈にするための中心的な道具になります。

## 学習目標

この章を終えると、次のことができるようになります。

- ISLISP の標準コンディションクラス階層を読める。
- `with-handler` と **`instancep`** で、エラーの**種類ごとに**処理を分けられる。
- コンディションから情報を取り出せる（`condition-message` / `arithmetic-error-operands` など）。
- 継続可能コンディションと `handler-case` の位置づけを説明できる。

## 前提知識

- [S1.1 strict プロファイルとは](01-strict-profile.md)
- part1 [3.4 エラー処理の基礎](../../part1-core/03-intermediate/04-error-handling-intro.md)（`with-handler` と非局所脱出）
- part1 [3.3 オブジェクトシステムの基礎](../../part1-core/03-intermediate/03-object-system-basics.md)（`instancep` の下敷き）

> この章のサンプルは strict プロファイルで確認しています。`./bin/isl --profile strict <file>` で実行してください。

---

## 1. コンディションは「クラスを持つオブジェクト」

part1 では「エラーが起きたらハンドラが呼ばれる」ことだけ見ました。実は、ハンドラに渡される `c` は**オブジェクトシステムのインスタンス**で、しかも「どんなエラーか」を表す**クラス**を持っています。ゼロ除算なら `<division-by-zero>`、未定義変数なら `<unbound-variable>`、という具合です。

3 章で学んだ `instancep`（「このオブジェクトはこのクラスか？」）を、そのままコンディションに使えます。ここが本章の要です。

---

## 2. 標準コンディションのクラス階層

ISLISP の標準コンディションは、階層（継承）を成しています。isl で実際に組み込まれている主な階層は次のとおりです。

```
<condition>
└─ <serious-condition>
   └─ <error>
      ├─ <arithmetic-error>
      │  └─ <division-by-zero>
      ├─ <program-error>
      │  └─ <undefined-entity>
      │     ├─ <unbound-variable>
      │     └─ <undefined-function>
      └─ <simple-error>          （error で作られる既定のエラー）
```

大事なのは**継承**です。`<division-by-zero>` は `<arithmetic-error>` でもあり、`<error>` でもあります。だから「`<division-by-zero>` だけ特別扱いし、それ以外の `<error>` はまとめて処理」といった、**粗いネットと細かいネットの使い分け**ができます。

---

## 3. 種類ごとに捕まえる——`with-handler` ＋ `instancep`

実際に撃ち分けてみます。1 つのハンドラの中で `instancep` を使い、コンディションの種類で分岐します。

```lisp
(defun describe-error (c)
  (cond ((instancep c (class <division-by-zero>))
         (format (standard-output) "ゼロ除算: operands=~S~%"
                 (arithmetic-error-operands c)))
        ((instancep c (class <error>))
         (format (standard-output) "その他のエラー: ~A~%"
                 (condition-message c)))))

(defun try-div (a b)
  (catch 'k
    (with-handler
      (lambda (c) (describe-error c) (throw 'k 'ng))
      (/ a b))))
```

実行します。

```lisp
> (try-div 10 2)
5
> (try-div 10 0)
ゼロ除算: operands=(10 0)
ng
```

`(/ 10 0)` が起こしたコンディションは、処理系が自動で通知する **`<division-by-zero>`** です（part1 の電卓のように自分で `error` を書く必要はありません）。ハンドラは `instancep` でそれを見分け、専用の診断を出しました。

`cond` の順序に注意してください。**細かいクラス（`<division-by-zero>`）を先に、粗いクラス（`<error>`）を後に**書きます。逆にすると、ゼロ除算も先に `<error>` の枝に吸い込まれてしまいます（`<division-by-zero>` は `<error>` でもあるため）。

---

## 4. コンディションから情報を取り出す

コンディションには、種類に応じた**アクセサ**が用意されています。診断メッセージやログを作るのに使います。

| アクセサ | 対象 | 返すもの |
|---------|------|---------|
| `condition-message` | すべての `<error>` | 人間向けメッセージ文字列 |
| `arithmetic-error-operation` | `<arithmetic-error>` | 演算（`div` など） |
| `arithmetic-error-operands` | `<arithmetic-error>` | オペランドのリスト |
| `undefined-entity-name` | `<undefined-entity>` | 未定義の名前（シンボル） |

未定義変数を捕まえて名前を取り出す例です。

```lisp
> (catch 'k
    (with-handler
      (lambda (c)
        (format (standard-output) "未定義: ~S~%"
                (undefined-entity-name c))
        (throw 'k 'x))
      nosuchvar))
未定義: nosuchvar
x
```

`error` で自分が投げたコンディションも、`condition-message` でメッセージを読めます。メッセージは `format` 風の指定が展開された後の文字列です。

```lisp
> (catch 'k
    (with-handler
      (lambda (c) (format (standard-output) "msg=~S~%" (condition-message c))
                  (throw 'k 'x))
      (error "unknown operator: ~A" '%)))
msg="unknown operator: %"
```

---

## 5. 継続可能コンディションと `continue-condition`（概観）

コンディションには「捕まえたあと、**中断した場所から実行を再開できる**」ものがあります。これを**継続可能**（continuable）コンディションと呼び、ハンドラは `continue-condition` で再開値を返して処理を続けさせられます。あるコンディションが継続可能かは `condition-continuable` で調べられます。

```lisp
> (catch 'k
    (with-handler
      (lambda (c)
        (format (standard-output) "continuable? ~S~%" (condition-continuable c))
        (throw 'k 'x))
      (/ 1 0)))
continuable? nil
x
```

上のとおり、ゼロ除算のような**深刻なエラー**（`<serious-condition>` 系）は継続不可で、再開はできません。継続可能コンディションは「軽微な通知に対して、呼び出し側が代替値を与えて先へ進ませたい」ような場面で使う道具です。本書の isl-calc では、失敗はすべて**非局所脱出で回収**する方針を採るので、`continue-condition` は概念として押さえるにとどめます。

---

## 6. `handler-case` との違い（注意）

isl には、Common Lisp 互換の **`handler-case`** も用意されています。`with-handler` とよく似ていますが、これは ISLISP 標準の機構とは別物です。本書は移植性を重視し、**標準の `with-handler` に統一**します。

両者の勘所だけ対比しておきます。

- `with-handler` … ISLISP 標準。ハンドラは**非局所脱出で抜ける**のが基本（part1 3.4 で学んだ型）。
- `handler-case` … CL 互換の拡張的な書き味。節ごとにコンディション型を書けて簡潔だが、標準ではない。

strict で規格準拠を目指す本編では、`with-handler` ＋ `instancep` の組み合わせが基本形です。

---

## ★ 処理系の中では（コラム）

isl のコンディションは、内部的には「クラス名・メッセージ・継続可否・irritants（関連値）・追加情報」を束ねた 1 個のオブジェクトです（`core/conditions.scm` の `make-isl-condition`）。`(/ 1 0)` は `isl-signal-div0` が `<division-by-zero>` のコンディションを組み立てて `isl-signal-condition` に渡し、未定義変数は `isl-signal-unbound-variable` が `<unbound-variable>` を組み立てます。`with-handler` はハンドラを内側から外側へ積んだスタック（`*isl-handler-stack*`）で、シグナル時に近いものから呼ばれます。`instancep` が効くのは、コンディションが正しく `<division-by-zero>` などのクラスに結び付けられているからです。

---

## 演習

1. **三分岐**: `describe-error` を拡張し、`<unbound-variable>`（`undefined-entity-name` で名前を表示）・`<division-by-zero>`・その他の `<error>` の 3 種類を撃ち分けよ。
2. **順序の罠**: 演習 1 の `cond` で `<error>` の枝を**先頭**に置くとどうなるか予想し、確かめよ。なぜそうなるかを継承で説明せよ。
3. **メッセージ収集**: 任意の式を評価し、失敗したら `(list 'error (condition-message c))`、成功したら `(list 'ok 値)` を返す関数 `safe` を書け（次章の `calc-try` の原型）。

---

## まとめ

- コンディションは**クラスを持つオブジェクト**。`<condition>` を頂点に、`<error>` → `<arithmetic-error>` → `<division-by-zero>` のような階層を成す。
- `with-handler` のハンドラ内で **`instancep`** を使えば、エラーの**種類ごと**に処理を分けられる。**細かいクラスを先に**書くのが鉄則。
- アクセサ（`condition-message` / `arithmetic-error-operands` / `undefined-entity-name`）で診断情報を取り出せる。
- 深刻なエラーは継続不可。継続可能コンディションと `continue-condition` は概念として押さえる。
- 本編は標準の `with-handler` に統一する（`handler-case` は CL 互換の別物）。

## 関連

- 前章: [S1.1 strict プロファイルとは](01-strict-profile.md)
- 次章: [パッケージとシンボル解決](03-packages.md)
- この先の応用: [★ strict 準拠版に書き直す](04-calc-strict-edition.md)（`instancep` 分岐で `calc-try` を作る）
