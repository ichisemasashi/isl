# S1.1 strict プロファイルとは

> スパイラル **3 周目** / 標準編（part2 第 1 編）第 1 章

part1-core（0〜2 周目）で、私たちは isl-calc を REPL の一行評価からオブジェクトシステムの AST 評価器まで育てました。ただし、これまで使ってきたのは isl の **extended（拡張）プロファイル**——処理系独自の便利機能が全部入りのモードでした。

3 周目のテーマは、そこから拡張を取り払った **strict（標準準拠）プロファイル**です。strict は、ISLISP 規格（ISO/IEC 13816）に厳密に沿った機能だけで動くモードです。この編では、strict の考え方を学び、最後に isl-calc を「規格に堪える」版へ書き直します。この章はその出発点として、strict とは何か・どう起動するか・何が変わるかを押さえます。

## 学習目標

この章を終えると、次のことができるようになります。

- **strict** と **extended** の違いを説明できる。
- `./bin/isl --profile strict <file>` で標準準拠モードを起動できる。
- strict で無効になる代表的な機能を挙げられる。
- なぜ規格準拠版を作るのか（移植性・仕様理解）を自分の言葉で言える。

## 前提知識

- part1-core（0〜2 周目）を読了していること。

---

## 1. なぜ規格準拠を考えるのか

isl は便利な拡張をたくさん持っています。しかしその拡張は **isl だけの方言**です。別の ISLISP 処理系に持っていくと動かないかもしれません。「規格の範囲だけで書く」ことには、はっきりした利点があります。

- **移植性** — 規格準拠のコードは、規格を満たす他の処理系でも動く見込みが高い。
- **仕様の理解** — 「どこまでが規格で、どこからが拡張か」を意識すると、言語の芯が見える。
- **拡張依存の可視化** — 拡張に無自覚に頼っていた箇所が、strict にすると浮かび上がる。

3 周目で isl-calc を strict 化する作業は、まさにこの「拡張に頼っていた箇所のあぶり出し」でもあります。

---

## 2. 起動と切り替え

プロファイルはコマンドラインの `--profile` で指定します。指定しなければ **extended が既定**です。

```sh
# 既定（extended）
./bin/isl examples/hello.lsp

# strict（標準準拠）
./bin/isl --profile strict examples/hello.lsp
```

REPL も同様に起動できます。

```sh
./bin/isl --profile strict
```

起動時のバナーに `Profile: strict` と表示されるので、いまどちらで動いているか一目でわかります。

---

## 3. strict で変わること

strict にすると、拡張として分類された機能が使えなくなります。無効化のされ方は 2 通りあります。

### (1) 拡張の特殊形式は「明示的に拒否」される

デバッグ用の `trace` / `untrace` は拡張の特殊形式です。strict で使うと、はっきりエラーになります。

```lisp
> (trace calc-eval)
*** ERROR: Special form is unavailable in strict profile trace
```

### (2) 拡張のプリミティブは「そもそも定義されていない」

システム連携（`getenv` / `setenv` / `system`）、`debug` / `break`、データベース系（`sqlite-*` など）、ネットワーク系（`tcp-*` / `http-*` など）、FFI 系は、strict では**登録されません**。呼ぶと「未定義関数」になります。

```lisp
> (getenv "HOME")
*** ERROR: undefined function: getenv
```

`getenv` が「存在しないが呼ばれた関数」として扱われている点に注目してください。strict は拡張プリミティブを環境に載せないので、結果として `<undefined-function>`（未定義関数）のコンディションになります。このコンディションを捕まえて診断する方法は、次章で扱います。

### 何が「共通」で使えるのか

逆に、算術・比較・リスト・ベクタ・文字列・制御構造といった基本機能や、`format` / `apply` / `funcall`、そして**オブジェクトシステム**（`defclass` / `defgeneric` / `defmethod` / `create`）とコンディション機構（`with-handler` など）は strict でも使えます。つまり、isl-calc がこれまで使ってきた道具の大半は規格の範囲内です。だからこそ 3 周目の書き直しは、思ったより小さな手直しで済みます（S1.4 で確認します）。

> **パッケージについての注意**: `defpackage` / `in-package`（Common Lisp 由来のパッケージ機能）は、isl では**拡張**に分類されます。ISLISP 規格そのものには、この形のパッケージ機能は含まれません。移植性を保つため、strict を意識したコードでは**パッケージに依存しない**書き方を選びます。詳しくは [S1.3](03-packages.md) で扱います。

---

## 4. 準拠テストという物差し

「本当に規格どおりか」を確かめるために、isl には strict 用の準拠テストが用意されています。

```sh
gosh test/conformance/run-strict.scm
```

これは規格が要求する動作を多数のケースで検証するもので、isl 本体が strict でどこまで規格を満たすかの物差しです。3 周目の最後（[S1.4](04-calc-strict-edition.md)）では、この「ケースを並べて期待どおりか自動で確かめる」発想を、自作の isl-calc にも小さく持ち込みます。

> テストランナーは `./bin/isl` ではなく **`gosh`（Gauche）**で直接動かします。これは isl の実装言語である Gauche のスクリプトだからです。

---

## ★ 処理系の中では（コラム）

プロファイルの実体は、`core.scm` のグローバル状態 `*isl-profile*` が `strict` か `extended` かを保持しているだけです。起動時に strict が選ばれると、拡張プリミティブを登録する処理がスキップされ（`disable-extended-primitives!` 相当）、拡張特殊形式は評価時に `extended-special-form?` で弾かれます。つまり strict とは「初期環境から拡張を抜いた状態」であり、言語の芯だけを載せた素の isl だと考えると腑に落ちます。この初期環境の組み立ては 6 周目（内部編）で読みます。

---

## 演習

1. **起動の確認**: 手元の適当な `.lsp` を `--profile strict` と無指定の両方で実行し、バナーの `Profile:` 表示の違いを確かめよ。
2. **拡張の検出**: `(trace foo)` と `(getenv "PATH")` を strict で評価し、エラーメッセージの**種類の違い**（特殊形式の拒否 vs 未定義関数）を観察せよ。
3. **共通機能の確認**: part1 の [isl-calc v2](../../part1-core/03-intermediate/code/isl-calc-v2.lsp) を `--profile strict` で実行し、オブジェクトシステムと `let` がそのまま動くことを確かめよ（S1.4 の伏線）。

---

## まとめ

- **extended が既定**、**strict は ISLISP 規格に厳密準拠**するモード。`--profile strict` で起動する。
- strict で無効になるもの: **拡張特殊形式**（`trace`/`untrace`）は明示的に拒否、**拡張プリミティブ**（`getenv`/`system`/DB/網/FFI/`debug` など）は未定義関数になる。
- 算術・リスト・`format`・**オブジェクトシステム**・**コンディション機構**は規格の範囲で、strict でも使える。
- パッケージ（`defpackage`/`in-package`）は拡張分類。strict コードでは非依存に保つ。
- 準拠テスト `gosh test/conformance/run-strict.scm` が「規格どおりか」の物差し。

## 関連

- 前の周: [★ オブジェクトシステムで AST 評価器に作り直す](../../part1-core/03-intermediate/05-redesign-with-object-system-ast.md)
- 参照: `docs/compliance-matrix.md`（strict/extended 差分の一覧）
- 次章: [標準コンディション機構](02-standard-conditions.md)
