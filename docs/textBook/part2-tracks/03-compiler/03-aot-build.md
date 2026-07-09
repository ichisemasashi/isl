# C1.3 AOT ビルド

> スパイラル **5 周目** / コンパイラ編 第 3 章

前章で、ソースが IR を経て LLVM 向けの CFG になるところまで見ました。この章では、その先——**LLVM IR を経てネイティブバイナリを作る**工程を扱います。実行前にすべてを機械語へ翻訳しておく方式を **AOT（Ahead-Of-Time）コンパイル**と呼びます。翻訳済みの実行ファイルは、isl を持たない環境でもそれ単体で動く、普通のネイティブバイナリです。

## 学習目標

この章を終えると、次のことができるようになります。

- `--emit-llvm` で LLVM IR を出力し、その役割を説明できる。
- `islc-aot` でネイティブバイナリをワンステップ生成し、実行できる。
- 最適化パスが whitelist で管理される理由をつかむ。

## 前提知識

- [C1.2 IR を理解する](02-understanding-ir.md)

---

## 1. LLVM IR を出力する——`--emit-llvm`

まず、パイプラインを **LLVM IR** まで進めて、テキストファイルに書き出してみます。

```sh
./bin/islc --emit-llvm hello.ll examples/hello.lsp
```

`hello.ll` が生成されます。中を覗くと、先頭にはランタイム関数の**宣言**が並んでいます。

```llvm
declare ptr @isl_rt_make_int(i64)
declare ptr @isl_rt_make_float(ptr)
declare ptr @isl_rt_make_symbol(ptr)
declare ptr @isl_rt_nil()
declare ptr @isl_rt_lookup(ptr, ptr)
declare ptr @isl_rt_set(ptr, ptr, ptr)
...
```

`isl_rt_make_int` は「整数オブジェクトを作る」、`isl_rt_lookup` は「変数を引く」といった、**ランタイム**（実行時ライブラリ）の関数です。生成された機械語は、数を作ったり変数を参照したりするたびに、これらを呼び出します。前章で見た IR の `(const 1)` や `(var +)` が、ここでは `isl_rt_make_int` や `isl_rt_lookup` の**呼び出し**へと具体化されている、と考えると筋が通ります。

LLVM IR は「機械語の一歩手前の共通言語」で、ここまで来れば、あとは LLVM のツール（clang）が各 CPU 向けの機械語に落としてくれます。

---

## 2. ワンステップでバイナリを作る——`islc-aot`

LLVM IR の出力から clang 呼び出しまでを、一手でやってくれるのが `islc-aot` です。

```sh
./bin/islc-aot -o hello-aot examples/hello.lsp
```

これで、カレントディレクトリに `hello-aot` という**ネイティブバイナリ**（実測で約 90KB）ができます。実行します。

```sh
./hello-aot
# => 3628800
```

`./bin/isl` も `./bin/islc` も介さず、**バイナリ単体**で `10` の階乗が計算されました。これが AOT の成果物です。配布して別の場所で動かすなら、この形が向いています。

> **`--emit-llvm` と `islc-aot` の関係**: `islc-aot` は内部で「LLVM IR を出す → clang でネイティブ化」を続けて行っています。`--emit-llvm` は、その途中の LLVM IR を**観察用に取り出す**モードです。中身を学ぶときは `--emit-llvm`、成果物が欲しいときは `islc-aot`、と使い分けます。

---

## 3. clang 経由という意味

islc は、機械語生成を**自前で書かず、clang（LLVM）に任せて**います。利点は大きく二つです。

- **移植性** … LLVM が対応する CPU なら、同じ LLVM IR からバイナリを作れる。
- **枯れた最適化** … LLVM の成熟した最適化を利用できる。

実行時に出る `overriding the module target triple with arm64-apple-macosx…` という警告は、islc が「このマシンの CPU（ここでは arm64 の macOS）向けに出力する」と LLVM に明示していることの**無害な通知**です。結果には影響しません。

---

## 4. 最適化の whitelist——速いだけでなく「正しい」ために

コンパイラの最適化は、速くする代わりに**プログラムの意味を変えてしまう**危険があります。とくに ISLISP のように評価順序や数値の扱いに規格上の約束がある言語では、最適化が約束を破ると困ります。

そこで islc は、**適用してよい最適化パスを whitelist（許可リスト）で管理**しています。前章の effect barrier（評価順序の固定）と合わせて、「**意味を保ったまま速くする**」範囲を明確にしているのです。どのパスを許可し、何を保証するかは `docs/optimization-policy.md` にまとまっています。速さは大事ですが、それは**正しさの上に**乗って初めて意味がある——コンパイラ設計の要点です。

---

## ★ 処理系の中では（コラム）

LLVM IR のテキスト生成は `src/isl/compiler/codegen.scm`、モード全体の統合（emit-llvm / emit-obj / run / jit）は `runner.scm` が担います。生成コードが呼ぶ `isl_rt_*` 関数の本体は、コンパイラ用ランタイム（`runtime.scm` と C レベルの実装）にあり、タグ付き値・環境・クロージャを扱います。`hello-aot` が単体で動くのは、このランタイムがバイナリにリンクされているからです。ABI（コンパイラとランタイムの取り決め）は `docs/runtime-abi.md` に規定されています。

---

## 演習

1. **LLVM IR を眺める**: `./bin/islc --emit-llvm out.ll examples/hello.lsp` を実行し、`out.ll` の中に `isl_rt_make_int` や `define` を探せ。宣言（`declare`）と定義（`define`）の違いを一言で述べよ。
2. **バイナリを作って動かす**: `islc-aot -o hello-aot examples/hello.lsp` でバイナリを作り、`./hello-aot` が `3628800` を返すことを確かめよ。ファイルサイズも見よ。
3. **正しさの問い**: 「評価順序を勝手に入れ替える最適化」がなぜ ISLISP で危険か、副作用の観点で説明せよ（前章の effect barrier がヒント）。

---

## まとめ

- **AOT** は実行前にすべてを機械語へ翻訳する方式。生成物は単体で動くネイティブバイナリ。
- `--emit-llvm` は LLVM IR を観察用に出力。中身は `isl_rt_*` ランタイム関数の宣言と、それらを呼ぶコード。
- `islc-aot` は「LLVM IR → clang → バイナリ」をワンステップで行う。成果物は `./hello-aot` として単体実行できる。
- 最適化は **whitelist** で管理し、effect barrier と合わせて「意味を保ったまま速くする」範囲を守る。

## 関連

- 参照: `docs/optimization-policy.md` / `docs/runtime-abi.md`
- 前章: [C1.2 IR を理解する](02-understanding-ir.md)
- 次章: [JIT 実行](04-jit-run.md)
