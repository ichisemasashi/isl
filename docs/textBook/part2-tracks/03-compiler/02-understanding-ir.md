# C1.2 IR を理解する

> スパイラル 5 周目 / コンパイラ編 第 2 章

## 学習目標
- フロントエンドが式をどう IR に正規化するかを読める。
- 評価順序が effect barrier で固定される意味を理解する。

## 前提知識
- islc 概観（[C1.1](01-islc-overview.md)）。

## 内容（アウトライン）
- フロントエンド: read → macroexpand → IR 正規化（`frontend.scm`）。
- IR データ構造（`ir.scm`）の概要。
- IR ダンプ: `./bin/islc-front --dump-ir examples/hello.lsp`。
- 副作用は左から順に実行（effect barrier）。
- isl-calc の式が IR でどう表現されるかを観察。

## サンプル
```sh
./bin/islc-front --dump-ir examples/hello.lsp
```

## 関連
- 参照: `docs/side-effect-order.md`
- 次章: [AOT ビルド](03-aot-build.md)
