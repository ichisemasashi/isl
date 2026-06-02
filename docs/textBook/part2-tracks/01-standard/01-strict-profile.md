# S1.1 strict プロファイルとは

> スパイラル 3 周目 / 標準編（part2 第 1 編）第 1 章

## 学習目標
- strict と extended の違いを説明できる。
- `./bin/isl --profile strict` で標準準拠モードを起動できる。

## 前提知識
- part1-core を読了していること。

## 内容（アウトライン）
- extended は既定、strict は ISLISP 規格に厳密準拠するモード。
- strict で無効になるもの（`trace` / `defpackage` / `getenv` / DB・網・FFI 系など）。
- 準拠テストの存在: `gosh test/conformance/run-strict.scm`。
- なぜ規格準拠版を作るのか（移植性・仕様の理解）。

## サンプル
```sh
./bin/isl --profile strict examples/hello.lsp
```

## 関連
- 参照: `docs/compliance-matrix.md`
- 次章: [標準コンディション機構](02-standard-conditions.md)
