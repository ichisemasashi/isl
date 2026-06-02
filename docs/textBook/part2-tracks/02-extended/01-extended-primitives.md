# E1.1 拡張プリミティブの世界

> スパイラル 4 周目 / 拡張編（part2 第 2 編）第 1 章

## 学習目標
- extended プロファイルで使える拡張機能の全体像をつかむ。
- 拡張機能と strict 規格の境界を意識できる。

## 前提知識
- 標準編を読了していること。

## 内容（アウトライン）
- extended は既定プロファイル。`debug` / `getenv` / `system` / DB 系 / ネットワーク系 / FFI 系など。
- 拡張に頼ると移植性が下がるトレードオフ。
- isl-calc を「実用ツール」に育てるため、ここからは extended を活用する。

## サンプル
```sh
./bin/isl examples/hello.lsp   # 既定で extended
```

## 関連
- 参照: `docs/compliance-matrix.md`
- 次章: [ファイル I/O と履歴永続化](02-file-io-history.md)
