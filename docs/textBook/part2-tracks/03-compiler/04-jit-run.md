# C1.4 JIT 実行

> スパイラル 5 周目 / コンパイラ編 第 4 章

## 学習目標
- JIT モードで即時コンパイル実行ができる。
- AOT と JIT の使い分けを理解する。

## 前提知識
- AOT ビルド（[C1.3](03-aot-build.md)）。

## 内容（アウトライン）
- ORC v2 ベースの JIT（M11）。
- `./bin/islc --jit examples/hello.lsp`、`./bin/islc-jit --repl`。
- AOT（配布向け）と JIT（対話・即時実行向け）の比較。

## サンプル
```sh
./bin/islc --jit examples/hello.lsp
./bin/islc-jit --repl
```

## 関連
- 次章: [★ ネイティブバイナリにする（isl-calc 5 周目）](05-calc-native-binary.md)
