# C1.3 AOT ビルド

> スパイラル 5 周目 / コンパイラ編 第 3 章

## 学習目標
- ISLISP プログラムを LLVM IR 経由でネイティブバイナリにできる。
- 生成物を実行して動作確認できる。

## 前提知識
- IR の理解（[C1.2](02-understanding-ir.md)）。

## 内容（アウトライン）
- `--emit-llvm` で LLVM IR を出力し、`clang` 経由でネイティブ化する流れ。
- `islc-aot` でワンステップにバイナリ生成。
- 生成バイナリの実行と入出力。
- 最適化パスの whitelist 方針（意味保証の範囲）。

## サンプル
```sh
./bin/islc --emit-llvm out.ll examples/hello.lsp
./bin/islc-aot -o hello-aot examples/hello.lsp
./hello-aot
```

## 関連
- 参照: `docs/optimization-policy.md`
- 次章: [JIT 実行](04-jit-run.md)
