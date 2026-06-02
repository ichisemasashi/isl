# C1.1 islc 概観

> スパイラル 5 周目 / コンパイラ編（part2 第 3 編）第 1 章

## 学習目標
- インタプリタとコンパイラの違いを isl の文脈で説明できる。
- `islc` の各モード（emit-llvm / AOT / run / jit）の役割を把握する。

## 前提知識
- part1-core と、できれば標準編。

## 内容（アウトライン）
- コンパイラの全体像（read → macroexpand → IR → LLVM → ネイティブ）。
- 主要コマンド:
  - `./bin/islc --emit-llvm out.ll examples/hello.lsp`
  - `./bin/islc --run examples/hello.lsp`
  - `./bin/islc-aot -o hello-aot examples/hello.lsp`
  - `./bin/islc --jit examples/hello.lsp`
- マイルストーン M0〜M11 の存在。

## 関連
- 参照: `docs/compiler-milestones.md`
- 次章: [IR を理解する](02-understanding-ir.md)
