# I1.3 ランタイム ABI

> スパイラル 6 周目 / 内部編 第 3 章

## 学習目標
- コンパイラとランタイムの境界（ABI）を理解する。
- タグ付き値・環境・クロージャの表現を把握する。

## 前提知識
- 評価ループ（[I1.2](02-eval-loop.md)）、コンパイラ編。

## 内容（アウトライン）
- タグ付き値・環境・クロージャのランタイム（`runtime.scm`）。
- IR → LLVM への lowering（`lowering.scm`）と codegen（`codegen.scm`）の役割。
- インタプリタの値表現とコンパイラの値表現の対応関係。

## 関連
- 参照: `docs/runtime-abi.md`、`docs/compiler-compliance-spec.md`
- 次章: [★ 処理系の中で isl-calc を動かす（isl-calc 6 周目）](04-how-calc-runs-inside.md)
