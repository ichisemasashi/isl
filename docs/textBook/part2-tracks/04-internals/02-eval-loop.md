# I1.2 評価ループ

> スパイラル 6 周目 / 内部編 第 2 章

## 学習目標
- `eval-islisp*` がどうフォームを評価するかを読める。
- 特殊形式のディスパッチと末尾呼び出し最適化の仕組みを理解する。

## 前提知識
- core.scm ツアー（[I1.1](01-core-scm-tour.md)）。

## 内容（アウトライン）
- 評価ループ `eval-islisp*`（末尾呼び出し最適化付き）。
- 特殊形式は `eval-special` に dispatch。
- シンボル解決 `resolve-symbol-in-package`、パッケージ状態 `*package-registry*` / `*current-package*`。
- 真偽値は内部で `#t` / `#f`、`nil` は `'()` として束縛される点。

## 関連
- 参照: `docs/symbol-resolution.md`
- 次章: [ランタイム ABI](03-runtime-abi.md)
