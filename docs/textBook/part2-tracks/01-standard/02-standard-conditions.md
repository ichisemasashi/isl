# S1.2 標準コンディション機構

> スパイラル 3 周目 / 標準編 第 2 章

## 学習目標
- ISLISP 標準のコンディションクラス階層を理解する。
- `with-handler` / `signal-condition` / `continue-condition` を使い分けられる。

## 前提知識
- strict プロファイル（[S1.1](01-strict-profile.md)）、エラー処理の基礎（part1 [3.4](../../part1-core/03-intermediate/04-error-handling-intro.md)）。

## 内容（アウトライン）
- 標準コンディションクラス（`<error>` / `<arithmetic-error>` / `<division-by-zero>` など）。
- `with-handler` によるハンドラ確立と非局所脱出。
- 継続可能コンディションと `continue-condition`。
- `handler-case`（Common Lisp 互換）との違いに注意（本書では標準の `with-handler` を使う）。

## サンプル
```lisp
(with-handler
  (lambda (c) (format (error-output) "error: ~A~%" c) (throw 'k nil))
  (block k (/ 1 0)))
```

## 関連
- 次章: [パッケージ](03-packages.md)
