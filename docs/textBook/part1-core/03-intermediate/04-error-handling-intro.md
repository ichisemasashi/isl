# 3.4 エラー処理の基礎

> スパイラル 2 周目 / 中級編 第 4 章

## 学習目標
- `error` でエラーを通知できる。
- `with-handler` でエラーを捕捉する基本形を理解する。

## 前提知識
- CLOS の基礎（[3.3](03-clos-basics.md)）。

## 内容（アウトライン）
- `error` によるエラー通知。
- ISLISP 標準のコンディション機構 `with-handler` の最小形。
- 「未知の演算子」「ゼロ除算」を電卓でどう扱うか。
- 本格的なコンディション設計は標準編（part2 第 1 編）で扱う。

## サンプル
```lisp
(with-handler
  (lambda (c) (format (standard-output) "caught~%") (throw 'done nil))
  (block done (error "boom")))
```

## 演習
- `calc-eval` が未知演算子で出すエラーを捕捉して既定値を返す。

## 関連
- 次章: [★ CLOS で AST 評価器に作り直す（isl-calc 2 周目）](05-redesign-with-clos-ast.md)
