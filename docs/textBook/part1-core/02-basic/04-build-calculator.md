# 2.4 ★ 四則電卓を作る（isl-calc 1 周目）

> スパイラル **1 周目** / 基本編 第 4 章 — 題材「isl-calc」を初めて自作する

## 学習目標
- 数式をリスト `(op a b)` として受け取り、再帰で評価する関数 `calc-eval` を書ける。
- `+` `-` `*` `/` の四則と、入れ子の式を評価できる。

## 前提知識
- リストで式を表す（[2.2](02-list-as-expression.md)）、再帰（[2.3](03-recursion.md)）。

## 内容（アウトライン）
- 数（アトム）はそのまま値。
- リストなら演算子で分岐し、オペランドを再帰評価してから演算する。
- 入れ子 `(+ 1 (* 2 3))` が正しく `7` になることを確認。
- この時点の限界: 変数も関数定義も無い。次の周で CLOS により再設計する。

## サンプル（スケルトン）
```lisp
(defun calc-eval (e)
  (if (consp e)
      (let ((op (car e))
            (a  (calc-eval (car (cdr e))))
            (b  (calc-eval (car (cdr (cdr e))))))
        (cond ((eq op '+) (+ a b))
              ((eq op '-) (- a b))
              ((eq op '*) (* a b))
              ((eq op '/) (/ a b))
              (t (error "unknown operator"))))
      e))

(calc-eval '(+ 1 (* 2 3)))   ; => 7
```

## 演習
- 単項マイナス `(- x)` に対応させる。

## スパイラルの次の一歩
2 周目（中級編）では、この `cond` の塊を **CLOS の AST ノード＋ジェネリック関数**へ作り直し、変数束縛と関数定義を加える。

## 関連
- 次の周: [★ CLOS で AST 評価器に作り直す（isl-calc 2 周目）](../03-intermediate/05-redesign-with-clos-ast.md)
