# 3.5 ★ CLOS で AST 評価器に作り直す（isl-calc 2 周目）

> スパイラル **2 周目** / 中級編 第 5 章 — isl-calc を構造化する

## 学習目標
- 数式を CLOS の AST ノード（数・二項演算・変数・関数呼び出し）として表現できる。
- `ast-eval` ジェネリック関数で型ごとにディスパッチして評価できる。
- 変数環境を導入し、`let` 相当の束縛と簡単なユーザ関数定義を評価できる。

## 前提知識
- CLOS（[3.3](03-clos-basics.md)）、クロージャと環境（[3.2](02-closures.md)）、エラー処理（[3.4](04-error-handling-intro.md)）。

## 内容（アウトライン）
- 1 周目の `cond` 分岐を「ノード型ごとの `defmethod`」へ置き換える。
- AST ノード: `num-node` / `binop-node` / `var-node` / `call-node`。
- 環境（変数→値）を引数で持ち回り、`var-node` で参照。
- S 式 → AST への簡易パーサ（`parse`）を用意。
- ゼロ除算・未定義変数を `with-handler` で扱う。

## サンプル（スケルトン）
```lisp
(defgeneric ast-eval (node env))
(defmethod ast-eval ((n num-node) env) (node-value n))
(defmethod ast-eval ((n binop-node) env)
  (apply-op (op-of n)
            (ast-eval (lhs-of n) env)
            (ast-eval (rhs-of n) env)))
;; var-node / call-node は演習・本文で補完
```

## 演習
- `let` 式 `(let ((x 1)) (+ x 2))` を評価できるよう環境を拡張する。

## スパイラルの次の一歩
3 周目（標準編）では、この評価器を **strict プロファイルで規格準拠**に書き直し、コンディションとパッケージで堅牢化する。

## 関連
- 次の周: [★ strict 準拠版に書き直す（isl-calc 3 周目）](../../part2-tracks/01-standard/04-calc-strict-edition.md)
