# S1.4 ★ strict 準拠版に書き直す（isl-calc 3 周目）

> スパイラル **3 周目** / 標準編 第 4 章 — isl-calc を規格準拠で堅牢化する

## 学習目標
- 2 周目の CLOS 評価器を strict プロファイルで動くよう書き直せる。
- ゼロ除算・未定義変数・引数不足を標準コンディションで通知・捕捉できる。
- 準拠テストの考え方で自作評価器を検証できる。

## 前提知識
- CLOS 評価器（part1 [3.5](../../part1-core/03-intermediate/05-redesign-with-clos-ast.md)）、標準編 [S1.1](01-strict-profile.md)〜[S1.3](03-packages.md)。

## 内容（アウトライン）
- extended 依存を排除し、`./bin/isl --profile strict` で全機能が動くことを確認。
- エラーを `<division-by-zero>` などの標準コンディションへ置き換える。
- `with-handler` で評価エラーを呼び出し側に返す API 設計。
- 簡易テストを書き、strict ランナーの発想で回す。

## サンプル（スケルトン）
```lisp
(defmethod ast-eval ((n binop-node) env)
  (let ((b (ast-eval (rhs-of n) env)))
    (when (and (eq (op-of n) '/) (= b 0))
      (error "division by zero"))   ; 本文では標準コンディションに置換
    ...))
```

## 演習
- 未定義変数参照時に捕捉可能なコンディションを通知する。

## スパイラルの次の一歩
4 周目（拡張編）では、この評価器を **extended 機能で永続化し、Web サービス化**する。

## 関連
- 次の周: [★ Web サービス化する（isl-calc 4 周目）](../02-extended/04-calc-as-web-service.md)
