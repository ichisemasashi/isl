# 3.3 CLOS の基礎

> スパイラル 2 周目 / 中級編 第 3 章

## 学習目標
- `defclass` でクラスとスロットを定義できる。
- `defgeneric` / `defmethod` でジェネリック関数と多重ディスパッチを書ける。

## 前提知識
- クロージャ（[3.2](02-closures.md)）。

## 内容（アウトライン）
- クラス定義 `defclass`、スロット、`create` / アクセサ。
- ジェネリック関数 `defgeneric` とメソッド `defmethod`。
- 型によるディスパッチ ＝ 電卓の `cond` 分岐を「ノード型ごとのメソッド」に置き換える発想。
- メソッド結合の概要（深入りは上級編）。

## サンプル
```lisp
(defclass num-node () ((value :initarg :value :accessor node-value)))
(defgeneric ast-eval (node))
(defmethod ast-eval ((n num-node)) (node-value n))
```

## 演習
- `add-node`（左右 2 つのオペランドを持つ）クラスを定義する。

## 関連
- 次章: [エラー処理の基礎](04-error-handling-intro.md)
