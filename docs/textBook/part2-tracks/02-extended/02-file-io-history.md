# E1.2 ファイル I/O と履歴永続化

> スパイラル 4 周目 / 拡張編 第 2 章

## 学習目標
- ストリームとファイル入出力を扱える。
- 評価履歴をファイルに保存・読み込みできる。

## 前提知識
- 拡張プリミティブ（[E1.1](01-extended-primitives.md)）。

## 内容（アウトライン）
- ストリームの概念、標準入出力。
- `with-open-file`（本処理系独自の統合形式）でのファイル読み書き。
- isl-calc の入力式と結果を履歴ファイルに追記する。
- 起動時に履歴を読み戻す。

## サンプル
```lisp
(with-open-file (out "history.txt" :direction :output :if-exists :append)
  (format out "~S => ~S~%" expr result))
```

## 演習
- 履歴ファイルから直近 5 件を読み出して表示する。

## 関連
- 次章: [ネットワーク](03-networking.md)
