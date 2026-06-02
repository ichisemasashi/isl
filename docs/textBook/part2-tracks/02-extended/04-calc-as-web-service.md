# E1.4 ★ Web サービス化する（isl-calc 4 周目）

> スパイラル **4 周目** / 拡張編 第 4 章 — isl-calc をネットワーク越しに使う

## 学習目標
- HTTP リクエストで受け取った式を `isl-calc` で評価して応答を返せる。
- 評価履歴をファイルに永続化しながらサービスを動かせる。

## 前提知識
- strict 版評価器（[S1.4](../01-standard/04-calc-strict-edition.md)）、ファイル I/O（[E1.2](02-file-io-history.md)）、ネットワーク（[E1.3](03-networking.md)）。

## 内容（アウトライン）
- 最小 HTTP サーバを立て、`?expr=(+ 1 2)` のような式を受ける。
- 受け取った文字列を `parse` → `ast-eval` で評価し、結果を本文で返す。
- 各リクエストを履歴ファイルへ追記。
- `examples/webserver` の構造を参考にする（TLS 化も視野）。

## サンプル（スケルトン）
```lisp
;; request -> expr 文字列を取り出す -> (ast-eval (parse expr) env) -> response
```

## 演習
- 不正な式に対し HTTP 400 とエラーメッセージを返す。

## スパイラルの次の一歩
5 周目（コンパイラ編）では、評価器を `islc` で **ネイティブバイナリ**にし、高速化する。

## 関連
- 次の周: [★ ネイティブバイナリにする（isl-calc 5 周目）](../03-compiler/05-calc-native-binary.md)
