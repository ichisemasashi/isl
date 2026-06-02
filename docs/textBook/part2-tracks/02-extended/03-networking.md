# E1.3 ネットワーク

> スパイラル 4 周目 / 拡張編 第 3 章

## 学習目標
- TCP ソケットの待ち受け・接続・送受信の基本を理解する。
- TLS 対応関数の存在を知る。

## 前提知識
- ファイル I/O（[E1.2](02-file-io-history.md)）。

## 内容（アウトライン）
- ソケットの待ち受け（listen）/ 受理（accept）/ 送受信。
- TLS 系関数（`tls-listen` / `tls-accept` / `tls-send` 等）がランタイムに実装済みであること。
- リクエストを 1 行受け取り、応答を返す最小エコーの形。
- 既存例 `examples/webserver` の存在に触れる（次章で接続）。

## サンプル
```lisp
;; リスナを開き接続を受理して 1 行返す最小形（詳細は本文）
```

## 関連
- 参照: `examples/webserver`
- 次章: [★ Web サービス化する（isl-calc 4 周目）](04-calc-as-web-service.md)
