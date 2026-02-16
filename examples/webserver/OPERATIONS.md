# webserver 運用手順 (T8)

## 1. 起動

```sh
WEBSERVER_ROOT=/Volumes/SSD-PLU3/work/LISP/islisp/isl/examples/webserver \
WEBSERVER_CONFIG=/Volumes/SSD-PLU3/work/LISP/islisp/isl/examples/webserver/conf/webserver.conf.lsp \
WEBSERVER_TRANSPORT=http \
/Volumes/SSD-PLU3/work/LISP/islisp/isl/bin/isl \
/Volumes/SSD-PLU3/work/LISP/islisp/isl/examples/webserver/app/main.lsp
```

- ポートは設定ファイル `listen_port` を使用する（8080固定ではない）。
- HTTPS起動時は `WEBSERVER_TRANSPORT=https` を指定する。

## 2. 停止

- 前面実行時: `Ctrl+C`
- 背面実行時:

```sh
kill <PID>
```

## 3. セキュリティ制限

- リクエストライン上限: `4096` bytes
- ヘッダ行上限: `8192` bytes
- ヘッダ合計上限: `32768` bytes
- ボディ上限: `1048576` bytes
- CGIタイムアウト秒: `WEBSERVER_CGI_TIMEOUT_SEC`（既定 `5`）

## 4. ログ

- アクセスログ: `WEBSERVER_ACCESS_LOG`（既定 `/tmp/webserver-access.log`）
  - 形式: `epoch method=... path=... status=... duration_ms=...`
- エラーログ: `WEBSERVER_ERROR_LOG`（既定 `/tmp/webserver-error.log`）
  - 主要イベント:
    - 起動失敗（TLS含む）
    - CGI失敗/タイムアウト
    - 異常リクエスト拒否（4xx）

## 5. トラブルシュート

- TLS起動失敗:
  - `tls_cert_file` / `tls_key_file` の読み取り可否と内容を確認する。
- `431` が返る:
  - ヘッダサイズが上限を超えているため、ヘッダを削減する。
- `413` が返る:
  - `Content-Length` が上限を超えているため、ボディを分割または縮小する。
- `504` が返る:
  - CGIがタイムアウトしている。スクリプト時間短縮、または `WEBSERVER_CGI_TIMEOUT_SEC` を調整する。

