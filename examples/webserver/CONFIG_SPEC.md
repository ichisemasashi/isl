# webserver 設定ファイル仕様 (T1)

この文書は `examples/webserver/IMPLEMENTATION_TASKS.md` の `T1. 設定ファイル仕様の確定` に対応する。

## 1. 形式

- 形式: Lisp S式（1つの設定フォーム）
- 文字コード: UTF-8
- 先頭フォームは必ず `webserver-config` とする
- 構造:
  ```lisp
  (webserver-config
    (listen_port 18080)
    (document_root "./examples/webserver/runtime/docroot")
    (tls_cert_file "./examples/webserver/runtime/tls/server.crt")
    (tls_key_file "./examples/webserver/runtime/tls/server.key")
    (cgi_enabled #t)
    (cgi_bin_dir "./examples/webserver/runtime/cgi-bin")
    (max_connections 100))
  ```

## 2. 必須キー

以下はすべて必須。

- `listen_port`
  - 型: 整数
  - 制約: `1..65535` の範囲
- `document_root`
  - 型: パス
  - 制約: 既存ディレクトリであること
- `tls_cert_file`
  - 型: パス
  - 制約: 既存の読み取り可能ファイルであること
- `tls_key_file`
  - 型: パス
  - 制約: 既存の読み取り可能ファイルであること
- `cgi_enabled`
  - 型: 真偽値
  - 許可値: `#t` / `#f`
- `cgi_bin_dir`
  - 型: パス
  - 制約: 既存ディレクトリであること
- `max_connections`
  - 型: 整数
  - 制約: `100` 固定（要件上限を固定）

## 3. パス正規化

- すべてのパス値は起動時に絶対パスへ正規化する。
- 正規化後の値を内部設定として利用する。

## 4. 起動ログ

起動成功時は以下をログ出力する（機密情報は除外）。

- `listen_port`
- 正規化後 `document_root`
- 正規化後 `tls_cert_file`
- 正規化後 `tls_key_file`
- `cgi_enabled`
- 正規化後 `cgi_bin_dir`
- `max_connections`

## 5. 実装エントリ

- 実行ファイル: `examples/webserver/app/main.lsp`
- 通信モード:
  - `WEBSERVER_TRANSPORT=http` (既定)
  - `WEBSERVER_TRANSPORT=https` (TLS終端)
- 実行方法:
  ```sh
  WEBSERVER_ROOT=/Volumes/SSD-PLU3/work/LISP/islisp/isl/examples/webserver \
  WEBSERVER_CONFIG=/Volumes/SSD-PLU3/work/LISP/islisp/isl/examples/webserver/conf/webserver.conf.lsp \
  WEBSERVER_TRANSPORT=https \
  WEBSERVER_CHECK_CONFIG=1 \
  /Volumes/SSD-PLU3/work/LISP/islisp/isl/bin/isl \
  /Volumes/SSD-PLU3/work/LISP/islisp/isl/examples/webserver/app/main.lsp
  ```
