# webserver 設定ファイル仕様 (T1)

この文書は `examples/webserver/IMPLEMENTATION_TASKS.md` の `T1. 設定ファイル仕様の確定` に対応する。

## 1. 形式

- 形式: `key=value` の行ベース設定
- 文字コード: UTF-8
- コメント: 行頭 `#`、または行内 `#` 以降をコメントとして扱う
- 空行: 無視する

## 2. 必須キー

以下はすべて必須。

- `listen_port`
  - 型: 整数
  - 制約: `8080` 固定（それ以外は起動失敗）
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
  - 許可値: `true` / `false`
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

- 実行ファイル: `examples/webserver/webserver`
- 検証のみ実行: `--check-config`
