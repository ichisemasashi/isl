# wiki (Apache CGI + ISL)

このディレクトリは、`Apache(httpd)` から `ISL` スクリプトを CGI で実行し、
Wiki システムを段階的に構築するための実装です。

## ファイル構成
- `app/wiki.lsp`: Webアプリ本体（現状は固定HTML）
- `cgi-bin/wiki.cgi`: Apache から呼ばれる CGI エントリ
- `conf/httpd-wiki.conf`: httpd に include する設定例
- `db/001_init.sql`: PostgreSQL 初期スキーマ

## PostgreSQL スキーマ（MVP）

`db/001_init.sql` で次を作成します。
- `pages`: 各ページの現在値（`slug`, `title`, `body_md`）
- `page_revisions`: 編集履歴（世代番号 `rev_no` つき）

設計方針:
- 読み取りは `pages` から単純に取得
- 履歴は `page_revisions` に追記
- `slug` は URL セーフ形式（英小文字・数字・ハイフン）に制限

適用例:

```sh
createdb isl_wiki
psql -d isl_wiki -f /Volumes/SSD-PLU3/work/LISP/islisp/isl/examples/wiki/db/001_init.sql
```

接続文字列の例（後続の `postgres-open` 用）:

```text
postgresql://USER:PASSWORD@127.0.0.1:5432/isl_wiki
```

## Apache 設定

Homebrew Apache / macOS 標準 Apache いずれでも、CGI 設定と実行ユーザー権限を満たせば動作します。

## 現在の動作確認

```text
http://localhost:8080/wiki
```

固定ページ `ISL Wiki (fixed page)` が表示されれば成功です。
