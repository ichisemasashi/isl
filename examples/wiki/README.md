# wiki (Apache CGI + ISL)

このディレクトリは、`Apache(httpd)` から `ISL` スクリプトを CGI で実行し、
Wiki システムを段階的に構築するための実装です。

## ファイル構成
- `app/wiki.lsp`: Webアプリ本体（MVP 3画面）
- `cgi-bin/wiki.cgi`: Apache から呼ばれる CGI エントリ
- `conf/httpd-wiki.conf`: httpd に include する設定例
- `db/001_init.sql`: PostgreSQL 初期スキーマ
- `docs/markdown-policy.md`: Markdown変換方式の決定記録
- `docs/url-naming-conventions.md`: URL / 命名規約の固定

## MVP 3画面
- `/wiki` : ページ一覧
- `/wiki/{slug}` : ページ表示（DBの Markdown を HTML 変換して表示）
- `/wiki/{slug}/edit` : 編集画面（POSTで保存可能）
- `/wiki/new` : 新規ページ作成（POSTで保存可能）
- `/wiki/media` : メディア一覧（画像/動画/音声）
- `/wiki/media/new` : メディア追加（POSTで保存）

`wiki.lsp` は `PATH_INFO` でルーティングします。

## URL / 命名規約（固定）

`docs/url-naming-conventions.md` に固定済みです。
要点:
- Canonical URL は末尾スラッシュなし
- `slug` は `^[a-z0-9](?:[a-z0-9-]{0,126}[a-z0-9])?$`
- URL は `title` ではなく `slug` を使用

## Markdown変換方式（決定）

`pandoc` を外部コマンドとして利用します。
詳細は `docs/markdown-policy.md` を参照してください。

要点:
- 変換コマンド: `pandoc --from=gfm-raw_html --to=html5`
- `ISL` から `system` 経由で実行
- 入出力は一時ファイル経由

## PostgreSQL スキーマ（MVP）

`db/001_init.sql` で次を作成します。
- `pages`: 各ページの現在値（`slug`, `title`, `body_md`）
- `page_revisions`: 編集履歴（世代番号 `rev_no` つき）
- `media_assets`（`db/002_media_assets.sql`）: メタ情報（実ファイルはストレージ保存）

設計方針:
- 読み取りは `pages` から単純に取得
- 履歴は `page_revisions` に追記
- `slug` は URL セーフ形式（英小文字・数字・ハイフン）に制限

適用例:

```sh
createdb isl_wiki
psql -d isl_wiki -f /Volumes/SSD-PLU3/work/LISP/islisp/isl/examples/wiki/db/001_init.sql
psql -d isl_wiki -f /Volumes/SSD-PLU3/work/LISP/islisp/isl/examples/wiki/db/002_media_assets.sql
```

接続文字列は環境変数 `ISL_WIKI_DB_URL` で指定できます。
未指定時は `postgresql://127.0.0.1:5432/isl_wiki` を使います。

例:

```sh
export ISL_WIKI_DB_URL='postgresql://USER:PASSWORD@127.0.0.1:5432/isl_wiki'
export ISL_WIKI_MEDIA_BASE_URL='/wiki/files'   # 任意。未指定時は自動で「現在のベースURL/files」
```

## Apache 設定

`conf/httpd-wiki.conf` の `ScriptAliasMatch` を include し、`mod_cgi` を有効化してください。  
メディアURLはデフォルトで現在のベースURLに追従し（`/wiki/files/...` または `/cgi-bin/wiki.cgi/files/...`）、CGI経由で配信されます。

```apache
Include "/Volumes/SSD-PLU3/work/LISP/islisp/isl/examples/wiki/conf/httpd-wiki.conf"
```

## 現在の動作確認

```text
http://localhost:8080/wiki
http://localhost:8080/wiki/home
http://localhost:8080/wiki/home/edit
http://localhost:8080/wiki/new
http://localhost:8080/wiki/media
http://localhost:8080/wiki/media/new
```

保存（POST）確認例:

```sh
curl -i -X POST "http://localhost:8080/wiki/home/edit" \
  -H "Content-Type: application/x-www-form-urlencoded" \
  --data-urlencode "title=Home" \
  --data-urlencode "body_md=# Updated from curl" \
  --data-urlencode "edit_summary=curl test"
```

メディア追加（サーバー上のファイルを登録）例:

```sh
curl -i -X POST "http://localhost:8080/wiki/media/new" \
  -H "Content-Type: application/x-www-form-urlencoded" \
  --data-urlencode "source_path=/tmp/sample.png" \
  --data-urlencode "title=Sample Image" \
  --data-urlencode "page_slug=home" \
  --data-urlencode "media_type=image" \
  --data-urlencode "mime_type=image/png" \
  --data-urlencode "edit_summary=add image"
```
