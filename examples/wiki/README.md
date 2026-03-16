# wiki (Apache CGI + ISL)

このディレクトリは、`Apache(httpd)` から `ISL` スクリプトを CGI で実行し、
Wiki システムを段階的に構築するための実装です。

## ファイル構成
- `app/wiki.lsp`: Webアプリ本体（MVP 3画面）
- `app/multipart_extract.pl`: `multipart/form-data` の単一ファイル抽出ヘルパ
- `cgi-bin/wiki.cgi`: Apache から呼ばれる CGI エントリ
- `conf/httpd-wiki.conf`: httpd に include する設定例
- `db/001_init.sql`: PostgreSQL 初期スキーマ
- `docs/markdown-policy.md`: Markdown変換方式の決定記録
- `docs/url-naming-conventions.md`: URL / 命名規約の固定

## MVP 3画面
- `/wiki` : ページ一覧
- `/wiki/{slug}` : ページ表示（DBの Markdown を HTML 変換して表示）
- `/wiki/{slug}/edit` : 編集画面（POSTで保存可能）
- `/wiki/{slug}/history` : 編集履歴一覧
- `/wiki/{slug}/revisions/{rev_no}` : 過去版表示
- `/wiki/{slug}/compare/{left_rev_no}/{right_rev_no}` : 2リビジョン比較
- `/wiki/new` : 新規ページ作成（POSTで保存可能）
- `/wiki/media` : メディア一覧（任意ファイル。画像/動画/音声は埋め込み表示）
- `/wiki/media/new` : メディア追加（POSTで保存）
- `/wiki/search?q=...` : 文書検索（title / body_md の部分一致）
  - 検索結果からページへ遷移すると、`q` の一致箇所をハイライト表示
- `/wiki/admin` : 管理メニュー
- `/wiki/admin/backup` : DB + メディアのバックアップ作成
- `/wiki/admin/restore` : バックアップからリストア
- `/wiki/login` : ログイン画面
- `/wiki/logout` : ログアウト

認可:
- `viewer`: 閲覧のみ
- `editor`: ページ作成 / 編集 / メディア追加
- `admin`: editor権限に加えて管理画面 / backup / restore

`wiki.lsp` は `PATH_INFO` でルーティングします。

## URL / 命名規約（固定）

`docs/url-naming-conventions.md` に固定済みです。
要点:
- Canonical URL は末尾スラッシュなし
- `slug` は `^[a-z0-9](?:[a-z0-9-]{0,126}[a-z0-9])?$`
- URL は `title` ではなく `slug` を使用

## Markdown変換方式（決定）

`examples/md2html/md2html` を外部コマンドとして利用します。
詳細は `docs/markdown-policy.md` を参照してください。

要点:
- 変換コマンド: `examples/md2html/md2html -o <tmp>.html <tmp>.md`
- `ISL` から `system` 経由で実行
- 入出力は一時ファイル経由
- Wiki内リンク記法を前処理:
  - `[[home]]` -> `/wiki/home` へのリンク
  - `[[home|Home Page]]` -> 表示名つきリンク
 - 必要に応じて `ISL_WIKI_MD2HTML` で実行バイナリを上書き可能

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
psql -d isl_wiki -f /Volumes/SSD-PLU3/work/LISP/islisp/isl/examples/wiki/db/003_media_assets_allow_file_type.sql
psql -d isl_wiki -f /Volumes/SSD-PLU3/work/LISP/islisp/isl/examples/wiki/db/004_auth.sql
psql -d isl_wiki -f /Volumes/SSD-PLU3/work/LISP/islisp/isl/examples/wiki/db/005_session_csrf.sql
psql -d isl_wiki -f /Volumes/SSD-PLU3/work/LISP/islisp/isl/examples/wiki/db/006_audit_logs.sql
psql -d isl_wiki -f /Volumes/SSD-PLU3/work/LISP/islisp/isl/examples/wiki/db/007_page_lock_version.sql
psql -d isl_wiki -f /Volumes/SSD-PLU3/work/LISP/islisp/isl/examples/wiki/db/008_soft_delete.sql
```

`004_auth.sql` は次を作成します。
- `roles`
- `users`
- `user_sessions`

`005_session_csrf.sql` は `user_sessions.csrf_token` を追加します。
`006_audit_logs.sql` は `audit_logs` を追加します。
`007_page_lock_version.sql` は `pages.current_rev_no` を追加します。
`008_soft_delete.sql` は `pages` / `media_assets` に論理削除カラムを追加します。

初期管理者:
- username: `admin`
- password: `admin`

初回ログイン後に必ず変更してください。

接続文字列は環境変数 `ISL_WIKI_DB_URL` で指定できます。
未指定時は `postgresql://127.0.0.1:5432/isl_wiki` を使います。

例:

```sh
export ISL_WIKI_DB_URL='postgresql://USER:PASSWORD@127.0.0.1:5432/isl_wiki'
export ISL_WIKI_MEDIA_BASE_URL='/wiki/files'   # 任意。未指定時は自動で「現在のベースURL/files」
export ISL_WIKI_BACKUP_DIR='/tmp/isl-wiki-backups'
```

アップロード上限は `conf/wiki.conf` の `media_max_bytes` で設定します。
アプリログ出力先は `conf/wiki.conf` の `log_dir` で設定します。

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
http://localhost:8080/wiki/home/history
http://localhost:8080/wiki/home/revisions/1
http://localhost:8080/wiki/home/compare/1/3
http://localhost:8080/wiki/new
http://localhost:8080/wiki/media
http://localhost:8080/wiki/media/new
http://localhost:8080/wiki/search?q=welcome
http://localhost:8080/wiki/admin
http://localhost:8080/wiki/admin/backup
http://localhost:8080/wiki/admin/restore
http://localhost:8080/wiki/admin/audit
http://localhost:8080/wiki/admin/deleted-pages
http://localhost:8080/wiki/admin/deleted-media
http://localhost:8080/wiki/login
```

注意:
- `/wiki/new`, `/wiki/{slug}/edit`, `/wiki/media/new` は `editor` 以上が必要
- `/wiki/admin`, `/wiki/admin/backup`, `/wiki/admin/restore` は `admin` が必要
- `/wiki/{slug}/delete` は `editor` 以上、削除済みページ/メディアの復元は `admin` が必要
- POST系操作はログイン済みユーザーのみ実行可能
- 認証済みPOSTにはフォーム埋め込みの `csrf_token` が必須
- `/wiki/media/new` はブラウザの `multipart/form-data` アップロードを受け付ける
- 旧 `source_path` 登録は `admin` のみ利用可能
- 添付はサイズ上限 `media_max_bytes` を超えると拒否される
- 許可拡張子/MIME のみ登録でき、拡張子と実測MIMEが不一致なファイルは拒否される
- `html/js/svg/exe/sh` など危険拡張子は拒否される
- メディア配信は `X-Content-Type-Options: nosniff` を付与し、画像/動画/音声以外は `attachment` 配信される
- 全レスポンスに `X-Request-Id` が付き、エラー画面にも同じ request id を表示する
- アプリ独自ログは `log_dir/wiki-app.log` に JSON Lines 形式で出力される
- page create/update、media create、backup、restore は structured log にも記録される
- 500画面には内部例外の詳細を直接表示せず、詳細はサーバーログ確認前提にした
- `/wiki/admin/backup` は確認語 `RUN BACKUP`、`/wiki/admin/restore` は `RESTORE WIKI` が必要
- ページ作成/編集、メディア追加/削除、backup/restore は監査ログに記録される
- 監査ログは `/wiki/admin/audit` で最新100件を参照できる
- 編集画面は `current_rev_no` を保持し、競合時は保存せず比較画面を表示する
- 履歴一覧から各リビジョンの `vs latest` で最新版との差分を参照できる
- ロールバックは `/wiki/{slug}/revisions/{rev_no}` から `admin` のみ実行でき、新しい履歴として保存される
- 論理削除されたページは一覧/検索/通常表示から除外され、`/wiki/admin/deleted-pages` から復元できる
- 論理削除されたメディアは一覧/配信から除外され、`/wiki/admin/deleted-media` から復元できる

保存（POST）確認例:

注意: これらのPOSTはログイン済みセッションCookieと有効な `csrf_token` が必要です。ブラウザ経由のフォーム送信を想定しています。

```sh
curl -i -X POST "http://localhost:8080/wiki/home/edit" \
  -H "Content-Type: application/x-www-form-urlencoded" \
  --data-urlencode "title=Home" \
  --data-urlencode "body_md=# Updated from curl" \
  --data-urlencode "edit_summary=curl test"
```

メディア追加:

- 通常は `/wiki/media/new` のブラウザフォームからアップロードします
- 既存ページからは `Attach Media To This Page` で `page_slug` を引き継いで遷移できます
- `source_path` を使う旧モードは `admin` 専用です

管理者向け `source_path` 登録例:

```sh
curl -i -X POST "http://localhost:8080/wiki/media/new" \
  -H "Content-Type: application/x-www-form-urlencoded" \
  --data-urlencode "source_path=/tmp/sample.pdf" \
  --data-urlencode "title=Sample PDF" \
  --data-urlencode "page_slug=home" \
  --data-urlencode "media_type=file" \
  --data-urlencode "mime_type=application/pdf" \
  --data-urlencode "edit_summary=add file"
```
