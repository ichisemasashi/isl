# wiki (Apache CGI + ISL)

このディレクトリは、`Apache(httpd)` から `ISL` スクリプトを CGI で実行し、
Wiki システムを段階的に構築するための実装です。

## ファイル構成
- `app/wiki.lsp`: Webアプリ本体（MVP 3画面）
- `app/multipart_extract.lsp`: `multipart/form-data` の単一ファイル抽出ヘルパ
- `cgi-bin/wiki.cgi`: Apache から呼ばれる CGI エントリ
- `conf/httpd-wiki.conf`: httpd に include する設定例
- `scripts/run-backup.lsp`: 定期バックアップ用の運用スクリプト
- `db/*.sql`: 旧 PostgreSQL 版のスキーマ資産
- `docs/markdown-policy.md`: Markdown変換方式の決定記録
- `docs/restore-runbook.md`: 復元手順書
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
- `/wiki/search?q=...` : 文書検索（`dbms` 上でのタイトル/本文検索 + タイトル優先）
  - 検索結果からページへ遷移すると、`q` の一致箇所をハイライト表示
- `/wiki/admin` : 管理メニュー
- `/wiki/admin/backup` : DB + メディアのバックアップ作成
- `/wiki/admin/restore` : バックアップからリストア
- `/wiki/admin/stats` : 管理用JSON統計
- `/wiki/healthz` : 監視用ヘルスチェック(JSON)
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
 - `cgi-bin/wiki.cgi` / `app/multipart_extract.lsp` / `scripts/run-backup.lsp` は ISL スクリプトとして実行される

## 起動方法

前提:
- リポジトリルートを `ISL_ROOT` に設定する
- `Apache httpd` で `mod_cgi` を有効にする
- `examples/md2html/md2html` を利用可能にしておく

最小設定:

```sh
cd /Volumes/SD_ONE/work/dev/isl
export ISL_ROOT=/Volumes/SD_ONE/work/dev/isl
export ISL_WIKI_DB_ROOT=./examples/dbms/storage/wiki
```

Apache 設定:

```apache
Include "/Volumes/SD_ONE/work/dev/isl/examples/wiki/conf/httpd-wiki.conf"
```

起動後の確認:

```text
http://localhost:8080/wiki
http://localhost:8080/wiki/healthz
```

補足:
- `dbms` のテーブル作成と初期データ投入は初回アクセス時に自動実行されます
- PostgreSQL 用の SQL を事前に流す必要はありません

## DBMS ストレージ

起動時に `dbms` ストレージ配下へ次を自動作成します。
- `pages`: 各ページの現在値（`slug`, `title`, `body_md`）
- `page_revisions`: 編集履歴（世代番号 `rev_no` つき）
- `media_assets`: メタ情報（実ファイルはストレージ保存）

設計方針:
- 読み取りは `pages` から単純に取得
- 履歴は `page_revisions` に追記
- `slug` は URL セーフ形式（英小文字・数字・ハイフン）に制限

設定例:

```sh
export ISL_WIKI_DB_ROOT='./examples/dbms/storage/wiki'
```

認証・監査・補助テーブルとして次も自動作成します。
- `roles`
- `users`
- `user_sessions`
- `audit_logs`
- `page_tags`
- `backup_runs`

初期管理者:
- username: `admin`
- password: `admin`

初回ログイン後に必ず変更してください。

ストレージルートは環境変数 `ISL_WIKI_DB_ROOT` で指定できます。
未指定時は `./examples/dbms/storage/wiki` を使います。

例:

```sh
export ISL_WIKI_DB_ROOT='./examples/dbms/storage/wiki'
export ISL_WIKI_MEDIA_BASE_URL='/wiki/files'   # 任意。未指定時は自動で「現在のベースURL/files」
export ISL_WIKI_BACKUP_DIR='/tmp/isl-wiki-backups'
```

アップロード上限は `conf/wiki.conf` の `media_max_bytes` で設定します。
アプリログ出力先は `conf/wiki.conf` の `log_dir` で設定します。
バックアップ保持数は `conf/wiki.conf` の `backup_keep_count` で設定します。

## Apache 設定

`conf/httpd-wiki.conf` の `ScriptAliasMatch` を include し、`mod_cgi` を有効化してください。  
メディアURLはデフォルトで現在のベースURLに追従し（`/wiki/files/...` または `/cgi-bin/wiki.cgi/files/...`）、CGI経由で配信されます。

```apache
Include "/Volumes/SD_ONE/work/dev/isl/examples/wiki/conf/httpd-wiki.conf"
```

必要なら `conf/httpd-wiki.conf` 内のパスも、このワークスペースに合わせて更新してください。

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
http://localhost:8080/wiki/admin/stats
http://localhost:8080/wiki/admin/audit
http://localhost:8080/wiki/admin/deleted-pages
http://localhost:8080/wiki/admin/deleted-media
http://localhost:8080/wiki/healthz
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
- 検索フォームは既存クエリを再表示する
- 検索はタイトル一致優先で並び、本文スニペットを表示する
- 検索は `dbms` 上でタイトル/本文の部分一致を利用し、`tag` と `status` で絞り込みできる
- ページ作成/編集画面で `status` と `tags` を編集できる
- 公開状態は `published=全員`, `draft=editor以上`, `private=adminのみ` で表示制御される
- 一覧と検索結果にはページの `status` が反映される
- `/wiki/admin/backup` は保存先、保持数、dry-run を指定でき、最近の履歴を表示する
- バックアップ/リストアの結果は `backup_runs` に履歴記録される
- 監視用に `/wiki/healthz`、運用統計用に `/wiki/admin/stats` を提供する
- 定期バックアップは `scripts/run-backup.lsp` を cron などから呼び出せる
- 復元手順と検証コマンドは `docs/restore-runbook.md` を参照
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
