# echospace (ISL)

`examples/echospace` は、Discord ライクな構成を持つチャット Web アプリです。

現状の MVP:

- ログイン / ログアウト
- ワークスペース一覧
- チャンネル一覧
- メッセージ表示 / 投稿
- メンバー一覧
- ポーリングによる新着反映
- 管理画面からのユーザー / ワークスペース / チャンネル / メンバーシップ追加
- 音声チャンネルの作成、参加 / 離脱、参加者一覧表示

## ファイル構成

- `app/echospace.lsp`: Web アプリ本体
- `cgi-bin/echospace.cgi`: CGI エントリ
- `tests/t1-echospace-unit.lsp`: 単体テスト
- `REQUIREMENTS.md`: 要件整理
- `VOICE_VIDEO_REQUIREMENTS.md`: 音声・動画チャット要件

`examples/webserver/runtime/cgi-bin/echospace.cgi` からも同じ本体を呼び出せます。

## 初期アカウント

- `admin / admin`
- `demo / demo`

管理者パスワードは `ECHOSPACE_INITIAL_ADMIN_PASSWORD` で上書きできます。

## 初回セットアップ

最小セットアップ:

1. リポジトリルートに移動する
2. `ISL_ROOT` と `WEBSERVER_ROOT` を設定する
3. `ISL_ECHOSPACE_DB_ROOT` を設定する
4. `WEBSERVER_CMD=-t` で設定確認する
5. `WEBSERVER_CMD=start` で起動する
6. `http://localhost:18080/cgi-bin/echospace.cgi/login` にアクセスする

初回セットアップ例:

```sh
export ISL_ROOT="$(pwd)"
export WEBSERVER_ROOT="$ISL_ROOT/examples/webserver"
export WEBSERVER_CONFIG="$WEBSERVER_ROOT/conf/webserver.conf.lsp"
export ISL_ECHOSPACE_DB_ROOT="./examples/dbms/storage/echospace"
export ECHOSPACE_INITIAL_ADMIN_PASSWORD="admin"

WEBSERVER_CMD=-t \
./bin/isl examples/webserver/app/ctl.lsp

WEBSERVER_CMD=start \
WEBSERVER_TRANSPORT=http \
WEBSERVER_PID_FILE=/tmp/webserver.pid \
WEBSERVER_SERVER_LOG=/tmp/webserver.log \
./bin/isl examples/webserver/app/ctl.lsp
```

補足:

- `ISL_ECHOSPACE_DB_ROOT` 配下には初回アクセス時にアプリ用データが作られます
- 初回ログインは `admin / admin` または `demo / demo` を使えます
- 既存の `admin` パスワードを固定したい場合は、初回起動前に `ECHOSPACE_INITIAL_ADMIN_PASSWORD` を設定してください

## 起動方法

前提:

- リポジトリルートで実行する
- `ISL_ROOT` を設定する

```sh
export ISL_ROOT="$(pwd)"
export WEBSERVER_ROOT="$ISL_ROOT/examples/webserver"
export WEBSERVER_CONFIG="$WEBSERVER_ROOT/conf/webserver.conf.lsp"
export ISL_ECHOSPACE_DB_ROOT="./examples/dbms/storage/echospace"
```

設定確認:

```sh
WEBSERVER_CMD=-t \
./bin/isl examples/webserver/app/ctl.lsp
```

起動:

```sh
WEBSERVER_CMD=start \
WEBSERVER_TRANSPORT=http \
WEBSERVER_PID_FILE=/tmp/webserver.pid \
WEBSERVER_SERVER_LOG=/tmp/webserver.log \
./bin/isl examples/webserver/app/ctl.lsp
```

前面起動:

```sh
WEBSERVER_CMD=start \
WEBSERVER_DAEMON=0 \
WEBSERVER_TRANSPORT=http \
WEBSERVER_PID_FILE=/tmp/webserver.pid \
WEBSERVER_SERVER_LOG=/tmp/webserver.log \
./bin/isl examples/webserver/app/ctl.lsp
```

アクセス:

```text
http://localhost:18080/cgi-bin/echospace.cgi
http://localhost:18080/cgi-bin/echospace.cgi/login
http://localhost:18080/cgi-bin/echospace.cgi/healthz
http://localhost:18080/cgi-bin/echospace.cgi/admin
```

ログ確認:

```sh
WEBSERVER_CMD=logs \
WEBSERVER_LOG_KIND=both \
WEBSERVER_TAIL_LINES=50 \
WEBSERVER_SERVER_LOG=/tmp/webserver.log \
WEBSERVER_ACCESS_LOG=/tmp/webserver-access.log \
WEBSERVER_ERROR_LOG=/tmp/webserver-error.log \
./bin/isl examples/webserver/app/ctl.lsp
```

## 停止方法

通常停止:

```sh
WEBSERVER_CMD=stop \
WEBSERVER_PID_FILE=/tmp/webserver.pid \
./bin/isl examples/webserver/app/ctl.lsp
```

再起動:

```sh
WEBSERVER_CMD=restart \
WEBSERVER_TRANSPORT=http \
WEBSERVER_PID_FILE=/tmp/webserver.pid \
WEBSERVER_SERVER_LOG=/tmp/webserver.log \
./bin/isl examples/webserver/app/ctl.lsp
```

`stop` が失敗した場合の強制停止:

```sh
kill -TERM "$(cat /tmp/webserver.pid)" 2>/dev/null || true
rm -f /tmp/webserver.pid
```

## 主要環境変数

- `ISL_ECHOSPACE_DB_ROOT`: DBMS ストレージルート
- `ECHOSPACE_INITIAL_ADMIN_PASSWORD`: 初期 admin パスワード
- `ISL_ECHOSPACE_POLL_INTERVAL_SEC`: クライアントのポーリング間隔秒
- `ISL_ECHOSPACE_LOG_DIR`: アプリログ出力先

## テスト

```sh
./bin/isl examples/echospace/tests/t1-echospace-unit.lsp
```

## トラブルシュート

`cgi timeout` が表示される:

- 古い `webserver` プロセスが残っていることがあります
- `WEBSERVER_CMD=stop` で停止してから再起動してください
- まだ直らない場合は `WEBSERVER_CMD=logs` で `/tmp/webserver-error.log` を確認してください

`database unavailable` が表示される:

- `ISL_ECHOSPACE_DB_ROOT` が未設定、または不正なパスの可能性があります
- README の環境変数例を再設定してから再起動してください
- `http://localhost:18080/cgi-bin/echospace.cgi/healthz` もあわせて確認してください

ログインできない:

- 初期アカウントは `admin / admin` と `demo / demo` です
- `ECHOSPACE_INITIAL_ADMIN_PASSWORD` を変更した場合は、その値で `admin` ログインしてください
- 以前の DB が残っていると、新しい環境変数より既存データが優先されます

変更を入れたのに反映されない:

- `echospace` は `webserver` に埋め込みロードされます
- Lisp ファイルを更新した後は `WEBSERVER_CMD=restart` で再起動してください

ポート `18080` で起動できない:

- 別プロセスが同じポートを使っている可能性があります
- 既存の `webserver` を停止するか、設定ファイルの `listen_port` を変更してください
