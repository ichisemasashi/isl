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

## ファイル構成

- `app/echospace.lsp`: Web アプリ本体
- `cgi-bin/echospace.cgi`: CGI エントリ
- `tests/t1-echospace-unit.lsp`: 単体テスト
- `REQUIREMENTS.md`: 要件整理

`examples/webserver/runtime/cgi-bin/echospace.cgi` からも同じ本体を呼び出せます。

## 初期アカウント

- `admin / admin`
- `demo / demo`

管理者パスワードは `ECHOSPACE_INITIAL_ADMIN_PASSWORD` で上書きできます。

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

起動:

```sh
WEBSERVER_CMD=start \
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

## 主要環境変数

- `ISL_ECHOSPACE_DB_ROOT`: DBMS ストレージルート
- `ECHOSPACE_INITIAL_ADMIN_PASSWORD`: 初期 admin パスワード
- `ISL_ECHOSPACE_POLL_INTERVAL_SEC`: クライアントのポーリング間隔秒
- `ISL_ECHOSPACE_LOG_DIR`: アプリログ出力先

## テスト

```sh
./bin/isl examples/echospace/tests/t1-echospace-unit.lsp
```
