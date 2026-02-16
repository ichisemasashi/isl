# webserver (ISL)

`examples/webserver/app/ctl.lsp` に運用コマンドを実装しています。

## 前提

- 実装言語: ISL
- ポートは固定 `8080` ではなく、設定ファイルの `listen_port` を使用します。
- 既定設定ファイル: `examples/webserver/conf/webserver.conf.lsp`

## 共通

```sh
WEBSERVER_ROOT=/Volumes/SSD-PLU3/work/LISP/islisp/isl/examples/webserver \
WEBSERVER_CONFIG=/Volumes/SSD-PLU3/work/LISP/islisp/isl/examples/webserver/conf/webserver.conf.lsp
```

## 起動コマンド

デーモン起動（既定）:

```sh
WEBSERVER_CMD=start \
WEBSERVER_TRANSPORT=http \
WEBSERVER_PID_FILE=/tmp/webserver.pid \
WEBSERVER_SERVER_LOG=/tmp/webserver.log \
./bin/isl examples/webserver/app/ctl.lsp
```

HTTPS 起動:

```sh
WEBSERVER_CMD=start \
WEBSERVER_TRANSPORT=https \
./bin/isl examples/webserver/app/ctl.lsp
```

前面起動（デバッグ）:

```sh
WEBSERVER_CMD=start \
WEBSERVER_DAEMON=0 \
WEBSERVER_TRANSPORT=http \
./bin/isl examples/webserver/app/ctl.lsp
```

## 停止コマンド

```sh
WEBSERVER_CMD=stop \
WEBSERVER_PID_FILE=/tmp/webserver.pid \
./bin/isl examples/webserver/app/ctl.lsp
```

## 設定検証コマンド（`-t` 相当）

```sh
WEBSERVER_CMD=-t \
./bin/isl examples/webserver/app/ctl.lsp
```

`WEBSERVER_CMD=test` / `WEBSERVER_CMD=check` でも同じ動作です。

## ログ確認コマンド

```sh
WEBSERVER_CMD=logs \
WEBSERVER_LOG_KIND=both \
WEBSERVER_TAIL_LINES=50 \
WEBSERVER_SERVER_LOG=/tmp/webserver.log \
WEBSERVER_ACCESS_LOG=/tmp/webserver-access.log \
WEBSERVER_ERROR_LOG=/tmp/webserver-error.log \
./bin/isl examples/webserver/app/ctl.lsp
```

`WEBSERVER_LOG_KIND` は `server` / `access` / `error` / `both` を指定できます。

## 主要環境変数

- `WEBSERVER_CMD`: `start` / `stop` / `-t` (`test`, `check`) / `logs`
- `WEBSERVER_CONFIG`: 設定ファイルパス
- `WEBSERVER_TRANSPORT`: `http` or `https`
- `WEBSERVER_PID_FILE`: PIDファイル（既定 `/tmp/webserver.pid`）
- `WEBSERVER_SERVER_LOG`: サーバーログ（既定 `/tmp/webserver.log`）
- `WEBSERVER_ACCESS_LOG`: アクセスログ（既定 `/tmp/webserver-access.log`）
- `WEBSERVER_ERROR_LOG`: エラーログ（既定 `/tmp/webserver-error.log`）
- `WEBSERVER_DAEMON`: `1`（既定, 背景起動） / `0`（前面起動）
- `WEBSERVER_TAIL_LINES`: ログ表示行数（既定 `50`）
