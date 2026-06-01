# webserver (ISL)

`examples/webserver/app/ctl.lsp` に運用コマンドを実装しています。

## 前提

- 実装言語: ISL
- ポートは設定ファイルの `listen_port` を使用します（既定: HTTPS は `18443`、HTTP は `18080`）
- 既定設定ファイル: `examples/webserver/conf/webserver.conf.lsp`（HTTPS、ポート 18443）
- HTTP 専用設定ファイル: `examples/webserver/conf/webserver-http.conf.lsp`（ポート 18080）
- TLS 証明書・秘密鍵: `examples/webserver/runtime/tls/server.crt` / `server.key`
  - 自己署名証明書（CN=localhost）。有効期限は 2027-02-16 まで。

## TLS 証明書の再生成

```sh
openssl req -x509 -newkey rsa:2048 -keyout examples/webserver/runtime/tls/server.key \
  -out examples/webserver/runtime/tls/server.crt \
  -days 365 -nodes -subj "/CN=localhost"
```

## 共通

リポジトリのルート (`isl/`) で実行する前提です。先に環境変数を設定してください。

```sh
export WEBSERVER_ROOT="$(pwd)/examples/webserver"
export WEBSERVER_CONFIG="$WEBSERVER_ROOT/conf/webserver.conf.lsp"
```

## 起動コマンド

HTTPS デーモン起動（既定）:

```sh
WEBSERVER_CMD=start \
./bin/isl examples/webserver/app/ctl.lsp
```

HTTPS 前面起動（デバッグ）:

```sh
WEBSERVER_CMD=start \
WEBSERVER_DAEMON=0 \
./bin/isl examples/webserver/app/ctl.lsp
```

HTTP デーモン起動（TLS なし）:

```sh
WEBSERVER_CMD=start \
WEBSERVER_CONFIG="$(pwd)/examples/webserver/conf/webserver-http.conf.lsp" \
./bin/isl examples/webserver/app/ctl.lsp
```

## 停止コマンド

通常停止:

```sh
WEBSERVER_CMD=stop \
WEBSERVER_PID_FILE=/tmp/webserver.pid \
./bin/isl examples/webserver/app/ctl.lsp
```

PIDファイル不整合などで通常停止できない場合（復旧手順）:

```sh
kill -TERM "$(cat /tmp/webserver.pid)" 2>/dev/null || true
rm -f /tmp/webserver.pid
```

`WEBSERVER_CMD=stop` / `WEBSERVER_CMD=restart` が失敗した場合のみ使用してください。

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
