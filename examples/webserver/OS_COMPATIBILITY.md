# webserver OS互換性対応 (T7)

この文書は `examples/webserver/IMPLEMENTATION_TASKS.md` の
`T7. OS互換性対応（Linux/FreeBSD）` 実装内容をまとめる。

## 1. 方針

- ターゲットOSは `Linux` / `FreeBSD`。
- OS差分が出やすい操作（シェルコマンド依存）を最小化し、POSIX互換コマンドに限定する。
- 非ターゲットOS（例: Darwin）でも開発検証は可能とし、起動時に warning を出す。

## 2. 実装内容

- 起動時に `uname -s` で OS を判定し、`os_family` として内部設定へ保持:
  - `linux`, `freebsd`, `darwin`, `unknown`
- 起動時に必要コマンドの存在を検証:
  - `sh`, `test`, `dirname`, `basename`, `uname`
- 依存コマンドは Linux/FreeBSD 共通で使える POSIX 範囲のみ使用:
  - パス検証: `test -d`, `test -f`, `test -r`, `test -x`, `test -L`
  - CGI実行: `sh` / リダイレクト
- ソケット層は ISL プリミティブ (`tcp-*`, `tls-*`) 経由で共通化。

## 3. 運用ログ

起動時ログに `os_family` を出力する。

- ターゲットOS: 通常起動
- 非ターゲットOS: 以下 warning を出力して継続
  - `warning: running on non-target os (...), target is Linux/FreeBSD`

## 4. 手動検証手順 (Linux/FreeBSD)

1. 設定検証:
   - `./bin/isl examples/webserver/tests/t1-config-validation.lsp`
2. HTTP/HTTPS/静的配信/CGI/接続上限制御:
   - `./bin/isl examples/webserver/tests/t2-http-integration.lsp`
   - `./bin/isl examples/webserver/tests/t3-https-integration.lsp`
   - `./bin/isl examples/webserver/tests/t4-static-integration.lsp`
   - `./bin/isl examples/webserver/tests/t5-cgi-integration.lsp`
   - `./bin/isl examples/webserver/tests/t6-connection-limit-integration.lsp`
3. OS互換性検証:
   - `./bin/isl examples/webserver/tests/t7-os-compat-validation.lsp`

