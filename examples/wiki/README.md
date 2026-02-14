# wiki (Apache CGI + ISL)

このディレクトリは、`Apache(httpd)` から `ISL` スクリプトを CGI で実行し、
まずは固定ページを返す最小構成です。

## ファイル構成
- `app/wiki.lsp`: 固定HTMLを出力する ISL 本体
- `cgi-bin/wiki.cgi`: Apache から呼ばれる CGI エントリ
- `conf/httpd-wiki.conf`: httpd に include する設定例

## 1. Apache 設定
メインの `httpd.conf` に次を追加してください。

```apache
Include "/Volumes/SSD-PLU3/work/LISP/islisp/isl/examples/wiki/conf/httpd-wiki.conf"
```

`mod_cgi` が無効なら有効化してください。

## 2. Apache 再起動
環境に合わせて再起動します。

```sh
apachectl -k restart
```

## 3. アクセス

```text
http://localhost/wiki
```

固定ページ `ISL Wiki (fixed page)` が表示されれば成功です。

## PostgreSQL 前提について
現時点では DB 接続はまだ使っていません。次の段階で `postgres-open` / `postgres-query` を使って記事一覧を表示できます。
