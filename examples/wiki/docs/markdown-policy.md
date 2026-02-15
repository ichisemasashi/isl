# Markdown変換方式（決定）

最終更新: 2026-02-15

## 採用方式

Wiki の Markdown 変換は **外部コマンド `pandoc`** を採用する。

- 入力: `pages.body_md`（Markdownテキスト）
- 出力: HTMLフラグメント（`<body>` を含まない）
- 実行方式: `ISL` から `system` で `pandoc` を呼び出す
- I/O: 一時ファイル経由（`ISL` の `system` は終了コードのみ返すため）

## 採用理由

1. 現環境で `pandoc` が利用可能（`/opt/homebrew/bin/pandoc`）。
2. ISLISP処理系にMarkdownパーサは未実装で、純Lisp実装は初期コストが高い。
3. 既存実装（CGI + PostgreSQL）へ最小変更で組み込みやすい。
4. 後で `cmark` などへ差し替える際も、変換関数の内部だけで済む。

## セキュリティ方針（MVP）

MVPでは、入力Markdown中の生HTMLを無効化して変換する。

```sh
pandoc --from=gfm-raw_html --to=html5
```

- `gfm-raw_html` により raw HTML 埋め込みを無効化。
- これに加え、将来段階で HTML サニタイズ（許可タグ方式）を追加する。

## 変換インターフェース（実装予定）

`wiki.lsp` に次の責務を持つ関数を置く。

- `markdown->html`:
  - Markdown文字列を受け取る
  - temp `.md` に書き出し
  - `pandoc` 実行で temp `.html` を生成
  - `.html` を読み込んで返す
  - 失敗時はエラーを返し、呼び出し側で 500 表示

## 前提条件

- `pandoc` が PATH 上にあること。
- Homebrew Apache 実行時に `pandoc` 実行可能であること。
- CGI 実行ユーザーが temp ディレクトリへ書き込み可能であること。

## 非採用案

- 純ISL実装: 実装工数が高く、MVP速度を下げるため不採用。
- `pandoc` ライブラリ直接連携: FFI整備コストが高く、現時点では過剰。

## 次ステップ

1. 編集保存（POST）実装時に `page_revisions` へ追記。
2. HTMLサニタイズ層を追加。
3. 変換失敗時の監視ログ整備（request id, slug, command status）。

## 実装状況（2026-02-15）

縦スライス1本目として、`/wiki/{slug}` は次の流れを実装済み。

1. PostgreSQL (`pages.body_md`) から本文取得。
2. `markdown->html` で `pandoc` 変換。
3. HTMLプレビューとして表示（併せて Markdown Source も表示）。
