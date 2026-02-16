# Markdown変換方式（決定）

最終更新: 2026-02-17

## 採用方式

Wiki の Markdown 変換は **外部コマンド `examples/md2html/md2html`** を採用する。

- 入力: `pages.body_md`（Markdownテキスト）
- 出力: HTMLフラグメント（`<body>` を含まない）
- 実行方式: `ISL` から `system` で `md2html` を呼び出す
- I/O: 一時ファイル経由（`ISL` の `system` は終了コードのみ返すため）

## 採用理由

1. 同一リポジトリ内で開発している `md2html` をそのまま再利用できる。
2. Apache CGI 実行時にも外部依存（pandocインストール）が不要になる。
3. 既存実装（CGI + PostgreSQL）へ最小変更で組み込みやすい。
4. 変換ロジックを自前管理できるため、wiki要件に合わせて拡張しやすい。

## セキュリティ方針（MVP）

MVPでは、wiki側で必要な Markdown 記法を `md2html` の対応範囲に合わせて運用する。

```sh
/Volumes/SSD-PLU3/work/LISP/islisp/isl/examples/md2html/md2html -o /tmp/out.html /tmp/in.md
```

- 変換前に wiki 内リンク記法（`[[slug]]`, `[[slug|Title]]`）を通常 Markdown リンクへ展開。
- これに加え、将来段階で HTML サニタイズ（許可タグ方式）を追加する。

## 変換インターフェース（実装予定）

`wiki.lsp` に次の責務を持つ関数を置く。

- `markdown->html`:
  - Markdown文字列を受け取る
  - temp `.md` に書き出し
  - `md2html` 実行で temp `.html` を生成
  - `.html` を読み込んで返す
  - 失敗時はエラーを返し、呼び出し側で 500 表示

## 前提条件

- `/Volumes/SSD-PLU3/work/LISP/islisp/isl/examples/md2html/md2html` が実行可能であること。
- 必要に応じて `ISL_WIKI_MD2HTML` で実行パスを指定できること。
- CGI 実行ユーザーが temp ディレクトリへ書き込み可能であること。

## 非採用案

- 純ISL実装: 実装工数が高く、MVP速度を下げるため不採用。
- `pandoc` 継続利用: 外部依存を増やすため不採用。

## 次ステップ

1. HTMLサニタイズ層を追加。
2. 変換失敗時の監視ログ整備（request id, slug, command status）。
3. 変換結果キャッシュ（`updated_at` ベース）を追加。

## 実装状況（2026-02-15）

縦スライス1本目として、`/wiki/{slug}` は次の流れを実装済み。

1. PostgreSQL (`pages.body_md`) から本文取得。
2. `markdown->html` で `md2html` 変換。
3. HTMLプレビューとして表示（併せて Markdown Source も表示）。
4. `/wiki/{slug}/edit` の POST 保存で `pages` 更新 + `page_revisions` 追記。
