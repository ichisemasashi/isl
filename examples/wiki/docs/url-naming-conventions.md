# URL / 命名規約（固定）

最終更新: 2026-02-15

## URL規約

ベースURLは `/wiki` とする。

- 一覧: `/wiki`
- 表示: `/wiki/{slug}`
- 編集: `/wiki/{slug}/edit`

クエリ文字列はMVPでは未使用。

## Canonical URL規約

- 末尾スラッシュなしを正規形とする。
  - 例: `/wiki/home/` -> `/wiki/home` (301)
- `/wiki` はそのまま利用（`/wiki/` への自動統一は行わない）。

## slug 命名規約

`slug` は以下を満たす。

- 文字種: `a-z`, `0-9`, `-`
- 長さ: `1..128`
- 先頭と末尾に `-` は不可

正規表現:

```text
^[a-z0-9](?:[a-z0-9-]{0,126}[a-z0-9])?$
```

## title 命名規約

- 空文字・空白のみは禁止
- 表示名として扱い、URLには使わない

## DB命名規約

- テーブル/カラムは `snake_case`
- 履歴テーブルは複数形 + `_revisions`
- 主キーは `id`
- 外部キーは `<table_singular>_id`

## 実装への反映

- `pages.slug` に DB 制約を設定済み（`db/001_init.sql`）
- `wiki.lsp` で `slug-safe-p` によるルーティング前バリデーションを実施
- `wiki.lsp` で末尾スラッシュURLを canonical URL へ 301 リダイレクト

## 今後の拡張時の規約

- 新規画面は `/wiki/{slug}/...` の階層に追加
- 動詞は末尾セグメントに置く（例: `/edit`, `/history`）
- URLに表示名(title)は含めない（slugのみ）
