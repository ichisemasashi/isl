# md2html (Phase 1)

`md2html` は Markdown を HTML に変換して標準出力へ出すツールです。  
この時点では **フェーズ1 Core Markdown（拡張なし）** まで実装しています。

## 使い方

```sh
/Volumes/SSD-PLU3/work/LISP/islisp/isl/examples/md2html/md2html input.md
cat input.md | /Volumes/SSD-PLU3/work/LISP/islisp/isl/examples/md2html/md2html
```

## 実装済み基盤

- 行Lexer: blank / heading / paragraph
- Inline Lexer: text / `*` / `**` / `` ` `` / `[]()` 記号
- Block AST:
  - `block-heading`
  - `block-paragraph`
- Inline AST:
  - `inline-text`
  - `inline-code`
  - `inline-emph`
  - `inline-strong`
  - `inline-link`
- HTML renderer
- 位置情報:
  - `line`, `col`, `offset`
- エラー体系:
  - `md-fail` / `md-error`

## 実装済み Markdown（Core）

- 段落
- 見出し
  - ATX (`#`)
  - Setext (`===` / `---`)
- 強調 (`*em*`, `**strong**`)
- コード（インライン `` `code` ``）
- リンク (`[text](url)`)
- 画像 (`![alt](url)`)
- 引用 (`>`)
- 水平線 (`---`, `***`, `___`)
- HTMLエンティティ保持（例: `&amp;`, `&#169;`）

## ディレクトリ構成

- `app/main.lsp`: エントリポイント
- `lib/errors.lsp`: エラー/位置情報
- `lib/ast.lsp`: AST定義
- `lib/escape.lsp`: HTMLエスケープ
- `lib/lexer.lsp`: 行・インラインLexer
- `lib/parser.lsp`: 基本パーサ
- `lib/renderer.lsp`: HTMLレンダラ
- `md2html`: CLIラッパー
