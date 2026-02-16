# md2html (Phase 0)

`md2html` は Markdown を HTML に変換して標準出力へ出すツールです。  
この時点では **フェーズ0基盤**（Lexer/AST/Renderer/エラー体系）を実装しています。

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

## ディレクトリ構成

- `app/main.lsp`: エントリポイント
- `lib/errors.lsp`: エラー/位置情報
- `lib/ast.lsp`: AST定義
- `lib/escape.lsp`: HTMLエスケープ
- `lib/lexer.lsp`: 行・インラインLexer
- `lib/parser.lsp`: 基本パーサ
- `lib/renderer.lsp`: HTMLレンダラ
- `md2html`: CLIラッパー
