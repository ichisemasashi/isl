# md2html Phase 0 基盤設計

## 1. Lexer

- 行Lexer (`lib/lexer.lsp`)
  - `lex-lines`: 入力全体を行トークンへ変換
  - 行種別:
    - `blank`
    - `heading` (`#` 1-6 + 空白)
    - `paragraph`
- Inline Lexer (`lib/lexer.lsp`)
  - `lex-inline`: 行本文をインライントークン列へ変換
  - トークン種別:
    - `text`
    - `star` / `dstar`
    - `backtick`
    - `lbrack` / `rbrack`
    - `lparen` / `rparen`

## 2. AST

- Block AST (`lib/ast.lsp`)
  - `block-heading`
  - `block-paragraph`
- Inline AST (`lib/ast.lsp`)
  - `inline-text`
  - `inline-code`
  - `inline-emph`
  - `inline-strong`
  - `inline-link`

## 3. HTML Renderer

- `lib/renderer.lsp`
  - Inline: text/code/emph/strong/link
  - Block: heading/paragraph
  - `render-html-document` は HTML フラグメントを出力

## 4. エスケープ/位置情報/エラー体系

- エスケープ (`lib/escape.lsp`)
  - `html-escape`
  - `attr-escape`
- 位置情報 (`lib/errors.lsp`)
  - `md-pos` = `(line col offset)`
- エラー体系 (`lib/errors.lsp`)
  - `md-error`
  - `md-fail`（`error` を統一フォーマットで送出）

## 5. 実行パス

- CLIラッパー: `md2html`（ファイル入力 or stdin）
- エントリ: `app/main.lsp`
- パイプライン:
  1. 入力読込
  2. `parse-markdown`（`lex-lines` + `lex-inline` + AST構築）
  3. `render-html-document`
  4. stdout出力
