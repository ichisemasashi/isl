# md2html (Phase 9)

`md2html` は Markdown を HTML に変換して標準出力へ出すツールです。  
この時点では **フェーズ9 互換・特殊機能** まで実装しています。

## 使い方

```sh
/Volumes/SSD-PLU3/work/LISP/islisp/isl/examples/md2html/md2html input.md
cat input.md | /Volumes/SSD-PLU3/work/LISP/islisp/isl/examples/md2html/md2html
/Volumes/SSD-PLU3/work/LISP/islisp/isl/examples/md2html/md2html -o output.html input.md
```

## エラー処理

- 入力ファイル不存在
  - `file not found` を表示して終了 (`exit 2`)
- 変換コマンド失敗
  - `conversion command failed` を表示して非0終了
- 出力先書き込み不可 (`-o` 指定時)
  - `output file is not writable` / `output directory is not writable` で終了 (`exit 2`)
- 文字コード不正
  - 入力は UTF-8 として検証し、不正時 `invalid UTF-8 input` で終了 (`exit 2`)

## テスト

```sh
/Volumes/SSD-PLU3/work/LISP/islisp/isl/examples/md2html/tests/run.sh
```

- 固定テストケース:
  - `phase2_nested_and_multi_paragraph_list`
  - `phase2_definition_multi_block`
  - `phase2_combined`
  - `phase3_code_blocks`
  - `phase3_blank_before_and_space_in_atx`
  - `phase3_line_blocks`
  - `phase3_escaped_and_hard_breaks`
  - `phase3_hard_line_breaks_mode`
  - `phase3_ignore_line_breaks_mode`
  - `phase3_east_asian_line_breaks_mode`
  - `phase4_pipe_tables`
  - `phase4_simple_tables`
  - `phase4_multiline_tables`
  - `phase4_grid_tables`
  - `phase4_table_captions`
  - `phase4_table_attributes`
  - `phase5_auto_identifiers`
  - `phase5_ascii_identifiers`
  - `phase5_gfm_auto_identifiers`
  - `phase5_mmd_header_identifiers`
  - `phase5_implicit_header_references`
  - `phase5_header_attributes`
  - `phase5_link_attributes`
  - `phase5_inline_code_attributes`
  - `phase5_bracketed_spans_and_native_spans`
  - `phase5_fenced_and_native_divs`
  - `phase5_mmd_link_attributes`
  - `phase5_markdown_in_html_blocks`
  - `phase6_smart_and_dashes`
  - `phase6_old_dashes`
  - `phase6_inline_extensions`
  - `phase6_abbreviations_and_refs`
  - `phase6_autolink_and_escapable`
  - `phase6_spaced_reference_links`
  - `phase7_tex_math_dollars`
  - `phase7_tex_math_single_backslash`
  - `phase7_tex_math_double_backslash`
  - `phase7_raw_html`
  - `phase7_raw_tex`
  - `phase7_latex_macros`
  - `phase8_yaml_metadata_block`
  - `phase8_pandoc_title_block`
  - `phase8_mmd_title_block`
  - `phase8_citations`
  - `phase8_inline_notes`
  - `phase8_footnotes`
  - `phase9_wikilinks_title_after_pipe`
  - `phase9_wikilinks_title_before_pipe`
  - `phase9_rebase_relative_paths`
  - `phase9_literate_haskell`
  - `phase9_gutenberg`

## 実装済み基盤

- 行Lexer: blank / heading / paragraph
- Inline Lexer: text / `*` / `**` / `` ` `` / `[]()` 記号
- Block AST:
  - `block-heading`
  - `block-paragraph`
  - `block-blockquote`
  - `block-hr`
  - `block-list`
  - `block-deflist`
- Inline AST:
  - `inline-text`
  - `inline-code`
  - `inline-emph`
  - `inline-strong`
  - `inline-link`
  - `inline-image`
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

## 実装済み Markdown（List 拡張）

- `startnum`
  - 例: `3. item` -> `<ol start="3">`
- `fancy_lists`
  - 例: `a)`, `A)`, `i)`, `I)` の順序付きリスト
- `example_lists`
  - 例: `(@) item`
- `definition_lists`
  - 例:
    - `Term`
    - `: definition`
- `task_lists`
  - 例: `- [ ]` / `- [x]`
- `lists_without_preceding_blankline`
  - 段落直後に空行なしでリスト開始してもリストとして解釈
- ネストしたリスト
  - 親 list item 内でインデントされた子リストを解釈
- 複数段落の list item
  - list item 内の空行区切り段落を保持
- definition の複数ブロック
  - 1つの `: definition` に対して複数段落/リストを保持
  - 同一 term に複数の `: definition` を許可

## 実装済み Markdown（Phase 3）

- `backtick_code_blocks`
- `fenced_code_blocks`
- `fenced_code_attributes`
- `four_space_rule`
- `blank_before_header`
- `blank_before_blockquote`
- `line_blocks`
- `escaped_line_breaks`
- `hard_line_breaks`
  - `MD2HTML_LINE_BREAK_MODE=hard`
- `ignore_line_breaks`
  - `MD2HTML_LINE_BREAK_MODE=ignore`
- `east_asian_line_breaks`
  - `MD2HTML_LINE_BREAK_MODE=east-asian`
- `space_in_atx_header`

## 実装済み Markdown（Phase 4）

- `simple_tables`
- `multiline_tables`
- `grid_tables`
- `pipe_tables`
- `table_captions`
- `table_attributes`

## 実装済み Markdown（Phase 5）

- `auto_identifiers`
- `ascii_identifiers`
- `gfm_auto_identifiers`
- `mmd_header_identifiers`
- `implicit_header_references`
- `header_attributes`
- `link_attributes`
- `inline_code_attributes`
- `bracketed_spans`
- `native_spans`
- `native_divs`
- `fenced_divs`
- `raw_attribute`
- `markdown_attribute`
- `mmd_link_attributes`
- `markdown_in_html_blocks`

識別子スタイルは `MD2HTML_IDENTIFIER_STYLE` で切り替え:
- `auto` (default)
- `ascii`
- `gfm`
- `mmd`

## 実装済み Markdown（Phase 6）

- `smart`
- `old_dashes`
- `strikeout`
- `subscript`
- `superscript`
- `short_subsuperscripts`
- `mark`
- `emoji`
- `abbreviations`
- `intraword_underscores`
- `all_symbols_escapable`
- `angle_brackets_escapable`
- `autolink_bare_uris`
- `shortcut_reference_links`
- `spaced_reference_links`

追加環境変数:
- `MD2HTML_SMART=0|1` (default: `1`)
- `MD2HTML_OLD_DASHES=0|1` (default: `0`)

## 実装済み Markdown（Phase 7）

- `tex_math_dollars`
- `tex_math_single_backslash`
- `tex_math_double_backslash`
- `raw_html`
- `raw_tex`
- `latex_macros`

## 実装済み Markdown（Phase 8）

- `yaml_metadata_block`
- `pandoc_title_block`
- `mmd_title_block`
- `citations`
- `inline_notes`
- `footnotes`

## 実装済み Markdown（Phase 9）

- `wikilinks_title_after_pipe`
- `wikilinks_title_before_pipe`
- `rebase_relative_paths`
- `literate_haskell`
- `gutenberg`

## ディレクトリ構成

- `app/main.lsp`: エントリポイント
- `lib/errors.lsp`: エラー/位置情報
- `lib/ast.lsp`: AST定義
- `lib/escape.lsp`: HTMLエスケープ
- `lib/lexer.lsp`: 行・インラインLexer
- `lib/parser.lsp`: 基本パーサ
- `lib/renderer.lsp`: HTMLレンダラ
- `md2html`: CLIラッパー
