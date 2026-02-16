# md2html (Phase 9)

`md2html` は Markdown を HTML に変換して標準出力へ出すツールです。  
この時点では **フェーズ9 互換・特殊機能** まで実装しています。

## インストール/実行方法

前提:
- ISL 本体がビルド済みで `/Volumes/SSD-PLU3/work/LISP/islisp/isl/bin/isl` が存在すること
- 実行権限: `/Volumes/SSD-PLU3/work/LISP/islisp/isl/examples/md2html/md2html`

初回セットアップ例:

```sh
cd /Volumes/SSD-PLU3/work/LISP/islisp/isl
chmod +x /Volumes/SSD-PLU3/work/LISP/islisp/isl/examples/md2html/md2html
```

実行例:

```sh
/Volumes/SSD-PLU3/work/LISP/islisp/isl/examples/md2html/md2html input.md
cat input.md | /Volumes/SSD-PLU3/work/LISP/islisp/isl/examples/md2html/md2html
/Volumes/SSD-PLU3/work/LISP/islisp/isl/examples/md2html/md2html -o output.html input.md
```

## オプション一覧

- `-o, --output <path>`
  - 変換結果を標準出力ではなくファイルへ書き込みます
- `-h, --help`
  - 使い方を表示して終了します

補足:
- 入力は `*.md` ファイル1つ、または標準入力のみ対応
- 複数入力ファイルは未対応

## 環境変数一覧（機能別早見表）

実行時に `VAR=value` を先頭に付けて指定できます。

```sh
MD2HTML_IDENTIFIER_STYLE=gfm /Volumes/SSD-PLU3/work/LISP/islisp/isl/examples/md2html/md2html input.md
```

| 機能 | 環境変数 | 値 | 既定値 | 説明 | 対応テストケース名 |
|---|---|---|---|---|---|
| 見出しID生成（Phase 5） | `MD2HTML_IDENTIFIER_STYLE` | `auto` / `ascii` / `gfm` / `mmd` | `auto` | 見出しから生成される識別子の正規化方式 | `phase5_auto_identifiers`, `phase5_ascii_identifiers`, `phase5_gfm_auto_identifiers`, `phase5_mmd_header_identifiers` |
| 改行処理（Phase 3） | `MD2HTML_LINE_BREAK_MODE` | `normal` / `hard` / `ignore` / `east-asian` | `normal` | 段落内改行の扱い | `phase3_hard_line_breaks_mode`, `phase3_ignore_line_breaks_mode`, `phase3_east_asian_line_breaks_mode` |
| スマート記法（Phase 6） | `MD2HTML_SMART` | `0` / `1` | `1` | 引用符・ダッシュ・三点リーダの置換を有効化 | `phase6_smart_and_dashes` |
| 旧ダッシュ挙動（Phase 6） | `MD2HTML_OLD_DASHES` | `0` / `1` | `0` | `smart` 有効時の `--` / `---` の割り当てを旧仕様に変更 | `phase6_old_dashes` |
| WikiLinks解釈（Phase 9） | `MD2HTML_WIKILINK_ORDER` | `after` / `before` / `both` | `both` | `[[A|B]]` を `title-after-pipe` か `title-before-pipe` で解釈 | `phase9_wikilinks_title_after_pipe`, `phase9_wikilinks_title_before_pipe` |
| 相対パス再基準化（Phase 9） | `MD2HTML_REBASE_RELATIVE` | `0` / `1` | `0` | リンク/画像の相対URLを再基準化 | `phase9_rebase_relative_paths` |
| 相対パス再基準化（Phase 9） | `MD2HTML_REBASE_PREFIX` | 文字列パス | （空） | 再基準化先プレフィックス（未指定時は入力ファイルのディレクトリ） | `phase9_rebase_relative_paths` |
| Literate Haskell（Phase 9） | `MD2HTML_LITERATE_HASKELL` | `0` / `1` | `0` | Bird-style (`> code`) をコードブロックとして扱う | `phase9_literate_haskell` |
| Gutenberg整形（Phase 9） | `MD2HTML_GUTENBERG` | `0` / `1` | `0` | Gutenbergの前後マーカー除去と本文整形を有効化 | `phase9_gutenberg` |

注記:
- `MD2HTML_ROOT` などの一部変数はラッパースクリプト内部用途です（通常は手動設定不要）。

## 制約事項（対応Markdown範囲）

- 対応範囲:
  - フェーズ0〜9で実装済みの機能（本 README の「実装済み Markdown」各節）
- 仕様上の制約:
  - Pandoc 完全互換ではありません（互換を意図した拡張のみ一部対応）
  - 出力形式は HTML（単一テキスト）で、テンプレート/CSS 自動付与はしません
  - 入力文字コードは UTF-8 前提（不正時はエラー終了）
  - `stdout` リダイレクト先の書き込み不可はシェル側エラーになります

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
- `ignore_line_breaks`
- `east_asian_line_breaks`
- `space_in_atx_header`

設定方法は「環境変数一覧（機能別早見表）」を参照してください。

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

設定方法は「環境変数一覧（機能別早見表）」を参照してください。

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

設定方法は「環境変数一覧（機能別早見表）」を参照してください。

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
