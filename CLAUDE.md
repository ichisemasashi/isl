# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## 実行環境

- 実装言語: **Gauche 0.9.15**（Scheme処理系）
- 起動コマンド: `gosh`

## コマンド早見表

```sh
# REPL 起動
./bin/isl

# ファイル実行
./bin/isl examples/hello.lsp

# strict プロファイル（標準準拠モード）で実行
./bin/isl --profile strict examples/hello.lsp

# 準拠テスト（strict）
gosh test/conformance/run-strict.scm

# 準拠テスト（extended）
gosh test/conformance/run.scm

# oracle 差分比較
gosh test/conformance/oracle-compare.scm strict
gosh test/conformance/oracle-compare.scm extended

# コンパイラ関連
./bin/islc-front --dump-ir examples/hello.lsp
gosh test/compiler/frontend-smoke.scm
gosh test/compiler/runtime-m2-smoke.scm
gosh test/compiler/runtime-m2-oracle.scm

# AOT コンパイル
./bin/islc --emit-llvm out.ll examples/hello.lsp
./bin/islc --run examples/hello.lsp
./bin/islc-aot -o hello-aot examples/hello.lsp

# JIT
./bin/islc --jit examples/hello.lsp
./bin/islc-jit --repl
```

## アーキテクチャ概要

### インタープリタ

中心は `src/isl/core.scm`（約 4700 行）の単一ファイル。

- **環境**: ネストした `frame`（vector）でレキシカルスコープを実現。`frame-find-pair` が親を辿る。
- **評価ループ**: `eval-islisp*` が末尾呼び出し最適化付きで全フォームを処理。特殊形式は `eval-special` に dispatch。
- **プリミティブ登録**: `install-primitives!` 内の `(def 'name proc)` ですべての組み込み関数を環境に登録。
- **プロファイル**: `*isl-profile*` が `strict`/`extended` を保持。`disable-extended-primitives!` で拡張を無効化。
- **パッケージ**: `*package-registry*`/`*current-package*` でグローバル状態を管理。シンボル解決は `resolve-symbol-in-package`。
- **CLOS**: `*class-table*`/`*accessor-slot-table*` でクラスとジェネリック関数を管理。

エントリポイント: `src/isl/main.scm` が CLI 引数を解析して `make-initial-env` → `repl` または `run-files`。

### コンパイラ

`src/isl/compiler/` 以下にマイルストーン別に実装。

| ファイル | 役割 |
|---------|------|
| `frontend.scm` | read → macroexpand → IR 正規化 |
| `ir.scm` | IR データ構造定義 |
| `lowering.scm` | IR → LLVM 直交 CFG（基本ブロック分解） |
| `runtime.scm` | タグ付き値・環境・クロージャのランタイム |
| `codegen.scm` | LLVM IR テキスト生成 |
| `runner.scm` | emit-llvm/emit-obj/run/jit モードの統合 |
| `jit.scm` | ORC v2 JIT（M11） |

IR の評価順序は effect barrier で固定（副作用は左から順に実行）。LLVM バックエンドは `clang` 経由でネイティブバイナリを生成。

### テスト構成

- `test/conformance/` — interpreter の準拠テスト。`milestones.scm` がケース定義、`common.scm` がランナー。
- `test/compiler/` — コンパイラの各マイルストーン smoke テストと oracle 比較。

各テストケースは `(kind form expected)` の 3 要素リスト:
- `'value` — `eval` 結果が `expected` と `equal?` であること
- `'error` — エラーが発生すること
- `'oracle` — interpreter の結果をそのまま記録（差分テスト向け）

### プロファイル分離

`strict` モードでは以下が無効:
- 特殊形式: `trace`, `untrace`, `defpackage`, `in-package`
- 組み込み関数: `debug`, `break`, `getenv`, `setenv`, `system`, DB 系, ネットワーク系, FFI 系

`extended` モードはデフォルト。既存の examples はすべて extended を前提。

## ドキュメント

`docs/` 以下に実装方針の仕様書がある:

- `compliance-matrix.md` — strict/extended 差分一覧
- `compiler-milestones.md` — M0〜M11 のマイルストーン定義と完了ゲート
- `compiler-compliance-spec.md` — 評価順序・非局所制御・数値セマンティクス等の難所仕様
- `runtime-abi.md` — コンパイラ/ランタイム間の ABI
- `side-effect-order.md` — 副作用順序の規範
- `symbol-resolution.md` — パッケージ/シンボル解決ポリシー
- `optimization-policy.md` — LLVM パス whitelist と意味保証の範囲

## 注意事項

- テストランナーは `gosh`（Gauche）で直接実行する。`./bin/isl` は ISLISP インタープリタ用。
- `src/` を load path に追加する必要があるため、テストはリポジトリルートから実行する。
- `with-open-file` は標準の `with-open-input-file`/`with-open-output-file` を統合した独自形式。
- 真偽値は内部的に Gauche の `#t`/`#f` を使うが、`nil` は `'()` として環境に束縛される。
- `handler-case` は Common Lisp 互換の形式で、ISLISP 標準の `with-handler` とは異なる。
