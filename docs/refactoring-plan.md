# リファクタリング計画（保守性の改善）

機能実装が一段落したため、本計画では **挙動を一切変えずに** 保守性を改善する。
焦点は「巨大関数の分割」「単一責務化」「重複の排除」「巨大ファイルの分割」。

---

## 0. 現状の定量把握（2026-06-06 時点）

### ファイル規模

| ファイル | 行数 | 役割 |
|---------|------|------|
| `src/isl/core.scm` | **7326** | インタプリタ本体（単一モジュール） |
| `src/isl/compiler/runtime.scm` | **5095** | JIT/IR インタプリタ |
| `src/isl/compiler/llvm_runtime.c` | **2301** | ネイティブ C ランタイム |
| `src/isl/compiler/lowering.scm` | 680 | IR → CFG |
| `src/isl/compiler/ir.scm` | 657 | read → 正規化 |
| `src/isl/compiler/codegen.scm` | 625 | LLVM IR 生成 |

### 突出して長い関数（保守性のボトルネック）

| 関数 | 行数 | ファイル | 問題 |
|------|------|---------|------|
| `install-primitives!` | **3234** | core.scm | 全組込みを 1 関数に登録。フェーズ別コメントで区切り済み |
| `install-primitives!` | **3087** | runtime.scm | 同上（JIT 側） |
| `runtime-eval-special` | **746** | runtime.scm | 特殊形式の巨大 `case` にロジック直書き |
| `normalize-special` | **369** | ir.scm | 正規化の巨大 `case` にロジック直書き |
| `make-initial-env` | 240 | core.scm | 環境構築 + プロファイル分岐 + 多数の補助 |
| `lower-expr` | 150 | lowering.scm | ディスパッチ + 一部の枝に直書き |
| `render-format` | 138 | core.scm | format 指示子を 1 関数で総処理 |
| `eval-special` | 121 | core.scm | ※既に `eval-*` への薄いディスパッチ（良い手本） |

### 重複（core.scm ↔ runtime.scm）

両ファイルに同名定義が多数存在: `parse-params`, `parse-method-params`,
`bind-params!`, `make-closure`/`closure-*`, `make-primitive`/`primitive-*`,
`ensure-generic-function!`, `resolve-class-designator`, `method-applicability-score`,
`make-go-signal`/`go-signal?`, `gauche-*`（スレッド/TLS ラッパ）等。
runtime.scm は既に `(use isl.core)` しているため共通化の余地が大きい。

---

## 1. 原則

1. **挙動不変（behavior-preserving）**: 純粋な構造変更のみ。出力・エラー・評価順序を変えない。
2. **テストゲート**: 各ステップの完了条件は全テストが緑であること（下記 §2）。
3. **小さく刻む**: 1 コミット = 1 つの意味的に独立したリファクタリング。レビュー可能な粒度。
4. **単一責務**: 1 関数 1 機能。目安として **トップレベル関数は 80 行以内**、
   ディスパッチャは「振り分けのみ」（枝のロジックは名前付き補助関数へ）。
5. **重複排除は最後・慎重に**: 共通化はリスクが高いため、関数分割が済んでから着手。

## 2. 安全網（各ステップで必ず緑を確認）

```sh
gosh test/conformance/run.scm          # extended 101/101
gosh test/conformance/run-strict.scm   # strict   101/101
gosh test/compiler/native-gap-probe.scm # 49/49 SAME / 0 DIFF
for f in test/compiler/*smoke*.scm; do gosh "$f"; done  # 全 smoke
gosh test/conformance/oracle-compare.scm extended   # 差分監視
```

ベースライン（現状）: extended 101/101・strict 101/101・gap-probe 49/49・
compiler smokes 38/38・spec-probe 227/227。**このすべてを各ステップ後に維持する。**

## 3. 達成目標（メトリクス）

| 指標 | 現状 | 目標 |
|------|------|------|
| 最長トップレベル関数 | 3234 行 | **≤ 200 行**（理想 80 行） |
| 200 行超の関数数 | 7 | **0** |
| `core.scm` 行数 | 7326 | 1 ファイル ≤ 2000 行（分割後） |
| `runtime.scm` 行数 | 5095 | 1 ファイル ≤ 2000 行 |
| core ↔ runtime 重複定義 | 多数 | 共通ユーティリティを単一の出所に |

---

## 4. フェーズ（優先度順：高価値・低リスクから）

実施は新ブランチ `refactor/maintainability`（現 `docs/textbook-init` から分岐）で行う。

### R1. `install-primitives!`（core.scm 3234 行）をテーマ別に分割 ★最優先
既存のフェーズ別コメント（算術 / 数値変換 / 型述語 / 三角 / リスト / 文字列 /
文字 / ベクタ・配列 / 入出力 / 条件 / CLOS / システム）を継ぎ目として、
`install-arithmetic-primitives!`、`install-list-primitives!` …のように
**テーマ別インストーラ関数**へ抽出。`install-primitives!` 本体はそれらを順に呼ぶだけにする。
- リスク: 低（機械的な切り出し。共有する内部 `define` は引数で渡すか上位スコープへ）
- 価値: 最大（最長関数を一掃、各機能の所在が明確化）

### R2. `install-primitives!`（runtime.scm 3087 行）を同様に分割
R1 と同じ方針を JIT ランタイム側に適用。

### R3. `runtime-eval-special`（runtime.scm 746 行）を分割
巨大 `case` の各枝を `rt-eval-<form>` 補助関数に抽出し、本体は薄いディスパッチャ化。
core.scm の `eval-special`（既に `eval-*` へ振り分けるだけ）を手本にする。

### R4. `normalize-special`（ir.scm 369 行）を分割
各特殊形式の正規化を `normalize-<form>` に抽出（一部は既存）。本体はディスパッチのみ。

### R5. 中規模関数の単一責務化
- `make-initial-env`（240）: プロファイル適用・パッケージ初期化・束縛投入を分離。
- `render-format`（138）: 指示子ごとのハンドラに分解（テーブル駆動も検討）。
- `lower-expr`（150）: 直書きの枝を `lower-<form>` へ寄せ、純粋ディスパッチャ化。
- `eval-with-open-file`(81) / `eval-defclass`(65) / `eval-loop-extended`(53)。
- codegen の `emit-rhs`(56) / `emit-const-build`(55) / `emit-call-rhs`(53)。

### R6. 巨大ファイルの分割（モジュール `include`）
`isl.core` モジュールを保ったまま Gauche の `include` で複数ファイルへ分割:
`reader/printer`・`evaluator`・`special-forms`・`primitives/*`・`clos`・
`packages`・`conditions`。`runtime.scm` も同様。
- リスク: 中（load path と `include` の解決を要検証。まず 1 ファイル切り出しで PoC）

### R7. core ↔ runtime 重複の削減
`parse-params`・`parse-method-params`・CLOS 補助・`gauche-*` ラッパ・クロージャ／
プリミティブのアクセサ等の共通部分を `isl.core` から export して再利用、または
共有モジュール `isl.common` へ切り出す。
- リスク: 高（runtime 側は `rt-value` ラップが絡む。差異を見極めて段階的に）
- 着手は R1〜R6 完了後。

### R8. C ランタイムの分割（llvm_runtime.c 2301 行）
論理単位（values / env / numbers / strings / vectors・arrays / conditions /
clos / io / primitive table）へ分割。単一翻訳単位なら集約ヘッダ + `#include`、
あるいはビルドを複数オブジェクト化。優先度: 低。

---

## 5. 進め方

1. `refactor/maintainability` ブランチを作成。
2. R1 から順に、1 フェーズ＝複数の小コミット。各コミット後に §2 の全テストを実行。
3. フェーズ完了ごとに本ドキュメントの進捗を更新。
4. まとまった単位で PR を作成（レビュー可能な粒度を維持）。

> 機能追加は行わない。テストが赤になったら直ちに切り戻し、原因を特定してから再開する。
