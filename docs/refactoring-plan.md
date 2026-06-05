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
| 最長トップレベル関数（**ロジックを含むもの**） | 3234 行 | **≤ 200 行**（理想 80 行） |
| `core.scm` 行数 | 7326 | 1 ファイル ≤ 2000 行（分割後） |
| `runtime.scm` 行数 | 5095 | 1 ファイル ≤ 2000 行 |
| core ↔ runtime 重複定義 | 多数 | 共通ユーティリティを単一の出所に |

> **メトリクスの注記（R1 で得た知見）**: プリミティブ登録関数（`install-*!`）は
> `(def 'name proc)` の**平坦な羅列**であり制御複雑度を持たない。テーマが多くの
> プリミティブを含む場合 200 行を超えても、**単一テーマであれば許容**する。
> ≤200 行の目標は「制御フロー／ロジックを持つ関数」に適用する。登録関数内で
> 個々の `proc` ラムダが長大な場合は、その**ラムダを名前付きヘルパへ抽出**する
> （登録リストの機械的な等分割より優先）。

---

## 4. フェーズ（優先度順：高価値・低リスクから）

実施は新ブランチ `refactor/maintainability`（現 `docs/textbook-init` から分岐）で行う。

### R1. `install-primitives!`（core.scm 3234 行）をテーマ別に分割 ★最優先 ✅完了
既存のフェーズ別コメントを継ぎ目として **15 個のテーマ別インストーラ**へ抽出し、
`install-primitives!` 本体は局所ヘルパ `def` の定義＋各インストーラ呼び出し（21 行）に縮小。
純粋ヘルパ 4 個をモジュールレベルへ持ち上げ。登録順は従来と完全一致で挙動不変。
- 結果: 最長関数 3234 → 490 行（`install-sequence-primitives!`、平坦な登録列）。
  ロジックを含む関数の最長は別途 R3〜R5 で対処。
- 検証: strict/extended 101/101・spec-probe 227/227・gap-probe 49/49・smokes 38/38。

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

---

## 6. 進捗ログ

すべて `refactor/maintainability` ブランチ、挙動不変（全テスト緑を維持）。
各ステップ後の検証: conformance strict/extended 101/101・spec-probe 227/227・
native-gap-probe 49/49・compiler 45/46（`runtime-m7-oracle` は本作業以前からの
既知の事前失敗＝CLOS instance identity）。

| フェーズ | 内容 | 結果 |
|---------|------|------|
| ✅ R1 | core.scm `install-primitives!` を 15 テーマ別インストーラへ | 3234 行 → 21 行ディスパッチャ |
| ✅ R2 | runtime.scm `install-primitives!` を 15 テーマ別インストーラへ | 3087 行 → 薄いディスパッチャ |
| ✅ R3 | `runtime-eval-special` を 33 個の `rt-eval-<form>` へ | 746 行 → 37 行ディスパッチャ |
| ✅ R4 | `normalize-special` を 38 個の `normalize-special-<form>` へ | 369 行 → 薄いディスパッチャ |
| ✅ R5 | `make-initial-env` を 4 補助へ分離 / `lower-expr` の special 枝を分離 | 240→54 行 / 150→81 行 |
| ✅ R6 | core.scm / runtime.scm を Gauche `include` で関心事別ファイルへ分割 | 下表参照 |

### R6 後のファイル構成

| ファイル | 行数 | 内容 |
|---------|------|------|
| `core.scm` | 3503（←7412） | モジュール宣言 + 評価器コア + 4 include |
| `core/primitives.scm` | 3304 | 組込みプリミティブ登録（install-* 群、平坦な登録列） |
| `core/bootstrap.scm` | 273 | make-initial-env と初期化補助 |
| `core/reader.scm` | 191 | ISLISP リーダ層と REPL |
| `core/conditions.scm` | 165 | 条件/配列/プロパティ/クラス階層/動的変数のデータ表現 |
| `runtime.scm` | 932（←5221） | モジュール宣言 + 値表現/評価ループ等のコア + 2 include |
| `runtime/primitives.scm` | 3156 | 組込みプリミティブ登録（平坦な登録列） |
| `runtime/special-forms.scm` | 1145 | rt-eval-* と runtime-eval-special |

各 include ファイルは `select-module` 後に展開されるため名前空間は不変。
Gauche の `include` はインクルード元ファイル基準で相対パス解決される（PoC で確認）。

> 残: `primitives.scm`(3304) / `runtime/primitives.scm`(3156) は平坦な登録列で
> 単一関心事のため許容（§3 の注記）。`core.scm`(3503) の評価器コアは
> パッケージ/CLOS/評価が密に結合した心臓部で、さらなる分割は高リスクのため
> 現状維持とした（必要なら evaluator/clos/packages への分割は将来検討）。

**R5 残（軽微・任意）**: `render-format`(138)・`runtime-eval-top`(124)・
`make-runtime-state`(93) は単一責務で凝集度が高く、`eval-special`(121) は既に
薄いディスパッチャのため、現状維持で許容範囲とした（必要なら指示子ハンドラの抽出は可能）。

**残フェーズ（要判断・大きめの構造変更）**:
- R6 巨大ファイルの `include` 分割（core.scm 7387 行 / runtime.scm 5143 行）— 中リスク
- R7 core ↔ runtime 重複の共通化 — 高リスク（`rt-value` ラップ）
- R8 C ランタイム `llvm_runtime.c` の分割 — 低優先
