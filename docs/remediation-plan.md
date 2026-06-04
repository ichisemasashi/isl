# ISLISP 処理系 全体修正計画

ISLISP (ISO/IEC 13816) への準拠度を、インタプリタとコンパイラの双方について
実証的に測定し、優先度付きの修正計画にまとめたもの。

- 調査日: 2026-06-04
- 測定ツール（再現可能）:
  - インタプリタ: `gosh test/conformance/spec-probe.scm` /
    `gosh test/conformance/run-spec.scm`
  - ネイティブ AOT: `gosh test/compiler/native-gap-probe.scm`
- 関連: [spec-gap-report.md](spec-gap-report.md)（インタプリタの欠落詳細）

---

## 0. 全体像（3 つの実行経路）

本処理系には実行経路が 3 つあり、準拠度が大きく異なる。

| 経路 | 実体 | コマンド | 準拠度 |
|------|------|----------|--------|
| インタプリタ | `src/isl/core.scm` | `./bin/isl` | 標準操作子 174/227（53 欠落/逸脱）|
| JIT / Scheme ランタイム | `src/isl/compiler/runtime.scm` | `./bin/islc --jit` | インタプリタとほぼ同等（feature 完備）|
| **ネイティブ AOT** | `codegen.scm` + `llvm_runtime.c` | `./bin/islc --run`, `islc-aot` | **49 項目中 17 のみ対応** |

重要な発見:

1. **ネイティブ AOT codegen が最大のボトルネック**。JIT 経路は `while` や CLOS、
   例外、文字列、ベクタを正しく扱えるのに、同じソースをネイティブ AOT で実行すると
   未対応かつ **一部は黙って誤った値を返す**。
2. **プリミティブが二重実装**されている（`core.scm` と `compiler/runtime.scm`）。
   標準関数を 1 つ足すには両方に手を入れる必要があり、欠落が二重に発生しやすい。

---

## 1. 優先度の定義

| 優先度 | 基準 |
|--------|------|
| **P0** | 正しさを脅かす（コンパイルが通り実行できるのに *黙って誤った値* を返す）|
| **P1** | 標準で必須かつ影響範囲が広い欠落 |
| **P2** | 標準だが影響が限定的／代替がある欠落 |
| **P3** | 軽微な逸脱・補完 |

---

## 2. ネイティブ AOT バックエンド（最優先領域）

`native-gap-probe.scm` の結果: **17 SAME / 26 未対応 / 6 誤出力（全 49）**。

対応済み（SAME）: 整数演算, 有理数, `<`, `if`, `cond`, `let`/`let*`,
`defun`+再帰, クロージャ, `funcall`, `apply`, `cons`/`list`, `mapcar`,
`append`/`reverse`, `format ~A/~S/~D`。

### P0 — 黙って誤る（最優先・正しさの欠陥）

これらは「未対応エラー」ではなく **コンパイル・実行が成功して誤った値を返す**ため、
最も危険。

| 機能 | 期待 | ネイティブ実出力 | 症状 |
|------|------|------------------|------|
| `while` | 6 | **0** | ループが実行されず初期値のまま |
| `dotimes` | 6 | **0** | 同上 |
| `dolist` | 6 | **0** | 同上 |
| `tagbody`/`go` | 3 | 誤 | 反復が機能しない |
| `unwind-protect` | 9 | 誤 | cleanup が実行されない |
| `format ~X`（16進）| `ff` | `~X` | 指示子が未処理でそのまま出力 |

→ **作業 N0**: codegen/lowering でループ系（`while`/`dotimes`/`dolist`/`tagbody`）の
本体評価と変数更新を正しく実装。`unwind-protect` の cleanup 経路を実装。
`format` の `~X`/`~O`/`~B`/`~C`/`~%` 指示子を `llvm_runtime.c` の format 実装に追加。
**まず誤出力を「未対応エラー」に格下げしてでも、誤った値を返さないようにする**のが先決。

### P1 — 制御構造の完成

frontend は認識するが codegen が "unsupported operation" を出す:

- `case`, `and`, `or` … lowering で `if` 連鎖へ落とす（`cond` は既に対応済みなので流用可）
- `block`/`return-from`, `catch`/`throw` … 非局所脱出。`llvm_runtime.c` に
  `setjmp/longjmp` ベースの脱出基盤を入れる（M5 の完了ゲート）
- `for`, `flet`, `labels` … `flet`/`labels` はローカル関数を closure 化して対応

### P1 — 数値・型の拡充

- **浮動小数点が `+` で扱えない**（`(+ 1.5 2.5)` → "expected number"）。
  `IslValue` に float タグを追加し、算術プリミティブを混合演算対応にする。
- `expt`, `mod`, `abs`, `max`, `min` 等の数値ライブラリ関数が未定義
  （`llvm_runtime.c` に登録 or runtime 呼び出しへ委譲）。
- 比較は `<` のみ確認済み。`>`,`<=`,`>=`,`=`,`/=` の網羅。

### P2 — データ型の拡張

- **文字列**: `string-append`（未定義）, `length` が文字列を「リストでない」と誤判定。
  文字列値表現と文字列プリミティブを native runtime に追加。
- **文字**: 文字リテラル `#\A` が "unsupported"、`char<` 未定義。文字型を追加。
- **ベクタ/配列**: `vector`, `elt`, `create-array`, `aref` すべて未定義。
  `ISL_V_VECTOR` を追加。
- **例外**: `handler-case`、ゼロ除算の `<division-by-zero>` 捕捉が未対応
  （非局所脱出基盤 P1 の上に構築）。

### P2/P3 — オブジェクト・動的・型注釈

- **CLOS**: `defclass`/`make-instance`/`defmethod` が native では未対応。
  最も重い。後段（JIT/インタプリタへフォールバックする手もある）。
- `dynamic`/`dynamic-let`, `the`（無視で可）, `convert` の native 対応。

### ネイティブ AOT 推奨着手順

```
N0 (P0 誤出力の是正) → N1 (制御: case/and/or/block/catch/loop) →
N2 (数値: float + 数値ライブラリ + 比較網羅) → N3 (文字列・文字・ベクタ) →
N4 (例外 + ゼロ除算条件) → N5 (CLOS・dynamic・convert)
```

---

## 3. インタプリタ（`core.scm`）

`spec-gap-report.md` の分類 A〜G に対応。各項目に推奨優先度を付す。

| ID | 区分 | 内容 | 優先 | 主な作業 |
|----|------|------|------|----------|
| I-A | §13 配列 | 多次元配列・`garef`/`set-garef`・`<basic-array*>`/`<general-array*>` | P1 | `create-array` を n 次元対応に。行優先の添字計算。配列クラス追加 |
| I-B | §18/19 I/O | 文字列ストリーム・`preview-char`・`format-*` 群 | P1 | `create-string-{in,out}put-stream`/`get-output-stream-string`、`format-integer/char/float/object/fresh-line/tab` |
| I-C | §10 シンボル | `property`/`set-property`/`remove-property` | P2 | シンボルごとの plist テーブル |
| I-D | §22 条件 | 条件アクセサ群・`report-condition`・ゼロ除算捕捉 | P1 | 各コンディションのスロットアクセサ、`/`・`quotient` のゼロ除算を `<division-by-zero>` として送出 |
| I-E | §21 OO | `create`/`initialize-object`・GF クラス | P2 | 標準 `create` を `make-instance` の別名 GF として実装、`<generic-function>` 等のクラス登録 |
| I-F | §11 数値 | `div`・`sinh/cosh/tanh/atanh`・`atan2`・`*most-*-float*`・`generic-function-p` | P2 | プリミティブ追加（Gauche に委譲可能なものが多い）|
| I-G | 逸脱 | `set-car`/`set-cdr` 引数順序、`error` のフォーマット展開 | P3 | 引数順を `(obj cons)` に修正、`condition-message` でフォーマット適用 |
| I-H | 特殊形式 | `case-using`・`assure` | P3 | `eval-special` に追加（`case`/`the` を流用）|

検証は `gosh test/conformance/run-spec.scm` の FAIL 件数で追跡（実装が進むと減る）。

---

## 4. 共通基盤の整理（推奨・横断）

- **プリミティブの二重実装を解消**: `core.scm` の `install-primitives!` と
  `compiler/runtime.scm` で同じ関数を別々に持っている。共通定義へ寄せると、
  以後の標準関数追加が 1 箇所で済み、インタプリタ/JIT の乖離も防げる。
- **strict プロファイルの欠落ゼロ化**: 現状、欠落はプロファイル非依存。
  まず strict で「標準必須のみ」を満たすことを当面のゴールに据える。

---

## 5. 進め方と検証ゲート

1. 3 つのプローブを CI に組み込み、各 PR で回帰を可視化:
   - `test/conformance/spec-probe.scm`（インタプリタ存在/挙動）
   - `test/conformance/run-spec.scm`（インタプリタ準拠の合否）
   - `test/compiler/native-gap-probe.scm`（ネイティブ AOT のオラクル差分）
2. 既存の緑（`run.scm` 101/101、各 smoke）を壊さないこと。
3. **ネイティブ P0（誤出力）を最優先**で潰す。次に I-D（ゼロ除算）など正しさ系。
4. マイルストーン運用ルール（`compiler-milestones.md`）に従い、機能追加時は
   必ず失敗系テストも同時追加。strict で未解決差分を残さない。

---

## 6. 優先タスク一覧（着手順サマリ）

1. **[P0]** ネイティブ: `while`/`dotimes`/`dolist`/`tagbody`/`unwind-protect`/`format ~X`
   の誤出力を是正（まず誤値を返さない）。
2. **[P1]** ネイティブ: `case`/`and`/`or` の lowering、非局所制御
   （`block`/`catch`）の脱出基盤。
3. **[P1]** ネイティブ: float 算術 + 数値ライブラリ + 比較網羅。
4. **[P1]** インタプリタ: 多次元配列（I-A）、文字列ストリーム/`format-*`（I-B）、
   ゼロ除算条件と条件アクセサ（I-D）。
5. **[P2]** ネイティブ: 文字列・文字・ベクタ。インタプリタ: plist（I-C）、
   `create`/`initialize-object`（I-E）、数値補完（I-F）。
6. **[P2/P3]** ネイティブ: 例外・CLOS。インタプリタ: 逸脱是正（I-G）、
   `case-using`/`assure`（I-H）。
7. **[横断]** プリミティブ二重実装の共通化。
