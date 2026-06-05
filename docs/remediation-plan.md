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

`native-gap-probe.scm` の初期結果: **17 SAME / 26 未対応 / 6 誤出力（全 49）**。
P0+P1（and/or/case・数値・非局所制御・float・setq/ループ/unwind-protect）+ P2 データ型
（文字・文字列・ベクタ）実施後: **39 SAME / 10 未対応 / 0 誤出力**。残る未対応は
flet/labels、CLOS（defclass/defmethod）、例外（handler-case/ゼロ除算捕捉）、
dynamic/the/convert。

対応済み（SAME）: 整数演算, 有理数, `<`, `if`, `cond`, `let`/`let*`,
`defun`+再帰, クロージャ, `funcall`, `apply`, `cons`/`list`, `mapcar`,
`append`/`reverse`, `format ~A/~S/~D`。

### P0 — 黙って誤る（最優先・正しさの欠陥）✅ 対応済み

これらは「未対応エラー」ではなく **コンパイル・実行が成功して誤った値を返す**ため、
最も危険だった。**2026-06-04 に是正済み**（`native-gap-probe.scm` の DIFF は 6→0）。

| 機能 | 期待 | 修正前 | 修正後 |
|------|------|--------|--------|
| `while` | 6 | **0**（黙って誤る）| exit 70 で失敗（`native backend: unsupported special form: while`）|
| `dotimes` | 6 | **0** | 同上（loud failure）|
| `dolist` | 6 | **0** | 同上 |
| `tagbody`/`go` | 3 | 誤 | 同上 |
| `unwind-protect` | 9 | 誤 | 同上 |
| `format ~X`（16進）| `ff` | `~X` | **`ff`（正しく実装）** |

実施した修正:

1. **`llvm_runtime.c` `isl_rt_unsupported`**: エラー値を返す（→結果が捨てられ
   黙って誤る）のをやめ、stderr に診断を出して `exit(70)` で停止。あわせて
   引数 ABI を修正（codegen は `IslValue*` ではなく生 C 文字列を渡していた）。
2. **`codegen.scm` `emit-rhs`**: 未対応 `special` ノードに *フォーム名入り* の
   診断メッセージを付与。
3. **`llvm_runtime.c` `prim_format`**: `~X`/`~O`/`~B`/`~&` を実装（基数出力
   ヘルパ `isl_format_radix` を追加）。未知の指示子は素通りさせず loud failure。

回帰テスト: `test/compiler/native-p0-smoke.scm`（format 基数 + 未対応形式が
黙って誤らないことを検証）。

> 補足: ループ系（`while`/`dotimes`/`dolist`/`tagbody`）と `unwind-protect` を
> ネイティブで *正しく動かす* には、変数変更（`setq`）とループ lowering が必要で、
> これは P1「制御構造の完成」の範囲（native は現状ミューテーション非対応）。
> P0 の責務は「誤った値を返さない」ことなので loud failure で達成とする。

### P1 — 制御構造の完成

frontend は認識するが codegen が "unsupported operation" を出す:

- ✅ `case`, `and`, `or` … **対応済み**（`lowering.scm` に `lower-and`/`lower-or`/
  `lower-case` を追加し branch/phi へ落とす。`case` は key を 1 度だけ束ねて
  `cond`+`eql` に書き換え。`if`/`cond` と同じ基盤を再利用、IR インタプリタも共用）。
- ✅ `block`/`return-from`, `catch`/`throw` … **対応済み**。lowering で body を
  0 引数サンクに包んだ `%block`/`%catch`/`%throw`/`%return-from` 呼び出しへ書き換え、
  `llvm_runtime.c` に `setjmp/longjmp` の統合 exit スタックを実装（throw が中間の
  block フレームを飛び越えても正しく破棄）。同じ lowered CFG を使う IR インタプリタ
  （runtime.scm）側にも同名プリミティブを追加。
- ✅ `setq` と反復（`while`/`dotimes`/`dolist`/`for`/`tagbody`/`go`）、`unwind-protect`
  … **対応済み**。setq は `(set name val)` 命令へ lowering し、変数読みを名前参照に
  統一（パラメータは entry で名前束縛済み）＋ `isl_rt_set` で可変化。ループは CFG の
  後方エッジへ lowering（値は可変 env 経由なので phi 不要）。dotimes/dolist/for は
  let+while+setq へ縮約。tagbody は各ラベルを CFG ブロックにし `go` を jmp 化。
  unwind-protect は exit スタックを段階的に巻き戻して各 cleanup を実行。
- ⬜ `flet`, `labels` … ローカル関数を closure 化して対応。**未着手**。
  （`go` がネスト lambda を越える/`unwind-protect` を `go` で抜ける場合の cleanup は
  現状未対応。lexically-direct なケースは動作する。）

### P1 — 数値・型の拡充

- ✅ **浮動小数点** … **対応済み**。`IslValue` に `ISL_V_FLOAT`（double）を追加し、
  算術（`+ - * /`）と比較を float 伝播（混合演算は float）対応に。float リテラルは
  10 進テキストを `strtod` する `isl_rt_make_float` で生成（LLVM の hex-float 要件を
  回避）。出力は最短往復表現でインタプリタと一致（`4.0`/`0.5`/`0.1`）。`floatp`/
  `float`/`expt`(float基数) も追加。整数・有理数は引き続き exact。
- ✅ `expt`, `mod`, `abs`, `max`, `min` … **対応済み**（`llvm_runtime.c` に有理数対応で
  追加。`mod` は除数の符号、`expt` 負指数は有理数）。
- ✅ 比較網羅 … `>`,`<=`,`>=`,`=` は既存、`/=` を追加して **網羅済み**。

### P2 — データ型の拡張

- ✅ **文字**: `ISL_V_CHAR` を追加。リテラル `#\A`、`char= /= < > <= >=`、
  `char->integer`/`integer->char`、印字（`#\A`）。
- ✅ **文字列**: `string-append`、`string=`、`create-string`、`char-index`、
  `length`/`elt` を文字列対応に（`elt` は文字を返す）。
- ✅ **ベクタ/1 次元配列**: `ISL_V_VECTOR` を追加。`vector`、`create-vector`、
  `create-array`(rank-1)、`elt`/`aref`/`vector-ref`、`set-elt`/`set-aref`、
  `length`、印字（`#(...)`）。多次元配列はインタプリタのみ（native は rank-1）。
- ⬜ **例外**: `handler-case`、ゼロ除算の `<division-by-zero>` 捕捉が未対応
  （非局所脱出基盤の上に構築。`error`/コンディションのネイティブ表現が必要）。

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

| ID | 区分 | 内容 | 優先 | 状態 | 主な作業 |
|----|------|------|------|------|----------|
| I-A | §13 配列 | 多次元配列・`garef`/`set-garef`・`<basic-array*>`/`<general-array*>` | P1 | ✅ | `create-array` を n 次元対応に（行優先 flat storage）。`aref`/`set-aref`/`garef`/`set-garef` を多添字対応。配列クラス追加・`general-array*-p`/`basic-array*-p` 是正 |
| I-B | §18/19 I/O | 文字列ストリーム・`preview-char`・`format-*` 群 | P1 | ✅ | `create-string-{in,out}put-stream`/`get-output-stream-string`、`format-integer/char/float/object/fresh-line/tab` を実装 |
| I-C | §10 シンボル | `property`/`set-property`/`remove-property` | P2 | ✅ | シンボルごとの plist テーブル（`*property-table*`）を実装 |
| I-D | §22 条件 | 条件アクセサ群・`report-condition`・ゼロ除算捕捉 | P1 | ✅ | コンディションに `extra` plist を追加し各アクセサ実装。`/`・`quotient`・`mod`・`rem` のゼロ除算を `<division-by-zero>`（arithmetic-error operation/operands 付き）として送出 |
| I-E | §21 OO | `create`/`initialize-object`・GF クラス | P2 | ✅ | `create` を `make-instance` へ委譲。`<generic-function>`/`<standard-generic-function>`/`<standard-class>`/`<storage-exhausted>` を登録。`function` 特殊形式が generic を返すよう是正 |
| I-F | §11 数値 | `div`・`sinh/cosh/tanh/atanh`・`atan2`・`*most-*-float*`・`generic-function-p` | P2 | ✅ | プリミティブ追加（`div` は床除算＋ゼロ除算捕捉、双曲線関数は exp 経由、float 定数は値束縛）|
| I-G | 逸脱 | `set-car`/`set-cdr` 引数順序、`error` のフォーマット展開 | P3 | ✅ | 引数順を `(obj cons)` に修正。`condition-message` をフォーマット適用（失敗時は raw にフォールバック）|
| I-H | 特殊形式 | `case-using`・`assure` | P3 | ✅ | `eval-case-using` を追加、`assure` は `the` へ委譲 |

検証は `gosh test/conformance/run-spec.scm` の FAIL 件数で追跡（実装が進むと減る）。
I-A/I-B/I-D 実装後の `spec-probe extended`: **203 OK / 23 MISSING / 1 WRONG / 0 ERR**
（実装前 174 OK / 45 MISSING / 1 WRONG / 7 ERR）。`run-spec` の Spec-Array /
Spec-Stream / Spec-Condition は全合格。既存 `run.scm` は 101/101 を維持。

I-C/I-E/I-F 追加後の `spec-probe extended`: **222 OK / 4 MISSING / 1 WRONG / 0 ERR**。

I-G/I-H 追加後の `spec-probe extended`: **225 OK / 2 MISSING / 0 WRONG / 0 ERR**。

undefined-entity 条件化後の `spec-probe extended`: **227 OK / 0 MISSING / 0 WRONG / 0 ERR**
（全 227 項目が標準準拠）。未定義関数/変数参照を `<undefined-function>`/
`<unbound-variable>`（`<undefined-entity>` の下位）コンディションとして送出するよう
評価器を変更し、`handler-case` で捕捉でき `undefined-entity-name`/`-namespace`
アクセサも機能するようになった。`<error>`/`<serious-condition>` での既存捕捉挙動は維持。
`run-spec` は **12/12 全合格**、`run.scm`/`run-strict` は 101/101、`oracle-compare` は
13 失敗（CLOS インスタンス同一性に起因する既存分、不変）。

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

1. ~~**[P0]** ネイティブ: `while`/`dotimes`/`dolist`/`tagbody`/`unwind-protect`/`format ~X`
   の誤出力を是正（まず誤値を返さない）。~~ ✅ **完了**（§2 P0 参照、DIFF 6→0）。
2. **[P1]** ネイティブ: `case`/`and`/`or` の lowering ✅、非局所制御
   （`block`/`catch`/`return-from`/`throw`）の setjmp/longjmp 脱出基盤 ✅。
3. **[P1]** ネイティブ: 数値ライブラリ（`expt`/`mod`/`abs`/`max`/`min`）+ 比較網羅
   （`/=`）✅。float 算術 ✅。
4. ~~**[P1]** インタプリタ: 多次元配列（I-A）、文字列ストリーム/`format-*`（I-B）、
   ゼロ除算条件と条件アクセサ（I-D）。~~ ✅ **完了**（§3 参照、spec-probe 174→203 OK）。
5. **[P2]** ネイティブ: 文字列・文字・ベクタ ⬜。インタプリタ: plist（I-C）✅、
   `create`/`initialize-object`（I-E）✅、数値補完（I-F）✅。
6. **[P2/P3]** ネイティブ: 例外・CLOS ⬜。インタプリタ: 逸脱是正（I-G）✅、
   `case-using`/`assure`（I-H）✅。
7. **[横断]** プリミティブ二重実装の共通化。
