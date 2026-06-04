# ISLISP 仕様カバレッジ／ギャップレポート

ISLISP (ISO/IEC 13816) 標準が定義する操作子・クラスに対して、本処理系
（インタプリタ）が何を実装し、何を欠いているかを実証的に整理した文書。

- 調査日: 2026-06-04
- 対象: `src/isl/core.scm`（インタプリタ）。プロファイル `extended` / `strict`
  いずれでも結果は同じ（欠落はプロファイルに依存しない）。
- 検証ツール:
  - `test/conformance/spec-probe.scm` — 標準操作子・クラスの存在/挙動を
    `OK / WRONG / ERR / MISSING` に分類するスキャナ。
  - `test/conformance/spec-milestones.scm` + `run-spec.scm` — 標準が要求する
    正しい振る舞いを `(kind form expected)` 形式で記述した準拠テスト。
    欠落/逸脱があるケースは意図的に FAIL する。

```sh
# 欠落スキャン（章別サマリ＋ MISSING/WRONG/ERR 一覧）
gosh test/conformance/spec-probe.scm extended
gosh test/conformance/spec-probe.scm strict

# 準拠テスト（標準どおりに動くかを判定／ギャップ追跡）
gosh test/conformance/run-spec.scm extended
gosh test/conformance/run-spec.scm strict
```

## サマリ（spec-probe / extended）

227 項目を検査して以下の通り:

| 区分 | 件数 | 意味 |
|------|------|------|
| OK | 174 | 標準どおり動作 |
| MISSING | 45 | 操作子・クラスそのものが未定義 |
| WRONG | 1 | 存在するが結果が標準と相違 |
| ERR | 7 | 存在するが想定外のエラー（主に多次元配列・ゼロ除算条件） |

章別の NG（OK 以外）件数:

| 章 | NG | 主な欠落 |
|----|----|----------|
| 01 Numbers | 9 | `div`, `sinh/cosh/tanh/atanh`, `atan2`, `*most-positive/negative-float*` |
| 02 Predicates | 3 | `generic-function-p`, 多次元配列述語 |
| 03 Cons/List | 0* | （`set-car`/`set-cdr` は引数順序逸脱・後述） |
| 04 Sequence | 0 | — |
| 05 Character | 0 | — |
| 06 String | 0 | — |
| 07 Array/Vector | 5 | 多次元配列全般・`garef`/`set-garef` |
| 08 Symbol | 3 | `property`/`set-property`/`remove-property`（plist API 全欠落）|
| 09 Stream/IO | 13 | 文字列ストリーム・`preview-char`・`format-*` 群 |
| 10 Condition | 10 | 条件アクセサ群・`report-condition`・ゼロ除算条件 |
| 11 Object System | 2 | `create`/`initialize-object`（標準のインスタンス生成 API）|
| 12 Control Forms | 2 | `case-using`, `assure` |
| 13 Misc | 0 | — |
| 14 Standard Classes | 6 | `<basic-array*>` 他クラス（後述）|

## 重大なギャップ（カテゴリ別）

### A. 多次元配列が未対応（§13）
`create-array` が 1 要素の次元リスト（= 1 次元）しか受け付けない。
`(create-array '(2 3) 0)` は `"dimension must be non-negative integer or
one-element list"` でエラー。これにより以下が連鎖的に機能しない:

- `create-array`（多次元）, `aref`/`set-aref`（多添字）, `array-dimensions`
- `garef` / `set-garef`（一般配列要素アクセス）… **そもそも未定義**
- `basic-array-p`, `basic-array*-p`, `general-array*-p`（多次元配列に対して）
- クラス `<basic-array*>`, `<general-array*>`

### B. 文字列ストリームと粒度の細かい出力 API が未実装（§18/§19）
ISLISP の I/O は文字列ストリームと `format-*` 系を中核に据えるが、いずれも欠落:

- `create-string-input-stream`, `create-string-output-stream`,
  `get-output-stream-string` … **未定義**
- `preview-char`（先読みの標準名。実装には拡張の `peek-char` のみ）
- `format-integer`, `format-char`, `format-float`, `format-object`,
  `format-fresh-line`, `format-tab` … **未定義**
  （実装には Common Lisp 風の `format` のみ）

### C. プロパティリスト API が全欠落（§10）
- `property`, `set-property`, `remove-property` … **すべて未定義**

### D. コンディションのアクセサ・報告 API が未実装（§22）
クラス階層（`<error>` 等）と `with-handler`/`handler-case` は概ね動作するが、
標準のコンディション・アクセサと報告関数が欠落:

- `simple-error-format-string`, `simple-error-format-arguments`
- `domain-error-object`, `domain-error-expected-class`
- `arithmetic-error-operation`, `arithmetic-error-operands`
- `undefined-entity-name`, `undefined-entity-namespace`
- `report-condition`
- `<storage-exhausted>` クラス
- **ゼロ除算が捕捉不可**: `(quotient 1 0)` / `(/ 1 0)` が Gauche 生のエラーを
  投げ、`<division-by-zero>` ハンドラで捕捉できない。

### E. オブジェクトシステムのインスタンス生成 API が非標準（§21）
- 標準の `create`（ジェネリック関数）と `initialize-object` が **未定義**。
  実装は Common Lisp 由来の `make-instance` を使う（非標準）。
- クラス `<generic-function>`, `<standard-generic-function>`,
  `<standard-class>` が未定義（`(class <generic-function>)` が失敗）。

### F. 数値ライブラリの欠落（§11）
- 整数除算 `div`（床関数）… 未定義（`mod` は存在）。
- 双曲線関数 `sinh`, `cosh`, `tanh`, `atanh` … 未定義。
- `atan2`（2 引数 atan）… 未定義。
- 浮動小数点限界定数 `*most-positive-float*`, `*most-negative-float*` … 未定義。
- 述語 `generic-function-p` … 未定義。

### G. 引数順序・セマンティクスの逸脱
- `set-car` / `set-cdr`: 標準は `(set-car obj cons)` だが実装は
  `(set-car cons obj)` と **引数が逆**（`src/isl/core.scm:4204`）。
- `error` のフォーマット: `(error "n=~a" 5)` の `condition-message` が
  `"n=~a"` のまま（引数が展開されない）。標準では `report-condition` が
  フォーマット文字列＋引数を整形して出力する。

## 注意: 「欠落ではない」誤検出を避けるための確認事項

調査の過程で、Common Lisp の習慣と ISLISP 標準が異なる点を確認した。
以下は **実装が正しく、テスト側を標準に合わせた** 例:

- 数値比較 `=`, `/=`, `<`, `<=`, `>`, `>=` は ISLISP では **2 引数**。
  実装が `(= 1 1 1)` を拒否するのは標準準拠（n-ary は CL 仕様）。
- `floor`/`ceiling`/`round`/`truncate` は ISLISP では **1 引数**。

## 実装済み（OK）の主な範囲

- 算術の基本（`+ - * /`, `quotient`, `mod`, `gcd`, `lcm`, `isqrt`, `expt`,
  `abs`, `max`, `min`, 1 引数の丸め関数）と 2 引数比較。
- 型述語（`integerp` `floatp` `numberp` `characterp` `stringp` `symbolp`
  `consp` `listp` `null` `functionp` `instancep` `subclassp`）。
- Cons/リスト処理（`cons car cdr create-list list reverse append member assoc`
  `mapcar`(多リスト) `mapcan maplist` 等）。
- シーケンス（`length elt set-elt subseq map-into`）。
- 文字・文字列（比較群, `create-string`, `string-append`, `char-index`,
  `string-index`）。
- 制御構造（`if cond case and or block/return-from catch/throw tagbody/go`
  `for while unwind-protect the flet labels let/let* dynamic-let convert`
  `apply funcall`）。
- CLOS の中核（`defclass defgeneric defmethod call-next-method next-method-p`
  メソッド限定子 `:before/:after/:around`、クラス階層、`class-of`）。
- コンディションのクラス階層と `handler-case` / `with-handler` / `ignore-errors`。
- 一般ベクタ（`vector create-vector general-vector-p` と 1 次元 `aref`）。
- 標準クラスの多く（`<integer> <float> <number> <string> <character>`
  `<symbol> <list> <cons> <null> <stream> <error>` 系階層など）。

## コンパイラ側について

本レポートはインタプリタ（`eval-islisp`）を対象とした。コンパイラ側の
ギャップは `test/compiler/native-gap-probe.scm` で実測済みで、**ネイティブ
AOT バックエンド（`./bin/islc --run`）は 49 項目中 17 のみ対応**、26 が未対応、
**6 が黙って誤った値を返す**（`while`/`dotimes`/`dolist` が 0 を返す等）。
一方 `--jit`（Scheme ランタイム）経路はインタプリタとほぼ同等。

詳細と優先度付きの修正計画は [remediation-plan.md](remediation-plan.md) を参照。

## 推奨対応順（インパクト大→小）

1. 多次元配列 + `garef`/`set-garef` + 配列述語/クラス（§13, カテゴリ A）
2. 文字列ストリーム + `format-*` 群 + `preview-char`（§18/19, カテゴリ B）
3. コンディションアクセサ + `report-condition` + ゼロ除算条件（§22, カテゴリ D）
4. `create`/`initialize-object` とジェネリック関数クラス（§21, カテゴリ E）
5. プロパティリスト API（§10, カテゴリ C）
6. 数値の補完: `div`, 双曲線関数, `atan2`, 限界定数, `generic-function-p`
7. 逸脱の是正: `set-car`/`set-cdr` 引数順序, `error` のフォーマット展開
8. 特殊形式 `case-using`, `assure`

検証用の正典テストは `test/conformance/spec-milestones.scm` にあり、
各ケースに `[GAP]` コメントで該当項目を明記している。実装が進むたびに
`gosh test/conformance/run-spec.scm` の FAIL が減るので、進捗指標になる。
