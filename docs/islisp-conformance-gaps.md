# ISLISP 標準準拠ギャップ一覧

基準: ISO/IEC 13816:2007 (ISLISP)  
対象: `src/isl/core.scm`（インタープリタ実装）  
調査日: 2026-05-03

この文書は **未実装・非準拠の機能を網羅的に列挙**し、今後の実装作業の入力とする。  
すでに動作している機能（`defun`, `defclass`, `let`, `block` 等）は記載しない。

---

## 1. 特殊形式（Special Forms）

### 1-1. `flet` — ローカル関数束縛

**標準定義**（§8.1）:

```
(flet (({function-name lambda-list form*})*) body-form*)
```

レキシカルスコープのローカル関数束縛。束縛は相互参照不可（`labels` と対照）。  
現在の isl には実装なし。`defun` のグローバル束縛で代替できないケースがある。

**実装方針**: `eval-let-form` と同様の構造で、関数オブジェクトとして環境に登録する。

---

### 1-2. `labels` — 再帰可能ローカル関数束縛

**標準定義**（§8.1）:

```
(labels (({function-name lambda-list form*})*) body-form*)
```

`flet` と同形だが、束縛同士が相互参照可能（相互再帰）。  
現在の isl には実装なし。

**実装方針**: `flet` 実装後、環境を共有してから各クロージャを登録する。

---

### 1-3. `the` — 型宣言

**標準定義**（§12.1）:

```
(the class-name form)
```

`form` の値が `class-name` のインスタンスでなければエラー。最適化ヒントにもなる。

**実装方針**: `eval-islisp*` の結果を `instancep` で検査し、不一致なら `<domain-error>` を通知する。  
strict モードでは必須。

---

### 1-4. `with-handler` — 標準条件ハンドラ

**標準定義**（§29.2）:

```
(with-handler handler-form protected-form*)
```

`handler-form` は引数 1 つの関数。条件発生時に動的に呼び出される。  
`handler-case` とは異なり、ハンドラが値を返すと継続的に実行が再開できる（continuable condition）。

**現状**: `handler-case`（Common Lisp 互換）で代替しているが意味論が異なる。  
`with-handler` は捕捉後の継続が可能であり、`handler-case` は非局所脱出に相当する。

**実装方針**:
1. ハンドラスタック（動的変数）を導入する。
2. `signal-condition` がハンドラスタックを探索し、見つかったハンドラを呼び出す。
3. ハンドラが正常リターンした場合は `signal-condition` の呼び出し元に継続する。

---

### 1-5. `ignore-errors` — エラー無視

**標準定義**（§29.3）:

```
(ignore-errors form*)
```

`<serious-condition>` が発生しても `nil` を返して継続する。  
現在の isl には実装なし。

**実装方針**: `(handler-case (progn ...) (<serious-condition> () nil))` に展開するマクロで実現可能。

---

### 1-6. `unwind-protect` — クリーンアップ保証

**標準定義**（§30.3）:

```
(unwind-protect protected-form cleanup-form*)
```

非局所脱出（`return-from`, `throw`, エラー）が発生した場合でも `cleanup-form*` を必ず実行する。  
現在の isl には実装なし。

**実装方針**: Gauche の `dynamic-wind` を使うと実装できる。IR レベルでは `UNWIND_PROTECT_ENTER/EXIT` ノードを導入する。

---

### 1-7. `with-open-input-file` / `with-open-output-file`

**標準定義**（§31.3）:

```
(with-open-input-file  (var filename-form option*) body-form*)
(with-open-output-file (var filename-form option*) body-form*)
```

ISLISP は入力・出力を別の特殊形式として分離している。  
現在の isl は `with-open-file` に統合しており、標準名が存在しない。

**実装方針**: `with-open-input-file` と `with-open-output-file` を `with-open-file` へのマクロ展開として定義する。

---

### 1-8. `dynamic` / `dynamic-let` — 動的変数

**標準定義**（§12.7）:

```
(dynamic var)
(dynamic-let ((var form)*) body-form*)
```

ISLISP はレキシカルスコープの変数（`let`）と動的スコープの変数（`dynamic-let`）を明確に分離する。  
`defvar` で宣言した変数は動的変数であり、`(dynamic var)` でアクセスする。  
現在の isl は `defvar` を定義しているが、`(dynamic var)` アクセス形式がなく、通常の `var` 参照と同一視されている。

**実装方針**:
1. 動的変数のアクセスを `frame-find-pair` の動的スコープ版として分離する。
2. `dynamic-let` で動的変数をシャドーイングできるようにする。

---

### 1-9. `defconstant` — 定数定義

**標準定義**（§12.5）:

```
(defconstant name form)
```

`defglobal` と類似するが、再束縛は処理系エラーとなる。  
現在の isl には実装なし（`defglobal` で代替）。

---

## 2. 標準関数

### 2-1. 算術・数値

| 関数 | シグネチャ | 備考 |
|------|-----------|------|
| `abs` | `(abs number)` | 絶対値 |
| `max` | `(max number+)` | 最大値 |
| `min` | `(min number+)` | 最小値 |
| `expt` | `(expt base exponent)` | 指数演算 |
| `sqrt` | `(sqrt number)` | 平方根（戻り値は float） |
| `rem` | `(rem dividend divisor)` | 剰余（`mod` とは符号規則が異なる） |
| `gcd` | `(gcd integer integer)` | 最大公約数 |
| `lcm` | `(lcm integer integer)` | 最小公倍数 |
| `exp` | `(exp number)` | e の累乗 |
| `log` | `(log number)` | 自然対数 |
| `sin` | `(sin number)` | 正弦（ラジアン） |
| `cos` | `(cos number)` | 余弦 |
| `tan` | `(tan number)` | 正接 |
| `asin` | `(asin number)` | 逆正弦 |
| `acos` | `(acos number)` | 逆余弦 |
| `atan` | `(atan y &optional x)` | 逆正接（2引数版は atan2） |
| `float` | `(float number)` | 整数 → 浮動小数点変換 |
| `floor` | `(floor number divisor?)` | 床関数（2引数版: 商と剰余） |
| `ceiling` | `(ceiling number divisor?)` | 天井関数（2引数版） |
| `truncate` | `(truncate number divisor?)` | 切り捨て（2引数版） |
| `round` | `(round number divisor?)` | 丸め（2引数版） |
| `quotient` | `(quotient dividend divisor)` | 整数商（`truncate` の整数版） |
| `number->string` | `(number->string number &optional radix)` | 数値 → 文字列 |
| `string->number` | `(string->number string &optional radix)` | 文字列 → 数値（失敗時 nil） |
| `isqrt` | `(isqrt n)` | 整数平方根 |
| `signum` | `(signum number)` | 符号 |
| `negate` | `(negate number)` | 符号反転（`-` の1引数版） |

**注意**: `floor`, `ceiling`, `truncate`, `round` は現在1引数のみ実装。2引数版（商と剰余の多値）が不足。  
ISLISP では多値を返さず、商のみ返す仕様（Common Lisp とは異なる）。

---

### 2-2. 型述語

| 関数 | シグネチャ | 備考 |
|------|-----------|------|
| `consp` | `(consp obj)` | cons セル判定 |
| `characterp` | `(characterp obj)` | 文字型判定 |
| `integerp` | `(integerp obj)` | 整数型判定（`numberp` より細かい） |
| `floatp` | `(floatp obj)` | 浮動小数点型判定 |
| `functionp` | `(functionp obj)` | 関数型判定（クロージャ・プリミティブ含む） |
| `input-stream-p` | `(input-stream-p obj)` | 入力ストリーム判定 |
| `output-stream-p` | `(output-stream-p obj)` | 出力ストリーム判定 |
| `general-vector-p` | `(general-vector-p obj)` | ベクター判定（`vectorp` で代替中） |
| `general-array*-p` | `(general-array*-p obj)` | 多次元配列判定 |

---

### 2-3. リスト操作

| 関数 | シグネチャ | 備考 |
|------|-----------|------|
| `reverse` | `(reverse list)` | リスト逆順コピー |
| `nreverse` | `(nreverse list)` | 破壊的逆順（オプション） |
| `map` | `(map function list+)` | `mapcar` に相当。引数順はISLISP標準 |
| `for-each` | `(for-each function list+)` | 副作用用 map（戻り値未規定） |
| `member` | `(member obj list &key test)` | リスト内包判定 |
| `assoc` | `(assoc key alist &key test)` | 連想リスト検索 |
| `list-ref` | `(list-ref list i)` | インデックスアクセス（`nth` 相当） |
| `list-tail` | `(list-tail list i)` | i 番目以降のサブリスト |
| `last-pair` | `(last-pair list)` | 末尾 cons セル |
| `create-list` | `(create-list n &optional init)` | 長さ n のリスト生成（`make-list` 相当） |
| `list-copy` | `(list-copy list)` | 浅いコピー |
| `nconc` | `(nconc list*)` | 破壊的 append |
| `mapc` | `(mapc function list+)` | 副作用のみ、第1リストを返す |
| `mapcan` | `(mapcan function list+)` | 結果リストを nconc |
| `maplist` | `(maplist function list+)` | cdr 部分を対象に map |
| `remove` | `(remove obj list &key test)` | 要素除去（非破壊） |
| `nthcdr` | `(nthcdr n list)` | n 回 cdr |
| `sort` | `(sort list predicate)` | ソート |

**注意**: `mapcar` は実装済みだが、ISLISP の標準名は `map` である。  
strict モードでは `map` が必要。

---

### 2-4. 文字列操作

| 関数 | シグネチャ | 備考 |
|------|-----------|------|
| `string-length` | `(string-length string)` | 文字列長（`length` はリスト用） |
| `string-ref` | `(string-ref string i)` | i 番目の文字を返す |
| `string-copy` | `(string-copy string)` | 文字列のコピー |
| `string-upcase` | `(string-upcase string)` | 大文字変換 |
| `string-downcase` | `(string-downcase string)` | 小文字変換 |
| `string-contains` | `(string-contains string substr)` | 部分文字列検索（非標準だが有用） |

**注意**: `length` は現在リスト・文字列・ベクター兼用だが、ISLISP では型別に分離されている。

---

### 2-5. 文字操作

| 関数 | シグネチャ | 備考 |
|------|-----------|------|
| `char=` | `(char= c1 c2)` | 文字比較（等値） |
| `char<` | `(char< c1 c2)` | 文字比較（小なり） |
| `char>` | `(char> c1 c2)` | 文字比較（大なり） |
| `char<=` | `(char<= c1 c2)` | 文字比較 |
| `char>=` | `(char>= c1 c2)` | 文字比較 |
| `char-upcase` | `(char-upcase char)` | 大文字化 |
| `char-downcase` | `(char-downcase char)` | 小文字化 |

---

### 2-6. ベクター（ISLISP 標準名）

ISLISP ではベクター関連の標準名が Scheme と異なる。

| ISLISP 標準名 | isl 現状 | 備考 |
|--------------|---------|------|
| `create-vector` | 未実装（`make-vector` で代替） | 初期値付きベクター生成 |
| `general-vector-ref` | `vector-ref` で代替 | ベクター要素アクセス |
| `(setf general-vector-ref)` | `vector-set!` で代替 | ベクター要素設定 |
| `general-vector-p` | `vectorp` で代替 | ベクター型判定 |

---

### 2-7. 配列（多次元）

ISLISP は多次元配列を規定している。

| 関数 | シグネチャ | 備考 |
|------|-----------|------|
| `create-array` | `(create-array dimensions &optional init)` | 多次元配列生成 |
| `aref` | `(aref array index+)` | 実装済み（要検証） |
| `(setf aref)` | — | 配列要素設定（`setf` 経由） |
| `array-dimensions` | `(array-dimensions array)` | 次元リスト |

---

### 2-8. I/O・ストリーム

ISLISP のストリームは「ストリームオブジェクト」として一級市民である。

#### ストリーム生成・終了

| 関数 | シグネチャ | 備考 |
|------|-----------|------|
| `open-input-stream` | `(open-input-stream filename)` | 入力ストリームを開く |
| `open-output-stream` | `(open-output-stream filename)` | 出力ストリームを開く（常に:supersede） |
| `open-io-stream` | `(open-io-stream filename)` | 入出力ストリーム |
| `close` | `(close stream)` | ストリームを閉じる |

#### 標準ストリーム変数

| 変数 | 説明 |
|------|------|
| `*standard-input*` | 標準入力 |
| `*standard-output*` | 標準出力 |
| `*error-output*` | エラー出力 |

#### 入出力関数（ストリーム引数付き）

| 関数 | シグネチャ | 備考 |
|------|-----------|------|
| `read` | `(read &optional stream)` | 実装済みだがストリーム引数なし |
| `read-char` | `(read-char &optional stream)` | 1文字読み込み |
| `peek-char` | `(peek-char &optional stream)` | 先読み（消費しない） |
| `read-line` | `(read-line &optional stream)` | 実装済みだがストリーム引数要確認 |
| `write` | `(write obj &optional stream)` | read で読み戻せる形式で出力 |
| `write-char` | `(write-char char &optional stream)` | 1文字出力 |
| `terpri` | `(terpri &optional stream)` | 改行出力 |
| `format` | `(format stream-or-t fmt arg*)` | 実装済みだがストリーム引数に `t`/`nil` の扱い要確認 |
| `print` | `(print obj &optional stream)` | 実装済みだが標準の `print` は改行付き |

**注意**: ISLISP の `write` は Common Lisp の `write`/`prin1` に相当する（エスケープあり）。  
現在の `print` がどちらの意味論かを確認・整理する必要がある。

#### `eof-value` の扱い

ISLISP では EOF 検出に `end-of-stream` 条件を使う（`eof-object?` は Scheme の概念）。  
`read-char` 等が EOF に達したとき `<end-of-stream>` 条件を通知するか、  
オプションの `eof-error-p` と `eof-value` 引数で制御する設計が必要。

---

### 2-9. 条件システム（Condition System）

ISLISP の条件システムは ISO 標準の重要な部分であり、現在の isl では不完全。

#### 組み込み条件クラス階層

```
<condition>
  <serious-condition>
    <error>
      <arithmetic-error>
        <division-by-zero>
        <floating-point-overflow>
        <floating-point-underflow>
      <control-error>
      <parse-error>
      <program-error>
        <domain-error>         ; 型不一致
        <undefined-entity>
          <unbound-variable>
          <undefined-function>
        <arity-error>          ; 引数数の不一致
        <index-out-of-range>
        <type-error>
      <simple-error>
      <stream-error>
        <end-of-stream>
```

#### 条件操作関数

| 関数 | シグネチャ | 備考 |
|------|-----------|------|
| `signal-condition` | `(signal-condition condition continuable)` | 条件を通知する |
| `condition-continuable` | `(condition-continuable condition)` | 継続可能かを返す |
| `continue-condition` | `(continue-condition condition &optional value)` | 継続する |
| `error` | `(error error-string argument*)` | `<simple-error>` を通知（実装要確認） |
| `cerror` | `(cerror continue-string error-string arg*)` | 継続可能エラー |
| `condition-message` | `(condition-message condition)` | エラーメッセージ取得 |

#### 現状の問題

- `handler-case` は `<serious-condition>` のインスタンスを条件として受け取るが、現在の実装は Gauche の native 例外オブジェクトをそのまま渡しており、ISLISP の条件クラス階層と対応していない。
- `<error>` 以外の条件クラス（`<arithmetic-error>`, `<domain-error>` 等）が存在しない。
- `signal-condition` / `condition-continuable` / `continue-condition` が未実装。

---

### 2-10. OOP 関連（不足分）

| 機能 | 標準参照 | 備考 |
|------|---------|------|
| `initialize-object` | §25.4 | `make-instance` 時に呼ばれる初期化メソッド |
| `standard-object` | §25.1 | ユーザ定義クラスのデフォルトスーパークラス |
| `built-in-class` | §25.1 | 組み込み型のクラスオブジェクト |
| `class` | `(class class-name)` | クラスオブジェクトを返す |
| `subclassp` | `(subclassp sub super)` | サブクラス関係の判定 |
| `call-next-method` | §26.3 | スーパーメソッドの呼び出し |
| `next-method-p` | §26.3 | 次のメソッドが存在するか |
| `:before` / `:after` / `:around` メソッド | §26.4 | 補助メソッド |
| `slot-boundp` | — | スロットが束縛されているか判定 |
| `slot-makunbound` | — | スロットを未束縛に戻す |

---

### 2-11. 関数オブジェクト

| 関数 | シグネチャ | 備考 |
|------|-----------|------|
| `function` | `(function name)` | 関数オブジェクトを取得（`#'name` 相当） |

**注意**: ISLISP では `(function name)` が特殊形式として定義されている。  
現在の isl では `'name` でクロージャを束縛から取り出すことで代替しているが、  
名前空間（関数セル vs 値セル）の分離がないため意味論が異なる。

---

## 3. 意味論的な相違・修正が必要な箇所

### 3-1. `nil` と `#f` の二重性

**問題**: ISLISP では `nil`（= `'()`）のみが偽。`#f` という概念はない。  
現在の isl は `truthy?` で `#f` と `'()` の両方を偽と判定している（Scheme の影響）。

**修正方針**: strict モードでは `#f` を真として扱い、`nil`（`'()`）のみを偽とする。  
または、内部的に `nil` と `#f` を統一する（`nil` = `#f` として一つの値にする）。

### 3-2. `t` と `#t` の統一

**問題**: ISLISP では真値は `t` のみ。`#t` という真値はない。  
内部で Gauche の `#t` を使っているため、ユーザコードで `#t` が機能してしまう。

**修正方針**: strict モードでは `#t` リテラルを認識しないか、`t` と同一視する。

### 3-3. `handler-case` → `with-handler` の意味論修正

**問題**: `handler-case` は非局所脱出（動的 GOTO）だが、`with-handler` は継続呼び出しである。  
これは本質的に異なる制御構造であり、`with-handler` を `handler-case` で代替することはできない。

**修正方針**: `with-handler` を新規実装し、継続可能条件（continuable condition）を正しく扱う。

### 3-4. `format` のストリーム引数

**問題**: ISLISP の `format` は第1引数がストリーム（`t` で標準出力、`nil` で文字列生成）。  
現在の実装の仕様を確認し、`t`/`nil`/ストリームオブジェクトを正しく処理する必要がある。

### 3-5. `vector-ref` / `vector-set!` の名称

**問題**: ISLISP 標準は `general-vector-ref` / `(setf general-vector-ref)` を使う。  
`vector-ref` は Scheme 名称であり、strict モードでは標準名が必要。

**修正方針**: `general-vector-ref` を `vector-ref` の別名として定義するか、  
`(setf general-vector-ref ...)` の `setf` 拡張を実装する。

### 3-6. `defvar` の動的スコープ

**問題**: ISLISP の `defvar` は動的変数を宣言し、`(dynamic var)` でアクセスする。  
現在の isl では `defvar` が定義されているが、アクセスが通常の変数参照と同一視されており、  
動的スコープのセマンティクス（`dynamic-let` によるシャドーイング）が実装されていない。

---

## 4. 実装優先度の提案

準拠度向上のコストパフォーマンスを考慮した優先順位:

### 優先度: 高（標準機能として頻繁に使用される）

1. `abs`, `max`, `min`, `expt`, `sqrt`（算術 §17）
2. `reverse`, `for-each`, `map`, `member`, `assoc`, `list-ref`（リスト §15）
3. `string-length`, `string-ref`（文字列 §18）
4. `consp`, `characterp`, `integerp`, `floatp`, `functionp`（型述語 §13）
5. `flet`, `labels`（ローカル関数 §8）
6. `unwind-protect`（制御 §30）
7. `with-open-input-file` / `with-open-output-file`（マクロ展開で対応可）
8. `number->string`, `string->number`（変換 §17/§18）

### 優先度: 中（条件システム・ストリーム）

9. `<error>` 条件クラス階層の定義
10. `signal-condition` / `condition-continuable` / `continue-condition`
11. `with-handler`（§29.2）
12. `read-char`, `write-char`, `peek-char`（文字I/O §31）
13. `open-input-stream`, `open-output-stream`, `close`
14. `write` / `terpri`（出力 §31）

### 優先度: 低（拡張・マイナー機能）

15. `the`（§12.1）
16. 三角関数 `sin`, `cos`, `tan`, `asin`, `acos`, `atan`（§17）
17. `exp`, `log`（§17）
18. `dynamic` / `dynamic-let`（§12.7）
19. `call-next-method`, `next-method-p`（§26.3）
20. `:before`/`:after`/`:around` メソッド（§26.4）
21. `subclassp`, `class` 関数（§25）
22. `ignore-errors`, `cerror`（§29.3）
23. 文字比較関数 `char=`, `char<` 等（§18）
24. `create-list`, `create-vector`（ISLISP 標準名）
25. `defconstant`（§12.5）

---

## 5. テスト追加が必要な領域

現在の `test/conformance/milestones.scm` は独自マイルストーン（M0〜M11）ベースであり、  
ISO 標準の章立てに対応したテストが不足している。

### 追加すべきテストカテゴリ

| カテゴリ | 標準参照 | 現状 |
|----------|---------|------|
| 算術関数網羅 | §17 | 不足 |
| 文字列関数網羅 | §18 | 不足 |
| 文字操作 | §18.6 | ほぼなし |
| ストリームI/O | §31 | ほぼなし |
| 条件システム | §29 | 最小限 |
| `flet`/`labels` | §8 | なし |
| `with-handler` | §29.2 | なし |
| `unwind-protect` | §30.3 | なし |
| OOP 完全性 | §25-27 | 最小限 |
| 動的変数 | §12.7 | なし |
| 型クラス階層 | §13 | 最小限 |

---

## 6. 参照

- ISO/IEC 13816:2007 — ISLISP 標準仕様（全章）
- `docs/compliance-matrix.md` — strict/extended プロファイル差分
- `docs/compiler-compliance-spec.md` — コンパイラ向け難所仕様
- `test/conformance/milestones.scm` — 現行テストケース定義
