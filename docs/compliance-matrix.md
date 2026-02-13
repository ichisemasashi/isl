# Compliance Matrix (M0 Baseline)

この文書は M0 の成果物として、`strict`（標準準拠）と `extended`（処理系拡張）の差分を固定する。

## Profiles

1. `strict`
- 目的: 標準準拠テストのベースライン
- 方針: 拡張特殊形式/拡張プリミティブを無効化

2. `extended`
- 目的: 現行 `isl` 互換
- 方針: 既存拡張を有効化

## Special Forms

1. `strict` + `extended` 共通
- `quote`, `quasiquote`, `if`, `cond`, `case`, `loop`, `while`, `do`, `dolist`, `dotimes`, `return-from`, `catch`, `throw`, `go`, `tagbody`, `lambda`, `defglobal`, `defvar`, `setq`, `setf`, `incf`, `defun`, `defmacro`, `defgeneric`, `defmethod`, `progn`, `block`, `let`, `let*`, `with-open-file`, `handler-case`, `defclass`

2. `extended` のみ
- `trace`, `untrace`, `defpackage`, `in-package`

## Primitive Functions

1. `strict` + `extended` 共通
- 算術/比較/リスト/ベクター/ハッシュ/文字列/制御の基本関数
- `load`, `provide`, `require`, `format`, `apply`, `funcall`, `make-instance`, `slot-value`, `class-of`, `instancep`

2. `extended` のみ
- デバッグ/システム: `debug`, `break`, `getenv`, `setenv`, `system`
- DB: `sqlite-*`, `postgres-*`, `mysql-*`
- Network: `tcp-*`, `udp-*`, `http-*`
- FFI: `ffi-call`, `load-foreign-library`, `current-foreign-library`

## Test Classification

1. Core (strict target)
- `/Volumes/SSD-PLU3/work/LISP/islisp/isl/test/conformance/run-strict.scm`
- 対象マイルストーン: `M0 M1 M2 M3 M4 M5 M6 M7 M8 M10 M11`

2. Extended
- `/Volumes/SSD-PLU3/work/LISP/islisp/isl/test/conformance/run.scm`
- 対象マイルストーン: `M0..M11`（`M9` を含む）
- `/Volumes/SSD-PLU3/work/LISP/islisp/isl/test/smoke.scm`

3. Oracle Compare Interface (M0)
- `/Volumes/SSD-PLU3/work/LISP/islisp/isl/test/conformance/oracle-compare.scm`
- 現状: oracle/candidate とも interpreter
- 将来: candidate を compiler 実行経路に置き換える

## Commands

1. strict conformance
```sh
gosh test/conformance/run-strict.scm
```

2. extended conformance
```sh
gosh test/conformance/run.scm
```

3. oracle compare (strict)
```sh
gosh test/conformance/oracle-compare.scm strict
```

4. oracle compare (extended)
```sh
gosh test/conformance/oracle-compare.scm extended
```
