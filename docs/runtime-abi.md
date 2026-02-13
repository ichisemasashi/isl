# Compiler Runtime ABI (M2 Minimal)

この文書は `isl` コンパイラ経路の M2 最小ランタイム ABI を定義する。

## 1. Value Representation

1. ランタイム値は `#(rt-value <tag> <payload>)`。
2. `<tag>` は最小集合: `number`, `string`, `char`, `boolean`, `null`, `symbol`, `pair`, `vector`, `function`, `object`。
3. `payload` はホスト値（Gauche 値）を保持する。

## 2. Function ABI

1. 関数値は `function` tag で保持。
2. 実体は以下のいずれか。
- closure: `#(closure <name> <params> <body-ir> <env>)`
- primitive: `#(primitive <name> <proc>)`

3. 呼出規約
- `runtime-apply(fn, args, state)`
- `args` は `rt-value` の列
- 戻り値は `rt-value`

## 3. Lexical Environment

1. 環境フレームは `#(env <parent> <bindings>)`。
2. `<bindings>` は `(symbol . rt-value)` の連想リスト。
3. 変数解決は lexical chain を親方向へ探索する。

## 4. Parameter Binding

1. 受理形
- required のみ: `(x y z)`
- `&rest`: `(x y &rest r)`
- rest-only: `r`（symbol）

2. エラー規約
- required 未満: `arity`
- required 超過（rest なし）: `arity`
- 不正パラメータ形: `invalid-params`

## 5. Error Object

1. ランタイムエラーは `#(runtime-error <code> <message> <details>)`。
2. 最小 code 集合
- `arity`
- `type-error`
- `unbound-variable`
- `invalid-ir`
- `invalid-unit`
- `unsupported-special`

## 6. Top-Level IR ABI

1. `define-fun`: `(define-fun <name> <params> <body-ir>)`
2. `expr`: `(expr <expr-ir>)`
3. `define-macro`: compile-time only として runtime では no-op

## 7. M2 Scope

1. 実装対象
- 値表現
- lexical 環境
- closure / rest 引数
- 最小 primitive (`+ - * / = < > <= >= list funcall not print`)

2. 非対象（M3以降）
- GC 管理
- 非局所制御
- 条件システム
- 完全 ISLISP primitive 互換
