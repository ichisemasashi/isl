# isl

ISLISPの標準仕様に従ったISLISP処理系を目指すプロジェクトです。
実装基盤は Gauche 0.9.15 で、可能な限りR7RS相当の素朴な機能で構築します。

## 現在の実装範囲 (MVP)
- REPL
- ファイル実行
- 基本特殊形式: `quote`, `if`, `cond`, `case`, `loop`, `while`, `do`, `dolist`, `dotimes`, `return-from`, `catch`, `throw`, `handler-case`, `go`, `tagbody`, `trace`, `untrace`, `lambda`, `defpackage`, `in-package`, `defglobal`, `defvar`, `setq`, `setf`, `incf`, `defun`, `defmacro`, `defgeneric`, `defmethod`, `progn`, `block`, `let`, `let*`, `with-open-file`, `defclass`
- 基本組み込み関数: 算術・比較・数値(`mod`/`floor`/`ceiling`/`truncate`/`round`)・数値述語(`zerop`/`plusp`/`minusp`/`evenp`/`oddp`)・リスト(`first`〜`tenth` 含む)・ベクター(`vector`/`make-array`/`make-vector`/`vector-ref`/`vector-set!`)・ハッシュテーブル(`make-hash-table`/`gethash`/`puthash`/`remhash`/`clrhash`)・述語・高階関数(`mapcar`/`reduce`/`find`/`remove-if`/`remove-if-not`)・文字列(`stringp`/`create-string`/`string=`/`string/=`/`string<`/`string<=`/`string>`/`string>=`/`string-concat`/`string-append`/`substring`/`char-index`/`string-index`/`length`)・パッケージ(`find-package`/`use-package`/`export`/`intern`/`import`/`provide`/`require`)・マクロ(`gensym`/`macroexpand`/`macroexpand-1`)・オブジェクト(`make-instance`/`slot-value`/`class-of`/`instancep`)・DB(`sqlite-open`/`sqlite-exec`/`sqlite-query`/`postgres-open`/`postgres-query`/`mysql-open`/`mysql-query`)・ネットワーク(`tcp-*`/`udp-*`/`http-get`/`http-head`/`http-post`)・`apply`/`funcall`・`format`・`read`・`load`・`system`・環境変数(`getenv`/`setenv`)・デバッグ(`debug`/`break` + 特殊形式 `trace`/`untrace`)・時刻(`get-universal-time`/`get-internal-real-time`/`get-internal-run-time`/`internal-time-units-per-second`)・FFI(`ffi-call`) など
- FFI 互換レイヤ: `ffi:load-foreign-library` / `ffi:define-foreign-function`（`cffi` パッケージ経由の `:use` も可）

## 実行方法
```sh
./bin/isl
```

準拠プロファイルを指定:
```sh
./bin/isl --profile strict
./bin/isl --profile extended
```

ファイル実行:
```sh
./bin/isl examples/hello.lsp
```

`strict` でファイル実行:
```sh
./bin/isl --profile strict examples/hello.lsp
```

## 例
```lisp
(defglobal x 10)
(defun fact (n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

(print (fact x))
```

## 今後の拡張方針
1. ISLISP仕様の章ごとに機能を追加 (オブジェクトシステム・条件システム・ストリーム等)
2. 仕様項目ごとに準拠テストを追加
3. R7RS依存部分を明示し、Gauche拡張依存を最小化

## 準拠テスト (M0)
strict conformance:
```sh
gosh test/conformance/run-strict.scm
```

extended conformance:
```sh
gosh test/conformance/run.scm
```

oracle compare:
```sh
gosh test/conformance/oracle-compare.scm strict
gosh test/conformance/oracle-compare.scm extended
```

## コンパイラ前段 (M1)
フロントエンド（`read -> macroexpand -> normalize(IR)`）の IR ダンプ:
```sh
./bin/islc-front --dump-ir examples/hello.lsp
```

フロントエンド smoke テスト:
```sh
gosh test/compiler/frontend-smoke.scm
```

## コンパイラランタイム最小核 (M2)
ランタイム経路でファイル実行:
```sh
./bin/islc-run examples/hello.lsp
```

M2 smoke テスト:
```sh
gosh test/compiler/runtime-m2-smoke.scm
```

M2 oracle 比較（interpreter と結果一致）:
```sh
gosh test/compiler/runtime-m2-oracle.scm
```

ランタイム ABI:
```text
docs/runtime-abi.md
```

## M3 向け IR 分解（LLVM 直交CFG）
`frontend` は正規化IRに加えて、`ll`（基本ブロックCFG）を出力します。  
`ll` は `assign/call/br/jmp/ret/phi` ノードで構成され、LLVM IR へ 1 対 1 で落とせる粒度です。

確認:
```sh
./bin/islc-front --dump-ir examples/hello.lsp
gosh test/compiler/lowering-smoke.scm
```

## M3. LLVM AOT バックエンド最小実装
LLVM IR モジュールを生成:
```sh
./bin/islc-llvm --mode module -o out.ll examples/hello.lsp
```

LLVM IR + 実行用 `main` を生成:
```sh
./bin/islc-llvm --mode aot -o out-aot.ll examples/hello.lsp
```

ネイティブバイナリ生成（`clang` 必須）:
```sh
./bin/islc-aot -o hello-aot examples/hello.lsp
./hello-aot
```

M3 codegen / AOT smoke:
```sh
gosh test/compiler/codegen-smoke.scm
gosh test/compiler/aot-smoke.scm
./test/compiler/aot-run-smoke.sh
```

## M4. 評価順序と副作用の完全固定
compiler runtime 経路で `setq/let/setf` を実装し、
副作用を伴う式の評価順（左から）を固定します。

M4 smoke / oracle:
```sh
gosh test/compiler/runtime-m4-smoke.scm
gosh test/compiler/runtime-m4-oracle.scm
```

## M5. 非局所制御の実装
compiler runtime 経路で `block/return-from`, `catch/throw`, `tagbody/go` を実装します。

M5 smoke / oracle:
```sh
gosh test/compiler/runtime-m5-smoke.scm
gosh test/compiler/runtime-m5-oracle.scm
```
