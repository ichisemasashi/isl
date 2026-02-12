# isl

ISLISPの標準仕様に従ったISLISP処理系を目指すプロジェクトです。
実装基盤は Gauche 0.9.15 で、可能な限りR7RS相当の素朴な機能で構築します。

## 現在の実装範囲 (MVP)
- REPL
- ファイル実行
- 基本特殊形式: `quote`, `if`, `cond`, `case`, `loop`, `while`, `do`, `dolist`, `dotimes`, `return-from`, `catch`, `throw`, `handler-case`, `go`, `tagbody`, `trace`, `untrace`, `lambda`, `defpackage`, `in-package`, `defglobal`, `defvar`, `setq`, `setf`, `incf`, `defun`, `defmacro`, `defgeneric`, `defmethod`, `progn`, `block`, `let`, `let*`, `with-open-file`, `defclass`
- 基本組み込み関数: 算術・比較・数値(`mod`/`floor`/`ceiling`/`truncate`/`round`)・数値述語(`zerop`/`plusp`/`minusp`/`evenp`/`oddp`)・リスト(`first`〜`tenth` 含む)・ベクター(`vector`/`make-array`/`make-vector`/`vector-ref`/`vector-set!`)・ハッシュテーブル(`make-hash-table`/`gethash`/`puthash`/`remhash`/`clrhash`)・述語・高階関数(`mapcar`/`reduce`/`find`/`remove-if`/`remove-if-not`)・文字列(`string=`/`string-concat`/`substring`/`length`)・パッケージ(`find-package`/`use-package`/`export`/`intern`/`import`/`provide`/`require`)・マクロ(`gensym`/`macroexpand`/`macroexpand-1`)・オブジェクト(`make-instance`/`slot-value`/`class-of`/`instancep`)・DB(`sqlite-open`/`sqlite-exec`/`sqlite-query`/`postgres-open`/`postgres-query`/`mysql-open`/`mysql-query`)・ネットワーク(`tcp-connect`/`tcp-listen`/`tcp-accept`/`tcp-send`/`tcp-send-line`/`tcp-receive-char`/`tcp-receive-byte`/`tcp-receive-line`)・`apply`/`funcall`・`format`・`read`・`load`・`system`・環境変数(`getenv`/`setenv`)・デバッグ(`debug`/`break` + 特殊形式 `trace`/`untrace`)・時刻(`get-universal-time`/`get-internal-real-time`/`get-internal-run-time`/`internal-time-units-per-second`)・FFI(`ffi-call`) など
- FFI 互換レイヤ: `ffi:load-foreign-library` / `ffi:define-foreign-function`（`cffi` パッケージ経由の `:use` も可）

## 実行方法
```sh
./bin/isl
```

ファイル実行:
```sh
./bin/isl examples/hello.lsp
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
