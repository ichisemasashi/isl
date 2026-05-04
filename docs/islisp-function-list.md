# ISLISP Function List (ISO/IEC 13816:2007)

Implementation status based on `src/isl/core.scm`.

- **実装済** = implemented
- **未実装** = not implemented
- **別名あり** = available under an alias

---

## §3 Syntax / Special Forms

| §章番号 | 機能名 | 実装状況 |
|---------|--------|----------|
| §3 | `quote` | 実装済 |
| §3 | `quasiquote` / `unquote` / `unquote-splicing` | 実装済 |
| §3 | `lambda` | 実装済 |
| §3 | `if` | 実装済 |
| §3 | `cond` | 実装済 |
| §3 | `case` | 実装済 |
| §3 | `and` | 実装済 |
| §3 | `or` | 実装済 |
| §3 | `not` | 実装済 |
| §3 | `progn` | 実装済 |
| §3 | `let` | 実装済 |
| §3 | `let*` | 実装済 |
| §3 | `block` | 実装済 |
| §3 | `return-from` | 実装済 |
| §3 | `catch` | 実装済 |
| §3 | `throw` | 実装済 |
| §3 | `tagbody` | 実装済 |
| §3 | `go` | 実装済 |
| §3 | `unwind-protect` | 実装済 |
| §3 | `setq` | 実装済 |
| §3 | `setf` | 実装済 |
| §3 | `defglobal` | 実装済 |
| §3 | `defconstant` | 実装済 |
| §3 | `defun` | 実装済 |
| §3 | `defmacro` | 実装済 |
| §3 | `flet` | 実装済 |
| §3 | `labels` | 実装済 |
| §3 | `function` | 実装済 |
| §3 | `the` | 実装済 |
| §3 | `while` | 実装済 |
| §3 | `do` / `dolist` / `dotimes` | 実装済 |
| §3 | `loop` | 実装済 |

---

## §4 Object-Oriented Programming

| §章番号 | 機能名 | 実装状況 |
|---------|--------|----------|
| §4 | `defclass` | 実装済 |
| §4 | `defgeneric` | 実装済 |
| §4 | `defmethod` | 実装済 |
| §4 | `make-instance` | 実装済 |
| §4 | `class-of` | 実装済 |
| §4 | `instancep` | 実装済 |
| §4 | `subclassp` | 実装済 |
| §4 | `slot-value` | 実装済 |
| §4 | `slot-boundp` | 実装済 |
| §4 | `slot-makunbound` | 実装済 |
| §4 | `call-next-method` | 実装済 |
| §4 | `next-method-p` | 実装済 |
| §4 | `initialize-object` | 実装済 |
| §4 | `class` (special form – `(class <name>)`) | 実装済 |

### §4 Built-in Class Hierarchy

| §章番号 | クラス名 | 実装状況 |
|---------|----------|----------|
| §4 | `<object>` | 実装済 |
| §4 | `<number>` | 実装済 |
| §4 | `<integer>` | 実装済 |
| §4 | `<float>` | 実装済 |
| §4 | `<character>` | 実装済 |
| §4 | `<symbol>` | 実装済 |
| §4 | `<function>` | 実装済 |
| §4 | `<stream>` | 実装済 |
| §4 | `<basic-array>` | 実装済 |
| §4 | `<basic-vector>` | 実装済 |
| §4 | `<general-vector>` | 実装済 |
| §4 | `<string>` | 実装済 |
| §4 | `<basic-list>` | 実装済 |
| §4 | `<list>` | 実装済 |
| §4 | `<cons>` | 実装済 |
| §4 | `<null>` | 実装済 |
| §4 | `<standard-object>` | 実装済 |
| §4 | `<built-in-class>` | 実装済 |

---

## §5 Condition System

| §章番号 | 機能名 | 実装状況 |
|---------|--------|----------|
| §5 | `error` | 実装済 |
| §5 | `cerror` | 実装済 |
| §5 | `signal-condition` | 実装済 |
| §5 | `continue-condition` | 実装済 |
| §5 | `condition-message` | 実装済 |
| §5 | `condition-continuable` | 実装済 |
| §5 | `handler-case` | 実装済 |
| §5 | `with-handler` | 実装済 |
| §5 | `ignore-errors` | 実装済 |

### §5 Condition Classes

| §章番号 | クラス名 | 実装状況 |
|---------|----------|----------|
| §5 | `<condition>` | 実装済 |
| §5 | `<serious-condition>` | 実装済 |
| §5 | `<error>` | 実装済 |
| §5 | `<simple-error>` | 実装済 |
| §5 | `<arithmetic-error>` | 実装済 |
| §5 | `<division-by-zero>` | 実装済 |
| §5 | `<floating-point-overflow>` | 実装済 |
| §5 | `<floating-point-underflow>` | 実装済 |
| §5 | `<control-error>` | 実装済 |
| §5 | `<parse-error>` | 実装済 |
| §5 | `<program-error>` | 実装済 |
| §5 | `<domain-error>` | 実装済 |
| §5 | `<undefined-entity>` | 実装済 |
| §5 | `<unbound-variable>` | 実装済 |
| §5 | `<undefined-function>` | 実装済 |
| §5 | `<arity-error>` | 実装済 |
| §5 | `<index-out-of-range>` | 実装済 |
| §5 | `<type-error>` | 実装済 |
| §5 | `<stream-error>` | 実装済 |
| §5 | `<end-of-stream>` | 実装済 |

---

## §6 Dynamic Variables

| §章番号 | 機能名 | 実装状況 |
|---------|--------|----------|
| §6 | `defvar` | 実装済 |
| §6 | `dynamic` | 実装済 |
| §6 | `dynamic-let` | 実装済 |

---

## §9 Evaluation

| §章番号 | 機能名 | 実装状況 |
|---------|--------|----------|
| §9 | `eval` | 未実装 |
| §9 | `apply` | 実装済 |
| §9 | `funcall` | 実装済 |
| §9 | `macroexpand-1` | 実装済 |
| §9 | `macroexpand` | 実装済 |

---

## §10 Symbol Functions

| §章番号 | 機能名 | 実装状況 |
|---------|--------|----------|
| §10 | `symbolp` | 実装済 |
| §10 | `symbol-name` | 実装済 |
| §10 | `gensym` | 実装済 |

---

## §11 Number Functions

| §章番号 | 機能名 | 実装状況 |
|---------|--------|----------|
| §11 | `numberp` | 実装済 |
| §11 | `integerp` | 別名あり (`integerp` = `(and (integer? x) (exact? x))`) |
| §11 | `floatp` | 実装済 |
| §11 | `+` `-` `*` `/` | 実装済 |
| §11 | `=` `<` `>` `<=` `>=` | 実装済 |
| §11 | `abs` | 実装済 |
| §11 | `quotient` | 実装済 |
| §11 | `rem` | 実装済 |
| §11 | `mod` | 未実装 |
| §11 | `max` | 未実装 (ISL標準名) |
| §11 | `min` | 未実装 (ISL標準名) |
| §11 | `gcd` | 未実装 |
| §11 | `lcm` | 未実装 |
| §11 | `floor` | 実装済 |
| §11 | `ceiling` | 実装済 |
| §11 | `round` | 実装済 |
| §11 | `truncate` | 実装済 |
| §11 | `float` | 実装済 |
| §11 | `signum` | 実装済 |
| §11 | `expt` | 実装済 |
| §11 | `sqrt` | 実装済 |
| §11 | `exp` | 実装済 |
| §11 | `log` | 未実装 |
| §11 | `sin` `cos` `tan` | 実装済 |
| §11 | `asin` `acos` `atan` | 実装済 |
| §11 | `plusp` | 実装済 |
| §11 | `zerop` | 実装済 |
| §11 | `evenp` | 実装済 |
| §11 | `oddp` | 実装済 |
| §11 | `negate` | 実装済 |
| §11 | `number->string` | 実装済 |
| §11 | `string->number` | 実装済 |

---

## §12 List Functions

| §章番号 | 機能名 | 実装状況 |
|---------|--------|----------|
| §12 | `consp` | 実装済 |
| §12 | `atom` | 実装済 |
| §12 | `null` | 実装済 |
| §12 | `listp` | 実装済 |
| §12 | `cons` | 実装済 |
| §12 | `car` `cdr` | 実装済 |
| §12 | `caar` `cadr` … `cddddr` | 実装済 |
| §12 | `list` | 実装済 |
| §12 | `create-list` | 実装済 |
| §12 | `list-length` | 実装済 |
| §12 | `length` | 実装済 |
| §12 | `append` | 実装済 |
| §12 | `nconc` | 実装済 |
| §12 | `reverse` | 実装済 |
| §12 | `nreverse` | 実装済 |
| §12 | `member` | 実装済 |
| §12 | `assoc` | 実装済 (equal? をデフォルト比較として使用) |
| §12 | `first` … `tenth` | 実装済 |
| §12 | `nth` | 別名あり (`list-ref`) |
| §12 | `list-ref` | 実装済 |
| §12 | `list-tail` | 実装済 |
| §12 | `nthcdr` | 実装済 |
| §12 | `last-pair` | 実装済 |
| §12 | `remove` | 実装済 |
| §12 | `remove-if` | 実装済 |
| §12 | `remove-if-not` | 実装済 |
| §12 | `reduce` | 実装済 |
| §12 | `find` | 実装済 |
| §12 | `sort` | 実装済 |
| §12 | `map-into` | 未実装 |

---

## §13 Higher-Order Functions

| §章番号 | 機能名 | 実装状況 |
|---------|--------|----------|
| §13 | `mapcar` | 別名あり (`map`) |
| §13 | `map` | 実装済 |
| §13 | `mapc` | 別名あり (`for-each`) |
| §13 | `for-each` | 実装済 |
| §13 | `some` | 実装済 |
| §13 | `every` | 実装済 |
| §13 | `notany` | 実装済 |
| §13 | `notevery` | 実装済 |
| §13 | `functionp` | 実装済 |

---

## §14 Array / Vector Functions

| §章番号 | 機能名 | 実装状況 |
|---------|--------|----------|
| §14 | `basic-array-p` | 未実装 |
| §14 | `basic-vector-p` | 実装済 |
| §14 | `general-vector-p` | 別名あり (`vectorp`) |
| §14 | `vectorp` | 実装済 |
| §14 | `vector` | 実装済 |
| §14 | `create-vector` | 実装済 |
| §14 | `general-vector-ref` | 実装済 |
| §14 | `general-vector-set!` (= `(setf (general-vector-ref v i) x)`) | 実装済 |
| §14 | `vector-ref` | 実装済 |
| §14 | `vector-set!` | 実装済 |
| §14 | `vector-length` | 実装済 |
| §14 | `aref` | 実装済 |
| §14 | `create-array` | 実装済 |
| §14 | `array-dimensions` | 実装済 |

---

## §15 String Functions

| §章番号 | 機能名 | 実装状況 |
|---------|--------|----------|
| §15 | `stringp` | 実装済 |
| §15 | `string` | 実装済 |
| §15 | `create-string` | 実装済 |
| §15 | `string-length` | 実装済 |
| §15 | `string-ref` | 実装済 |
| §15 | `(setf (string-ref s i) ch)` | 実装済 |
| §15 | `string-copy` | 実装済 |
| §15 | `string-append` | 実装済 |
| §15 | `string-concat` | 実装済 (= `string-append`) |
| §15 | `string=` `string/=` `string<` `string>` `string<=` `string>=` | 実装済 |
| §15 | `string-upcase` | 実装済 |
| §15 | `string-downcase` | 実装済 |
| §15 | `substring` | 実装済 |
| §15 | `string-index` | 実装済 |
| §15 | `string->number` | 実装済 |
| §15 | `number->string` | 実装済 |

---

## §16 Character Functions

| §章番号 | 機能名 | 実装状況 |
|---------|--------|----------|
| §16 | `characterp` | 実装済 |
| §16 | `char=` `char/=` `char<` `char>` `char<=` `char>=` | 実装済 |
| §16 | `char-code` | 実装済 |
| §16 | `code-char` | 実装済 |
| §16 | `char->integer` | 実装済 (= `char-code`) |
| §16 | `integer->char` | 別名あり (`code-char`) |
| §16 | `char-upcase` | 実装済 |
| §16 | `char-downcase` | 実装済 |
| §16 | `char-alphabetic-p` | 実装済 |
| §16 | `char-numeric-p` | 実装済 |
| §16 | `char-whitespace-p` | 実装済 |
| §16 | `char-upper-case-p` | 実装済 |
| §16 | `char-lower-case-p` | 実装済 |
| §16 | `char-index` | 実装済 |

---

## §17 Identifier / EQ / Type Functions

| §章番号 | 機能名 | 実装状況 |
|---------|--------|----------|
| §17 | `eq` | 実装済 |
| §17 | `eql` | 実装済 |
| §17 | `equal` | 実装済 |
| §17 | `identity` | 未実装 |

---

## §18 I/O Functions

| §章番号 | 機能名 | 実装状況 |
|---------|--------|----------|
| §18 | `open-input-stream` | 実装済 |
| §18 | `open-output-stream` | 実装済 |
| §18 | `open-io-stream` | 実装済 |
| §18 | `close` | 実装済 |
| §18 | `with-open-input-file` | 実装済 |
| §18 | `with-open-output-file` | 実装済 |
| §18 | `with-open-file` | 実装済 |
| §18 | `input-stream-p` | 実装済 |
| §18 | `output-stream-p` | 実装済 |
| §18 | `read` | 実装済 (EOF時 `<end-of-stream>` を発生) |
| §18 | `read-char` | 実装済 |
| §18 | `peek-char` | 実装済 |
| §18 | `read-line` | 実装済 |
| §18 | `write` | 実装済 |
| §18 | `write-char` | 実装済 |
| §18 | `write-line` | 実装済 |
| §18 | `print` | 実装済 |
| §18 | `terpri` | 実装済 |
| §18 | `format` | 実装済 (Common Lisp 風) |
| §18 | `*standard-input*` `*standard-output*` `*error-output*` | 実装済 |

---

## §19 Hash Table Functions

| §章番号 | 機能名 | 実装状況 |
|---------|--------|----------|
| §19 | `make-hash-table` | 実装済 |
| §19 | `hash-table-p` | 実装済 |
| §19 | `gethash` | 実装済 |
| §19 | `(setf (gethash key table) val)` | 実装済 |
| §19 | `puthash` | 実装済 |
| §19 | `remhash` | 実装済 |
| §19 | `clrhash` | 実装済 |
| §19 | `hash-table-count` | 実装済 |

---

## その他（ISL独自拡張）

| 機能名 | 実装状況 |
|--------|----------|
| `require` / `provide` | 実装済 |
| `load` | 実装済 |
| `gensym` | 実装済 |
| `trace` / `untrace` | 実装済 |
| `getenv` / `setenv` | 実装済 |
| `system` / `system-timeout` | 実装済 |
| `sqlite-*` / `postgres-*` / `mysql-*` | 実装済 (拡張) |
| `tcp-*` / `tls-*` / `udp-*` | 実装済 (拡張) |
| `http-get` / `http-head` / `http-post` | 実装済 (拡張) |
| `ffi-call` / `load-foreign-library` | 実装済 (拡張) |
| `thread-spawn` / `thread-join` | 実装済 (拡張) |
| `mutex-open` / `mutex-lock` / `mutex-unlock` | 実装済 (拡張) |
