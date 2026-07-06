# 3.4 エラー処理の基礎

> スパイラル **2 周目** / 中級編 第 4 章

基本編の電卓は、未知の演算子を渡すと `(error "unknown operator" op)` でエラーを**投げ**ていました。投げっぱなしだと、プログラムはそこで止まります。実用的な電卓なら、「未定義変数」や「ゼロ除算」に出会っても、既定値を返したり、丁寧なメッセージを出したりして**処理を続けたい**ことがあります。

この章では、エラーを**投げる**側（`error`）と、投げられたエラーを**捕まえる**側（`with-handler`）の最小形を学びます。ここで身につける「捕まえて既定値を返す」型が、5 章の電卓に頑丈さを与えます。

## 学習目標

この章を終えると、次のことができるようになります。

- `error` で自分のエラーを通知できる。
- `with-handler` と非局所脱出（`catch`/`throw`）を組み合わせ、エラーを**捕捉**できる。
- 「未知の演算子」「ゼロ除算」を、止まらずに扱える。

## 前提知識

- [3.3 オブジェクトシステムの基礎](03-object-system-basics.md)
- 基本編で見た `error` の投げ方（[2.5 電卓](../02-basic/05-build-calculator.md)）

---

## 1. エラーを投げる——`error`

`error` は、その場でエラー（**コンディション**と呼ばれる状態）を発生させ、通常の実行を打ち切ります。第 1 引数がメッセージ、以降は関連する値です。

```lisp
> (error "unknown operator" '%)
*** ERROR: unknown operator (%)
```

捕まえる相手がいなければ、エラーはトップレベルまで伝わり、REPL がメッセージを表示して実行を止めます。基本編の電卓はここまででした。この章の目的は、この「伝わっていく途中で**捕まえる**」ことです。

---

## 2. エラーを捕まえる——`with-handler` と非局所脱出

ISLISP 標準のエラー捕捉は `with-handler` です。形はこうです。

```lisp
(with-handler ハンドラ関数
  本体...)
```

「本体を評価する間にエラーが起きたら、**ハンドラ関数**を呼ぶ」という意味です。ハンドラはコンディション `c` を 1 引数で受け取ります。

ここで ISLISP の作法として大切な点があります。**ハンドラは、最後に必ず「非局所脱出」で抜けなければなりません**。捕まえたあと「どこへ戻って続きを実行するか」を、脱出先で示す必要があるのです。脱出には次の対を使います。

- `catch` / `throw` … `(catch タグ 本体)` の外へ、`(throw タグ 値)` で脱出する。
- `block` / `return-from` … `(block 名前 本体)` の外へ、`(return-from 名前 値)` で脱出する。

> **よくある落とし穴**: `catch` と対になるのは `throw`、`block` と対になるのは `return-from` です。`block` の中で `throw` しても捕まりません（「No enclosing catch」エラーになります）。対を取り違えないよう注意してください。

### 捕まえて既定値を返す

`catch`/`throw` を使い、「ゼロ除算が起きたら既定値 `0` を返す」割り算を書きます。

```lisp
> (defun safe-div (a b)
    (catch 'div
      (with-handler
        (lambda (c) (throw 'div 0))   ; エラーを捕まえたら 0 を投げ返す
        (/ a b))))
safe-div
> (safe-div 10 2)
5
> (safe-div 10 0)
0
```

`(safe-div 10 0)` は、`(/ 10 0)` がゼロ除算コンディションを起こし、ハンドラが `(throw 'div 0)` で `(catch 'div ...)` の外へ脱出、結果として `0` が返ります。**プログラムは止まりません**。

`block`/`return-from` でも同じことが書けます。好みで選んでかまいません。

```lisp
> (defun safe-div2 (a b)
    (block done
      (with-handler
        (lambda (c) (return-from done 0))
        (/ a b))))
safe-div2
> (safe-div2 10 0)
0
```

---

## 3. 電卓に当てはめる——止まらない評価

この型を、電卓の 2 大トラブル「未知の演算子」「ゼロ除算」に当てはめます。汎用の「安全に評価する」ラッパを 1 つ用意すれば、どちらもまとめて面倒を見られます。

```lisp
> (defun safe-eval (thunk default)
    (catch 'k
      (with-handler
        (lambda (c) (throw 'k default))
        (funcall thunk))))
safe-eval
```

`thunk`（引数なしの関数）で「評価したい計算」を包んで渡し、失敗したら `default` を返す、という道具です。

```lisp
> (safe-eval (lambda () (/ 10 0)) 'infinity)     ; ゼロ除算 → 既定値
infinity
> (safe-eval (lambda () (error "unknown operator" '%)) 'undefined)
undefined
> (safe-eval (lambda () (/ 10 2)) 'infinity)     ; 正常時はそのまま
5
```

基本編の電卓は「未知の演算子」で `error` を投げるところまででしたが、この `safe-eval` で包めば、**エラーを既定値に変えて処理を続ける**ことができます。5 章のオブジェクトシステム版電卓では、この考え方で「未定義変数」や「ゼロ除算」を扱います。

---

## ★ 処理系の中では（コラム）

`error` が起こす「コンディション」は、オブジェクトシステムのクラスとして表現されたオブジェクトです（`core.scm` の `isl-signal-condition` あたりが投げる側）。`with-handler` は、いま有効なハンドラを内側から外側へ積んだスタックのようなもので、エラーが起きると近いハンドラから順に呼ばれます。ハンドラが非局所脱出しなければ、エラーは「まだ処理されていない」ものとして外側へ伝わり続けます——だから最後に脱出が必要なのです。より本格的なコンディション設計（種類ごとの分類、`continue` による再開など）は、標準編（part2 第 1 編）で扱います。

---

## 演習

1. **既定値を返す電卓**: 基本編の `calc-eval` を `safe-eval` で包み、`(safe-eval (lambda () (calc-eval '(% 1 2))) 0)` が `0` を返すことを確かめよ。
2. **対の取り違え**: `(block done (with-handler (lambda (c) (throw 'done 0)) (/ 1 0)))` を実行するとどうなるか予想し、確かめよ。なぜそうなるか（`block` と `throw` は対でない）を説明せよ。
3. **メッセージを添える（発展）**: `safe-eval` を改造し、既定値を返す前に `(format (standard-output) "recovered~%")` と出力してから脱出する版を書け。

---

## まとめ

- `error` はコンディションを発生させ実行を打ち切る。捕まえる相手がいなければトップレベルまで伝わる。
- `with-handler ハンドラ 本体...` で捕捉する。**ハンドラは最後に非局所脱出しなければならない**。
- 脱出の対は `catch`/`throw` と `block`/`return-from`。**対を取り違えない**こと。
- 「`catch`＋`with-handler`＋`throw` で既定値を返す」型（`safe-eval`）で、ゼロ除算や未知演算子を**止まらずに**扱える。これが 5 章の電卓の頑丈さになる。

## 関連

- 前章: [3.3 オブジェクトシステムの基礎](03-object-system-basics.md)
- 次章: [★ オブジェクトシステムで AST 評価器に作り直す（isl-calc 2 周目）](05-redesign-with-object-system-ast.md)
- この先: 本格的なコンディション設計は [標準編 2-1.2 標準コンディション](../../part2-tracks/01-standard/02-standard-conditions.md)
