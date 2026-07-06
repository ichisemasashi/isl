# E1.2 ファイル I/O と履歴永続化

> スパイラル **4 周目** / 拡張編 第 2 章

これまでの isl-calc は、式を評価したらその結果を返すだけで、**何も覚えていません**でした。プログラムを終了すれば、それまでの計算はすべて消えます。この章では、拡張機能の**ファイル I/O** を使って、評価の履歴を**ファイルに残す**——つまり永続化します。これは capstone（[E1.4](04-calc-as-web-service.md)）の Web サービスが「リクエストのログを取る」ための土台になります。

## 学習目標

この章を終えると、次のことができるようになります。

- ストリームとファイル入出力の基本を理解する。
- `with-open-file` でファイルに書き込み・読み込みできる。
- 評価履歴をファイルへ**追記**し、起動時に読み戻せる。

## 前提知識

- [E1.1 拡張プリミティブの世界](01-extended-primitives.md)

> この章は既定の extended プロファイルで動かします。`./bin/isl <file>`。

---

## 1. ストリームという考え方

ファイルやネットワーク、標準入出力——データの「流れる管」を、Lisp では**ストリーム**（stream）として統一的に扱います。これまで何気なく使ってきた `(standard-output)` も、標準出力へつながるストリームです。

```lisp
> (format (standard-output) "hello~%")
hello
```

ファイルに書くとは、「標準出力の代わりに、**ファイルへつながるストリーム**へ `format` する」だけのことです。読むときは逆に、ファイルへつながる**入力ストリーム**から 1 行ずつ引き出します。この見方を持てば、ファイル I/O は「行き先／来元が変わっただけの入出力」として自然に扱えます。

---

## 2. `with-open-file`——開いて、使って、確実に閉じる

ファイルは「開く→読み書き→閉じる」の 3 段階で扱います。閉じ忘れを防ぐため、isl では **`with-open-file`** が開閉を面倒みてくれます。本体を抜けると自動的に閉じられます。

> **isl の `with-open-file`**: 標準の `with-open-input-file` / `with-open-output-file` を統合した、本処理系独自の形式です。`:direction` で入出力を選びます。

### 書き込み（出力ストリーム）

```lisp
(with-open-file (out "memo.txt" :direction :output
                                :if-exists :append
                                :if-does-not-exist :create)
  (format out "~S => ~S~%" '(+ 1 2) 3))
```

キーワードの意味は次のとおりです。

- `:direction :output` … 書き込み用に開く。
- `:if-exists :append` … ファイルが既にあれば**末尾に追記**（`:supersede` なら上書き）。
- `:if-does-not-exist :create` … 無ければ**新規作成**。

第 1 要素 `out` が、ファイルへつながる出力ストリームです。あとは `(standard-output)` の代わりに `out` へ `format` するだけです。

### 読み込み（入力ストリーム）

```lisp
(with-open-file (in "memo.txt" :direction :input)
  (read-line in nil))       ; 1 行読む。終端なら nil
```

`read-line` は 1 行を文字列で返し、ファイル終端では第 2 引数の既定値（ここでは `nil`）を返します。この「終端で nil」を使って、ループの止めどきを判断します。

---

## 3. 履歴を追記する——`calc-save-entry`

isl-calc に「評価するたびに履歴を残す」関数を足します。式と結果を 1 行として追記するだけです。

```lisp
(defglobal *calc-history* "calc-history.txt")

(defun calc-save-entry (expr result)
  (with-open-file (out *calc-history*
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
    (format out "~S => ~S~%" expr result)))
```

呼ぶたびに 1 行ずつ積まれていきます。

```lisp
> (calc-save-entry '(+ 1 2) 3)
> (calc-save-entry '(* 4 5) 20)
```

`:if-exists :append` のおかげで、2 回目の呼び出しは 1 回目を消さずに**下に足されます**。これが「履歴」の本質です。

---

## 4. 履歴を読み戻す——`calc-show-history`

保存した履歴を、起動時などに読み戻して表示します。入力ストリームから、終端（`nil`）まで 1 行ずつ読みます。

```lisp
(defun calc-show-history ()
  (with-open-file (in *calc-history* :direction :input)
    (let ((line (read-line in nil)))
      (while line
        (format (standard-output) "~A~%" line)
        (setq line (read-line in nil))))))
```

実行すると、追記した順に出てきます。

```lisp
> (calc-show-history)
(+ 1 2) => 3
(* 4 5) => 20
```

> **ISLISP には「名前付き let」がない**: Scheme の `(let loop (...) ...)` のような**名前付き `let`** は ISLISP にはありません。上のように `while` と `setq` で回すか、再帰関数として書きます。part1 の再帰（[2.4](../../part1-core/02-basic/04-recursion.md)）と同じく、「1 行読んで、残りを繰り返す」構造です。

---

## 5. 文字列を「式」として読み戻す（発展）

`read-line` は 1 行を**文字列**で返しますが、`read` を使うと、ストリームから **S 式そのもの**を読めます。文字列を S 式に変えるには、文字列を入力ストリームに見立てる `create-string-input-stream` を挟みます。

```lisp
> (read (create-string-input-stream "(+ 1 (* 2 3))"))
(+ 1 (* 2 3))
```

返ってきたのは、**評価可能な S 式**（リスト）です。これは capstone で決定的な役割を果たします。Web サービスは、リクエストから**文字列**として式を受け取ります。それを `read` で S 式に変え、`calc-parse` に渡す——という流れです（[E1.4](04-calc-as-web-service.md)）。「文字列 → S 式 → AST → 値」という変換の鎖を、ここで手にしました。

---

## ★ 処理系の中では（コラム）

`with-open-file` は、内部では Gauche のポートを開き、本体の評価を `unwind-protect` で包んで、正常終了でも例外でも**必ず閉じる**ように作られています。だから途中でエラーが起きてもファイルディスクリプタが漏れません。`read` がストリームから 1 つの S 式を組み立てる処理は、6 周目（内部編）で読む**リーダ**（`core/reader.scm`）そのものです。私たちが REPL に打ち込む式も、この同じリーダが文字列から S 式へ変換しています。

---

## 演習

1. **直近 N 件**: `calc-show-history` を改造し、末尾 5 件だけ表示する `calc-tail-history` を書け。ヒント: 全行をいったんリストに読み込み、長さから後ろ 5 件を切り出す。
2. **件数を数える**: 履歴ファイルの行数（＝評価回数）を返す `calc-history-count` を書け。
3. **S 式で往復**: `(+ 1 2)` という式を `~S` でファイルに書き、`read` で読み戻して、元の式と `equal` になることを確かめよ（文字列経由でないと壊れる例と対比するとなお良い）。

---

## まとめ

- ファイル・標準入出力は**ストリーム**として統一的に扱う。ファイル I/O は「行き先／来元が変わった入出力」。
- **`with-open-file`** で開閉を任せる。`:direction` / `:if-exists :append` / `:if-does-not-exist :create` を使い分ける。
- `calc-save-entry` で履歴を**追記**、`read-line`＋`while` で**読み戻す**（ISLISP に名前付き let はない）。
- `read` ＋ `create-string-input-stream` で**文字列を S 式に**変換できる——capstone の「リクエスト文字列 → 式」の鍵。

## 関連

- 前章: [E1.1 拡張プリミティブの世界](01-extended-primitives.md)
- 次章: [ネットワーク](03-networking.md)
- この先の応用: [★ Web サービス化する](04-calc-as-web-service.md)（履歴永続化を組み込む）
