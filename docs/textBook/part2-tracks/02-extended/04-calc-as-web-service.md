# E1.4 ★ Web サービス化する（isl-calc 4 周目）

> スパイラル **4 周目** / 拡張編 第 4 章 — isl-calc をネットワーク越しに使う

ここまでの 3 章で、道具はそろいました。ファイル I/O（[E1.2](02-file-io-history.md)）で履歴を残せ、TCP（[E1.3](03-networking.md)）で接続を受けられます。そして評価コアは、3 周目で `calc-parse` / `calc-try` として規格準拠に固めてあります（[S1.4](../01-standard/04-calc-strict-edition.md)）。

4 周目の仕上げは、これらを組み合わせて **isl-calc を Web サービスにする**ことです。`http://127.0.0.1:18080/eval/(+ 1 2)` のような URL でアクセスすると、式を評価して結果を返す——そんなサーバを作ります。完成版は [`code/isl-calc-v4.lsp`](code/isl-calc-v4.lsp) に同梱しています。

## 学習目標

この章を終えると、次のことができるようになります。

- 最小の HTTP サーバを立て、リクエストを 1 件ずつさばける。
- URL のパスから式文字列を取り出し、評価して応答を返せる。
- 各リクエストを履歴ファイルへ永続化できる。
- 不正な式に対し **HTTP 400** とエラーメッセージを返せる。

## 前提知識

- [S1.4 strict 準拠版に書き直す](../01-standard/04-calc-strict-edition.md)（`calc-parse` / `calc-try` の評価コア）
- [E1.2 ファイル I/O](02-file-io-history.md) / [E1.3 ネットワーク](03-networking.md)

> この章は extended プロファイルで動かします。ネットワークは拡張機能です。

---

## 1. 全体像——芯は規格準拠、外殻は拡張

作るものを図にします。[E1.1](01-extended-primitives.md) で示した層構造そのものです。

```
   HTTP リクエスト  GET /eval/(+%201%202)
        │
        ▼
  ┌─────────────────────────┐   ← 拡張の外殻（この章で作る）
  │ accept → パス抽出 → URLデコード │
  └───────────┬─────────────┘
              ▼ 文字列 "(+ 1 2)"
  ┌─────────────────────────┐   ← 規格準拠の芯（3周目のまま）
  │ read → calc-parse → calc-try │
  └───────────┬─────────────┘
              ▼ (ok 3) / (error …)
     HTTP 応答（200 / 400）＋ 履歴追記
```

評価の中身（`calc-parse` / `calc-try`）には**一切手を入れません**。周りに HTTP の殻をかぶせるだけです。3 周目で芯を固めた恩恵が、ここで効いてきます。

---

## 2. リクエストからパスを取り出す

HTTP リクエストの 1 行目は `GET /eval/(...) HTTP/1.1` の形です。空白で区切られた真ん中がパスです。[E1.3](03-networking.md) の `tcp-receive-line` で 1 行目を受け取り、2 つの空白の間を切り出します。

```lisp
(defun request-path (line)
  (let ((sp1 (string-index " " line)))
    (if sp1
        (let ((sp2 (string-index " " line (+ sp1 1))))
          (if sp2 (subseq line (+ sp1 1) sp2) '()))
        '())))
```

`string-index` は「探す文字列・探される文字列・（省略可）開始位置」の順に取り、見つかった位置を返します。1 つ目の空白の**次**から 2 つ目の空白の**手前**までが、パスです。

---

## 3. パスを式文字列に変える——URL デコード

パスは `/eval/(+%201%202)` のように**URL エンコード**されています。空白は `%20` に化けています。これを元に戻すのが URL デコードです。`%XX` を見つけたら、続く 2 桁を 16 進として読み、その文字コードの 1 文字に置き換えます。

```lisp
(defun url-decode (s)
  (let ((out "") (i 0) (n (length s)))
    (while (< i n)
      (let ((ch (elt s i)))
        (cond ((char= ch #\%)
               (setq out (string-append
                          out (create-string
                               1 (integer->char
                                  (string->number (subseq s (+ i 1) (+ i 3)) 16)))))
               (setq i (+ i 3)))
              (t
               (setq out (string-append out (create-string 1 ch)))
               (setq i (+ i 1))))))
    out))
```

`(string->number "20" 16)` が 16 進 `20` を `32`（空白の文字コード）に変え、`integer->char` が空白文字に戻します。

> **なぜクエリ `?expr=` でなくパス `/eval/` か**: URL の**クエリ文字列**では、`+` は空白にデコードされる決まりです。すると加算演算子 `+` が消えてしまいます。**パス**では `+` はそのまま生き残るので、`(+ 1 2)` の `+` を素直に送れます。空白だけ `%20` にエンコードすればよく、扱いが楽になります。これがパス方式を選んだ理由です。

パスの先頭が `/eval/` なら、その後ろをデコードして式文字列にします。

```lisp
(defun path->expr (path)
  (if (and (>= (length path) 6) (string= (subseq path 0 6) "/eval/"))
      (url-decode (subseq path 6 (length path)))
      '()))          ; /eval/ 以外は nil
```

---

## 4. 評価して応答する

式文字列が手に入れば、あとは芯に渡すだけです。[E1.2](02-file-io-history.md) で見た `read`＋`create-string-input-stream` で S 式にし、`calc-try`（[S1.4](../01-standard/04-calc-strict-edition.md)）で評価します。

```lisp
(defun calc-try-string (str)
  (catch 'calc-done
    (with-handler
      (lambda (c)
        (throw 'calc-done
          (cond ((instancep c (class <division-by-zero>))
                 (list 'error 'division-by-zero (arithmetic-error-operands c)))
                ((instancep c (class <error>))
                 (list 'error 'other (condition-message c)))
                (t (list 'error 'unknown)))))
      (list 'ok (calc-eval (read (create-string-input-stream str)))))))
```

`calc-try-string` は [S1.4](../01-standard/04-calc-strict-edition.md) の `calc-try` に「文字列を `read` する」入口を足しただけです。返り値は同じく `(ok 値)` / `(error 種別 …)`。この**型付きの結果**が、HTTP のステータスコードにそのまま対応します。

```lisp
(defun handle-connection (conn)
  (let ((reqline (tcp-receive-line conn '())))
    (drain-headers conn)                        ; ヘッダを空行まで読み飛ばす
    (if (stringp reqline)
        (let ((expr-str (path->expr (request-path reqline))))
          (if (stringp expr-str)
              (let ((r (calc-try-string expr-str)))
                (calc-save-entry expr-str r)     ; ← 履歴に追記（E1.2）
                (if (eq (car r) 'ok)
                    (send-response conn "200 OK"
                                   (format nil "~A => ~S~%" expr-str (cadr r)))
                    (send-response conn "400 Bad Request"
                                   (format nil "error: ~A : ~S~%"
                                           expr-str (car (cddr r))))))
              (send-response conn "404 Not Found" (format nil "use /eval/<expr>~%"))))
        nil)
    (tcp-close conn)))
```

`calc-try` が `ok` を返せば **200**、`error` を返せば **400**。評価器の型付き結果が、そのまま HTTP の意味論（成功／クライアントエラー）に写ります。これが 3 周目で `calc-try` を設計しておいた狙いでした。各リクエストは `calc-save-entry` で履歴にも残ります。

`send-response` は HTTP レスポンスを組み立てて送るだけです。

```lisp
(defun send-response (conn status body)
  (tcp-send conn
    (string-append "HTTP/1.1 " status "\r\n"
                   "Content-Type: text/plain; charset=utf-8\r\n"
                   "Content-Length: " (convert (length body) <string>) "\r\n"
                   "Connection: close\r\n\r\n"
                   body)))
```

サーバ本体は、[E1.3](03-networking.md) の accept ループそのものです。

```lisp
(defun calc-serve (port)
  (let ((listener (tcp-listen port)))
    (format (standard-output) "isl-calc web service on http://127.0.0.1:~D/eval/<expr>~%" port)
    (while t
      (handle-connection (tcp-accept listener)))))

(calc-serve 18080)
```

---

## 5. 動かす

サーバを起動します。

```sh
./bin/isl docs/textBook/part2-tracks/02-extended/code/isl-calc-v4.lsp
```

```
isl-calc web service on http://127.0.0.1:18080/eval/<expr>
```

別の端末から `curl` で叩きます。空白は `%20`、`+` はそのままでかまいません。`--path-as-is` はパス中の `()` などをそのまま送るための指定です。

```sh
$ curl --path-as-is 'http://127.0.0.1:18080/eval/(+%201%20(*%202%203))'
(+ 1 (* 2 3)) => 7

$ curl --path-as-is 'http://127.0.0.1:18080/eval/(let%20((x%2010))%20(+%20x%205))'
(let ((x 10)) (+ x 5)) => 15
```

入れ子の式も `let` も、ネットワーク越しに評価できました 🎉 3 周目の評価器が、そのまま Web サービスの心臓として動いています。

---

## 6. エラーは HTTP 400 で返す

不正な式は、`calc-try` が `error` を返し、それが 400 になります。ステータス行と本文を確かめます。

```sh
$ curl -i --path-as-is 'http://127.0.0.1:18080/eval/(/%201%200)'
HTTP/1.1 400 Bad Request
...
error: (/ 1 0) : (1 0)

$ curl --path-as-is 'http://127.0.0.1:18080/eval/(+%20y%201)'
error: (+ y 1) : "unbound variable: y"
```

ゼロ除算は `<division-by-zero>` として捕まえ、オペランド `(1 0)` を添えて 400。未定義変数はメッセージ付きで 400。`/eval/` 以外のパスは 404 です。

```sh
$ curl -i --path-as-is 'http://127.0.0.1:18080/foo'
HTTP/1.1 404 Not Found
...
use /eval/<expr>
```

「エラーもまた応答の一部」という 3 周目の姿勢が、HTTP のステータスコードという形で実を結びました。

---

## 7. 履歴を確かめる

サーバに何度かアクセスしたあと、`calc-history.txt` を見ると、リクエストの記録が残っています。

```
(+ 1 (* 2 3)) => (ok 7)
(let ((x 10)) (+ x 5)) => (ok 15)
(/ 1 0) => (error division-by-zero (1 0))
(+ y 1) => (error other "unbound variable: y")
```

成功も失敗も、区別されて永続化されています。[E1.2](02-file-io-history.md) のファイル I/O と [S1.2](../01-standard/02-standard-conditions.md) の型付きコンディションが、ここで一つに合流しました。

---

## ★ 処理系の中では（コラム）

私たちが 100 行ほどで書いたこのサーバは、リポジトリの [`examples/webserver`](../../../../examples/webserver)（1200 行超）の最小核と同じ骨格です。あちらは TLS 対応・設定ファイル・ルーティング・運用コマンドまで備えますが、心臓は同じ「listen → accept ループ → リクエストを解釈して応答」です。本格実装との差は**機能の量**であって、**発想**ではありません。最小を自分で組めたなら、`examples/webserver` のコードも「拡張された同じ骨格」として読めるはずです。

---

## 演習

1. **JSON 風応答**: 200 応答の本文を `{"expr": "...", "result": ...}` の形に変えよ（`format nil` で組み立てる）。
2. **ルート追加**: `/history` にアクセスしたら履歴ファイルの中身を返すルートを足せ（`path` の判定を増やし、[E1.2](02-file-io-history.md) の読み戻しを流用）。
3. **上限を設ける**: 1 リクエストの式文字列が長すぎる（例: 200 文字超）場合に、評価せず 400 を返すガードを足せ。なぜサーバでこうした上限が要るか、一言添えよ。

---

## スパイラルの次の一歩

5 周目（コンパイラ編）では、この評価器を `islc` で **ネイティブバイナリ**にコンパイルし、高速化します。いまはインタプリタが 1 式ずつ評価していますが、コンパイルすれば式の評価そのものが機械語になります。「同じ isl-calc を、今度は速く」——スパイラルは最後の 2 周へ向かいます。

## まとめ

- Web サービス化とは、**規格準拠の評価コアに、拡張の HTTP 外殻をかぶせる**こと。芯（`calc-parse`/`calc-try`）は無改造。
- リクエスト行からパスを取り、**URL デコード**して式文字列に。パス方式なら `+` が生き残る。
- `read`＋`create-string-input-stream` で S 式化し、`calc-try` で評価。**`ok`→200 / `error`→400** とステータスに写す。
- 各リクエストを履歴に永続化。成功も失敗も残る。
- 100 行の自作サーバは、`examples/webserver` と同じ骨格。差は機能量であって発想ではない。

## 関連

- 前章: [E1.3 ネットワーク](03-networking.md)
- 前の周: [★ strict 準拠版に書き直す（isl-calc 3 周目）](../01-standard/04-calc-strict-edition.md)
- 次の周: [★ ネイティブバイナリにする（isl-calc 5 周目）](../03-compiler/05-calc-native-binary.md)
- 同梱コード: [`code/isl-calc-v4.lsp`](code/isl-calc-v4.lsp)
- 参考実装: [`examples/webserver`](../../../../examples/webserver)
