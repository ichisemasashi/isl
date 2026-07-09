# E1.3 ネットワーク

> スパイラル **4 周目** / 拡張編 第 3 章

前章でファイルへの読み書きを覚えました。ストリームの考え方を身につけたなら、次の一歩は自然です——**ネットワーク**も、つないだ相手との「ストリーム」として扱えるのです。この章では、TCP ソケットで**サーバを立て、接続を受け、行を送受信する**基本を学びます。これは capstone（[E1.4](04-calc-as-web-service.md)）で HTTP サーバを組む直前の準備運動です。

## 学習目標

この章を終えると、次のことができるようになります。

- TCP の**待ち受け（listen）／受理（accept）／送受信**の流れを理解する。
- `tcp-listen` / `tcp-accept` / `tcp-receive-line` / `tcp-send` で最小のサーバを書ける。
- TLS 対応関数が TCP とほぼ対称に存在することを知る。

## 前提知識

- [E1.2 ファイル I/O と履歴永続化](02-file-io-history.md)（ストリームの考え方）

> この章は extended プロファイルで動かします。ネットワークは拡張機能です。

---

## 1. サーバの 3 動作——listen / accept / 送受信

TCP サーバの骨格は、いつも同じ 3 動作です。

1. **listen（待ち受け）** … ポートを開いて接続を待つ。`tcp-listen` が**リスナ**を返す。
2. **accept（受理）** … 接続が来るまで待ち、来たら**コネクション**を得る。`tcp-accept` はここで**ブロック**（接続が来るまで待つ）する。
3. **送受信** … コネクションを相手に、行を読む／書く。`tcp-receive-line` / `tcp-send` / `tcp-send-line`。

使い終わったら、コネクションとリスナをそれぞれ `tcp-close` / `tcp-listener-close` で閉じます。ファイルの「開く→使う→閉じる」と同じリズムです。

主なプリミティブをまとめます。

| 関数 | 役割 |
|------|------|
| `(tcp-listen port)` | ポートを開いてリスナを返す |
| `(tcp-accept listener)` | 接続を受理してコネクションを返す（ブロックする） |
| `(tcp-receive-line conn [eof-error-p])` | 1 行受信（`eof-error-p` を `nil` にすると終端で `nil`） |
| `(tcp-send conn text)` | 文字列を送る（改行なし） |
| `(tcp-send-line conn text)` | 文字列＋改行を送る |
| `(tcp-close conn)` / `(tcp-listener-close listener)` | 閉じる |

---

## 2. 最小のエコーサーバ

「受け取った行に `echo: ` を付けて返す」だけの最小サーバを書きます。1 接続を処理したら終了する版です。

```lisp
;; echo.lsp — 1 接続だけ受けて返す最小エコー
(defglobal listener (tcp-listen 18095))
(format (standard-output) "echo server ready~%")

(defglobal conn (tcp-accept listener))          ; 接続が来るまでここで待つ
(defglobal line (tcp-receive-line conn))        ; 1 行受信
(tcp-send-line conn (string-append "echo: " line))  ; 返信

(tcp-close conn)
(tcp-listener-close listener)
```

起動します（接続が来るまで待ち受けます）。

```sh
./bin/isl echo.lsp
```

別の端末からクライアントで叩きます。ここでは Python を使いますが、`nc`（netcat）でも同じことができます。

```python
import socket
s = socket.create_connection(('127.0.0.1', 18095))
s.sendall(b'hello isl\n')
print(s.recv(100).decode().strip())   # => echo: hello isl
s.close()
```

サーバ側が受け取った `hello isl` に `echo: ` を付けて返し、クライアントに `echo: hello isl` が表示されます。**ネットワーク越しに、行を受けて行を返す**——これが到達点です。HTTP サーバも、突き詰めればこの「行を受けて応答を返す」の延長にすぎません。

> **accept はブロックする**: `tcp-accept` は接続が来るまで**待ち続けます**。実用サーバは「accept して 1 件処理し、また accept…」を繰り返す**無限ループ**にします（capstone でそうします）。上の例は最小化のため 1 件で終えています。

---

## 3. 複数リクエストをさばく——accept ループ

現実のサーバは、1 件で終わらず何件でもさばきます。`while` で accept を回すだけです。

```lisp
(defglobal listener (tcp-listen 18095))
(while t
  (let ((conn (tcp-accept listener)))       ; 1 接続受理（次が来るまで待つ）
    (let ((line (tcp-receive-line conn)))
      (tcp-send-line conn (string-append "echo: " line)))
    (tcp-close conn)))                       ; 閉じて、また accept へ
```

`(while t ...)` は「ずっと繰り返す」ループです。1 接続を受け、処理し、閉じ、また次の接続を待つ——このリズムが、あらゆる素朴なサーバの心臓部です。止めるときは Ctrl-C です。capstone の HTTP サーバも、この骨格の「1 接続の処理」を HTTP のやり取りに置き換えたものになります。

---

## 4. TLS——TCP とほぼ対称

暗号化された通信（HTTPS の下地）を張るには、TLS 系の関数を使います。うれしいことに、**TCP 版とほぼ対称**の名前で用意されています。

| TCP | TLS |
|-----|-----|
| `tcp-listen` | `tls-listen` |
| `tcp-accept` | `tls-accept` |
| `tcp-receive-line` | `tls-receive-line` |
| `tcp-send` / `tcp-send-line` | `tls-send` / `tls-send-line` |
| `tcp-close` | `tls-close` |

つまり、いったん TCP でサーバを組めば、関数を `tls-*` に差し替え、証明書を渡すだけで HTTPS 化に近づけます。実際に TLS で動く本格的なサーバは、リポジトリの [`examples/webserver`](../../../../examples/webserver) にあり、`tls-listen` と `tcp-listen` を設定で切り替える構造になっています。この編の capstone は平文 TCP の最小 HTTP に絞りますが、その先に TLS 化の道が地続きで続いていることは知っておいてください。

---

## ★ 処理系の中では（コラム）

`tcp-listen` は、実装言語 Gauche の `make-server-socket` を薄くラップして「リスナ」オブジェクトを作り、`tcp-accept` は `socket-accept` を呼んで、得たソケットの**入力ポート／出力ポート**を「コネクション」として束ねています（`core/primitives.scm`）。だから `tcp-receive-line` の実体は、そのコネクションの入力ポートに対する `read-line` であり、前章のファイル入力とまったく同じ仕組みです。「ネットワークもストリーム」という見立ては、比喩ではなく実装そのものなのです。

---

## 演習

1. **エコーの改造**: 受け取った行をそのまま返すのではなく、`(string-append "you said: " line)` を返すよう変えて動かせ。
2. **回数制限**: `while t` の無限ループを、3 接続処理したら終わる版に変えよ（カウンタ変数と `while` の条件で制御する）。
3. **設計の対応づけ**: TCP エコーサーバの「1 接続の処理」を、次章の HTTP サーバの「1 リクエストの処理」に対応づけると、何がどう置き換わるか予想して書き出せ。

---

## まとめ

- TCP サーバの骨格は **listen → accept → 送受信 → close**。`tcp-accept` は接続が来るまで**ブロック**する。
- `tcp-listen` / `tcp-accept` / `tcp-receive-line` / `tcp-send(-line)` / `tcp-close` で最小エコーが書ける。
- 実用サーバは `(while t ...)` で accept を回し、1 接続ずつ処理する。
- **TLS 系**は TCP とほぼ対称。差し替えで HTTPS 化に近づく。本格例は `examples/webserver`。
- 「ネットワークもストリーム」——受信は入力ポートの `read-line` にすぎない。

## 関連

- 前章: [E1.2 ファイル I/O と履歴永続化](02-file-io-history.md)
- 参照: [`examples/webserver`](../../../../examples/webserver)（TLS 対応の本格実装）
- 次章: [★ Web サービス化する（isl-calc 4 周目）](04-calc-as-web-service.md)
