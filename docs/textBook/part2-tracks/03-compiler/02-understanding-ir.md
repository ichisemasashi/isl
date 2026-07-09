# C1.2 IR を理解する

> スパイラル **5 周目** / コンパイラ編 第 2 章

前章で、islc のパイプラインが「ソース → **IR** → LLVM IR → ネイティブ」と進むことを見ました。この章の主役は、その途中に挟まる **IR（Intermediate Representation, 中間表現）**です。

なぜ S 式を直接機械語にせず、わざわざ IR を挟むのでしょうか。答えは「**翻訳を段階に分けると、各段が単純になる**」からです。IR は「S 式より機械語に近いが、まだ読める」ちょうど中間の形で、ここで評価順序や制御の流れをきれいに整えておくと、最終段の機械語生成が楽になります。幸い isl には IR をそのまま覗ける道具があるので、実物を見ながら理解します。

## 学習目標

この章を終えると、次のことができるようになります。

- フロントエンドが式をどう IR に正規化するかを、実出力を読んで追える。
- `islc-front --dump-ir` で IR と LLVM 向け CFG を観察できる。
- 評価順序が **effect barrier** で左から順に固定される意味を説明できる。

## 前提知識

- [C1.1 islc 概観](01-islc-overview.md)

---

## 1. フロントエンド：read → macroexpand → IR

コンパイラの入口（フロントエンド、`frontend.scm`）は、ソースを 3 段で整えます。

1. **read** … テキストを S 式として読む。
2. **macroexpand** … マクロを展開し、基本的な形だけに減らす。
3. **IR 正規化** … 展開済み S 式を IR データ構造へ変換する。

この 3 段を一度に見せてくれるのが `islc-front --dump-ir` です。

---

## 2. IR を覗く

小さなプログラムを用意します。

```lisp
;; small.lsp
(defglobal r (+ 1 (* 2 3)))
(print r)
```

ダンプします。

```sh
./bin/islc-front --dump-ir small.lsp
```

出力（抜粋）:

```
[1] source:   (defglobal r (+ 1 (* 2 3)))
    expanded: (defglobal r (+ 1 (* 2 3)))
    ir: (define-global r
          (effect (call (var +) (const 1)
                        (effect (call (var *) (const 2) (const 3))))))
    ll: (ll-define-global r
          (cfg bb0 ((block bb0
            ((assign t0 (var +)) (assign t1 (const 1))
             (assign t2 (var *)) (assign t3 (const 2)) (assign t4 (const 3))
             (assign t5 (call t2 (t3 t4)))
             (assign t6 (call t0 (t1 t5))))
            (ret t6)))))
[2] source:   (print r)
    ...
```

各フォームについて、**source（元）→ expanded（展開後）→ ir（中間表現）→ ll（LLVM 向け CFG）**の 4 段が並びます。順に読み解きます。

---

## 3. IR の読み方

`ir:` の行を見ます。

```
(define-global r
  (effect (call (var +) (const 1)
                (effect (call (var *) (const 2) (const 3))))))
```

- `(const 1)` … 定数。
- `(var +)` … 変数（ここでは関数 `+`）の参照。
- `(call ...)` … 関数適用。
- `(effect ...)` … 「この計算は副作用（順序）を持つ」という印。次節の主役です。

元の `(+ 1 (* 2 3))` と見比べてください。**構造はほぼそのまま**です。IR はまだ「読める」——ここが IR のありがたさです。ただし、機械語に向けて**評価の順序**が明示されている点が違います。

---

## 4. 評価順序と effect barrier

`ir:` に現れた `(effect ...)` は、**「ここは決められた順序で実行する」という壁（effect barrier）**を表します。ISLISP は「オペランドを左から順に評価する」と規格で定めており、コンパイラはこの順序を IR の段階で**固定**します。最適化で計算の順番が勝手に入れ替わって、副作用（表示・代入など）の順序が狂わないようにするためです。

`ll:`（LLVM 向け CFG）を見ると、この順序が**一本道の代入列**になっているのがわかります。

```
(assign t2 (var *)) (assign t3 (const 2)) (assign t4 (const 3))
(assign t5 (call t2 (t3 t4)))     ; ← 先に (* 2 3) = t5
(assign t6 (call t0 (t1 t5)))     ; ← その後 (+ 1 t5)
```

内側の `(* 2 3)` が `t5` として**先に**計算され、その結果を使って `(+ 1 t5)` が `t6` になります。part1 の電卓で「オペランドを先に評価してから演算」と再帰で書いたあの順序が、コンパイラでは**一時変数 `t0, t1, …` への代入の並び**として現れているのです。人間が再帰で表現した評価順序を、機械は平らな代入列に落とし込みます。

> 評価順序の規範は `docs/side-effect-order.md` にまとまっています。「左から順」という素朴な約束が、コンパイラでは effect barrier として厳密に守られます。

---

## 5. 分岐は「基本ブロック」に分かれる

もう少し複雑な例——分岐を含む関数を見ると、CFG（制御フローグラフ）の姿がはっきりします。`examples/hello.lsp` の `fact` をダンプすると、`if` が複数の**基本ブロック**（`bb0`〜`bb3`）に分かれます。

```
(cfg bb0
  ((block bb0 (... (assign t3 (call ...))) (br t3 bb1 bb2))  ; 条件で分岐
   (block bb1 ((assign t4 (const 1)))       (jmp bb3))       ; then 側
   (block bb2 (... 再帰呼び出し ...)          (jmp bb3))       ; else 側
   (block bb3 ((assign t14 (phi ((bb1 t4) (bb2 t13)))))       ; 合流
              (ret t14))))
```

- `(br t3 bb1 bb2)` … 条件 `t3` が真なら `bb1`、偽なら `bb2` へ**分岐**。
- `bb1`（then）と `bb2`（else）はそれぞれ計算して `bb3` へ**合流**。
- `(phi ((bb1 t4) (bb2 t13)))` … 「どちらのブロックから来たか」で値を選ぶ **phi ノード**。then から来たら `t4`、else から来たら `t13`。

`if` という一つの式が、「分岐 → それぞれ計算 → 合流して値を選ぶ」という**ブロックの地図**に変換されました。これが機械語の制御構造（ジャンプ命令）に素直に対応します。part1 で書いた `if` が、コンパイラの中ではこう見えている——という発見が、この章の収穫です。

---

## ★ 処理系の中では（コラム）

IR のデータ構造は `src/isl/compiler/ir.scm` に、S 式 → IR の正規化は `frontend.scm` に、IR → CFG の lowering は `lowering.scm` にあります。`(effect ...)` による順序固定は、最適化パス（`optimization-policy.md` の whitelist）が副作用の順序を壊さないための土台です。phi ノードは LLVM の SSA（静的単一代入）形式の要で、「変数は一度だけ代入される」という制約の下で分岐後の値をどう選ぶかを表現します。私たちが手で書いた再帰と分岐が、機械にとって扱いやすい平らな形へ翻訳されていく——その中間地点が IR です。

---

## 演習

1. **calc 式の IR**: `(defglobal r (- (* 4 5) (/ 8 2)))` を含むファイルを `--dump-ir` し、`ir:` と `ll:` を読め。`(* 4 5)` と `(/ 8 2)` のどちらが先に計算されるか、代入列の順で確かめよ。
2. **effect の意味**: `ir:` に現れる `(effect ...)` が何を保証しているか、自分の言葉で一文にまとめよ。
3. **分岐の観察**: 自作の小さな `(if (= n 0) 1 2)` を含む関数を `--dump-ir` し、`br` / `jmp` / `phi` を指させ。

---

## まとめ

- **IR** は「S 式より機械語に近いが、まだ読める」中間表現。翻訳を段階に分けて各段を単純にするために挟む。
- `islc-front --dump-ir` は **source → expanded → ir → ll** の 4 段を見せる。
- `(effect ...)` は **effect barrier**＝評価順序（左から順）の固定。副作用の順序が最適化で狂わないための壁。
- `if` は基本ブロック（`bb0`…）に分かれ、`br`（分岐）/ `jmp`（合流）/ `phi`（値の選択）で表される。

## 関連

- 参照: `docs/side-effect-order.md`
- 前章: [C1.1 islc 概観](01-islc-overview.md)
- 次章: [AOT ビルド](03-aot-build.md)
