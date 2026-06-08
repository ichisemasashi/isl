# 1.2 導入とセットアップ

> スパイラル 0 周目 / 入門編 第 2 章

学び始めるには、まず手元で isl が動く状態を作ります。この章は「環境づくり」の章です。一度通れば二度と戻ってこなくてよい場所なので、つまずきやすい点を先回りして潰しておきましょう。

## 学習目標

- isl を動かすための前提（Gauche / `gosh`）を用意できる。
- `./bin/isl` を起動して、式が 1 つ評価できる状態にする。
- うまく動かないときの典型的なつまずきを、自分で切り分けられる。

## 1. 必要なもの

isl は **Gauche 0.9.15**（Scheme 処理系）の上で動きます。Gauche をインストールすると `gosh` というコマンドが使えるようになり、`./bin/isl` はこの `gosh` を内部で呼び出します。

- macOS なら Homebrew で `brew install gauche`
- Linux なら各ディストリビューションのパッケージ、または公式配布物

インストールできたか確認します。

```sh
$ gosh -V
Gauche scheme shell, version 0.9.15 ...
```

バージョン番号が表示されれば成功です。

## 2. リポジトリを用意する

isl 本体（このリポジトリ）を手元に置き、ディレクトリの中へ移動します。

```sh
$ cd path/to/isl
$ ls bin/
isl  islc  islc-aot  islc-front  islc-jit
```

`bin/` に `isl` が見えれば準備完了です。

## 3. 起動を確認する

REPL（対話環境）を起動してみましょう。

```sh
$ ./bin/isl
ISLISP interpreter (Gauche 0.9.15)
Profile: extended
Ctrl-D to exit.
ISLISP>
```

`ISLISP>` というプロンプトが出れば成功です。ためしに式を 1 つ打ち込みます。

```lisp
ISLISP> (+ 1 2)
3
```

`3` が返ってきたら、isl は正しく動いています。終了するには **Ctrl-D**（EOF）を押します。REPL の使い方そのものは次章でくわしく見ます。

## 4. よくあるつまずき

環境づくりで引っかかりやすいのは、だいたい次の 3 つです。

### `gosh: command not found`

`gosh` にパスが通っていません。Gauche をインストールした場所（例: `/opt/homebrew/bin` や `/usr/local/bin`）を `PATH` に加えます。

```sh
$ which gosh        # 場所を確認
$ export PATH="/path/to/gosh/dir:$PATH"
```

### `permission denied: ./bin/isl`

実行権限がない場合は付与します。

```sh
$ chmod +x ./bin/isl
```

### 「ファイルが見つからない」系のエラー

isl はリポジトリのルート（`bin/` が見える場所）から実行する前提です。別の場所からではなく、リポジトリ直下で `./bin/isl` を実行してください。

## 5. プロファイルの予告

起動バナーに `Profile: extended` と出ていたことに気づいたでしょうか。これは既定の拡張モードです。規格にきっちり準拠した strict モードに切り替えるには、起動時にフラグを付けます。

```sh
$ ./bin/isl --profile strict
```

入門編ではずっと既定の extended のまま進めます。strict が主役になるのは標準編（3 周目）です。

## ★ 処理系の中では（コラム）

`./bin/isl` の正体は、`gosh` で `src/isl/main.scm` を起動するだけの短いシェルスクリプトです。特別な仕掛けはありません。中身は普通の Scheme スクリプトなので、興味があれば `bin/isl` をテキストエディタで開いてみてください。

## 関連

- 前章: [ISLISP とは](01-what-is-islisp.md)
- 次章: [REPL を起動する](03-start-repl.md)
