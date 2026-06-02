# 1.2 REPL を起動する

> スパイラル 0 周目 / 入門編 第 2 章

## 学習目標
- `./bin/isl` で REPL を起動し、式を 1 つ評価して終了できる。
- ファイルを `./bin/isl <file>` で実行できる。

## 前提知識
- ターミナルの基本操作。

## 内容（アウトライン）
- REPL（Read-Eval-Print Loop）とは何か。
- 起動: `./bin/isl`
- 最初の評価: `(+ 1 2)` を入力して `3` が返ることを確認。
- 終了方法。
- ファイル実行: `./bin/isl examples/hello.lsp`

## サンプル
```lisp
> (+ 1 2)
3
```

## 演習
- 自分の名前を `(format (standard-output) "~A~%" "...")` で表示してみる。

## 関連
- 前章: [ISLISP とは](01-what-is-islisp.md)
- 次章: [S 式を読む](03-s-expression.md)
