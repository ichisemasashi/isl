# 2.1 変数束縛と関数定義

> スパイラル 1 周目 / 基本編 第 1 章

## 学習目標
- `defglobal` / `let` で値を束縛できる。
- `defun` で関数を定義し、呼び出せる。
- `lambda` で無名関数を作れる。

## 前提知識
- 入門編（第 1 章）を読了していること。

## 内容（アウトライン）
- グローバル束縛 `defglobal`、局所束縛 `let` / `let*`。
- 関数定義 `defun`、引数と戻り値。
- 無名関数 `lambda`。
- ISLISP の関数名前空間（`function` / `#'`）への軽い導入。

## サンプル
```lisp
(defun square (x) (* x x))
(square 5)        ; => 25
```

## 演習
- 2 数の平均を返す `average` を定義する。

## 関連
- 次章: [リストで式を表す](02-list-as-expression.md)
