# I1.1 core.scm ツアー

> スパイラル 6 周目 / 内部編（part2 第 4 編）第 1 章

## 学習目標
- 処理系 isl の中心 `src/isl/core.scm` の全体構成を俯瞰できる。
- プリミティブがどう登録されるかを知る。

## 前提知識
- 全編をひととおり読了していること（コントリビュータ向け）。

## 内容（アウトライン）
- `core.scm`（約 4700 行）の単一ファイル構成。
- 環境＝ネストした `frame`（vector）でレキシカルスコープを実現、`frame-find-pair` が親を辿る。
- プリミティブ登録: `install-primitives!` 内の `(def 'name proc)`。
- プロファイル管理 `*isl-profile*`、`disable-extended-primitives!`。

## 関連
- 参照: `CLAUDE.md`（アーキテクチャ概要）
- 次章: [評価ループ](02-eval-loop.md)
