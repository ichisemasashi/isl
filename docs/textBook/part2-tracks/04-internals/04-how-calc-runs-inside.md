# I1.4 ★ 処理系の中で isl-calc を動かす（isl-calc 6 周目）

> スパイラル **6 周目（最終）** / 内部編 第 4 章 — 自作評価器と本物の評価器を並べる

## 学習目標
- 自作 `ast-eval` と `core.scm` の `eval-islisp*` を対応づけて理解できる。
- 「処理系の中で動く処理系（メタサーキュラ）」という構図を俯瞰できる。
- 本シリーズ全体を振り返り、次に学ぶべき方向を見出す。

## 前提知識
- 内部編 [I1.1](01-core-scm-tour.md)〜[I1.3](03-runtime-abi.md)、および全 6 周の isl-calc。

## 内容（アウトライン）
- 自作評価器の AST ノード／環境と、`core.scm` のフォーム評価／`frame` 環境を対比。
- 自作の `apply-op` と、`install-primitives!` が登録する組み込み演算の対応。
- isl-calc を isl 上で動かす＝「インタプリタの上でインタプリタを動かす」入れ子。
- 6 周の総括: 同じ題材が REPL → リスト → CLOS → strict → Web → ネイティブ → 内部、と姿を変えてきた道のり。

## 振り返りと次の一歩
- さらに学ぶなら: マクロによる構文拡張、最適化パス、準拠テストへの貢献。
- 参照: `docs/compiler-milestones.md`、`docs/compliance-matrix.md`。

## 関連
- 最初の周: [★ REPL で式を評価する（isl-calc 0 周目）](../../part1-core/01-introduction/04-first-eval-in-repl.md)
- 全体ガイド: [00-README](../../00-README.md)
