# 副作用順序仕様（M4）

この文書は compiler 実行経路における副作用順序の規範を定義する。

## 1. 基本規則

1. 評価は左から右へ行う。
2. 副作用を持ち得る演算は `effect` バリアノード配下に正規化する。
3. 最適化の有無（少なくとも `-O0`）で観測順序を変更してはならない。

## 2. 対象演算

1. 関数呼び出し（`call`）
2. 代入（`setq`/`setf`）
3. 非局所制御を発生し得る演算（`throw`, `return-from`, `go`）
4. I/O を行うプリミティブ（例: `print`）

## 3. 形式別規則

1. 関数呼び出し:
- callee, 引数1, 引数2... の順で評価する。

2. `let`:
- 初期化式は外側環境で左から右に評価し、
  評価結果を新環境へ一括束縛してから body を実行する。

3. `setf` (`vector-ref` place):
- vector 式, index 式, value 式の順で各1回だけ評価する。

4. `if`:
- test を評価し、選択された分岐だけを評価する。

## 4. 検証

1. `test/compiler/runtime-m4-smoke.scm`
2. `test/compiler/runtime-m4-oracle.scm`

上記テストで interpreter と compiler runtime の結果が一致することを M4 完了条件とする。
