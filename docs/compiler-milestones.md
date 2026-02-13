# ISLISP コンパイラ実装マイルストーン分解（標準準拠）

この計画は `docs/compiler-compliance-spec.md` の準拠要件を実装工程に分解したもの。
各マイルストーンは「完了条件を満たすまで次へ進まない」ことを前提とする。

## M0. 準備: 準拠基準とテスト土台

目的:

- 準拠判定の土台を先に固定する。

作業:

1. `strict`/`extended` プロファイルを仕様化し CLI フラグ設計を確定。
2. interpreter をオラクルとして差分テストハーネスを作成。
3. 既存テストを「標準コア」「拡張」に分類。

成果物:

1. `docs/compliance-matrix.md`（機能一覧と対応状態）
2. `test/conformance/` 雛形（oracle compare 実行）
3. `--profile strict|extended` の実行モード定義

完了ゲート:

1. `strict` テストセットが自動実行可能。
2. interpreter/compiler 比較インタフェースが固定。


## M1. フロントエンド骨格（read/macroexpand/normalize）

目的:

- コンパイラ入力を展開済みフォームに統一。

作業:

1. `read -> macroexpand -> normalize(IR)` パイプラインを実装。
2. `macroexpand` の動作を interpreter と同一化。
3. 正規化 IR（副作用順序を明示）を定義。

成果物:

1. `src/compiler/frontend.*`
2. `src/compiler/ir.*`
3. IR ダンプ機能（デバッグ用）

完了ゲート:

1. マクロ展開一致テスト pass。
2. 主要フォーム（定数・変数・call・if・progn）が IR 化可能。


## M2. ランタイム最小核（値表現・環境・呼出規約）

目的:

- 標準意味論を保持できる実行基盤を先に作る。

作業:

1. tagged value 仕様を確定（number/symbol/cons/function など）。
2. lexical 環境・クロージャ・可変長引数の runtime 実装。
3. 例外/エラー共通表現の導入。

成果物:

1. `runtime/`（値表現・メモリ管理・関数呼び出し）
2. ABI ドキュメント（コンパイラと runtime の境界）

完了ゲート:

1. クロージャ/シャドーイング/rest 引数テスト pass。
2. パラメータ不一致時エラーが interpreter と一致。


## M3. LLVM AOT バックエンド最小実装

目的:

- 独自ネイティブ生成ではなく LLVM で AOT 実行を成立させる。

作業:

1. `IR -> LLVM IR` 変換実装。
2. `llvm-opt/llc/lld` または API で object 生成。
3. ランタイムとリンクして実行可能バイナリ/共有ライブラリ化。

成果物:

1. `islc`（AOT コンパイラ実行ファイル）
2. `--emit-llvm` / `--emit-obj` / `--run` モード

完了ゲート:

1. 単純関数（算術、if、let、defun 呼び出し）で interpreter と同値。
2. x86_64/arm64 の smoke テスト pass。


## M4. 評価順序と副作用の完全固定

目的:

- 最適化前に順序の同値性を完成させる。

作業:

1. effect barrier ノードを IR に導入。
2. call/set/IO/throw 可能演算を順序固定ノード化。
3. `setf` place 評価順序を明文化し実装。

成果物:

1. 副作用順序仕様書（短い規範）
2. 順序検証テスト群（ログ比較）

完了ゲート:

1. `-O0` で順序テスト全件 pass。
2. interpreter/compiler の副作用ログ差分ゼロ。


## M5. 非局所制御の実装（最難関1）

目的:

- `block/return-from`, `catch/throw`, `tagbody/go` を正確実装。

作業:

1. unwind 基盤を 1 方式に統一（`invoke/landingpad` か `setjmp/longjmp`）。
2. 脱出ターゲット管理スタックを runtime 実装。
3. 脱出先不在時エラーを規範通り実装。

成果物:

1. 非局所制御 runtime モジュール
2. IR 命令 `BLOCK_ENTER/EXIT`, `NONLOCAL_EXIT`, `TAGBODY_JUMP`

完了ゲート:

1. ネスト脱出の組合せテスト pass。
2. 失敗系テスト（未捕捉 throw 等）pass。


## M6. 条件処理とハンドラ（最難関2）

目的:

- `handler-case` を非局所制御と整合した形で成立。

作業:

1. handler stack 実装。
2. 発生・捕捉・再送出の制御遷移を IR/Runtime に実装。
3. 非局所脱出と cleanup を同じ unwind で保証。

成果物:

1. 条件処理 runtime
2. 条件処理テスト（捕捉/未捕捉/再送出）

完了ゲート:

1. 例外経路で副作用順序一致。
2. ハンドラ探索順序一致。


## M7. オブジェクトシステム移植

目的:

- `defclass/defgeneric/defmethod` を準拠挙動で一致させる。

作業:

1. class/generic/method データ構造を runtime へ実装。
2. 適用可能性判定・曖昧性判定を interpreter 同等化。
3. slot accessor の生成規則を一致。

成果物:

1. CLOS 相当 runtime 層
2. 多重継承・曖昧性テスト群

完了ゲート:

1. ディスパッチ結果一致。
2. 曖昧ケースのエラー一致。


## M8. 数値境界・比較・型エラー整合

目的:

- 数値と型境界の差異を潰し、strict 準拠を安定化。

作業:

1. exact/inexact 変換規則の固定。
2. 比較述語と演算の型境界を統一。
3. fast-math 無効を既定にして差異を遮断。

成果物:

1. 数値セマンティクス仕様書
2. 境界値テスト群

完了ゲート:

1. 境界値テストで差分ゼロ。
2. `-O0/-O2` で結果一致。


## M9. パッケージ/シンボル解決整合

目的:

- コンパイル済みコードにおけるシンボル解決の意味を固定。

作業:

1. 早期解決/遅延解決ポリシーを選択し固定。
2. package 切替時の影響範囲を仕様化。
3. 同名シンボル衝突の挙動を一致。

成果物:

1. symbol resolution 仕様
2. package 回帰テスト

完了ゲート:

1. package 系テスト pass。
2. strict モードで標準外の漏れなし。


## M10. 最適化段階導入（意味不変）

目的:

- 準拠成立後に性能改善を段階導入。

作業:

1. LLVM パス whitelist を作成。
2. パスごとに意味回帰テストを実施。
3. inline cache 等の高級最適化をオプトインで追加。

成果物:

1. `docs/optimization-policy.md`
2. 最適化有無比較ベンチ

完了ゲート:

1. 全準拠テストが `-O0/-O2` で一致。
2. 主要ベンチで性能改善が確認できる。


## M11. ORC JIT（任意・後段）

目的:

- REPL/インタラクティブ用途を高速化。

作業:

1. ORC v2 JIT 実装。
2. AOT と同じ runtime ABI を共有。
3. JIT/AOT の結果一致テスト追加。

成果物:

1. `--jit` モード
2. JIT 回帰テスト

完了ゲート:

1. AOT/JIT/interpreter の三者一致。
2. REPL で安定稼働。


## マイルストーン横断の運用ルール

1. 各 M の完了ゲートを満たすまで次 M に進まない。
2. 失敗系テスト（エラー経路）を必ず同時追加する。
3. 仕様変更は `docs/compiler-compliance-spec.md` を先に更新してから実装する。
4. 既知差分を残す場合は issue 化し、`strict` では未解決差分を許容しない。


## 推奨実行順（最短準拠ルート）

1. M0
2. M1
3. M2
4. M3
5. M4
6. M5
7. M6
8. M7
9. M8
10. M9
11. M10
12. M11（任意）
