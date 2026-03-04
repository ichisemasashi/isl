# DBMS 本番RDBMS化 Issue 分解

## 1. 使い方
- 本書は `PROD_RDBMS_ROADMAP.md` を実装タスク粒度へ分解したバックログ。
- 各 Issue は「単体で着手可能」「完了判定が明確」になるように記述する。
- すべての Issue で `./bin/isl` と `./bin/islc-run` の挙動一致を必須とする。

共通完了条件（全 Issue）:
- 仕様差分を `examples/dbms/SPEC.md` に反映
- テストを `examples/dbms/tests/` に追加
- 既存回帰（mvp/wiki 既存テスト）が壊れない

## 2. 優先順（実装順）
1. `P1` ACID 基盤
2. `P2` 同時実行制御
3. `P3` 性能基盤
4. `P4` 運用基盤

---

## 3. Issue 一覧

## P1: ACID 基盤

### DBMS-P1-001: 回帰ベースライン固定
- 目的: 既存機能の非意図的破壊を即時検知する。
- 依存: なし
- 作業:
  - 両経路テスト実行スクリプトを追加（`examples/dbms/tests/run_all.sh`）
  - 失敗時に SQL/期待/実際/エラーコードを表示
- 変更想定:
  - `examples/dbms/tests/`
- 受け入れ条件:
  - 既存 `t*.lsp` 全件を連続実行し、結果サマリを出力できる。

### DBMS-P1-002: トランザクション状態管理の導入
- 目的: 文単位更新から tx 単位更新へ移行する。
- 依存: `DBMS-P1-001`
- 作業:
  - tx-state 構造（`idle/active/aborted/committed`）を導入
  - セッション内の read/write set を保持
- 変更想定:
  - `examples/dbms/app/engine.lsp`
  - `examples/dbms/app/repr.lsp`
- 受け入れ条件:
  - `BEGIN` 後に tx-state が `active` になる。
  - `COMMIT/ROLLBACK` 後に `idle` へ戻る。

### DBMS-P1-003: `ROLLBACK` の構文/実行対応
- 目的: 失敗時の巻き戻しを提供する。
- 依存: `DBMS-P1-002`
- 作業:
  - `ROLLBACK` を lexer/parser/engine へ追加
  - `active` tx の staged 変更を破棄
- 変更想定:
  - `examples/dbms/app/sql_lexer.lsp`
  - `examples/dbms/app/sql_parser.lsp`
  - `examples/dbms/app/engine.lsp`
- 受け入れ条件:
  - `BEGIN; INSERT; ROLLBACK;` 後に挿入行が存在しない。

### DBMS-P1-004: commit staging 実装
- 目的: tx 中間状態を外部不可視にする。
- 依存: `DBMS-P1-002`
- 作業:
  - table/catalog 更新を staging 領域へ書く
  - `COMMIT` 成功時のみ本体へ反映
- 変更想定:
  - `examples/dbms/app/storage.lsp`
  - `examples/dbms/app/engine.lsp`
- 受け入れ条件:
  - tx 中に別 read から未コミット更新が見えない。

### DBMS-P1-005: tx エラーコード契約固定
- 目的: 呼び出し側のハンドリングを安定化する。
- 依存: `DBMS-P1-003`, `DBMS-P1-004`
- 作業:
  - tx 系エラーコードを `repr.lsp` で固定
  - 契約テストを追加
- 変更想定:
  - `examples/dbms/app/repr.lsp`
  - `examples/dbms/tests/`
- 受け入れ条件:
  - 二重 `BEGIN`、`COMMIT` without `BEGIN` などのエラーが固定コードで返る。

### DBMS-P1-006: WAL record 仕様策定
- 目的: recovery 実装前に永続ログ形式を固定する。
- 依存: `DBMS-P1-005`
- 作業:
  - WAL レコード形式（LSN/txid/op/payload/commit）を定義
  - `SPEC.md` に追記
- 変更想定:
  - `examples/dbms/SPEC.md`
- 受け入れ条件:
  - 形式・順序保証・破損時挙動が文書で確定している。

### DBMS-P1-007: WAL append/flush 実装
- 目的: write-ahead 原則を実装する。
- 依存: `DBMS-P1-006`
- 作業:
  - DML/DDL 変更前に WAL 追記
  - `COMMIT` で WAL flush 完了を保証
- 変更想定:
  - `examples/dbms/app/storage.lsp`
  - `examples/dbms/app/engine.lsp`
- 受け入れ条件:
  - data 書き込み前に対応 WAL が必ず存在する。

### DBMS-P1-008: 起動時 recovery 実装
- 目的: クラッシュ後整合性を回復する。
- 依存: `DBMS-P1-007`
- 作業:
  - 起動時に WAL スキャン
  - committed を redo、未commit を取り消し
- 変更想定:
  - `examples/dbms/app/main.lsp`
  - `examples/dbms/app/storage.lsp`
- 受け入れ条件:
  - 強制 kill 後の再起動で committed のみ見える。

### DBMS-P1-009: durability 試験追加
- 目的: クラッシュ耐性を自動検証する。
- 依存: `DBMS-P1-008`
- 作業:
  - kill/restart を含む耐久テストを追加
- 変更想定:
  - `examples/dbms/tests/prod/`
- 受け入れ条件:
  - 10 回以上の繰り返し試験で不整合 0 件。

---

## P2: 同時実行制御

### DBMS-P2-001: ロックマネージャ（table lock）導入
- 目的: 競合更新を制御する最小実装。
- 依存: `DBMS-P1-009`
- 作業:
  - S/X lock 管理
  - lock table の可視化 API（debug）
- 変更想定:
  - `examples/dbms/app/engine.lsp`
- 受け入れ条件:
  - write-write 競合時に待機または失敗が発生する。

### DBMS-P2-002: `READ COMMITTED` 実装
- 目的: 最低限の分離レベル提供。
- 依存: `DBMS-P2-001`
- 作業:
  - statement 境界で read lock 解放
  - uncommitted read を防止
- 受け入れ条件:
  - dirty read シナリオが再現しない。

### DBMS-P2-003: `SERIALIZABLE` 実装（保守的）
- 目的: 高整合レベル提供。
- 依存: `DBMS-P2-002`
- 作業:
  - tx 完了まで必要 lock を保持
  - 競合時は abort 優先実装
- 受け入れ条件:
  - lost update/write skew 系ケースを防止。

### DBMS-P2-004: デッドロック検知
- 目的: 無限待機を防ぐ。
- 依存: `DBMS-P2-001`
- 作業:
  - wait-for graph もしくは lock timeout
  - victim abort ポリシー導入
- 受け入れ条件:
  - deadlock シナリオで有限時間内に 1 tx が abort される。

### DBMS-P2-005: 並行実行テストハーネス
- 目的: 競合シナリオを継続検証する。
- 依存: `DBMS-P2-004`
- 作業:
  - multi-session テスト（2〜4セッション）
  - dirty read/lost update/deadlock を固定
- 変更想定:
  - `examples/dbms/tests/prod/`
- 受け入れ条件:
  - 両経路で同じ最終結果・同じ abort 条件となる。

---

## P3: 性能基盤

### DBMS-P3-001: B+Tree ページフォーマット定義
- 目的: index 実体の I/O 形式を固定。
- 依存: `DBMS-P1-009`
- 作業:
  - internal/leaf ノード形式、split/merge 規約の定義
- 変更想定:
  - `examples/dbms/SPEC.md`
- 受け入れ条件:
  - 形式がテスト可能なレベルで文書化されている。

### DBMS-P3-002: B+Tree 基本実装（insert/search）
- 目的: point lookup を index 化する。
- 依存: `DBMS-P3-001`
- 作業:
  - node split、root 昇格、key search
- 変更想定:
  - `examples/dbms/app/storage.lsp`
  - `examples/dbms/app/engine.lsp`
- 受け入れ条件:
  - 1万件規模で full scan より lookup が有意に高速。

### DBMS-P3-003: unique/pk 制約の index 統合
- 目的: 制約判定の一貫性と性能向上。
- 依存: `DBMS-P3-002`
- 作業:
  - PK/UNIQUE チェックを index 経由へ置換
- 受け入れ条件:
  - 競合挿入時に重複判定が正確で race しない。

### DBMS-P3-004: range scan 実装
- 目的: `ORDER BY/LIMIT` や範囲検索を最適化。
- 依存: `DBMS-P3-002`
- 作業:
  - leaf 連結走査
  - lower/upper bound seek
- 受け入れ条件:
  - 範囲クエリで seq scan より低コストになる。

### DBMS-P3-005: 統計情報収集（ANALYZE）
- 目的: プラン選択の精度を上げる。
- 依存: `DBMS-P3-002`
- 作業:
  - row count, NDV, null fraction を保持
- 変更想定:
  - `examples/dbms/app/catalog.lsp`
  - `examples/dbms/app/engine.lsp`
- 受け入れ条件:
  - `ANALYZE` 後に統計メタが更新される。

### DBMS-P3-006: 最小オプティマイザ（cost 比較）
- 目的: scan/join 戦略を自動選択。
- 依存: `DBMS-P3-005`
- 作業:
  - seq scan vs index scan 選択
  - 簡易 join order 探索
- 受け入れ条件:
  - 誤った固定プランを回避し、回帰クエリの p95 が改善。

### DBMS-P3-007: `EXPLAIN` 実装
- 目的: 運用時のプラン可観測性を提供。
- 依存: `DBMS-P3-006`
- 作業:
  - chosen plan と cost 根拠を出力
- 受け入れ条件:
  - 実行計画の再現に必要な情報が表示される。

---

## P4: 運用基盤

### DBMS-P4-001: ページストレージ層導入
- 目的: ファイル断片化と肥大を抑える。
- 依存: `DBMS-P1-009`
- 作業:
  - page header（checksum/LSN/free space）
  - free page 管理
- 変更想定:
  - `examples/dbms/app/storage.lsp`
- 受け入れ条件:
  - 更新/削除繰り返し後の容量増加が制御される。

### DBMS-P4-002: compaction/vacuum 実装
- 目的: 長期運用時の領域再利用。
- 依存: `DBMS-P4-001`
- 作業:
  - dead tuple 回収
  - 断片化率メトリクス追加
- 受け入れ条件:
  - vacuum 実行前後で断片化が改善する。

### DBMS-P4-003: オンライン `ALTER TABLE`（ADD COLUMN）
- 目的: 長時間停止なしでスキーマ変更する。
- 依存: `DBMS-P2-005`, `DBMS-P4-001`
- 作業:
  - metadata only 変更 + 遅延バックフィル
- 受け入れ条件:
  - 大テーブルで操作中 read/write が継続可能。

### DBMS-P4-004: 制約検証フェーズ分離
- 目的: 重い制約検証を段階適用する。
- 依存: `DBMS-P4-003`
- 作業:
  - `NOT VALID` 相当
  - 後続 `VALIDATE CONSTRAINT`
- 受け入れ条件:
  - 既存データ大量時でも DDL 開始が高速。

### DBMS-P4-005: 認証・ロール・権限
- 目的: 本番運用最低限のアクセス制御。
- 依存: `DBMS-P1-009`
- 作業:
  - users/roles/grants メタ
  - object level 権限チェック
- 変更想定:
  - `examples/dbms/app/engine.lsp`
  - `examples/dbms/app/catalog.lsp`
- 受け入れ条件:
  - 未許可 SQL が拒否され、監査ログに残る。

### DBMS-P4-006: 監査ログ
- 目的: 追跡可能性を確保する。
- 依存: `DBMS-P4-005`
- 作業:
  - DDL、権限変更、tx abort を記録
  - 監査ログローテーション
- 受け入れ条件:
  - 誰が何をいつ実行したかを復元できる。

### DBMS-P4-007: バックアップ世代管理 + PITR
- 目的: 復旧運用の実効性向上。
- 依存: `DBMS-P1-008`
- 作業:
  - base backup + WAL archive
  - 時点復旧 API
- 受け入れ条件:
  - 任意時点 T への復元が検証できる。

### DBMS-P4-008: メトリクス公開
- 目的: SLO 監視と性能劣化検知。
- 依存: `DBMS-P2-005`, `DBMS-P3-006`
- 作業:
  - tx/sec、lock wait、cache hit、recovery lag を出力
- 受け入れ条件:
  - 運用ダッシュボードに必要な値が取得できる。

### DBMS-P4-009: Runbook/障害手順書
- 目的: 担当交代でも運用可能にする。
- 依存: `DBMS-P4-006`, `DBMS-P4-007`, `DBMS-P4-008`
- 作業:
  - 障害分類、一次切り分け、復旧手順
  - RTO/RPO 想定値を明記
- 受け入れ条件:
  - 手順書のみで演習復旧が成功する。

---

## 4. すぐチケット化する最小セット（推奨）
- `DBMS-P1-001` 〜 `DBMS-P1-009`
- `DBMS-P2-001` 〜 `DBMS-P2-005`
- `DBMS-P3-001` 〜 `DBMS-P3-004`

理由:
- ACID と同時実行を先に固めると、その後の性能改善の検証が成立するため。

## 5. Issue テンプレート
各チケットは以下テンプレートで作成する。

```
Title: [DBMS-Px-xxx] <short summary>
Goal:
Scope:
Out of scope:
Dependencies:
Changes:
- files/modules:
- spec section:
Tests:
- unit:
- integration:
- dual-path parity (isl / islc-run):
Acceptance criteria:
Rollback plan:
```
