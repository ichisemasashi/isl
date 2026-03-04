# DBMS 本番RDBMS化ロードマップ

## 1. 目的
本書は、`examples/dbms` を Wiki 互換 DB から「一般的な本番RDBMS運用が可能な水準」へ拡張するための実装手順を定義する。

前提:
- 現行は `MVP Core` + `Wiki Compatibility v1` を満たす。
- `./bin/isl` と `./bin/islc-run` の挙動同一を継続保証する。

## 2. 現状ギャップ（要約）
- 同時実行制御が弱い（`BEGIN/COMMIT` はバッチ境界、分離保証なし）
- クラッシュ復旧（WAL/REDO/UNDO）がない
- ストレージ整合性検証・破損検知が弱い
- インデックス実体と最適化が限定的
- 運用機能（認証/権限、監視、保守、バックアップ管理）が不足

## 3. 実装原則
- 原則は小さく分割し、各段階で「互換を壊さない」。
- 各段階で必ず:
  - 仕様追記（`SPEC.md`）
  - conformance test 追加（`examples/dbms/tests/`）
  - `isl` / `islc-run` の両経路同一確認
- 不可避の言語機能不足があれば interpreter/compiler を同時拡張する。

## 4. 実装ステップ

### Step 0: ベースライン固定
作業:
- 現行テストをスナップショット化（回帰基準）
- テスト実行スクリプトを固定（両経路連続実行）

完了条件:
- 全既存テストが両経路で一致してパス
- 失敗時に差分（入力SQL、結果、エラーコード）を必ず出力できる

---

### Step 1: トランザクション基盤（単一ノードACIDの土台）
作業:
- `BEGIN/COMMIT/ROLLBACK` の意味を強化
- トランザクションコンテキスト導入（read/write set、状態遷移）
- 文単位 atomic からトランザクション単位 atomic へ拡張

実装ポイント:
- `engine.lsp`: tx-state と commit path を分離
- `storage.lsp`: commit staging 領域を導入

完了条件:
- 明示的 `ROLLBACK` で完全巻き戻し
- 複数文トランザクションの中間状態が外部に見えない

---

### Step 2: ロック管理と分離レベル
作業:
- 2PL 系のロックマネージャ導入（table lock から開始し、必要なら row lock）
- 分離レベルを最低 2 段階提供:
  - `READ COMMITTED`
  - `SERIALIZABLE`（初期は保守的実装で可）
- デッドロック検知（wait-for graph）または timeout rollback

完了条件:
- 競合シナリオで dirty read / lost update を防止
- デッドロック時に規定エラーコードで一方を abort

---

### Step 3: WAL とクラッシュリカバリ
作業:
- WAL 形式を定義（LSN, txid, redo payload, commit marker）
- `write-ahead` 順序保証
- 起動時 recovery 実装（redo/undo 方針を固定）

実装ポイント:
- `storage.lsp`: data file 書き込み前に WAL flush
- `main.lsp` 起動時に recovery フック

完了条件:
- 強制クラッシュ試験で committed tx の永続性を確認
- uncommitted tx が復旧後に見えない

---

### Step 4: ストレージエンジン再編（ページ化）
作業:
- 行単位ファイル管理からページ管理へ移行
- page header（checksum, LSN, free space）導入
- フリーページ管理、vacuum 相当の compaction を実装

完了条件:
- 大量更新/削除後のファイル肥大が制御される
- page checksum エラーを検知して復旧または停止できる

---

### Step 5: インデックス実体と整合性
作業:
- B+Tree 実装（最低: unique index + range scan）
- 主キー/ユニーク制約をインデックスベースへ統合
- DML と index メンテナンスの原子性保証

完了条件:
- point lookup / range query の性能改善を計測で確認
- 破損注入テストで不整合を検知可能

---

### Step 6: オプティマイザ最小実用化
作業:
- 統計情報（row count, ndv, null fraction）収集
- 実行計画の候補比較（seq scan vs index scan, join order）
- `EXPLAIN` 互換出力

完了条件:
- 主要クエリで誤ったプラン固定を回避
- `ANALYZE` 実行後にプランが改善される

---

### Step 7: DDL とオンライン運用性
作業:
- `ALTER TABLE` のオンライン実行戦略（lock 粒度最適化）
- バックフィル付きカラム追加
- 制約追加の検証フェーズ分離（`NOT VALID` 相当を検討）

完了条件:
- 大テーブルでのDDLが長時間全停止を引き起こさない

---

### Step 8: セキュリティ・運用管理
作業:
- ユーザー/ロール/権限モデル
- 監査ログ（DDL, 権限変更, tx abort）
- バックアップ世代管理、PITR（WAL 利用）
- メトリクス公開（tx/sec, lock wait, cache hit, recovery lag）

完了条件:
- 最低限の運用 Runbook で保守可能
- 監査証跡から変更経路を追跡できる

## 5. テスト計画（追加）
テスト階層:
- unit: lock manager / WAL parser / B+Tree node split
- integration: tx + DML + DDL + recovery
- durability: 強制 kill 後の再起動検証
- concurrency: 多セッション競合（lost update, deadlock）
- compatibility: 既存 Wiki 回帰テスト全件再実行

必須ゲート:
- 全テストを `./bin/isl` と `./bin/islc-run` の両方で一致パス
- 新規エラーコードを `repr.lsp` で固定し、契約テスト化

## 6. 推奨実装順（現実的）
1. Step 1（tx強化）
2. Step 3（WAL/recovery）
3. Step 2（ロック/分離）
4. Step 5（B+Tree index）
5. Step 6（optimizer）
6. Step 4（ページ化）
7. Step 7（オンラインDDL）
8. Step 8（運用機能）

理由:
- 先に durability/atomicity を固めないと、後段機能の正当性テストが不安定になるため。

## 7. マイルストーン提案
- `P1`: ACID 基盤（Step 1, 3）
- `P2`: 同時実行制御（Step 2）
- `P3`: 性能基盤（Step 5, 6）
- `P4`: 運用基盤（Step 4, 7, 8）

各 `P` 完了時に `SPEC.md` へ正式仕様として昇格し、実験実装を削除して契約を固定する。
