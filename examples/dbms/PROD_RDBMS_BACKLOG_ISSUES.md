# DBMS 本番化バックログ Issue 展開

本書は `examples/dbms/PROD_RDBMS_BACKLOG.md` の 12 項目を、実装着手可能な Issue テンプレート形式へ展開したものです。

---

Title: [ST-001] P2-005 並行実行テストハーネスの本格実装
Goal:
- 同時実行バグ（dirty read/lost update/deadlock）の検出精度を高める。
Scope:
- 2〜4 セッションの競合実行ドライバ
- 代表競合シナリオの固定化
- 失敗時の tx timeline 出力
Out of scope:
- row lock / MVCC の実装そのもの
Dependencies:
- 現行 lock manager / isolation 実装（P2 系）
Changes:
- files/modules:
  - `examples/dbms/tests/prod/`
  - `examples/dbms/tests/run_all.sh`（必要時）
  - `examples/dbms/app/engine.lsp`（診断 API が必要な場合）
- spec section:
  - `examples/dbms/SPEC.md` に並行テスト契約を追記
Tests:
- unit:
  - wait-for graph / lock snapshot 補助関数
- integration:
  - multi-session 競合ケース（dirty read/lost update/deadlock）
- dual-path parity (isl / islc-run):
  - 全シナリオで同一最終状態・同一エラーコード
Acceptance criteria:
- 競合シナリオが再現可能で、両経路で結果が一致する。
Rollback plan:
- 新規ハーネスを feature flag 化し、既存回帰は維持して切り戻せるようにする。

---

Title: [ST-002] WAL/復旧の運用安全化（checkpoint 基礎）
Goal:
- recovery 時間を短縮し、復旧時のリスクを下げる。
Scope:
- checkpoint メタ（LSN）形式定義
- checkpoint 生成/読込 API
- recovery 開始位置の checkpoint 基準化
Out of scope:
- 分散レプリケーション
Dependencies:
- WAL/recovery 基盤（P1-007, P1-008）
Changes:
- files/modules:
  - `examples/dbms/app/storage.lsp`
  - `examples/dbms/app/engine.lsp`
  - `examples/dbms/tests/prod/`
- spec section:
  - `examples/dbms/SPEC.md` に checkpoint 契約追記
Tests:
- unit:
  - checkpoint 読込/妥当性検証
- integration:
  - checkpoint あり/なし両ケースの再起動復旧
- dual-path parity (isl / islc-run):
  - 同一 WAL から同一復旧結果
Acceptance criteria:
- WAL 全走査なしで復旧でき、復旧時間短縮が確認できる。
Rollback plan:
- checkpoint を無効化して従来 recovery パスへ戻せる。

---

Title: [ST-003] PITR 運用強化（世代保持ポリシー）
Goal:
- バックアップ世代を運用可能な形で継続管理する。
Scope:
- 保持件数/保持期間ポリシー
- 古い世代削除 API
- Runbook の定期ドリル実行支援
Out of scope:
- 外部オブジェクトストレージ連携
Dependencies:
- P4-007（backup generation + PITR）
- P4-009（Runbook）
Changes:
- files/modules:
  - `examples/dbms/app/storage.lsp`
  - `examples/dbms/app/engine.lsp`
  - `examples/dbms/tests/prod/`
  - `examples/dbms/RUNBOOK.md`
- spec section:
  - `examples/dbms/SPEC.md` の backup/PITR 節拡張
Tests:
- unit:
  - 世代選別・削除判定
- integration:
  - 期限超過世代削除 + PITR 成功ケース
- dual-path parity (isl / islc-run):
  - 世代一覧/削除/PITR の同一挙動
Acceptance criteria:
- 世代が無制限増加せず、PITR 運用が継続可能。
Rollback plan:
- 保持ポリシー適用を停止し、既存世代管理のみで運用継続。

---

Title: [ST-004] メトリクス外部公開形式固定
Goal:
- 監視連携時の互換崩れを防ぐ。
Scope:
- `dbms-admin-metrics` 出力スキーマ固定
- 機械可読フォーマットの追加（互換 API）
- しきい値サンプルの文書化
Out of scope:
- Prometheus サーバ実装
Dependencies:
- P4-008（メトリクス公開）
Changes:
- files/modules:
  - `examples/dbms/app/engine.lsp`
  - `examples/dbms/tests/prod/`
  - `examples/dbms/RUNBOOK.md`
- spec section:
  - `examples/dbms/SPEC.md` の metrics 節拡張
Tests:
- unit:
  - メトリクスキー/型検証
- integration:
  - 監視取り込み想定の固定フォーマット検証
- dual-path parity (isl / islc-run):
  - 出力スキーマと値型が一致
Acceptance criteria:
- 外部ダッシュボードが仕様変更なしで継続利用できる。
Rollback plan:
- 互換 API を残し、新形式のみ切り戻し可能にする。

---

Title: [MT-001] row lock 導入（table lock から段階移行）
Goal:
- 非競合更新の並列性を向上させる。
Scope:
- row lock データ構造
- table+row 二層 lock 判定
- deadlock 検知の row 対応
Out of scope:
- MVCC visibility ルール
Dependencies:
- ST-001（並行実行ハーネス）
Changes:
- files/modules:
  - `examples/dbms/app/engine.lsp`
  - `examples/dbms/tests/prod/`
- spec section:
  - `examples/dbms/SPEC.md` の lock/isolation 節更新
Tests:
- unit:
  - lock compatibility matrix（row/table）
- integration:
  - 同一 table 異なる row の並行更新
- dual-path parity (isl / islc-run):
  - lock 判定・abort 条件が一致
Acceptance criteria:
- 異なる行更新が並列に進み、競合ケースのみ待機/失敗する。
Rollback plan:
- row lock 経路を無効化し table lock のみへ戻せる。

---

Title: [MT-002] MVCC ベース読み取り分離
Goal:
- read/write 干渉を減らし、分離保証を改善する。
Scope:
- version chain 形式
- snapshot visibility ルール
- write conflict 判定
Out of scope:
- 分散トランザクション
Dependencies:
- MT-001
- ST-002
Changes:
- files/modules:
  - `examples/dbms/app/storage.lsp`
  - `examples/dbms/app/engine.lsp`
  - `examples/dbms/app/repr.lsp`（必要時）
  - `examples/dbms/tests/prod/`
- spec section:
  - `examples/dbms/SPEC.md` に MVCC 意味論追記
Tests:
- unit:
  - visibility 判定関数
- integration:
  - snapshot read / write skew 防止ケース
- dual-path parity (isl / islc-run):
  - 同一 snapshot で同一読取結果
Acceptance criteria:
- 代表ワークロードで lock 待機が減少し、整合性契約を満たす。
Rollback plan:
- MVCC 有効化フラグを切り、従来 lock 主体経路へ戻せる。

---

Title: [MT-003] optimizer 強化（JOIN/複合条件）
Goal:
- 複雑クエリで不適切プラン選択を減らす。
Scope:
- join order 探索改善
- 統計利用範囲拡張
- explain 出力拡充
Out of scope:
- 完全なコストベース最適化
Dependencies:
- P3-005, P3-006, P3-007
- MT-002（必要に応じて）
Changes:
- files/modules:
  - `examples/dbms/app/engine.lsp`
  - `examples/dbms/app/catalog.lsp`
  - `examples/dbms/tests/prod/`
- spec section:
  - `examples/dbms/SPEC.md` optimizer/explain 契約更新
Tests:
- unit:
  - cost 比較ロジック
- integration:
  - JOIN/複合条件の plan 選択回帰
- dual-path parity (isl / islc-run):
  - 同一 SQL で同一プラン選択
Acceptance criteria:
- 主要クエリで seq scan 固定が減り、p95 が改善する。
Rollback plan:
- 新 planner を無効化し最小 planner へ戻せる。

---

Title: [MT-004] DDL 運用性強化（バックフィル管理）
Goal:
- 大テーブル変更時の可用性を維持する。
Scope:
- ADD COLUMN バックフィル進捗管理
- 速度制御・中断再開
- VALIDATE CONSTRAINT の並行制御
Out of scope:
- 全 DDL の完全オンライン化
Dependencies:
- P4-003, P4-004
Changes:
- files/modules:
  - `examples/dbms/app/engine.lsp`
  - `examples/dbms/app/catalog.lsp`
  - `examples/dbms/tests/prod/`
- spec section:
  - `examples/dbms/SPEC.md` DDL 運用節追記
Tests:
- unit:
  - 進捗状態遷移
- integration:
  - 長時間 backfill の中断再開
- dual-path parity (isl / islc-run):
  - 進捗値・最終整合性が一致
Acceptance criteria:
- DDL 実行中も read/write が継続し、最終整合が保たれる。
Rollback plan:
- オンラインモードを停止し、従来手順へ切り替え可能。

---

Title: [LT-001] レプリケーション（非同期→同期）
Goal:
- 単一ノード障害時の可用性を高める。
Scope:
- WAL shipping
- follower apply
- manual failover 手順
Out of scope:
- 自動フェイルオーバー
Dependencies:
- ST-002
- ST-003
Changes:
- files/modules:
  - `examples/dbms/app/storage.lsp`
  - `examples/dbms/app/engine.lsp`
  - `examples/dbms/RUNBOOK.md`
  - `examples/dbms/tests/prod/`
- spec section:
  - `examples/dbms/SPEC.md` replication 契約追記
Tests:
- unit:
  - WAL apply 一致性
- integration:
  - primary/follower の追従検証
- dual-path parity (isl / islc-run):
  - 同一 WAL stream で同一最終状態
Acceptance criteria:
- 障害演習で RTO/RPO 目標内の復旧が可能。
Rollback plan:
- レプリケーション停止で単一ノード運用へ戻せる。

---

Title: [LT-002] 自動フェイルオーバー
Goal:
- 障害切替の人手依存を削減する。
Scope:
- health check
- leader election
- split-brain 防止
Out of scope:
- 地理冗長（multi-region）
Dependencies:
- LT-001
Changes:
- files/modules:
  - `examples/dbms/app/`（制御モジュール追加）
  - `examples/dbms/RUNBOOK.md`
  - `examples/dbms/tests/prod/`
- spec section:
  - `examples/dbms/SPEC.md` failover 契約追記
Tests:
- unit:
  - election/health 判定
- integration:
  - primary 障害時の自動昇格シナリオ
- dual-path parity (isl / islc-run):
  - 同一障害注入で同一切替結果
Acceptance criteria:
- 自動切替演習が成功し、split-brain を起こさない。
Rollback plan:
- 自動切替を停止し manual failover に戻す。

---

Title: [LT-003] ストレージ保守の高度化
Goal:
- 長期運用時の性能劣化を抑える。
Scope:
- autovacuum
- auto analyze
- compaction scheduler
- 破損検知時の対応方針
Out of scope:
- 分散ストレージ化
Dependencies:
- P4-001, P4-002
- MT-002（推奨）
Changes:
- files/modules:
  - `examples/dbms/app/storage.lsp`
  - `examples/dbms/app/engine.lsp`
  - `examples/dbms/tests/prod/`
  - `examples/dbms/RUNBOOK.md`
- spec section:
  - `examples/dbms/SPEC.md` maintenance 節追加
Tests:
- unit:
  - スケジューラ判定
- integration:
  - 長時間更新後の自動保守効果検証
- dual-path parity (isl / islc-run):
  - 保守起動条件と結果が一致
Acceptance criteria:
- 手動介入なしで断片化/統計劣化を抑制できる。
Rollback plan:
- 自動保守を無効化し手動保守運用へ戻す。

---

Title: [LT-004] セキュリティ強化（認証・通信・監査耐改ざん）
Goal:
- 監査要件・セキュリティ要件への適合度を上げる。
Scope:
- 認証情報ハッシュ方針の強化
- 通信保護（TLS）方針
- 監査ログ改ざん検知
Out of scope:
- IAM 外部プロバイダ統合
Dependencies:
- P4-005, P4-006
- LT-001（推奨）
Changes:
- files/modules:
  - `examples/dbms/app/engine.lsp`
  - `examples/dbms/app/storage.lsp`
  - `examples/dbms/RUNBOOK.md`
  - `examples/dbms/tests/prod/`
- spec section:
  - `examples/dbms/SPEC.md` security/audit 節拡張
Tests:
- unit:
  - ハッシュ検証・改ざん検知
- integration:
  - 権限拒否/監査追跡/耐改ざんシナリオ
- dual-path parity (isl / islc-run):
  - 認証・監査結果の一致
Acceptance criteria:
- セキュリティレビュー観点で重大欠陥が残らない状態を達成。
Rollback plan:
- 強化機能を段階無効化し、既存 RBAC/監査機能へ戻す。

