# DBMS Runbook / 障害手順書

本書は `examples/dbms` の運用引き継ぎ用手順書です。  
目的は「障害分類」「一次切り分け」「復旧手順」「RTO/RPO 目標」を固定し、担当交代時でも同一手順で復旧できるようにすることです。

## 1. 前提
- 対象実装: ISL DBMS (`examples/dbms/app/*.lsp`)
- ストレージルート: `DBMS_STORAGE_ROOT`
- 主要管理 API:
  - `dbms-admin-metrics`
  - `dbms-admin-backup-create`
  - `dbms-admin-backup-list`
  - `dbms-admin-restore-pitr`
  - `dbms-admin-dump`
  - `dbms-admin-restore`

## 2. RTO / RPO
- `S1`（停止・破損）:
  - 目標 RTO: 15 分
  - 目標 RPO: 5 分
- `S2`（劣化・競合増大）:
  - 目標 RTO: 30 分（恒久対応まで）
  - 目標 RPO: 0 分（データ欠損なし）
- `S3`（権限・監査）:
  - 目標 RTO: 10 分
  - 目標 RPO: 0 分

注: 目標値は現行の単一ノード運用前提。運用条件が変わる場合は本書を更新すること。

## 3. 障害分類
- `S1` 可用性/整合性障害:
  - DB 起動不可
  - ストレージ破損疑い
  - 誤更新/誤削除からの時点復旧が必要
- `S2` 性能/同時実行障害:
  - lock conflict/deadlock 増加
  - tx/sec 低下
  - cache hit ratio 低下
  - recovery lag が高止まり
- `S3` セキュリティ/運用統制障害:
  - permission denied の急増
  - 権限設定誤り
  - 監査ログ確認要求

## 4. 一次切り分け（5分以内）
1. 監視取得:
   - `dbms-admin-metrics` で次を確認:
     - `tx-per-sec`
     - `lock-wait-events`
     - `cache-hit-ratio`
     - `recovery-lag-records`
2. 直近監査:
   - `audit.lspdata` と `audit.archive.*.lspdata` を確認
   - `TX-ABORT`, `PRIVILEGE`, `DENY` の増加有無を確認
3. バックアップ可用性:
   - `dbms-admin-backup-list` が空でないか確認
4. 判定:
   - 起動不可/データ不整合は `S1`
   - エラー率増大・遅延は `S2`
   - 権限/監査要件は `S3`

## 5. 復旧手順

### 5.1 S1: 時点復旧（PITR）
1. 現状保全:
   - `DBMS_STORAGE_ROOT` を別ディレクトリへ退避
2. 復旧点選定:
   - `dbms-admin-backup-list` の世代から `generation-id` を選択
   - 監査・アプリログ時刻から `target-lsn` を選ぶ
3. 実行:
   - `dbms-admin-restore-pitr <generation-id> <target-lsn>`
4. 検証:
   - 主要テーブル件数・代表レコードを `SELECT` で確認
   - `dbms-admin-metrics` の `recovery-lag-records` を確認
5. 再開:
   - 書込みトラフィックを段階的に戻す

### 5.2 S2: ロック競合/性能劣化
1. `dbms-admin-metrics` で `lock-wait-events`, `lock-conflicts`, `deadlock-detected` を確認
2. 競合テーブルを特定し、長時間 tx を終了
3. 必要に応じて短時間の書込み制限を行い、再計測
4. 低下継続時は `ANALYZE` / `VACUUM` の適用可否を判断

### 5.3 S3: 権限・監査
1. 監査ログから実行主体/対象オブジェクトを特定
2. `GRANT` / `REVOKE` の誤設定を修正
3. 再発防止としてロール境界を見直し

## 6. 定常運用
- 日次:
  - `dbms-admin-backup-create` を実行
  - 必要に応じて `dbms-admin-backup-prune <keep-count> <keep-days>` を実行
  - 既定ポリシーは `DBMS_BACKUP_KEEP_COUNT` / `DBMS_BACKUP_KEEP_DAYS` で管理
  - 世代一覧を確認
  - 監査ログローテーション状態を確認
- 週次:
  - PITR 演習を実行（本書 7章）
  - メトリクス傾向を確認し閾値を見直し

## 7. 演習手順（Drill）
- 演習テスト:
  - `examples/dbms/tests/prod/t220-runbook-drill.lsp`
- 実行:
  - `./bin/isl examples/dbms/tests/prod/t220-runbook-drill.lsp`
  - `./bin/islc-run examples/dbms/tests/prod/t220-runbook-drill.lsp`
- 合格条件:
  - バックアップ作成→PITR復旧→主要データ検証が通過する
  - メトリクス取得・監査確認が通過する

## 8. エスカレーション基準
- 15分以内に `S1` 復旧不能
- 同一障害が 24 時間で 2 回以上再発
- 監査ログ欠損または改ざん疑い
