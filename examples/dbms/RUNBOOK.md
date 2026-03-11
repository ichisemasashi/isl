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

## 9. レプリケーション運用（LT-001）
単一ノード障害に備え、manual failover 前提で follower を追従させる。

### 9.1 初期セットアップ
1. primary 起動後に follower root を決定
2. `dbms-admin-replication-set-mode "ASYNC"` を設定
3. `dbms-admin-replication-register-follower <id> <root>` を実行
4. `dbms-admin-replication-status` で follower が登録されていることを確認

### 9.2 定常同期
- 非同期運用:
  - `dbms-admin-replication-ship`
  - `dbms-admin-replication-apply-follower <id>`（運用バッチ）
- 同期運用:
  - `dbms-admin-replication-set-mode "SYNC"`
  - ship 後に `sync-state=SYNC-PENDING-APPLY` を確認
  - apply 後に `sync-state=SYNC-ACK` を確認

### 9.3 手動フェイルオーバー
1. `dbms-admin-replication-failover <id>` を実行
2. active root が follower root へ切替わったことを確認
3. 代表 `SELECT` で整合を確認
4. 旧 primary は停止し、split-brain を回避する

### 9.4 演習
- `examples/dbms/tests/prod/t290-replication.lsp` を実行して
  - bootstrap
  - async ship/apply
  - sync state 遷移
  - manual failover
  を定期的に確認する

## 10. 自動フェイルオーバー運用（LT-002）
primary 障害時の切替を自動化する。通常は heartbeat を定期実行し、監視側で tick を回す。

### 10.1 有効化
1. `dbms-admin-replication-auto-failover-enable <ttl-sec>`
2. `dbms-admin-replication-heartbeat` を primary 側で周期実行（`ttl-sec/2` 目安）
3. `dbms-admin-replication-auto-failover-status` で `lease-owner="primary"` を確認

### 10.2 監視/昇格
1. 監視側で `dbms-admin-replication-auto-failover-tick` を周期実行
2. `PRIMARY_HEALTHY` は継続監視
3. heartbeat 欠落かつ lease 期限切れで `action=PROMOTE` を確認
4. `fence-epoch` の増分と active root 切替を確認

### 10.3 split-brain 回避
- 昇格後は `lease-owner != "primary"` になるため追加昇格は抑止される
- 旧 primary の再参加前に自動切替を `disable` し、運用者判断で再構成する

### 10.4 手動ロールバック
1. `dbms-admin-replication-auto-failover-disable`
2. LT-001 の manual failover 手順に戻す

### 10.5 演習
- `examples/dbms/tests/prod/t300-auto-failover.lsp` を実行して
  - health 判定
  - election 決定性
  - 自動昇格
  - split-brain 抑止
  を定期的に確認する

## 11. ストレージ自動保守運用（LT-003）
長期運用向けに autovacuum / auto analyze を定期実行し、破損検知時は自動停止して手動復旧へ切り替える。

### 11.1 有効化
1. `dbms-admin-maintenance-configure t <frag-threshold> <analyze-sec> <vacuum-sec> <corruption-check-sec> <max-vacuum-per-tick>`
2. ジョブ側で `dbms-admin-maintenance-tick` を周期実行
3. `dbms-admin-maintenance-status` で `enabled=t` と閾値設定を確認

### 11.2 定常監視
- `tick` 結果の `vacuumed-count` / `analyzed-count` を監視
- 断片化の高いテーブルが上位優先で処理されることを確認
- 処理件数が継続的に0かつ断片化が上昇する場合は閾値を見直す

### 11.3 破損検知時
- `action=ERROR-DETECTED` を受信したら自動保守は停止済み（`enabled=nil`）
- 直ちに以下を実施:
  1. 影響テーブルを隔離して更新停止
  2. backup/restore または PITR 手順へ移行
  3. 復旧後に整合確認クエリと回帰テストを実行

### 11.4 ロールバック
1. `dbms-admin-maintenance-configure nil ...` で自動保守を明示停止
2. `ANALYZE` / `VACUUM` を手動運用へ切替

### 11.5 演習
- `examples/dbms/tests/prod/t310-storage-maintenance.lsp` を実行して
  - scheduler 判定
  - 自動保守効果
  - 破損検知時の自動停止
  を定期的に確認する

## 12. セキュリティ運用（LT-004）
認証情報は credential hash で管理し、remote transport は TLS policy、監査ログは chain verify で保護する。

### 12.1 初期化
1. `dbms-admin-security-set-password "admin" "<rotated-password>"` で bootstrap admin password を即時変更
2. 必要ユーザ作成後に `dbms-admin-security-set-password <user> <password>` を設定
3. `dbms-admin-security-status` で `kdf-spec` と credential 数を確認

### 12.2 TLS ポリシー
1. `dbms-admin-security-configure-tls "REQUIRED" <cert> <key> <ca>`
2. remote transport 認証時は `dbms-admin-security-authenticate <user> <password> "TCP" t`
3. local maintenance/CLI は `transport="LOCAL"` を利用可能
4. 実TLS終端は前段 proxy / transport 実装で行う

### 12.3 監査整合確認
1. 定期ジョブで `dbms-admin-audit-verify-integrity`
2. 失敗時は改ざん疑いとして `S1` 扱い
3. `audit.lspdata` と `audit.archive.*.lspdata` を保全し、復旧前に上書きしない

### 12.4 インシデント対応
- `dbms/authentication-failed` 増加:
  - パスワードローテーション、credential 再設定、監査追跡
- `dbms/tls-required`:
  - 前段 transport の TLS 終端状態を確認
- `dbms/audit-tamper-detected`:
  - 監査ファイル隔離
  - integrity verify 失敗時点の backup/PITR 点を特定
  - 監査要件に従い保全・調査を優先

### 12.5 演習
- `examples/dbms/tests/prod/t320-security-hardening.lsp` を実行して
  - password hash
  - TLS required policy
  - audit tamper detection
  を定期確認する
