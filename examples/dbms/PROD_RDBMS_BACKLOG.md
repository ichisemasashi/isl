# DBMS 本番化バックログ（現行実装ベース）

## 1. この文書の目的
本書は、現行 `examples/dbms` 実装を「本番RDBMS運用へ近づける」ための優先度付きバックログです。  
`PROD_RDBMS_ISSUES.md` を補完し、現在の到達点から次に着手すべき項目を **短期/中期/長期** で明確化します。

## 2. 現在地（2026-03時点）
- 既存実装で到達済み:
  - SQL実行基盤（MVP + Wiki互換）
  - tx/WAL/recovery 基盤
  - table lock + read committed + serializable（保守的）
  - deadlock 検知
  - B+Tree 基本、range scan、ANALYZE、最小 optimizer、EXPLAIN
  - page storage、vacuum、オンライン ADD COLUMN、制約検証分離
  - RBAC、監査ログ、backup generation + PITR、メトリクス、Runbook
- 直近の品質状態:
  - `examples/dbms/tests/run_all.sh` が `isl/islc-run` 両経路で全通

結論: 小規模単一ノード運用は可能。ただし一般的な本番RDBMS水準には未達。

## 3. 優先度ルール
- 短期: 事故率を下げる項目（壊れにくさ、復旧確実性）
- 中期: 負荷時の性能・並行性を伸ばす項目
- 長期: HA/分散/大規模運用に必要な項目

---

## 4. 短期バックログ（0〜6週間）

### ST-001: P2-005 並行実行テストハーネスの本格実装
- 目的: 同時実行バグの検出精度を上げる。
- 実装:
  - 2〜4セッションの実行ドライバ追加
  - dirty read / lost update / deadlock シナリオを固定化
  - 失敗時に tx timeline を出力
- 完了条件:
  - 競合ケースの再現性が高く、両経路で同一判定になる。

### ST-002: WAL/復旧の運用安全化（checkpoint 基礎）
- 目的: 再起動時の recovery 時間とリスクを抑える。
- 実装:
  - checkpoint メタ導入（最後に整合が確定した LSN）
  - recovery 開始位置を checkpoint 基準に短縮
  - checkpoint 生成/読み込み API 追加
- 完了条件:
  - WAL 全走査なしで復旧でき、復旧時間が短縮される。

### ST-003: PITR 運用強化（世代保持ポリシー）
- 目的: バックアップ運用を手順依存から仕組み依存へ寄せる。
- 実装:
  - 世代保持数/保持期間のポリシー設定
  - 古い世代の安全削除 API
  - Runbook の定期ドリル自動化スクリプト
- 完了条件:
  - 世代が無制限増加せず、定期ドリルが自動で回る。

### ST-004: メトリクスの外部公開形式固定
- 目的: ダッシュボード連携を安定化する。
- 実装:
  - `dbms-admin-metrics` のスキーマ固定（互換ポリシー）
  - 機械可読エクスポート（例: key-value/JSON 互換）
  - しきい値サンプル（警告条件）を文書化
- 完了条件:
  - 外部監視がメトリクスを継続取得できる。

---

## 5. 中期バックログ（1〜3か月）

### MT-001: row lock 導入（table lock からの段階移行）
- 目的: 高競合時のスループット改善。
- 実装:
  - lock 粒度を table + row の二層へ
  - lock manager の互換性行列拡張
  - deadlock 検知の wait-for graph を row 対応
- 完了条件:
  - 競合しない行更新が並列に進む。

### MT-002: MVCC ベースの読み取り分離
- 目的: 読み取り性能と分離保証の両立。
- 実装:
  - version chain / visibility ルール導入
  - snapshot read と write conflict 判定
  - 現行 SERIALIZABLE 実装との整合設計
- 完了条件:
  - read/write 干渉が減り、代表ワークロードの待機が低下する。

### MT-003: optimizer 強化（JOIN/複合条件）
- 目的: Wiki以外の実運用クエリに耐える計画選択。
- 実装:
  - join order 探索の改善
  - 統計（NDV/null fraction）の利用範囲拡大
  - explain に選択理由を明示
- 完了条件:
  - 主要クエリで seq scan 固定を回避できる。

### MT-004: DDL 運用性強化（バックフィル管理）
- 目的: 大テーブル変更時の停止を最小化。
- 実装:
  - ADD COLUMN の進捗管理
  - バックフィル速度制御と中断再開
  - 制約 VALIDATE の並行実行制御
- 完了条件:
  - DDL 実行中も SLA を維持できる。

---

## 6. 長期バックログ（3か月〜）

### LT-001: レプリケーション（非同期→同期）
- 目的: 障害時の可用性向上。
- 実装:
  - WAL shipping/subscribe
  - follower 遅延監視
  - 昇格手順（manual failover）
- 完了条件:
  - primary 障害時に定義した RTO/RPO 内で復旧可能。

### LT-002: 自動フェイルオーバー
- 目的: 人手依存を減らす。
- 実装:
  - health check + leader election
  - split-brain 回避策
  - フェイルバック手順
- 完了条件:
  - 障害演習で自動切替が成功する。

### LT-003: ストレージ保守の高度化
- 目的: 長期運用時の性能劣化を抑える。
- 実装:
  - autovacuum/auto analyze
  - ページ破損検知と修復戦略
  - compaction スケジューラ
- 完了条件:
  - 運用負荷を増やさず性能劣化を抑制できる。

### LT-004: セキュリティ強化
- 目的: 本番監査要件に対応。
- 実装:
  - 認証情報ハッシュ方針の強化
  - 通信保護（TLS）方針
  - 監査ログ改ざん検知
- 完了条件:
  - 監査・セキュリティレビューで指摘ゼロを目指せる。

---

## 7. 実行順（推奨）
1. ST-001
2. ST-002
3. ST-003
4. ST-004
5. MT-001
6. MT-002
7. MT-003
8. MT-004
9. LT-001
10. LT-002
11. LT-003
12. LT-004

## 8. 運用ルール（このバックログの使い方）
- 各項目を着手時に Issue 化し、以下を必ず付与する:
  - 仕様差分（`SPEC.md`）
  - テスト差分（`examples/dbms/tests/`）
  - 両経路同一確認（`isl` / `islc-run`）
- 受け入れ前に `examples/dbms/tests/run_all.sh` を必須実行する。

