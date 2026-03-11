# DBMS 固定仕様

## 1. スコープ
本ドキュメントは、`examples/dbms` 配下に実装する ISL 製の小規模リレーショナル DBMS の仕様を固定します。
本仕様は以下の 2 プロファイルを定義します。
- `MVP Core`（最小SQLエンジン）
- `Wiki Compatibility v1`（`examples/wiki` の PostgreSQL 利用を代替するための互換プロファイル）

`MVP Core` に含める SQL 文:
- `CREATE TABLE`
- `INSERT`
- `SELECT`
- `UPDATE`
- `DELETE`

`MVP Core` での対象外:
- `ALTER TABLE`, `DROP TABLE`
- `JOIN`、サブクエリ、集約（`COUNT`, `SUM`, ...）
- トランザクション制御（`BEGIN`, `COMMIT`, `ROLLBACK`）
- 複数ライタの同時実行サポート

## 2. 言語・ランタイム方針
- `examples/dbms` 配下の DBMS コードは **ISL のみ** で実装する。
- DBMS コアロジックに外部 DB（SQLite/Postgres/MySQL）を使わない。
- 既存 ISL 機能で MVP 実装は可能:
  - ファイルI/O（`with-open-file`, `read`, `format`）
  - 文字列処理（`string-index`, `substring`, `string-append`）
  - メモリ構造（list/vector/hash table）

### 2.1 実装可能性判定
`MVP Core` と `Wiki Compatibility v1` の両方について、仕様策定時点では interpreter/compiler 拡張は不要と判断します。
実装は既存 ISL 機能（Lispコードによるパーサ/実行器、ファイル永続化、時刻 API）を使用します。

### 2.2 拡張ルール
実装時に、既存 ISL 機能だけでは実現不可能と判明した場合は、以下を **両方** 拡張すること。
- interpreter 経路（`./bin/isl`）
- compiler/runtime 経路（`./bin/islc-run`）

拡張時の必須条件:
- 両経路で外部挙動が同一であること
- 追加プリミティブごとの conformance テストを追加すること
- 本仕様へ厳密な意味定義を追記すること

## 3. ディレクトリ/モジュール契約
目標構成:
- `examples/dbms/app/main.lsp` : CLI エントリポイント
- `examples/dbms/app/repr.lsp` : 共通データ表現（AST/スキーマ/行/結果/エラー）
- `examples/dbms/app/sql_lexer.lsp` : トークナイザ
- `examples/dbms/app/sql_parser.lsp` : パーサ（AST生成）
- `examples/dbms/app/engine.lsp` : 文実行器
- `examples/dbms/app/catalog.lsp` : スキーマメタデータ管理
- `examples/dbms/app/storage.lsp` : 永続化層
- `examples/dbms/tests/` : 単体/結合テスト
- `examples/dbms/storage/` : 永続データ

## 4. データモデル

### 4.1 データベース
- ストレージルートごとに単一データベース。
- MVP ではデータベース名を `default` 固定とする。

### 4.2 テーブル
テーブルは次で構成される:
- テーブル名
- 順序付きカラム定義
- 行集合（物理順は不定、論理順はクエリで決定）

### 4.3 カラム型
MVP でサポートする型は 3 種のみ:
- `INT`
- `TEXT`
- `BOOL`

暗黙キャストは次のみ許可:
- `TRUE/FALSE` リテラル -> `BOOL`
- 整数リテラル -> `INT`
- クォート文字列 `'...'` -> `TEXT`

### 4.4 制約
MVP の制約:
- `PRIMARY KEY`（単一カラムのみ）
- `NOT NULL`

制約の挙動:
- `PRIMARY KEY` は `NOT NULL` を内包する
- `INSERT`/`UPDATE` で主キー重複はエラー

## 5. SQL 文法（MVP）
キーワードは大文字小文字を区別しない。識別子は大文字小文字を区別する。

### 5.1 CREATE TABLE
形式:
`CREATE TABLE <table> (<col-def> [, <col-def> ...]);`

`<col-def>`:
`<col-name> <type> [PRIMARY KEY] [NOT NULL]`

制約:
- `PRIMARY KEY` は 0 個または 1 個のみ
- カラム名重複はエラー

### 5.2 INSERT
形式A:
`INSERT INTO <table> VALUES (<expr> [, <expr> ...]);`

形式B:
`INSERT INTO <table> (<col> [, <col> ...]) VALUES (<expr> [, <expr> ...]);`

制約:
- MVP では 1 文につき 1 行のみ
- カラム数と値数の不一致はエラー

### 5.3 SELECT
形式:
`SELECT <select-list> FROM <table> [WHERE <predicate>] [ORDER BY <col> [ASC|DESC]];`

`<select-list>`:
- `*`
- `<col> [, <col> ...]`

`<predicate>`（MVP）:
- `<col> = <expr>`
- `<col> != <expr>`
- `<col> < <expr>`
- `<col> <= <expr>`
- `<col> > <expr>`
- `<col> >= <expr>`

論理演算子（`AND`, `OR`）は MVP では対象外。

### 5.4 UPDATE
形式:
`UPDATE <table> SET <col>=<expr> [, <col>=<expr> ...] [WHERE <predicate>];`

- `WHERE` 省略時は全行対象
- 制約検証は対象行への代入後に行う

### 5.5 DELETE
形式:
`DELETE FROM <table> [WHERE <predicate>];`

- `WHERE` 省略時は全行削除

## 6. 式と比較の意味論
サポートするリテラル:
- `INT`: 10進整数（`-? [0-9]+`）
- `TEXT`: 単一引用符文字列（MVP ではエスケープ非対応）
- `BOOL`: `TRUE` / `FALSE`
- `NULL`: `NULL`

比較規則:
- 型不一致比較はエラー
- `NULL` を含む比較は MVP では常に偽
- `=` と `!=` は同一型同士のみ

## 7. 結果契約

### 7.1 SELECT の返却
エンジンは以下を返す:
- 順序付きカラム一覧
- 行リスト（行を表す vector/list の list）

CLI は人間可読の表形式で表示し、テストは構造化値で検証する。

### 7.2 更新系文の返却
`INSERT/UPDATE/DELETE` は影響行数（整数）を返す。
`CREATE TABLE` はシンボル `ok` を返す。

## 8. エラー契約
SQL 実行エラーは、安定したエラーコード + メッセージで通知すること。

最小エラーコード:
- `dbms/parse-error`
- `dbms/table-not-found`
- `dbms/column-not-found`
- `dbms/duplicate-table`
- `dbms/duplicate-column`
- `dbms/type-mismatch`
- `dbms/not-null-violation`
- `dbms/primary-key-violation`
- `dbms/arity-mismatch`

## 9. 永続化契約

### 9.1 ファイル
`examples/dbms/storage/` 配下:
- `catalog.lspdata` : テーブルスキーマ
- `table_<name>.lspdata` : 行データ

### 9.2 エンコーディング
- `read` で直接ロード可能な ISL の S式データ形式を使う。

### 9.3 耐久性
各書き込み操作で以下を行う:
1. 一時ファイルへ全内容を書き込む
2. 目標ファイルへ原子的に置換する

現行プラットフォームで pure ISL による原子的 rename が使えない場合は、警告付きのフォールバックとテストを定義する。

## 10. 順序と決定性
- `ORDER BY` なしでは挿入順を返す。
- `ORDER BY` ありでは対象カラムで安定ソートする。
- 同値キーの相対順は維持する。

## 11. テスト受け入れ基準（本固定仕様）
最低限、以下を含める:
- 各 SQL 文の成功ケース
- 各エラーコードのケースを少なくとも1つ
- 永続化再読込テスト（`CREATE/INSERT` 後に再起動相当）
- `INSERT` と `UPDATE` での主キー一意性検証
- `INT/TEXT/BOOL` に対する `WHERE` 比較

合格条件:
- `examples/dbms/tests` 配下テストが `./bin/isl ...` で全通
- 本仕様と矛盾する挙動がない

## 12. 非目標 / 後続対応（`MVP Core`）
`MVP Core` では後続対応とする項目:
- 複数テーブルクエリ
- トランザクション/ロック
- 主キー補助以外のインデックス
- SQL エスケープの完全対応と完全準拠レキサ

## 13. `examples/wiki`（PostgreSQL）との差分確認
現行 `MVP Core` は、wiki の DB 利用をそのまま置き換えるには **不十分**。

wiki のスキーマ・クエリから確認できた不足機能:
- 複数テーブルクエリ（`JOIN`）
- サブクエリ（`SELECT ...`, `NOT EXISTS`, select-list 内のスカラサブクエリ）
- 複合キーソート（`ORDER BY a DESC, b ASC`）
- 行数制限（`LIMIT 1`）
- 論理述語（`OR`）
- 関数的式（`LOWER`, `POSITION`, `COALESCE`, `MAX`, `TO_CHAR`）
- CTE（`WITH ...`）
- `INSERT ... SELECT`
- `UPDATE ... FROM ... RETURNING`
- DDL/制約の拡張（`IF NOT EXISTS`, `UNIQUE`, 複合一意, `CHECK`, `FOREIGN KEY ... ON DELETE ...`）
- マイグレーション構文（`ALTER TABLE ... DROP/ADD CONSTRAINT`）
- 衝突時制御（`ON CONFLICT ... DO NOTHING`）
- wiki スキーマ型（`BIGINT`, `INTEGER`, `TIMESTAMPTZ`, 自動採番 id）

このため、以下の `Wiki Compatibility v1` を本仕様に追加する。

## 14. Wiki Compatibility v1（必須追加仕様）
`examples/wiki` の DB 経路を置換するために必須。

### 14.1 SQL 文
必須サポート:
- `CREATE TABLE [IF NOT EXISTS]`
- `CREATE INDEX [IF NOT EXISTS]`（実行は no-op 可。ただし parse と成功応答は必須）
- `ALTER TABLE <table> DROP CONSTRAINT IF EXISTS <name>`
- `ALTER TABLE <table> ADD CONSTRAINT <name> CHECK (...)`
- `INSERT INTO ... VALUES ...`
- `INSERT INTO ... SELECT ...`
- `INSERT ... ON CONFLICT (<col>) DO NOTHING`
- `SELECT ... FROM ...`
- `UPDATE ... SET ... [FROM ...] [WHERE ...] [RETURNING ...]`
- `DELETE FROM ... [WHERE ...]`
- `WITH <name> AS (<subquery>) [, ...] <final-statement>`

### 14.2 クエリ機能
`SELECT/UPDATE/DELETE` で必須:
- `INNER JOIN ... ON ...`
- `WHERE` での `AND` と `OR`
- 複数キー `ORDER BY`（`ASC|DESC`）
- `LIMIT <int>`
- 式位置でのスカラサブクエリ
- `EXISTS` / `NOT EXISTS`

### 14.3 式・組み込み
必須演算子:
- 比較: `= != < <= > >=`
- 論理: `AND OR NOT`
- null 判定: `IS NULL`, `IS NOT NULL`

必須 SQL 関数（関数名は大文字小文字非依存）:
- `LOWER(text) -> text`
- `POSITION(substr IN text) -> int`（1始まり。未検出は 0）
- `COALESCE(a, b[, ...])`
- `MAX(expr)`（集約。非 GROUP 利用を必須）
- `TO_CHAR(timestamp, format)`（最低限 `'YYYY-MM-DD HH24:MI:SS'` をサポート）

### 14.4 型（Wikiプロファイル）
必須論理型:
- `BIGINT`
- `INTEGER`
- `TEXT`
- `BOOL`
- `TIMESTAMPTZ`

型エイリアス方針:
- DDL の `BIGSERIAL` は `BIGINT` + 自動採番デフォルトとして扱う。

### 14.5 DDL 制約
必須制約:
- `PRIMARY KEY`（本プロファイルでは単一カラム）
- `NOT NULL`
- `UNIQUE`（単一/複合）
- `CHECK (<predicate>)`
- `FOREIGN KEY (<col>) REFERENCES <table>(<col>) ON DELETE CASCADE|SET NULL`

制約検証タイミング:
- 各更新系文の永続化コミット前に検証する。

### 14.6 タイムスタンプ自動更新
手続きSQLトリガを必須にしないため、次を仕様化する:
- `TIMESTAMPTZ DEFAULT now()` をサポートする。
- `pages` テーブル更新時、`updated_at` カラムが存在し、かつ文中で明示代入されない場合は、現在時刻を自動設定する。

### 14.7 マイグレーション用トランザクション
`BEGIN` と `COMMIT` と `ROLLBACK` を parse/実行可能にする。
単一プロセス v1 エンジンでは以下実装を許容:
- 文単位のベストエフォート原子性（immediate mode）
- `BEGIN/COMMIT` は明示バッチ境界として扱う（同時実行分離は保証しない）

### 14.7.1 トランザクション状態管理（P1-002）
エンジンはセッション内 tx 状態を保持する。

- 形式: `(dbms-tx-state <status> <read-set> <write-set> <tx-id>)`
- `<status>`: `idle | active | aborted | committed`
- `<read-set>/<write-set>`: 文字列テーブル名の集合（重複なし）
- `<tx-id>`: 非負整数（`idle` は `0`）

状態遷移:
- 初期状態は `idle`
- `BEGIN` 成功時に `active` へ遷移し、`read-set/write-set` を空集合で開始する
- `active` 中に成功した文の対象テーブルを `read-set`/`write-set` に反映する
- `COMMIT` 後は `idle` に戻す
- `ROLLBACK` 成功時は `BEGIN` 時点のスナップショットへ復元し、`idle` へ戻す

注記:
- 本節は状態追跡の導入を目的とし、隔離性や rollback 保証は後続マイルストーンで強化する。

### 14.7.2 Commit staging（P1-004）
`active` トランザクション中の更新系文は、永続ストレージへ即時反映せず staging 領域へ保持する。

- staging 対象:
  - catalog 変更
  - テーブル行変更（INSERT/UPDATE/DELETE、FK 連鎖更新を含む）
- `COMMIT` 成功時のみ staging 内容を永続ストレージへ反映する
- `ROLLBACK` 時は staging 内容を破棄する

可視性:
- トランザクション外の committed read（storage 直読）は、`COMMIT` 前の未確定変更を観測してはならない

### 14.7.3 tx エラーコード契約（P1-005）
トランザクション境界文は次の契約コードを返す。

- `dbms/tx-already-active`:
  - `active` 状態で `BEGIN` を実行した場合
- `dbms/tx-not-active`:
  - `idle` 状態で `COMMIT` を実行した場合
  - `idle` 状態で `ROLLBACK` を実行した場合

これらは `dbms-result` の `error` payload に格納し、実行経路（`isl`/`islc-run`）で同一挙動とする。

### 14.7.4 WAL record 仕様（P1-006）
本節は `P1-007`（WAL append/flush 実装）と `P1-008`（recovery 実装）に先行して、
永続ログ形式を固定する。

#### 14.7.4.1 目的
- トランザクション変更を append-only ログとして記録する。
- `COMMIT` の永続性判定を commit record で一意に行う。
- クラッシュ時の再生単位（redo/undo 境界）を固定する。

#### 14.7.4.2 物理配置（v1）
- ストレージルート直下に WAL ファイルを置く。
  - `wal.log`（単一ファイル運用）
- `P1-006` 時点では単一ファイル固定。セグメント分割は後続拡張とする。

#### 14.7.4.3 レコード表現（S式）
1 レコードは次の S 式で永続化する:

`(dbms-wal-record <lsn> <txid> <kind> <op> <payload>)`

- `<lsn>`: 単調増加整数（1 以上）
- `<txid>`: 非負整数（tx 非依存イベントは `0`）
- `<kind>`: `begin | data | commit | abort | checkpoint`
- `<op>`: `symbol`（例: `catalog-replace`, `table-replace`）
- `<payload>`: 操作再生に必要な S 式データ

commit marker:
- `kind=commit` のレコードを commit marker と定義する。
- `kind=commit` の `op` は `tx-commit` 固定とする。

例:
- `(dbms-wal-record 101 7 begin tx-begin ())`
- `(dbms-wal-record 102 7 data table-replace ("pages" <table-state>))`
- `(dbms-wal-record 103 7 data catalog-replace (<catalog>))`
- `(dbms-wal-record 104 7 commit tx-commit ())`

#### 14.7.4.4 順序保証（write-ahead 規約）
- 同一 tx 内では `lsn` 昇順で `begin -> data* -> (commit|abort)` を記録する。
- `data` 対応の実データファイル反映は、対応 WAL record が永続化された後にのみ許可する。
- `COMMIT` 成功応答条件:
  - 当該 tx の `commit` record が `wal.log` に永続化済みであること。

#### 14.7.4.5 妥当性制約
- `lsn` は重複してはならない。
- `txid>0` の `commit` は、同一 `txid` の `begin` を先行必須とする。
- `commit` 後に同一 `txid` の `data` を書いてはならない。

違反時は実装が記録を拒否し、`dbms/result error` を返すこと。

#### 14.7.4.6 破損・不正時の扱い（recovery 前提契約）
`wal.log` 読み取り時に以下を検出した場合:
- S 式として読めない
- `dbms-wal-record` 形でない
- 必須フィールド不足または型不正
- `lsn` 非単調

挙動:
- 最初の不正レコード位置以降は無効 tail として切り捨てる。
- `commit` marker が無い tx は未コミット扱いとする。
- 不正検出イベントはエラーとして報告可能にしつつ、
  「有効先頭部分のみ」を使った復旧続行を許可する。

#### 14.7.4.7 実装境界
- `P1-006` は「形式と意味の固定」のみを対象とする。
- 実際の append/flush 実装は `P1-007`、起動時 replay は `P1-008` で実施する。

### 14.7.5 WAL append/flush（P1-007）
`active` tx の `COMMIT` 処理では、以下順序を必須とする。

1. `begin` record を append
2. `data` record 群（`catalog-replace`, `table-replace`）を append
3. `commit` record（`tx-commit`）を append
4. その後に実データファイル（catalog/table）へ反映

`COMMIT` 成功応答は、少なくとも 1〜3 の WAL 永続化成功を前提条件とする。

失敗時:
- WAL append に失敗した場合、データファイル反映を行わず `error` を返す。
- 反映フェーズ失敗時は `error` を返す（recovery は `P1-008` 範囲）。

### 14.7.6 起動時 recovery（P1-008）
起動時に `wal.log` を走査し、以下を実施する。

1. `begin` と `commit(tx-commit)` が揃っている `txid` を committed と判定
2. committed tx の `data` record を `lsn` 順に redo 適用
3. `commit` の無い tx は未コミットとして不適用（取り消し）

redo 対象 op:
- `catalog-replace`: catalog 全体を置換
- `table-replace`: table state 全体を置換

起動完了後:
- 可視データは committed tx のみを反映した状態であること
- 次 txid は WAL の最大 txid より大きい値に進めること

### 14.7.7 durability 試験（P1-009）
クラッシュ耐性は kill/restart を含む反復試験で検証する。

試験契約:
- 反復回数は 10 回以上
- 各反復で `COMMIT` 中（WAL commit 後、data file 反映前）に強制 kill を注入
- 再起動時 recovery 後、committed 変更のみが可視であること
- 不整合件数 0 件で合格

実装ではテスト専用 failpoint（環境変数）を利用して
「WAL 永続化済みだが data file 未反映」のクラッシュ窓を決定的に再現してよい。

### 14.7.8 table lock manager（P2-001）
最小同時実行制御として table lock を導入する。

lock mode:
- `S`（shared）: `SELECT`
- `X`（exclusive）: `INSERT/UPDATE/DELETE/DDL`

互換性:
- `S` と `S` は両立可
- `X` は他 tx の `S/X` と両立不可
- 同一 tx の `S -> X` は、当該 table の holder が自 tx のみの場合に限り upgrade 可

挙動:
- tx 実行中の statement 先頭で必要 lock を取得する
- 競合時は待機せず即時 `dbms/lock-conflict` を返す
- `COMMIT` / `ROLLBACK` で当該 tx の lock を解放する

debug API:
- lock table の snapshot を取得できる API を提供する

### 14.7.9 READ COMMITTED（P2-002）
`P2-001` の lock manager を前提に、最小分離レベル `READ COMMITTED` を提供する。

規約:
- `SELECT` により取得した `S` lock は statement 終了時に解放する
- `INSERT/UPDATE/DELETE/DDL` の `X` lock は tx 終了（`COMMIT/ROLLBACK`）まで保持する
- 他 tx が `X` lock を保持中の table への `SELECT` は `dbms/lock-conflict` で失敗させる

これにより dirty read（未コミット更新の読取り）を防止する。

### 14.7.10 SERIALIZABLE（P2-003, 保守的）
保守的実装として、`READ COMMITTED` より強い lock 保持を行う。

規約:
- tx 分離レベル `SERIALIZABLE` では、`SELECT` による `S` lock を tx 終了まで保持する
- lock 競合時は待機せず、競合した tx を優先的に abort する（abort-first）
- abort された tx の lock / staging / snapshot は即時破棄する

分離レベル API:
- session 分離レベルを `READ-COMMITTED` / `SERIALIZABLE` で切替可能
- `BEGIN` 時に current session level を tx level として固定する

### 14.7.11 デッドロック検知（P2-004）
lock 競合時に wait-for graph を構築し、cycle を検知する。

規約:
- lock 競合で待機関係 `requester -> holder(s)` を記録する
- 新規待機辺追加後に cycle が成立する場合は deadlock と判定する
- deadlock 検出時は `dbms/deadlock-detected` を返し、victim tx を abort する

victim ポリシー（現行実装）:
- 競合を発生させた request 側 tx を victim として abort-first

完了条件:
- deadlock シナリオで有限時間内に少なくとも 1 tx が abort されること

### 14.7.11a row lock 導入（MT-001, table lock から段階移行）
`P2-001` の table lock を維持したまま、更新競合判定を row lock へ段階移行する。

規約:
- tx 実行中の `UPDATE` / `DELETE` は table `S` lock を取得した上で、実際に変更対象となる行ごとに row `X` lock を取得する
- 同一 table の異なる row-key への `X` lock は両立可
- 同一 row-key への `X` lock は競合し、`dbms/lock-conflict` もしくは `dbms/deadlock-detected` を返す
- row lock 競合でも wait-for graph を共有し、cycle 検出は table lock と同一規約で動作する
- row lock の解放は tx 境界（`COMMIT` / `ROLLBACK` / abort）で実施する

row-key:
- 初期実装では単一 primary key を row-key として使う
- PK が無い/複合PKの table は row lock 対象外として table lock のみで保護する

運用ロールバック導線:
- 環境変数 `DBMS_ENABLE_ROW_LOCKS=0` で SQL 実行経路の row lock 取得を無効化できる
- 未設定または `1/true/on` の場合は有効

### 14.7.11b MVCC ベース読み取り分離（MT-002）
`READ COMMITTED` で read/write 干渉を減らすため、MVCC 読み取り規約を追加する。

有効化:
- 環境変数 `DBMS_ENABLE_MVCC=1|true|on` で有効
- 未設定/その他値は無効（従来 lock 主体経路）

可視性規約（READ COMMITTED + MVCC 有効時）:
- `SELECT/EXPLAIN` は table `S` lock を取得しない（read lock elision）
- statement 開始時に read snapshot を再評価する（statement snapshot）
- ただし、同一 tx が既に書き込んだ table は tx 内 staged state を優先し、own-write を保持する
- 未コミット他txの変更は可視化しない

write conflict:
- `UPDATE/DELETE` の競合判定は `MT-001` の row `X` lock を利用する
- 同一 row-key の同時更新は `dbms/lock-conflict` または `dbms/deadlock-detected`

SERIALIZABLE:
- 本段階では既存の lock 保持規約を維持する（保守的実装）

### 14.7.12 B+Tree ページフォーマット（P3-001）
index 実体実装（P3-002）に先行して、ページ形式を固定する。

#### 14.7.12.1 定数
- page size: `4096` bytes
- header size: `64` bytes
- leaf entry size（見積り）: `24` bytes
- internal entry size（見積り）: `16` bytes

#### 14.7.12.2 ヘッダ形式
S式表現:
- `(dbms-btree-page-header <page-id> <lsn> <checksum> <free-start> <free-end> <flags>)`

制約:
- `page-id > 0`
- `lsn >= 0`
- `checksum >= 0`
- `0 <= free-start <= free-end <= page-size`
- `flags` は list

#### 14.7.12.3 ページ形式
S式表現:
- `(dbms-btree-page <header> <kind> <entries> <leftmost-child> <right-sibling>)`

`kind`:
- `leaf`
- `internal`

entry:
- leaf: `(dbms-btree-leaf-entry <key> <rid>)`
- internal: `(dbms-btree-internal-entry <key> <child-page-id>)`

制約:
- leaf page:
  - `leftmost-child` は `NIL`
  - `entries` は leaf-entry のみ
  - `right-sibling` は `NIL` または page-id
- internal page:
  - `leftmost-child` は page-id 必須
  - `entries` は internal-entry のみ
  - `right-sibling` は `NIL` または page-id

#### 14.7.12.4 split/merge 規約（固定）
- split:
  - overflow 時、中央値で `left/right` へ分割
  - leaf split は右ページ最小キーを親へ昇格
  - internal split は中央値キーを親へ昇格し、中央値キー自体は子に残さない
- merge/redistribute:
  - underflow 時、隣接 sibling から再分配を優先
  - 再分配不可時に merge
  - root が空に縮退した場合は子を新 root とする

#### 14.7.12.5 テスト可能性契約
- 表現コンストラクタとバリデータを提供し、以下を自動テスト可能にする:
  - header/page の妥当性判定
  - leaf/internal entry 型判定
  - capacity 見積り（正値）

### 14.7.13 optimizer 強化（MT-003, JOIN/複合条件）
`P3-006/P3-007` の最小 optimizer を拡張し、`select-wiki`（JOIN/複合条件）でも
コスト比較ベースの計画選択情報を返せるようにする。

規約:
- `EXPLAIN SELECT ...` で inner statement が `select-wiki` の場合も成功し、`("item","value")` 形式で計画詳細を返す
- `from` 句の table refs を基に、以下を算出する:
  - `naive-order`（記述順）
  - `optimized-order`（推定行数昇順）
  - `chosen-order`（`join-cost` 比較で採用）
- 推定行数は `ANALYZE` 統計の `row-count` を優先し、無い場合は実行時 row 件数を利用する
- `where` の複合度を `where-complexity` として返し、`AND/OR/NOT/IN/IS` と比較演算子を反映する
- 複合 `ORDER BY` は `order-by-items` として可視化する

EXPLAIN 出力（select-wiki）必須キー:
- `query-kind`
- `tables`
- `join-count`
- `naive-order`
- `optimized-order`
- `chosen-order`
- `join-cost-naive`
- `join-cost-optimized`
- `where-complexity`
- `order-by-items`
- `limit`
- `optimizer-version`

### 14.8 互換・リライト境界
許容:
- PostgreSQL 固有機能回避のための、小規模な wiki 側 SQL リライト

不許容:
- page list/search/edit/new/media ルートの挙動劣化
- スキーマ整合性の弱化（slug 一意性、revision 一意性、media filename 一意性、FK の delete 挙動）

### 14.9 Wiki 管理画面のバックアップ/リストア互換
現行 wiki 管理ルートは PostgreSQL ツール（`pg_dump`, `psql`）を呼び出す。
DBMS 置換時は等価ワークフローを提供する:
- DB 状態を単一ファイルへダンプ（S式または SQL 互換形式）
- そのダンプファイルからリストア
- wiki テーブル（`pages`, `page_revisions`, `media_assets`）で決定的ラウンドトリップを保証

実装メモ:
- DBMS ネイティブ utility/API として提供し、wiki 側が `pg_dump/psql` から切替可能であること。

## 15. 更新後の非目標
`Wiki Compatibility v1` 後も後続対応とする:
- PostgreSQL の完全互換
- ユーザー定義 SQL 関数/手続き言語
- 汎用トリガエンジン
- 高度なプランナ/コストベース最適化

## 16. 実装マイルストーン分割（必須）
本DBMS実装は、以下 2 マイルストーンに分割して進めること。

1. `M1: MVP Core`
2. `M2: Wiki Compatibility v1`

着手順序は固定で、`M1` 完了前に `M2` の機能実装へ着手しない。

### 16.1 M1 (`MVP Core`) 完了条件
以下をすべて満たした時点で `M1` 完了とする:
- `CREATE TABLE/INSERT/SELECT/UPDATE/DELETE` が本仕様 5 章の範囲で動作する
- 8章の最小エラーコードを返す
- 9章の永続化契約（`catalog.lspdata`, `table_<name>.lspdata`）を満たす
- 11章の受け入れ基準を満たすテストが `examples/dbms/tests/mvp_core/` で全通する

### 16.2 M2 (`Wiki Compatibility v1`) 受け入れ条件
`M1` 完了後に、14章の要件を満たした時点で `M2` 完了とする。
検証は `examples/dbms/tests/wiki_compat_v1/` で行う。

### 16.3 実装中のプロファイル管理
実行時の機能ゲートは `examples/dbms/app/profile.lsp` で管理する。
- デフォルトの有効プロファイルは `MVP Core`
- `Wiki Compatibility v1` の機能は、`M1` 完了後に段階的に有効化する

### 16.4 ISL機能拡張の扱い
実装中に ISL だけでは不可能な機能が見つかった場合は 2.2 の拡張ルールに従う。
拡張が必要になった時点で、本節に対象機能と適用マイルストーン（`M1` か `M2`）を追記すること。

## 17. 共通データ表現（内部表現の固定）
`examples/dbms/app/repr.lsp` を単一の定義元とし、以下の内部表現を固定する。

### 17.1 AST
- ルート: `(dbms-ast <statements> <meta>)`
- 文: `(dbms-stmt <kind> <payload>)`

### 17.2 テーブル定義
- カラム定義: `(dbms-column <name> <type> <attrs>)`
- テーブル定義: `(dbms-table-def <name> <columns> <constraints> <options>)`
- 制約定義: `(dbms-constraint <kind> <name> <spec>)`
  - `<kind>`: `primary-key | unique | check | foreign-key`
  - `primary-key` の `<spec>`: `(<col-name>)`
  - `unique` の `<spec>`: `(<col-name> ...)`
  - `check` の `<spec>`: チェック式の内部表現（非空）
  - `foreign-key` の `<spec>`: `(<local-cols> <ref-table> <ref-cols> <on-delete>)`
    - `<local-cols>` / `<ref-cols>` は列名リスト
    - `<on-delete>` は `CASCADE` または `SET-NULL`
- カタログ: `(dbms-catalog <repr-version> <table-pairs>)`
  - `<table-pairs>` は `(<table-name> <table-def>)` のリスト

### 17.3 行データ
- 行: `(dbms-row <row-id> <values>)`
  - `<values>` は `(<column-name> <value>)` のリスト
- テーブル状態: `(dbms-table-state <table-name> <rows> <next-row-id>)`

### 17.4 実行結果
- 汎用結果: `(dbms-result <kind> <payload>)`
- `SELECT` 結果: `(dbms-result rows (<columns> <rows>))`
- 更新件数結果: `(dbms-result count <n>)`
- 正常結果: `(dbms-result ok <payload>)`
- エラー結果: `(dbms-result error <dbms-error>)`

### 17.5 エラー
- エラー本体: `(dbms-error <code> <message> <detail>)`
- `code` は 8章で定義した `dbms/*` を利用し、実装上の補助コード（例: `dbms/not-implemented`, `dbms/no-input`, `dbms/invalid-representation`）の追加を許可する。

### 17.6 層間契約
- `sql_parser.lsp` は AST か `dbms-error` を返す。
- `catalog.lsp` と `storage.lsp` は 17.2, 17.3 の形式を返す。
- `engine.lsp` は常に `dbms-result` を返す。
- `main.lsp` は `dbms-result` をそのまま出力し、CLI整形は後続ステップで追加する。

## 18. P4-005 認証・ロール・権限
最小運用要件として users/roles/grants のメタを永続化し、statement 実行前に object-level 権限検証を行う。

### 18.1 管理SQL
- `CREATE USER <name>`
- `CREATE ROLE <name>`
- `GRANT ROLE <role> TO <user>`
- `REVOKE ROLE <role> FROM <user>`
- `GRANT <priv-list> ON [TABLE] <object> TO <grantee>`
- `REVOKE <priv-list> ON [TABLE] <object> FROM <grantee>`
- `SET ROLE <role>`

`<priv-list>`: `SELECT/INSERT/UPDATE/DELETE/CREATE/ALTER/DROP/INDEX/ANALYZE/VACUUM/ALL`

### 18.2 永続メタ
- `auth.lspdata`: `(dbms-auth-meta <users> <roles> <memberships> <grants>)`
  - `<memberships>`: `(<user> <role>)` のリスト
  - `<grants>`: `(<grantee> <object> <privilege>)` のリスト
- 初期ブートストラップ時に `admin` ユーザ/ロールと `("admin" "*" ALL)` を必ず存在させる。

### 18.3 権限判定
- statement 実行前に required privilege を決定し、現在ユーザ（+ 付与済みロール）に対して照合する。
- 未許可時は `dbms/permission-denied` を返し、実行を拒否する。

### 18.4 監査（最小）
- 監査レコード形式: `(dbms-audit-record <ts> <user> <action> <object> <result> <detail>)`
- 最低限、以下を記録する:
  - 権限拒否（`result=DENY`）
  - DDL 実行成功（`action=DDL`）
  - 権限変更実行成功（`action=PRIVILEGE`）
  - tx abort（`action=TX-ABORT`, `result=ABORT`）
- 現行ログは `audit.lspdata` に保持する。
- ローテーション:
  - 環境変数 `DBMS_AUDIT_MAX_ENTRIES` を上限として現行ログ件数を制限する。
  - 上限超過分は `audit.archive.<seq>.lspdata` へ退避する。

## 19. P4-007 バックアップ世代管理 + PITR
運用復旧を目的に、base backup と WAL archive による世代管理と時点復旧（PITR）を提供する。

### 19.1 永続形式
- バックアップディレクトリ: `<storage-root>/backups/`
- 世代インデックス: `backups/index.lspdata`
  - `(dbms-backup-index 1 <generations>)`
- 世代エントリ:
  - `(dbms-backup-generation <id> <created-at> <base-lsn> <max-lsn> <dump-path> <wal-path>)`
- 世代実体:
  - `backups/backup.<id>.dump.lspdata`
  - `backups/backup.<id>.wal.lspdata`

### 19.2 API 契約
- `dbms-admin-backup-create`
  - 現在の catalog/table 状態を base dump として保存し、同時点までの WAL を archive する。
  - 成功時に生成された `dbms-backup-generation` を返す。
- `dbms-admin-backup-list`
  - 既存世代一覧（`<generations>`）を返す。
- `dbms-admin-restore-pitr <generation-id> <target-lsn>`
  - 対象 generation の dump を restore 後、archive WAL を `target-lsn` まで再生する。
  - `target-lsn` は `[base-lsn, max-lsn]` の範囲でなければならない。
  - 復旧完了後、`wal.log` は `target-lsn` までで切り詰める（再起動後の再適用で復旧点が進まないこと）。

### 19.3 受け入れ基準
- 世代作成後に追加変更を行い、`restore-pitr` で任意の `target-lsn` へ戻せる。
- 復旧後データが `target-lsn` 時点の committed 状態と一致する。

## 20. P4-008 メトリクス公開
運用監視向けに DBMS 内部メトリクスを取得可能にする。

### 20.1 公開API
- `dbms-admin-metrics`
  - `("metric" "value")` 形式の rows 結果を返す。
- `dbms-admin-metrics-reset`
  - メトリクスカウンタを初期化する（テスト/検証用途）。

### 20.2 必須メトリクス
- `tx-per-sec`
- `lock-wait-events`
- `cache-hit-ratio`
- `recovery-lag-records`

加えて運用解析のために、以下の生カウンタも返してよい:
- `tx-committed`, `tx-aborted`
- `lock-conflicts`, `deadlock-detected`
- `cache-hit`, `cache-miss`

### 20.3 計測点
- tx:
  - `COMMIT` 成功で committed を加算
  - `ROLLBACK` / conflict abort / commit failure で aborted を加算
- lock:
  - lock 競合発生時に wait/conflict を加算
  - deadlock 検出時に deadlock を加算
- cache:
  - catalog/table の read cache で hit/miss を加算
- recovery:
  - 起動 recovery で適用した data record 件数を `recovery-lag-records` として保持

## 21. P4-009 Runbook/障害手順書
担当交代時でも復旧可能にするため、運用 Runbook を固定する。

### 21.1 ドキュメント
- `examples/dbms/RUNBOOK.md` を運用手順の正本とする。
- 最低限、以下を含める:
  - 障害分類（可用性/性能/権限）
  - 一次切り分け手順
  - 復旧手順（backup generation + PITR を含む）
  - RTO/RPO 想定値

### 21.2 演習可能性契約
- Runbook の内容は `tests/prod` の drill で再現可能であること。
- `examples/dbms/tests/prod/t220-runbook-drill.lsp` を標準 drill とし、以下を検証する:
  - メトリクス取得（`tx/sec`, `lock wait`, `cache hit`, `recovery lag`）
  - バックアップ作成と `restore-pitr` による復元
  - 監査ログによる tx abort 確認

### 21.3 受け入れ基準
- Runbook のみを参照して drill テストが成功する。

## 22. ST-001 並行実行テストハーネス
同時実行回帰の再現性を上げるため、固定ステップ実行のハーネスを提供する。

### 22.1 ハーネス方針
- `examples/dbms/tests/prod/helpers/concurrency_harness.lsp` を標準ハーネスとする。
- 仮想セッションを txid で表現し、`lock acquire/release` をステップ実行する。
- 失敗時は tx timeline（ステップ番号、操作、期待値/実測）を必ず出力する。

### 22.2 固定シナリオ
- dirty-read guard
- lost-update guard
- deadlock detection

### 22.3 受け入れ基準
- `examples/dbms/tests/prod/t230-concurrency-harness.lsp` が両経路で安定再現する。
- 失敗時に timeline だけで原因切り分け可能な情報が出る。

## 23. ST-002 WAL/復旧の運用安全化（checkpoint 基礎）
recovery の運用安全性向上のため、checkpoint メタを導入する。

### 23.1 永続形式
- `checkpoint.lspdata`:
  - `(dbms-checkpoint <version> <last-lsn> <created-at> <max-txid>)`
- `<last-lsn>` は checkpoint 時点で整合確定済みの最大 LSN
- `<max-txid>` は txid 単調増加維持に利用する

### 23.2 API
- `dbms-admin-checkpoint-create`
  - active tx 中は拒否
  - checkpoint メタを作成し返す
- `dbms-admin-checkpoint-info`
  - 現在の checkpoint メタを返す

### 23.3 recovery 規約
- 起動 recovery は `checkpoint.last-lsn` より後の WAL を対象にする。
- recovery report は最低限以下を含む:
  - `checkpoint-lsn`
  - `wal-total-record-count`
  - `wal-scanned-record-count`
  - `applied-data-record-count`

### 23.4 受け入れ基準
- checkpoint 作成後の recovery で scan 開始位置が checkpoint 基準になる。
- txid は checkpoint 後も再利用されない（単調増加）。

## 24. ST-003 PITR 運用強化（世代保持ポリシー）
バックアップ世代の増加を運用可能範囲に抑えるため、保持ポリシーと prune API を導入する。

### 24.1 保持ポリシー
- 環境変数:
  - `DBMS_BACKUP_KEEP_COUNT`（非負整数、`0` は無制限）
  - `DBMS_BACKUP_KEEP_DAYS`（非負整数日、`0` は日数制限なし）
- `dbms-admin-backup-create` 成功後、保持ポリシーを自動適用して古い世代を prune する。

### 24.2 管理API
- `dbms-admin-backup-prune <keep-count> <keep-days>`
  - 明示的に prune を実行する。
  - 返却: `(dbms-backup-prune-report ...)`
- prune 対象は index と実体ファイル（dump/wal）の両方から削除する。

### 24.3 受け入れ基準
- keep-count 超過時に最古世代から削除される。
- keep-days 超過時に期限切れ世代が削除される。
- 削除後に index とファイル実体が不一致にならない。

## 25. LT-001 レプリケーション（非同期→同期）
単一ノード障害時の可用性向上のため、WAL shipping + follower apply + manual failover を提供する。

### 25.1 永続メタ
- `replication.lspdata`:
  - `(dbms-repl-meta <version> <mode> <followers>)`
- follower:
  - `(dbms-repl-follower <id> <root> <last-shipped-lsn> <last-applied-lsn> <sync-state>)`
- `<mode>`:
  - `"ASYNC"` or `"SYNC"`

### 25.2 管理API
- `dbms-admin-replication-set-mode <"ASYNC"|"SYNC">`
- `dbms-admin-replication-status`
- `dbms-admin-replication-register-follower <id> <root>`
  - follower root へ bootstrap dump を適用し、初回 WAL ship/apply を実行する
- `dbms-admin-replication-ship`
  - primary WAL/checkpoint を follower root へ配送する
- `dbms-admin-replication-apply-follower <id>`
  - follower root で WAL recovery を実行し追従させる
- `dbms-admin-replication-failover <id>`
  - manual failover として active storage root を follower root へ切替える

### 25.3 複製規約
- shipping は `wal.log` と `checkpoint.lspdata` を follower root へ配送する
- apply は follower 側 `recover-from-wal` を利用し committed tx のみ反映する
- ASYNC:
  - ship 完了後に即時 ACK を返してよい（apply は後続）
- SYNC:
  - `sync-state` を `SYNC-PENDING-APPLY -> SYNC-ACK` で管理し、運用上 apply 完了を確認する

### 25.4 Failover 規約
- 手動 failover は次を実施:
  1. 対象 follower へ最終 ship
  2. follower apply
  3. active root を follower root へ切替
- 切替後は follower root を primary として起動し直す

### 25.5 受け入れ基準
- 同一 WAL stream に対して primary/follower の最終可視データが一致する
- 両経路（`isl` / `islc-run`）で同一 replication 状態遷移になる

## 26. LT-002 自動フェイルオーバー
障害切替の人手依存を減らすため、health check + leader election + lease/fence による split-brain 防止を提供する。

### 26.1 永続メタ
- `failover.lspdata`:
  - `(dbms-failover-meta <version> <enabled> <ttl-sec> <last-heartbeat-at> <lease-owner> <lease-until> <fence-epoch>)`
- `<enabled>`:
  - `t` で自動切替有効、`nil` で無効
- `<lease-owner>`:
  - 通常 `"primary"`、昇格後は follower id
- `<fence-epoch>`:
  - 昇格ごとに単調増加し、運用上の fencing 番号として扱う

### 26.2 管理API
- `dbms-admin-replication-auto-failover-enable <ttl-sec>`
- `dbms-admin-replication-auto-failover-disable`
- `dbms-admin-replication-heartbeat`
  - primary の健全性を更新し lease を延長する
- `dbms-admin-replication-auto-failover-status`
  - failover/repl の複合状態を返す
- `dbms-admin-replication-auto-failover-tick`
  - 1回分の health 判定と昇格判定を実行
  - primary 不健全かつ lease 期限切れ時に候補 follower を自動昇格する

### 26.3 Health/Election 規約
- health 判定:
  - `now - last-heartbeat-at <= ttl-sec` かつ `now <= lease-until` を健全とみなす
- election:
  - follower の `last-applied-lsn` が最大の候補を選ぶ
  - 同値時は follower id の辞書順最小を採用する（決定性確保）

### 26.4 Split-brain 防止規約
- 自動昇格は lease 期限切れ時のみ実行可能
- 昇格時に `lease-owner` を follower id へ更新し、`fence-epoch` を増分
- `lease-owner != "primary"` の間は追加の自動昇格を抑止する

### 26.5 受け入れ基準
- primary 障害注入時に自動昇格が成立し、読取整合が維持される
- split-brain（重複昇格）が発生しない
- 両経路（`isl` / `islc-run`）で同一昇格結果になる

## 27. LT-003 ストレージ保守の高度化
長期運用での断片化・統計劣化を抑えるため、自動保守（autovacuum / auto analyze / compaction scheduler）を提供する。

### 27.1 永続メタ
- `maintenance.lspdata`:
  - `(dbms-maint-meta <version> <enabled> <vacuum-frag-threshold> <analyze-interval-sec> <vacuum-interval-sec> <corruption-check-interval-sec> <last-corruption-check-at> <max-vacuum-tables-per-tick> <table-states>)`
- table-state:
  - `(dbms-maint-table <table-name> <last-analyze-at> <last-vacuum-at> <last-frag-ratio> <last-row-count>)`

### 27.2 管理API
- `dbms-admin-maintenance-configure <enabled> <vacuum-frag-threshold> <analyze-interval-sec> <vacuum-interval-sec> <corruption-check-interval-sec> <max-vacuum-tables-per-tick>`
- `dbms-admin-maintenance-status`
- `dbms-admin-maintenance-tick`

### 27.3 スケジューラ規約
- `tick` は次順序で実行する:
  1. 破損検知（interval 到達時）
  2. compaction 候補選定（`fragmentation-ratio` 降順）
  3. 上位 `max-vacuum-tables-per-tick` 件を vacuum
  4. analyze 必要判定テーブルを analyze
- analyze 必要判定:
  - 統計未作成、または `analyze-interval-sec` 超過、または行数変化が有意な場合。

### 27.4 破損検知時の方針
- チェック対象:
  - page checksum 不一致
  - header free-space と実slot free数の不一致
  - slot-capacity 不一致 / page-pair 不正
- 破損検知時:
  - `action=ERROR-DETECTED` を返す
  - 自動保守を自動無効化（`enabled=nil`）
  - 運用者は手動復旧（backup/restore, pitr, 再起動検証）へ移行する

### 27.5 受け入れ基準
- 手動介入なしで断片化/統計劣化を抑制できる。
- 破損検知時に保守が自動停止し、誤った自動修復を行わない。
- 両経路（`isl` / `islc-run`）で起動条件・結果が一致する。

## 28. LT-004 セキュリティ強化（認証・通信・監査耐改ざん）
認証情報の平文依存を避け、通信保護ポリシーと監査ログ改ざん検知を導入する。

### 28.1 Security メタ
- `security.lspdata`:
  - `(dbms-security-meta <version> <kdf-spec> <credentials> <tls-mode> <cert-path> <key-path> <ca-path> <audit-key>)`
- credential:
  - `(dbms-credential <user> <salt> <rounds> <digest>)`
- `<kdf-spec>`:
  - 現行は `"KDF-V1-32"` とし、salted iterative hash を用いる
- `<tls-mode>`:
  - `"DISABLED"` or `"REQUIRED"`

### 28.2 管理API
- `dbms-admin-security-set-password <user> <password>`
- `dbms-admin-security-configure-tls <mode> <cert-path> <key-path> <ca-path>`
- `dbms-admin-security-status`
- `dbms-admin-security-authenticate <user> <password> <transport> <tls-enabled>`
- `dbms-admin-audit-verify-integrity`

### 28.3 認証規約
- password は `security.lspdata` に credential として保存する
- digest は平文を直接保存せず、salt + iterative hash で生成する
- bootstrap admin は初期 credential を持つ。運用開始後に rotate を必須とする
- user が存在しても credential 未設定、または password 不一致時は `dbms/authentication-failed`

### 28.4 TLS ポリシー規約
- DBMS 自身は TLS 終端実装を持たず、ここでは「TLS 必須ポリシー」を管理する
- `tls-mode="REQUIRED"` の場合、`transport != "LOCAL"` かつ `tls-enabled=nil` の認証は `dbms/tls-required`
- 実運用では DBMS 前段の transport/webserver/proxy 側 TLS 終端と組み合わせる

### 28.5 監査耐改ざん規約
- audit record は chain hash 付きで保存する:
  - legacy: `(dbms-audit-record <ts> <user> <action> <object> <result> <detail>)`
  - signed: `(dbms-audit-record <ts> <user> <action> <object> <result> <detail> <prev-hash> <entry-hash>)`
- `entry-hash` は `audit-key + prev-hash + record-body` から導出する
- archive/current を跨いで chain を維持し、`dbms-admin-audit-verify-integrity` で検証する
- chain 不整合、hash 不一致、archive 境界不整合は `dbms/audit-tamper-detected`

### 28.6 受け入れ基準
- credential digest が平文ではなく、一致/不一致判定が安定する
- TLS required policy 下で非TLS remote auth が拒否される
- audit 改ざん時に integrity verify が失敗する
- 両経路（`isl` / `islc-run`）で認証・監査結果が一致する
