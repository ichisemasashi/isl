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
`BEGIN` と `COMMIT` を parse/実行可能にする。
単一プロセス v1 エンジンでは以下実装を許容:
- 文単位のベストエフォート原子性（immediate mode）
- `BEGIN/COMMIT` は明示バッチ境界として扱う（同時実行分離は保証しない）

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
