# DBMS Fixed Specification (MVP)

## 1. Scope
This document fixes the specification for an ISL implementation of a small relational DBMS under `examples/dbms`.
It defines two profiles:
- `MVP Core` (minimum SQL engine)
- `Wiki Compatibility v1` (replacement target for `examples/wiki` PostgreSQL usage)

`MVP Core` included SQL statements:
- `CREATE TABLE`
- `INSERT`
- `SELECT`
- `UPDATE`
- `DELETE`

Out of scope in `MVP Core`:
- `ALTER TABLE`, `DROP TABLE`
- `JOIN`, subquery, aggregation (`COUNT`, `SUM`, ...)
- transaction control (`BEGIN`, `COMMIT`, `ROLLBACK`)
- concurrent writer support

## 2. Language and Runtime Policy
- Implementation language is **ISL only** for DBMS code under `examples/dbms`.
- External DB (SQLite/Postgres/MySQL) must not be used for DBMS core logic.
- Existing ISL features are sufficient for MVP:
  - file IO (`with-open-file`, `read`, `format`)
  - string processing (`string-index`, `substring`, `string-append`)
  - in-memory structures (list/vector/hash table)

### 2.1 Capability Verdict
For both `MVP Core` and `Wiki Compatibility v1`, no interpreter/compiler extension is required at spec time.
Implementation uses existing ISL facilities (parser/executor in Lisp code, file persistence, time API).

### 2.2 Extension Rule
If implementation later proves impossible only with current ISL features, extend both:
- interpreter path (`./bin/isl` runtime)
- compiler/runtime path (`./bin/islc-run` runtime)

Any such extension must include:
- same external behavior in both paths
- conformance tests for each new primitive
- this spec update with exact semantic definition

## 3. Directory/Module Contract
Target structure:
- `examples/dbms/app/main.lsp` : CLI entry
- `examples/dbms/app/sql_lexer.lsp` : tokenizer
- `examples/dbms/app/sql_parser.lsp` : parser -> AST
- `examples/dbms/app/engine.lsp` : statement execution
- `examples/dbms/app/catalog.lsp` : schema metadata
- `examples/dbms/app/storage.lsp` : file persistence
- `examples/dbms/tests/` : unit/integration tests
- `examples/dbms/storage/` : persisted data files

## 4. Data Model

### 4.1 Database
- Single database per storage root.
- Database name is fixed to `default` in MVP.

### 4.2 Table
Each table consists of:
- table name
- ordered columns
- rows (unordered physically; logical order defined by query)

### 4.3 Column Types
MVP supports 3 types only:
- `INT`
- `TEXT`
- `BOOL`

No implicit cast except:
- literal `TRUE/FALSE` -> `BOOL`
- integer literal -> `INT`
- quoted literal `'...'` -> `TEXT`

### 4.4 Constraints
MVP constraints:
- `PRIMARY KEY` (single column only)
- `NOT NULL`

Constraint behavior:
- `PRIMARY KEY` implies `NOT NULL`
- duplicate primary key on `INSERT`/`UPDATE` is error

## 5. SQL Grammar (MVP)
Case-insensitive keywords. Identifiers are case-sensitive.

### 5.1 CREATE TABLE
Form:
`CREATE TABLE <table> (<col-def> [, <col-def> ...]);`

`<col-def>`:
`<col-name> <type> [PRIMARY KEY] [NOT NULL]`

Restrictions:
- exactly zero or one `PRIMARY KEY` column allowed
- duplicate column names are error

### 5.2 INSERT
Form A:
`INSERT INTO <table> VALUES (<expr> [, <expr> ...]);`

Form B:
`INSERT INTO <table> (<col> [, <col> ...]) VALUES (<expr> [, <expr> ...]);`

Restrictions:
- exactly one row per statement in MVP
- column/value count mismatch is error

### 5.3 SELECT
Form:
`SELECT <select-list> FROM <table> [WHERE <predicate>] [ORDER BY <col> [ASC|DESC]];`

`<select-list>`:
- `*`
- `<col> [, <col> ...]`

`<predicate>` (MVP):
- `<col> = <expr>`
- `<col> != <expr>`
- `<col> < <expr>`
- `<col> <= <expr>`
- `<col> > <expr>`
- `<col> >= <expr>`

Logical operators (`AND`, `OR`) are out of scope for MVP.

### 5.4 UPDATE
Form:
`UPDATE <table> SET <col>=<expr> [, <col>=<expr> ...] [WHERE <predicate>];`

- no `WHERE` means all rows
- constraints validated after assignment per target row

### 5.5 DELETE
Form:
`DELETE FROM <table> [WHERE <predicate>];`

- no `WHERE` means all rows

## 6. Expression and Comparison Semantics
Supported literals:
- `INT`: decimal integer (`-? [0-9]+`)
- `TEXT`: single-quoted string (no escape support in MVP)
- `BOOL`: `TRUE` / `FALSE`
- `NULL`: `NULL`

Comparison rules:
- type mismatch in comparison is error
- comparison with `NULL` always false in MVP
- `=` and `!=` on same-typed values only

## 7. Result Contract

### 7.1 SELECT output
Engine returns:
- ordered column list
- row list (`list` of row vectors/lists)

CLI prints tabular text (human readable); tests validate structured value.

### 7.2 Mutating statements
`INSERT/UPDATE/DELETE` returns affected row count as integer.
`CREATE TABLE` returns symbol `ok`.

## 8. Error Contract
All SQL execution errors must be signaled with stable error code + message.

Minimum error codes:
- `dbms/parse-error`
- `dbms/table-not-found`
- `dbms/column-not-found`
- `dbms/duplicate-table`
- `dbms/duplicate-column`
- `dbms/type-mismatch`
- `dbms/not-null-violation`
- `dbms/primary-key-violation`
- `dbms/arity-mismatch`

## 9. Persistence Contract

### 9.1 Files
Under `examples/dbms/storage/`:
- `catalog.lspdata` : table schemas
- `table_<name>.lspdata` : row data

### 9.2 Encoding
- Use ISL-readable s-expression data so `read` can load directly.

### 9.3 Durability
For each write operation:
1. write full content to temporary file
2. atomically replace target file

If atomic rename is unavailable in pure ISL on current platform, define fallback with explicit warning and test.

## 10. Ordering and Determinism
- Without `ORDER BY`, row order is insertion order.
- With `ORDER BY`, stable sort on target column.
- Equal keys keep previous relative order.

## 11. Test Acceptance Criteria (for this fixed spec)
At minimum, tests must include:
- each statement success path
- each error code at least one case
- persistence reload test (`CREATE/INSERT` then process restart equivalent)
- primary key uniqueness on `INSERT` and `UPDATE`
- `WHERE` comparisons for INT/TEXT/BOOL

Pass criteria:
- all tests under `examples/dbms/tests` pass via `./bin/isl ...`
- no behavior contradicts this specification

## 12. Non-Goals / Deferred (`MVP Core`)
Deferred in `MVP Core`:
- multi-table query
- transaction/lock
- index other than primary-key hash assist
- SQL escaping and full lexer compliance

## 13. Gap Check Against `examples/wiki` (PostgreSQL)
Current `MVP Core` is **insufficient** as a drop-in replacement for wiki DB usage.

Missing capabilities observed from wiki schema and queries:
- multi-table query (`JOIN`)
- subquery (`SELECT ...`, `NOT EXISTS`, scalar subquery in select-list)
- multi-key sort (`ORDER BY a DESC, b ASC`)
- row limit (`LIMIT 1`)
- logical predicates (`OR`)
- function-like expressions (`LOWER`, `POSITION`, `COALESCE`, `MAX`, `TO_CHAR`)
- CTE (`WITH ...`)
- `INSERT ... SELECT`
- `UPDATE ... FROM ... RETURNING`
- richer DDL/constraints (`IF NOT EXISTS`, `UNIQUE`, composite unique, `CHECK`, `FOREIGN KEY ... ON DELETE ...`)
- migration-related syntax (`ALTER TABLE ... DROP/ADD CONSTRAINT`)
- conflict handling (`ON CONFLICT ... DO NOTHING`)
- wiki schema types (`BIGINT`, `INTEGER`, `TIMESTAMPTZ`, auto-increment id)

Therefore, this spec adds `Wiki Compatibility v1` below.

## 14. Wiki Compatibility v1 (Required Additions)
This profile is required to replace PostgreSQL for `examples/wiki` data paths.

### 14.1 SQL Statements
Required statement support:
- `CREATE TABLE [IF NOT EXISTS]`
- `CREATE INDEX [IF NOT EXISTS]` (execution may be no-op, but must parse and succeed)
- `ALTER TABLE <table> DROP CONSTRAINT IF EXISTS <name>`
- `ALTER TABLE <table> ADD CONSTRAINT <name> CHECK (...)`
- `INSERT INTO ... VALUES ...`
- `INSERT INTO ... SELECT ...`
- `INSERT ... ON CONFLICT (<col>) DO NOTHING`
- `SELECT ... FROM ...`
- `UPDATE ... SET ... [FROM ...] [WHERE ...] [RETURNING ...]`
- `DELETE FROM ... [WHERE ...]`
- `WITH <name> AS (<subquery>) [, ...] <final-statement>`

### 14.2 Query Features
Required in `SELECT/UPDATE/DELETE`:
- `INNER JOIN ... ON ...`
- `WHERE` with `AND` and `OR`
- `ORDER BY` multiple keys with `ASC|DESC`
- `LIMIT <int>`
- scalar subquery in expression position
- `EXISTS` / `NOT EXISTS`

### 14.3 Expressions and Built-ins
Required expression operators:
- comparison: `= != < <= > >=`
- boolean: `AND OR NOT`
- null checks: `IS NULL`, `IS NOT NULL`

Required built-in SQL functions (exact names, case-insensitive):
- `LOWER(text) -> text`
- `POSITION(substr IN text) -> int` (1-based, 0 when not found)
- `COALESCE(a, b[, ...])`
- `MAX(expr)` (aggregate; group-free usage required)
- `TO_CHAR(timestamp, format)` with minimum format `'YYYY-MM-DD HH24:MI:SS'`

### 14.4 Types (Wiki profile)
Required logical types:
- `BIGINT`
- `INTEGER`
- `TEXT`
- `BOOL`
- `TIMESTAMPTZ`

Type alias policy:
- `BIGSERIAL` accepted in DDL as `BIGINT` + auto-increment default.

### 14.5 DDL Constraints
Required constraints:
- `PRIMARY KEY` (single-column in this profile)
- `NOT NULL`
- `UNIQUE` (single and composite)
- `CHECK (<predicate>)`
- `FOREIGN KEY (<col>) REFERENCES <table>(<col>) ON DELETE CASCADE|SET NULL`

Constraint enforcement timing:
- on each mutating statement before persistence commit.

### 14.6 Timestamp/Auto-Update Semantics
To replace wiki trigger usage without requiring procedural SQL:
- `TIMESTAMPTZ DEFAULT now()` must be supported.
- For table `pages`, when row is updated and column `updated_at` exists, engine must auto-set it to current timestamp if statement does not explicitly assign it.

### 14.7 Transactions for Migration Scripts
`BEGIN` and `COMMIT` must parse and execute.
For v1 single-process engine, transaction behavior may be implemented as:
- immediate mode with best-effort atomicity per statement, and
- `BEGIN/COMMIT` treated as explicit batch boundary (no concurrent isolation guarantee).

### 14.8 Compatibility/Rewrite Boundary
Allowed: small SQL rewrites in wiki app to avoid PostgreSQL-only procedural features.
Not allowed:
- behavior regression in wiki routes using page list/search/edit/new/media.
- schema-level integrity weakening (slug uniqueness, revision uniqueness, media filename uniqueness, FK delete behavior).

### 14.9 Wiki Admin Backup/Restore Compatibility
Wiki currently invokes PostgreSQL tools (`pg_dump`, `psql`) in admin routes.
For DBMS replacement, provide equivalent workflow:
- dump current DB state to a single file (s-expression or SQL-like format)
- restore from that dump file
- deterministic round-trip for wiki tables (`pages`, `page_revisions`, `media_assets`)

Implementation note:
- This may be exposed as DBMS-native utility/API and wiki app may switch from `pg_dump/psql` to that utility.

## 15. Updated Non-Goals
Still deferred after `Wiki Compatibility v1`:
- full PostgreSQL compatibility
- user-defined SQL functions/procedural language
- general trigger engine
- advanced planner/cost-based optimization
