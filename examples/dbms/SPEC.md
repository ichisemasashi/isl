# DBMS Fixed Specification (MVP)

## 1. Scope
This document fixes the MVP specification for an ISL implementation of a small relational DBMS under `examples/dbms`.

Included SQL statements:
- `CREATE TABLE`
- `INSERT`
- `SELECT`
- `UPDATE`
- `DELETE`

Out of scope in MVP:
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
For MVP (`CREATE/INSERT/SELECT/UPDATE/DELETE`), no interpreter/compiler extension is required.

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

## 12. Non-Goals / Deferred
Deferred to next phase:
- multi-table query
- transaction/lock
- index other than primary-key hash assist
- SQL escaping and full lexer compliance

