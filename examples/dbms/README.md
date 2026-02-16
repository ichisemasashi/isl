# dbms サンプル

このディレクトリには、ISL で実装する小規模 RDBMS を配置します。

- 固定仕様: `examples/dbms/SPEC.md`
- 最小SQLスコープ: `CREATE TABLE`, `INSERT`, `SELECT`, `UPDATE`, `DELETE`
- 実装順序: `MVP Core` 完了後に `Wiki Compatibility v1`
- プロファイル管理: `examples/dbms/app/profile.lsp`
- 共通データ表現: `examples/dbms/app/repr.lsp`

追加機能を実装する前に、まず固定仕様に準拠してください。
