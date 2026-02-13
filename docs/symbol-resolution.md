# M9 Symbol Resolution Policy (strict)

This document fixes package/symbol behavior for compiler milestone M9.

## Policy Choice

- Adopt **runtime (late) symbol resolution** for package-sensitive symbols.
- Rationale: preserve interpreter semantics under package mutations (`in-package`, `use-package`, `export`, `import`) that may occur between compiled forms.

## Rules

1. Symbol canonicalization
- Compiler runtime resolves symbols via package resolver before env operations.
- `env-ref`, `env-set!`, `env-define!` use canonical symbols.

2. Package special forms
- `defpackage` and `in-package` are kept as `special` nodes in normalized IR.
- Runtime executes them with the same package subsystem used by interpreter.

3. Package primitives in runtime
- Runtime provides: `current-package`, `find-package`, `use-package`, `export`, `intern`, `import`.
- Arity/type checks follow existing strict runtime error policy.

4. Visibility/collision behavior
- Export/import/use-package effects are reflected in subsequent symbol resolution.
- Unexported external access remains an error.

## Scope Note

- This milestone targets semantic consistency in compiler runtime path.
- AOT backend symbol linkage policy is unchanged in M9.
