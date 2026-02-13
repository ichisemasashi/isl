# M8 Numeric Semantics (strict)

This document fixes the compiler runtime numeric behavior for milestone M8.

## Scope

- Compiler runtime (`src/isl/compiler/runtime.scm`)
- AOT build flags (`bin/islc`, `bin/islc-aot`)

## Rules

1. Numeric value model
- Runtime numbers are delegated to Gauche number semantics.
- No implicit string/symbol coercion is allowed.

2. Arithmetic/comparison type boundary
- `+ - * / = < > <= >=` require numeric operands.
- Non-numeric operands raise `type-error`.
- `mod`, `oddp`, `evenp` require integer operands.
- `zerop`, `plusp`, `minusp`, `floor`, `ceiling`, `truncate`, `round` require numeric operands.

3. exact/inexact policy
- exact/inexact propagation follows host arithmetic behavior.
- Compiler runtime does not add additional coercion rules.

4. Error policy
- Arity errors are raised as `arity`.
- Type boundary violations are raised as `type-error`.
- M8 gate checks error occurrence (not exact message string) unless explicitly required.

5. LLVM optimization policy
- AOT compile defaults to `-O0`.
- `--opt-level` supports `0|1|2|3|s|z`.
- `-fno-fast-math` is always enabled by default to prevent numeric semantic drift.

## M8 Conformance Targets

- Boundary tests on zero/negative/large integer and predicate behavior.
- Type error boundary tests for mixed-type numeric operators.
- `-O0/-O2` observable result parity on AOT execution.
