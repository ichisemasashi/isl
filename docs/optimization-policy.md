# M10 Optimization Policy

This document defines staged optimization rules for compiler milestone M10.

## Goals

- Keep language semantics invariant while introducing optimization stages.
- Make optimization behavior reproducible and explicit.
- Keep high-risk optimizations opt-in.

## Stage Model

1. `none` (default)
- Backend flags: `-O0`
- Semantic guard flags:
  - `-fno-fast-math`
  - `-fno-vectorize`
  - `-fno-slp-vectorize`

2. `safe`
- Backend flags: `-O2`
- Same semantic guard flags as `none`.
- Intended as the default optimization stage for semantic regression checks.

3. `aggressive` (opt-in)
- Backend flags: `-O3`
- Same semantic guard flags plus:
  - `-funroll-loops`
- Intended only for optional performance experiments.

## CLI Mapping

- `bin/islc` and `bin/islc-aot` accept:
  - `--opt-level 0|1|2|3|s|z`
  - `--opt-preset none|safe|aggressive`
- `--opt-preset` maps to `--opt-level`:
  - `none -> 0`
  - `safe -> 2`
  - `aggressive -> 3`

## Whitelist Principle

The optimization whitelist in M10 is represented by the controlled stage presets above.
Only explicitly allowed flag sets are used in semantic gates.

Disallowed by default:
- fast-math family
- vectorization families that may alter observable behavior in edge cases

## Regression Procedure

1. Semantic parity:
- Run `test/compiler/m10-opt-parity-smoke.sh`
- Verify `-O0` and `-O2` outputs are identical on M10 semantic probes.

2. Stage regression:
- Run `test/compiler/m10-stage-regression.sh`
- Verify `none`, `safe`, `aggressive` all preserve baseline results on the probe set.

3. Performance report:
- Run `test/compiler/m10-opt-bench.sh`
- Compare median runtime for `none` vs `safe`.
