# aihc-fc Tests

Use `aihc-fc` tests for System FC desugaring, pretty-printed FC output, evaluator behavior, and focused FC unit tests.

## Choose The Test

- Use FC golden fixtures when parsed source should desugar to precise pretty-printed FC.
- Use FC eval fixtures when the behavior is runtime evaluation of an expression or module binding.
- Use unit tests in `components/aihc-fc/test/Test/Fc/Suite.hs` when the assertion is small and direct, such as evaluator primitives or helper functions.

## Golden Fixtures

Root: `components/aihc-fc/test/Test/Fixtures/golden`.

Fixture shape:

```yaml
extensions: []
modules:
  - |
    module Test where
    id x = x
expected: |
  <rendered FC program>
status: pass
reason: ""
```

Required keys: `extensions`, `modules`, `status`. `expected` may be a string or list of strings. The runner parses each module, desugars it, renders the FC program, trims both sides, and compares.

Statuses: `pass`, `fail`, `xfail`, `xpass`. Use `fail` when desugaring should fail. Use `xfail` only for a known gap and include `reason`.

Copy expected FC from the renderer when possible. The pretty-printer may use symbols or formatting that are hard to reproduce by hand; exact output is the contract.

## Eval Fixtures

Root: `components/aihc-fc/test/Test/Fixtures/eval`.

Fixture shape:

```yaml
extensions: []
dependencies: []
modules:
  - |
    module Test where
output: |
  : 'x' []
expression: |
  ['x']
status: pass
reason: renders evaluated list literals in raw constructor form
```

Required keys: `extensions`, `modules`, `expression`, `output`, `status`. `dependencies` is optional and defaults to `[]`, but include it explicitly when the fixture relies on shared library modules.

Use the optional `stdout` string when evaluation must write to standard output. The fixture runner captures file descriptor 1, flushes libc output, and requires an exact text match without trimming. Prefer a quoted YAML string when leading, trailing, or newline characters matter. The golden updater skips fixtures with `stdout` expectations so it cannot leak side effects into its own output; validate them with the FC test suite and maintain both expectations manually.

Use `dependencies: [aihc-base]` when an eval fixture depends on `Prelude` names or modules from `core-libs/aihc-base` instead of defining all dependencies inline in `modules`. The runner loads `aihc-base` from `core-libs/aihc-base` by default. Set `AIHC_BASE_SRC` only when local development, CI, or branch testing needs the fixture runner to use a modified or alternate `aihc-base` checkout; the value must be the package root whose `src` tree should replace the default `core-libs/aihc-base`. Dependency loading starts from `Prelude` plus the fixture modules' imports and follows imports transitively. Unknown dependency names fail the fixture.

Keep self-contained eval fixtures dependency-free: define helper functions and operators in `modules` when the behavior under test is local FC evaluation rather than integration with `aihc-base`.

The runner parses modules and expression, synthesizes an `__aihc_eval__` binding in the last fixture module, resolves dependency modules before fixture modules, desugars with synthetic type-checker output, evaluates that binding, and compares rendered raw value.

## Validation

Run:

```bash
cabal test -v0 aihc-fc:spec --test-options="--hide-successes"
```

To refresh expected `expected` or `output` fields from the current implementation, run the shared updater from the repository root:

```bash
cabal run -v0 aihc-dev -- update-goldens --dry-run
cabal run -v0 aihc-dev -- update-goldens
```

Always rerun the FC suite afterwards. For eval fixtures with `dependencies`, treat the FC suite as the authority because it loads dependency modules through the eval fixture runner. The updater may skip dependency-backed eval fixtures when its inline-module path cannot resolve `Prelude` or `aihc-base` names, or when parsing, resolving, desugaring, or evaluating the fixture fails while computing a replacement `output`. These skips are distinct from the `fail` and `xfail` status skips described in `validation.md`; keep the fixture unchanged and use the FC suite failure output to decide whether the expected value should change.
