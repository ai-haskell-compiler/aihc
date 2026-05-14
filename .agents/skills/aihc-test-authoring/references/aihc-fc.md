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

Required keys: `extensions`, `modules`, `expression`, `output`, `status`. The fixture must define at least one module. The runner parses modules and expression, synthesizes an `__aihc_eval__` binding, desugars with synthetic type-checker output, evaluates that binding, and compares rendered raw value.

## Validation

Run:

```bash
cabal test -v0 aihc-fc:spec --test-options="--hide-successes"
```
