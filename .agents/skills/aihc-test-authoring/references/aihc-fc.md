# aihc-fc Tests

Use `aihc-fc` tests for System FC desugaring, text output, lint checks, and properties.

## Choose The Test

- Use FC golden fixtures when parsed source must desugar to exact FC text.
- Use FC text fixtures when FC parse and print operations must give the same text.
- Use FC lint fixtures when the linter must accept or reject an FC program.
- Use a property when the requirement is broad or algebraic.

## Golden Fixtures

Root: `bin/aihc/compiler/fc/test/Test/Fixtures/golden`.

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

Required keys are `extensions`, `modules`, and `status`.
`expected` can be a string or a list of strings.

Statuses are `pass`, `fail`, `xfail`, and `xpass`.
Use `xfail` only for a known gap.
Always give a reason for `xfail`.

## Text And Lint Fixtures

Use `bin/aihc/compiler/fc/test/Test/Fixtures/fc` for parse and print tests.
Use `bin/aihc/compiler/fc/test/Test/Fixtures/fc-lint` for lint tests.

Put accepted lint fixtures in `pass`.
Put rejected lint fixtures in `fail`.
Use `mutual` for modules that need each other.

## Disabled Tests

The shared evaluation fixtures do not have an FC evaluator.
No test must use `test/Test/Fixtures/eval` until an FC evaluator exists.

## Validation

Run:

```bash
cabal test -v0 aihc:fc-spec --test-options="--hide-successes"
```
