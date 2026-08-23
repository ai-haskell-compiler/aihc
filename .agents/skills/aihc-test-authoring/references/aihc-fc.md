# aihc-fc Tests

Use `aihc-fc` tests for System FC 2 desugaring, text output, lint checks, and properties.

## Choose The Test

- Use FC2 golden fixtures when parsed source must desugar to exact FC2 text.
- Use FC2 text fixtures when FC2 parse and print operations must give the same text.
- Use FC2 lint fixtures when the linter must accept or reject an FC2 program.
- Use a property when the requirement is broad or algebraic.

## Golden Fixtures

Root: `components/aihc-fc/test/Test/Fixtures/golden-v2`.

Fixture shape:

```yaml
extensions: []
modules:
  - |
    module Test where
    id x = x
expected: |
  <rendered FC2 program>
status: pass
reason: ""
```

Required keys are `extensions`, `modules`, and `status`.
`expected` can be a string or a list of strings.

Statuses are `pass`, `fail`, `xfail`, and `xpass`.
Use `xfail` only for a known gap.
Always give a reason for `xfail`.

## Text And Lint Fixtures

Use `components/aihc-fc/test/Test/Fixtures/fc2` for parse and print tests.
Use `components/aihc-fc/test/Test/Fixtures/fc2-lint` for lint tests.

Put accepted lint fixtures in `pass`.
Put rejected lint fixtures in `fail`.
Use `mutual` for modules that need each other.

## Disabled Tests

The shared evaluation fixtures do not have an FC2 evaluator.
No test must use `test/Test/Fixtures/eval` until an FC2 evaluator exists.

## Validation

Run:

```bash
cabal test -v0 aihc-fc:spec --test-options="--hide-successes"
```
