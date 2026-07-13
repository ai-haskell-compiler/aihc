# aihc-grin Tests

Use `aihc-grin` tests for FC-to-GRIN lowering, strict IR invariants, explicit
heap and control operations, and reference-interpreter behavior.

## Choose The Test

- Use shared evaluation fixtures under `test/Test/Fixtures/eval` when the
  contract is Haskell behavior preserved through the whole pipeline. Both FC
  and GRIN must run these fixtures.
- Use a focused GRIN unit test when the contract is structural, such as
  closure conversion, explicit `store`/`eval`/`apply`, lint rejection, heap
  update behavior, or a particular interpreter error.
- Add GRIN golden fixtures only when the exact rendered IR shape is itself the
  contract. Do not encode source-language behavior solely as a GRIN golden.

The shared harness parses, resolves, type-checks, and desugars each fixture once
per evaluator, then passes the resulting System FC program to the selected
runtime evaluator. A compile failure is classified using the fixture's normal
`pass`/`fail`/`xfail`/`xpass` status.

## Validation

Run:

```bash
cabal test -v0 aihc-grin:spec --test-options="--hide-successes"
```

When shared fixtures change, also run the FC suite so both semantic evaluators
remain aligned:

```bash
cabal test -v0 aihc-fc:spec --test-options="--hide-successes"
```
