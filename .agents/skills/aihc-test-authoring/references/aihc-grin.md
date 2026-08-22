# aihc-grin Tests

Use `aihc-grin` tests for the active GRIN text property.

## Active Test

The suite checks this property:

```text
parseProgram . renderProgram = id
```

Add a property when the GRIN syntax requirement is broad or algebraic.

## Disabled Tests

GRIN lowering tests need an FC2 lowerer.
GRIN interpreter tests need FC2-generated programs.
The shared evaluation fixtures need an FC2 evaluator.

Do not enable these tests until their FC2 dependencies exist.

## Validation

Run:

```bash
cabal test -v0 aihc-grin:spec --test-options="--hide-successes"
```
