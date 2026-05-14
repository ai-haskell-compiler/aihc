# aihc-tc Tests

Use `aihc-tc` tests for type inference, type-checker diagnostics, unification, zonking, constraints, and solver behavior.

## Choose The Test

- Use golden fixtures when the contract is the inferred binding types for one or more modules, or a known type-check failure.
- Use unit tests when the behavior is an internal operation such as unification, constraint solving, environment handling, or diagnostic construction.
- Use QuickCheck properties for algebraic type-checker invariants. Existing examples live in `components/aihc-tc/test/Test/Tc/Properties.hs`.

## Golden Fixtures

Root: `components/aihc-tc/test/Test/Fixtures/golden`.

Fixture shape:

```yaml
extensions: []
modules:
  - |
    module Test where
    id x = x
expected:
  - "id :: forall a. a -> a"
status: pass
reason: ""
```

Required keys: `extensions`, `modules`, `status`. `expected` may be a string, list of strings, or object keyed by module name. The runner parses each module, runs `typecheckModule`, and compares rendered binding types.

Statuses: `pass`, `fail`, `xfail`, `xpass`. Use `fail` when the expected behavior is rejection and rendered diagnostics are not the contract. Use `xfail` for known gaps; include a reason.

## Unit And Property Tests

Add tests under `components/aihc-tc/test/Test/Tc/Suite.hs` for focused examples. Add properties to `Test/Tc/Properties.hs` and keep them in the `properties` group.

Prefer properties for invariants such as idempotency, reflexivity, or stability under zonking/substitution. Prefer golden fixtures for user-visible inference examples.

## Validation

Run:

```bash
cabal test -v0 aihc-tc:spec --test-options="--hide-successes"
```
