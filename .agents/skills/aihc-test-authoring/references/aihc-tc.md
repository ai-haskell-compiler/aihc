# aihc-tc Tests

Use `aihc-tc` tests for type inference, type-checker diagnostics, AST annotations, unification, zonking, constraints, and solver behavior.

## Choose The Test

- Use annotated golden fixtures when the user-visible contract is the type-checker overlay for one or more modules: inferred binding types, expression annotations, kind annotations, evidence annotations, or rendered type diagnostics.
- Use unit tests when the behavior is an internal operation or precise API invariant such as unification, constraint solving, environment handling, diagnostic construction, annotation structure, or a focused expression/module result.
- Use QuickCheck properties for algebraic type-checker invariants. Existing examples live in `components/aihc-tc/test/Test/Tc/Properties.hs`.

Prefer annotated golden fixtures for source-level behavior because they exercise parse, resolve, type-check, and rendering in the same path used by downstream FC tests. Prefer unit tests for assertions that need direct access to `TcType`, `TcDiagnostic`, evidence terms, or helper APIs.

## Annotated Golden Fixtures

Root: `components/aihc-tc/test/Test/Fixtures/annotated`.

Fixture shape:

```yaml
extensions: []
modules:
  - |
    module Test where
    id x = x
annotated:
  - |-
    module Test where
    id x = x
    │  └─ x ∷ a
    └─ id ∷ ∀ a. a → a
status: pass
reason: ""
```

Required keys: `extensions`, `modules`, `annotated`, `status`. `reason` is optional except where noted below. `modules` and `annotated` are lists of strings; each `annotated` entry is the rendered overlay for the corresponding module source.

Supported statuses are `pass`, `xfail`, and `xpass`. There is no `fail` status for TC annotated fixtures. Use `pass` for current expected output. Use `xfail` for a known gap and include a concrete `reason`; an `xfail` whose output starts matching becomes an `XPASS` and fails the suite. Use `xpass` only as strict tracking for a known bug that currently matches the annotated output; include `reason`.

The runner:

1. Parses each module with the fixture extensions.
2. Resolves the parsed modules with `aihc-resolve`.
3. Runs `typecheck` over the resolved module list.
4. Renders source overlays with `renderAnnotatedTcResults`.
5. Compares trimmed overlays with the `annotated` list.

Type errors are part of the overlay contract when type checking still returns a rendered module. Parse or resolve failures are failures for `pass`, tolerated for `xfail`, and failures for `xpass`.

Regenerate `annotated` from the current implementation only when the changed output is intentional:

```bash
cabal run -v0 aihc-dev -- update-goldens --dry-run
cabal run -v0 aihc-dev -- update-goldens
```

The updater scans `components/aihc-tc/test/Test/Fixtures/annotated` and updates `annotated` only for `pass` and `xpass` fixtures. It skips `xfail` fixtures and fixtures that cannot parse, resolve, type-check, or render a successful output.

## Unit And Property Tests

Add tests under `components/aihc-tc/test/Test/Tc/Suite.hs` for focused examples. Add properties to `Test/Tc/Properties.hs` and keep them in the `properties` group.

Current unit groups live under `tc-unit` and include literals, application, case, if-then-else, lambda, variables, kinds, annotations, and error-cases. Keep new unit tests in the nearest existing group, or add a narrowly named group when the behavior does not fit.

Prefer properties for invariants such as idempotency, reflexivity, or stability under zonking/substitution. Prefer annotated golden fixtures for user-visible inference and diagnostic examples.

## Validation

Run:

```bash
cabal test -v0 aihc-tc:spec --test-options="--hide-successes"
cabal test -v0 aihc-tc:spec --test-options="--pattern tc-annotated-golden --hide-successes"
cabal test -v0 aihc-tc:spec --test-options="--pattern tc-unit --hide-successes"
cabal test -v0 aihc-tc:spec --test-options="--pattern properties --hide-successes"
```
