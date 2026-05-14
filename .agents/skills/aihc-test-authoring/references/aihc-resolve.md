# aihc-resolve Tests

Use `aihc-resolve` tests for name-resolution behavior, scopes, imports, builtins, annotations, and diagnostics rendered by the resolver.

## Choose The Test

- Use golden fixtures for resolver output and annotated source output.
- Use unit tests in `components/aihc-resolve/test/Test/Resolver/Suite.hs` only for resolver test harness behavior or assertions that cannot be represented as a source fixture.

## Golden Fixtures

Root: `components/aihc-resolve/test/Test/Fixtures/golden`.

Fixture shape:

```yaml
extensions: []
modules:
  - |
    module Main where
    x = 1
expected:
  Main:
    - "2:1-2:2 x => (value) Main.x"
annotated:
  - |
    module Main where
    x = 1
status: pass
```

Required keys: `extensions`, `modules`, `annotated`, `status`. `pass` and `xpass` require `expected` and non-empty `annotated`. `xfail` and `xpass` require `reason`.
`reason` is optional for `pass`.

`modules` is a list because resolver behavior often needs multiple modules. Use one declaration module plus one use module for import/export regressions. The parser source name for each module is the first line up to newline, so include a normal `module X where` header.

`expected` may be a string, a list of strings, or an object keyed by module name. `annotated` is always a list of source renderings.

## Validation

Run:

```bash
cabal test -v0 aihc-resolve:spec --test-options="--pattern resolver-golden --hide-successes"
```
