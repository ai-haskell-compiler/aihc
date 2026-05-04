---
name: aihc-resolve-test-cases
description: Create, update, and validate `aihc-resolve` golden fixtures for parser and scope regressions.
---

# AIHC Resolve Test Cases

Use for `components/aihc-resolve/test/Test/Fixtures/golden/*.yaml`.

1. Put one minimal case in `modules`; each entry is parsed as one module.
2. Required keys: `extensions`, `modules`, `status`, `annotated`. `pass` also requires `expected`; `xfail` requires `reason`; `xpass` requires both.
3. Statuses: `pass` must match output; `xfail` must fail; `xpass` is a strict pass that still fails CI.
4. Prefer one issue per fixture. For class/import regressions, use one declaration module plus one lookup/use module.
5. Check with `cabal test -v0 aihc-resolve:spec --test-options="--pattern resolver-golden"`. Loader/schema failures come before semantic claims.

## Minimal fixture shape

```yaml
extensions: []
modules:
  - |
    module Main where
    x = 1
annotated: []
status: xfail
reason: "Short explanation of the known gap."
```

## Class/import fixture shape

```yaml
extensions: []
modules:
  - |
    module ClassMod where
    class Box a where
      pick :: a -> a
  - |
    module Main where
    import ClassMod (Box (pick))

    f = pick
annotated: []
status: xfail
reason: "Short explanation of the known gap."
```
