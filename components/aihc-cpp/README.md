# Haskell CPP

This component implements a pure Haskell C preprocessor used by the parser pipeline.

## Why not use an off-the-shelf CPP?

- `cpp` doesn't work with WASM, so we can't use it.
- `cpphs` is ruled out because: Nondeterministic `__DATE__` and `__TIME__` handling, can't be used without IO, LGPL license.

## Progress Tracking

Coverage is tracked with a manifest-driven corpus under:

- `test/Test/Fixtures/progress/manifest.tsv`

Current baseline:

<!-- AUTO-GENERATED: START cpp-progress -->

- `37/37` implemented (`100.00%` complete)
<!-- AUTO-GENERATED: END cpp-progress -->

## Commands

Run all cpp tests:

```bash
nix run .#cpp-test
```

Run progress summary:

```bash
nix run .#cpp-progress
```

Strict mode (non-zero on `FAIL` or `XPASS`):

```bash
nix run .#cpp-progress-strict
```
