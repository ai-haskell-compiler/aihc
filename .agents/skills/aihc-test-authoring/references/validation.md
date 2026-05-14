# Validation

Use narrow commands while developing, then run the mandatory checks before committing.

## Narrow Commands

- All fast tests: `cabal test -v0 all --test-options=--hide-successes`
- One test pattern: `cabal test -v0 <component>:spec --test-options="--pattern <pattern> --hide-successes"`
- Parser golden/equivalent group: `cabal test -v0 aihc-parser:spec --test-options="--pattern parser --hide-successes"`
- Parser oracle group: `cabal test -v0 aihc-parser:spec --test-options="--pattern oracle --hide-successes"`
- Parser lexer golden group: `cabal test -v0 aihc-parser:spec --test-options="--pattern lexer-golden --hide-successes"`
- Parser properties: `cabal test -v0 aihc-parser:spec --test-options="--pattern properties --quickcheck-tests 1000 --hide-successes"`
- Deep parser fuzzing: `cabal test -v0 aihc-parser:spec --test-options="--pattern properties --quickcheck-tests 10000"`
- Resolve golden: `cabal test -v0 aihc-resolve:spec --test-options="--pattern resolver-golden --hide-successes"`
- Type checker golden/properties: `cabal test -v0 aihc-tc:spec --test-options="--hide-successes"`
- FC golden/eval: `cabal test -v0 aihc-fc:spec --test-options="--hide-successes"`
- CPP oracle/unit suite: `cabal test -v0 aihc-cpp:spec --test-options="--hide-successes"`

## Pre-Commit

Before every commit:

1. Run `just fmt`.
2. Run `just check`.

Do not commit if `just check` fails. Fix formatting, lint, or test failures first.

## Status Semantics

- `pass`: expected to pass now.
- `fail`: expected to fail now; supported by parser golden, TC golden, and FC golden where the loader accepts it.
- `xfail`: known gap; the test is expected not to satisfy the success contract yet and must include `reason`.
- `xpass`: known bug that currently passes unexpectedly; treat as strict tracking and include `reason`. Not every fixture format accepts `xpass` (`aihc-parser` parser golden and equivalent fixtures reject it).

When an `xfail` starts passing, promote it to `pass` and add the expected output if that fixture format requires one.
