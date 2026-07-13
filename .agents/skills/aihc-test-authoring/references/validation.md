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
- Type checker annotated golden/unit/properties: `cabal test -v0 aihc-tc:spec --test-options="--hide-successes"`
- FC golden/eval: `cabal test -v0 aihc-fc:spec --test-options="--hide-successes"`
- GRIN lowering/interpreter/shared eval: `cabal test -v0 aihc-grin:spec --test-options="--hide-successes"`
- CPP oracle/unit suite: `cabal test -v0 aihc-cpp:spec --test-options="--hide-successes"`

## Updating Golden Outputs

Use the golden updater when a fixture's expected output should be regenerated from the current implementation instead of hand-written:

```bash
cabal run -v0 aihc-dev -- update-goldens --dry-run
cabal run -v0 aihc-dev -- update-goldens
```

Run it from the repository root. Use `--root <dir>` only when invoking it from another working directory. The updater scans parser AST goldens, lexer goldens, parser error messages, resolver goldens, TC annotated goldens, FC goldens, FC eval outputs, formatter goldens, and parser CLI goldens. It updates generated expectation fields such as `ast`, `tokens`, `expected`, `output`, and `annotated` only for `pass` and `xpass` fixtures; `fail` and `xfail` cases are skipped because their expected output is not the current success contract.

After running without `--dry-run`, inspect the diff before accepting it. A changed golden is evidence that behavior changed, not evidence that the new behavior is correct. Keep intentional fixture metadata (`status`, `reason`, extension lists, dependencies, module sources, and expression sources) under review.

Use `--external-formatters` only when intentionally refreshing formatter goldens for external tools that are installed locally. Otherwise the updater refreshes the in-repo formatter outputs and skips external formatter outputs.

## Pre-Commit

Before every commit:

1. Run `just fmt`.
2. Run `just check`.

Do not commit if `just check` fails. Fix formatting, lint, or test failures first.

## Status Semantics

- `pass`: expected to pass now.
- `fail`: expected to fail now; supported by parser golden, FC golden, and FC eval fixtures where the loader accepts it. TC annotated fixtures do not accept `fail`.
- `xfail`: known gap; the test is expected not to satisfy the success contract yet and must include `reason`.
- `xpass`: known bug that currently passes unexpectedly; treat as strict tracking and include `reason`. Not every fixture format accepts `xpass` (`aihc-parser` parser golden and equivalent fixtures reject it).

When an `xfail` starts passing, promote it to `pass` and add the expected output if that fixture format requires one.
