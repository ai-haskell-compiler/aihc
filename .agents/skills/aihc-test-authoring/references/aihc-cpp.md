# aihc-cpp Tests

Use `aihc-cpp` tests for preprocessor behavior in `components/aihc-cpp`.

## Choose The Test

- Use progress fixtures for behavior that should match `cpphs` output.
- Use unit tests in `components/aihc-cpp/test/Spec.hs` when `cpphs` is not the right authority, when `cpphs` is imperfect, or when the behavior is an internal API/invariant such as include continuations, configured predefined macros, or diagnostics.
- Use `xfail` progress rows for known `cpphs` mismatches that should remain visible.

## Progress Fixtures

Files live in `components/aihc-cpp/test/Test/Fixtures/progress`.

Create a `.hs` source file, then add a tab-separated row to `manifest.tsv`:

```text
id<TAB>category<TAB>path<TAB>expected<TAB>reason
```

Columns:

- `id`: unique test id, usually filename without `.hs`.
- `category`: grouping such as `macro`, `conditionals`, `diagnostics`, `include`, `lexing`, or `pkg`.
- `path`: fixture path relative to `test/Test/Fixtures/progress`.
- `expected`: `pass` or `xfail`.
- `reason`: required for `xfail`; optional for `pass`.

The runner preprocesses the fixture with AIHC and with `cpphs`, normalizes `#line` locations into located records, and compares the results.

Includes are resolved relative to the current source/include directory. Put include files under `test/Test/Fixtures/progress/includes` or next to the fixture if that matches the scenario.

## Unit Tests

Add focused `tasty-hunit` tests in `components/aihc-cpp/test/Spec.hs` when the assertion needs exact `Result`, `Step`, diagnostics, include continuation behavior, macro configuration, or output independent from `cpphs`.

Keep unit tests short: build an input `ByteString`, run `preprocess defaultConfig{...}`, drive `NeedInclude` when needed, and assert on `resultOutput` or diagnostics.

## Validation

Run:

```bash
cabal test -v0 aihc-cpp:spec --test-options="--hide-successes"
```
