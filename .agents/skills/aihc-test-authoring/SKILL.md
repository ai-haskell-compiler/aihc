---
name: aihc-test-authoring
description: Choose, author, and validate tests for the AIHC repository. Use when Codex needs to add or update tests in components/aihc-cpp, components/aihc-parser, components/aihc-resolve, components/aihc-tc, components/aihc-fc, or related compatibility/property suites, including unit tests, golden YAML fixtures, oracle fixtures, lexer fixtures, error-message fixtures, eval fixtures, and QuickCheck properties.
---

# AIHC Test Authoring

Use this skill to decide which AIHC test framework matches a change, then load the relevant reference file before editing tests.

## Workflow

1. Identify the component touched by the change.
2. Choose the narrowest test that proves the behavior.
3. Read the matching component reference before creating or changing fixtures.
4. Add a minimal regression case plus nearby corner cases when the behavior has boundaries.
5. Run the narrow component test first, then the repository check required by `AGENTS.md` before commit.

## Which Test To Use

- Use a unit test when the behavior depends on a specific internal API, failure mode, edge case, or invariant that an oracle cannot express reliably.
- Use a golden fixture when the important contract is the component's rendered output: parsed AST shorthand, resolver annotations, inferred types, System FC output, lexer token kinds, or diagnostics.
- Use an oracle fixture when there is an external source of truth. Parser oracle tests compare with GHC and round-trip/fingerprint validation; CPP progress tests compare with `cpphs`.
- Use an equivalent parser fixture when several source spellings should normalize to the same parsed tree after annotation stripping and paren normalization.
- Use a QuickCheck property when the requirement is broad, algebraic, or generative: round trips, idempotency, no exceptions, shrinking, coverage, or compatibility over generated syntax.
- Use an `xfail` fixture only to record a known gap while preventing silent regressions. Always include a concrete reason, and expect unexpected passes to fail CI until the fixture is promoted. Use `xpass` only in fixture formats that explicitly support it.

Prefer a single focused fixture over a large scenario unless the behavior only appears through cross-module interaction.

## Component References

- `aihc-cpp`: read `references/aihc-cpp.md` for `cpphs` progress fixtures and preprocessor unit tests.
- `aihc-parser`: read `references/aihc-parser.md` for parser golden, equivalent, lexer golden, error-message, oracle, unit, performance, and property tests.
- `aihc-resolve`: read `references/aihc-resolve.md` for resolver golden fixtures and unit tests.
- `aihc-tc`: read `references/aihc-tc.md` for type-checker golden fixtures, unit tests, and properties.
- `aihc-fc`: read `references/aihc-fc.md` for FC golden fixtures, eval fixtures, and unit tests.
- Shared commands and status semantics: read `references/validation.md`.

## Ground Rules

- Keep fixture names descriptive and scoped to the behavior, not the implementation detail that happens to expose it.
- Put parser oracle fixtures under the feature directory that describes the language construct, not under a bug bucket, unless the existing suite has a more precise local convention.
- Preserve the repository status model: `PASS`, `XFAIL`, `FAIL`, `XPASS`. Any `FAIL` or unexpected `XPASS` blocks merge.
- Do not update repository READMEs for progress counts. PR descriptions may mention count changes, but README progress is cron-managed.
- Never run `cabal test` in parallel in this repository.
