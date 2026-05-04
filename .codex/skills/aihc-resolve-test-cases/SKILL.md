---
name: aihc-resolve-test-cases
description: Use for creating, updating, and validating `aihc-resolve` golden fixtures for parser and scope behavior regressions.
---

# AIHC Resolve Test Cases

## Use case

Use this skill when you need to add or repair fixtures for `components/aihc-resolve/test/Test/Fixtures/golden/`.

## Quick workflow

1. Read `references/aihc-resolve-golden.md`.
2. Create a minimal failing snippet in `modules` and classify it as `pass`, `xfail`, or `xpass`.
3. Ensure required keys are present in the YAML fixture (`extensions`, `modules`, `status`, and `annotated`).
4. Keep scope as small as possible and prefer one issue per fixture.
5. Run the focused test target when available.

## Common validation points

- Use `status: xfail` for known parser/resolve gaps.
- Keep `annotated` present as a list; for `xfail` it may be `[]`.
- For `pass` and `xpass`, include both `expected` and `annotated`.
- For class-related regressions, include the declaration module and a second module that exercises the lookup behavior.
- If uncertain about fixture validity, run the fixture loader through resolver tests and confirm it parses before expecting semantics.

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

## References

- [`/Users/lemmih/.codex/skills/aihc-resolve-test-cases/references/aihc-resolve-golden.md`](references/aihc-resolve-golden.md)
