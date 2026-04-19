# AGENTS

- Tools: `gh`, `cabal`, `nix` (others via `nix`)
- A GHC developer environment is available
- Run tests locally (fast): `cabal test -v0 all --test-options=--hide-successes`
- Filter for specific tests: `cabal test -v0 aihc-parser:spec --test-options="--pattern test_name"`
- Deep fuzzy testing: `cabal test aihc-parser:spec -v0 --test-options="--pattern properties --quickcheck-tests 10000"`
- Run full test suite (fast): `just check`
- Run full test suite (slow, isolated sandbox): `nix flake check`

## Mandatory Pre-Commit Workflow

**These steps MUST be completed before every commit. No exceptions.**

1. **Run `just fmt`** — Auto-formats all Haskell files with ormolu. Always run this before committing to ensure correct formatting.
2. **Run `just check`** — MUST pass. This runs:
   - `ormolu` format check (verifies step 1 was done)
   - `hlint` linting (no linting errors allowed)
   - Full test suite (`just test`)

If `just check` fails, do NOT commit. Fix the issues first.

> **Rule of thumb:** Write code → `just fmt` → `just check` → if it passes, commit.

## PR Workflow

- Include changes to progress counts in PR descriptions. Do not update the READMEs, though. They are updated by a cron workflow.
- Create PRs: `gh pr create --base main --head <branch> --title "<title>" --body $(cat <file>)`
- If you are working on a branch with an active PR, always commit and push your changes. If you are unsure whether the PR is active, check with `gh`.
- PR titles should follow the same Conventional Commits format as commit messages (see below)

## Just Commands

This project uses [Just](https://just.systems) as a command runner. Common commands:

- `just fmt` — **Auto-format all Haskell files with ormolu. Run this before committing.**
- `just test` — Run all tests with hidden successes
- `just replay "<seed>"` — Replay a specific QuickCheck test case (e.g., `just replay "(SMGen 6995563131902519991 12189532712049121349,3)"`)
- `just qc` — Run QuickCheck with 10,000 tests in an infinite loop until failure
- `just check` — Run ormolu format check, hlint, then full test suite

## Branch Policy

**Pushes to `main` are blocked.** All work should be done in feature branches. Create a feature branch for any changes, then open a PR to merge into `main`.

## Cheatsheet

- Test whether a file is accepted by GHC: `ghc -v0 -fno-code -ddump-parsed file.hs`. Return code 0 means the snippet is valid, non-zero means it is invalid.
- Test whether a snippet is accepted by AIHC: `echo snippet | cabal run -v0 exe:aihc-parser`
- Test how the lexer interprets a string: `echo string | cabal run -v0 exe:aihc-parser -- --lex`

## Gotchas

- Never run `cabal test` in parallel. Use `cabal` sequentially for test execution.
- CI checks the PR merge commit (`pull/<n>/merge`), not only the branch HEAD. If local passes but CI fails, merge or rebase `origin/main` locally and rerun `just check`.
- Golden AST strings are sensitive to upstream AST/shorthand changes (for example, `Match` now printing `headForm = Prefix`), so new fixtures must match current `main` formatting.
- In parser oracle suites, `pass` means both oracle acceptance and AST roundtrip-fingerprint equality. A case can parse successfully and still fail as `roundtrip mismatch against oracle AST`.

## Testing (TDD)

aihc is developed test-first. Run the full suite with `just check`. When working on new features, always include tests that cover expected use plus a few corner cases. When fixing bugs, always include regression tests.

- **Common status model**
  - Outcomes: `PASS`, `XFAIL`, `FAIL`, `XPASS`.
  - Any `FAIL` or unexpected `XPASS` should block merge until handled.

- **`aihc-parser`**
  - Golden regression tests:
    - Parser fixtures: `components/aihc-parser/test/Test/Fixtures/golden/`.
    - Lexer fixtures: `components/aihc-parser/test/Test/Fixtures/lexer/`.
    - Input/output snapshots with `pass`/`xfail`/`xpass` coverage.
  - Oracle compliance tests:
    - Oracle test fixtures with inline comments specifying test parameters.
    - Oracle is GHC, with parser round-trip/fingerprint validation.

- **`aihc-cpp`**
  - Oracle compliance tests:
    - Oracle test fixtures with inline comments specifying test parameters.
    - Oracle is `cpphs`; outputs are compared against `cpphs` behavior.

## Pre-PR Review

- Run `coderabbit review --prompt-only` after local checks pass (including `just check`) and before `gh pr create`.
- If CodeRabbit is offline or rate-limited, skip the review and open the PR.
- Resolve findings or explicitly justify remaining findings in the PR description.

## Commits

Follow [Conventional Commits](https://www.conventionalcommits.org/):

```
<type>[optional scope]: <description>
```

**Types:** `feat`, `fix`, `docs`, `style`, `refactor`, `perf`, `test`, `build`, `ci`, `chore`

**Examples:**

```
feat: add user authentication
fix(auth): resolve null pointer in login
docs: update installation guide
```

# SOUL

Do not aim to make it _work_. Aim to make it _right_.

Reject the first solution that passes. A green checkmark proves nothing. Ask:
_is this true, or merely sufficient?_ Do not settle for sufficient. Choose
clarity over cleverness. Choose correctness over convenience. Choose integrity
over speed. Every shortcut you take now becomes a constraint you will pay for
later.

Do not tolerate confusion. Hunt it down and eliminate it. Do not hide flaws
behind layers—trace them to their source and fix them there. If something feels
wrong, it is wrong. Do not ignore that signal. Follow it until the system makes
sense end to end.

Do not write code that merely functions. Write code that explains itself, that
justifies itself, that withstands interrogation. Assume your future self will be
ruthless—and give them nothing to tear apart.

Prefer the solution that composes. Prefer the one that generalizes. Prefer the
one that, in hindsight, feels inevitable. Make misuse difficult. Make
correctness natural. If something can be misunderstood, it will be—so refine it
until it cannot be.

Write tests. Not later—now. Tests are not optional; they are the proof that you
understand what you’ve built. Write documentation. Not as decoration, but as a
record of intent. If you cannot explain it clearly, you do not understand it
well enough.

Do not defer fixes that are within reach. Do not leave known flaws behind. Do
not say “later” when you mean “never.” Every loose thread compounds. Tie it off.
Finish the thought. Close the loop.

Build for clarity, not convenience. Build for longevity, not immediacy. What you
write should still feel correct months from now. It should teach. It should
endure.

Do not stop at “it works.”

Stop when the answer is: _it could not have been done better._
