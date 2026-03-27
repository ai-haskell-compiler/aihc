# AGENTS

- Tools: `gh`, `nix` (others via `nix`)
- Run tests: `nix flake check`
- Include changes to progress counts in PR descriptions. Do not update the READMEs, though. They are updated by a cron workflow.
- Commands:
  - Parser: `nix run .#parser-progress`
  - Extensions: `nix run .#parser-extension-progress`
  - CPP: `nix run .#cpp-progress`

- Create PRs: `gh pr create --base main --head <branch> --title "<title>" --body $(cat <file>)`
- PR titles should follow the same Conventional Commits format as commit messages (see below)

## Cheatsheet

- Test whether a snippet is accepted by GHC: `echo snippet | ghci -v0`. Return code 0 means the snippet is valid, non-zero means it is invalid.
- Test whether a snippet is accepted by AIHC: `echo snippet | nix run .#aihc-parser`
- Test how the lexer interprets a string: `echo string | nix run .#aihc-lexer`

## Gotchas

- CI checks the PR merge commit (`pull/<n>/merge`), not only the branch HEAD. If local passes but CI fails, merge or rebase `origin/main` locally and rerun `nix flake check`.
- Golden AST strings are sensitive to upstream AST/shorthand changes (for example, `Match` now printing `headForm = Prefix`), so new fixtures must match current `main` formatting.
- `nix run .#parser-test` in local shell may fail on CLI executable discovery (`aihc-parser`/`aihc-lexer` not in `PATH`) even when derivation-based `nix flake check` is green.

## Testing (TDD)

aihc is developed test-first. Run the full suite with `nix flake check`. When working on new features, always include tests that cover expected use plus a few corner cases. When fixing bugs, always include regression tests.

- **Common status model**
  - Outcomes: `PASS`, `XFAIL`, `FAIL`, `XPASS`.
  - Any `FAIL` or unexpected `XPASS` should block merge until handled.

- **`aihc-parser`**
  - Golden regression tests:
    - Parser fixtures: `components/aihc-parser/test/Test/Fixtures/golden/`.
    - Lexer fixtures: `components/aihc-parser/test/Test/Fixtures/lexer/`.
    - Input/output snapshots with `pass`/`xfail`/`xpass` coverage.
  - Oracle compliance tests:
    - Haskell2010 manifest: `components/aihc-parser/test/Test/Fixtures/haskell2010/manifest.tsv`.
    - Extension manifests: per-extension `manifest.tsv` files under `components/aihc-parser/test/Test/Fixtures/`.
    - Oracle is GHC, with parser round-trip/fingerprint validation.
  - Fuzz completeness tests:
    - Run `nix run .#parser-fuzz -- ...`.
    - Generates random modules, finds parser-validation failures, and shrinks to minimal repros.
    - Promote minimized repros into golden/oracle fixtures.

- **`aihc-cpp`**
  - Oracle compliance tests:
    - Manifest: `components/aihc-cpp/test/Test/Fixtures/progress/manifest.tsv`.
    - Oracle is `cpphs`; outputs are compared against `cpphs` behavior.

## Gotchas

- `nix flake check` builds from tracked Git sources. If a manifest references a newly created fixture file, run `git add <file>` before `nix flake check` or the suite may fail with "Manifest references missing case file".
- In parser oracle suites, `pass` means both oracle acceptance and AST roundtrip-fingerprint equality. A case can parse successfully and still fail as `roundtrip mismatch against oracle AST`.
- `xfail` rows in fixture `manifest.tsv` files require a reason column (5th TSV field).

## Pre-PR Review

- Run `coderabbit review --prompt-only` after local checks pass (including `nix flake check`) and before `gh pr create`.
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
