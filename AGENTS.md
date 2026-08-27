# AGENTS

## Language

Always use ASD-STE100 Simplified Technical English, Issue 9, for all English text.
Obey its writing rules and use its approved dictionary.
Use technical nouns and technical verbs only as ASD-STE100 permits.

This requirement applies to all project communication.
Examples are PR descriptions, chats, documentation, code comments, feedback, and reviews.
It also applies to commit messages, issue text, test descriptions, and user messages.

Use short sentences and give only one instruction in each sentence.
Use a maximum of 20 words in an instruction.
Use a maximum of 25 words in a descriptive sentence.
Use the active voice.
Use the imperative form for instructions.
Use the same word for the same meaning.

Do not use a different word only to make the text varied.
Do not use contractions or semicolons.
Use the `-ing` form of a verb only as an approved technical noun or its modifier.
Put a condition before the action when the reader must know the condition first.
If ASD-STE100 and another writing rule disagree, use ASD-STE100.

## Development Environment

- Use `gh`, `cabal`, and `nix`.
- Use `nix` to get other tools.
- Use the available GHC development environment.
- For a fast local test, use `cabal test -v0 all --test-options=--hide-successes`.
- For a fast check of all items, use `just check`.
- For a slow check in an isolated sandbox, use `nix flake check`.

## Component Boundaries

Each component controls one compiler domain.
Do not put the same domain in two components.

| Component | Input | Output | Domain |
| --- | --- | --- | --- |
| `aihc-resolve` | Parsed surface modules | The same surface AST with binding resolution, use resolution, exports, and resolve diagnostics | Name resolution only |
| `aihc-tc` | Resolved surface AST | The same surface AST with types, kinds, evidence, and type diagnostics | Haskell type checks only |
| `aihc:compiler` FC modules | Type-checked surface AST | System FC program and System FC diagnostics | Desugaring and System FC only |
| `aihc:compiler` GRIN modules | System FC program | Strict GRIN program and GRIN diagnostics | Closure conversion, runtime operations, and GRIN transformations only |

Do not duplicate an upstream responsibility in a downstream component.
Do not do name resolution in `aihc-tc`.
Do not do Haskell type checks in the FC modules.
The FC modules can lint types that are already in System FC.

If a downstream component needs upstream facts, change the upstream component.
Then, use its output in the downstream component.
For example, an FC feature can need new Haskell type information.
In this case, change `aihc-tc` and do not add local type checks to the FC modules.

The GRIN modules must keep the semantics that System FC gives.
It can remove types and coercions.
It can validate GRIN structural invariants.
It must not reconstruct Haskell type information.
It must not duplicate System FC desugaring.

## Mandatory Pre-Commit Procedure

Do these steps before each commit:

1. Run `just fmt`.
   This command formats all Haskell files with Ormolu.
2. Run `just check`.
   This command must complete successfully.
   It does an Ormolu format check, an HLint check, and the full test suite.

If `just check` fails, do not commit.
Correct each problem and run `just check` again.

Use this sequence: write code, run `just fmt`, run `just check`, and commit.

## PR Procedure

- In each PR description, give all changes to progress counts.
- Do not update the README files.
- A cron workflow updates the README files.
- Use `gh pr create --base main --head <branch> --title "<title>" --body "$(cat <file>)"` to create a PR.
- If the branch has an active PR, commit and push all applicable changes.
- If you do not know whether the PR is active, use `gh` to get its status.
- Use the Conventional Commits format for each PR title.
- Write each PR description in ASD-STE100.

## Just Commands

This project uses Just as its command runner.
Use these commands:

- `just fmt` formats all Haskell files with Ormolu.
- `just test` runs all tests and hides successful results.
- `just replay "<seed>"` runs one Hedgehog case again.
  Example: `just replay "3:b2 Seed 10332913068362713902 1302058653756691475"`.
- `just hedgehog` runs 10,000 Hedgehog tests in a continuous cycle until a test fails.
- `just check` runs the Ormolu format check, HLint, and the full test suite.

## Branch Policy

Do not push to `main`.
The repository blocks these pushes.
Make a feature branch for each change.
Then, open a PR to merge the branch into `main`.

## Important Test Information

- Do not run two `cabal test` commands at the same time.
- Run each `cabal` test command separately.
- CI checks the PR merge commit at `pull/<n>/merge`.
- CI does not only check the branch `HEAD`.
- If local checks pass and CI fails, merge or rebase `origin/main` into the branch.
- Then, run `just check` again.

## Test-First Development

Use test-first development for `aihc`.
For a new feature, first add tests for its expected use and some limit conditions.
For a defect correction, first add a regression test.

### Mandatory Fixture Test Gate

Use a fixture when source text can trigger the tested behavior.
An internal API or invariant does not by itself permit a hand-written unit test.
Do not add a hand-written unit test when an existing or new fixture can trigger the same failure.

Before you add a hand-written unit test, complete these steps:

1. Identify the essential property that requires the unit test.
2. Explain why no fixture can test that property.
3. Get explicit user approval for the exception.
4. Add this explanation as a comment next to the test.

If you cannot complete all four steps, do not add the unit test.

Before each commit, inspect the diff for new hand-written tests.
Remove each test that does not have an approved exception comment.

Run the full suite with `just check`.
The common test results are `PASS`, `XFAIL`, `FAIL`, and `XPASS`.
Do not merge a change that has a `FAIL` result or an unexpected `XPASS` result.

The standalone repositories control parser and preprocessor changes.
They also control their fixtures, fuzz tests, and oracle tests.
This repository uses only their released public libraries.

## Commits

Use the Conventional Commits format:

```text
<type>[optional scope]: <description>
```

Use one of these types:
`feat`, `fix`, `docs`, `style`, `refactor`, `perf`, `test`, `build`, `ci`, or `chore`.

Examples:

```text
feat: add user authentication
fix(auth): correct null pointer in login
docs: update installation guide
```

Write each commit message in ASD-STE100.
