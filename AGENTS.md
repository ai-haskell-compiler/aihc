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

## Mandatory Post-Push Procedure

Open a PR as soon as possible.

After each push, run `just fmt` and then `just check` locally.

If `just fmt` changes files, commit and push those changes.
Then, run `just fmt` and `just check` again.

If `just check` fails, correct each problem, commit and push the changes, then run both commands again.

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

## Test Rules

### Do

- Each new test must fail without the other changes in the PR.
  Run the test on `main` or with the fix reverted.
  If the test passes before and after, delete it.
- Before you add a test, name the defect it finds.
  No existing test must find that defect.
  If you cannot name one, do not add the test.
- Put the test in the existing harness for its layer.
  Examples are the resolver, tc, FC, GRIN, and eval fixtures.
  Do not write a new harness for one test.
- Test at the lowest layer that shows the defect.
  A resolver defect gets a resolver fixture, not an end-to-end program.
  If a defect only shows end-to-end, use an end-to-end test.
- For a bug fix, first reduce the input.
  Find the smallest program that shows the bug.
  Then add that program as the test.
- Test observable results only.
  Examples are diagnostics, golden output, and program output.
- Keep golden output small.
  A golden file that changes on each unrelated optimizer change is too costly.
- A property test must state a property that is not the implementation.
  Say what the generator covers.
- Make each test give the same result on each run.
  Do not depend on wall-clock time, the network, the environment, or map order.
- When a test fails after your change, give a reason in the PR for each changed expectation.
  Do not accept a new golden output without a reason.
- Give the cost of each new test in the PR.
  The cost is run time, flakiness, and the number of unrelated changes that will break it.

### Do Not

- Do not add tests to a refactor PR.
  If the behavior is the same, the existing tests are the proof.
  An empty test diff is the evidence that the PR is a refactor.
- Do not test what the type checker already proves.
  A test that a bad program gives a diagnostic is correct.
  A test that a well-typed value has a type is not.
- Do not test trivial code.
  Examples are getters, constants, Show instances, and derived instances.
- Do not compute the expected value with the function under test.
  That test cannot fail.
- Do not use mocks.
  The compiler is pure.
  Run the real pipeline.
- Do not add timing tests.
  They are fragile.
  If performance is the subject, measure allocation or instruction counts.
  As an alternative, report the numbers in the PR and add no test.
- Do not skip, weaken, or delete a failing test to make CI green.
- Do not assert on internal data structures.
  A refactor can change them without a change in behavior.
- Do not use test coverage as a goal.
- Do not put test-only code in the `aihc` library.
  Put it in `aihc-testing` or beside the caller.

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
