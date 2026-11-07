# Contributing to AIHC

Thank you for your interest in AIHC.
This document tells you how to make a change and how to get it merged.
The file [AGENTS.md](AGENTS.md) gives the full rules for code, tests, and text.
Humans and AI agents obey the same rules.

## Who can contribute

Pull requests from everyone are welcome.
You can write a change by hand or with AI tools.
The project accepts both.
The maintainer reviews each pull request with AI tools.

You do not need to open an issue before you make a pull request.
Instead, explain in the pull request description why the change is an improvement.

If you want to discuss an idea first, use the [Discord server](https://discord.gg/uGWkhMCZrZ) or open an issue.

## License

AIHC is in the public domain under [the Unlicense](LICENSE).
Each contribution must be under the same terms.

When you open a pull request, you dedicate your contribution to the public domain under the Unlicense.
If you cannot make this dedication, do not open the pull request.

Obey these rules:

- Do not add a copyright notice or a license header to a file.
- Do not copy code from a project with a different license.
  For example, GHC and its boot libraries use the BSD-3-Clause license.
  Write new code instead.
- Do not copy text from a document with a different license.
  For example, the GHC User's Guide is not in the public domain.

## Development environment

The project uses `nix`, `cabal`, `gh`, and `just`.
Enter the development environment with `nix develop`.
The environment gives you GHC, Cabal, Ormolu, HLint, and the other tools.

| Command | Effect |
| --- | --- |
| `cabal build exe:aihc` | Build the compiler. |
| `just fmt` | Format all Nix, Cabal, Haskell, and C files. |
| `just test` | Run all tests and hide successful results. |
| `just check` | Run the format check, HLint, and the full test suite. |
| `just replay "<seed>"` | Run one Hedgehog case again. |
| `just hedgehog` | Run 10,000 Hedgehog tests in a continuous cycle until a test fails. |
| `nix flake check` | Run the slow check in an isolated sandbox. |

Do not run two `cabal test` commands at the same time.

## Make a change

1. Make a feature branch from `main`.
   Do not push to `main`.
   The repository blocks these pushes.
2. Make the change.
   Keep each change in one component.
   See the component boundaries in [AGENTS.md](AGENTS.md#component-boundaries).
3. Add a test for each defect that the change corrects.
   See the test rules in [AGENTS.md](AGENTS.md#test-rules).
4. Run `just fmt`.
   Then, run `just check`.
5. Push the branch.
   Then, open a pull request against `main`.

## Pull requests

### Title

Use the [Conventional Commits](https://www.conventionalcommits.org/) format for the title:

```text
<type>[optional scope]: <description>
```

Use one of these types:
`feat`, `fix`, `docs`, `style`, `refactor`, `perf`, `test`, `build`, `ci`, or `chore`.

The scope names the component.
Examples are `resolve`, `tc`, `fc`, `grin`, `lir`, `rts`, `base`, `native`, `llvm`, and `wasm`.

```text
fix(tc): keep a rule with an open equality in source form
perf(lir): copy small continuations into the functions that call them
refactor(resolve): use MonoidMap in ModuleExports
```

### Description

The repository merges each pull request as one squash commit.
The pull request title becomes the commit title.
The pull request description becomes the commit body.
Thus, write the description for a reader of `git log`.

- Explain why the change is an improvement.
- Keep the context that a future reader needs.
  Examples are the steps of a refactor and the reason for a design decision.
- Remove text that only repeats the code change.
- Give all changes to progress counts.
- For each changed test expectation, give the reason.
- For each new test, give its cost.
  The cost is run time, flakiness, and the number of unrelated changes that will break it.
- For a performance change, give the measurements before and after the change.

Do not update the README files.
A cron workflow updates them.

### Checks and merge

CI runs on the merge commit at `pull/<n>/merge`, not only on the branch head.
These checks must pass:

- `flake-check`, which runs `nix flake check`.
- A build of the components with each supported GHC version.

If local checks pass and CI fails, merge `origin/main` into the branch.
Then, run `just check` again.

A maintainer merges the pull request when CI passes.
A merge queue applies the squash merge.
A contributor with write access can merge their own pull request when CI passes.

## Report a bug

Open an issue with this information:

- The smallest program that shows the bug.
- The commit of AIHC that you used.
- The `aihc` command that you ran, with its `--target` option.
- The output that you got.
- The output that you expected.
