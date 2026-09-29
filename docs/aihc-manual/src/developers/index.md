# Developer manual

This part of the manual is for people who change the compiler.
It describes the design of the compiler and of its run-time system.

| Page | Content |
| --- | --- |
| [Compiler pipeline](pipeline/index.md) | The components of the compiler and their boundaries. |
| [Info tables](info-tables.md) | How the run-time system describes each object in managed memory. |

## Development workflow

The repository uses `just` as its command runner.

| Command | Purpose |
| --- | --- |
| `just fmt` | Format all Haskell files with Ormolu. |
| `just test` | Run all tests and hide successful results. |
| `just check` | Run the format check, HLint, and the full test suite. |
| `just docs` | Serve this manual at `http://127.0.0.1:8000/`. |
| `just docs-examples` | Compile the [pipeline examples](pipeline/examples.md) of this manual with the local compiler. `just docs` does this first. |

Run `just fmt` and `just check` before each commit.
The file [AGENTS.md](https://github.com/ai-haskell-compiler/aihc/blob/main/AGENTS.md) gives the full development rules.

## Design documents

The directory [docs/](https://github.com/ai-haskell-compiler/aihc/tree/main/docs) of the repository has the design documents of each compiler stage.
This manual will absorb them over time.
