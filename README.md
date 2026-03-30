[![Documentation](https://img.shields.io/github/actions/workflow/status/ai-haskell-compiler/aihc/deploy-docs.yml?label=docs)](https://ai-haskell-compiler.github.io/aihc/)
[![Generated Reports](https://img.shields.io/github/actions/workflow/status/ai-haskell-compiler/aihc/generated-reports-update.yml?label=reports)](https://github.com/ai-haskell-compiler/aihc/actions/workflows/generated-reports-update.yml)
[![aihc-parser coverage](https://img.shields.io/endpoint?url=https://ai-haskell-compiler.github.io/aihc/coverage/aihc-parser-badge.json)](https://ai-haskell-compiler.github.io/aihc/coverage/aihc-parser-html/hpc_index.html)
[![aihc-cpp coverage](https://img.shields.io/endpoint?url=https://ai-haskell-compiler.github.io/aihc/coverage/aihc-cpp-badge.json)](https://ai-haskell-compiler.github.io/aihc/coverage/aihc-cpp-html/hpc_index.html)

# AI-written Haskell Compiler (aihc)

Can gpt-5.3-codex and Claude Opus 4.5 write a Haskell compiler? Probably not but let's find out. We'll need preprocessing, parsing, name resolution, type checking, desugaring, and code generation. Progress will be tracked as a percentage of stackage for each component.

## Progress

| Name             | Progress                                                                                                                            |
| ---------------- | ----------------------------------------------------------------------------------------------------------------------------------- |
| Parser Stackage  | <!-- AUTO-GENERATED: START parser-stackage-progress --> `1153/3390` (`34.01%`) <!-- AUTO-GENERATED: END parser-stackage-progress --> |
| Parser Tests     | <!-- AUTO-GENERATED: START parser-progress --> `472/675` (`69.93%`) <!-- AUTO-GENERATED: END parser-progress -->                    |
| Lexer Tests      | <!-- AUTO-GENERATED: START lexer-progress --> `64/65` (`98.46%`) <!-- AUTO-GENERATED: END lexer-progress -->                        |
| CPP preprocessor | <!-- AUTO-GENERATED: START cpp-progress --> `37/37` (`100.00%`) <!-- AUTO-GENERATED: END cpp-progress -->                           |

## Lines of code

<!-- AUTO-GENERATED: START line-counts -->
```
| Component       |   Code |   Tests |   Total |
|-----------------|--------|---------|---------|
| aihc-cpp        |   1064 |     512 |    1576 |
| aihc-parser     |   6815 |   12177 |   18992 |
| aihc-parser-cli |    159 |     375 |     534 |
| **Total**       |   8038 |   13064 |   21102 |
```
<!-- AUTO-GENERATED: END line-counts -->

<!-- Both commands are broken atm
## Ways to contribute

- PRs accepted and bugs are welcomed.
- If you have spare inference, run this command to generate a prompt: `nix run .#prompt`
- If you have spare compute, run this command to fuzz test aihc: `nix run .#parser-fuzz`
-->

## Nix Commands

Run the full test suite:

```bash
nix flake check
```

### Apps

| Command                               | Description                               |
| ------------------------------------- | ----------------------------------------- |
| `nix run .#parser-test`               | Run parser test suite                     |
| `nix run .#cpp-test`                  | Run CPP preprocessor test suite           |
| `nix run .#parser-progress`           | Show parser oracle test progress          |
| `nix run .#lexer-progress`            | Show lexer oracle test progress           |
| `nix run .#parser-extension-progress` | Show parser extension test progress       |
| `nix run .#cpp-progress`              | Show CPP preprocessor test progress       |
| `nix run .#stackage-progress`         | Show parser progress on Stackage packages |
| `nix run .#prompt`                    | Generate a prompt for contributing        |
| `nix run .#hackage-tester`            | Test parser against Hackage packages      |
| `nix run .#parser-fuzz`               | Fuzz test the parser                      |
| `nix run .#parser-quickcheck-batch`   | Run QuickCheck property tests             |
| `nix run .#parser-quickcheck-soak`    | Run QuickCheck soak tests                 |
| `nix run .#line-counts`               | Show line counts per component            |
| `nix run .#generate-reports`          | Update generated README content           |
| `nix run .#check-reports`             | Check if generated content is up-to-date  |

Strict variants (fail on unexpected results): `parser-progress-strict`, `lexer-progress-strict`, `parser-extension-progress-strict`, `cpp-progress-strict`.
