[![Documentation](https://img.shields.io/github/actions/workflow/status/ai-haskell-compiler/aihc/deploy-docs.yml?label=docs)](https://ai-haskell-compiler.github.io/aihc/)
[![Generated Reports](https://img.shields.io/github/actions/workflow/status/ai-haskell-compiler/aihc/generated-reports-update.yml?label=reports)](https://github.com/ai-haskell-compiler/aihc/actions/workflows/generated-reports-update.yml)
[![aihc-parser coverage](https://img.shields.io/endpoint?url=https://ai-haskell-compiler.github.io/aihc/coverage/aihc-parser-badge.json)](https://ai-haskell-compiler.github.io/aihc/coverage/aihc-parser-html/hpc_index.html)
[![aihc-cpp coverage](https://img.shields.io/endpoint?url=https://ai-haskell-compiler.github.io/aihc/coverage/aihc-cpp-badge.json)](https://ai-haskell-compiler.github.io/aihc/coverage/aihc-cpp-html/hpc_index.html)

# AI-written Haskell Compiler (aihc)

Can gpt-5.3-codex, Claude Opus 4.5 and Gemini 3 write a Haskell compiler? Probably not but let's find out. We'll need preprocessing, parsing, name resolution, type checking, desugaring, and code generation. Progress will be tracked as a percentage of stackage for each component.

Find more information here:
- [**aihc-cpp** README](https://github.com/ai-haskell-compiler/aihc/tree/main/components/aihc-cpp#readme)
- [**aihc-parser** README](https://github.com/ai-haskell-compiler/aihc/tree/main/components/aihc-parser#readme)
- [Supported extensions](https://github.com/ai-haskell-compiler/aihc/blob/main/docs/haskell-parser-extension-support.md)

## Progress

| Name             | Progress                                                                                                                            |
| ---------------- | ----------------------------------------------------------------------------------------------------------------------------------: |
| Parser Stackage  | <!-- AUTO-GENERATED: START parser-stackage-progress --> `2175/3390` (`64.16%`) <!-- AUTO-GENERATED: END parser-stackage-progress --> |
| Parser Tests     | <!-- AUTO-GENERATED: START parser-progress --> `642/737` (`87.11%`) <!-- AUTO-GENERATED: END parser-progress -->                    |
| Lexer Tests      | <!-- AUTO-GENERATED: START lexer-progress --> `79/80` (`98.75%`) <!-- AUTO-GENERATED: END lexer-progress -->                        |
| CPP preprocessor | <!-- AUTO-GENERATED: START cpp-progress --> `38/38` (`100.00%`) <!-- AUTO-GENERATED: END cpp-progress -->                           |

## Lines of code

<!-- AUTO-GENERATED: START line-counts -->
```
| Component       |   Code |   Tests |   Total |
|-----------------|--------|---------|---------|
| aihc-cpp        |   1522 |     525 |    2047 |
| aihc-parser     |   8334 |   13982 |   22316 |
| aihc-parser-cli |   1882 |     379 |    2261 |
| **Total**       |  11738 |   14886 |   26624 |
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
