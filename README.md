[![Documentation](https://img.shields.io/github/actions/workflow/status/ai-haskell-compiler/aihc/deploy-docs.yml?label=docs)](https://ai-haskell-compiler.github.io/aihc/)
[![Generated Reports](https://img.shields.io/github/actions/workflow/status/ai-haskell-compiler/aihc/generated-reports-update.yml?label=reports)](https://github.com/ai-haskell-compiler/aihc/actions/workflows/generated-reports-update.yml)
[![aihc-parser coverage](https://img.shields.io/endpoint?url=https://ai-haskell-compiler.github.io/aihc/coverage/aihc-parser-badge.json)](https://ai-haskell-compiler.github.io/aihc/coverage/aihc-parser-html/hpc_index.html)
[![aihc-cpp coverage](https://img.shields.io/endpoint?url=https://ai-haskell-compiler.github.io/aihc/coverage/aihc-cpp-badge.json)](https://ai-haskell-compiler.github.io/aihc/coverage/aihc-cpp-html/hpc_index.html)

# AI-written Haskell Compiler (aihc)

Can gpt-5.4, Claude Opus 4.5 and Qwen-3.6 write a Haskell compiler? Probably not but let's find out. We'll need preprocessing, parsing, name resolution, type checking, desugaring, and code generation. Progress will be tracked as a percentage of stackage for each component.

Find more information here:
- [**aihc-cpp** README](https://github.com/ai-haskell-compiler/aihc/tree/main/components/aihc-cpp#readme)
- [**aihc-parser** README](https://github.com/ai-haskell-compiler/aihc/tree/main/components/aihc-parser#readme) - [Supported extensions](https://github.com/ai-haskell-compiler/aihc/blob/main/docs/haskell-parser-extension-support.md)
- [**aihc-tc** README](https://github.com/ai-haskell-compiler/aihc/tree/main/components/aihc-tc#readme) - [Supported extensions](https://github.com/ai-haskell-compiler/aihc/blob/main/docs/aihc-tc-supported-extensions.md)

## Progress

| Name               | Progress                                                                                                                             |
| ------------------ | -----------------------------------------------------------------------------------------------------------------------------------: |
| TypeCHeck Stackage | `0/3390` (`0.00%`)                                                                                                                   |
| Resolve Stackage   | `0/3390` (`0.00%`)                                                                                                                   |
| Parser Stackage    | <!-- AUTO-GENERATED: START parser-stackage-progress --> `2831/3390` (`83.51%`) <!-- AUTO-GENERATED: END parser-stackage-progress --> |
| TypeCheck Tests    | <!-- AUTO-GENERATED: START tc-progress --> `6/6` (`100.00%`) <!-- AUTO-GENERATED: END tc-progress -->                                |
| Resolve Tests      | <!-- AUTO-GENERATED: START resolve-progress --> `8/8` (`100.00%`) <!-- AUTO-GENERATED: END resolve-progress -->                      |
| Parser Tests       | <!-- AUTO-GENERATED: START parser-progress --> `798/821` (`97.20%`) <!-- AUTO-GENERATED: END parser-progress -->                     |
| Lexer Tests        | <!-- AUTO-GENERATED: START lexer-progress --> `90/90` (`100.00%`) <!-- AUTO-GENERATED: END lexer-progress -->                        |
| CPP Tests          | <!-- AUTO-GENERATED: START cpp-progress --> `38/38` (`100.00%`) <!-- AUTO-GENERATED: END cpp-progress -->                            |

## Lines of code

<!-- AUTO-GENERATED: START line-counts -->
```
| Component       |   Code |   Tests |   Total |
|-----------------|--------|---------|---------|
| aihc-cpp        |   1522 |     525 |    2047 |
| aihc-parser     |  10582 |   14079 |   24661 |
| aihc-parser-cli |   1624 |     379 |    2003 |
| aihc-resolve    |   1012 |     583 |    1595 |
| **Total**       |  14740 |   15566 |   30306 |
```
<!-- AUTO-GENERATED: END line-counts -->

<!-- Both commands are broken atm
## Ways to contribute

- PRs accepted and bugs are welcomed.
- If you have spare inference, run this command to generate a prompt: `nix run .#prompt`
- If you have spare compute, run this command to fuzz test aihc: `nix run .#parser-fuzz`
-->

## Useful Commands

Run the full test suite:

```
just check
```

Run the full test suite in a hermetic build environment (slower than `just check`):

```bash
nix flake check
```

