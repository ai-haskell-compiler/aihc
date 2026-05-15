[![Documentation](https://img.shields.io/github/actions/workflow/status/ai-haskell-compiler/aihc/deploy-docs.yml?label=docs)](https://ai-haskell-compiler.github.io/aihc/)
[![Generated Reports](https://img.shields.io/github/actions/workflow/status/ai-haskell-compiler/aihc/generated-reports-update.yml?label=reports)](https://github.com/ai-haskell-compiler/aihc/actions/workflows/generated-reports-update.yml)
[![aihc-parser coverage](https://img.shields.io/endpoint?url=https://ai-haskell-compiler.github.io/aihc/coverage/aihc-parser-badge.json)](https://ai-haskell-compiler.github.io/aihc/coverage/aihc-parser-html/hpc_index.html)
[![aihc-cpp coverage](https://img.shields.io/endpoint?url=https://ai-haskell-compiler.github.io/aihc/coverage/aihc-cpp-badge.json)](https://ai-haskell-compiler.github.io/aihc/coverage/aihc-cpp-html/hpc_index.html)

# AI-written Haskell Compiler (aihc)

Can gpt, Claude Opus and Qwen-Coder write a Haskell compiler? Probably not but let's find out. We'll need preprocessing, parsing, name resolution, type checking, desugaring, and code generation. Progress will be tracked as a percentage of stackage for each component.

Find more information here:
- [**aihc-cpp** README](https://github.com/ai-haskell-compiler/aihc/tree/main/components/aihc-cpp#readme)
- [**aihc-parser** README](https://github.com/ai-haskell-compiler/aihc/tree/main/components/aihc-parser#readme) - [Supported extensions](https://github.com/ai-haskell-compiler/aihc/blob/main/docs/aihc-parser-supported-extensions.md)
- [**aihc-resolve** README](https://github.com/ai-haskell-compiler/aihc/tree/main/components/aihc-resolve#readme) - [Supported extensions](https://github.com/ai-haskell-compiler/aihc/blob/main/docs/aihc-resolve-supported-extensions.md)
- [**aihc-tc** README](https://github.com/ai-haskell-compiler/aihc/tree/main/components/aihc-tc#readme) - [Supported extensions](https://github.com/ai-haskell-compiler/aihc/blob/main/docs/aihc-tc-supported-extensions.md)

## Progress

| Name               | Progress                                                                                                                             |
| ------------------ | -----------------------------------------------------------------------------------------------------------------------------------: |
| TypeCheck Stackage | `0/3390` (`0.00%`) ○○○○○                                                                                                              |
| Resolve Stackage   | <!-- AUTO-GENERATED: START resolve-stackage-progress --> `160/3427` (`4.67%`) ○○○○○ <!-- AUTO-GENERATED: END resolve-stackage-progress -->  |
| Parser Stackage    | <!-- AUTO-GENERATED: START parser-stackage-progress --> `2937/2937` (`100.00%`) ●●●●● <!-- AUTO-GENERATED: END parser-stackage-progress --> |
| &nbsp; | &nbsp; |
| TypeCheck Tests    | <!-- AUTO-GENERATED: START tc-progress --> `24/34` (`70.58%`) ●●●○○ <!-- AUTO-GENERATED: END tc-progress -->                                |
| Resolve Tests      | <!-- AUTO-GENERATED: START resolve-progress --> `27/28` (`96.42%`) ●●●●○ <!-- AUTO-GENERATED: END resolve-progress -->                      |
| Parser Tests       | <!-- AUTO-GENERATED: START parser-progress --> `2315/2315` (`100.00%`) ●●●●● <!-- AUTO-GENERATED: END parser-progress -->                     |
| Lexer Tests        | <!-- AUTO-GENERATED: START lexer-progress --> `104/104` (`100.00%`) ●●●●● <!-- AUTO-GENERATED: END lexer-progress -->                        |
| CPP Tests          | <!-- AUTO-GENERATED: START cpp-progress --> `46/46` (`100.00%`) ●●●●● <!-- AUTO-GENERATED: END cpp-progress -->                            |

## Lines of code

<!-- AUTO-GENERATED: START line-counts -->
```
| Component          |   Code |   Tests |   Total |
|--------------------|--------|---------|---------|
| aihc-cpp           |   1823 |     790 |    2613 |
| aihc-fc            |   1218 |     637 |    1855 |
| aihc-parser        |  12907 |   17204 |   30111 |
| aihc-parser-compat |   1578 |     735 |    2313 |
| aihc-resolve       |   2138 |    1365 |    3503 |
| aihc-tc            |   2559 |    1056 |    3615 |
| **Total**          |  22223 |   21787 |   44010 |
```
<!-- AUTO-GENERATED: END line-counts -->

<!-- Both commands are broken atm
## Ways to contribute

- PRs accepted and bugs are welcomed.
- If you have spare inference, run this command to generate a prompt: `nix run .#aihc-dev -- parser-stackage-progress --prompt`
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
