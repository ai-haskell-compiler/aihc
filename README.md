[![User guide](https://img.shields.io/github/actions/workflow/status/ai-haskell-compiler/aihc/deploy-docs.yml?label=user%20guide)](https://ai-haskell-compiler.github.io/aihc/)
[![API docs](https://img.shields.io/github/actions/workflow/status/ai-haskell-compiler/aihc/deploy-docs.yml?label=API%20docs)](https://ai-haskell-compiler.github.io/aihc/api/)
[![Generated Reports](https://img.shields.io/github/actions/workflow/status/ai-haskell-compiler/aihc/generated-reports-update.yml?label=reports)](https://github.com/ai-haskell-compiler/aihc/actions/workflows/generated-reports-update.yml)

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
| TypeCheck Stackage | <!-- AUTO-GENERATED: START tc-stackage-progress --> `84/3427` (`2.45%`) ○○○○○ <!-- AUTO-GENERATED: END tc-stackage-progress -->             |
| Resolve Stackage   | <!-- AUTO-GENERATED: START resolve-stackage-progress --> `241/3427` (`7.03%`) ○○○○○ <!-- AUTO-GENERATED: END resolve-stackage-progress -->  |
| Parser Stackage    | <!-- AUTO-GENERATED: START parser-stackage-progress --> `2937/2937` (`100.00%`) ●●●●● <!-- AUTO-GENERATED: END parser-stackage-progress --> |
| aihc-prim / ghc-prim | <!-- AUTO-GENERATED: START ghc-prim-progress --> `52/3425` (`1.52%`) ○○○○○ <!-- AUTO-GENERATED: END ghc-prim-progress -->                    |
| aihc-base / base   | <!-- AUTO-GENERATED: START base-progress --> `405/10057` (`4.03%`) ○○○○○ <!-- AUTO-GENERATED: END base-progress -->                             |
| &nbsp; | &nbsp; |
| TypeCheck Tests    | <!-- AUTO-GENERATED: START tc-progress --> `29/39` (`74.35%`) ●●●○○ <!-- AUTO-GENERATED: END tc-progress -->                                |
| Resolve Tests      | <!-- AUTO-GENERATED: START resolve-progress --> `44/45` (`97.77%`) ●●●●○ <!-- AUTO-GENERATED: END resolve-progress -->                      |
| Parser Tests       | <!-- AUTO-GENERATED: START parser-progress --> `2373/2373` (`100.00%`) ●●●●● <!-- AUTO-GENERATED: END parser-progress -->                     |
| Lexer Tests        | <!-- AUTO-GENERATED: START lexer-progress --> `107/107` (`100.00%`) ●●●●● <!-- AUTO-GENERATED: END lexer-progress -->                        |
| CPP Tests          | <!-- AUTO-GENERATED: START cpp-progress --> `46/46` (`100.00%`) ●●●●● <!-- AUTO-GENERATED: END cpp-progress -->                            |

## Lines of code

<!-- AUTO-GENERATED: START line-counts -->
```
| Component    |   Code |   Tests |   Total |
|--------------|--------|---------|---------|
| aihc-amd64   |   2175 |     833 |    3008 |
| aihc-arm64   |   2066 |     917 |    2983 |
| aihc-fc      |   5860 |    1553 |    7413 |
| aihc-grin    |   5692 |    2516 |    8208 |
| aihc-llvm    |   1877 |     454 |    2331 |
| aihc-native  |   3312 |     389 |    3701 |
| aihc-resolve |   2444 |    1644 |    4088 |
| aihc-tc      |   8876 |    3423 |   12299 |
| aihc-wasm    |   2139 |     673 |    2812 |
| **Total**    |  34441 |   12402 |   46843 |
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
