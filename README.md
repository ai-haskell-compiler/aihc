[![User guide](https://img.shields.io/github/actions/workflow/status/ai-haskell-compiler/aihc/deploy-docs.yml?label=user%20guide)](https://ai-haskell-compiler.github.io/aihc/)
[![API docs](https://img.shields.io/github/actions/workflow/status/ai-haskell-compiler/aihc/deploy-docs.yml?label=API%20docs)](https://ai-haskell-compiler.github.io/aihc/api/)
[![Generated Reports](https://img.shields.io/github/actions/workflow/status/ai-haskell-compiler/aihc/generated-reports-update.yml?label=reports)](https://github.com/ai-haskell-compiler/aihc/actions/workflows/generated-reports-update.yml)

# AI-written Haskell Compiler (aihc)

Can gpt, Claude Opus and Qwen-Coder write a Haskell compiler? Probably not but let's find out. We'll need preprocessing, parsing, name resolution, type checking, desugaring, and code generation. Progress will be tracked as a percentage of stackage for each component.

Find more information here:
- [**aihc-cpp** README](https://github.com/ai-haskell-compiler/aihc-cpp#readme)
- [**aihc-parser** README](https://github.com/ai-haskell-compiler/aihc-parser#readme) - [Supported extensions](https://github.com/ai-haskell-compiler/aihc-parser/blob/main/docs/aihc-parser-supported-extensions.md)
- [**aihc-resolve** README](https://github.com/ai-haskell-compiler/aihc/tree/main/components/aihc-resolve#readme) - [Supported extensions](https://github.com/ai-haskell-compiler/aihc/blob/main/docs/aihc-resolve-supported-extensions.md)
- [**aihc-tc** README](https://github.com/ai-haskell-compiler/aihc/tree/main/components/aihc-tc#readme) - [Supported extensions](https://github.com/ai-haskell-compiler/aihc/blob/main/docs/aihc-tc-supported-extensions.md)

## Progress

| Name               | Progress                                                                                                                             |
| ------------------ | -----------------------------------------------------------------------------------------------------------------------------------: |
| TypeCheck Stackage | <!-- AUTO-GENERATED: START tc-stackage-progress --> `84/3427` (`2.45%`) ○○○○○ <!-- AUTO-GENERATED: END tc-stackage-progress -->             |
| Resolve Stackage   | <!-- AUTO-GENERATED: START resolve-stackage-progress --> `244/3427` (`7.12%`) ○○○○○ <!-- AUTO-GENERATED: END resolve-stackage-progress -->  |
| aihc-prim / ghc-prim | <!-- AUTO-GENERATED: START ghc-prim-progress --> `52/3425` (`1.52%`) ○○○○○ <!-- AUTO-GENERATED: END ghc-prim-progress -->                    |
| aihc-base / base   | <!-- AUTO-GENERATED: START base-progress --> `483/10057` (`4.80%`) ○○○○○ <!-- AUTO-GENERATED: END base-progress -->                             |
| &nbsp; | &nbsp; |
| TypeCheck Tests    | <!-- AUTO-GENERATED: START tc-progress --> `29/39` (`74.35%`) ●●●○○ <!-- AUTO-GENERATED: END tc-progress -->                                |
| Resolve Tests      | <!-- AUTO-GENERATED: START resolve-progress --> `47/48` (`97.91%`) ●●●●○ <!-- AUTO-GENERATED: END resolve-progress -->                      |

## Lines of code

<!-- AUTO-GENERATED: START line-counts -->
```
| Component    |   Code |   Tests |   Total |
|--------------|--------|---------|---------|
| aihc-amd64   |   2171 |     833 |    3004 |
| aihc-arm64   |   2062 |     920 |    2982 |
| aihc-fc      |   7863 |    2426 |   10289 |
| aihc-grin    |   5896 |    2808 |    8704 |
| aihc-llvm    |   1900 |     505 |    2405 |
| aihc-native  |   3404 |     471 |    3875 |
| aihc-resolve |   2546 |    1766 |    4312 |
| aihc-tc      |   9067 |    3442 |   12509 |
| aihc-wasm    |   2160 |     720 |    2880 |
| **Total**    |  37069 |   13891 |   50960 |
```
<!-- AUTO-GENERATED: END line-counts -->

## Useful Commands

Run the full test suite:

```
just check
```

Run the full test suite in a hermetic build environment (slower than `just check`):

```bash
nix flake check
```
