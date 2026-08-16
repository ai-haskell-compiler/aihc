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
| Resolve Stackage   | <!-- AUTO-GENERATED: START resolve-stackage-progress --> `247/3427` (`7.21%`) ○○○○○ <!-- AUTO-GENERATED: END resolve-stackage-progress -->  |
| aihc-prim / ghc-prim | <!-- AUTO-GENERATED: START ghc-prim-progress --> `58/3425` (`1.69%`) ○○○○○ <!-- AUTO-GENERATED: END ghc-prim-progress -->                    |
| aihc-base / base   | <!-- AUTO-GENERATED: START base-progress --> `505/10057` (`5.02%`) ○○○○○ <!-- AUTO-GENERATED: END base-progress -->                             |
| &nbsp; | &nbsp; |
| TypeCheck Tests    | <!-- AUTO-GENERATED: START tc-progress --> `29/39` (`74.35%`) ●●●○○ <!-- AUTO-GENERATED: END tc-progress -->                                |
| Resolve Tests      | <!-- AUTO-GENERATED: START resolve-progress --> `50/51` (`98.03%`) ●●●●○ <!-- AUTO-GENERATED: END resolve-progress -->                      |

## Lines of code

<!-- AUTO-GENERATED: START line-counts -->
```
| Component    |   Code |   Tests |   Total |
|--------------|--------|---------|---------|
| aihc-amd64   |   2172 |     834 |    3006 |
| aihc-arm64   |   2063 |     921 |    2984 |
| aihc-fc      |   8664 |    2387 |   11051 |
| aihc-grin    |   5941 |    2811 |    8752 |
| aihc-llvm    |   1901 |     506 |    2407 |
| aihc-native  |   3405 |     472 |    3877 |
| aihc-resolve |   2532 |    1898 |    4430 |
| aihc-tc      |   9814 |    2593 |   12407 |
| aihc-wasm    |   2161 |     722 |    2883 |
| **Total**    |  38653 |   13144 |   51797 |
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
