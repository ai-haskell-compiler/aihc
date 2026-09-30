# Libraries

AIHC is a series of reusable libraries.
Two libraries have stable releases on Hackage.
Other projects can use them.

| Library | Purpose | Links |
| --- | --- | --- |
| `aihc-cpp` | A Haskell-aware C preprocessor. | [Hackage](https://hackage.haskell.org/package/aihc-cpp) · [GitHub](https://github.com/ai-haskell-compiler/aihc-cpp) |
| `aihc-parser` | A Haskell parser with a lexer, a syntax tree, and a pretty-printer. | [Hackage](https://hackage.haskell.org/package/aihc-parser) · [GitHub](https://github.com/ai-haskell-compiler/aihc-parser) |

The Hackage pages give the API documentation of each release.

## Libraries without a stable release

The name resolver `aihc-resolve` and the type checker `aihc-tc` live in the [aihc repository](https://github.com/ai-haskell-compiler/aihc/tree/main/components).
Their interfaces change often.
They do not have a Hackage release.
Releases will come later.

## Internal libraries

The `aihc` package contains the desugarer, the GRIN back end, and the code generators.
This package is internal to the compiler.
Do not depend on it from another project.
