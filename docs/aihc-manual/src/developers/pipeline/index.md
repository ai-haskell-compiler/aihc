# Compiler pipeline

The compiler is a sequence of components.
Each component controls one domain.
No two components share a domain.

## Stages

| Stage | Component | Input | Output |
| --- | --- | --- | --- |
| Preprocess | `aihc-cpp` | Haskell source with CPP directives | Haskell source |
| Parse | `aihc-parser` | Haskell source | Surface syntax tree |
| Resolve | `aihc-resolve` | Parsed surface modules | The same tree with binding resolution, use resolution, exports, and resolve diagnostics |
| Type check | `aihc-tc` | Resolved surface tree | The same tree with types, kinds, evidence, and type diagnostics |
| Desugar | `aihc` FC modules | Type-checked surface tree | System FC program and System FC diagnostics |
| Lower | `aihc` GRIN modules | System FC program | Strict GRIN program and GRIN diagnostics |
| Generate | `aihc` Lir modules and back ends | GRIN program | Lir, then native code, LLVM IR, or WebAssembly |

`aihc-cpp` and `aihc-parser` live in their own repositories.
The other components live in the [aihc repository](https://github.com/ai-haskell-compiler/aihc).

## Boundaries

Each component does the work of its own domain only.

- `aihc-resolve` does name resolution only.
- `aihc-tc` does Haskell type checks only. It does not do name resolution.
- The FC modules desugar to System FC. They do not do Haskell type checks. They can lint types that are already in System FC.
- The GRIN modules do closure conversion, run-time operations, and GRIN transformations. They keep the semantics that System FC gives. They can remove types and coercions. They do not reconstruct Haskell type information.

If a downstream component needs an upstream fact, change the upstream component.
Then, use its output in the downstream component.

## Intermediate languages

| Language | Purpose |
| --- | --- |
| [System FC](system-fc.md) | The typed core language. The optimizer works on this language. |
| [GRIN](grin.md) | A strict, first-order language with explicit heap operations. |
| [Lir](lir.md) | A low-level language close to machine code. The back ends consume it. |

The page [Examples](examples.md) shows each intermediate program of some small Haskell programs.
The manual build makes these programs with the current compiler.

Use `--keep-core`, `--keep-grin`, and `--keep-lir` to inspect the intermediate programs of a build.
See [Debug options](../../users/command-line.md#debug-options).
