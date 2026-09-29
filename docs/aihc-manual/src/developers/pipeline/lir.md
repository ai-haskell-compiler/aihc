# Lir

Lir is the low-level intermediate language of AIHC.
The module `Aihc.Lir.Lower` lowers GC-GRIN to Lir.
Each back end takes Lir as its input.

The code is in `bin/aihc/compiler/lir/`.
The specification is [docs/lir.md](https://github.com/ai-haskell-compiler/aihc/blob/main/docs/lir.md).

## Properties

- Lir is a control-flow graph in static single assignment form.
- A block has parameters. Lir has no phi instructions.
- Each operation has a defined result. An operation that has no valid result traps.
- Lir has no garbage collector and no exception handler. GRIN makes both explicit before the Lir lowering.
- Lir has no hidden state. The runtime registers are declared globals or function parameters.
- The text format and the in-memory representation have the same information.
  The printer and the parser round-trip each module.

Parts of the runtime system are also written in Lir.
Thus, the optimizer can see user code and runtime code as one program.

## Names and types

| Form | Example | Meaning |
| --- | --- | --- |
| `%name` | `%acc` | A value in a function |
| `@name` | `@aihc_lir_eval` | A symbol: a function, a global, or a data object |
| `name` | `eval_slow_9` | A block label in a function |

| Type | Meaning |
| --- | --- |
| `i1` | A boolean |
| `i8`, `i16`, `i32`, `i64` | An integer with no sign. Each operation gives the sign that it uses. |
| `f32`, `f64` | An IEEE 754 binary float |
| `ptr` | The address of data |
| `code` | The address of a function |

## Back ends

| Target | Back end | Output |
| --- | --- | --- |
| `apple-arm64` | `Aihc.Arm64.Lir` | Mach-O objects |
| `linux-amd64` | `Aihc.Amd64.Lir` | ELF objects |
| `llvm` | `Aihc.Llvm.Lir` | Textual LLVM IR |
| `wasm32-wasip3` | `Aihc.Wasm.Lir` | WebAssembly for WASI P3 |

`aihc build --keep-lir` writes the Lir of each module to `<Module>.o.lir` beside its object.

## Example

This module has a recursive function on lists:

```haskell
--8<-- "03-recursion/Example.hs"
```

The compiler makes this Lir program for the module:

```text
--8<-- "03-recursion/lir"
```

--8<-- "settings.md"
The page [Examples](examples.md) shows more programs.
