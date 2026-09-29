# GRIN

GRIN (Graph Reduction Intermediate Notation) is a strict, first-order language with explicit heap operations.
The GRIN modules lower System FC to GRIN.
Then, they lower GRIN to Lir.

The code is in `bin/aihc/compiler/grin/src/Aihc/Grin/`.
The design document is [docs/cps-grin.md](https://github.com/ai-haskell-compiler/aihc/blob/main/docs/cps-grin.md).

## Properties

GRIN makes the lazy evaluation of Haskell explicit.

- Each lazy expression becomes a thunk. `store` puts the thunk on the heap.
- `eval` evaluates a heap value to weak-head normal form.
- `apply` calls an unknown function with its arguments.
- A local function becomes a top-level function. Its free variables become arguments.
- GRIN has no Haskell types.
  Each variable has a runtime representation, for example `BoxedRep Lifted` or `IntRep`.

The GRIN modules keep the semantics of the System FC program.
They remove types and coercions.
They do not reconstruct Haskell type information.

## Nodes

A node is an object on the heap.
The first letter of a node tag gives the sort of the node.

| Tag | Node | Example |
| --- | --- | --- |
| `C` | A data constructor and its fields | `(C7.IS (1 :: IntRep))` |
| `F` | A thunk: a function with no arguments that makes the value | `(F9.$main_thunk)` |
| `P` | A partial application: a function and the number of arguments that it needs | `(P9.$square/1)` |

## Stages

The GRIN modules transform a program in a sequence of stages.
`--keep-grin` keeps the program of each stage.

| Stage | File | Content |
| --- | --- | --- |
| GRIN | `grin` | The program after the lowering from System FC and the GRIN simplifier. |
| CPS-GRIN | `cps.grin` | Each continuation is a function. Its frame is on the stack of the Haskell thread. |
| GC-GRIN | `gc.grin` | Each allocation has a heap check, and the live roots are explicit. |

The Lir lowering takes the GC-GRIN program.
This manual shows the first stage only.

## Example

This program uses type classes:

```haskell
--8<-- "01-type-classes/Main.hs"
```

The compiler makes this GRIN program for the module `Main`:

```text
--8<-- "01-type-classes/grin"
```

--8<-- "settings.md"
The page [Examples](examples.md) shows more programs.
