# Loom IR

Loom is AIHC's continuation-passing intermediate representation. It is named
for the way control flow is woven from explicit continuations.

Loom sits after strict GRIN and before backend-specific lowering:

```text
Haskell -> surface AST -> resolved AST -> typed AST -> System FC -> GRIN -> Loom -> backend IR
```

The native ARM64 backend still consumes GRIN while Loom is being introduced.
The compiler nevertheless constructs and validates Loom on every compilation,
so moving a backend across the boundary will not change the language exposed by
the preceding stages.

## Syntax

A Loom program contains the same heap-layout declarations, globals, CAFs,
primitive declarations, and foreign-call descriptors as its source GRIN
program. Only function bodies change representation.

```text
function     ::= code f(value-parameters; return-continuation) = term

term         ::= letcont continuation = term in term
               | invoke operation -> continuation
               | continue continuation values
               | case value as variable of alternatives
               | store-rec bindings in term

continuation ::= k(value-parameters) = term
alternative  ::= constructor value-parameters -> term
               | literal -> term
               | _ -> term
```

Every continuation carries the flattened `RuntimeRep` layout of the values it
accepts. Function parameters remain ordinary GRIN register values. The return
continuation is a distinct continuation parameter, not an ordinary heap value.

`term` has no `return`, fall-through, or sequence constructor. `continue`
transfers directly to a continuation, and `invoke` transfers the result of an
operation to a continuation. Consequently every executable path names its next
control destination.

Local continuations are lexical. Their bodies may capture value variables and
outer continuation variables. A future continuation-closure-conversion pass
can make those captures explicit when required by a backend.

## GRIN-to-Loom translation

Write `C[e, k]` for lowering GRIN expression `e` with successor continuation
`k`.

```text
C[return xs, k]             = continue k xs

C[xs <- operation; body, k] =
  letcont k1(xs) = C[body, k]
  in invoke operation -> k1

C[xs <- nested; body, k]    =
  letcont k1(xs) = C[body, k]
  in C[nested, k1]

C[case x of alts, k]        = case x of map (C[alt, k]) alts

C[store-rec bindings; e, k] = store-rec bindings in C[e, k]
```

Each function result `RuntimeRep` is flattened using the existing GRIN calling
convention. That layout becomes the signature of the function's explicit
return continuation. A GRIN bind's variables determine the signature of the
local continuation introduced for its body.

## Static invariants

The Loom linter enforces:

- every referenced continuation is lexically bound;
- continuation identities cannot be redeclared in the same scope;
- a local continuation's parameter layout equals its declared signature;
- values passed by `continue` match the target continuation's signature; and
- an operation's result layout matches its successor's signature.

`throw` has no result layout because it never returns, so it may occur with any
successor signature. This keeps exceptional control explicit without claiming
that the successor is reachable.

The Haskell data type enforces the remaining control-flow invariant: it is not
possible to construct a Loom term that implicitly returns or falls through.

## Runtime boundary

Loom does not introduce scheduler requests such as `fork`, `yield`, or `delay`.
Those facilities can be ordinary Loom code built around captured/resumable
continuations. A later lowering may give a small set of continuation operations
target-specific representations after control flow and roots are explicit.

Runtime operations inherited from GRIN (`eval`, `apply`, `catch`, and similar
operations) remain visible operations at this boundary. They are intentionally
not part of Loom's control structure and can be replaced by ordinary Loom code
in subsequent whole-program transformations.

## Compiler artifact

`aihc compile SOURCE --keep-cps-ir` writes the validated textual form to
`OUTPUT.cps`. The file starts with `loom-ir 1`; this version identifies the
debugging format and permits deliberate syntax changes later.
