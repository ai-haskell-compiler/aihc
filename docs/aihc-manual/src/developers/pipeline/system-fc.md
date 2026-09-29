# System FC

System FC is the typed core language of AIHC.
The FC modules desugar the type-checked surface tree into System FC.
The optimizer works on System FC.
Then, the GRIN modules lower System FC to GRIN.

The code is in `bin/aihc/compiler/fc/src/Aihc/Fc/`.
The design document is [docs/system-fc.md](https://github.com/ai-haskell-compiler/aihc/blob/main/docs/system-fc.md).

## Properties

System FC is similar to Haskell, but it has no syntactic sugar and no ambiguity.

- Each binder has a type. A use of a binder has no type.
- Each type argument is explicit, for example `f @tInt`.
- Each class constraint is an explicit dictionary argument.
  A class becomes a data type with the name `$Dict$Class`.
- A `case` expression has a case binder and a result type.
- A newtype becomes a type and an axiom.
  A cast `e ▷ γ` changes the type of an expression with a coercion `γ`.
- Types and kinds are one language.
  The function arrow `→` is `FUN` on lifted types.
- The FC lint checks the types of a program again.
  Use `--lint` to run it.

## Names

Each name has a scope number and a prefix.
The scope table at the start of a file gives the package and the module of each scope number.
The prefix gives the sort of the name.

| Prefix | Sort | Example |
| --- | --- | --- |
| `t` | Type constructor | `12.tInt` |
| `s` | Type synonym | `12.sType` |
| `c` | Data constructor | `12.cI#` |
| `v` | Value | `5.vprint` |

A local binder has no prefix.
If a local binder has the same name as an outer binder, the printer adds a number, for example `x{1}`.

## Top-level forms

| Form | Meaning |
| --- | --- |
| `scope N = "package" Module` | A scope number for the names of one module. |
| `import headers` | The types of the imported names that the module uses. |
| `type T :: kind { constructors }` | A data type and its constructors. |
| `val x :: type = expression` | A top-level value. |
| `pub` | The module exports the declaration. |
| `axiom` | An axiom, for example for a newtype or a type family instance. |

## Example

This module has a recursive function on lists:

```haskell
--8<-- "03-recursion/Example.hs"
```

The compiler makes this System FC program for the module:

```aihc-fc
--8<-- "03-recursion/core"
```

--8<-- "settings.md"
The page [Examples](examples.md) shows more programs.
