# System FC 2

Status: in progress.
Date: 2026-08-18.

This document is the plan for System FC 2.
The code lives in `components/aihc-fc/src/Aihc/Fc2/`.
The current System FC in `Aihc.Fc` stays in place.

System FC 2 is a Core language.
It is similar to Haskell.
It has no sugar.
It has no ambiguity.

## Goals

- Put a type on each binder.
- Put no type on a use.
- Use one namespace for types and values.
- Make the text readable for a person.
- Keep `Aihc.Fc` and its tests until the switch is complete.

## Non-goals

- Do not replace `Aihc.Fc` in this plan.
- Do not add join points.
- Do not add `INLINE`, demand, unfolding, or linear types.
- Do not add source spans in the first version.
- Do not add an evaluator in the first version.
- Do not add a CBOR schema in the first version.
- Do not point GRIN at Fc2 in this plan.

## Place in the compiler

```text
source
  -> aihc-resolve
  -> aihc-tc
  -> Aihc.Fc.Desugar     -> store .../core
  -> Aihc.Fc2.Desugar    -> store .../core-v2
  -> aihc-grin           (still reads Aihc.Fc)
```

`aihc-grin` still reads `Aihc.Fc`.
`core-v2` is for a person in this plan.

## Key decisions

1. One namespace.
   Collapse the type namespace and the value namespace.

2. Prefix marks are print and parse only.
   The AST stores the Haskell name plus a sort.
   Print `tBool` and `vFalse`.
   Do not store `tBool` as the real name.

3. Name equality uses origin, text, and name class.
   Type constructors and synonyms share the `t` class.
   Values and data constructors share the `v` class.
   So `t[]` is not `v[]`.

4. Types sit on binders only.

5. Local binders are non-recursive or a recursive group.
   A top-level `val` has no recursion mark.
   It may be recursive.

6. A local binder must not hide an outer binder.
   If the text name would hide, print `x{12}`.

7. `pub` means export.
   There is no `external` form.

8. One module file is not standalone.
   Parse accepts free `N.name` after a scope table.
   Later checks load the package or store.

9. Types and kinds are one language.
   `TYPE`, `RuntimeRep`, `Levity`, and `Constraint` are ordinary `GHC.Types` names.

10. The function arrow is `FUN` in the AST.
    `TyFun` always has four arguments.
    `forall` is a pi-binder.
    A later binder type may mention an earlier binder.

11. Print `a → b` when both stored representations are lifted.
    Parse `→` and `->` as `FUN` on `LiftedRep`.
    Print `FUN @r1 @r2 a b` when a side is not lifted.

12. Coercions stay a separate AST.
    First set: `CoVar`, `Refl`, `Sym`, `Trans`, `TyConAppCo`, `AxiomInstCo`, roles, and casts.
    Add a form only when a fixture needs it.

13. Each data constructor has a full GADT type.

14. Classes become `$Dict$Class` data types.
    Newtypes become a type plus a representational axiom.
    Type families become named axioms.

15. Data families become an empty type plus instance axioms.
    Omit `pub` to hide constructors.

16. Keep type synonym declarations.
    `Type` and `LiftedRep` stay as binders.

17. `a ~ b` is the one equality type.
    Axioms and type-constructor parameters carry roles.

18. `foreign import prim` is the only foreign form.
    If desugar sees `ccall`, it fails the module.

19. Recognise `GHC.Types` names by package and module identity.
    Do not add extra axioms for them.

20. Scope syntax stays: `scope 1 = "pkg-id" ModuleName`.

## Syntax

```text
scope 1 = "" Test
scope 2 = "aihc-prim" GHC.Types

pub type 1.tBool :: 2.tType {
    pub 1.vFalse :: 1.tBool
    pub 1.vTrue :: 1.tBool
}

pub val 1.vnot :: 1.tBool → 1.tBool
 = λ(x : 1.tBool).
     case x as (w{1} : 1.tBool) return (1.tBool) of {
       1.vTrue → 1.vFalse;
       1.vFalse → 1.vTrue
     }
```

Use `::` on declarations.
Use `:` on `λ`, `Λ`, and `∀` binders.
Use Core-style `case` with a case binder, a result type, and `_` for default.
Permit an empty alternative set when the case has an explicit result type.
Use `let` and `rec` for local groups.
Use `val` for top-level values.
Use `type T { cons }` for data types.
Use `axiom` for axioms.
Use `e ▷ γ` for cast.
`[]` and `:` are ordinary names.

## Names

| Sort | Prefix | Example print | Real name |
| --- | --- | --- | --- |
| Type constructor | `t` | `1.tBool` | `Bool` |
| Synonym | `t` | `2.tType` | `Type` |
| Data constructor | `v` | `1.vFalse` | `False` |
| Value | `v` | `1.vnot` | `not` |
| Type variable | none | `a`, `x{12}` | `a`, `x` |
| Axiom | none | `1.$ax$Age` | `$ax$Age` |

`aihc-tc` stores the list type as `[]`.
Print that type as `t[]` until `aihc-tc` stores `List`.

## FUN

`FUN` is an AST constructor.
It is not a scoped name.

A full `TyFun r1 r2 a b` has type `Type`.

Print `2.tFlag → 1.tBox` when both representations are lifted.
This includes an imported type.
Print explicit `FUN` only when a representation is not lifted.

## Coercions

A cast is legal when the term type is the left side of the coercion type.
The result type is the right side.

```text
Γ ⊢ e : τ1
Γ ⊢ γ : τ1 ~ τ2
--------------------
Γ ⊢ e ▷ γ : τ2
```

An axiom prints its role as `~N` or `~R`.

```text
axiom 1.$ax$Age : 1.tAge ~R 1.tInt
```

## Desugar

Read checked facts from `aihc-tc`.
Do not rebuild Haskell types in Fc2.

| Surface | Fc2 |
| --- | --- |
| Data type | `type T { cons }` with full constructor types |
| Type synonym | `type T :: k = rhs` |
| Class | `$Dict$Class` data type plus selectors |
| Newtype | empty type plus `~R` axiom |
| Type family | empty type plus named axioms |
| Data family | empty family type plus instance type and axiom |
| Value | `val` with no `rec` mark |
| `foreign import prim` | `foreign import prim` |
| `foreign import ccall` | fail the module |

`desugarModuleFc2` already emits data types, synonyms, values, and `foreign import prim`.

## Tests

Use test-first development.

Before desugar exists for a form, put Fc2 text in `test/Test/Fixtures/fc2/`.
After desugar exists, put Haskell fixtures in `test/Test/Fixtures/golden-v2/`.

Do not change `test/Test/Fixtures/golden/`.
Hedgehog may build an AST.
Do not add a `testCase` that builds `Expr`.

Hedgehog checks `parseProgram . renderProgram = id` on self-contained programs.

## install-v2

`install-v2` still writes `core`.
It must also write `core-v2` next to it:

```text
{store}/{pkg}-{version}-{dephash}/{Module/Path}/core-v2
```

Example: `Demo/A/core-v2`.

If Fc2 desugar fails, the install fails.
If Fc2 lint fails, the install fails.
Write `core-v2.bad` when Fc2 lint fails.
Do not write `core` without `core-v2`.
`install-v2` does not parse the `core-v2` file that it writes.
It may parse imported `core-v2` files through the store loader.

## PR plan

| PR | Title | Status |
| --- | --- | --- |
| 1 | `feat(fc2): add System FC 2 AST, pretty printer, and parser` | done, #1488 |
| 2 | `feat(fc2): desugar data types and synonyms to System FC 2` | done, #1489 |
| 3 | `feat(fc2): desugar values, lambda, application, and case` | done, #1490 |
| 4 | `feat(fc2): desugar classes to dictionary data types` | done, #1492 |
| 5 | `feat(fc2): desugar newtypes to types, axioms, and casts` | done, #1492 |
| 6 | `feat(fc2): desugar data families` | done, #1492 |
| 7 | `feat(fc2): desugar type families` | done, #1492 |
| 8 | `feat(fc2): accept foreign import prim and reject ccall` | done, #1493 |
| 9 | `feat(fc2): write core-v2 from install-v2` | done |
| 10 | `fix(fc2): correct System FC 2 output for aihc-prim` | done |
| 11 | `feat(fc2): add System FC 2 type linter` | done |

PR 4 depends on PR 3.
PR 5 depends on PR 3.
PR 6 depends on PR 5.
PR 7 depends on PR 6 and an `aihc-tc` equation store.
PR 8 depends on PR 3.
PR 9 depends on PR 4, PR 5, PR 6, and PR 8.
PR 10 depends on PR 9.

Do not invent type-family equations in Fc2.
Do not remove `Aihc.Fc`.

## Defaults

| Topic | Default |
| --- | --- |
| Axiom print | `axiom N.$ax$Name ∀(a : τ) : lhs ~N rhs` |
| Role list | After the kind as `@N @R @P`. Omit when all are `R`. |
| Data roles | All `Representational` until `aihc-tc` stores them |
| Family roles | All `Nominal` |
| `Constraint` | `GHC.Types.Constraint` with `primPackageId` |
| Parser | Hand-written Megaparsec in `Aihc.Fc2.Parser` |
| Lifted kind print | `Type` |

## References

- `components/aihc-fc/src/Aihc/Fc2/`
- `docs/compilation.md`
- `docs/system-fc-primer.md`
- `core-libs/aihc-prim/src/GHC/Types.hs`
