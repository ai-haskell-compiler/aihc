# aihc-resolve

**Status:** Proposed
**Scope:** `aihc-resolve`

## Problem

AIHC needs a dedicated name-resolution component that captures the name
resolution rules of Haskell 2010 plus supported Haskell language extensions. Today the
parser produces `[Module]` from `Aihc.Parser.Syntax`, but later phases need a
resolved view of the same program: each definition site should have a unique
identity, and each use site should point at the definition it refers to.

This component must work on groups of modules, not one module at a time.
Recursive modules are a supported use case, and AIHC should not require
GHC-style boot files in order to resolve them.

The resolver must also account for language features where surface syntax
implicitly refers to names. Extensions such as `RebindableSyntax`,
`OverloadedLists`, and `OverloadedStrings` materially change how those
implicit names are resolved.

## Goals

- Implement Haskell 2010 name-resolution rules.
- Extend those rules with supported Haskell language extensions that affect name
  resolution.
- Resolve mutually recursive groups of modules without boot files.
- Preserve the parser AST and only add annotations.
- Resolve both explicit occurrences and names referenced implicitly by
  surface syntax.
- Support human-verifiable golden tests and machine-readable annotation
  assertions.
- Track resolver progress both by targeted tests and by real package-level
  coverage.

## Non-goals

- No desugaring.
- No AST rewriting beyond adding annotations.
- No type inference or type checking.
- No requirement for boot files to break recursive module cycles.

## Overview

`aihc-resolve` takes `[Module]` from `Aihc.Parser.Syntax` and returns the same
modules with annotations attached to the existing syntax tree.

```
           parser
source ─────────────> [Module]
                          |
                          |  aihc-resolve
                          v
                [Module with annotations]
```

The output annotations are expected to include node-specific payloads such as
`TAnn`, `EAnn`, `DeclAnn`, and related annotation types.

The guiding principle is simple: the resolver should preserve the original
source structure and source spans while enriching the tree with binding and
reference information.

## Resolution Unit: Module Groups

The resolution unit is a group of modules, not a single module.

This is required to support recursive modules without boot files. The resolver
should therefore:

- accept a collection of modules together
- build the inter-module dependency graph
- resolve strongly connected groups as a unit
- allow references across modules in the same recursive component

Parsing remains per-module, but renaming is defined over a module group.

## Core Responsibility

The resolver assigns each definition site a unique identity and attaches that
identity to each use site that resolves to it.

This includes:

- top-level bindings
- local bindings
- imported names
- exported names
- type constructors, data constructors, classes, methods, and other
  namespace-specific entities
- names referenced implicitly by surface syntax

The resolver should also record unresolved and ambiguous occurrences so later
phases can report precise diagnostics without repeating scope analysis.

## Annotation Model

The annotation scheme should be rich enough to distinguish:

- definition sites
- use sites
- unresolved names
- ambiguous names
- implicit names introduced by syntax

The exact shape of `TAnn`, `EAnn`, `DeclAnn`, and related types is still open,
but they should be designed around a common identity model.

### Definition identities

Each definition site should get a unique identifier.

For top-level definitions, a stable identifier can be derived from:

- package id
- module name
- identifier
- namespace, if needed for disambiguation

For local definitions, the resolver should mint a fresh unique id, for example
using a resolver-generated integer supply.

This yields stable, globally meaningful ids for top-level bindings and unique
local ids for lexical binders.

### Use-site annotations

Each occurrence site should record the unique identifier of the definition it
resolves to.

That makes later passes simpler: once a program has been renamed, downstream
phases can work from identities rather than textual names and local scope
lookups.

## Implicit Names and Extension-Sensitive Resolution

`aihc-resolve` must resolve not only explicit identifiers but also names that
surface syntax refers to implicitly.

This is especially important because several extensions change which names are
looked up and where they are expected to come from.

Examples include:

- `RebindableSyntax`
- `OverloadedLists`
- `OverloadedStrings`

Examples of implicit names that may need resolution include:

- `fromInteger`
- `fromRational`
- `fromString`
- `ifThenElse`
- `(>>=)`
- `(>>)`
- `fail`
- `fromList`
- `fromListN`
- `toList`
- `enumFrom`
- `enumFromThen`
- `enumFromTo`
- `enumFromThenTo`
- operators used implicitly in overloaded patterns such as `(==)`, `(-)`, and
  `(>=)`

The resolver must expose enough information for later desugaring, but it
should not perform the desugaring itself. A useful rule is: if the surface
syntax implies a function occurrence, the resolver should resolve that implied
occurrence and attach it as annotation.

## Prelude and Implicit Imports

The resolver must model implicit imports, especially `Prelude`, according to
Haskell 2010 and relevant extension behavior.

Important cases include:

- the default implicit `Prelude` import
- `NoImplicitPrelude`
- `RebindableSyntax` implying `NoImplicitPrelude`
- an explicit `import Prelude (...)` suppressing the implicit whole-module
  import
- custom modules named `Prelude`
- package-qualified imports that do not replace the implicit `Prelude` unless
  implicit prelude import is disabled

This behavior matters because many implicit names are expected to come from the
prelude environment unless extensions change the lookup rules.

## No Desugaring, but Desugaring-Friendly Output

`aihc-resolve` should not change the AST beyond adding annotations. In
particular, it should not rewrite `do`, literals, list notation, or any other
surface syntax into an explicitly desugared form.

However, the annotations should make later desugaring straightforward. If a
`do`-block implicitly depends on `(>>=)` and `(>>)`, or an overloaded string
depends on `fromString`, those resolved names should be available to the next
phase without having to rerun scope resolution.

## Testing

Testing should be centered on golden files.

Each golden test should contain:

- one or more input modules
- the annotated output in a human-readable format
- a machine-readable list of `(source span, annotation)` pairs
- the set of enabled extensions for the test case

Tests with no enabled extensions should count as `Haskell2010`.

### Human-readable golden output

The golden output should be easy to inspect and verify manually. For example:

```text
Input:  x = let y = 10 in y + y
Output: x = let y = 10 in y + y
        │       └─ 0      │   └─ 0
        └─ global         └─ 0
```

The exact printed representation is still undecided. In particular, it remains
open whether the display format should use:

- symbolic labels such as `global`
- numeric unique ids
- both symbolic names and unique ids

A mixed format is likely the most useful: stable symbolic names where possible,
with unique ids where disambiguation matters.

### Machine-readable annotation output

Each test should also include a structured representation of annotations,
covering at least:

- source span
- annotation kind
- resolved identifier
- whether the occurrence is explicit or implicit

This format supports precise regression testing and tool-driven inspection.

### Multi-module fixtures

Golden tests must support multiple modules in a single fixture so that import,
export, and recursive-module behavior can be tested directly.

## Tracking Progress

Resolver progress should be measured in two ways.

### Feature coverage

Track supported language features by aggregating test results by enabled
extension set.

Tests with no extensions enabled count toward Haskell 2010 coverage.

### Ecosystem coverage

Track how many Stackage packages can be successfully resolved.

This requires modeling packages such as `base` that are assumed to be
available, even when they are not part of the package set currently being
processed. In practice, this means maintaining synthetic or preloaded package
environments for foundational libraries.

This metric provides a useful real-world signal alongside focused regression
tests.

## Suggested Pipeline

A reasonable high-level pipeline is:

1. Collect module headers, imports, exports, fixities, and top-level binders.
2. Build the module dependency graph.
3. Resolve strongly connected module groups together.
4. Build global environments for packages, modules, and imports.
5. Build local lexical environments for nested scopes.
6. Resolve explicit occurrences.
7. Resolve implicit occurrences introduced by syntax and extensions.
8. Attach annotations and record unresolved or ambiguous occurrences.

This keeps `aihc-resolve` focused on binding structure, environments, and name
identities.

## Open Questions

- What exact annotation schema should be exposed on `TAnn`, `EAnn`,
  `DeclAnn`, and related types?
- Should printed golden annotations prefer symbolic names, numeric uniques, or
  both?
- How should unresolved and ambiguous names be represented in the annotated
  AST?
- How much namespace detail should be encoded in a top-level identifier?
- How should synthetic package environments for assumed libraries such as
  `base` be represented and versioned?
- Which extension set should be in scope for the initial implementation?

## Summary

`aihc-resolve` is the name-resolution pass for AIHC. It takes a group of
parsed modules and returns the same modules annotated with binding and use-site
information, implementing Haskell 2010 plus extension-specific resolution
rules. It does not desugar or rewrite the AST, but it must still resolve names
referenced implicitly by syntax such as literals, `do`, `if`, and overloaded
list and string constructs. The component is tested with multi-module golden
fixtures that provide both human-readable annotated output and machine-readable
span-to-annotation data, and progress is tracked through both extension
coverage and the number of real Stackage packages that can be successfully
resolved.
