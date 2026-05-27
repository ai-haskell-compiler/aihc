# aihc-parser

`aihc-parser` is a Haskell parser library. It parses modules, declarations,
expressions, patterns, and types into an AST, and can pretty-print that AST
back to source code.

Its goal is to accept exactly the same Haskell as GHC. There are currently no
known compatibility bugs; if you find one, please report it.

## A Quick Taste

```console
% echo 'main = putStrLn "hello world"' | aihc-dev parser
Module {[DeclValue (PatternBind (PVar "main") (EApp (EVar "putStrLn") (EString "hello world")))]}
```

It understands modern GHC syntax too:

```console
% echo 'x = (.f.g)' | aihc-dev parser -XOverloadedRecordDot
Module {[DeclValue (PatternBind (PVar "x") (EParen (EGetFieldProjection ["f", "g"])))]}
```

Use it for source inspection, syntax-aware rewriting, Haskell syntax
experiments, or compiler-adjacent tools that want a regular library API instead
of reaching into GHC internals.

## What It Supports

`aihc-parser` tracks GHC's parser behavior across Haskell2010 and modern
Haskell. The test suite checks both whether syntax is accepted and whether
`aihc-parser` builds the same AST that GHC does.

For the current support overview, see
[aihc-parser-supported-extensions.md](../../docs/aihc-parser-supported-extensions.md).

## Caveats

`aihc-parser` is ready to try, but it has not had anything like GHC's years of
production exposure.

The intended behavior is exact GHC compatibility. Rejecting code that GHC
accepts is a bug. Accepting code that GHC rejects is also a bug, but that class
of bug is harder to test exhaustively, so some cases almost certainly remain.

It is also slower than GHC's parser.

## What Gets Tested

The test suite uses golden examples, GHC oracle checks, package fixtures, and
generated syntax. The core properties are:

- `parse ∘ pretty = id`: pretty-printed ASTs parse back to the same
  `aihc-parser` AST.
- `to_ghc_ast ∘ parse = ghc_parse`: parsed ASTs convert to exactly the same
  GHC AST that `ghc-lib-parser` produces.
- `parse`, `pretty`, `parens`, and shorthand rendering do not throw on generated
  inputs.
