# aihc-parser

`aihc-parser` is a Haskell parser written in Haskell. It parses modules,
declarations, expressions, patterns, and types into a Haskell AST, and it can
pretty-print that AST back to source code.

The goal is simple: accept the same Haskell that GHC accepts. The parser is
intended to be 100% compatible with GHC, and there are currently no known
compatibility bugs. If you find one, that is a bug worth reporting.

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

Use it when you want a regular library API for source inspection,
syntax-aware rewriting, Haskell syntax experiments, or compiler-adjacent tools
without reaching into GHC internals.

## What It Supports

`aihc-parser` tracks GHC's parser behavior across Haskell2010 and modern
Haskell. The test suite checks both whether syntax is accepted and whether
`aihc-parser` builds the same AST that GHC does.

For the current support overview, see
[aihc-parser-supported-extensions.md](../../docs/aihc-parser-supported-extensions.md).

## Caveats

`aihc-parser` is ready to try, but it is still young:

- It is not battle-tested in the way GHC's parser is.
- It is more lenient than GHC in some places. In other words, `aihc-parser` can
  sometimes accept programs that GHC rejects.
- It is slower than GHC's parser.

The compatibility target is still GHC: accepting less syntax than GHC is a bug,
and accepting more syntax than GHC is also a bug. The latter is listed as a
caveat because it is difficult to test exhaustively, and there are likely still
bugs of that kind.

## What Gets Tested

The parser is tested with golden examples, GHC oracle checks, package fixtures,
and generated syntax. The main properties are:

- Roundtripping: parsed syntax should pretty-print to Haskell that parses back
  into the same `aihc-parser` AST.
- GHC compatibility: test cases are parsed with both `aihc-parser` and
  `ghc-lib-parser`; the `aihc-parser` AST is converted into GHC's AST and
  checked for exact equality with `ghc-lib-parser`'s result.
- Totality: parser, parenthesization, shorthand rendering, and pretty-printing
  should not throw exceptions on generated inputs.
- Parentheses stability: required parentheses should be inserted consistently,
  and running the parenthesization pass again should not keep changing the tree.
