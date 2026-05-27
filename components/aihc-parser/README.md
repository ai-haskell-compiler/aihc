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
Haskell. The test suite checks both whether syntax is accepted and whether the
parsed result agrees with GHC after pretty-printing and reparsing.

For the current support overview, see
[aihc-parser-supported-extensions.md](../../docs/aihc-parser-supported-extensions.md).

## Caveats

`aihc-parser` is ready to try, but it is still young:

- It is not battle-tested in the way GHC's parser is.
- It is more lenient than GHC in some places. In other words, `aihc-parser` can
  sometimes accept programs that GHC rejects.
- It is slower than GHC's parser.

The compatibility target is still GHC: accepting less than GHC is a bug;
accepting more than GHC is currently a known caveat.

## What Gets Tested

The parser is tested with golden examples, GHC oracle checks, package fixtures,
and generated syntax. The main properties are:

- GHC compatibility: syntax accepted by GHC should parse with `aihc-parser`.
- Pretty-print + parse roundtrip: parsed syntax should pretty-print to Haskell
  that parses again.
- GHC AST fingerprint preservation: parser output should pretty-print into
  source that GHC reads as the same AST shape.
- Totality: parser, parenthesization, shorthand rendering, and pretty-printing
  should not throw exceptions on generated inputs.
- Parentheses stability: required parentheses should be inserted consistently,
  and running the parenthesization pass again should not keep changing the tree.
- Haskell2010 coverage: the Haskell2010 syntax corpus is expected to pass at
  100%.

## Tests

From the repository root:

```bash
cabal test -v0 all --test-options=--hide-successes
```

For the full local check used before commits:

```bash
just check
```

## Contributing Syntax Cases

If you find a GHC-compatible program that `aihc-parser` rejects, please turn it
into a regression test. The most useful reports include the source snippet, the
required parser flags, whether GHC accepts it, and the `aihc-parser` error
message or roundtrip mismatch.

The project is test-first. A compatibility bug is only fixed once the test suite
can keep it fixed :)
