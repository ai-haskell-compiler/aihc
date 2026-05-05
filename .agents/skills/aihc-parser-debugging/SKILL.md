---
name: aihc-parser-debugging
description: Debug `aihc-parser` parse, pretty-printer, paren-insertion, and QuickCheck round-trip failures using `aihc-dev snippet` to distinguish invalid generated code from parser regressions.
---

# AIHC Parser Debugging

Use this when investigating `aihc-parser` failures from QuickCheck, oracle cases,
golden fixtures, hackage-tester, or hand-written snippets.

## First Principle

Do not assume the failing source is valid Haskell. Establish that first with
`aihc-dev snippet`.

```bash
printf '%s\n' 'x = 1' | cabal run -v0 exe:aihc-dev -- snippet
```

Pass any required extensions explicitly:

```bash
printf '%s\n' 'x = if | True -> 0' \
  | cabal run -v0 exe:aihc-dev -- snippet -XMultiWayIf
```

`aihc-dev snippet` compares GHC, `aihc-parser`, and the paren/pretty round trip.
Use its classification to decide where the bug probably lives.

## Possible Bug Sources

### QuickCheck Generator

The generator builds ASTs; it is not proof that the pretty-printed source is
valid Haskell. Some generated ASTs represent combinations GHC rejects.

Example workflow:

1. Copy the `Original source:` from the failing QuickCheck output.
2. Run it through `aihc-dev snippet` with the same extensions as the property.
3. If GHC rejects it and `aihc-parser` also rejects it, the generator may have
   produced an invalid case. Fix the generator or shrinker unless the property
   should exclude that AST shape.

Example invalid-vs-valid check:

```bash
# Invalid: the typed generator RHS is not parenthesized before the guard arrow.
printf '%s\n' \
  '(# | | [] | #) =' \
  '  if | (# #) <- ()' \
  '      :: (# (# [a||] | C #) | * #)' \
  '      -> 0' \
  | cabal run -v0 exe:aihc-dev -- snippet \
      -XUnboxedSums -XUnboxedTuples -XMultiWayIf -XQuasiQuotes

# Valid: the typed generator RHS is parenthesized.
printf '%s\n' \
  '(# | | [] | #) =' \
  '  if | (# #) <- (() :: (# (# [a||] | C #) | * #))' \
  '      -> 0' \
  | cabal run -v0 exe:aihc-dev -- snippet \
      -XUnboxedSums -XUnboxedTuples -XMultiWayIf -XQuasiQuotes
```

### `Aihc.Parser.Parens`

`Parens` is a best-effort pass that inserts explicit grouping into an AST before
pretty-printing. It may add too few parens, add too many parens, or change parse
meaning in a context-sensitive grammar position.

`aihc-dev snippet` reports this directly when `Parens.addModuleParens` changes a
parsed snippet. Treat that as a parens bug unless the original snippet is invalid
and the property should never have generated it.

Typical parens bugs:

- Missing parens before a guard arrow, such as `expr :: T -> body`.
- Missing parens around block arguments at the right edge of an infix expression.
- Extra parens that make an oracle fingerprint change.

Prefer fixing the smallest context rule in `components/aihc-parser/src/Aihc/Parser/Parens.hs`.

### Pretty-Printer

The pretty-printer can emit invalid or meaning-changing syntax even when the AST
has the right paren nodes.

Suspect the pretty-printer when:

- The AST after `add*Parens` is correct, but rendered layout or token spelling is
  invalid.
- GHC accepts a compact spelling but rejects the pretty-printed layout.
- The issue depends on whitespace-sensitive syntax, virtual layout, quasiquotes,
  record braces, or symbolic names.

Use `aihc-dev snippet` on both the original and a hand-adjusted pretty-printed
variant. If adding only whitespace or changing line breaks flips the result, look
in `components/aihc-parser/src/Aihc/Parser/Pretty.hs`.

### Parser

The parser is probably wrong when GHC accepts the snippet and `aihc-parser`
rejects it, or when `aihc-parser` accepts a snippet GHC rejects.

Examples:

```bash
# Parser false negative: GHC accepts, aihc-parser rejects.
cabal run -v0 exe:aihc-dev -- snippet -XSomeExtension valid-ghc-snippet.hs

# Parser false positive: GHC rejects, aihc-parser accepts.
cabal run -v0 exe:aihc-dev -- snippet invalid-ghc-snippet.hs
```

For parser fixes, add an oracle fixture when validity matters:

```haskell
{- ORACLE_TEST pass -}
{-# LANGUAGE SomeExtension #-}
module Regression where

-- Minimal GHC-accepted case here.
```

Put oracle fixtures under
`components/aihc-parser/test/Test/Fixtures/oracle/<Feature>/name.hs`.

## Debugging Checklist

1. Reproduce the failure exactly, including `--quickcheck-replay` when present.
2. Copy the smallest source into `aihc-dev snippet` with required `-X` flags.
3. Classify the result:
   - GHC rejects, AIHC rejects: generator or invalid test input.
   - GHC rejects, AIHC accepts: parser false positive.
   - GHC accepts, AIHC rejects: parser false negative.
   - Both accept, round trip changes: `Parens` or pretty-printer.
4. If both accept, compare the original source, `addModuleParens` output, and
   pretty-printed output from the snippet report.
5. Add the narrowest regression:
   - Oracle fixture for GHC validity or parser compliance.
   - Golden fixture for AST-shape expectations only.
   - QuickCheck generator/shrinker change when the property produces invalid
     ASTs.
6. Rerun the targeted test, then `just fmt` and `just check` before committing.

