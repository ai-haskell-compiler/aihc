---
name: find-minimal-test-case
description: Reduce Haskell parser failures to minimal repros. Use when Codex needs to take a parse error from `aihc-dev hackage-tester`, `aihc-dev snippet`, a source file, or a failing fixture and shrink it to the smallest snippet that GHC accepts but `aihc-parser` rejects, while preserving the relevant language settings, token shape, and parser behavior needed for a regression test.
---

# Minimize Parse Failures

## Overview

Construct the smallest snippet that still demonstrates the parser gap. Keep the
workflow oracle-driven: GHC must accept the snippet, and `aihc-parser` must still
fail for the same underlying reason.

Use `aihc-dev snippet` as the main reduction loop. It runs the GHC oracle and
`aihc-parser` with the same source and extension settings, then also reports
roundtrip and `Parens.addModuleParens` mismatches when both parsers accept the
snippet.

## Workflow

1. Start from the failing artifact.
2. Recover the active language edition and extensions.
3. Isolate the smallest enclosing declaration or expression.
4. Shrink aggressively while re-running `aihc-dev snippet` after each change.
5. Stop when removing any remaining part either makes GHC reject the snippet or
   makes `aihc-parser` stop exhibiting the target failure.

## Capture the Failure

Prefer the raw failing file and the exact parser output over a paraphrase.

If the failure comes from package testing, begin with:

```bash
cabal run -v0 exe:aihc-dev -- hackage-tester <package>
```

Record:

- The failing file path
- The first relevant parse error location
- The parser context line if present
- Whether there is a secondary parse error caused by layout recovery

Treat the first surprising token as the primary clue. Secondary errors are often
fallout.

## Recover Language Settings

Check source pragmas first, then the package's `.cabal` file.

Look for:

- `default-language`
- `default-extensions`
- `other-extensions`
- File-local `{-# LANGUAGE #-}` pragmas

Preserve only settings needed for the reduced snippet. When a package uses an
edition such as `GHC2021`, pass that edition directly instead of expanding it
into many extensions unless the reduction needs a narrower setting.

For `aihc-dev snippet`, use file-local pragmas or `-X` flags:

```bash
cabal run -v0 exe:aihc-dev -- snippet -XGHC2021 fail.hs
cabal run -v0 exe:aihc-dev -- snippet -XBlockArguments fail.hs
```

The snippet command defaults to `Haskell2010` when no edition is provided. It
also reads `LANGUAGE` pragmas from the snippet itself, so a standalone repro can
usually keep the same pragmas that will later go into a fixture.

## Build a Standalone Repro

Copy only the smallest region around the failing location into a scratch file.
Prefer one declaration over a whole module. Delete imports, signatures, and
bindings as soon as they stop being necessary.

Use placeholders freely:

- Replace bodies with `undefined`
- Replace subexpressions with variables or constructors
- Replace types with unconstrained variables
- Drop names that do not affect parsing

Keep only syntax that contributes to the parser shape. For parse failures,
semantics do not matter unless they affect what GHC accepts.

## Validate With `aihc-dev snippet`

Run the snippet command after every edit:

```bash
cabal run -v0 exe:aihc-dev -- snippet fail.hs
```

For one-off reductions, piping source is often faster than managing a scratch
file:

```bash
printf '%s\n' \
  'f = x' \
  '  where' \
  '  a :$ b `g` c | True = x' \
  | cabal run -v0 exe:aihc-dev -- snippet
```

Interpret the result as follows:

- Exit code `0`: GHC accepts, `aihc-parser` accepts, and no validation mismatch
  was found.
- `Bug found: code rejected by aihc-parser but parsed by GHC.`: this is the
  target shape for a parser rejection repro.
- `Snippet fails to parse with both GHC and aihc-parser.`: the reduction is too
  aggressive or missing language settings.
- `Bug found: code rejected by GHC but parsed by aihc-parser.`: the parser is
  accepting too much; keep the snippet only if that is the bug being reduced.
- Roundtrip or `Parens.addModuleParens` output: both parsers accepted the input,
  but a later validation step found a different bug. Decide whether that is a
  separate issue or the actual target.

If the bug might be lexical rather than syntactic, inspect tokens with:

```bash
cat fail.hs | cabal run -v0 exe:aihc-dev -- parser --lex
```

Use raw `ghc -v0 -fno-code -ddump-parsed fail.hs` only when you need to inspect
GHC's grouping. It is no longer necessary for the ordinary accept/reject loop
because `aihc-dev snippet` already performs the GHC oracle check.

## Shrink Strategically

Prefer large deletions first, then local simplifications.

Good reductions:

- Remove surrounding declarations, exports, and module headers
- Inline away irrelevant calls and arguments
- Replace complex branches with `undefined`
- Collapse `do`, `case`, or `if` bodies to one line when layout is not the point
- Shorten patterns and binders while preserving the token sequence around the failure
- Rename constructors and variables to short placeholders

When the failure mentions a token class such as `TkTypeApp`, `;`, or `}`, preserve
the nearby syntax that could cause that tokenization or parse path. Keep the token
neighborhood intact even if the names become nonsense.

For layout-sensitive bugs, normalize indentation deliberately. A reduction that
accidentally changes layout class is a different test case.

## Stopping Rule

Stop only when all three conditions hold:

- `aihc-dev snippet` still reports that GHC accepts and `aihc-parser` rejects
  the snippet
- The first surprising token or parser context still points at the target
  construct
- Further deletions change the acceptance behavior or remove the target parse
  shape

The final error text does not need to match character-for-character, but the
repro should still exercise the same parser hole. If the failure migrates to a
different construct, keep shrinking only if the new construct is clearly the
same root cause.

## Case Study: `speculate`

The `speculate` package produced:

```text
PARSE_ERROR: .../Test/Speculate/Reason/Order.hs
  Order.hs:62:20:
  62 |   ef :$ (eg :$ ex) `fn` ey | isVar ey && ef == eg = fn (eg :$ ex) ey
     |                    ^
  unexpected '`'
  expecting = or guarded right-hand side
```

The package uses `default-language: Haskell2010`, so no extra extension flag was
needed. The important clue was that the parser expected an equation RHS before
the backtick. That means it had treated `ef :$ (eg :$ ex)` as a complete local
pattern binding instead of continuing to parse the backticked function head.

The reduction loop was:

```bash
printf '%s\n' \
  'f = x' \
  '  where' \
  '  a :$ b `g` c | True = x' \
  | cabal run -v0 exe:aihc-dev -- snippet
```

Before the parser fix, that snippet was accepted by GHC and rejected by
`aihc-parser`. Token inspection showed the lexer produced the expected
`TkSpecialBacktick` tokens, so the bug was in declaration parsing rather than
lexing.

The root cause was that top-level declarations used the full `patternParser` for
infix function-head operands, while local declarations used the narrower
`asOrAppPatternParser`. Local function heads therefore could not use an infix
constructor pattern, such as `a :$ b`, as the left operand of a backticked
function definition. The parser fix is to make local function declarations use
the same full-pattern operand parser as top-level value declarations.

## Output

Report the result as:

- The minimized snippet
- The required language pragmas, edition, or `-X` flags
- The `aihc-dev snippet` command and result
- Any token inspection or `-ddump-parsed` command used for diagnosis
- One sentence describing the suspected parser gap

If working in `aihc`, prefer turning the repro into a fixture after
minimization:

- Use `components/aihc-parser/test/Test/Fixtures/golden/` for direct parser fixtures
- Use `components/aihc-parser/test/Test/Fixtures/oracle/` when the point is
  "GHC accepts, parser should match oracle behavior"

Remember that oracle `pass` cases in this repo require both oracle acceptance
and AST roundtrip-fingerprint agreement. A snippet that parses but roundtrips
differently may need to start as `xfail`.
