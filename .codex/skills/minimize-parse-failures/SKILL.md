---
name: minimize-parse-failures
description: Reduce Haskell parser failures to minimal repros. Use when Codex needs to take a parse error from `hackage-tester`, a source file, or a failing fixture and shrink it to the smallest snippet that GHC accepts but `aihc-parser` rejects, while preserving the relevant language settings, token shape, and parser behavior needed for a regression test.
---

# Minimize Parse Failures

## Overview

Construct the smallest snippet that still demonstrates the parser gap. Keep the workflow oracle-driven: GHC must accept the snippet, and `aihc-parser` must still fail for the same underlying reason.

## Workflow

1. Start from the failing artifact.
2. Recover the active language edition and extensions.
3. Isolate the smallest enclosing declaration or expression.
4. Shrink aggressively while re-checking both parsers after each change.
5. Stop when removing any remaining part either makes GHC reject the snippet or makes `aihc-parser` stop exhibiting the target failure.

## Capture the Failure

Prefer the raw failing file and the exact parser output over a paraphrase.

If the failure comes from package testing, begin with:

```bash
cabal run exe:hackage-tester -v0 -- <package>
```

Record:

- The failing file path
- The first relevant parse error location
- The parser context line if present
- Whether there is a secondary parse error caused by layout recovery

Treat the first surprising token as the primary clue. Secondary errors are often fallout.

## Recover Language Settings

Check source pragmas first, then the package's `.cabal` file.

Look for:

- `default-language`
- `default-extensions`
- `other-extensions`
- File-local `{-# LANGUAGE #-}` pragmas

Preserve only settings needed for the reduced snippet. When a package uses an edition such as `GHC2021`, keep the edition pragma instead of expanding it into many extensions unless the reduction needs a narrower setting.

For this repo, a standalone repro usually starts as:

```haskell
{-# LANGUAGE GHC2021 #-}
```

Add individual pragmas such as `BlockArguments` only when they are not already implied by the edition or when the original source used them explicitly and the distinction matters.

## Build a Standalone Repro

Copy only the smallest region around the failing location into a scratch file. Prefer one declaration over a whole module. Delete imports, signatures, and bindings as soon as they stop being necessary.

Use placeholders freely:

- Replace bodies with `undefined`
- Replace subexpressions with variables or constructors
- Replace types with unconstrained variables
- Drop names that do not affect parsing

Keep only syntax that contributes to the parser shape. For parse failures, semantics do not matter unless they affect what GHC accepts.

## Validate Both Parsers

Check GHC first:

```bash
ghc -v0 -fno-code -ddump-parsed fail.hs 2> /dev/null
```

Return code `0` means the snippet is syntactically valid for GHC. `-ddump-parsed` is useful because it shows how GHC grouped the code after layout and extension handling.

Then check `aihc-parser`:

```bash
cat fail.hs | cabal run -v0 exe:aihc-parser
```

If the bug might be lexical rather than syntactic, inspect tokens with:

```bash
cat fail.hs | cabal run -v0 exe:aihc-parser -- --lex
```

Do not trust a reduction until both commands have been rerun after the last edit.

## Shrink Strategically

Prefer large deletions first, then local simplifications.

Good reductions:

- Remove surrounding declarations, exports, and module headers
- Inline away irrelevant calls and arguments
- Replace complex branches with `undefined`
- Collapse `do`, `case`, or `if` bodies to one line when layout is not the point
- Shorten patterns and binders while preserving the token sequence around the failure
- Rename constructors and variables to short placeholders

When the failure mentions a token class such as `TkTypeApp`, `;`, or `}`, preserve the nearby syntax that could cause that tokenization or parse path. Keep the token neighborhood intact even if the names become nonsense.

For layout-sensitive bugs, normalize indentation deliberately. A reduction that accidentally changes layout class is a different test case.

## Stopping Rule

Stop only when all three conditions hold:

- GHC still accepts the snippet
- `aihc-parser` still rejects it
- Further deletions change the acceptance behavior or remove the target parse shape

The final error text does not need to match character-for-character, but the repro should still exercise the same parser hole. If the failure migrates to a different construct, keep shrinking only if the new construct is clearly the same root cause.

## Output

Report the result as:

- The minimized snippet
- The required language pragmas or edition
- The GHC validation command and result
- The `aihc-parser` validation command and result
- One sentence describing the suspected parser gap

If working in `aihc`, prefer turning the repro into a fixture after minimization:

- Use `components/aihc-parser/test/Test/Fixtures/golden/` for direct parser fixtures
- Use `components/aihc-parser/test/Test/Fixtures/oracle/` when the point is "GHC accepts, parser should match oracle behavior"

Remember that oracle `pass` cases in this repo require both oracle acceptance and AST roundtrip-fingerprint agreement. A snippet that parses but roundtrips differently may need to start as `xfail`.

## Example Pattern

Given a failure inside a larger function:

1. Copy only the failing declaration into `fail.hs`.
2. Add the minimum edition or extensions from the package metadata.
3. Confirm GHC accepts the declaration.
4. Confirm `aihc-parser` still fails.
5. Replace irrelevant subexpressions with placeholders until only the triggering syntax remains.

Aim for a result shaped like:

```haskell
{-# LANGUAGE GHC2021, BlockArguments #-}
label f =
  catch f \err@(X j x) ->
    undefined
```

This is the right kind of end state: tiny, parser-focused, GHC-accepted, and still failing in `aihc-parser`.
