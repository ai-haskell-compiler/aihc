# aihc-parser Tests

Use `aihc-parser` tests for lexing, parsing, pretty-printing, error messages, GHC oracle compliance, compatibility, performance, and generative properties.

## Choose The Test

- Use parser golden fixtures when one source fragment should produce a precise shorthand AST or a precise parse failure.
- Use equivalent fixtures when multiple spellings should parse to the same normalized tree.
- Use lexer golden fixtures when tokenization, layout, pragma tokens, source directives, or extension-sensitive lexing is the contract.
- Use error-message fixtures when both GHC rejection and AIHC diagnostic rendering matter.
- Use oracle fixtures when GHC is the authority for accepting a full module and AIHC must also pass round-trip/fingerprint validation.
- Use unit tests in `components/aihc-parser/test/Spec.hs` for small internal behaviors, pretty-printer expectations, regression checks around shrunk examples, and assertions that need custom setup.
- Use QuickCheck properties under `components/aihc-parser/test/Test/Properties` for generators, round trips, paren insertion, no-exception guarantees, coverage, and idempotency.
- Use parser-compat tests under `components/aihc-parser-compat/test` when AIHC syntax conversion to normalized GHC AST is the contract.

## Parser Golden Fixtures

Root: `components/aihc-parser/test/Test/Fixtures/golden`.

Subdirectories select parser entry point:

- `expr`: `parseExpr`
- `module`: `parseModule`
- `pattern`: `parsePattern`

Fixture shape:

```yaml
extensions: []
input: |
  42
ast: |-
  EInt 42 TInteger
status: pass
reason: ""
```

Required keys: `extensions`, `input`, `status`. Supported statuses are `pass`, `fail`, and `xfail`; `xpass` is rejected. `pass` requires `ast`; `xfail` requires `reason`. `fail` means the parser should reject the input. The expected `ast` is `show (shorthand ast)`, so prefer generating it from the current parser rather than hand-formatting large trees.

## Equivalent Fixtures

Root: `components/aihc-parser/test/Test/Fixtures/equivalent`.

Subdirectories select parser entry point: `expr`, `module`, `decl`, `pattern`.

Fixture shape:

```yaml
extensions: []
equivalent:
  - a + b + c
  - (a + b) + c
status: pass
reason: parser matches GHC's left-associated parsed infix tree
```

Use at least two inputs. Supported statuses are `pass`, `fail`, and `xfail`; `xpass` is rejected. The runner parses every input, strips annotations, normalizes parens, and compares the trees.

## Lexer Golden Fixtures

Root: `components/aihc-parser/test/Test/Fixtures/lexer`.

Fixture shape:

```yaml
extensions: []
input: |
  x --> y
tokens:
  - 'TkVarId "x"'
  - 'TkVarSym "-->"'
  - 'TkVarId "y"'
  - 'TkEOF'
status: pass
```

Required keys: `extensions`, `input`, `tokens`, `status`. Supported statuses are `pass`, `xfail`, and `xpass`; `xfail` and `xpass` require `reason`. Fixtures whose relative path is under `module/` use `lexModuleTokensWithExtensions`; other lexer fixtures use `lexTokensWithExtensions`.

Token strings are read as `LexTokenKind` constructors. `TkPragma` fixtures omit `pragmaRawText`; the runner normalizes it away.

## Error-Message Fixtures

Root: `components/aihc-parser/test/Test/Fixtures/error-messages`.

Fixture shape:

```yaml
src: |
  x =
ghc: |
  test.hs:2:1: error: [GHC-58481]
      parse error (possibly incorrect indentation or mismatched brackets)
aihc: |
  test.hs:1:3:
  1 | x =
    |   ^
  unexpected end of input
  expecting expression
```

Allowed keys are exactly `src`, `ghc`, and `aihc`. The runner first verifies that GHC rejects the source with the expected normalized message, then checks AIHC's formatted parser errors.

## Oracle Fixtures

Root: `components/aihc-parser/test/Test/Fixtures/oracle`.

Each fixture is a `.hs` file whose first non-space text is an oracle block:

```haskell
{- ORACLE_TEST pass -}
{-# LANGUAGE BlockArguments #-}
module BasicDo where

f = id do
  pure ()
```

Use `{- ORACLE_TEST xfail <reason> -}` for known gaps. The runner derives extensions from the module header, optionally preprocesses CPP-enabled sources, asks GHC for an AST fingerprint, then validates AIHC parsing and round-trip/fingerprint equality. `pass` therefore means more than "AIHC accepted the file".

## Unit And Property Tests

Add unit tests to `components/aihc-parser/test/Spec.hs` when a fixture cannot express the assertion or a reduced QuickCheck counterexample should be pinned directly.

Add properties under `components/aihc-parser/test/Test/Properties`. Reuse existing arbitrary modules under `Test/Properties/Arb` and preserve the existing group names in `Spec.hs`, especially `properties` and `no exceptions`, so `just replay` and pattern filters keep working.

## Useful Commands

```bash
cabal test -v0 aihc-parser:spec --test-options="--pattern parser --hide-successes"
cabal test -v0 aihc-parser:spec --test-options="--pattern lexer-golden --hide-successes"
cabal test -v0 aihc-parser:spec --test-options="--pattern oracle --hide-successes"
cabal test aihc-parser:spec -v0 --test-options="--pattern properties --quickcheck-tests 10000"
```
