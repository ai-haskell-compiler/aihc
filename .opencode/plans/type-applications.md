# TypeApplications Implementation Plan

## Summary

Implement parsing support for the `TypeApplications` extension, which allows visible type application syntax in expressions (e.g., `f @Int`, `show (read @Int "5")`).

## Current State

The infrastructure is well-prepared:
- **AST node** `ETypeApp SourceSpan Expr Type` already exists (`Syntax.hs:1089`)
- **Pretty-printer** handles `ETypeApp` (`Pretty.hs:891-892`)
- **Shorthand** handles `ETypeApp` (`Shorthand.hs:545`)
- **Extension enum** `TypeApplications` exists in `Syntax.hs`
- **`TkReservedAt`** token exists for `@`
- **`isExtensionEnabled`** helper available
- **`typeAtomParser`** available for parsing the type after `@`
- **Test fixtures** exist but are marked `xfail`

The only missing piece is the parser code in `appExprParser`.

## Changes

### 1. Modify `appExprParser` in `Expr.hs` (lines 237-242)

**Current code:**
```haskell
appExprParser :: TokParser Expr
appExprParser = withSpan $ do
  first <- atomOrRecordExprParser
  rest <- MP.many atomOrRecordExprParser
  pure $ \span' ->
    foldl (EApp span') first rest
```

**New code:**
```haskell
appExprParser :: TokParser Expr
appExprParser = withSpan $ do
  typeAppsEnabled <- isExtensionEnabled TypeApplications
  first <- atomOrRecordExprParser
  rest <- MP.many (appArg typeAppsEnabled)
  pure $ \span' ->
    foldl (applyArg span') first rest
  where
    appArg :: Bool -> TokParser (Either Type Expr)
    appArg typeAppsEnabled
      | typeAppsEnabled = (Left <$> typeAppArg) <|> (Right <$> atomOrRecordExprParser)
      | otherwise = Right <$> atomOrRecordExprParser

    typeAppArg :: TokParser Type
    typeAppArg = MP.try $ do
      expectedTok TkReservedAt
      typeAtomParser

    applyArg :: SourceSpan -> Expr -> Either Type Expr -> Expr
    applyArg span' fn (Left ty) = ETypeApp span' fn ty
    applyArg span' fn (Right arg) = EApp span' fn arg
```

**Rationale:**
- When `TypeApplications` is enabled, each "argument" in function application can be either a normal expression atom or `@Type` (a type application)
- `typeAppArg` uses `MP.try` so that if `@` is followed by something that's not a valid type atom, we backtrack (e.g., for as-patterns in related contexts)
- `typeAtomParser` is used (not `typeParser`) because GHC parses type applications as atomic types - complex types need parentheses: `f @(Maybe Int)` not `f @Maybe Int`
- The disambiguation from as-patterns is natural: `appExprParser` only runs in expression context, while as-pattern parsing happens in pattern context (`asOrAppPatternParser`, `simplePatternParser`)

### 2. Fix existing test fixtures (add `{-# LANGUAGE TypeApplications #-}`)

The existing test files are missing the language pragma, which means the oracle (GHC) can't parse them either (TypeApplications is not in Haskell2010).

**File: `test/Test/Fixtures/oracle/TypeApplications/type-applications-expr-basic.hs`**
```haskell
{- ORACLE_TEST pass -}
{-# LANGUAGE TypeApplications #-}
module TypeApplicationsExprBasic where

f :: a -> b -> a
f x _ = x

h :: (Int, Int)
h = (f 2 @Int 3, f @Int @Bool 1 True)
```

**File: `test/Test/Fixtures/oracle/TypeApplications/type-applications-no-space.hs`**
```haskell
{- ORACLE_TEST pass -}
{-# LANGUAGE TypeApplications #-}
module TypeApplicationsNoSpace where

f :: a -> a
f x = x

x :: Int
x = f @Int 1
```

### 3. Add new test cases

**File: `test/Test/Fixtures/oracle/TypeApplications/type-applications-paren-type.hs`**
```haskell
{- ORACLE_TEST pass -}
{-# LANGUAGE TypeApplications #-}
module TypeApplicationsParenType where

f :: a -> a
f x = x

x :: Maybe Int
x = f @(Maybe Int) (Just 1)
```

**File: `test/Test/Fixtures/oracle/TypeApplications/type-applications-list-type.hs`**
```haskell
{- ORACLE_TEST pass -}
{-# LANGUAGE TypeApplications #-}
module TypeApplicationsListType where

f :: a -> a
f x = x

x :: [Int]
x = f @[Int] [1, 2, 3]
```

**File: `test/Test/Fixtures/oracle/TypeApplications/type-applications-let-where.hs`**
```haskell
{- ORACLE_TEST pass -}
{-# LANGUAGE TypeApplications #-}
module TypeApplicationsLetWhere where

f :: a -> a
f x = x

g :: Int
g = let y = f @Int 1 in y

h :: Int
h = result
  where result = f @Int 2
```

**File: `test/Test/Fixtures/oracle/TypeApplications/type-applications-chained.hs`**
```haskell
{- ORACLE_TEST pass -}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TypeApplicationsChained where

f :: a -> b -> a
f x _ = x

x :: Int
x = f @Int @Bool 1 True
```

## Testing

1. First verify test files are valid with GHC: `echo '<content>' | ghci -v0`
2. Run `nix flake check` to verify all tests pass
3. Verify with `echo 'f @Int 1' | nix run .#aihc-parser` that the parser produces `ETypeApp`

## Disambiguation Notes

GHC uses whitespace sensitivity for `@`: the `@` must NOT be immediately preceded by an identifier character (otherwise it's an as-pattern in pattern context). In our implementation:

1. **In expression context** (where `appExprParser` runs): `@` is always a type application when `TypeApplications` is enabled. There are no as-patterns in expression context.
2. **In pattern context** (where `asOrAppPatternParser` runs): `@` is always an as-pattern. Type applications in patterns are a separate feature (`TypeAbstractions`).

The lexer always emits `TkReservedAt` for `@` regardless of context. The disambiguation happens at the parser level based on which grammar production is active.

## Risk Assessment

- **Low risk**: The change is isolated to `appExprParser` and only activates when `TypeApplications` is enabled
- **No regression risk for patterns**: Pattern parsing uses separate functions that don't go through `appExprParser`
- **Pretty-print roundtrip**: The pretty printer already handles `ETypeApp` with `@` notation, so roundtrip should work
