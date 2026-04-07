# TH Declaration Quote Parsing

## Issue
The AIHC parser cannot parse Template Haskell declaration quotes containing top-level declarations like `data`, `type`, `newtype`, `class`, etc.

Example that fails:
```haskell
decl :: Q [Dec]
decl = [d| data Nat = Z |]
```

## Root Cause
- `thDeclQuoteParser` in `Expr.hs` uses `localDeclsParser`
- `localDeclsParser` only handles local declarations (functions, patterns, type sigs)
- It does NOT handle top-level declarations (`data`, `type`, `newtype`, `class`, etc.)

## Circular Dependency
- `Decl.hs` imports from `Expr.hs` (for `exprParser`, `typeParser`, etc.)
- `Expr.hs` cannot import `declParser` from `Decl.hs` without creating a circular dependency
- This prevents reusing the comprehensive `declParser` for TH declaration quotes

## Proper Fix Options
1. **Extract shared module**: Move declaration parsers to a shared module
2. **Inline implementation**: Implement full TH declaration quote parser in `Expr.hs`

Both require substantial refactoring beyond a simple test fix.

## Test Status
- Test file: `components/aihc-parser/test/Test/Fixtures/oracle/TemplateHaskell/th_decl_quote_data_decl.hs`
- Current status: `xfail` (expected failure)
- Should be: `pass` after implementation
