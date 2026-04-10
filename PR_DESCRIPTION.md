# Fix: Guarded class method parsing

## Root Cause

The parser's `classDefaultItemParser` only accepted simple `= expr` right-hand sides for class default methods, not guarded forms (`| guard = expr`). This was inconsistent with:
- Top-level function declarations (use `equationRhsParser`)
- Instance method declarations (use `equationRhsParser`)
- Local let/where bindings (use `equationRhsParser`)

**Exact location**: `components/aihc-parser/src/Aihc/Parser/Internal/Decl.hs`, lines 836-844

The parser manually consumed `=` and parsed an expression, missing the `rhsParserWithArrow` dispatch that handles both unguarded and guarded RHS forms.

## Solution

Changed `classDefaultItemParser` to use `equationRhsParser` instead of manually parsing `= expr`. This is the same approach used by:
- `valueDeclParser` (top-level functions)
- `instanceValueItemParser` (instance methods)
- `localValueDeclParser` (let/where bindings)

This is a minimal, correct fix that aligns class method parsing with all other function binding contexts.

## Changes

1. **Fixed parser**: Updated `classDefaultItemParser` to use `equationRhsParser`
2. **Updated test**: Changed `guarded-method.hs` from `xfail` to `pass`
3. **Added comprehensive tests**:
   - `guarded-method-comprehensive.hs`: Tests simple, guarded, pattern guard, and let guard cases
   - `guarded-method-variations.hs`: Tests multi-parameter classes, infix methods, and multiple equations

## Test Results

- All 1209 parser tests pass
- All 775 oracle tests pass (including 3 new ClassGuards tests)
- No regressions or unexpected passes
