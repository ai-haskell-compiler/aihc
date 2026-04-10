# Fix: Parenthesized sections with trailing infix operators

## Root Cause Analysis

The parser failed to handle left sections where the left-hand side contains an infix expression chain, such as `(foo . f <$>)` or `(n-1-)`.

### The Problem

When parsing `(foo . f <$>)`:
1. Parser correctly identifies `foo` as the base expression
2. Parses `.` as the first infix operator
3. Parses `f` as the right-hand side
4. Attempts to parse more infix operators with `MP.many`
5. Encounters `<$>` and parses it as an operator
6. **Fails** when trying to parse an expression after `<$>` (finds `)` instead)

The issue was in the `MP.many` loop at line 1154-1159 of `Expr.hs`. Without `MP.try`, once the operator was consumed, the subsequent failure to parse an expression would propagate up as a parse error rather than backtracking.

Additionally, even if the `MP.many` backtracked properly, there was no code after the infix chain to recognize a trailing operator followed by `)` as a left section pattern.

### Why It Failed

The parser had two bugs:
1. **No backtracking**: The `MP.many` for parsing additional infix operators lacked `MP.try`, causing it to consume the trailing operator and then fail
2. **No section detection**: After building the infix chain, there was no check for a trailing operator + `)` pattern that would form a left section

## Solution

The fix has two parts:

### 1. Add `MP.try` to the `more` parser (line 1156)
```haskell
more <-
  MP.many
    ( MP.try
        ( (,)
            <$> infixOperatorParserExcept []
            <*> region "after infix operator" lexpParser
        )
    )
```

This ensures that when the parser sees an operator followed by `)` (not an expression), it backtracks and doesn't consume the operator.

### 2. Check for trailing operator after building the infix chain (line 1165-1170)
```haskell
-- Check for trailing operator to form a left section: (expr1 op1 expr2 op2)
mTrailingOp <- MP.optional (infixOperatorParserExcept [])
case mTrailingOp of
  Just trailOp -> do
    expectedTok closeTok
    pure (\span' -> EParen span' (ESectionL span' fullInfix trailOp))
  Nothing -> do
    -- Continue with arrow/type/where parsing...
```

After building the full infix expression, we check if there's a trailing operator followed by `)`. If so, we create a left section `ESectionL fullInfix trailOp`.

### 3. Fix pretty printer precedence (line 1309 of Pretty.hs)
```haskell
ESectionL _ lhs op -> parens (prettyExprPrec 1 lhs <+> prettyInfixOp op)
```

Changed from precedence 3 to 1 to avoid adding unnecessary parentheses around infix expressions in sections. This ensures `(foo . f <$>)` roundtrips correctly without becoming `((foo . f) <$>)`.

## Changes Made

### Core Fix
- `components/aihc-parser/src/Aihc/Parser/Internal/Expr.hs`: Added `MP.try` and trailing operator check

### Pretty Printer Fix
- `components/aihc-parser/src/Aihc/Parser/Pretty.hs`: Adjusted section precedence for correct roundtripping

### Test Updates
- `components/aihc-parser/test/Test/Fixtures/oracle/haskell2010/expressions/paren-section-fmap.hs`: Changed from xfail to pass
- `components/aihc-parser/test/Test/Fixtures/golden/expr/operator-section-trailing-minus.yaml`: Changed from xfail to pass (bonus fix!)

### New Tests
- `components/aihc-parser/test/Test/Fixtures/oracle/haskell2010/expressions/paren-section-multiple-ops.hs`: Edge cases with multiple infix operators
- `components/aihc-parser/test/Test/Fixtures/golden/expr/paren-section-infix-fmap.yaml`: Golden test for infix + section

## Test Results

All 1213 tests pass, including:
- Original failing test: `paren-section-fmap.hs` (now passing)
- Bonus fix: `operator-section-trailing-minus.yaml` (was xfail, now passing)
- New edge cases: `paren-section-multiple-ops.hs` (passing)
- New golden test: `paren-section-infix-fmap.yaml` (passing)

## Examples Now Working

```haskell
-- Original failing case
(foo . f <$>)  -- Left section with infix chain

-- Bonus case that also got fixed
(n-1-)         -- Left section with infix expression

-- More complex cases
(a . b . c <$>)
(a `f` b `g` c <*>)
((x + y) <$>)
```

## Refactoring Opportunities

The fix is minimal and focused. No additional refactoring is needed at this time.
