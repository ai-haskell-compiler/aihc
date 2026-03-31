# Plan: Implement Template Haskell Splice Parsing

## Goal
Make all 6 `xfail` TemplateHaskell oracle test fixtures pass by implementing splice parsing.

## Test Cases
1. `th_splice_e.hs` - Expression splices: `$expr`, `$(expr arg)`
2. `th_typed_splice.hs` - Typed splices: `$$expr`, `$$(expr arg)`
3. `th_splice_t.hs` - Type splices: `$typ`, `$(typ arg)`
4. `th_splice_p.hs` - Pattern splices: `$pat`, `$(pat arg)`
5. `th_splice_decl.hs` - Top-level declaration splices: `$decl`, `$(makeLenses ''Foo)`
6. `th_nested_splice.hs` - Nested splices inside quotes: `[| 1 + $y |]`, `[|| 1 + $$y ||]`

## Implementation Steps

### 1. Lexer (Lex.hs)
- Add `TkTHSplice` and `TkTHTypedSplice` to `LexTokenKind`
- Add `lexPrefixDollar` function (following `lexPrefixSensitiveOp` pattern)
- Insert in `nextToken` dispatch before `lexOperator`
- Handle `$$` before `$` (greedy match)
- Only emit prefix tokens when `TemplateHaskell` extension is enabled
- Update `canStartPrefixPatternAtom` to include `$` for nested splices

### 2. AST (Syntax.hs)
- Add `ETHSplice SourceSpan Expr` to `Expr` - untyped splice
- Add `ETHTypedSplice SourceSpan Expr` to `Expr` - typed splice
- Add `PSplice SourceSpan Expr` to `Pattern` - pattern splice
- Add `TSplice SourceSpan Expr` to `Type` - type splice
- Add `DeclSplice SourceSpan Expr` to `Decl` - top-level splice
- Update all HasSourceSpan instances and exhaustive pattern matches

### 3. Expression Parser (Expr.hs)
- Add `thSpliceExprParser` and `thTypedSpliceExprParser`
- Parse `$name` as `ETHSplice (EVar name)`
- Parse `$(expr)` as `ETHSplice expr`
- Parse `$$name` as `ETHTypedSplice (EVar name)`
- Parse `$$(expr)` as `ETHTypedSplice expr`
- Add to `atomExprParser` guarded by `thFullEnabled`

### 4. Pattern Parser (Expr.hs)
- Add `thSplicePatternParser` to `patternAtomParser`
- Parse `$pat` and `$(pat)` as `PSplice expr`

### 5. Type Parser (Expr.hs)
- Add `thSpliceTypeParser` to `typeAtomParser`
- Parse `$typ` and `$(typ)` as `TSplice expr`

### 6. Declaration Parser (Decl.hs)
- Add `DeclSplice` case to `declParser` for `TkTHSplice`

### 7. Pretty-printer (Pretty.hs)
- Render splices matching GHC's ppr format for roundtrip validation
- `$name` for bare variable splices, `$(expr)` for complex expressions

### 8. Shorthand (Shorthand.hs)
- Add rendering for new AST nodes

### 9. Common.hs
- Add token descriptions for new token kinds

### 10. Test Fixtures
- Change all 6 xfail fixtures to pass

### 11. Verification
- Run `nix flake check` to verify all tests pass
