# OverloadedRecordDot Implementation Plan

## Overview

Support the `OverloadedRecordDot` GHC extension so that `record.field` is parsed as
field access rather than function composition. The approach uses whitespace-sensitive
lexing — the same pattern as `TkPrefixMinus`/`TkPrefixBang` — to emit a dedicated
`TkRecordDot` token only when `.` is immediately adjacent to the preceding expression
token (no trivia) and is followed by an identifier start character.

GHC's rule: `a.b` is field access when the dot has no space before it **and** is
immediately followed by a lowercase identifier.

`Con.field` is never affected — `gatherQualified` in the lexer already consumes
`Con.field` as a single `TkQVarId "Con" "field"` token before `lexOperator` is reached.

---

## Files to Change

| File | Change |
|---|---|
| `src/Aihc/Parser/Lex/Types.hs` | Add `TkRecordDot` token kind |
| `src/Aihc/Parser/Lex.hs` | Emit `TkRecordDot` in `lexOperator` |
| `src/Aihc/Parser/Syntax.hs` | Add `EGetField Expr Name` constructor to `Expr` |
| `src/Aihc/Parser/Internal/Expr.hs` | Parse `.field` suffixes in `applyRecordSuffixes` |
| `src/Aihc/Parser/Pretty.hs` | Render `EGetField` as `base.field` |
| `src/Aihc/Parser/Shorthand.hs` | Render `EGetField` in shorthand |
| `src/Aihc/Parser/Parens.hs` | Propagate through `EGetField`, parenthesize base |
| `test/Test/Fixtures/golden/expr/record-dot-*.yaml` | 7 new oracle test fixtures |

---

## Step 1 — `Lex/Types.hs`: Add `TkRecordDot`

Add alongside the other prefix-sensitive tokens (near `TkPrefixBang`):

```haskell
| TkRecordDot  -- '.' in field-access position (OverloadedRecordDot)
```

---

## Step 2 — `Lex.hs`: Emit `TkRecordDot` in `lexOperator`

Inside `lexOperator`, before the general `reservedOpTokenKind` dispatch, add a guard
when `opText == "."`:

```haskell
| hasExt OverloadedRecordDot env
, opText == "."
, not (lexerHadTrivia st)
, Just prevKind <- lexerPrevTokenKind st
, not (prevTokenAllowsTightPrefix prevKind)   -- prev token ends an expression
, c :< _ <- T.drop 1 inp
, isVarIdentifierStartChar c                  -- immediately followed by lowercase ident
= Just (mkToken st st' "." TkRecordDot, st')
```

Predicate rationale:

- `not (lexerHadTrivia st)` — rejects `record . field` (space before dot)
- `not (prevTokenAllowsTightPrefix prevKind)` — rejects operator/open-paren positions;
  allows `TkVarId`, `TkConId`, `TkQVarId`, `TkSpecialRParen`, `TkSpecialRBracket`, etc.
- `isVarIdentifierStartChar c` — rejects `a.+` (dot-operator) and `a..` (dot-dot);
  only fires for `a.fieldname`

---

## Step 3 — `Syntax.hs`: Add `EGetField`

Add to the `Expr` ADT after `ERecordUpd`:

```haskell
| EGetField Expr Name     -- a.b  (OverloadedRecordDot)
```

---

## Step 4 — `Internal/Expr.hs`: Parse `.field` suffixes

In `atomOrRecordExprParser`, extend `applyRecordSuffixes` to also consume `TkRecordDot`
+ field name when the extension is enabled. Recursion naturally handles chained access
(`a.b.c` → `EGetField (EGetField (EVar "a") "b") "c"`):

```haskell
applyRecordSuffixes e = do
  mRecordFields <- MP.optional recordBracesParser
  case mRecordFields of
    Just ...  -- existing path
    Nothing -> do
      recordDotEnabled <- isExtensionEnabled OverloadedRecordDot
      if not recordDotEnabled
        then pure e
        else do
          mDot <- MP.optional (expectedTok TkRecordDot)
          case mDot of
            Nothing -> pure e
            Just () -> do
              fieldName <- recordFieldNameParser
              applyRecordSuffixes (EGetField e fieldName)
```

---

## Step 5 — `Pretty.hs`: Pretty-print `EGetField`

Add a case to `prettyExpr` near the `ERecordUpd` case:

```haskell
EGetField base field ->
  prettyExprPrec 3 base <> "." <> prettyName field
```

Using precedence 3 for the base (same as `ERecordUpd`) ensures infix expressions
like `(a + b).field` are parenthesized correctly in the pretty-printer.

---

## Step 6 — `Shorthand.hs`: Shorthand representation

Add a case near `ERecordUpd`:

```haskell
EGetField base field ->
  "EGetField" <+> parens (docExpr base) <+> docName field
```

Oracle strings will look like: `EGetField (EVar "record") "field"`.

---

## Step 7 — `Parens.hs`: Parenthesization

`EGetField` is postfix at atom-level precedence (higher than function application).

- `EGetField` itself never needs outer parens when used as a function argument.
- Add `EGetField base _ -> ... base` passthrough cases in `startsWithDollar`,
  `startsWithBlockExpr`, `startsWithOverloadedLabel`, and `isOpenEnded`/`isGreedyExpr`
  (same pattern as `ERecordUpd base _`).
- In `addExprParensPrec` for `EGetField base field`: apply `addExprParensPrec 3 base`
  so that `(a + b).field` roundtrips correctly.

---

## Step 8 — Oracle Test Fixtures

Location: `components/aihc-parser/test/Test/Fixtures/golden/expr/`

| Filename | Input | Expected AST |
|---|---|---|
| `record-dot-simple.yaml` | `record.field` | `EGetField (EVar "record") "field"` |
| `record-dot-chained.yaml` | `a.b.c` | `EGetField (EGetField (EVar "a") "b") "c"` |
| `record-dot-in-app.yaml` | `f record.field` | `EApp (EVar "f") (EGetField (EVar "record") "field")` |
| `record-dot-paren-base.yaml` | `(f x).field` | `EGetField (EParen (EApp (EVar "f") (EVar "x"))) "field"` |
| `record-dot-update.yaml` | `record.field { x = 1 }` | `ERecordUpd (EGetField ...) [...]` |
| `record-dot-composition-unaffected.yaml` | `f . g` | `EInfix (EVar "f") "." (EVar "g")` |
| `record-dot-disabled.yaml` | `record.field` (no extension) | parses as composition |

---

## Out of Scope (Future Work)

- **Projection sections** `(.field)` — `TkRecordDot` is intentionally not emitted after
  `TkSpecialLParen` (which `prevTokenAllowsTightPrefix` returns `True` for). Requires
  either a separate `TkPrefixDot` token or special-casing in the paren parser.
- **`OverloadedRecordUpdate`** — `record { field = value }` setter via dot syntax.
- **`HasField` constraint solving** — type-checking concern; the parser just produces
  `EGetField`.
