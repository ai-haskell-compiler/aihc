# Unified Expression/Pattern Parsing

**Status:** Proposed
**Scope:** `aihc-parser` — `Aihc.Parser.Internal.Expr`, `Aihc.Parser.Internal.Decl`, `Aihc.Parser.Internal.Common`

## Problem

The parser uses `MP.try` extensively around alternatives that share long
common prefixes. Every `MP.try` that wraps a parser consuming _k_ tokens
before failing wastes O(_k_) work. When these nest or chain, we get O(_n_²)
or worse.

The single biggest source of this backtracking is the **pattern-vs-expression
ambiguity**. Patterns and expressions share nearly identical surface syntax
(`x`, `Con x y`, `(a, b)`, literals, `[a,b]`, etc.), but the parser
maintains separate `patternParser` and `exprParser` code paths. Every
context that could contain either one needs `MP.try`:

```haskell
-- Expr.hs:109 — do-statement: try pattern+arrow, backtrack to expression
doStmtParser = MP.try doBindStmtParser <|> MP.try doLetStmtParser <|> doExprStmtParser

-- Expr.hs:647 — guard qualifier: try pattern+arrow, backtrack to expression
guardQualifierParser = MP.try guardPatParser <|> MP.try guardLetParser <|> guardExprParser

-- Expr.hs:915 — comprehension statement: try pattern+arrow, backtrack to expression
compStmtParser = MP.try compGenStmtParser <|> MP.try compLetStmtParser <|> compGuardStmtParser

-- Common.hs:411 — function head: try infix, try paren-infix, fall back to prefix
functionHeadParserWith = MP.try parenthesizedInfixHeadParser <|> MP.try infixHeadParser <|> prefixHeadParser

-- Expr.hs:966 — local declaration: try type sig, try function, fall back to pattern
localDeclParser = MP.try localTypeSigDeclParser <|> MP.try localFunctionDeclParser <|> localPatternDeclParser
```

In each case, the first alternative can parse an arbitrarily long
expression/pattern before failing at the disambiguating token (`<-`, `::`,
`=`, an operator). The parser then backtracks and re-parses the same tokens
via the next alternative.

### Scope of the problem

This is not a theoretical concern. Previous PRs have already fixed specific
instances of exponential backtracking:

- **PR #470** (`perf(parser): fix O(2^N) backtracking in nested paren expressions`) —
  introduced `parseBoxedContent` to avoid re-parsing the inner expression
  when distinguishing `(expr)`, `(expr op)`, `(op expr)`, and `(expr, expr)`.
- **PR #461** (`fix(parser): avoid backtracking in tuple section detection`)
- **PR #457** (`fix(parser): avoid backtracking on nested function types`)
- **PR #454** (`fix(parser): avoid excessive pattern backtracking`)
- **PR #469** (`fix(parser): avoid tuple-pattern function head rescans`)

Each fix was ad-hoc: custom code to parse a shared prefix once and then
branch. These fixes are correct but increase complexity and maintenance
burden. The systemic solution is to eliminate the root cause.

## Prior art: how GHC solves this

GHC's parser (`compiler/GHC/Parser.y`) uses a single `exp` production for
both expressions and patterns. After parsing, a family of reclassification
functions (`checkPattern`, `checkAPat`, etc. in `GHC.Parser.PostProcess`)
walks the expression tree and converts it to a pattern tree, rejecting
invalid forms with precise error messages.

This approach is well-tested (GHC has used it for decades) and generalises
cleanly to new syntax extensions.

## Design

### Overview

Parse a superset using `exprParser`, then reclassify into `Pattern` where
the grammar requires a pattern. A new pure function `checkPattern` performs
the conversion.

```
              exprParser
tokens ──────────────────────> Expr
                                 │
                  checkPattern   │  (where grammar expects a pattern)
                                 ▼
                              Pattern
```

No new AST types are introduced. The existing `Expr` and `Pattern` types
are unchanged. The only new code is the `checkPattern` family of functions,
and the only changed code is the call sites listed above.

### The `checkPattern` function

A new module `Aihc.Parser.Internal.CheckPattern` exports:

```haskell
module Aihc.Parser.Internal.CheckPattern
  ( checkPattern,
    checkPatterns,
    checkLhsPattern,
  ) where

-- | Convert an expression tree into a pattern.
-- Returns Left with a diagnostic message if the expression cannot be
-- interpreted as a valid pattern.
checkPattern :: Expr -> Either Text Pattern

-- | Convert a list of expressions into patterns.
checkPatterns :: [Expr] -> Either Text [Pattern]

-- | Convert an expression into an LHS pattern suitable for function
-- binding heads (more restrictive than checkPattern).
checkLhsPattern :: Expr -> Either Text Pattern
```

The implementation is a straightforward structural recursion over `Expr`:

```haskell
checkPattern :: Expr -> Either Text Pattern
checkPattern = \case
  -- Direct correspondences
  EVar sp name
    | isConLikeName name -> Right (PCon sp name [])
    | otherwise          -> Right (PVar sp name)
  EParen sp inner        -> PParen sp <$> checkPattern inner
  ETuple sp fl elems     -> PTuple sp fl <$> traverse checkMaybePattern elems
  EList sp elems         -> PList sp <$> traverse checkPattern elems
  EUnboxedSum sp i n e   -> PUnboxedSum sp i n <$> checkPattern e
  EInfix sp l op r       -> PInfix sp <$> checkPattern l <*> pure op <*> checkPattern r
  ETypeSig sp e ty       -> PTypeSig sp <$> checkPattern e <*> pure ty
  ENegate sp inner       -> checkNegLitPattern sp inner

  -- Literals
  EInt sp n repr         -> Right (PLit sp (LitInt sp n repr))
  EIntHash sp n repr     -> Right (PLit sp (LitIntHash sp n repr))
  EIntBase sp n repr     -> Right (PLit sp (LitIntBase sp n repr))
  EIntBaseHash sp n repr -> Right (PLit sp (LitIntBaseHash sp n repr))
  EFloat sp x repr       -> Right (PLit sp (LitFloat sp x repr))
  EFloatHash sp x repr   -> Right (PLit sp (LitFloatHash sp x repr))
  EChar sp c repr        -> Right (PLit sp (LitChar sp c repr))
  ECharHash sp c repr    -> Right (PLit sp (LitCharHash sp c repr))
  EString sp s repr      -> Right (PLit sp (LitString sp s repr))
  EStringHash sp s repr  -> Right (PLit sp (LitStringHash sp s repr))

  -- Application: accumulate into PCon
  EApp sp f x -> do
    fPat <- checkPattern f
    xPat <- checkPattern x
    case fPat of
      PCon csp name args -> Right (PCon sp name (args ++ [xPat]))
      PVar csp name | isConLikeName name -> Right (PCon sp name [xPat])
      _ -> Left ("invalid pattern: application of non-constructor")

  -- Record construction -> record pattern
  ERecordCon sp name fields wc -> do
    patFields <- traverse (\(n, e) -> (n,) <$> checkPattern e) fields
    Right (PRecord sp name patFields wc)

  -- TH splice
  ETHSplice sp body      -> Right (PSplice sp body)

  -- Quasi-quote
  EQuasiQuote sp q b     -> Right (PQuasiQuote sp q b)

  -- Expression-only constructs: clear error
  EIf {}            -> Left "unexpected if-then-else in pattern"
  ECase {}          -> Left "unexpected case expression in pattern"
  EDo {}            -> Left "unexpected do expression in pattern"
  ELambdaPats {}    -> Left "unexpected lambda in pattern"
  ELambdaCase {}    -> Left "unexpected lambda-case in pattern"
  ELetDecls {}      -> Left "unexpected let expression in pattern"
  EArithSeq {}      -> Left "unexpected arithmetic sequence in pattern"
  EListComp {}      -> Left "unexpected list comprehension in pattern"
  ESectionL {}      -> Left "unexpected left section in pattern"
  ESectionR {}      -> Left "unexpected right section in pattern"
  ERecordUpd {}     -> Left "unexpected record update in pattern"
  ETypeApp {}       -> Left "unexpected type application in pattern"
  _                 -> Left "expression not valid in pattern context"
```

#### Pattern-only syntax

Some pattern constructs have no expression counterpart:

| Pattern construct | Surface syntax | How it's handled |
|---|---|---|
| `PAs` | `x@pat` | `@` is already lexed as `TkReservedAt`. In expression context, `@` is a type application. The parser sees `EVar "x"` followed by `TkReservedAt` — `checkPattern` can handle this if we parse `x@pat` as a special expression form, or we keep the existing `startsWithAsPattern` lookahead. |
| `PStrict` | `!pat` | `!` is lexed as `TkPrefixBang` (whitespace-sensitive). In expression context this is already handled. We can let the expression parser produce a node for prefix `!` and reclassify. |
| `PIrrefutable` | `~pat` | Similarly, `~` is lexed as `TkPrefixTilde`. |
| `PWildcard` | `_` | `_` is lexed as `TkKeywordUnderscore`. The expression parser can treat this as a special variable. |
| `PNegLit` | `-5` | Already handled: `ENegate` with a literal child maps to `PNegLit`. |
| `PView` | `(expr -> pat)` | View patterns appear inside parentheses. The existing `parseBoxedContent` / `parenExprParser` code path already handles this context. This can remain as a special case in the paren parser. |

**Strategy:** For `!pat`, `~pat`, `_`, and `x@pat`, introduce lightweight
expression-level wrappers that carry the information through to
`checkPattern`. For example:

```haskell
-- New expression constructors for pattern-only surface syntax.
-- These are produced by the unified parser and consumed only by checkPattern.
-- They never appear in a final expression AST.
| EBangPat SourceSpan Expr      -- !e  (prefix bang, pattern context)
| ETildePat SourceSpan Expr     -- ~e  (prefix tilde, pattern context)
| EAsPat SourceSpan Text Expr   -- x@e (as-pattern)
| EWildPat SourceSpan           -- _   (wildcard)
```

Alternatively, keep the existing `patternAtomParser` for these constructs
and only unify the _shared_ syntax. This is the more conservative approach
and what the phased implementation plan below uses.

### How call sites change

#### `doStmtParser` (Expr.hs:109)

Before:
```haskell
doStmtParser = MP.try doBindStmtParser <|> MP.try doLetStmtParser <|> doExprStmtParser
```

After:
```haskell
doStmtParser = doLetStmtParser <|> doBindOrExprStmtParser

doBindOrExprStmtParser = withSpan $ do
  expr <- exprParser
  mArrow <- MP.optional (expectedTok TkReservedLeftArrow)
  case mArrow of
    Just () -> do
      pat <- liftCheck (checkPattern expr)
      rhs <- region "while parsing '<-' binding" exprParser
      pure (\sp -> DoBind sp pat rhs)
    Nothing ->
      pure (`DoExpr` expr)
```

No `MP.try` needed. The `let` alternative is distinguished by its leading
keyword and doesn't overlap.

#### `guardQualifierParser` (Expr.hs:647)

Before:
```haskell
guardQualifierParser = MP.try guardPatParser <|> MP.try guardLetParser <|> guardExprParser
```

After:
```haskell
guardQualifierParser = guardLetParser <|> guardBindOrExprParser

guardBindOrExprParser = withSpan $ do
  expr <- exprParser
  mArrow <- MP.optional (expectedTok TkReservedLeftArrow)
  case mArrow of
    Just () -> do
      pat <- liftCheck (checkPattern expr)
      rhs <- exprParser
      pure (\sp -> GuardPat sp pat rhs)
    Nothing ->
      pure (`GuardExpr` expr)
```

#### `compStmtParser` (Expr.hs:915)

Same pattern as `doStmtParser`:

```haskell
compStmtParser = compLetStmtParser <|> compGenOrGuardParser

compGenOrGuardParser = withSpan $ do
  expr <- exprParser
  mArrow <- MP.optional (expectedTok TkReservedLeftArrow)
  case mArrow of
    Just () -> do
      pat <- liftCheck (checkPattern expr)
      rhs <- region "while parsing '<-' generator" exprParser
      pure (\sp -> CompGen sp pat rhs)
    Nothing ->
      pure (`CompGuard` expr)
```

#### `localDeclParser` (Expr.hs:966)

Before:
```haskell
localDeclParser = MP.try localTypeSigDeclParser <|> MP.try localFunctionDeclParser <|> localPatternDeclParser
```

After:
```haskell
localDeclParser = do
  -- Parse the head as an expression/name list.
  -- Then dispatch on what follows (::, =, patterns, guards).
  names <- binderNameParser `MP.sepBy1` expectedTok TkSpecialComma
  tok <- lookAhead anySingle
  case lexTokenKind tok of
    TkReservedDoubleColon -> do
      expectedTok TkReservedDoubleColon
      ty <- typeParser
      pure (DeclTypeSig names ty)
    _ -> do
      -- Re-check: was it a single name (function) or a pattern?
      -- ... dispatch to function or pattern binding
```

This is more involved because function heads have their own ambiguity
(prefix vs infix). The recommended approach is to first implement the
simpler do/guard/comp cases, gain confidence in `checkPattern`, and then
tackle declaration heads.

#### `functionHeadParserWith` (Common.hs:411)

This requires parsing a general "application-like expression" and then
reclassifying the head as a function name and the arguments as patterns.
GHC does this with `checkFunBind` in `PostProcess`. The approach is:

1. Parse `expr₁ op expr₂` or `expr₁ expr₂ expr₃ ...`
2. Check if this is an infix pattern (`expr op expr`) or a prefix
   application (`name pat₁ pat₂ ...`)
3. Extract the function name and convert arguments via `checkPattern`

### Error reporting

`checkPattern` returns `Either Text Pattern`. At call sites, convert the
`Left` branch into a megaparsec error at the current offset:

```haskell
liftCheck :: Either Text a -> TokParser a
liftCheck (Right a) = pure a
liftCheck (Left msg) = fail (T.unpack msg)
```

Error messages should include the source span of the offending
sub-expression. Since every `Expr` constructor carries a `SourceSpan`, the
error can pinpoint exactly which sub-expression is invalid:

```
unexpected case expression in pattern
  at line 42, column 10
```

## Implementation plan

The refactoring is broken into phases. Each phase is independently
shippable, preserves all existing tests, and provides incremental
performance improvement.

### Phase 1: `checkPattern` module + `doStmtParser`

1. Create `Aihc.Parser.Internal.CheckPattern` with `checkPattern` and
   `checkPatterns`.
2. Write unit tests for `checkPattern` covering all `Expr`-to-`Pattern`
   mappings and all error cases.
3. Refactor `doStmtParser` to parse as expression, then reclassify.
4. Run the full test suite (`nix flake check`) and oracle compliance.

**Metric:** `MP.try` count in `doStmtParser` goes from 2 to 0.

### Phase 2: `guardQualifierParser` + `compStmtParser`

1. Apply the same parse-then-reclassify pattern.
2. Verify test suite.

**Metric:** 4 more `MP.try` eliminated.

### Phase 3: `localDeclParser`

1. Unify type signature and function/pattern binding parsing.
2. Parse the head (names or pattern) as expression, then dispatch on `::` or `=`.
3. Verify test suite.

### Phase 4: `functionHeadParserWith`

1. Parse the function head as a general expression.
2. Implement `checkFunctionHead :: Expr -> Either Text (MatchHeadForm, Text, [Pattern])`.
3. Remove the three-way `MP.try` chain.
4. Verify test suite.

### Phase 5: Pattern-only syntax in expression parser (optional)

If desired, extend the expression parser to handle `!`, `~`, `_`, and `@`
natively (via new `Expr` constructors or sentinel values), allowing
`checkPattern` to handle all pattern syntax without any separate pattern
parsing code path.

This phase is optional — Phases 1-4 provide the main performance wins
without it.

## What this does NOT change

- **`parseBoxedContent`** (Expr.hs:728) — This handles the parenthesized
  expression ambiguity (paren vs section vs tuple), which is orthogonal to
  pattern/expression unification. It stays as-is.

- **`declParser` keyword dispatch** (Decl.hs:210-245) — The top-level
  declaration dispatch on the leading keyword is already O(1) and well-structured.

- **`atomExprParser` alternatives** (Expr.hs:326) — Most alternatives are
  distinguished by leading token or single-token lookahead. The remaining
  `MP.try` uses are low-cost.

- **The `Expr` and `Pattern` types** — No constructors are added or removed
  (unless Phase 5 is pursued). The AST surface is unchanged.

- **Pretty-printer, shorthand, and downstream consumers** — These consume
  the final `Pattern`/`Expr` types, which are unchanged.

## Risks and mitigations

| Risk | Mitigation |
|---|---|
| `checkPattern` silently accepts invalid patterns | Exhaustive unit tests for every `Expr` constructor. Property tests: `checkPattern . exprFromPattern === Right pat` for all generated patterns. |
| Error messages degrade | `checkPattern` reports the `SourceSpan` of the invalid sub-expression. Compare error messages before/after in the error-messages test suite. |
| Expressions parse differently when used in pattern context | Oracle suite catches any behavioral change. The oracle tests parse full modules through GHC and compare AST fingerprints. |
| Phase 5 bloats the `Expr` type with pattern-only constructors | Phase 5 is optional. Phases 1-4 avoid adding any new constructors. |
| Merge conflicts with concurrent parser work | Each phase is small (one PR each) and touches isolated call sites. |

## Appendix: Expr-to-Pattern mapping

| Category | Expr constructor(s) | Pattern constructor | Notes |
|---|---|---|---|
| Variable | `EVar` | `PVar` / `PCon` | Uppercase → `PCon`, lowercase → `PVar` |
| Literal | `EInt`, `EFloat`, `EChar`, `EString` (+ Hash/Base) | `PLit` | Wrap in `Literal` |
| Negated literal | `ENegate` + literal child | `PNegLit` | Reject if child is not a literal |
| Application | `EApp` | `PCon` | Accumulate args; head must be constructor |
| Infix | `EInfix` | `PInfix` | Direct mapping |
| Tuple | `ETuple` | `PTuple` | Reject `Nothing` elements (tuple sections) |
| Unboxed sum | `EUnboxedSum` | `PUnboxedSum` | Direct mapping |
| List | `EList` | `PList` | Direct mapping |
| Parenthesized | `EParen` | `PParen` | Direct mapping |
| Record con | `ERecordCon` | `PRecord` | Convert field exprs to patterns recursively |
| Type signature | `ETypeSig` | `PTypeSig` | Direct mapping |
| TH splice | `ETHSplice` | `PSplice` | Direct mapping |
| Quasi-quote | `EQuasiQuote` | `PQuasiQuote` | Direct mapping |
| All others | `EIf`, `ECase`, `EDo`, `ELambda*`, `ELet*`, `EWhere*`, `EArithSeq`, `EListComp*`, `ESection*`, `ERecordUpd`, `ETypeApp`, `ETH*Quote`, `ETH*Splice` (typed) | — | Rejected with error message |
