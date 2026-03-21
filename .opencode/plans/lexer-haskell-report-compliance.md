# Lexer Haskell Report Compliance Plan

This plan updates the lexer in `components/haskell-parser/src/Parser/Lexer.hs` to comply with the Haskell Language Report specification for lexical structure.

## Summary of Changes

1. **Token Type Distinction**: Replace `TkIdentifier` with `TkVarId`/`TkConId` and `TkOperator` with `TkVarSym`/`TkConSym`
2. **Reserved Identifiers**: Add exhaustive list per Section 2.4
3. **Reserved Operators**: Add dedicated tokens for each reserved operator
4. **Special Characters**: Replace `TkSymbol` with specific `TkSpecial*` tokens

---

## Phase 1: Update LexTokenKind (Lexer.hs lines 74-103)

Replace the entire `LexTokenKind` data type:

```haskell
data LexTokenKind
  = -- Keywords (reserved identifiers per Haskell Report Section 2.4)
    TkKeywordCase
  | TkKeywordClass
  | TkKeywordData
  | TkKeywordDefault
  | TkKeywordDeriving
  | TkKeywordDo
  | TkKeywordElse
  | TkKeywordForeign
  | TkKeywordIf
  | TkKeywordImport
  | TkKeywordIn
  | TkKeywordInfix
  | TkKeywordInfixl
  | TkKeywordInfixr
  | TkKeywordInstance
  | TkKeywordLet
  | TkKeywordModule
  | TkKeywordNewtype
  | TkKeywordOf
  | TkKeywordThen
  | TkKeywordType
  | TkKeywordWhere
  | TkKeywordUnderscore -- _ (wildcard, reserved per Report)
    -- Context-sensitive keywords (not strictly reserved per Report, but needed for imports)
  | TkKeywordQualified
  | TkKeywordAs
  | TkKeywordHiding
    -- Reserved operators (per Haskell Report Section 2.4)
  | TkReservedDotDot -- ..
  | TkReservedColon -- :
  | TkReservedDoubleColon -- ::
  | TkReservedEquals -- =
  | TkReservedBackslash -- \
  | TkReservedPipe -- |
  | TkReservedLeftArrow -- <-
  | TkReservedRightArrow -- ->
  | TkReservedAt -- @
  | TkReservedTilde -- ~
  | TkReservedDoubleArrow -- =>
    -- Identifiers (per Haskell Report Section 2.4)
  | TkVarId Text -- variable identifier (starts lowercase/_)
  | TkConId Text -- constructor identifier (starts uppercase)
  | TkQVarId Text -- qualified variable identifier
  | TkQConId Text -- qualified constructor identifier
    -- Operators (per Haskell Report Section 2.4)
  | TkVarSym Text -- variable symbol (doesn't start with :)
  | TkConSym Text -- constructor symbol (starts with :)
  | TkQVarSym Text -- qualified variable symbol
  | TkQConSym Text -- qualified constructor symbol
    -- Literals
  | TkInteger Integer
  | TkIntegerBase Integer Text
  | TkFloat Double Text
  | TkChar Char
  | TkString Text
    -- Special characters (per Haskell Report Section 2.2)
  | TkSpecialLParen -- (
  | TkSpecialRParen -- )
  | TkSpecialComma -- ,
  | TkSpecialSemicolon -- ;
  | TkSpecialLBracket -- [
  | TkSpecialRBracket -- ]
  | TkSpecialBacktick -- `
  | TkSpecialLBrace -- {
  | TkSpecialRBrace -- }
    -- Pragmas
  | TkPragmaLanguage [ExtensionSetting]
  | TkPragmaWarning Text
  | TkPragmaDeprecated Text
    -- Other
  | TkQuasiQuote Text Text
  | TkError Text
  deriving (Eq, Ord, Show, Read)
```

---

## Phase 2: Update keywordTokenKind (Lexer.hs lines 1332-1349)

Replace with exhaustive reserved identifier list:

```haskell
keywordTokenKind :: Text -> Maybe LexTokenKind
keywordTokenKind txt = case txt of
  "case"     -> Just TkKeywordCase
  "class"    -> Just TkKeywordClass
  "data"     -> Just TkKeywordData
  "default"  -> Just TkKeywordDefault
  "deriving" -> Just TkKeywordDeriving
  "do"       -> Just TkKeywordDo
  "else"     -> Just TkKeywordElse
  "foreign"  -> Just TkKeywordForeign
  "if"       -> Just TkKeywordIf
  "import"   -> Just TkKeywordImport
  "in"       -> Just TkKeywordIn
  "infix"    -> Just TkKeywordInfix
  "infixl"   -> Just TkKeywordInfixl
  "infixr"   -> Just TkKeywordInfixr
  "instance" -> Just TkKeywordInstance
  "let"      -> Just TkKeywordLet
  "module"   -> Just TkKeywordModule
  "newtype"  -> Just TkKeywordNewtype
  "of"       -> Just TkKeywordOf
  "then"     -> Just TkKeywordThen
  "type"     -> Just TkKeywordType
  "where"    -> Just TkKeywordWhere
  "_"        -> Just TkKeywordUnderscore
  -- Context-sensitive keywords (not strictly reserved per Report)
  "qualified" -> Just TkKeywordQualified
  "as"        -> Just TkKeywordAs
  "hiding"    -> Just TkKeywordHiding
  _           -> Nothing
```

---

## Phase 3: Add reservedOpTokenKind (new function after keywordTokenKind)

```haskell
-- | Classify reserved operators per Haskell Report Section 2.4.
reservedOpTokenKind :: Text -> Maybe LexTokenKind
reservedOpTokenKind txt = case txt of
  ".."  -> Just TkReservedDotDot
  ":"   -> Just TkReservedColon
  "::"  -> Just TkReservedDoubleColon
  "="   -> Just TkReservedEquals
  "\\"  -> Just TkReservedBackslash
  "|"   -> Just TkReservedPipe
  "<-"  -> Just TkReservedLeftArrow
  "->"  -> Just TkReservedRightArrow
  "@"   -> Just TkReservedAt
  "~"   -> Just TkReservedTilde
  "=>"  -> Just TkReservedDoubleArrow
  _     -> Nothing
```

---

## Phase 4: Update lexIdentifier (Lexer.hs lines 629-649)

Replace to distinguish varid/conid and handle qualified names:

```haskell
lexIdentifier :: LexerState -> Maybe (LexToken, LexerState)
lexIdentifier st =
  case lexerInput st of
    c : rest
      | isIdentStart c ->
          let (seg, rest0) = span isIdentTail rest
              firstChunk = c : seg
              (consumed, qualParts) = gatherQualified [firstChunk] rest0
              ident = T.pack consumed
              kind = classifyIdentifier qualParts ident
              st' = advanceChars consumed st
           in Just (mkToken st st' ident kind, st')
    _ -> Nothing
  where
    gatherQualified parts chars =
      case chars of
        '.' : c : more
          | isIdentStart c ->
              let (seg, rest) = span isIdentTail more
                  newPart = c : seg
               in gatherQualified (parts ++ [newPart]) rest
        _ -> (intercalate "." parts, parts)

    classifyIdentifier parts ident
      | length parts > 1 =
          -- Qualified name: check first char of final part
          let finalPart = last parts
              firstCharFinal = head finalPart
           in if isAsciiUpper firstCharFinal
                then TkQConId ident
                else TkQVarId ident
      | otherwise =
          -- Unqualified: check for keyword first
          case keywordTokenKind ident of
            Just kw -> kw
            Nothing ->
              let firstChar = T.head ident
               in if isAsciiUpper firstChar
                    then TkConId ident
                    else TkVarId ident
```

Note: Need to import `Data.List (intercalate)` at the top of the file.

---

## Phase 5: Update lexOperator (Lexer.hs lines 651-658)

Replace to distinguish varsym/consym and check reserved operators:

```haskell
lexOperator :: LexerState -> Maybe (LexToken, LexerState)
lexOperator st =
  case span isSymbolicOpChar (lexerInput st) of
    ("", _) -> Nothing
    (op, _) ->
      let txt = T.pack op
          st' = advanceChars op st
          kind = case reservedOpTokenKind txt of
            Just reserved -> reserved
            Nothing ->
              if head op == ':'
                then TkConSym txt
                else TkVarSym txt
       in Just (mkToken st st' txt kind, st')
```

---

## Phase 6: Update lexSymbol (Lexer.hs lines 660-684)

Replace with special characters only (remove `..` and `@` which are now reserved operators):

```haskell
lexSymbol :: LexerState -> Maybe (LexToken, LexerState)
lexSymbol st =
  firstJust
    [ ("(", TkSpecialLParen),
      (")", TkSpecialRParen),
      ("[", TkSpecialLBracket),
      ("]", TkSpecialRBracket),
      ("{", TkSpecialLBrace),
      ("}", TkSpecialRBrace),
      (",", TkSpecialComma),
      (";", TkSpecialSemicolon),
      ("`", TkSpecialBacktick)
    ]
  where
    firstJust xs =
      case xs of
        [] -> Nothing
        (txt, kind) : rest ->
          if txt `List.isPrefixOf` lexerInput st
            then
              let st' = advanceChars txt st
               in Just (mkToken st st' (T.pack txt) kind, st')
            else firstJust rest
```

---

## Phase 7: Update Layout Handling (Lexer.hs)

Update all references to old token types in layout code:

### Line 350 (`applyNegativeLiterals`):
```haskell
-- Change:
lexTokenKind minusTok == TkOperator "-"
-- To:
lexTokenKind minusTok == TkVarSym "-"
```

### Line 462 (`openPendingLayout`):
```haskell
-- Change:
TkSymbol "{" -> ...
-- To:
TkSpecialLBrace -> ...
```

### Line 489 (`closeBeforeToken`):
```haskell
-- Change:
TkSymbol ","
-- To:
TkSpecialComma
```

### Lines 515-521 (`suppressesVirtualSemicolon`):
```haskell
suppressesVirtualSemicolon :: LexToken -> Bool
suppressesVirtualSemicolon tok =
  case lexTokenKind tok of
    TkKeywordThen -> True
    TkKeywordElse -> True
    TkReservedDoubleArrow -> True   -- =>
    TkReservedRightArrow -> True    -- ->
    TkReservedEquals -> True        -- =
    TkReservedPipe -> True          -- |
    TkReservedDoubleColon -> True   -- ::
    _ -> False
```

### Lines 555 (`stepTokenContext`):
```haskell
-- Change:
layoutPrevTokenKind st == Just (TkOperator "\\")
-- To:
layoutPrevTokenKind st == Just TkReservedBackslash
```

### Lines 559-564 (`stepTokenContext`):
```haskell
-- Change:
TkSymbol "(" -> st {layoutDelimiterDepth = layoutDelimiterDepth st + 1}
TkSymbol "[" -> st {layoutDelimiterDepth = layoutDelimiterDepth st + 1}
TkSymbol ")" -> st {layoutDelimiterDepth = max 0 (layoutDelimiterDepth st - 1)}
TkSymbol "]" -> st {layoutDelimiterDepth = max 0 (layoutDelimiterDepth st - 1)}
TkSymbol "{" -> st {layoutContexts = LayoutExplicit : layoutContexts st}
TkSymbol "}" -> st {layoutContexts = popOneContext (layoutContexts st)}
-- To:
TkSpecialLParen -> st {layoutDelimiterDepth = layoutDelimiterDepth st + 1}
TkSpecialLBracket -> st {layoutDelimiterDepth = layoutDelimiterDepth st + 1}
TkSpecialRParen -> st {layoutDelimiterDepth = max 0 (layoutDelimiterDepth st - 1)}
TkSpecialRBracket -> st {layoutDelimiterDepth = max 0 (layoutDelimiterDepth st - 1)}
TkSpecialLBrace -> st {layoutContexts = LayoutExplicit : layoutContexts st}
TkSpecialRBrace -> st {layoutContexts = popOneContext (layoutContexts st)}
```

### Lines 608-614 (`virtualSymbolToken`):
This function creates virtual layout tokens. Update to use new token types:

```haskell
virtualSymbolToken :: Text -> SourceSpan -> LexToken
virtualSymbolToken sym span' =
  LexToken
    { lexTokenKind = case sym of
        "{" -> TkSpecialLBrace
        "}" -> TkSpecialRBrace
        ";" -> TkSpecialSemicolon
        _   -> error ("virtualSymbolToken: unexpected symbol " ++ T.unpack sym),
      lexTokenText = sym,
      lexTokenSpan = span'
    }
```

---

## Phase 8: Update Parser/Internal/Common.hs

### Update symbolLikeTok (lines 51-57):
```haskell
symbolLikeTok :: Text -> TokParser ()
symbolLikeTok expected =
  tokenSatisfy ("symbol " <> show (T.unpack expected)) $ \tok ->
    case (expected, lexTokenKind tok) of
      ("(", TkSpecialLParen) -> Just ()
      (")", TkSpecialRParen) -> Just ()
      (",", TkSpecialComma) -> Just ()
      (";", TkSpecialSemicolon) -> Just ()
      ("[", TkSpecialLBracket) -> Just ()
      ("]", TkSpecialRBracket) -> Just ()
      ("`", TkSpecialBacktick) -> Just ()
      ("{", TkSpecialLBrace) -> Just ()
      ("}", TkSpecialRBrace) -> Just ()
      _ -> Nothing
```

### Update operatorLikeTok (lines 59-65):
```haskell
operatorLikeTok :: Text -> TokParser ()
operatorLikeTok expected =
  tokenSatisfy ("operator " <> show (T.unpack expected)) $ \tok ->
    case lexTokenKind tok of
      TkVarSym op | op == expected -> Just ()
      TkConSym op | op == expected -> Just ()
      TkReservedDotDot | expected == ".." -> Just ()
      TkReservedColon | expected == ":" -> Just ()
      TkReservedDoubleColon | expected == "::" -> Just ()
      TkReservedEquals | expected == "=" -> Just ()
      TkReservedBackslash | expected == "\\" -> Just ()
      TkReservedPipe | expected == "|" -> Just ()
      TkReservedLeftArrow | expected == "<-" -> Just ()
      TkReservedRightArrow | expected == "->" -> Just ()
      TkReservedAt | expected == "@" -> Just ()
      TkReservedTilde | expected == "~" -> Just ()
      TkReservedDoubleArrow | expected == "=>" -> Just ()
      _ -> Nothing
```

### Update moduleNameParser (lines 75-81):
```haskell
moduleNameParser :: TokParser Text
moduleNameParser =
  tokenSatisfy "module name" $ \tok ->
    case lexTokenKind tok of
      TkConId ident | isModuleName ident -> Just ident
      TkQConId ident | isModuleName ident -> Just ident
      _ -> Nothing
```

### Update identifierTextParser (lines 83-88):
```haskell
identifierTextParser :: TokParser Text
identifierTextParser =
  tokenSatisfy "identifier" $ \tok ->
    case lexTokenKind tok of
      TkVarId ident -> Just ident
      TkConId ident -> Just ident
      TkQVarId ident -> Just ident
      TkQConId ident -> Just ident
      _ -> Nothing
```

### Update lowerIdentifierParser (lines 90-96):
```haskell
lowerIdentifierParser :: TokParser Text
lowerIdentifierParser =
  tokenSatisfy "lowercase identifier" $ \tok ->
    case lexTokenKind tok of
      TkVarId ident -> Just ident
      TkQVarId ident -> Just ident
      _ -> Nothing
```

### Update constructorIdentifierParser (lines 98-104):
```haskell
constructorIdentifierParser :: TokParser Text
constructorIdentifierParser =
  tokenSatisfy "constructor identifier" $ \tok ->
    case lexTokenKind tok of
      TkConId ident -> Just ident
      TkQConId ident -> Just ident
      _ -> Nothing
```

### Update identifierExact (lines 111-117):
```haskell
identifierExact :: Text -> TokParser ()
identifierExact expected =
  tokenSatisfy ("identifier " <> show (T.unpack expected)) $ \tok ->
    case lexTokenKind tok of
      TkVarId ident | ident == expected -> Just ()
      TkConId ident | ident == expected -> Just ()
      _ -> Nothing
```

### Update operatorTextParser (lines 119-124):
```haskell
operatorTextParser :: TokParser Text
operatorTextParser =
  tokenSatisfy "operator" $ \tok ->
    case lexTokenKind tok of
      TkVarSym op -> Just op
      TkConSym op -> Just op
      TkQVarSym op -> Just op
      TkQConSym op -> Just op
      _ -> Nothing
```

---

## Phase 9: Update Parser/Internal/Expr.hs

### Line 51:
```haskell
-- Change:
TkOperator "\\" -> lambdaExprParser
-- To:
TkReservedBackslash -> lambdaExprParser
```

### Lines 112, 194, 227, 349-350:
Update all `TkOperator` pattern matches to use appropriate new token types.

---

## Phase 10: Update Parser/Internal/Decl.hs

### Lines 163, 470, 572:
Update `TkIdentifier` to appropriate `TkVarId`/`TkConId`/etc.

### Lines 251, 614:
Update `TkOperator` to appropriate `TkVarSym`/`TkConSym`/reserved tokens.

---

## Phase 11: Update Test Files

### test/Spec.hs (lines 304-338):
Update all `TkIdentifier` expectations to `TkVarId` or `TkConId` as appropriate.

### test/Test/Lexer/Suite.hs:
Update example fixture format.

### test/Test/Fixtures/lexer/*.yaml:
Update expected token types in all fixture files.

---

## Verification

After all changes:

```bash
nix flake check
```

This should run all tests and verify the lexer changes work correctly with the parser.

---

## Notes

1. **Comment lexing**: The Report says `--` only starts a comment if it's not part of a legal lexeme. Need to verify current implementation handles `-->` and `|--` correctly.

2. **Qualified operators**: The Report allows qualified operators like `M.+`. The current implementation may need verification.

3. **Unicode**: The Report specifies Unicode character classes. Current implementation uses ASCII only which is simpler but incomplete for full compliance.
