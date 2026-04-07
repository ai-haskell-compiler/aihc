{-# LANGUAGE OverloadedStrings #-}

module Aihc.Parser.Internal.Common
  ( TokParser,
    label,
    region,
    keywordTok,
    expectedTok,
    eofTok,
    varIdTok,
    tokenSatisfy,
    moduleNameParser,
    identifierTextParser,
    lowerIdentifierParser,
    implicitParamNameParser,
    constructorIdentifierParser,
    binderNameParser,
    operatorTextParser,
    infixOperatorNameParser,
    stringTextParser,
    withSpan,
    sourceSpanFromPositions,
    parens,
    braces,
    skipSemicolons,
    bracedSemiSep,
    bracedSemiSep1,
    plainSemiSep1,
    constraintParserWith,
    constraintsParserWith,
    contextParserWith,
    functionHeadParserWith,
    functionBindValue,
    functionBindDecl,
    isExtensionEnabled,
    closeImplicitLayout,
    layoutSepEndBy,
    layoutSepBy1,
    drainParseErrors,
  )
where

import Aihc.Parser.Lex (LayoutState (..), LexToken (..), LexTokenKind (..), closeImplicitLayoutContext)
import Aihc.Parser.Syntax
import Aihc.Parser.Types (ParserErrorComponent (..), TokStream (..), mkFoundToken)
import Control.Monad (guard)
import Data.Char (isUpper)
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Text.Megaparsec (Parsec, anySingle, lookAhead, (<|>))
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Error qualified as MPE
import Text.Megaparsec.Pos (SourcePos (..))

type TokParser = Parsec ParserErrorComponent TokStream

label :: Text -> TokParser a -> TokParser a
label expected parser = do
  outcome <- MP.observing parser
  case outcome of
    Right parsed -> pure parsed
    Left err ->
      case err of
        MPE.TrivialError off _ _ -> do
          mTok <- MP.optional (lookAhead anySingle)
          let mFound = mkFoundToken <$> mTok
          MP.parseError $
            MPE.FancyError
              off
              ( Set.singleton
                  ( MPE.ErrorCustom
                      UnexpectedTokenExpecting
                        { unexpectedFound = mFound,
                          unexpectedExpecting = expected,
                          unexpectedContext = []
                        }
                  )
              )
        _ -> MP.parseError err

region :: Text -> TokParser a -> TokParser a
region context =
  MP.region addContextToError
  where
    addContextToError err =
      case err of
        MPE.FancyError off fancySet ->
          MPE.FancyError off (Set.map appendContext fancySet)
        _ -> err
    appendContext fancyErr =
      case fancyErr of
        MPE.ErrorCustom custom ->
          case custom of
            UnexpectedTokenExpecting found expecting contexts ->
              MPE.ErrorCustom (UnexpectedTokenExpecting found expecting (contexts <> [context]))
        _ -> fancyErr

keywordTok :: LexTokenKind -> TokParser ()
keywordTok expected =
  tokenSatisfy ("keyword " <> renderKeyword expected) $ \tok ->
    if lexTokenKind tok == expected then Just () else Nothing

-- | Match a specific token kind exactly.
expectedTok :: LexTokenKind -> TokParser ()
expectedTok expected =
  tokenSatisfy (renderTokenKind expected) $ \tok ->
    if lexTokenKind tok == expected then Just () else Nothing

-- | Match the end-of-file token.
--
-- The lexer emits a 'TkEOF' token at the end of input. This parser consumes
-- that token, ensuring the entire input has been processed.
eofTok :: TokParser ()
eofTok =
  tokenSatisfy "end of input" $ \tok ->
    if lexTokenKind tok == TkEOF then Just () else Nothing

-- | Match a specific variable identifier (contextual keyword).
varIdTok :: Text -> TokParser ()
varIdTok expected =
  tokenSatisfy ("identifier '" <> T.unpack expected <> "'") $ \tok ->
    case lexTokenKind tok of
      TkVarId ident | ident == expected -> Just ()
      _ -> Nothing

renderTokenKind :: LexTokenKind -> String
renderTokenKind tk = case tk of
  TkSpecialLParen -> "symbol '('"
  TkSpecialRParen -> "symbol ')'"
  TkSpecialUnboxedLParen -> "symbol '(#'"
  TkSpecialUnboxedRParen -> "symbol '#)'"
  TkSpecialComma -> "symbol ','"
  TkSpecialSemicolon -> "symbol ';'"
  TkSpecialLBracket -> "symbol '['"
  TkSpecialRBracket -> "symbol ']'"
  TkSpecialBacktick -> "symbol '`'"
  TkSpecialLBrace -> "symbol '{'"
  TkSpecialRBrace -> "symbol '}'"
  TkReservedDotDot -> "operator '..'"
  TkReservedColon -> "operator ':'"
  TkReservedDoubleColon -> "operator '::'"
  TkReservedEquals -> "operator '='"
  TkReservedBackslash -> "operator '\\'"
  TkReservedPipe -> "operator '|'"
  TkReservedLeftArrow -> "operator '<-'"
  TkReservedRightArrow -> "operator '->'"
  TkReservedAt -> "operator '@'"
  TkReservedDoubleArrow -> "operator '=>'"
  TkArrowTail -> "operator '-<'"
  TkArrowTailReverse -> "operator '>-'"
  TkDoubleArrowTail -> "operator '-<<'"
  TkDoubleArrowTailReverse -> "operator '>>-'"
  TkBananaOpen -> "operator '(|'"
  TkBananaClose -> "operator '|)'"
  TkPrefixBang -> "bang pattern '!'"
  TkPrefixTilde -> "irrefutable pattern '~'"
  TkTypeApp -> "type application '@'"
  TkTHExpQuoteOpen -> "TH expression quote '[|'"
  TkTHExpQuoteClose -> "TH expression quote close '|]'"
  TkTHTypedQuoteOpen -> "TH typed quote '[||'"
  TkTHTypedQuoteClose -> "TH typed quote close '||]'"
  TkTHDeclQuoteOpen -> "TH declaration quote '[d|'"
  TkTHTypeQuoteOpen -> "TH type quote '[t|'"
  TkTHPatQuoteOpen -> "TH pattern quote '[p|'"
  TkTHQuoteTick -> "TH name quote '''"
  TkTHTypeQuoteTick -> "TH type name quote ''''"
  TkTHSplice -> "TH splice '$'"
  TkTHTypedSplice -> "TH typed splice '$$'"
  TkImplicitParam name -> "implicit parameter " <> show name
  TkVarSym op -> "operator '" <> show op <> "'"
  TkConSym op -> "operator '" <> show op <> "'"
  _ -> show tk

tokenSatisfy :: String -> (LexToken -> Maybe a) -> TokParser a
tokenSatisfy expectedLabel f =
  MP.token f expectedItems
  where
    expectedItems =
      Set.singleton $
        if null expectedLabel
          then MPE.EndOfInput
          else MPE.Label (NE.fromList expectedLabel)

moduleNameParser :: TokParser Text
moduleNameParser =
  label "module name" $
    tokenSatisfy "module name" $ \tok ->
      case lexTokenKind tok of
        TkConId ident | isModuleName ident -> Just ident
        TkQConId ident | isModuleName ident -> Just ident
        _ -> Nothing

identifierTextParser :: TokParser Text
identifierTextParser =
  tokenSatisfy "identifier" $ \tok ->
    case lexTokenKind tok of
      TkVarId ident -> Just ident
      TkConId ident -> Just ident
      TkQVarId ident -> Just ident
      TkQConId ident -> Just ident
      -- Context-sensitive keywords that can be used as identifiers
      -- (not reserved per Haskell Report §2.4)
      TkKeywordAs -> Just "as"
      TkKeywordQualified -> Just "qualified"
      TkKeywordHiding -> Just "hiding"
      _ -> Nothing

lowerIdentifierParser :: TokParser Text
lowerIdentifierParser =
  tokenSatisfy "lowercase identifier" $ \tok ->
    case lexTokenKind tok of
      TkVarId ident -> Just ident
      TkQVarId ident -> Just ident
      -- Context-sensitive keywords that can be used as identifiers
      TkKeywordAs -> Just "as"
      TkKeywordQualified -> Just "qualified"
      TkKeywordHiding -> Just "hiding"
      _ -> Nothing

implicitParamNameParser :: TokParser Text
implicitParamNameParser =
  tokenSatisfy "implicit parameter" $ \tok ->
    case lexTokenKind tok of
      TkImplicitParam name -> Just name
      _ -> Nothing

constructorIdentifierParser :: TokParser Text
constructorIdentifierParser =
  tokenSatisfy "constructor identifier" $ \tok ->
    case lexTokenKind tok of
      TkConId ident -> Just ident
      TkQConId ident -> Just ident
      _ -> Nothing

binderNameParser :: TokParser Text
binderNameParser =
  identifierTextParser
    <|> parens operatorTextParser

operatorTextParser :: TokParser Text
operatorTextParser =
  tokenSatisfy "operator" $ \tok ->
    case lexTokenKind tok of
      TkVarSym op -> Just op
      TkConSym op -> Just op
      TkQVarSym op -> Just op
      TkQConSym op -> Just op
      _ -> Nothing

-- | Parse an infix operator name (varop) for function definitions.
-- Per Haskell Report section 4.4.3, funlhs uses 'varop' which is:
--   varop → varsym | ` varid `
-- This excludes constructor operators (consym) and qualified operators.
-- Note: Whitespace-sensitive lexing (GHC proposal 0229) now distinguishes
-- TkVarSym "!" (infix operator) from TkPrefixBang (bang pattern), so we
-- can accept all VarSym operators here.
infixOperatorNameParser :: TokParser Text
infixOperatorNameParser =
  symbolicOperatorParser <|> backtickIdentifierParser
  where
    symbolicOperatorParser =
      tokenSatisfy "variable operator" $ \tok ->
        case lexTokenKind tok of
          TkVarSym op -> Just op
          _ -> Nothing
    backtickIdentifierParser = do
      expectedTok TkSpecialBacktick
      op <- varIdTextParser
      expectedTok TkSpecialBacktick
      pure op
    varIdTextParser =
      tokenSatisfy "variable identifier" $ \tok ->
        case lexTokenKind tok of
          TkVarId name -> Just name
          _ -> Nothing

stringTextParser :: TokParser Text
stringTextParser =
  tokenSatisfy "string literal" $ \tok ->
    case lexTokenKind tok of
      TkString txt -> Just txt
      _ -> Nothing

withSpan :: TokParser (SourceSpan -> a) -> TokParser a
withSpan parser = do
  ts <- fmap MP.stateInput MP.getParserState
  let startSpan
        | tokStreamEOFEmitted ts = noSourceSpan
        | tok : _ <- layoutBuffer (tokStreamLayoutState ts) = lexTokenSpan tok
        | rawTok : _ <- tokStreamRawTokens ts = lexTokenSpan rawTok
        | otherwise = noSourceSpan
  out <- parser
  lastToken <- fmap (tokStreamPrevToken . MP.stateInput) MP.getParserState
  let endSpan = maybe noSourceSpan lexTokenSpan lastToken
      parserSpan = mergeSourceSpans startSpan endSpan
  pure (out parserSpan)

sourceSpanFromPositions :: SourcePos -> SourcePos -> SourceSpan
sourceSpanFromPositions start end =
  SourceSpan
    { sourceSpanSourceName = sourceName start,
      sourceSpanStartLine = MP.unPos (sourceLine start),
      sourceSpanStartCol = MP.unPos (sourceColumn start),
      sourceSpanEndLine = MP.unPos (sourceLine end),
      sourceSpanEndCol = MP.unPos (sourceColumn end),
      sourceSpanStartOffset = 0,
      sourceSpanEndOffset = 0
    }

parens :: TokParser a -> TokParser a
parens parser = do
  expectedTok TkSpecialLParen
  res <- parser
  expectedTok TkSpecialRParen
  pure res

braces :: TokParser a -> TokParser a
braces parser = do
  expectedTok TkSpecialLBrace
  res <- parser
  closeAndExpectRBrace
  pure res

-- | Expect a @}@ token, closing implicit layout contexts if needed.
-- This implements the parse-error rule for closing braces: if @}@ is not found
-- but there is an implicit layout context, close it (which buffers a virtual @}@)
-- and consume that virtual @}@.
closeAndExpectRBrace :: TokParser ()
closeAndExpectRBrace =
  expectedTok TkSpecialRBrace <|> do
    closed <- closeImplicitLayout
    if closed then expectedTok TkSpecialRBrace else MP.empty

skipSemicolons :: TokParser ()
skipSemicolons = MP.skipMany (expectedTok TkSpecialSemicolon)

bracedSemiSep :: TokParser a -> TokParser [a]
bracedSemiSep parser =
  braces $ do
    skipSemicolons
    parser `MP.sepEndBy` expectedTok TkSpecialSemicolon

bracedSemiSep1 :: TokParser a -> TokParser [a]
bracedSemiSep1 parser =
  braces $ do
    skipSemicolons
    parser `MP.sepEndBy1` expectedTok TkSpecialSemicolon

plainSemiSep1 :: TokParser a -> TokParser [a]
plainSemiSep1 parser = MP.some (parser <* skipSemicolons)

constraintParserWith :: TokParser Type -> TokParser Type -> TokParser Constraint
constraintParserWith typeParser typeAtomParser =
  MP.try parenthesizedConstraintParser <|> MP.try kindSigConstraintParser <|> bareConstraintParser
  where
    bareConstraintParser = withSpan $ do
      tok <- lookAhead anySingle
      case lexTokenKind tok of
        TkImplicitParam {} -> do
          (className, args) <- implicitParamConstraintParser
          pure $ \span' ->
            Constraint
              { constraintSpan = span',
                constraintClass = className,
                constraintArgs = args
              }
        TkKeywordUnderscore -> do
          _ <- anySingle
          pure CWildcard
        _ -> do
          (className, args) <- MP.try infixConstraintParser <|> prefixConstraintParser
          pure $ \span' ->
            Constraint
              { constraintSpan = span',
                constraintClass = className,
                constraintArgs = args
              }
    implicitParamConstraintParser = do
      name <- implicitParamNameParser
      expectedTok TkReservedDoubleColon
      ty <- typeParser
      pure (name, [ty])
    prefixConstraintParser = do
      className <- identifierTextParser
      args <- MP.many typeAtomParser
      pure (className, args)
    infixConstraintParser = do
      lhs <- constraintTypeParser
      op <- operatorTextParser
      guard (op == "~")
      rhs <- constraintTypeParser
      pure (op, [lhs, rhs])
    parenthesizedConstraintParser = withSpan $ do
      expectedTok TkSpecialLParen
      constraint <- constraintParserWith typeParser typeAtomParser
      expectedTok TkSpecialRParen
      pure (`CParen` constraint)
    -- \| Parse a type followed by `::` and another type (kind annotation).
    -- This handles cases like `(c :: Type -> Constraint)` in superclass contexts,
    -- both as standalone parenthesized constraints and as items in comma-separated lists.
    -- Uses lookahead to check for `::` at top bracket depth to avoid ambiguity.
    -- IMPORTANT: Uses `constraintTypeAppParser` (not `typeParser`) for the left side
    -- to avoid a parsing cycle: typeParser -> contextTypeParser -> constraintsParserWith
    -- -> constraintParserWith -> kindSigConstraintParser -> typeParser.
    kindSigConstraintParser :: TokParser Constraint
    kindSigConstraintParser = withSpan $ do
      guard =<< hasKindSignatureAtTopLevel
      ty <- constraintTypeAppParser
      expectedTok TkReservedDoubleColon
      kind <- kindTypeParser
      let resultTy = TKindSig (mergeSourceSpans (getSourceSpan ty) (getSourceSpan kind)) ty kind
      pure (`CKindSig` resultTy)

    -- \| Lookahead: check if there's a `::` at the top bracket depth.
    -- This avoids ambiguity with the bare constraint parser.
    hasKindSignatureAtTopLevel :: TokParser Bool
    hasKindSignatureAtTopLevel = MP.lookAhead (go 0)
      where
        go :: Int -> TokParser Bool
        go depth = do
          tok <- anySingle
          case lexTokenKind tok of
            TkEOF -> pure False
            TkReservedDoubleColon | depth == 0 -> pure True
            TkReservedRightArrow | depth == 0 -> pure False
            TkSpecialComma | depth == 0 -> pure False
            TkSpecialLParen -> go (depth + 1)
            TkSpecialRParen
              | depth > 0 -> go (depth - 1)
              | otherwise -> pure False
            TkSpecialUnboxedLParen -> go (depth + 1)
            TkSpecialUnboxedRParen
              | depth > 0 -> go (depth - 1)
              | otherwise -> pure False
            TkSpecialLBracket -> go (depth + 1)
            TkSpecialRBracket
              | depth > 0 -> go (depth - 1)
              | otherwise -> pure False
            _ -> go depth
    constraintTypeParser = do
      first <- constraintTypeAppParser
      rest <- MP.many ((,) <$> constraintTypeInfixOperatorParser <*> constraintTypeAppParser)
      pure (foldl buildInfixType first rest)
    constraintTypeAppParser = do
      first <- typeAtomParser
      rest <- MP.many typeAtomParser
      pure (foldl buildTypeApp first rest)
    buildTypeApp lhs rhs =
      TApp (mergeSourceSpans (getSourceSpan lhs) (getSourceSpan rhs)) lhs rhs
    -- \| Parse a type expression that can appear as a kind annotation.
    -- Handles function types (e.g., Type -> Constraint) and type applications,
    -- but NOT context types (C a => ...) to avoid parsing cycles.
    kindTypeParser = do
      first <- constraintTypeAppParser
      rest <- MP.many ((,) <$> constraintTypeInfixOperatorParser <*> constraintTypeAppParser)
      let baseType = foldl buildInfixType first rest
      mRhs <- MP.optional (expectedTok TkReservedRightArrow *> kindTypeParser)
      case mRhs of
        Just rhs -> pure (TFun (mergeSourceSpans (getSourceSpan baseType) (getSourceSpan rhs)) baseType rhs)
        Nothing -> pure baseType
    buildInfixType lhs ((op, promoted), rhs) =
      let span' = mergeSourceSpans (getSourceSpan lhs) (getSourceSpan rhs)
          opType = TCon span' op promoted
       in TApp span' (TApp span' opType lhs) rhs
    constraintTypeInfixOperatorParser =
      MP.try promotedInfixOperatorParser <|> unpromotedInfixOperatorParser
    unpromotedInfixOperatorParser =
      tokenSatisfy "type infix operator" $ \tok ->
        case lexTokenKind tok of
          TkVarSym op
            | op /= "."
                && op /= "!"
                && op /= "-"
                && op /= "~" ->
                Just (op, Unpromoted)
          TkConSym op -> Just (op, Unpromoted)
          TkQVarSym op
            | op /= "~" -> Just (op, Unpromoted)
          TkQConSym op -> Just (op, Unpromoted)
          _ -> Nothing
    promotedInfixOperatorParser = do
      expectedTok (TkVarSym "'")
      expectedTok TkReservedColon
      pure (":", Promoted)

constraintsParserWith :: TokParser Type -> TokParser Type -> TokParser [Constraint]
constraintsParserWith typeParser typeAtomParser =
  MP.try parenthesizedConstraintsParser <|> fmap pure (constraintParserWith typeParser typeAtomParser)
  where
    parenthesizedConstraintsParser = withSpan $ do
      constraints <- parens (constraintParserWith typeParser typeAtomParser `MP.sepEndBy` expectedTok TkSpecialComma)
      pure $ \span' ->
        case constraints of
          [constraint] -> [CParen span' constraint]
          _ -> constraints

contextParserWith :: TokParser Type -> TokParser Type -> TokParser [Constraint]
contextParserWith = constraintsParserWith

functionHeadParserWith :: TokParser Pattern -> TokParser Pattern -> TokParser (MatchHeadForm, Text, [Pattern])
functionHeadParserWith fullPatternParser prefixPatternParser = do
  isParenInfix <- startsWithParenInfixHead
  if isParenInfix
    then parenthesizedInfixHeadParser
    else do
      isInfix <- startsWithInfixHead
      if isInfix
        then infixHeadParser
        else prefixHeadParser
  where
    prefixHeadParser = do
      name <- binderNameParser
      pats <- MP.many prefixPatternParser
      pure (MatchHeadPrefix, name, pats)

    infixHeadParser = do
      lhsPat <- fullPatternParser
      op <- infixOperatorNameParser
      rhsPat <- fullPatternParser
      pure (MatchHeadInfix, op, [lhsPat, rhsPat])

    parenthesizedInfixHeadParser = do
      expectedTok TkSpecialLParen
      lhsPat <- fullPatternParser
      op <- infixOperatorNameParser
      rhsPat <- fullPatternParser
      expectedTok TkSpecialRParen
      tailPats <- MP.many prefixPatternParser
      pure (MatchHeadInfix, op, [lhsPat, rhsPat] <> tailPats)

-- | Non-consuming lookahead: does the input start with a parenthesized infix
-- function head @( pat varop pat ) ...@?
--
-- Scans inside the outermost parentheses for a variable operator ('TkVarSym')
-- or backtick-quoted variable at bracket depth 0. Excludes the case where
-- the parenthesised content is a single operator like @(+)@ — that is a
-- parenthesised operator name (prefix head), not a parenthesised infix head.
startsWithParenInfixHead :: TokParser Bool
startsWithParenInfixHead = MP.lookAhead $ do
  tok <- anySingle
  case lexTokenKind tok of
    TkSpecialLParen -> do
      -- Peek at the first token inside parens. If it is already a varop,
      -- this is @(op)@ — a parenthesised operator name, not an infix head.
      inner <- anySingle
      case lexTokenKind inner of
        TkVarSym _ -> pure False -- (op) or (op ...) — treated as prefix name
        TkSpecialBacktick -> pure False
        TkEOF -> pure False
        TkSpecialRParen -> pure False -- empty parens
        -- First token inside is not an operator → scan for a varop
        _ -> goInner []
    _ -> pure False
  where
    -- Scan inside the outermost parens (we already consumed '(' and one token).
    -- We're looking for a TkVarSym or TkSpecialBacktick at depth 0.
    goInner :: [LexTokenKind] -> TokParser Bool
    -- At depth 0 (inside the outermost parens)
    goInner [] = do
      tok <- anySingle
      case lexTokenKind tok of
        TkEOF -> pure False
        TkSpecialRParen -> pure False -- closed without finding varop
        TkVarSym _ -> pure True
        TkSpecialBacktick -> pure True
        TkSpecialLParen -> goInner [TkSpecialRParen]
        TkSpecialUnboxedLParen -> goInner [TkSpecialUnboxedRParen]
        TkSpecialLBracket -> goInner [TkSpecialRBracket]
        TkSpecialLBrace -> goInner [TkSpecialRBrace]
        _ -> goInner []
    -- Inside nested brackets
    goInner stack@(expectedClose : rest) = do
      tok <- anySingle
      case lexTokenKind tok of
        TkEOF -> pure False
        kind
          | kind == expectedClose ->
              case rest of
                [] -> goInner []
                _ -> goInner rest
        TkSpecialLParen -> goInner (TkSpecialRParen : stack)
        TkSpecialUnboxedLParen -> goInner (TkSpecialUnboxedRParen : stack)
        TkSpecialLBracket -> goInner (TkSpecialRBracket : stack)
        TkSpecialLBrace -> goInner (TkSpecialRBrace : stack)
        _ -> goInner stack

-- | Non-consuming lookahead: does the input start with an infix function head
-- @pat varop pat@?
--
-- Scans the token stream at bracket depth 0 for a variable operator
-- ('TkVarSym') or backtick-quoted variable before reaching @=@ or @|@.
-- Returns 'False' for prefix heads like @f x y = ...@ (no varop before @=@).
startsWithInfixHead :: TokParser Bool
startsWithInfixHead = MP.lookAhead (go [])
  where
    go :: [LexTokenKind] -> TokParser Bool
    -- At depth 0
    go [] = do
      tok <- anySingle
      case lexTokenKind tok of
        TkEOF -> pure False
        -- Stop tokens: we reached '=' or '|' without finding a varop
        TkReservedEquals -> pure False
        TkReservedPipe -> pure False
        -- Statement/declaration boundaries
        TkSpecialSemicolon -> pure False
        TkSpecialRBrace -> pure False
        TkSpecialRParen -> pure False
        TkSpecialRBracket -> pure False
        -- Found a varop at the top level → infix head
        TkVarSym _ -> pure True
        TkSpecialBacktick -> pure True
        -- Nested brackets
        TkSpecialLParen -> go [TkSpecialRParen]
        TkSpecialUnboxedLParen -> go [TkSpecialUnboxedRParen]
        TkSpecialLBracket -> go [TkSpecialRBracket]
        TkSpecialLBrace -> go [TkSpecialRBrace]
        _ -> go []
    -- Inside nested brackets
    go stack@(expectedClose : rest) = do
      tok <- anySingle
      case lexTokenKind tok of
        TkEOF -> pure False
        kind
          | kind == expectedClose ->
              case rest of
                [] -> go []
                _ -> go rest
        TkSpecialLParen -> go (TkSpecialRParen : stack)
        TkSpecialUnboxedLParen -> go (TkSpecialUnboxedRParen : stack)
        TkSpecialLBracket -> go (TkSpecialRBracket : stack)
        TkSpecialLBrace -> go (TkSpecialRBrace : stack)
        _ -> go stack

functionBindValue :: SourceSpan -> MatchHeadForm -> Text -> [Pattern] -> Rhs -> ValueDecl
functionBindValue span' headForm name pats rhs =
  FunctionBind
    span'
    name
    [ Match
        { matchSpan = span',
          matchHeadForm = headForm,
          matchPats = pats,
          matchRhs = rhs
        }
    ]

functionBindDecl :: SourceSpan -> MatchHeadForm -> Text -> [Pattern] -> Rhs -> Decl
functionBindDecl span' headForm name pats rhs =
  DeclValue span' (functionBindValue span' headForm name pats rhs)

renderKeyword :: LexTokenKind -> String
renderKeyword keyword =
  case keyword of
    TkKeywordModule -> "'module'"
    TkKeywordWhere -> "'where'"
    TkKeywordDo -> "'do'"
    TkKeywordData -> "'data'"
    TkKeywordImport -> "'import'"
    TkKeywordQualified -> "'qualified'"
    TkKeywordAs -> "'as'"
    TkKeywordHiding -> "'hiding'"
    TkKeywordCase -> "'case'"
    TkKeywordOf -> "'of'"
    TkKeywordLet -> "'let'"
    TkKeywordIn -> "'in'"
    TkKeywordIf -> "'if'"
    TkKeywordThen -> "'then'"
    TkKeywordElse -> "'else'"
    TkKeywordProc -> "'proc'"
    TkKeywordRec -> "'rec'"
    _ -> "keyword"

isModuleName :: Text -> Bool
isModuleName name =
  case T.splitOn "." name of
    [] -> False
    segments -> all isConstructorIdentifier segments

isConstructorIdentifier :: Text -> Bool
isConstructorIdentifier txt =
  case T.uncons txt of
    Just (c, _) -> isUpper c
    Nothing -> False

isExtensionEnabled :: Extension -> TokParser Bool
isExtensionEnabled ext = do
  pst <- MP.getParserState
  pure (ext `elem` tokStreamExtensions (MP.stateInput pst))

-- | Signal to the layout engine that a virtual close brace should be inserted.
-- This implements the parse-error rule: when the parser encounters a token that
-- is illegal in the current context but @}@ would be legal, it calls this to
-- close the innermost implicit layout context.
--
-- Returns @True@ if a layout was closed, @False@ if there was no implicit
-- layout context to close.
closeImplicitLayout :: TokParser Bool
closeImplicitLayout = do
  pst <- MP.getParserState
  let ts = MP.stateInput pst
  case closeImplicitLayoutContext (tokStreamLayoutState ts) of
    Nothing -> pure False
    Just laySt' -> do
      MP.updateParserState (\s -> s {MP.stateInput = (MP.stateInput s) {tokStreamLayoutState = laySt'}})
      pure True

-- | Like Megaparsec's 'MP.sepEndBy' but implements the parse-error rule for
-- the separator. When the separator fails, we try closing an implicit layout
-- context and retrying — this handles cases like:
--
-- @R { f = case y of A -> 1, g = 2 }@
--
-- where the comma is a record field separator but appears inside the implicit
-- @case@ layout.
layoutSepEndBy :: TokParser a -> TokParser sep -> TokParser [a]
layoutSepEndBy p sep = layoutSepEndBy1 p sep <|> pure []

layoutSepEndBy1 :: TokParser a -> TokParser sep -> TokParser [a]
layoutSepEndBy1 p sep = do
  x <- p
  rest <- MP.option [] $ do
    _ <- layoutSep sep
    layoutSepEndBy p sep
  pure (x : rest)

-- | Like Megaparsec's 'MP.sepBy1' but implements the parse-error rule for
-- the separator.
layoutSepBy1 :: TokParser a -> TokParser sep -> TokParser [a]
layoutSepBy1 p sep = do
  x <- p
  rest <- MP.many $ do
    _ <- layoutSep sep
    p
  pure (x : rest)

-- | Try to match a separator token. If that fails, try closing an implicit
-- layout context and then matching the separator. This implements the
-- parse-error rule: if a token is illegal in the current context but would
-- be legal after inserting a virtual @}@, insert the @}@ and retry.
layoutSep :: TokParser sep -> TokParser sep
layoutSep sep =
  MP.try sep <|> do
    closed <- closeImplicitLayout
    if closed then sep else MP.empty

-- | Drain all registered parse errors from the parser state, returning them
-- and resetting the error list to empty. This prevents 'runParser' from
-- converting a successful parse into a failure due to registered errors
-- (from 'MP.registerParseError' / 'MP.withRecovery').
drainParseErrors :: TokParser [MPE.ParseError TokStream ParserErrorComponent]
drainParseErrors = do
  st <- MP.getParserState
  let errs = MP.stateParseErrors st
  MP.updateParserState (\s -> s {MP.stateParseErrors = []})
  pure errs
