{-# LANGUAGE OverloadedStrings #-}

module Aihc.Parser.Internal.Common
  ( TokParser,
    label,
    region,
    expectedTok,
    eofTok,
    varIdTok,
    tokenSatisfy,
    hiddenPragma,
    optionalHiddenPragma,
    moduleNameParser,
    identifierNameParser,
    identifierUnqualifiedNameParser,
    identifierTextParser,
    lowerIdentifierParser,
    tyVarNameParser,
    implicitParamNameParser,
    constructorNameParser,
    constructorUnqualifiedNameParser,
    constructorOperatorUnqualifiedNameParser,
    constructorIdentifierParser,
    binderNameParser,
    recordFieldNameParser,
    operatorNameParser,
    operatorUnqualifiedNameParser,
    operatorTextParser,
    infixOperatorNameParser,
    constructorInfixOperatorNameParser,
    stringTextParser,
    withSpan,
    withSpanAnn,
    optionalSuffix,
    sourceSpanFromPositions,
    parens,
    braces,
    thQuoteParser,
    skipSemicolons,
    bracedSemiSep,
    bracedSemiSep1,
    plainSemiSep,
    plainSemiSep1,
    contextItemParserWith,
    contextItemsParserWith,
    contextParserWith,
    functionHeadParserWith,
    functionHeadParserWithBinder,
    functionBindValue,
    functionBindDecl,
    isExtensionEnabled,
    thAnyEnabled,
    asPatternParser,
    tupleDelimsParser,
    recordFieldsWithWildcardsParser,
    closeImplicitLayout,
    layoutSepEndBy,
    layoutSepBy1,
    drainParseErrors,
    startsWithContextType,
    startsWithTypeSig,
    startsWithAsPattern,
    startsWithTypeBinder,
    isConLikeName,
    isConLikeNameType,
    qualifiedVarName,
    liftCheck,
    infixOperatorParserExcept,
    foldInfixR,
  )
where

import Aihc.Parser.Lex (LayoutState (..), LexToken (..), LexTokenKind (..), closeImplicitLayoutContext)
import Aihc.Parser.Syntax
import Aihc.Parser.Types (ParserErrorComponent (..), TokStream (..), mkFoundToken)
import Control.Monad (guard)
import Data.Char (isUpper)
import Data.Functor (($>))
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
  TkKeywordModule -> "keyword 'module'"
  TkKeywordWhere -> "keyword 'where'"
  TkKeywordDo -> "keyword 'do'"
  TkKeywordData -> "keyword 'data'"
  TkKeywordImport -> "keyword 'import'"
  TkKeywordCase -> "keyword 'case'"
  TkKeywordOf -> "keyword 'of'"
  TkKeywordLet -> "keyword 'let'"
  TkKeywordIn -> "keyword 'in'"
  TkKeywordIf -> "keyword 'if'"
  TkKeywordThen -> "keyword 'then'"
  TkKeywordElse -> "keyword 'else'"
  TkKeywordProc -> "keyword 'proc'"
  TkKeywordPattern -> "keyword 'pattern'"
  TkKeywordRec -> "keyword 'rec'"
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

hiddenPragma :: String -> (Pragma -> Maybe a) -> TokParser a
hiddenPragma expectedLabel f = do
  mResult <- optionalHiddenPragma f
  case mResult of
    Just result -> pure result
    Nothing -> fail expectedLabel

optionalHiddenPragma :: (Pragma -> Maybe a) -> TokParser (Maybe a)
optionalHiddenPragma f = do
  pst <- MP.getParserState
  case spanNoMatch (tokStreamPendingPragmas (MP.stateInput pst)) of
    (ignored, pragmaTok : rest)
      | Just result <- f pragmaTok -> do
          MP.updateParserState $ \st ->
            st
              { MP.stateInput =
                  (MP.stateInput st)
                    { tokStreamPendingPragmas = ignored <> rest
                    }
              }
          pure (Just result)
      | otherwise -> pure Nothing
    _ -> pure Nothing
  where
    spanNoMatch pragmas =
      case pragmas of
        pragmaTok : rest
          | Just _ <- f pragmaTok -> ([], pragmaTok : rest)
          | otherwise ->
              let (ignored, remaining) = spanNoMatch rest
               in (pragmaTok : ignored, remaining)
        [] -> ([], [])

moduleNameParser :: TokParser Text
moduleNameParser =
  label "module name" $
    tokenSatisfy "module name" $ \tok ->
      case lexTokenKind tok of
        TkConId ident | isModuleName ident -> Just ident
        TkQConId modName name | isModuleName (modName <> "." <> name) -> Just (modName <> "." <> name)
        _ -> Nothing

identifierNameParser :: TokParser Name
identifierNameParser =
  tokenSatisfy "identifier" $ \tok ->
    case lexTokenKind tok of
      TkVarId ident -> Just (qualifyName Nothing (mkUnqualifiedName NameVarId ident))
      TkConId ident -> Just (qualifyName Nothing (mkUnqualifiedName NameConId ident))
      TkQVarId modName ident -> Just (mkName (Just modName) NameVarId ident)
      TkQConId modName ident -> Just (mkName (Just modName) NameConId ident)
      _ -> Nothing

identifierUnqualifiedNameParser :: TokParser UnqualifiedName
identifierUnqualifiedNameParser =
  tokenSatisfy "unqualified identifier" $ \tok ->
    case lexTokenKind tok of
      TkVarId ident -> Just (mkUnqualifiedName NameVarId ident)
      TkConId ident -> Just (mkUnqualifiedName NameConId ident)
      _ -> Nothing

identifierTextParser :: TokParser Text
identifierTextParser = renderName <$> identifierNameParser

lowerIdentifierParser :: TokParser Text
lowerIdentifierParser =
  tokenSatisfy "lowercase identifier" $ \tok ->
    case lexTokenKind tok of
      TkVarId ident -> Just ident
      TkQVarId modName ident -> Just (modName <> "." <> ident)
      _ -> Nothing

tyVarNameParser :: TokParser Text
tyVarNameParser =
  lowerIdentifierParser
    <|> (expectedTok TkKeywordUnderscore $> "_")

implicitParamNameParser :: TokParser Text
implicitParamNameParser =
  tokenSatisfy "implicit parameter" $ \tok ->
    case lexTokenKind tok of
      TkImplicitParam name -> Just name
      _ -> Nothing

constructorIdentifierParser :: TokParser Text
constructorIdentifierParser = renderName <$> constructorNameParser

constructorNameParser :: TokParser Name
constructorNameParser =
  tokenSatisfy "constructor identifier" $ \tok ->
    case lexTokenKind tok of
      TkConId ident -> Just (qualifyName Nothing (mkUnqualifiedName NameConId ident))
      TkQConId modName ident -> Just (mkName (Just modName) NameConId ident)
      _ -> Nothing

constructorUnqualifiedNameParser :: TokParser UnqualifiedName
constructorUnqualifiedNameParser =
  tokenSatisfy "unqualified constructor identifier" $ \tok ->
    case lexTokenKind tok of
      TkConId ident -> Just (mkUnqualifiedName NameConId ident)
      _ -> Nothing

constructorOperatorUnqualifiedNameParser :: TokParser UnqualifiedName
constructorOperatorUnqualifiedNameParser =
  tokenSatisfy "unqualified constructor operator" $ \tok ->
    case lexTokenKind tok of
      TkConSym op -> Just (mkUnqualifiedName NameConSym op)
      TkReservedColon -> Just (mkUnqualifiedName NameConSym ":")
      _ -> Nothing

binderNameParser :: TokParser UnqualifiedName
binderNameParser =
  identifierUnqualifiedNameParser
    <|> parens operatorUnqualifiedNameParser

recordFieldNameParser :: TokParser Name
recordFieldNameParser =
  identifierNameParser
    <|> parens operatorNameParser

operatorTextParser :: TokParser Text
operatorTextParser = renderName <$> operatorNameParser

operatorNameParser :: TokParser Name
operatorNameParser =
  tokenSatisfy "operator" $ \tok ->
    case lexTokenKind tok of
      TkVarSym op -> Just (qualifyName Nothing (mkUnqualifiedName NameVarSym op))
      TkConSym op -> Just (qualifyName Nothing (mkUnqualifiedName NameConSym op))
      TkQVarSym modName op -> Just (mkName (Just modName) NameVarSym op)
      TkQConSym modName op -> Just (mkName (Just modName) NameConSym op)
      _ -> Nothing

operatorUnqualifiedNameParser :: TokParser UnqualifiedName
operatorUnqualifiedNameParser =
  tokenSatisfy "unqualified operator" $ \tok ->
    case lexTokenKind tok of
      TkVarSym op -> Just (mkUnqualifiedName NameVarSym op)
      TkConSym op -> Just (mkUnqualifiedName NameConSym op)
      TkReservedRightArrow -> Just (mkUnqualifiedName NameVarSym "->")
      TkReservedLeftArrow -> Just (mkUnqualifiedName NameVarSym "<-")
      TkReservedDoubleArrow -> Just (mkUnqualifiedName NameVarSym "=>")
      TkReservedEquals -> Just (mkUnqualifiedName NameVarSym "=")
      TkReservedPipe -> Just (mkUnqualifiedName NameVarSym "|")
      TkReservedDotDot -> Just (mkUnqualifiedName NameVarSym "..")
      TkReservedDoubleColon -> Just (mkUnqualifiedName NameVarSym "::")
      TkReservedColon -> Just (mkUnqualifiedName NameConSym ":")
      _ -> Nothing

-- | Parse an infix operator name (varop) for function definitions.
-- Per Haskell Report section 4.4.3, funlhs uses 'varop' which is:
--   varop → varsym | ` varid `
-- This excludes constructor operators (consym) and qualified operators.
-- Note: Whitespace-sensitive lexing (GHC proposal 0229) now distinguishes
-- TkVarSym "!" (infix operator) from TkPrefixBang (bang pattern), so we
-- can accept all VarSym operators here.
infixOperatorNameParser :: TokParser UnqualifiedName
infixOperatorNameParser =
  symbolicOperatorParser <|> backtickIdentifierParser
  where
    symbolicOperatorParser =
      tokenSatisfy "variable operator" $ \tok ->
        case lexTokenKind tok of
          TkVarSym op -> Just (mkUnqualifiedName NameVarSym op)
          _ -> Nothing
    backtickIdentifierParser = do
      expectedTok TkSpecialBacktick
      op <- varIdTextParser
      expectedTok TkSpecialBacktick
      pure (mkUnqualifiedName NameVarId op)
    varIdTextParser =
      tokenSatisfy "variable identifier" $ \tok ->
        case lexTokenKind tok of
          TkVarId name -> Just name
          _ -> Nothing

-- | Parse an infix constructor operator name (conop) for pattern synonym where clauses.
-- Per Haskell Report, pattern synonym where-clause equations use the constructor
-- name in infix position: @pat ConOp pat = expr@.
-- This is the constructor counterpart of 'infixOperatorNameParser'.
--   conop → consym | ` conid `
constructorInfixOperatorNameParser :: TokParser UnqualifiedName
constructorInfixOperatorNameParser =
  symbolicConstructorOperatorParser <|> backtickConstructorIdentifierParser
  where
    symbolicConstructorOperatorParser =
      tokenSatisfy "constructor operator" $ \tok ->
        case lexTokenKind tok of
          TkConSym op -> Just (mkUnqualifiedName NameConSym op)
          TkReservedColon -> Just (mkUnqualifiedName NameConSym ":")
          _ -> Nothing
    backtickConstructorIdentifierParser = do
      expectedTok TkSpecialBacktick
      op <- constructorIdentifierTextParser
      expectedTok TkSpecialBacktick
      pure (mkUnqualifiedName NameConId op)
    constructorIdentifierTextParser =
      tokenSatisfy "constructor identifier" $ \tok ->
        case lexTokenKind tok of
          TkConId name -> Just name
          _ -> Nothing

stringTextParser :: TokParser Text
stringTextParser =
  tokenSatisfy "string literal" $ \tok ->
    case lexTokenKind tok of
      TkString txt -> Just txt
      _ -> Nothing

withSpanAnn :: (SourceSpan -> a -> a) -> TokParser a -> TokParser a
withSpanAnn f parser = do
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
  pure $ f parserSpan out

-- FIXME: Remove.
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

optionalSuffix :: TokParser b -> (a -> b -> a) -> TokParser a -> TokParser a
optionalSuffix suffixParser attach parser = do
  base <- parser
  mSuffix <- MP.optional suffixParser
  pure $
    case mSuffix of
      Just suffix -> attach base suffix
      Nothing -> base

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

-- | Parse a delimited construct with an annotation wrapper.
-- Used for Template Haskell quotes: @open body close@.
thQuoteParser :: (SourceSpan -> c -> c) -> LexTokenKind -> LexTokenKind -> TokParser a -> (a -> c) -> TokParser c
thQuoteParser ann openTok closeTok bodyParser ctor =
  withSpanAnn ann $ do
    expectedTok openTok
    body <- bodyParser
    expectedTok closeTok
    pure (ctor body)

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
    MP.many (parser <* skipSemicolons)

bracedSemiSep1 :: TokParser a -> TokParser [a]
bracedSemiSep1 parser =
  braces $ do
    skipSemicolons
    x <- parser
    skipSemicolons
    rest <- MP.many (parser <* skipSemicolons)
    pure (x : rest)

-- | Zero-or-more variant of 'plainSemiSep1'.
-- Parses zero or more items separated by semicolons (no surrounding braces).
plainSemiSep :: TokParser a -> TokParser [a]
plainSemiSep parser = MP.many (parser <* skipSemicolons)

plainSemiSep1 :: TokParser a -> TokParser [a]
plainSemiSep1 parser = MP.some (parser <* skipSemicolons)

contextItemParserWith :: TokParser Type -> TokParser Type -> TokParser Type
contextItemParserWith typeParser typeAtomParser =
  withSpanAnn (TAnn . mkAnnotation) $
    MP.try parenthesizedContextItemParser <|> MP.try kindSigContextItemParser <|> bareContextItemParser
  where
    bareContextItemParser =
      do
        name <- implicitParamNameParser
        expectedTok TkReservedDoubleColon
        TImplicitParam name <$> typeParser
        <|> do
          expectedTok TkKeywordUnderscore
          pure TWildcard
        <|> constraintTypeParser
    parenthesizedContextItemParser = do
      expectedTok TkSpecialLParen
      item <- contextItemParserWith typeParser typeAtomParser
      expectedTok TkSpecialRParen
      guardNotFollowedByConstraintInfixOp
      pure (TParen item)
      where
        guardNotFollowedByConstraintInfixOp = do
          isFollowed <-
            fmap (either (const False) (const True))
              . MP.observing
              . MP.try
              . MP.lookAhead
              $ constraintTypeInfixOperatorParser
          guard (not isFollowed)
    -- \| Parse a type followed by `::` and another type (kind annotation).
    -- This handles cases like `(c :: Type -> Constraint)` in superclass contexts,
    -- both as standalone parenthesized constraints and as items in comma-separated lists.
    -- Uses lookahead to check for `::` at top bracket depth to avoid ambiguity.
    -- IMPORTANT: Uses `constraintTypeAppParser` (not `typeParser`) for the left side
    -- to avoid a parsing cycle: typeParser -> contextTypeParser -> constraintsParserWith
    -- -> constraintParserWith -> kindSigConstraintParser -> typeParser.
    kindSigContextItemParser :: TokParser Type
    kindSigContextItemParser = do
      guard =<< hasKindSignatureAtTopLevel
      ty <- constraintTypeAppParser
      expectedTok TkReservedDoubleColon
      TKindSig ty <$> kindTypeParser

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
      pure (foldInfixR buildInfixType first rest)
    constraintTypeAppParser = do
      first <- typeAtomParser
      rest <- MP.many constraintTypeAppArgParser
      pure (foldl applyConstraintAppArg first rest)
    constraintTypeAppArgParser =
      (Left <$> MP.try (expectedTok TkTypeApp *> typeAtomParser))
        <|> (Right <$> typeAtomParser)
    applyConstraintAppArg fn (Left ty) = TTypeApp fn ty
    applyConstraintAppArg fn (Right ty) = TApp fn ty
    -- \| Parse a type expression that can appear as a kind annotation.
    -- Handles function types (e.g., Type -> Constraint) and type application,
    -- but NOT context types (C a => ...) to avoid parsing cycles.
    kindTypeParser = do
      first <- constraintTypeAppParser
      rest <- MP.many ((,) <$> constraintTypeInfixOperatorParser <*> constraintTypeAppParser)
      let baseType = foldInfixR buildInfixType first rest
      mRhs <- MP.optional (expectedTok TkReservedRightArrow *> kindTypeParser)
      case mRhs of
        Just rhs ->
          pure (TFun ArrowUnrestricted baseType rhs)
        Nothing -> pure baseType
    buildInfixType lhs ((op, promoted), rhs) =
      TInfix lhs op promoted rhs
    constraintTypeInfixOperatorParser =
      MP.try promotedInfixOperatorParser <|> backtickConstraintOperatorParser <|> unpromotedInfixOperatorParser
    backtickConstraintOperatorParser = MP.try $ do
      expectedTok TkSpecialBacktick
      op <- constraintOperatorIdentifierParser
      expectedTok TkSpecialBacktick
      pure (op, Unpromoted)
    constraintOperatorIdentifierParser =
      tokenSatisfy "constraint operator identifier" $ \tok ->
        case lexTokenKind tok of
          TkVarId name -> Just (qualifyName Nothing (mkUnqualifiedName NameVarId name))
          TkConId name -> Just (qualifyName Nothing (mkUnqualifiedName NameConId name))
          _ -> Nothing
    unpromotedInfixOperatorParser =
      tokenSatisfy "type infix operator" $ \tok ->
        case lexTokenKind tok of
          TkVarSym op
            | op /= "."
                && op /= "!" ->
                Just (qualifyName Nothing (mkUnqualifiedName NameVarSym op), Unpromoted)
          TkConSym op -> Just (qualifyName Nothing (mkUnqualifiedName NameConSym op), Unpromoted)
          TkQVarSym modName op ->
            Just (mkName (Just modName) NameVarSym op, Unpromoted)
          TkQConSym modName op -> Just (mkName (Just modName) NameConSym op, Unpromoted)
          _ -> Nothing
    promotedInfixOperatorParser = do
      expectedTok (TkVarSym "'")
      expectedTok TkReservedColon
      pure (qualifyName Nothing (mkUnqualifiedName NameConSym ":"), Promoted)

contextItemsParserWith :: TokParser Type -> TokParser Type -> TokParser [Type]
contextItemsParserWith typeParser typeAtomParser =
  MP.try parenthesizedContextItemsParser <|> fmap pure (contextItemParserWith typeParser typeAtomParser)
  where
    parenthesizedContextItemsParser = do
      items <- parens (contextItemParserWith typeParser typeAtomParser `MP.sepEndBy` expectedTok TkSpecialComma)
      guardNotFollowedByConstraintInfixOp
      case items of
        [] -> fail "empty constraint list in parens"
        [item] -> pure [typeAnnSpan (getTypeSourceSpan item) (TParen item)]
        _ -> pure items
    guardNotFollowedByConstraintInfixOp = do
      isFollowed <-
        fmap (either (const False) (const True))
          . MP.observing
          . MP.try
          . MP.lookAhead
          $ constraintInfixOpStartParser
      guard (not isFollowed)
    constraintInfixOpStartParser =
      tokenSatisfy "constraint infix operator" $ \tok ->
        case lexTokenKind tok of
          TkVarSym op
            | op /= "."
                && op /= "!" ->
                Just ()
          TkConSym _ -> Just ()
          TkQVarSym _ _ -> Just ()
          TkQConSym _ _ -> Just ()
          TkSpecialBacktick -> Just ()
          _ -> Nothing

contextParserWith :: TokParser Type -> TokParser Type -> TokParser [Type]
contextParserWith = contextItemsParserWith

functionHeadParserWith :: TokParser Pattern -> TokParser Pattern -> TokParser (MatchHeadForm, UnqualifiedName, [Pattern])
functionHeadParserWith = functionHeadParserWithBinder functionBinderNameParser infixOperatorNameParser

functionHeadParserWithBinder :: TokParser UnqualifiedName -> TokParser UnqualifiedName -> TokParser Pattern -> TokParser Pattern -> TokParser (MatchHeadForm, UnqualifiedName, [Pattern])
functionHeadParserWithBinder binderParser infixOpParser fullPatternParser prefixPatternParser =
  MP.try parenthesizedInfixHeadParser
    <|> MP.try infixHeadParser
    <|> prefixHeadParser
  where
    prefixHeadParser = do
      name <- binderParser
      pats <- MP.many prefixPatternParser
      pure (MatchHeadPrefix, name, pats)

    infixHeadParser = do
      lhsPat <- fullPatternParser
      op <- infixOpParser
      rhsPat <- fullPatternParser
      pure (MatchHeadInfix, op, [lhsPat, rhsPat])

    parenthesizedInfixHeadParser = do
      expectedTok TkSpecialLParen
      lhsPat <- fullPatternParser
      op <- infixOpParser
      rhsPat <- fullPatternParser
      expectedTok TkSpecialRParen
      tailPats <- MP.many prefixPatternParser
      pure (MatchHeadInfix, op, [lhsPat, rhsPat] <> tailPats)

functionBinderNameParser :: TokParser UnqualifiedName
functionBinderNameParser =
  variableIdentifierParser <|> parens variableOperatorParser
  where
    variableIdentifierParser =
      tokenSatisfy "function binder" $ \tok ->
        case lexTokenKind tok of
          TkVarId ident -> Just (mkUnqualifiedName NameVarId ident)
          _ -> Nothing
    variableOperatorParser =
      tokenSatisfy "variable operator" $ \tok ->
        case lexTokenKind tok of
          TkVarSym ident -> Just (mkUnqualifiedName NameVarSym ident)
          _ -> Nothing

functionBindValue :: MatchHeadForm -> UnqualifiedName -> [Pattern] -> Rhs Expr -> ValueDecl
functionBindValue _headForm name [] rhs =
  -- Zero-argument bindings (e.g. @x = 5@, @x | g = 5@) are pattern bindings,
  -- not function bindings. 'FunctionBind' is reserved for declarations with
  -- at least one argument pattern.
  PatternBind NoMultiplicityTag (PVar name) rhs
functionBindValue headForm name pats rhs =
  FunctionBind
    name
    [ Match
        { matchAnns = [],
          matchHeadForm = headForm,
          matchPats = pats,
          matchRhs = rhs
        }
    ]

functionBindDecl :: MatchHeadForm -> UnqualifiedName -> [Pattern] -> Rhs Expr -> Decl
functionBindDecl headForm name pats rhs =
  DeclValue (functionBindValue headForm name pats rhs)

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

-- | Check whether any Template Haskell extension is enabled (quotes or full TH).
thAnyEnabled :: TokParser Bool
thAnyEnabled = do
  thEnabled <- isExtensionEnabled TemplateHaskellQuotes
  thFullEnabled <- isExtensionEnabled TemplateHaskell
  pure (thEnabled || thFullEnabled)

asPatternParser :: TokParser Pattern -> TokParser Pattern
asPatternParser bodyParser = withSpanAnn (PAnn . mkAnnotation) $ do
  name <- binderNameParser
  expectedTok TkReservedAt
  PAs name <$> bodyParser

tupleDelimsParser :: TokParser (TupleFlavor, LexTokenKind)
tupleDelimsParser =
  (expectedTok TkSpecialLParen $> (Boxed, TkSpecialRParen))
    <|> (expectedTok TkSpecialUnboxedLParen $> (Unboxed, TkSpecialUnboxedRParen))

recordFieldsWithWildcardsParser :: TokParser [a] -> TokParser ([a], Bool)
recordFieldsWithWildcardsParser fieldsParser = do
  rwcEnabled <- isExtensionEnabled RecordWildCards
  fields <- fieldsParser
  if rwcEnabled
    then do
      mDotDot <- MP.optional (expectedTok TkReservedDotDot)
      case mDotDot of
        Nothing -> pure (fields, False)
        Just _ -> do
          _ <- MP.optional (expectedTok TkSpecialComma)
          pure (fields, True)
    else pure (fields, False)

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

-- | Non-consuming lookahead dispatch for optional context types.
-- Uses scanning to probe for @=>@ at top bracket depth.
-- Returns 'True' when the input looks like a context.
startsWithContextType :: TokParser Bool
startsWithContextType = MP.lookAhead (go [])
  where
    go :: [LexTokenKind] -> TokParser Bool
    go [] = do
      tok <- anySingle
      case lexTokenKind tok of
        TkEOF -> pure False
        TkReservedDoubleArrow -> pure True
        TkReservedDoubleColon -> pure False
        TkReservedRightArrow -> pure False
        TkReservedEquals -> pure False
        TkSpecialComma -> pure False
        TkSpecialSemicolon -> pure False
        TkReservedPipe -> pure False
        TkSpecialRParen -> pure False
        TkSpecialUnboxedRParen -> pure False
        TkSpecialRBracket -> pure False
        TkSpecialRBrace -> pure False
        TkTHExpQuoteClose -> pure False
        TkTHTypedQuoteClose -> pure False
        TkSpecialLParen -> go [TkSpecialRParen]
        TkSpecialUnboxedLParen -> go [TkSpecialUnboxedRParen]
        TkSpecialLBracket -> go [TkSpecialRBracket]
        TkTHExpQuoteOpen -> go [TkTHExpQuoteClose]
        TkTHTypedQuoteOpen -> go [TkTHTypedQuoteClose]
        TkTHDeclQuoteOpen -> go [TkTHExpQuoteClose]
        TkTHTypeQuoteOpen -> go [TkTHExpQuoteClose]
        TkTHPatQuoteOpen -> go [TkTHExpQuoteClose]
        TkSpecialLBrace -> go [TkSpecialRBrace]
        -- Keywords that cannot appear inside a type expression: stop scanning.
        -- This also prevents an enclosing expression form (such as if/then/else)
        -- from being mistaken for a later top-level context arrow.
        TkKeywordThen -> pure False
        TkKeywordElse -> pure False
        TkKeywordOf -> pure False
        TkKeywordIn -> pure False
        TkKeywordInstance -> pure False
        TkKeywordWhere -> pure False
        TkKeywordDeriving -> pure False
        TkKeywordClass -> pure False
        TkKeywordData -> pure False
        TkKeywordNewtype -> pure False
        _ -> go []
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
        TkTHExpQuoteOpen -> go (TkTHExpQuoteClose : stack)
        TkTHTypedQuoteOpen -> go (TkTHTypedQuoteClose : stack)
        TkTHDeclQuoteOpen -> go (TkTHExpQuoteClose : stack)
        TkTHTypeQuoteOpen -> go (TkTHExpQuoteClose : stack)
        TkTHPatQuoteOpen -> go (TkTHExpQuoteClose : stack)
        TkSpecialLBrace -> go (TkSpecialRBrace : stack)
        _ -> go stack

-- | Non-consuming lookahead: does the input start with @name1, name2, ... ::@?
-- Used by declaration parsers to dispatch to the type-signature path without
-- 'MP.try', eliminating backtracking over the name list.
startsWithTypeSig :: TokParser Bool
startsWithTypeSig =
  fmap (either (const False) (const True)) . MP.observing . MP.try . MP.lookAhead $ do
    _ <- sigBinderNameParser
    let moreNames = (expectedTok TkSpecialComma *> sigBinderNameParser *> moreNames) <|> pure ()
    moreNames
    expectedTok TkReservedDoubleColon
  where
    sigBinderNameParser =
      binderNameParser
        <|> parens sigOperatorParser

    sigOperatorParser =
      tokenSatisfy "signature operator" $ \tok ->
        case lexTokenKind tok of
          TkVarSym op -> Just (mkUnqualifiedName NameVarSym op)
          TkConSym op -> Just (mkUnqualifiedName NameConSym op)
          TkReservedColon -> Just (mkUnqualifiedName NameConSym ":")
          _ -> Nothing

-- | Non-consuming lookahead: does the input start with @name \@@?
startsWithAsPattern :: TokParser Bool
startsWithAsPattern =
  fmap (either (const False) (const True)) . MP.observing . MP.try . MP.lookAhead $ do
    _ <- binderNameParser
    expectedTok TkReservedAt

-- | Non-consuming lookahead: does the input start with a type binder (@\@@var or @\@@_)?
-- 'TypeAbstractions' implies 'TypeApplications', so the lexer always emits 'TkTypeApp' (not
-- 'TkReservedAt') for @\@@ preceded by whitespace. All valid type binder positions have
-- whitespace before @\@@, so only 'TkTypeApp' is checked. Accepting 'TkReservedAt' here
-- would produce false positives for as-patterns such as @x\@p@.
startsWithTypeBinder :: TokParser Bool
startsWithTypeBinder =
  fmap (either (const False) (const True)) . MP.observing . MP.try . MP.lookAhead $ do
    expectedTok TkTypeApp
    _ <- lowerIdentifierParser <|> (expectedTok TkKeywordUnderscore $> "_")
    pure ()

-- | Check whether a name looks like a constructor (starts with uppercase or ':').
isConLikeName :: Name -> Bool
isConLikeName = isConLikeNameType . nameType

-- | Check whether a name type is constructor-like.
isConLikeNameType :: NameType -> Bool
isConLikeNameType NameConId = True
isConLikeNameType NameConSym = True
isConLikeNameType _ = False

-- | Reconstruct a possibly-qualified variable name from its textual representation.
qualifiedVarName :: Text -> Name
qualifiedVarName ident =
  case T.breakOnEnd "." ident of
    ("", _) -> qualifyName Nothing (mkUnqualifiedName NameVarId ident)
    (qualifierWithDot, localName) ->
      mkName (Just (T.dropEnd 1 qualifierWithDot)) NameVarId localName

-- | Lift an @Either Text a@ into the parser, converting @Left@ into a parse error.
liftCheck :: Either Text a -> TokParser a
liftCheck (Right a) = pure a
liftCheck (Left msg) = fail (T.unpack msg)

-- | Parse an infix operator, optionally excluding specified operators.
infixOperatorParserExcept :: [Text] -> TokParser Name
infixOperatorParserExcept forbidden =
  symbolicOperatorParser <|> backtickIdentifierOperatorParser
  where
    allowed op = renderName op `notElem` forbidden

    symbolicOperatorParser =
      tokenSatisfy "infix operator" $ \tok ->
        case lexTokenKind tok of
          TkVarSym op ->
            let name = qualifyName Nothing (mkUnqualifiedName NameVarSym op)
             in if allowed name then Just name else Nothing
          TkConSym op ->
            let name = qualifyName Nothing (mkUnqualifiedName NameConSym op)
             in if allowed name then Just name else Nothing
          TkQVarSym modName op ->
            let name = mkName (Just modName) NameVarSym op
             in if allowed name then Just name else Nothing
          TkQConSym modName op ->
            let name = mkName (Just modName) NameConSym op
             in if allowed name then Just name else Nothing
          -- TkMinusOperator is minus when LexicalNegation is enabled but used as infix
          TkMinusOperator ->
            let name = qualifyName Nothing (mkUnqualifiedName NameVarSym "-")
             in if allowed name then Just name else Nothing
          -- Reserved operators that can be used as infix operators
          TkReservedColon ->
            let name = qualifyName Nothing (mkUnqualifiedName NameConSym ":")
             in if allowed name then Just name else Nothing
          _ -> Nothing

    backtickIdentifierOperatorParser = do
      expectedTok TkSpecialBacktick
      op <- identifierNameParser
      expectedTok TkSpecialBacktick
      if allowed op then pure op else fail "forbidden infix operator"

-- | Build a right-associated infix chain from a left operand and a list
-- of @(operator, operand)@ pairs.  Given @lhs@ and
-- @[(op1, a), (op2, b), (op3, c)]@ this produces
-- @lhs \`op1\` (a \`op2\` (b \`op3\` c))@.
--
-- Right-association is the correct default when no fixity information is
-- available: the end-user of the library is responsible for
-- re-associating the tree according to operator precedence.
foldInfixR :: (a -> (op, a) -> a) -> a -> [(op, a)] -> a
foldInfixR _ lhs [] = lhs
foldInfixR build lhs ((op, rhs) : rest) =
  build lhs (op, foldInfixR build rhs rest)
