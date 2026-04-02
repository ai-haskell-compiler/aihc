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
  )
where

import Aihc.Parser.Lex (LexToken (..), LexTokenKind (..))
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
  startToken <- MP.optional (lookAhead anySingle)
  out <- parser
  lastToken <- fmap (tokStreamPrevToken . MP.stateInput) MP.getParserState
  let startSpan = maybe noSourceSpan lexTokenSpan startToken
      endSpan = maybe noSourceSpan lexTokenSpan lastToken
      parserSpan = mergeSourceSpans startSpan endSpan
  pure (out parserSpan)

sourceSpanFromPositions :: SourcePos -> SourcePos -> SourceSpan
sourceSpanFromPositions start end =
  SourceSpan
    { sourceSpanStartLine = MP.unPos (sourceLine start),
      sourceSpanStartCol = MP.unPos (sourceColumn start),
      sourceSpanEndLine = MP.unPos (sourceLine end),
      sourceSpanEndCol = MP.unPos (sourceColumn end)
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
  expectedTok TkSpecialRBrace
  pure res

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

constraintParserWith :: TokParser Type -> TokParser Constraint
constraintParserWith typeAtomParser =
  MP.try parenthesizedConstraintParser <|> bareConstraintParser
  where
    bareConstraintParser = withSpan $ do
      (className, args) <- MP.try infixConstraintParser <|> prefixConstraintParser
      pure $ \span' ->
        Constraint
          { constraintSpan = span',
            constraintClass = className,
            constraintArgs = args
          }
    prefixConstraintParser = do
      className <- identifierTextParser
      args <- MP.many typeAtomParser
      pure (className, args)
    infixConstraintParser = do
      lhs <- typeAtomParser
      op <- operatorTextParser
      guard (op == "~")
      rhs <- typeAtomParser
      pure (op, [lhs, rhs])
    parenthesizedConstraintParser = withSpan $ do
      constraint <- parens (constraintParserWith typeAtomParser)
      pure (`CParen` constraint)

constraintsParserWith :: TokParser Type -> TokParser [Constraint]
constraintsParserWith typeAtomParser =
  MP.try parenthesizedConstraintsParser <|> fmap pure (constraintParserWith typeAtomParser)
  where
    parenthesizedConstraintsParser = withSpan $ do
      constraints <- parens (constraintParserWith typeAtomParser `MP.sepEndBy` expectedTok TkSpecialComma)
      pure $ \span' ->
        case constraints of
          [constraint] -> [CParen span' constraint]
          _ -> constraints

contextParserWith :: TokParser Type -> TokParser [Constraint]
contextParserWith = constraintsParserWith

functionHeadParserWith :: TokParser Pattern -> TokParser Pattern -> TokParser (MatchHeadForm, Text, [Pattern])
functionHeadParserWith fullPatternParser prefixPatternParser =
  MP.try parenthesizedInfixHeadParser <|> MP.try infixHeadParser <|> prefixHeadParser
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
