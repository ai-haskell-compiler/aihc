{-# LANGUAGE OverloadedStrings #-}

module Parser.Internal.Common
  ( TokParser,
    keywordTok,
    symbolLikeTok,
    operatorLikeTok,
    tokenSatisfy,
    moduleNameParser,
    identifierTextParser,
    lowerIdentifierParser,
    constructorIdentifierParser,
    binderNameParser,
    identifierExact,
    operatorTextParser,
    stringTextParser,
    withSpan,
    sourceSpanFromPositions,
    markSingleParenConstraint,
    parens,
    skipSemicolons,
    bracedSemiSep,
    bracedSemiSep1,
    plainSemiSep1,
    constraintParserWith,
    constraintsParserWith,
    contextParserWith,
    functionBindValue,
    functionBindDecl,
  )
where

import Data.Char (isUpper)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Parser.Ast
import Parser.Lexer (LexToken (..), LexTokenKind (..))
import Parser.Types (TokStream)
import Text.Megaparsec (Parsec, anySingle, lookAhead, (<|>))
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Pos (SourcePos (..))

type TokParser = Parsec Void TokStream

keywordTok :: LexTokenKind -> TokParser ()
keywordTok expected =
  tokenSatisfy ("keyword " <> renderKeyword expected) $ \tok ->
    if lexTokenKind tok == expected then Just () else Nothing

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

operatorLikeTok :: Text -> TokParser ()
operatorLikeTok expected =
  tokenSatisfy ("operator " <> show (T.unpack expected)) $ \tok ->
    case lexTokenKind tok of
      TkVarSym op | op == expected -> Just ()
      TkConSym op | op == expected -> Just ()
      TkQVarSym op | op == expected -> Just ()
      TkQConSym op | op == expected -> Just ()
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

tokenSatisfy :: String -> (LexToken -> Maybe a) -> TokParser a
tokenSatisfy label f =
  MP.label label $ do
    tok <- lookAhead anySingle
    case f tok of
      Just out -> out <$ anySingle
      Nothing -> fail label

moduleNameParser :: TokParser Text
moduleNameParser =
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
      _ -> Nothing

lowerIdentifierParser :: TokParser Text
lowerIdentifierParser =
  tokenSatisfy "lowercase identifier" $ \tok ->
    case lexTokenKind tok of
      TkVarId ident -> Just ident
      TkQVarId ident -> Just ident
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

identifierExact :: Text -> TokParser ()
identifierExact expected =
  tokenSatisfy ("identifier " <> show (T.unpack expected)) $ \tok ->
    case lexTokenKind tok of
      TkVarId ident | ident == expected -> Just ()
      TkConId ident | ident == expected -> Just ()
      -- Also match reserved keywords that are used as contextual identifiers
      TkKeywordClass | expected == "class" -> Just ()
      TkKeywordDefault | expected == "default" -> Just ()
      TkKeywordDeriving | expected == "deriving" -> Just ()
      TkKeywordForeign | expected == "foreign" -> Just ()
      TkKeywordInfix | expected == "infix" -> Just ()
      TkKeywordInfixl | expected == "infixl" -> Just ()
      TkKeywordInfixr | expected == "infixr" -> Just ()
      TkKeywordInstance | expected == "instance" -> Just ()
      TkKeywordNewtype | expected == "newtype" -> Just ()
      TkKeywordType | expected == "type" -> Just ()
      TkKeywordUnderscore | expected == "_" -> Just ()
      _ -> Nothing

operatorTextParser :: TokParser Text
operatorTextParser =
  tokenSatisfy "operator" $ \tok ->
    case lexTokenKind tok of
      TkVarSym op -> Just op
      TkConSym op -> Just op
      TkQVarSym op -> Just op
      TkQConSym op -> Just op
      _ -> Nothing

stringTextParser :: TokParser Text
stringTextParser =
  tokenSatisfy "string literal" $ \tok ->
    case lexTokenKind tok of
      TkString txt -> Just txt
      _ -> Nothing

withSpan :: TokParser (SourceSpan -> a) -> TokParser a
withSpan parser = do
  start <- MP.getSourcePos
  out <- parser
  out . sourceSpanFromPositions start <$> MP.getSourcePos

sourceSpanFromPositions :: SourcePos -> SourcePos -> SourceSpan
sourceSpanFromPositions start end =
  SourceSpan
    { sourceSpanStartLine = MP.unPos (sourceLine start),
      sourceSpanStartCol = MP.unPos (sourceColumn start),
      sourceSpanEndLine = MP.unPos (sourceLine end),
      sourceSpanEndCol = MP.unPos (sourceColumn end)
    }

markSingleParenConstraint :: [Constraint] -> [Constraint]
markSingleParenConstraint constraints =
  case constraints of
    [constraint] -> [constraint {constraintParen = True}]
    _ -> constraints

parens :: TokParser a -> TokParser a
parens parser = do
  symbolLikeTok "("
  res <- parser
  symbolLikeTok ")"
  pure res

skipSemicolons :: TokParser ()
skipSemicolons = MP.skipMany (symbolLikeTok ";")

bracedSemiSep :: TokParser a -> TokParser [a]
bracedSemiSep parser = do
  symbolLikeTok "{"
  skipSemicolons
  items <- parser `MP.sepEndBy` symbolLikeTok ";"
  symbolLikeTok "}"
  pure items

bracedSemiSep1 :: TokParser a -> TokParser [a]
bracedSemiSep1 parser = do
  symbolLikeTok "{"
  skipSemicolons
  items <- parser `MP.sepEndBy1` symbolLikeTok ";"
  symbolLikeTok "}"
  pure items

plainSemiSep1 :: TokParser a -> TokParser [a]
plainSemiSep1 parser = MP.some (parser <* skipSemicolons)

constraintParserWith :: TokParser Type -> TokParser Constraint
constraintParserWith typeAtomParser = withSpan $ do
  className <- constructorIdentifierParser
  args <- MP.many typeAtomParser
  pure $ \span' ->
    Constraint
      { constraintSpan = span',
        constraintClass = className,
        constraintArgs = args,
        constraintParen = False
      }

constraintsParserWith :: TokParser Type -> TokParser [Constraint]
constraintsParserWith typeAtomParser =
  MP.try (parens (markSingleParenConstraint <$> (constraintParserWith typeAtomParser `MP.sepEndBy` symbolLikeTok ",")))
    <|> fmap pure (constraintParserWith typeAtomParser)

contextParserWith :: TokParser Type -> TokParser [Constraint]
contextParserWith = constraintsParserWith

functionBindValue :: SourceSpan -> Text -> [Pattern] -> Rhs -> ValueDecl
functionBindValue span' name pats rhs =
  FunctionBind
    span'
    name
    [ Match
        { matchSpan = span',
          matchPats = pats,
          matchRhs = rhs
        }
    ]

functionBindDecl :: SourceSpan -> Text -> [Pattern] -> Rhs -> Decl
functionBindDecl span' name pats rhs =
  DeclValue span' (functionBindValue span' name pats rhs)

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
