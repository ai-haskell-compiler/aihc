{-# LANGUAGE OverloadedStrings #-}

module Aihc.Parser.Internal.Type
  ( typeParser,
    forallTelescopeParser,
    typeInfixParser,
    typeInfixOperatorParser,
    typeHeadInfixParser,
    typeAppParser,
    buildTypeApp,
    buildInfixType,
    typeAtomParser,
    contextItemsParser,
    thSpliceTypeParser,
  )
where

import Aihc.Parser.Internal.Common
import {-# SOURCE #-} Aihc.Parser.Internal.Expr (exprParser)
import Aihc.Parser.Lex (LexToken (..), LexTokenKind (..), lexTokenKind, lexTokenSpan, lexTokenText)
import Aihc.Parser.Syntax
import Data.Char (isLower)
import Data.Functor (($>))
import Data.Text (Text)
import Data.Text qualified as T
import Text.Megaparsec (anySingle, (<|>))
import Text.Megaparsec qualified as MP

-- | Parse a Template Haskell type splice: $typ or $(typ)
thSpliceTypeParser :: TokParser Type
thSpliceTypeParser = withSpanAnn (TAnn . mkAnnotation) $ do
  expectedTok TkTHSplice
  body <- parenSpliceBody <|> bareSpliceBody
  pure (TSplice body)
  where
    parenSpliceBody = withSpanAnn (EAnn . mkAnnotation) $ do
      body <- parens exprParser
      pure (EParen body)
    bareSpliceBody = withSpanAnn (EAnn . mkAnnotation) $ do
      EVar <$> identifierNameParser

typeParser :: TokParser Type
typeParser = label "type" $ forallTypeParser <|> kindSigTypeParser

kindSigTypeParser :: TokParser Type
kindSigTypeParser = do
  ty <- contextOrFunTypeParser
  mKind <- MP.optional (expectedTok TkReservedDoubleColon *> typeParser)
  pure $
    case mKind of
      Just kind -> TKindSig ty kind
      Nothing -> ty

contextOrFunTypeParser :: TokParser Type
contextOrFunTypeParser = do
  isContextType <- startsWithContextType
  if isContextType then contextTypeParser else typeFunParser

forallTypeParser :: TokParser Type
forallTypeParser = withSpanAnn (TAnn . mkAnnotation) $ do
  telescope <- forallTelescopeParser
  TForall telescope <$> typeParser

forallTelescopeParser :: TokParser ForallTelescope
forallTelescopeParser = do
  expectedTok TkKeywordForall
  binders <- MP.some forallBinderParser
  visibility <-
    (expectedTok (TkVarSym ".") $> ForallInvisible)
      <|> (expectedTok TkReservedRightArrow $> ForallVisible)
  pure (ForallTelescope visibility binders)

-- | Parse a single forall binder: {k} | (k :: *) | k
forallBinderParser :: TokParser TyVarBinder
forallBinderParser =
  withSpan $
    -- Inferred binder: {k} | {k :: Type}
    ( do
        expectedTok TkSpecialLBrace
        ident <- forallBinderNameParser
        mKind <- MP.optional (expectedTok TkReservedDoubleColon *> typeParser)
        expectedTok TkSpecialRBrace
        pure (\span' -> TyVarBinder [mkAnnotation span'] ident mKind TyVarBInferred TyVarBVisible)
    )
      <|> ( do
              expectedTok TkSpecialLParen
              ident <- forallBinderNameParser
              expectedTok TkReservedDoubleColon
              kind <- typeParser
              expectedTok TkSpecialRParen
              pure (\span' -> TyVarBinder [mkAnnotation span'] ident (Just kind) TyVarBSpecified TyVarBVisible)
          )
      <|> ( do
              ident <- forallBinderNameParser
              pure (\span' -> TyVarBinder [mkAnnotation span'] ident Nothing TyVarBSpecified TyVarBVisible)
          )

forallBinderNameParser :: TokParser Text
forallBinderNameParser =
  lowerIdentifierParser
    <|> (expectedTok TkKeywordUnderscore $> "_")

contextTypeParser :: TokParser Type
contextTypeParser = do
  constraints <- contextItemsParser
  expectedTok TkReservedDoubleArrow
  TContext constraints <$> typeParser

contextItemsParser :: TokParser [Type]
contextItemsParser = contextItemsParserWith typeParser typeAtomParser

typeFunParser :: TokParser Type
typeFunParser = do
  lhs <- typeInfixParser
  mRhs <- MP.optional (expectedTok TkReservedRightArrow *> typeParser)
  pure $
    case mRhs of
      Just rhs -> TFun lhs rhs
      Nothing -> lhs

typeInfixParser :: TokParser Type
typeInfixParser = do
  lhs <- typeAppParser
  rest <- MP.many ((,) <$> typeInfixOperatorParser <*> typeAppParser)
  pure (foldl buildInfixType lhs rest)

-- | Parse a type head that may contain infix operators but NOT type applications.
-- Used for type family heads like @type family l `And` r@ where the head is
-- an infix type, but we don't want to consume trailing type parameters.
typeHeadInfixParser :: TokParser Type
typeHeadInfixParser = do
  lhs <- typeAtomParser
  foldl buildInfixType lhs <$> typeHeadInfixLoopParser

typeHeadInfixLoopParser :: TokParser [((Name, TypePromotion), Type)]
typeHeadInfixLoopParser = MP.many $ MP.try $ do
  op <- typeInfixOperatorParser
  atom <- typeAtomParser
  pure (op, atom)

buildInfixType :: Type -> ((Name, TypePromotion), Type) -> Type
buildInfixType lhs ((op, promoted), rhs) =
  let opType = TCon op promoted
   in TApp (TApp opType lhs) rhs

typeInfixOperatorParser :: TokParser (Name, TypePromotion)
typeInfixOperatorParser =
  promotedInfixOperatorParser
    <|> backtickTypeOperatorParser
    <|> unpromotedInfixOperatorParser
  where
    unpromotedInfixOperatorParser =
      tokenSatisfy "type infix operator" $ \tok ->
        case lexTokenKind tok of
          TkReservedColon -> Just (qualifyName Nothing (mkUnqualifiedName NameConSym ":"), Unpromoted)
          TkVarSym op
            | op /= "."
                && op /= "!"
                && op /= "'" ->
                Just (qualifyName Nothing (mkUnqualifiedName NameVarSym op), Unpromoted)
          TkConSym op -> Just (qualifyName Nothing (mkUnqualifiedName NameConSym op), Unpromoted)
          TkQVarSym modName op -> Just (mkName (Just modName) NameVarSym op, Unpromoted)
          TkQConSym modName op -> Just (mkName (Just modName) NameConSym op, Unpromoted)
          _ -> Nothing

    backtickTypeOperatorParser = MP.try $ do
      expectedTok TkSpecialBacktick
      op <- typeOperatorIdentifierParser
      expectedTok TkSpecialBacktick
      pure (op, Unpromoted)

    typeOperatorIdentifierParser =
      tokenSatisfy "type operator identifier" $ \tok ->
        case lexTokenKind tok of
          TkVarId name -> Just (qualifyName Nothing (mkUnqualifiedName NameVarId name))
          TkConId name -> Just (qualifyName Nothing (mkUnqualifiedName NameConId name))
          _ -> Nothing

    promotedInfixOperatorParser = MP.try $ do
      -- Accept both TkVarSym "'" and TkTHQuoteTick for promoted operators
      expectedTok (TkVarSym "'") <|> expectedTok TkTHQuoteTick
      -- After the quote, accept any symbolic infix operator (e.g., ': for promoted cons,
      -- or ':$$: for a promoted user-defined type operator)
      tokenSatisfy "promoted type infix operator" $ \tok ->
        case lexTokenKind tok of
          TkReservedColon -> Just (qualifyName Nothing (mkUnqualifiedName NameConSym ":"), Promoted)
          TkVarSym sym
            | sym /= "." && sym /= "!" ->
                Just (qualifyName Nothing (mkUnqualifiedName NameVarSym sym), Promoted)
          TkConSym sym -> Just (qualifyName Nothing (mkUnqualifiedName NameConSym sym), Promoted)
          TkQVarSym modQual sym -> Just (mkName (Just modQual) NameVarSym sym, Promoted)
          TkQConSym modQual sym -> Just (mkName (Just modQual) NameConSym sym, Promoted)
          _ -> Nothing

typeAppParser :: TokParser Type
typeAppParser = do
  first <- typeAtomParser
  rest <- MP.many typeAtomParser
  pure (foldl buildTypeApp first rest)

buildTypeApp :: Type -> Type -> Type
buildTypeApp = TApp

typeAtomParser :: TokParser Type
typeAtomParser = do
  thEnabled <- isExtensionEnabled TemplateHaskellQuotes
  thFullEnabled <- isExtensionEnabled TemplateHaskell
  ipEnabled <- isExtensionEnabled ImplicitParams
  let thAny = thEnabled || thFullEnabled
  MP.try promotedTypeParser
    <|> typeLiteralTypeParser
    <|> typeQuasiQuoteParser
    <|> (if thAny then thSpliceTypeParser else MP.empty)
    <|> (if ipEnabled then typeImplicitParamParser else MP.empty)
    <|> typeListParser
    <|> MP.try typeParenOperatorParser
    <|> typeParenOrTupleParser
    <|> typeStarParser
    <|> typeWildcardParser
    <|> typeIdentifierParser

-- | Parse an implicit parameter type: @?name :: Type@
typeImplicitParamParser :: TokParser Type
typeImplicitParamParser = withSpanAnn (TAnn . mkAnnotation) $ do
  name <- implicitParamNameParser
  expectedTok TkReservedDoubleColon
  TImplicitParam name <$> typeParser

typeWildcardParser :: TokParser Type
typeWildcardParser = withSpanAnn (TAnn . mkAnnotation) $ do
  expectedTok TkKeywordUnderscore
  pure TWildcard

typeLiteralTypeParser :: TokParser Type
typeLiteralTypeParser = withSpanAnn (TAnn . mkAnnotation) $ do
  lit <- tokenSatisfy "type literal" $ \tok ->
    case lexTokenKind tok of
      TkInteger n -> Just (TypeLitInteger n (lexTokenText tok))
      TkIntegerBase n _ -> Just (TypeLitInteger n (lexTokenText tok))
      TkString s -> Just (TypeLitSymbol s (lexTokenText tok))
      TkChar c -> Just (TypeLitChar c (lexTokenText tok))
      _ -> Nothing
  pure (TTypeLit lit)

promotedTypeParser :: TokParser Type
promotedTypeParser = withSpanAnn (TAnn . mkAnnotation) $ do
  -- Accept both TkVarSym "'" and TkTHQuoteTick for promoted types
  -- This handles ambiguity between TH value quotes and promoted types
  expectedTok (TkVarSym "'") <|> expectedTok TkTHQuoteTick
  MP.try promotedStructuredTypeParser <|> promotedRawTypeParser

promotedStructuredTypeParser :: TokParser Type
promotedStructuredTypeParser = do
  ty <-
    MP.try typeListParser
      <|> MP.try typeParenOrTupleParser
      <|> MP.try typeParenOperatorParser
      <|> typeIdentifierParser
  maybe (fail "promoted type") pure (markTypePromoted ty)

promotedRawTypeParser :: TokParser Type
promotedRawTypeParser = withSpanAnn (TAnn . mkAnnotation) $ do
  suffix <- promotedBracketedSuffixParser <|> promotedParenthesizedSuffixParser
  pure (TCon (qualifyName Nothing (mkUnqualifiedName NameConId suffix)) Promoted)

promotedBracketedSuffixParser :: TokParser Text
promotedBracketedSuffixParser = collectDelimitedRaw TkSpecialLBracket TkSpecialRBracket

promotedParenthesizedSuffixParser :: TokParser Text
promotedParenthesizedSuffixParser = collectDelimitedRaw TkSpecialLParen TkSpecialRParen

collectDelimitedRaw :: LexTokenKind -> LexTokenKind -> TokParser Text
collectDelimitedRaw openKind closeKind = do
  openTxt <- tokenSatisfy ("opening delimiter " <> show openKind) $ \tok ->
    if lexTokenKind tok == openKind then Just (lexTokenText tok) else Nothing
  go 1 openTxt
  where
    go :: Int -> Text -> TokParser Text
    go depth acc = do
      tok <- anySingle
      let kind = lexTokenKind tok
          txt = lexTokenText tok
          acc' = acc <> txt
      case () of
        _
          | kind == openKind -> go (depth + 1) acc'
          | kind == closeKind ->
              if depth == 1
                then pure acc'
                else go (depth - 1) acc'
          | otherwise -> go depth acc'

typeParenOperatorParser :: TokParser Type
typeParenOperatorParser = withSpanAnn (TAnn . mkAnnotation) $ do
  expectedTok TkSpecialLParen
  starIsType <- isExtensionEnabled StarIsType
  op <- tokenSatisfy "type operator" $ \tok ->
    case lexTokenKind tok of
      TkVarSym sym | not starIsType || sym /= "*" -> Just (qualifyName Nothing (mkUnqualifiedName NameVarSym sym))
      TkConSym sym | not starIsType || sym /= "*" -> Just (qualifyName Nothing (mkUnqualifiedName NameConSym sym))
      TkQVarSym modQual sym -> Just (mkName (Just modQual) NameVarSym sym)
      TkQConSym modQual sym -> Just (mkName (Just modQual) NameConSym sym)
      -- Handle reserved operators that can be used as type constructors
      TkReservedRightArrow -> Just (qualifyName Nothing (mkUnqualifiedName NameVarSym "->"))
      -- Note: ~ is now lexed as TkVarSym "~" so TkVarSym case handles it
      _ -> Nothing
  expectedTok TkSpecialRParen
  pure (TCon op Unpromoted)

typeQuasiQuoteParser :: TokParser Type
typeQuasiQuoteParser =
  tokenSatisfy "type quasi quote" $ \tok ->
    case lexTokenKind tok of
      TkQuasiQuote quoter body -> Just (typeAnnSpan (lexTokenSpan tok) (TQuasiQuote quoter body))
      _ -> Nothing

typeIdentifierParser :: TokParser Type
typeIdentifierParser = withSpanAnn (TAnn . mkAnnotation) $ do
  name <- identifierNameParser
  pure $
    case (nameQualifier name, nameType name, T.uncons (nameText name)) of
      (Nothing, NameVarId, Just (c, _))
        | isLower c || c == '_' ->
            TVar (mkUnqualifiedName NameVarId (nameText name))
      _ -> TCon name Unpromoted

typeStarParser :: TokParser Type
typeStarParser = withSpanAnn (TAnn . mkAnnotation) $ do
  starIsType <- isExtensionEnabled StarIsType
  if starIsType
    then do
      expectedTok (TkVarSym "*")
      pure TStar
    else MP.empty

typeListParser :: TokParser Type
typeListParser = withSpanAnn (TAnn . mkAnnotation) $ do
  expectedTok TkSpecialLBracket
  mClosed <- MP.optional (expectedTok TkSpecialRBracket)
  case mClosed of
    Just () -> pure (TCon (qualifyName Nothing (mkUnqualifiedName NameConId "[]")) Unpromoted)
    Nothing -> do
      elems <- typeParser `MP.sepBy1` expectedTok TkSpecialComma
      expectedTok TkSpecialRBracket
      pure (TList Unpromoted elems)

typeParenOrTupleParser :: TokParser Type
typeParenOrTupleParser = withSpanAnn (TAnn . mkAnnotation) $ do
  (tupleFlavor, closeTok) <-
    (expectedTok TkSpecialLParen $> (Boxed, TkSpecialRParen))
      <|> (expectedTok TkSpecialUnboxedLParen $> (Unboxed, TkSpecialUnboxedRParen))
  mClosed <- MP.optional (expectedTok closeTok)
  case mClosed of
    Just () -> pure (TTuple tupleFlavor Unpromoted [])
    Nothing -> do
      MP.try (tupleConstructorParser tupleFlavor closeTok) <|> parenthesizedTypeOrTupleParser tupleFlavor closeTok
  where
    tupleConstructorParser tupleFlavor closeTok = do
      _ <- expectedTok TkSpecialComma
      moreCommas <- MP.many (expectedTok TkSpecialComma)
      expectedTok closeTok
      let arity = 2 + length moreCommas
          tupleConName =
            case tupleFlavor of
              Boxed -> "(" <> T.replicate (arity - 1) "," <> ")"
              Unboxed -> "(#" <> T.replicate (arity - 1) "," <> "#)"
      pure (TCon (qualifyName Nothing (mkUnqualifiedName NameConId tupleConName)) Unpromoted)

    parenthesizedTypeOrTupleParser tupleFlavor closeTok = do
      first <- typeParser
      mKind <- if tupleFlavor == Boxed then MP.optional (expectedTok TkReservedDoubleColon *> typeParser) else pure Nothing
      case mKind of
        Just kind -> do
          expectedTok closeTok
          pure (TParen (TKindSig first kind))
        Nothing -> do
          mComma <- MP.optional (expectedTok TkSpecialComma)
          case mComma of
            Nothing -> do
              -- Check for pipe (unboxed sum type)
              mPipe <- if tupleFlavor == Unboxed then MP.optional (expectedTok TkReservedPipe) else pure Nothing
              case mPipe of
                Just () -> do
                  -- (# Type1 | Type2 | ... #) - unboxed sum type
                  rest <- typeParser `MP.sepBy1` expectedTok TkReservedPipe
                  expectedTok closeTok
                  pure (TUnboxedSum (first : rest))
                Nothing -> do
                  expectedTok closeTok
                  if tupleFlavor == Boxed
                    then pure (TParen first)
                    else fail "not an unboxed tuple type"
            Just () -> do
              second <- typeParser
              more <- MP.many (expectedTok TkSpecialComma *> typeParser)
              expectedTok closeTok
              pure (TTuple tupleFlavor Unpromoted (first : second : more))

markTypePromoted :: Type -> Maybe Type
markTypePromoted ty =
  case ty of
    TAnn ann sub
      | Just inner <- markTypePromoted sub ->
          Just (TAnn ann inner)
    TCon name _ -> Just (TCon name Promoted)
    TList _ elems -> Just (TList Promoted elems)
    TTuple tupleFlavor _ elems -> Just (TTuple tupleFlavor Promoted elems)
    _ -> Nothing
