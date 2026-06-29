{-# LANGUAGE OverloadedStrings #-}

module Aihc.Parser.Internal.Type
  ( typeParser,
    typeSignatureParser,
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
    arrowKindParser,
  )
where

import Aihc.Parser.Internal.Common
import {-# SOURCE #-} Aihc.Parser.Internal.Expr (exprParser)
import Aihc.Parser.Lex (LexTokenKind (..), lexTokenKind, lexTokenSpan, lexTokenText)
import Aihc.Parser.Syntax
import Data.Char (isLower)
import Data.Functor (($>))
import Data.Text qualified as T
import Text.Megaparsec ((<|>))
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

-- | Report core plus outer extension forms:
--
-- > type -> btype ['->' type]
typeParser :: TokParser Type
typeParser = label "type" $ forallTypeParser <|> contextOrKindSigTypeParser

-- | Type syntax accepted after expression signatures. This keeps the same
-- report core while allowing outer @forall@ and context forms.
typeSignatureParser :: TokParser Type
typeSignatureParser = label "type" $ forallSignatureTypeParser <|> contextOrFunSignatureTypeParser

-- | Extension form wrapping the report core with a kind annotation.
kindSigTypeParser :: TokParser Type
kindSigTypeParser =
  optionalSuffix
    (expectedTok TkReservedDoubleColon *> typeSignatureParser)
    TKindSig
    typeFunParser

-- | Extension form:
--
-- > forall a_1 ... a_n . type
forallSignatureTypeParser :: TokParser Type
forallSignatureTypeParser = withSpanAnn (TAnn . mkAnnotation) $ do
  telescope <- forallTelescopeParser
  attachForallBody telescope <$> typeSignatureParser

contextOrFunSignatureTypeParser :: TokParser Type
contextOrFunSignatureTypeParser = do
  isContextType <- startsWithContextType
  if isContextType then contextSignatureTypeParser else typeFunSignatureParser

contextSignatureTypeParser :: TokParser Type
contextSignatureTypeParser = do
  constraints <- contextItemsParserWith typeSignatureParser typeSignatureContextAtomParser
  expectedTok TkReservedDoubleArrow
  TContext constraints <$> typeSignatureParser

-- | Report core plus extension infix-type support:
--
-- > type -> btype ['->' type]
typeFunSignatureParser :: TokParser Type
typeFunSignatureParser = do
  lhs <- typeSignatureInfixParser
  mArrow <- MP.optional arrowKindParser
  case mArrow of
    Nothing -> pure lhs
    Just arrowKind -> TFun arrowKind lhs <$> typeSignatureParser

contextOrKindSigTypeParser :: TokParser Type
contextOrKindSigTypeParser = do
  isContextType <- startsWithContextType
  if isContextType then contextTypeParser else kindSigTypeParser

-- | Extension form:
--
-- > forall a_1 ... a_n . type
forallTypeParser :: TokParser Type
forallTypeParser = withSpanAnn (TAnn . mkAnnotation) $ do
  telescope <- forallTelescopeParser
  attachForallBody telescope <$> typeParser

attachForallBody :: ForallTelescope -> Type -> Type
attachForallBody telescope body =
  case body of
    TAnn ann (TKindSig ty kind) -> TAnn ann (TKindSig (TForall telescope ty) kind)
    TKindSig ty kind -> TKindSig (TForall telescope ty) kind
    _ -> TForall telescope body

-- | Extension form:
--
-- > forall -> 'forall' binder_1 ... binder_n ('.' | '->')
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
        ident <- tyVarNameParser
        mKind <- MP.optional (expectedTok TkReservedDoubleColon *> typeParser)
        expectedTok TkSpecialRBrace
        pure (\span' -> TyVarBinder [mkAnnotation span'] ident mKind TyVarBInferred TyVarBVisible)
    )
      <|> ( do
              expectedTok TkSpecialLParen
              ident <- tyVarNameParser
              expectedTok TkReservedDoubleColon
              kind <- typeParser
              expectedTok TkSpecialRParen
              pure (\span' -> TyVarBinder [mkAnnotation span'] ident (Just kind) TyVarBSpecified TyVarBVisible)
          )
      <|> ( do
              ident <- tyVarNameParser
              pure (\span' -> TyVarBinder [mkAnnotation span'] ident Nothing TyVarBSpecified TyVarBVisible)
          )

contextTypeParser :: TokParser Type
contextTypeParser = contextTypeParserWith typeParser

contextItemsParser :: TokParser [Type]
contextItemsParser = contextItemsParserWith typeParser typeAtomParser

contextTypeParserWith :: TokParser Type -> TokParser Type
contextTypeParserWith rhsParser = do
  constraints <- contextItemsParserWith rhsParser typeAtomParser
  expectedTok TkReservedDoubleArrow
  rhs <- rhsParser
  pure $
    case rhs of
      TAnn ann (TKindSig ty kind) -> TAnn ann (TKindSig (TContext constraints ty) kind)
      TKindSig ty kind -> TKindSig (TContext constraints ty) kind
      _ -> TContext constraints rhs

-- | Report core plus extension infix-type support:
--
-- > type -> btype ['->' type]
-- > btype -> [btype] atype
typeFunParser :: TokParser Type
typeFunParser = typeFunParserWith typeSignatureParser

typeFunParserWith :: TokParser Type -> TokParser Type
typeFunParserWith rhsParser = do
  lhs <- typeInfixParser
  mArrow <- MP.optional arrowKindParser
  case mArrow of
    Nothing -> pure lhs
    Just arrowKind -> TFun arrowKind lhs <$> rhsParser

-- | Parse an arrow annotation and consume the @->@ token.
-- Handles @->@ (unrestricted), @⊸@ (linear), @%1 ->@ (linear), and @%m ->@
-- (explicit multiplicity).  The @%@-prefixed forms are only attempted when
-- @LinearTypes@ is enabled.
arrowKindParser :: TokParser ArrowKind
arrowKindParser = do
  linearEnabled <- isExtensionEnabled LinearTypes
  (expectedTok TkReservedRightArrow $> ArrowUnrestricted)
    <|> (if linearEnabled then linearArrowKindParser else MP.empty)

linearArrowKindParser :: TokParser ArrowKind
linearArrowKindParser =
  (expectedTok TkLinearArrow $> ArrowLinear)
    <|> MP.try multiplicityAnnotatedArrowParser

-- | Parse @%<mult> ->@, consuming all three tokens.
-- Only matches when @%@ is a prefix operator (no space between @%@ and the
-- multiplicity expression), which is how GHC distinguishes @a %1 -> b@ (linear)
-- from @a % 1 -> b@ (type operator applied to @a@ and @1@, then unrestricted arrow).
multiplicityAnnotatedArrowParser :: TokParser ArrowKind
multiplicityAnnotatedArrowParser = do
  expectedTok TkPrefixPercent
  mult <- typeAtomParser
  expectedTok TkReservedRightArrow
  pure (multiplicityToArrowKind mult)

multiplicityToArrowKind :: Type -> ArrowKind
multiplicityToArrowKind ty =
  case peelTypeAnn ty of
    TTypeLit (TypeLitInteger 1 _) -> ArrowLinear
    _ -> ArrowExplicit ty

-- | Extension layer over the report's @btype@ chain:
--
-- > btype -> [btype] atype
typeInfixParser :: TokParser Type
typeInfixParser = do
  lhs <- typeAppParser
  rest <- MP.many ((,) <$> typeInfixOperatorParser <*> typeInfixRhsParser)
  pure (foldInfixR buildInfixType lhs rest)

typeInfixRhsParser :: TokParser Type
typeInfixRhsParser = do
  rhs <- typeAppParser
  rejectBareTypeImplicitParam rhs

typeSignatureInfixParser :: TokParser Type
typeSignatureInfixParser = do
  lhs <- typeSignatureAppParser
  rest <- MP.many ((,) <$> typeInfixOperatorParser <*> typeSignatureInfixRhsParser)
  pure (foldInfixR buildInfixType lhs rest)

typeSignatureInfixRhsParser :: TokParser Type
typeSignatureInfixRhsParser = do
  rhs <- typeSignatureAppParser
  rejectBareSignatureImplicitParam rhs

-- | Parse a type head that may contain infix operators but NOT type applications.
-- Used for type family heads like @type family l `And` r@ where the head is
-- an infix type, but we don't want to consume trailing type parameters.
typeHeadInfixParser :: TokParser Type
typeHeadInfixParser = do
  lhs <- typeAtomParser
  foldInfixR buildInfixType lhs <$> typeHeadInfixLoopParser

typeHeadInfixLoopParser :: TokParser [((Name, TypePromotion), Type)]
typeHeadInfixLoopParser = MP.many $ MP.try $ do
  op <- typeInfixOperatorParser
  atom <- typeAtomParser
  pure (op, atom)

buildInfixType :: Type -> ((Name, TypePromotion), Type) -> Type
buildInfixType lhs ((op, promoted), rhs) = TInfix lhs op promoted rhs

typeInfixOperatorParser :: TokParser (Name, TypePromotion)
typeInfixOperatorParser =
  promotedInfixOperatorParser
    <|> backtickTypeOperatorParser
    <|> unpromotedInfixOperatorParser
  where
    unpromotedInfixOperatorParser =
      tokenSatisfy "type infix operator" $ \tok ->
        case lexTokenKind tok of
          TkReservedColon -> Just (qualifyName Nothing (mkUnqualifiedNameAt tok NameConSym ":"), Unpromoted)
          TkVarSym op
            | op /= "."
                && op /= "!"
                && op /= "'" ->
                Just (qualifyName Nothing (mkUnqualifiedNameAt tok NameVarSym op), Unpromoted)
          TkConSym op -> Just (qualifyName Nothing (mkUnqualifiedNameAt tok NameConSym op), Unpromoted)
          TkQVarSym modName op -> Just (mkNameAt tok (Just modName) NameVarSym op, Unpromoted)
          TkQConSym modName op -> Just (mkNameAt tok (Just modName) NameConSym op, Unpromoted)
          _ -> Nothing

    backtickTypeOperatorParser = MP.try $ do
      expectedTok TkSpecialBacktick
      op <- typeOperatorIdentifierParser
      expectedTok TkSpecialBacktick
      pure (op, Unpromoted)

    typeOperatorIdentifierParser =
      tokenSatisfy "type operator identifier" $ \tok ->
        case lexTokenKind tok of
          TkVarId name -> Just (qualifyName Nothing (mkUnqualifiedNameAt tok NameVarId name))
          TkConId name -> Just (qualifyName Nothing (mkUnqualifiedNameAt tok NameConId name))
          TkQVarId modName name -> Just (mkNameAt tok (Just modName) NameVarId name)
          TkQConId modName name -> Just (mkNameAt tok (Just modName) NameConId name)
          _ -> Nothing

    promotedInfixOperatorParser = MP.try $ do
      -- Accept both TkVarSym "'" and TkTHQuoteTick for promoted operators
      expectedTok (TkVarSym "'") <|> expectedTok TkTHQuoteTick
      -- After the quote, accept any symbolic infix operator (e.g., ': for promoted cons,
      -- or ':$$: for a promoted user-defined type operator)
      tokenSatisfy "promoted type infix operator" $ \tok ->
        case lexTokenKind tok of
          TkReservedColon -> Just (qualifyName Nothing (mkUnqualifiedNameAt tok NameConSym ":"), Promoted)
          TkVarSym sym
            | sym /= "." && sym /= "!" ->
                Just (qualifyName Nothing (mkUnqualifiedNameAt tok NameVarSym sym), Promoted)
          TkConSym sym -> Just (qualifyName Nothing (mkUnqualifiedNameAt tok NameConSym sym), Promoted)
          TkQVarSym modQual sym -> Just (mkNameAt tok (Just modQual) NameVarSym sym, Promoted)
          TkQConSym modQual sym -> Just (mkNameAt tok (Just modQual) NameConSym sym, Promoted)
          _ -> Nothing

-- | Report core:
--
-- > btype -> [btype] atype
typeAppParser :: TokParser Type
typeAppParser = do
  first <- typeAtomParser
  rest <- MP.many typeAppArgParser
  pure (foldl applyTypeAppArg first rest)

typeSignatureAppParser :: TokParser Type
typeSignatureAppParser = do
  first <- typeSignatureAtomParser
  rest <- MP.many typeSignatureAppArgParser
  pure (foldl applyTypeAppArg first rest)

buildTypeApp :: Type -> Type -> Type
buildTypeApp = TApp

typeAppArgParser :: TokParser (Either Type Type)
typeAppArgParser =
  (Left <$> MP.try invisibleTypeAppParser)
    <|> (Right <$> (typeAtomParser >>= rejectBareTypeImplicitParam))

invisibleTypeAppParser :: TokParser Type
invisibleTypeAppParser = do
  expectedTok TkTypeApp
  typeAtomParser >>= rejectBareTypeImplicitParam

typeSignatureAppArgParser :: TokParser (Either Type Type)
typeSignatureAppArgParser =
  (Left <$> MP.try invisibleSignatureTypeAppParser)
    <|> (Right <$> (typeSignatureAtomParser >>= rejectBareSignatureImplicitParam))

typeSignatureContextAtomParser :: TokParser Type
typeSignatureContextAtomParser = typeSignatureAtomParser >>= rejectBareSignatureImplicitParam

rejectBareSignatureImplicitParam :: Type -> TokParser Type
rejectBareSignatureImplicitParam = rejectBareTypeImplicitParam

rejectBareTypeImplicitParam :: Type -> TokParser Type
rejectBareTypeImplicitParam ty =
  case peelTypeAnn ty of
    TImplicitParam {} -> fail "implicit parameter type must be parenthesized"
    _ -> pure ty

invisibleSignatureTypeAppParser :: TokParser Type
invisibleSignatureTypeAppParser = do
  expectedTok TkTypeApp
  typeSignatureAtomParser >>= rejectBareSignatureImplicitParam

applyTypeAppArg :: Type -> Either Type Type -> Type
applyTypeAppArg fn (Left ty) = TTypeApp fn ty
applyTypeAppArg fn (Right ty) = TApp fn ty

-- | Report core:
--
-- > atype -> gtycon
-- >       | tyvar
-- >       | '(' type_1 ',' ... ',' type_k ')'
-- >       | '[' type ']'
-- >       | '(' type ')'
--
-- This parser also admits extension atoms such as promoted types,
-- quasi-quotes, splices, wildcards, and implicit-parameter types.
typeAtomParser :: TokParser Type
typeAtomParser = typeAtomParserByToken

typeSignatureAtomParser :: TokParser Type
typeSignatureAtomParser = typeAtomParserByToken

typeAtomParserByToken :: TokParser Type
typeAtomParserByToken = do
  tok <- MP.lookAhead MP.anySingle
  case lexTokenKind tok of
    TkInteger {} -> typeLiteralTypeParser
    TkString {} -> typeLiteralTypeParser
    TkChar {} -> typeLiteralTypeParser
    TkQuasiQuote {} -> typeQuasiQuoteParser
    TkTHSplice -> do
      thAny <- thAnyEnabled
      if thAny then thSpliceTypeParser else typeAtomParserAlternatives False False
    TkImplicitParam {} -> do
      ipEnabled <- isExtensionEnabled ImplicitParams
      if ipEnabled then typeImplicitParamParser else typeAtomParserAlternatives False False
    TkSpecialLBracket -> typeListParser
    TkSpecialLParen -> MP.try typeParenOperatorParser <|> typeParenOrTupleParser
    TkSpecialUnboxedLParen -> typeParenOrTupleParser
    TkKeywordUnderscore -> typeWildcardParser
    TkVarId {} -> typeIdentifierParser
    TkConId {} -> typeIdentifierParser
    TkQVarId {} -> typeIdentifierParser
    TkQConId {} -> typeIdentifierParser
    _ -> do
      thAny <- thAnyEnabled
      ipEnabled <- isExtensionEnabled ImplicitParams
      typeAtomParserAlternatives thAny ipEnabled

typeAtomParserAlternatives :: Bool -> Bool -> TokParser Type
typeAtomParserAlternatives thAny ipEnabled =
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

-- | Parse an implicit parameter type.
--
-- The body uses 'typeSignatureParser' so @?x :: (T :: K)@ is accepted
-- through the parenthesized atom grammar, but @?x :: T :: K@ is left for an
-- outer kind-signature parser in a plain type context and rejected in a
-- signature context.
typeImplicitParamParser :: TokParser Type
typeImplicitParamParser = withSpanAnn (TAnn . mkAnnotation) $ do
  name <- implicitParamNameParser
  expectedTok TkReservedDoubleColon
  TImplicitParam name <$> typeSignatureParser

typeWildcardParser :: TokParser Type
typeWildcardParser =
  tokenSatisfy "wildcard" $ \tok ->
    case lexTokenKind tok of
      TkKeywordUnderscore -> Just (TAnn (mkAnnotation (lexTokenSpan tok)) TWildcard)
      _ -> Nothing

typeLiteralTypeParser :: TokParser Type
typeLiteralTypeParser =
  tokenSatisfy "type literal" $ \tok ->
    case lexTokenKind tok of
      TkInteger n _ -> Just (TAnn (mkAnnotation (lexTokenSpan tok)) (TTypeLit (TypeLitInteger n (lexTokenText tok))))
      TkString s -> Just (TAnn (mkAnnotation (lexTokenSpan tok)) (TTypeLit (TypeLitSymbol s (lexTokenText tok))))
      TkChar c -> Just (TAnn (mkAnnotation (lexTokenSpan tok)) (TTypeLit (TypeLitChar c (lexTokenText tok))))
      _ -> Nothing

promotedTypeParser :: TokParser Type
promotedTypeParser = withSpanAnn (TAnn . mkAnnotation) $ do
  -- Accept both TkVarSym "'" and TkTHQuoteTick for promoted types
  -- This handles ambiguity between TH value quotes and promoted types
  expectedTok (TkVarSym "'") <|> expectedTok TkTHQuoteTick
  ty <- typeAtomParser
  maybe (fail "promoted type") pure (markTypePromoted ty)

typeParenOperatorParser :: TokParser Type
typeParenOperatorParser = withSpanAnn (TAnn . mkAnnotation) $ do
  expectedTok TkSpecialLParen
  starIsType <- isExtensionEnabled StarIsType
  unicodeSyntax <- isExtensionEnabled UnicodeSyntax
  op <- tokenSatisfy "type operator" $ \tok ->
    case lexTokenKind tok of
      TkVarSym sym | not (isStarTypeSymbol starIsType unicodeSyntax sym) -> Just (qualifyName Nothing (mkUnqualifiedNameAt tok NameVarSym sym))
      TkConSym sym | not (isStarTypeSymbol starIsType unicodeSyntax sym) -> Just (qualifyName Nothing (mkUnqualifiedNameAt tok NameConSym sym))
      TkQVarSym modQual sym -> Just (mkNameAt tok (Just modQual) NameVarSym sym)
      TkQConSym modQual sym -> Just (mkNameAt tok (Just modQual) NameConSym sym)
      -- Handle reserved operators that can be used as type constructors
      TkReservedRightArrow -> Just (qualifyName Nothing (mkUnqualifiedNameAt tok NameVarSym "->"))
      TkReservedColon -> Just (qualifyName Nothing (mkUnqualifiedNameAt tok NameConSym ":"))
      -- Note: ~ is now lexed as TkVarSym "~" so TkVarSym case handles it
      _ -> Nothing
  expectedTok TkSpecialRParen
  pure $
    case (nameQualifier op, nameType op, nameText op) of
      (Nothing, NameVarSym, "->") -> TBuiltinCon TBuiltinArrow
      (Nothing, NameConSym, ":") -> TBuiltinCon TBuiltinCons
      _ -> TCon op Unpromoted

typeQuasiQuoteParser :: TokParser Type
typeQuasiQuoteParser =
  tokenSatisfy "type quasi quote" $ \tok ->
    case lexTokenKind tok of
      TkQuasiQuote quoter body -> Just (typeAnnSpan (lexTokenSpan tok) (TQuasiQuote quoter body))
      _ -> Nothing

typeIdentifierParser :: TokParser Type
typeIdentifierParser = do
  (tok, name) <- identifierNameWithTokenParser
  pure $
    TAnn (mkAnnotation (lexTokenSpan tok)) $
      case (nameQualifier name, nameType name, T.uncons (nameText name)) of
        (Nothing, NameVarId, Just (c, _))
          | isLower c || c == '_' ->
              TVar (nameToUnqualified name)
        _ -> TCon name Unpromoted

typeStarParser :: TokParser Type
typeStarParser = do
  starIsType <- isExtensionEnabled StarIsType
  unicodeSyntax <- isExtensionEnabled UnicodeSyntax
  if starIsType
    then tokenSatisfy "star kind" $ \tok ->
      case lexTokenKind tok of
        TkVarSym sym | isStarTypeSymbol starIsType unicodeSyntax sym -> Just (TAnn (mkAnnotation (lexTokenSpan tok)) (TStar (lexTokenText tok)))
        TkConSym sym | isStarTypeSymbol starIsType unicodeSyntax sym -> Just (TAnn (mkAnnotation (lexTokenSpan tok)) (TStar (lexTokenText tok)))
        _ -> Nothing
    else MP.empty

isStarTypeSymbol :: Bool -> Bool -> T.Text -> Bool
isStarTypeSymbol starIsType unicodeSyntax sym =
  starIsType && (sym == "*" || (unicodeSyntax && sym == "★"))

typeListParser :: TokParser Type
typeListParser = withSpanAnn (TAnn . mkAnnotation) $ do
  expectedTok TkSpecialLBracket
  mClosed <- MP.optional (expectedTok TkSpecialRBracket)
  case mClosed of
    Just () -> pure (TBuiltinCon TBuiltinList)
    Nothing -> do
      elems <- typeParser `MP.sepBy1` expectedTok TkSpecialComma
      expectedTok TkSpecialRBracket
      pure (TList Unpromoted elems)

typeParenOrTupleParser :: TokParser Type
typeParenOrTupleParser = withSpanAnn (TAnn . mkAnnotation) $ do
  (tupleFlavor, closeTok) <- tupleDelimsParser
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
      case tupleFlavor of
        Boxed -> pure (TBuiltinCon (TBuiltinTuple arity))
        Unboxed -> do
          let tupleConName = "(#" <> T.replicate (arity - 1) "," <> "#)"
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
                  case tupleFlavor of
                    Boxed -> pure (TParen first)
                    Unboxed -> pure (TTuple Unboxed Unpromoted [first])
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
    TBuiltinCon con -> Just (promoteBuiltinCon con)
    TList _ elems -> Just (TList Promoted elems)
    TTuple tupleFlavor _ elems -> Just (TTuple tupleFlavor Promoted elems)
    TTypeApp fn arg -> TTypeApp <$> markTypePromoted fn <*> pure arg
    _ -> Nothing

promoteBuiltinCon :: TypeBuiltinCon -> Type
promoteBuiltinCon con =
  TCon
    ( qualifyName Nothing $
        case con of
          TBuiltinTuple arity -> mkUnqualifiedName NameConId ("(" <> T.replicate (max 0 (arity - 1)) "," <> ")")
          TBuiltinArrow -> mkUnqualifiedName NameVarSym "->"
          TBuiltinList -> mkUnqualifiedName NameConId "[]"
          TBuiltinCons -> mkUnqualifiedName NameConSym ":"
    )
    Promoted
