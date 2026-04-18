{-# LANGUAGE OverloadedStrings #-}

module Aihc.Parser.Internal.Pattern
  ( patternParser,
    simplePatternParser,
    appPatternParser,
    lambdaCasePatternParser,
    literalParser,
  )
where

import Aihc.Parser.Internal.CheckPattern (checkPattern)
import Aihc.Parser.Internal.Common
import {-# SOURCE #-} Aihc.Parser.Internal.Expr (exprParser)
import Aihc.Parser.Internal.Type (typeParser)
import Aihc.Parser.Lex (LexToken (..), LexTokenKind (..), lexTokenKind, lexTokenText)
import Aihc.Parser.Syntax
import Data.Functor (($>))
import Text.Megaparsec (anySingle, lookAhead, (<|>))
import Text.Megaparsec qualified as MP

patternParser :: TokParser Pattern
patternParser = label "pattern" $ do
  pat <- infixPatternParser
  mTypeSig <- MP.optional (expectedTok TkReservedDoubleColon *> typeParser)
  case mTypeSig of
    Just ty -> pure (PTypeSig pat ty)
    Nothing -> pure pat

infixPatternParser :: TokParser Pattern
infixPatternParser = do
  lhs <- asOrAppPatternParser
  rest <- MP.many ((,) <$> conOperatorParser <*> asOrAppPatternParser)
  pure (foldl buildInfixPattern lhs rest)

-- | Parse either an as-pattern (name@atom) or an application pattern.
-- As-patterns bind tighter than infix but looser than application,
-- so they appear as operands of infix patterns.
asOrAppPatternParser :: TokParser Pattern
asOrAppPatternParser = do
  isAsPattern <- startsWithAsPattern
  if isAsPattern
    then withSpanAnn (PAnn . mkAnnotation) $ do
      name <- identifierTextParser
      expectedTok TkReservedAt
      PAs name <$> patternAtomParser
    else appPatternParser

buildInfixPattern :: Pattern -> (Name, Pattern) -> Pattern
buildInfixPattern lhs (op, rhs) =
  PInfix lhs op rhs

conOperatorParser :: TokParser Name
conOperatorParser =
  symbolicConOp <|> backtickConOp
  where
    symbolicConOp =
      tokenSatisfy "constructor operator" $ \tok ->
        case lexTokenKind tok of
          TkConSym op -> Just (qualifyName Nothing (mkUnqualifiedName NameConSym op))
          TkQConSym modName op -> Just (mkName (Just modName) NameConSym op)
          TkReservedColon -> Just (qualifyName Nothing (mkUnqualifiedName NameConSym ":"))
          _ -> Nothing
    backtickConOp = MP.try $ do
      expectedTok TkSpecialBacktick
      name <- constructorNameParser
      expectedTok TkSpecialBacktick
      pure name

-- | Parse a left pattern (@lpat@ in the Haskell Report).
--
-- @
-- lpat → apat
--      | - (integer | float)       (negative literal)
--      | gcon apat₁ … apatₖ       (arity gcon = k, k ≥ 1)
-- @
--
-- Negative literals and constructor application live here, not in
-- 'patternAtomParser' (@apat@).  This distinction is critical: since
-- constructor arguments are @apat@s, @Con - 0@ cannot be misparsed as
-- @Con (-0)@ — the @-@ is not valid inside an @apat@.
appPatternParser :: TokParser Pattern
appPatternParser =
  negativeLiteralPatternParser
    <|> conAppOrAtomParser
  where
    conAppOrAtomParser = do
      first <- patternAtomParser
      if isPatternAppHead first
        then do
          rest <- MP.many patternAtomParser
          pure (foldl buildPatternApp first rest)
        else pure first

buildPatternApp :: Pattern -> Pattern -> Pattern
buildPatternApp lhs rhs =
  case peelPatternAnn lhs of
    PCon name typeArgs args ->
      PAnn
        (mkAnnotation (mergeSourceSpans (getPatternSourceSpan lhs) (getPatternSourceSpan rhs)))
        (PCon name typeArgs (args <> [rhs]))
    _ -> lhs

-- | Parse an atomic pattern (@apat@ in the Haskell Report).
--
-- This intentionally does NOT handle negative literals (@- integer@),
-- which belong to the @lpat@ level ('appPatternParser').
patternAtomParser :: TokParser Pattern
patternAtomParser = do
  thEnabled <- isExtensionEnabled TemplateHaskellQuotes
  thFullEnabled <- isExtensionEnabled TemplateHaskell
  explicitNamespacesEnabled <- isExtensionEnabled ExplicitNamespaces
  requiredTypeArgumentsEnabled <- isExtensionEnabled RequiredTypeArguments
  typeAbstractionsEnabled <- isExtensionEnabled TypeAbstractions
  let thAny = thEnabled || thFullEnabled
  tok <- lookAhead anySingle
  case lexTokenKind tok of
    TkTypeApp
      | typeAbstractionsEnabled -> typeBinderPatternParser
    TkPrefixBang -> strictPatternParser
    TkPrefixTilde -> irrefutablePatternParser
    TkKeywordType
      | explicitNamespacesEnabled || requiredTypeArgumentsEnabled -> explicitTypePatternParser
    TkQuasiQuote {} -> quasiQuotePatternParser
    TkTHSplice | thAny -> thSplicePatternParser
    TkKeywordUnderscore -> wildcardPatternParser
    TkInteger {} -> literalPatternParser
    TkIntegerHash {} -> literalPatternParser
    TkIntegerBase {} -> literalPatternParser
    TkIntegerBaseHash {} -> literalPatternParser
    TkFloat {} -> literalPatternParser
    TkFloatHash {} -> literalPatternParser
    TkChar {} -> literalPatternParser
    TkCharHash {} -> literalPatternParser
    TkString {} -> literalPatternParser
    TkStringHash {} -> literalPatternParser
    TkSpecialLBracket -> listPatternParser
    TkSpecialLParen -> parenOrTuplePatternParser
    TkSpecialUnboxedLParen -> parenOrTuplePatternParser
    _ -> do
      isAsPattern <- startsWithAsPattern
      if isAsPattern then atomAsPatternParser else varOrConPatternParser
  where
    -- Parse an as-pattern as an atom: name@atom
    -- This allows as-patterns within constructor application patterns
    -- (e.g., Con x@(Con' y z)).
    atomAsPatternParser :: TokParser Pattern
    atomAsPatternParser = withSpanAnn (PAnn . mkAnnotation) $ do
      name <- identifierTextParser
      expectedTok TkReservedAt
      PAs name <$> patternAtomParser

typeBinderPatternParser :: TokParser Pattern
typeBinderPatternParser = withSpanAnn (PAnn . mkAnnotation) $ do
  expectedTok TkTypeApp
  PTypeBinder <$> visibleTypeBinderCoreParser

explicitTypePatternParser :: TokParser Pattern
explicitTypePatternParser = withSpanAnn (PAnn . mkAnnotation) $ do
  expectedTok TkKeywordType
  PTypeSyntax TypeSyntaxExplicitNamespace <$> typeParser

strictPatternParser :: TokParser Pattern
strictPatternParser = withSpanAnn (PAnn . mkAnnotation) $ do
  expectedTok TkPrefixBang
  PStrict <$> patternAtomParser

irrefutablePatternParser :: TokParser Pattern
irrefutablePatternParser = withSpanAnn (PAnn . mkAnnotation) $ do
  expectedTok TkPrefixTilde
  PIrrefutable <$> patternAtomParser

negativeLiteralPatternParser :: TokParser Pattern
negativeLiteralPatternParser = MP.try $ withSpanAnn (PAnn . mkAnnotation) $ do
  expectedTok (TkVarSym "-")
  PNegLit <$> numericLiteralParser

-- | Parse only numeric literals (integer or float), used for negative literal
-- patterns where GHC only allows @-@ before numeric literals, not strings or chars.
numericLiteralParser :: TokParser Literal
numericLiteralParser = intLiteralParser <|> intBaseLiteralParser <|> floatLiteralParser

wildcardPatternParser :: TokParser Pattern
wildcardPatternParser = withSpanAnn (PAnn . mkAnnotation) $ do
  expectedTok TkKeywordUnderscore
  pure PWildcard

literalPatternParser :: TokParser Pattern
literalPatternParser = withSpanAnn (PAnn . mkAnnotation) $ do
  PLit <$> literalParser

quasiQuotePatternParser :: TokParser Pattern
quasiQuotePatternParser = withSpanAnn (PAnn . mkAnnotation) $ do
  (quoter, body) <- tokenSatisfy "quasi quote" $ \tok ->
    case lexTokenKind tok of
      TkQuasiQuote q b -> Just (q, b)
      _ -> Nothing
  pure (PQuasiQuote quoter body)

literalParser :: TokParser Literal
literalParser = intLiteralParser <|> intBaseLiteralParser <|> floatLiteralParser <|> charLiteralParser <|> stringLiteralParser

intLiteralParser :: TokParser Literal
intLiteralParser = withSpanAnn (LitAnn . mkAnnotation) $ do
  (ctor, n, repr) <- tokenSatisfy "integer literal" $ \tok ->
    case lexTokenKind tok of
      TkInteger i -> Just (LitInt, i, lexTokenText tok)
      TkIntegerHash i txt -> Just (LitIntHash, i, txt)
      _ -> Nothing
  pure (ctor n repr)

intBaseLiteralParser :: TokParser Literal
intBaseLiteralParser = withSpanAnn (LitAnn . mkAnnotation) $ do
  (ctor, n, repr) <- tokenSatisfy "based integer literal" $ \tok ->
    case lexTokenKind tok of
      TkIntegerBase i txt -> Just (LitIntBase, i, txt)
      TkIntegerBaseHash i txt -> Just (LitIntBaseHash, i, txt)
      _ -> Nothing
  pure (ctor n repr)

floatLiteralParser :: TokParser Literal
floatLiteralParser = withSpanAnn (LitAnn . mkAnnotation) $ do
  (ctor, n, repr) <- tokenSatisfy "floating literal" $ \tok ->
    case lexTokenKind tok of
      TkFloat x txt -> Just (LitFloat, x, txt)
      TkFloatHash x txt -> Just (LitFloatHash, x, txt)
      _ -> Nothing
  pure (ctor n repr)

charLiteralParser :: TokParser Literal
charLiteralParser = withSpanAnn (LitAnn . mkAnnotation) $ do
  (ctor, c, repr) <- tokenSatisfy "character literal" $ \tok ->
    case lexTokenKind tok of
      TkChar x -> Just (LitChar, x, lexTokenText tok)
      TkCharHash x txt -> Just (LitCharHash, x, txt)
      _ -> Nothing
  pure (ctor c repr)

stringLiteralParser :: TokParser Literal
stringLiteralParser = withSpanAnn (LitAnn . mkAnnotation) $ do
  (ctor, s, repr) <- tokenSatisfy "string literal" $ \tok ->
    case lexTokenKind tok of
      TkString x -> Just (LitString, x, lexTokenText tok)
      TkStringHash x txt -> Just (LitStringHash, x, txt)
      _ -> Nothing
  pure (ctor s repr)

-- | Parse Template Haskell pattern splice: $pat or $(pat)
thSplicePatternParser :: TokParser Pattern
thSplicePatternParser = withSpanAnn (PAnn . mkAnnotation) $ do
  expectedTok TkTHSplice
  body <- parenSpliceBody <|> bareSpliceBody
  pure (PSplice body)
  where
    parenSpliceBody = withSpanAnn (EAnn . mkAnnotation) $ do
      body <- parens exprParser
      pure (EParen body)
    bareSpliceBody = withSpanAnn (EAnn . mkAnnotation) $ do
      EVar <$> identifierNameParser

simplePatternParser :: TokParser Pattern
simplePatternParser =
  do
    typeAbstractionsEnabled <- isExtensionEnabled TypeAbstractions
    let typeBinderParser = if typeAbstractionsEnabled then MP.try typeBinderPatternParser else MP.empty
    MP.try
      ( withSpanAnn (PAnn . mkAnnotation) $ do
          name <- identifierTextParser
          expectedTok TkReservedAt
          PAs name <$> patternAtomParser
      )
      <|> typeBinderParser
      <|> patternAtomParser

-- | Parse a pattern for lambda-cases alternatives.
-- Accepts complex patterns (infix, as-patterns, strict, irrefutable, etc.)
-- but does NOT combine consecutive patterns into constructor applications.
-- This allows @\cases { True False -> 0 }@ to parse as two patterns,
-- not as @True@ applied to @False@.
lambdaCasePatternParser :: TokParser Pattern
lambdaCasePatternParser = label "pattern" $ do
  pat <- lambdaCaseInfixPatternParser
  mTypeSig <- MP.optional (expectedTok TkReservedDoubleColon *> typeParser)
  case mTypeSig of
    Just ty -> pure (PTypeSig pat ty)
    Nothing -> pure pat

lambdaCaseInfixPatternParser :: TokParser Pattern
lambdaCaseInfixPatternParser = do
  lhs <- lambdaCaseAsOrSimpleParser
  rest <- MP.many ((,) <$> conOperatorParser <*> lambdaCaseAsOrSimpleParser)
  pure (foldl buildInfixPattern lhs rest)

lambdaCaseAsOrSimpleParser :: TokParser Pattern
lambdaCaseAsOrSimpleParser = do
  isAsPattern <- startsWithAsPattern
  if isAsPattern
    then withSpanAnn (PAnn . mkAnnotation) $ do
      name <- identifierTextParser
      expectedTok TkReservedAt
      PAs name <$> simplePatternParser
    else
      MP.try negativeLiteralPatternParser
        <|> simplePatternParser

visibleTypeBinderCoreParser :: TokParser TyVarBinder
visibleTypeBinderCoreParser =
  withSpan $
    ( do
        ident <- lowerIdentifierParser <|> (expectedTok TkKeywordUnderscore $> "_")
        pure (\span' -> TyVarBinder [mkAnnotation span'] ident Nothing TyVarBSpecified TyVarBInvisible)
    )
      <|> ( do
              expectedTok TkSpecialLParen
              ident <- lowerIdentifierParser <|> (expectedTok TkKeywordUnderscore $> "_")
              expectedTok TkReservedDoubleColon
              kind <- typeParser
              expectedTok TkSpecialRParen
              pure (\span' -> TyVarBinder [mkAnnotation span'] ident (Just kind) TyVarBSpecified TyVarBInvisible)
          )

varOrConPatternParser :: TokParser Pattern
varOrConPatternParser = withSpanAnn (PAnn . mkAnnotation) $ do
  name <- identifierNameParser
  mNextTok <- MP.optional (lookAhead anySingle)
  case mNextTok of
    Just nextTok
      | isConLikeName name && lexTokenKind nextTok == TkSpecialLBrace -> do
          (fields, hasWildcard) <- braces recordPatternFieldListParser
          pure (PRecord name fields hasWildcard)
    _ ->
      pure $
        if isConLikeName name
          then PCon name [] []
          else PVar (mkUnqualifiedName (nameType name) (nameText name))

recordFieldPatternParser :: TokParser (Name, Pattern)
recordFieldPatternParser = do
  field <- identifierNameParser
  mEq <- MP.optional (expectedTok TkReservedEquals)
  case mEq of
    Just () -> do
      pat <- patternParser
      pure (field, pat)
    Nothing -> do
      -- NamedFieldPuns: just "field" means "field = field"
      pure (field, PVar (mkUnqualifiedName (nameType field) (nameText field)))

-- | Parse the contents of record pattern braces, supporting RecordWildCards ".."
recordPatternFieldListParser :: TokParser ([(Name, Pattern)], Bool)
recordPatternFieldListParser = do
  rwcEnabled <- isExtensionEnabled RecordWildCards
  fields <- recordFieldPatternParser `MP.sepEndBy` expectedTok TkSpecialComma
  if rwcEnabled
    then do
      mDotDot <- MP.optional (expectedTok TkReservedDotDot)
      case mDotDot of
        Nothing -> pure (fields, False)
        Just _ -> do
          _ <- MP.optional (expectedTok TkSpecialComma)
          pure (fields, True)
    else pure (fields, False)

listPatternParser :: TokParser Pattern
listPatternParser = withSpanAnn (PAnn . mkAnnotation) $ do
  expectedTok TkSpecialLBracket
  elems <- listPatternElementParser `MP.sepBy` expectedTok TkSpecialComma
  expectedTok TkSpecialRBracket
  pure (PList elems)
  where
    -- List elements can contain bare view patterns such as [id -> x].
    -- Try the expr -> pattern form first, then fall back to normal patterns.
    listPatternElementParser :: TokParser Pattern
    listPatternElementParser = do
      mView <- MP.optional . MP.try $ do
        expr <- exprParser
        expectedTok TkReservedRightArrow
        PView expr <$> patternParser
      maybe patternParser pure mView

parenOrTuplePatternParser :: TokParser Pattern
parenOrTuplePatternParser = withSpanAnn (PAnn . mkAnnotation) $ do
  (tupleFlavor, closeTok) <-
    (expectedTok TkSpecialLParen $> (Boxed, TkSpecialRParen))
      <|> (expectedTok TkSpecialUnboxedLParen $> (Unboxed, TkSpecialUnboxedRParen))
  mNextTok <- MP.optional (lookAhead anySingle)
  case fmap lexTokenKind mNextTok of
    Just nextKind
      | nextKind == closeTok -> unitPatternParser tupleFlavor closeTok
      | tupleFlavor == Unboxed && nextKind == TkReservedPipe -> parseUnboxedSumPatLeadingBars closeTok
    _ -> do
      -- For boxed parens, try parsing as a top-level view pattern first.
      -- View patterns like (expr -> pat) produce PParen(PView(...)) so that
      -- the explicit source parens are faithfully represented in the AST.
      mView <-
        if tupleFlavor == Boxed
          then viewPatternParser closeTok
          else pure Nothing
      case mView of
        Just mkView -> pure mkView
        Nothing -> tupleOrParenPatternParser tupleFlavor closeTok
  where
    unitPatternParser tupleFlavor closeTok = do
      expectedTok closeTok
      pure (PTuple tupleFlavor [])

    -- Try to parse the paren content as a view pattern: expr -> pat.
    -- Uses exprParser which stops before '->', then checks for the arrow.
    -- Returns Nothing if the content is not a view pattern.
    viewPatternParser :: LexTokenKind -> TokParser (Maybe Pattern)
    viewPatternParser closeTok = MP.optional . MP.try $ do
      expr <- exprParser
      expectedTok TkReservedRightArrow
      inner <- patternParser
      expectedTok closeTok
      pure (PParen (PView expr inner))

    -- Parse a single element inside a paren/tuple/unboxed-sum pattern.
    -- Uses "parse as expression, then reclassify" to avoid backtracking
    -- for the common case. Pattern-only prefixes (!, ~, @) are dispatched
    -- to patternParser directly. When exprParser fails (e.g., nested parens
    -- containing pattern-only syntax like as-patterns or view patterns),
    -- we fall back to patternParser.
    --
    -- Operator tokens (TkVarSym, TkConSym, etc.) are handled directly here
    -- because they are valid patterns (binding the operator as a variable or
    -- constructor) but may not parse as expressions on their own.
    parenPatElementParser :: TokParser Pattern
    parenPatElementParser = do
      tok <- lookAhead anySingle
      case lexTokenKind tok of
        TkPrefixBang -> patternParser
        TkPrefixTilde -> patternParser
        -- Operator tokens can be valid patterns when they appear alone in parentheses.
        -- Examples: (+) in "f (+) = ...", (??) in "foldl' (??) z xs = ..."
        -- We detect this by checking if an operator is followed by a closing delimiter.
        TkVarSym {} -> operatorOrExprPatternParser
        TkConSym {} -> operatorOrExprPatternParser
        _ -> do
          isAs <- startsWithAsPattern
          if isAs
            then patternParser
            else exprThenReclassify
      where
        -- Try to parse an operator as a pattern if it's alone (followed by closing delim),
        -- otherwise fall back to parsing as an expression.
        operatorOrExprPatternParser :: TokParser Pattern
        operatorOrExprPatternParser = do
          -- Look ahead to check what comes after the operator
          mNext <- MP.optional . lookAhead . MP.try $ do
            _ <- anySingle -- skip the operator token itself
            lookAhead anySingle
          case fmap lexTokenKind mNext of
            -- If followed by closing delimiters, parse as operator pattern
            Just TkSpecialRParen -> operatorPatternParser
            Just TkSpecialUnboxedRParen -> operatorPatternParser
            Just TkSpecialComma -> operatorPatternParser
            Just TkReservedPipe -> operatorPatternParser
            -- Otherwise, try parsing as expression (for cases like (x + y))
            _ -> exprThenReclassify

        -- Parse an operator token as a variable or constructor pattern.
        operatorPatternParser :: TokParser Pattern
        operatorPatternParser = withSpanAnn (PAnn . mkAnnotation) $ do
          tok' <- anySingle
          case lexTokenKind tok' of
            TkVarSym op -> pure (PVar (mkUnqualifiedName NameVarSym op))
            TkConSym op -> pure (PCon (qualifyName Nothing (mkUnqualifiedName NameConSym op)) [] [])
            _ -> fail "expected operator token"

    -- Try to parse as expression, then reclassify via checkPattern.
    -- When exprParser fails, does not consume the full element (e.g.,
    -- '@' from an as-pattern), or checkPattern rejects it (e.g., variable
    -- operator in infix position), fall back to patternParser.
    --
    -- View patterns within tuple elements are also handled here: if '->'
    -- follows the parsed expression, it is a view pattern.
    exprThenReclassify :: TokParser Pattern
    exprThenReclassify = do
      mResult <- MP.optional . MP.try $ do
        expr <- exprParser
        -- Verify the expression consumed the full element: the next token
        -- must be a valid delimiter in paren/tuple/sum context. If not
        -- (e.g., '@' from an as-pattern), the expression parser stopped
        -- too early and we should backtrack to patternParser.
        tok <- lookAhead anySingle
        case lexTokenKind tok of
          TkReservedRightArrow -> pure (Left expr) -- view pattern: defer arrow handling
          TkSpecialComma -> Right <$> liftCheck (checkPattern expr)
          TkSpecialRParen -> Right <$> liftCheck (checkPattern expr)
          TkSpecialUnboxedRParen -> Right <$> liftCheck (checkPattern expr)
          TkReservedPipe -> Right <$> liftCheck (checkPattern expr)
          _ -> fail "incomplete element parse"
      case mResult of
        Just (Left expr) -> do
          -- View pattern: expr -> pattern
          expectedTok TkReservedRightArrow
          PView expr <$> patternParser
        Just (Right pat) ->
          pure pat
        Nothing ->
          patternParser

    tupleOrParenPatternParser tupleFlavor closeTok = do
      first <- parenPatElementParser
      mComma <- MP.optional (expectedTok TkSpecialComma)
      case mComma of
        Nothing -> do
          -- Check for pipe (unboxed sum: pattern in first slot)
          mPipe <- if tupleFlavor == Unboxed then MP.optional (expectedTok TkReservedPipe) else pure Nothing
          case mPipe of
            Just () -> do
              -- (# pat | ... #) - pattern in first slot of sum
              trailingBars <- MP.many (expectedTok TkReservedPipe)
              expectedTok closeTok
              let arity = 2 + length trailingBars
              pure (PUnboxedSum 0 arity first)
            Nothing -> do
              expectedTok closeTok
              if tupleFlavor == Boxed
                then pure (PParen first)
                else pure (PTuple Unboxed [first])
        Just () -> do
          second <- parenPatElementParser
          more <- MP.many (expectedTok TkSpecialComma *> parenPatElementParser)
          expectedTok closeTok
          pure (PTuple tupleFlavor (first : second : more))

    parseUnboxedSumPatLeadingBars closeTok = do
      -- Parse (# | | ... | pat | ... | #) where pattern is not in first slot
      _ <- expectedTok TkReservedPipe
      leadingBars <- MP.many (MP.try (expectedTok TkReservedPipe))
      let altIdx = 1 + length leadingBars
      inner <- parenPatElementParser
      trailingBars <- MP.many (expectedTok TkReservedPipe)
      expectedTok closeTok
      let arity = altIdx + 1 + length trailingBars
      pure (PUnboxedSum altIdx arity inner)

isPatternAppHead :: Pattern -> Bool
isPatternAppHead pat =
  case peelPatternAnn pat of
    PCon {} -> True
    PVar name -> isConLikeNameType (unqualifiedNameType name)
    _ -> False
