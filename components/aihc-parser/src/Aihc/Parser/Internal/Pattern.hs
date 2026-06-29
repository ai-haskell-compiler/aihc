{-# LANGUAGE OverloadedStrings #-}

module Aihc.Parser.Internal.Pattern
  ( patternParser,
    patternParserWithTypeSigParser,
    caseAltPatternParser,
    patParser,
    lpatParser,
    apatParser,
    literalParser,
  )
where

import Aihc.Parser.Internal.CheckPattern (checkPattern)
import Aihc.Parser.Internal.Common
import {-# SOURCE #-} Aihc.Parser.Internal.Expr (atomExprParser, exprParser)
import Aihc.Parser.Internal.Type (typeParser)
import Aihc.Parser.Lex (LexToken (..), LexTokenKind (..), lexTokenKind, lexTokenSpan, lexTokenText)
import Aihc.Parser.Syntax
import Aihc.Parser.Types (ParserErrorComponent (..), TokStream (..), mkFoundToken)
import Text.Megaparsec (anySingle, lookAhead, (<|>))
import Text.Megaparsec qualified as MP

-- | Report core:
--
-- > pat -> lpat qconop pat
-- >     | lpat
patternParser :: TokParser Pattern
patternParser = patternParserWithTypeSigParser typeParser

patternParserWithTypeSigParser :: TokParser Type -> TokParser Pattern
patternParserWithTypeSigParser typeSigParser =
  label "pattern" $
    optionalSuffix
      (expectedTok TkReservedDoubleColon *> typeSigParser)
      PTypeSig
      patParser

-- | Parse the pattern position of a case alternative.
--
-- GHC does not accept a top-level pattern type signature before the alternative
-- arrow: @case x of _ :: T -> rhs@ is rejected, while @case x of (_ :: T) ->
-- rhs@ is accepted because the signature is parenthesized into an atomic
-- pattern.  Use the report @pat@ level here so the outer @::@ is left for the
-- case RHS parser, which then rejects it.
-- | Case alternatives intentionally use the report @pat@ level rather than
-- the outer typed-pattern wrapper.
caseAltPatternParser :: TokParser Pattern
caseAltPatternParser = patParser

-- | Parse a pattern (@pat@ in the Haskell Report).
--
-- @
-- pat → lpat qconop pat
--     | lpat
-- @
patParser :: TokParser Pattern
patParser = do
  lhs <- lpatParser
  rest <- MP.many ((,) <$> conOperatorParser <*> lpatParser)
  pure (foldInfixL buildInfixPattern lhs rest)

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
          TkConSym op -> Just (qualifyName Nothing (mkUnqualifiedNameAt tok NameConSym op))
          TkQConSym modName op -> Just (mkNameAt tok (Just modName) NameConSym op)
          TkReservedColon -> Just (qualifyName Nothing (mkUnqualifiedNameAt tok NameConSym ":"))
          _ -> Nothing
    backtickConOp =
      MP.try $
        expectedTok TkSpecialBacktick *> constructorNameParser <* expectedTok TkSpecialBacktick

-- | Parse a left pattern (@lpat@ in the Haskell Report).
--
-- @
-- lpat → apat
--      | - (integer | float)       (negative literal)
--      | gcon apat₁ … apatₖ       (arity gcon = k, k ≥ 1)
-- @
--
-- Negative literals and constructor application live here, not in
-- 'apatParser' (@apat@).  This distinction is critical: since
-- constructor arguments are @apat@s, @Con - 0@ cannot be misparsed as
-- @Con (-0)@ — the @-@ is not valid inside an @apat@.
lpatParser :: TokParser Pattern
lpatParser =
  negativeLiteralPatternParser <|> do
    first <- apatParser
    if isPatternAppHead first
      then foldl buildPatternApp first <$> MP.many apatParser
      else pure first

buildPatternApp :: Pattern -> Pattern -> Pattern
buildPatternApp lhs rhs =
  case peelPatternAnn lhs of
    PCon name typeArgs args ->
      PAnn
        (mkAnnotation NoSourceSpan)
        (PCon name typeArgs (args <> [rhs]))
    _ -> lhs

-- | Parse an atomic pattern (@apat@ in the Haskell Report).
--
-- This intentionally does NOT handle negative literals (@- integer@),
-- which belong to the @lpat@ level ('lpatParser').
apatParser :: TokParser Pattern
apatParser =
  label "pattern atom" $
    asApatParser <|> nonAsApatParser

-- | Parse an as-pattern as an atomic pattern: @binder\@apat@.
--
-- The binder may be a parenthesized operator, e.g. @(+)\@C@.  Trying this
-- before the parenthesized-pattern parser keeps @(+)\@(+)@C@ in the as-pattern
-- grammar instead of prematurely committing to the bare operator pattern @(+)@.
asApatParser :: TokParser Pattern
asApatParser =
  asPatternParser apatParser

nonAsApatParser :: TokParser Pattern
nonAsApatParser = do
  thAny <- thAnyEnabled
  explicitNamespacesEnabled <- isExtensionEnabled ExplicitNamespaces
  requiredTypeArgumentsEnabled <- isExtensionEnabled RequiredTypeArguments
  typeAbstractionsEnabled <- isExtensionEnabled TypeAbstractions
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
    TkFloat {} -> literalPatternParser
    TkChar {} -> literalPatternParser
    TkCharHash {} -> literalPatternParser
    TkString {} -> literalPatternParser
    TkStringHash {} -> literalPatternParser
    TkSpecialLBracket -> listPatternParser
    TkSpecialLParen -> parenOrTuplePatternParser
    TkSpecialUnboxedLParen -> parenOrTuplePatternParser
    _ -> varOrConPatternParser

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
  PStrict <$> apatParser

irrefutablePatternParser :: TokParser Pattern
irrefutablePatternParser = withSpanAnn (PAnn . mkAnnotation) $ do
  expectedTok TkPrefixTilde
  PIrrefutable <$> apatParser

negativeLiteralPatternParser :: TokParser Pattern
negativeLiteralPatternParser = MP.try $ withSpanAnn (PAnn . mkAnnotation) $ do
  expectedTok (TkVarSym "-")
  PNegLit <$> numericLiteralParser

-- | Parse only numeric literals (integer or float), used for negative literal
-- patterns where GHC only allows @-@ before numeric literals, not strings or chars.
numericLiteralParser :: TokParser Literal
numericLiteralParser = intLiteralParser <|> floatLiteralParser

wildcardPatternParser :: TokParser Pattern
wildcardPatternParser =
  tokenSatisfy "wildcard" $ \tok ->
    case lexTokenKind tok of
      TkKeywordUnderscore -> Just (PAnn (mkAnnotation (lexTokenSpan tok)) PWildcard)
      _ -> Nothing

literalPatternParser :: TokParser Pattern
literalPatternParser =
  tokenSatisfy "literal" $ \tok -> do
    lit <- literalFromToken tok
    let ann = mkAnnotation (lexTokenSpan tok)
    pure (PAnn ann (PLit (LitAnn ann lit)))

quasiQuotePatternParser :: TokParser Pattern
quasiQuotePatternParser =
  tokenSatisfy "quasi quote" $ \tok ->
    case lexTokenKind tok of
      TkQuasiQuote quoter body -> Just (PAnn (mkAnnotation (lexTokenSpan tok)) (PQuasiQuote quoter body))
      _ -> Nothing

literalParser :: TokParser Literal
literalParser = intLiteralParser <|> floatLiteralParser <|> charLiteralParser <|> stringLiteralParser

tokenLiteralParser :: String -> (LexToken -> Maybe Literal) -> TokParser Literal
tokenLiteralParser expected matchToken =
  tokenSatisfy expected $ \tok ->
    LitAnn (mkAnnotation (lexTokenSpan tok)) <$> matchToken tok

literalFromToken :: LexToken -> Maybe Literal
literalFromToken tok =
  intLiteralFromToken tok
    <|> floatLiteralFromToken tok
    <|> charLiteralFromToken tok
    <|> stringLiteralFromToken tok

intLiteralParser :: TokParser Literal
intLiteralParser =
  tokenLiteralParser "integer literal" intLiteralFromToken

intLiteralFromToken :: LexToken -> Maybe Literal
intLiteralFromToken tok =
  case lexTokenKind tok of
    TkInteger i nt -> Just (LitInt i nt (lexTokenText tok))
    _ -> Nothing

floatLiteralParser :: TokParser Literal
floatLiteralParser =
  tokenLiteralParser "floating literal" floatLiteralFromToken

floatLiteralFromToken :: LexToken -> Maybe Literal
floatLiteralFromToken tok =
  case lexTokenKind tok of
    TkFloat x ft -> Just (LitFloat x ft (lexTokenText tok))
    _ -> Nothing

charLiteralParser :: TokParser Literal
charLiteralParser =
  tokenLiteralParser "character literal" charLiteralFromToken

charLiteralFromToken :: LexToken -> Maybe Literal
charLiteralFromToken tok =
  case lexTokenKind tok of
    TkChar x -> Just (LitChar x (lexTokenText tok))
    TkCharHash x txt -> Just (LitCharHash x txt)
    _ -> Nothing

stringLiteralParser :: TokParser Literal
stringLiteralParser =
  tokenLiteralParser "string literal" stringLiteralFromToken

stringLiteralFromToken :: LexToken -> Maybe Literal
stringLiteralFromToken tok =
  case lexTokenKind tok of
    TkString x -> Just (LitString x (lexTokenText tok))
    TkStringHash x txt -> Just (LitStringHash x txt)
    _ -> Nothing

-- | Parse Template Haskell pattern splice: $pat or $(pat)
thSplicePatternParser :: TokParser Pattern
thSplicePatternParser = withSpanAnn (PAnn . mkAnnotation) $ do
  expectedTok TkTHSplice
  tok <- lookAhead anySingle
  case lexTokenKind tok of
    TkKeywordType -> MP.empty
    _ -> PSplice <$> atomExprParser

visibleTypeBinderCoreParser :: TokParser TyVarBinder
visibleTypeBinderCoreParser =
  withSpan $
    ( do
        ident <- tyVarNameParser
        pure (\span' -> TyVarBinder [mkAnnotation span'] ident Nothing TyVarBSpecified TyVarBInvisible)
    )
      <|> ( do
              expectedTok TkSpecialLParen
              ident <- tyVarNameParser
              expectedTok TkReservedDoubleColon
              kind <- typeParser
              expectedTok TkSpecialRParen
              pure (\span' -> TyVarBinder [mkAnnotation span'] ident (Just kind) TyVarBSpecified TyVarBInvisible)
          )

varOrConPatternParser :: TokParser Pattern
varOrConPatternParser = do
  (tok, name) <- identifierNameWithTokenParser
  mNextTok <- MP.optional (lookAhead anySingle)
  let ann = mkAnnotation (lexTokenSpan tok)
  case mNextTok of
    Just nextTok
      | isConLikeName name && lexTokenKind nextTok == TkSpecialLBrace -> do
          (fields, hasWildcard) <- braces recordPatternFieldListParser
          endInput <- MP.getInput
          let endSpan = maybe noSourceSpan lexTokenSpan (tokStreamPrevToken endInput)
              recordSpan = mergeSourceSpans (lexTokenSpan tok) endSpan
          pure (PAnn (mkAnnotation recordSpan) (PRecord name fields hasWildcard))
    _ ->
      pure $
        PAnn ann $
          if isConLikeName name
            then PCon name [] []
            else PVar (nameToUnqualified name)

recordFieldPatternParser :: TokParser (RecordField Pattern)
recordFieldPatternParser = do
  field <- recordFieldNameParser
  mEq <- MP.optional (expectedTok TkReservedEquals)
  case mEq of
    Just () -> do
      pat <- subpatternWithBareViewParser
      pure (RecordField field pat False)
    Nothing -> do
      -- NamedFieldPuns: just "field" means "field = field"
      pure (RecordField field (PVar (nameToUnqualified field)) True)

-- | Parse the contents of record pattern braces, supporting RecordWildCards ".."
recordPatternFieldListParser :: TokParser ([RecordField Pattern], Bool)
recordPatternFieldListParser =
  recordFieldsWithWildcardsParser (recordFieldPatternParser `MP.sepEndBy` expectedTok TkSpecialComma)

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
    listPatternElementParser = subpatternWithBareViewParser

-- | Parse a subpattern position that admits a bare view pattern @expr -> pat@.
-- This is needed in delimited pattern contexts like list elements and record
-- fields, where there is no surrounding pair of parens to disambiguate the
-- view-pattern arrow from the enclosing syntax.
--
-- This parser is recursive so that deeply nested view patterns such as
-- @expr1 -> expr2 -> pat@ are accepted without requiring explicit parentheses
-- around each intermediate view pattern.
subpatternWithBareViewParser :: TokParser Pattern
subpatternWithBareViewParser = do
  mView <- MP.optional . MP.try $ do
    expr <- exprParser
    expectedTok TkReservedRightArrow
    PView expr <$> subpatternWithBareViewParser
  maybe patternParser pure mView

parenOrTuplePatternParser :: TokParser Pattern
parenOrTuplePatternParser = withSpanAnn (PAnn . mkAnnotation) $ do
  (tupleFlavor, closeTok) <- tupleDelimsParser
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
    -- When a bare operator like + or :+ appears directly inside parens, the
    -- parens serve as prefix notation (e.g. (+), (:+)), not grouping.
    -- prettyPrefixName already adds parens when printing symbolic names, so
    -- wrapping with PParen would produce double parens.
    --
    -- However, when the inner pattern came from expression reclassification
    -- (e.g. ((:+)) where the expression parser already consumed the inner
    -- prefix-notation parens), the outer parens ARE grouping and must produce
    -- PParen.
    --
    -- The @isBareOperator@ flag distinguishes these two cases:
    --   True  -> parens are prefix notation, strip PParen
    --   False -> parens are grouping, wrap with PParen
    parenOrSymConParser isBareOperator inner = do
      case peelPatternAnn inner of
        PVar name
          | isBareOperator,
            unqualifiedNameType name == NameVarSym ->
              pure inner
        PCon con [] []
          | isBareOperator,
            nameType con == NameConSym -> do
              mBrace <- MP.optional . lookAhead $ anySingle
              case fmap lexTokenKind mBrace of
                Just TkSpecialLBrace -> do
                  (fields, hasWildcard) <- braces recordPatternFieldListParser
                  pure (PRecord con fields hasWildcard)
                _ -> pure inner
        _ -> pure (PParen inner)

    unitPatternParser tupleFlavor closeTok = do
      expectedTok closeTok
      pure (PTuple tupleFlavor [])

    -- Try to parse the paren content as a view pattern: expr -> pat.
    -- Uses exprParser which stops before '->', then checks for the arrow.
    -- Returns Nothing if the content is not a view pattern.
    viewPatternParser :: LexTokenKind -> TokParser (Maybe Pattern)
    viewPatternParser closeTok = MP.optional . MP.try $ do
      expr <- exprParser <* lookAhead (expectedTok TkReservedRightArrow)
      expectedTok TkReservedRightArrow
      inner <- subpatternWithBareViewParser
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
    --
    -- Returns (isBareOperator, pattern) where isBareOperator indicates the
    -- pattern was parsed as a bare operator token (e.g. :+ in (:+)), meaning
    -- the surrounding parens serve as prefix notation rather than grouping.
    parenPatElementParser :: TokParser (Bool, Pattern)
    parenPatElementParser = do
      tok <- lookAhead anySingle
      case lexTokenKind tok of
        TkPrefixBang -> (False,) <$> patternParser
        TkPrefixTilde -> (False,) <$> patternParser
        -- Operator tokens can be valid patterns when they appear alone in parentheses.
        -- Examples: (+) in "f (+) = ...", (??) in "foldl' (??) z xs = ..."
        -- We detect this by checking if an operator is followed by a closing delimiter.
        TkVarSym {} -> operatorOrExprPatternParser
        TkConSym {} -> operatorOrExprPatternParser
        TkQConSym {} -> operatorOrExprPatternParser
        TkReservedColon -> operatorOrExprPatternParser
        TkReservedAt -> operatorOrExprPatternParser
        _ -> do
          isAs <- startsWithAsPattern
          if isAs
            then (False,) <$> patternParser
            else (False,) <$> exprThenReclassify
      where
        -- Try to parse an operator as a pattern if it's alone (followed by closing delim),
        -- otherwise fall back to parsing as an expression.
        operatorOrExprPatternParser :: TokParser (Bool, Pattern)
        operatorOrExprPatternParser = do
          -- Look ahead to check what comes after the operator
          mNext <- MP.optional . lookAhead . MP.try $ do
            _ <- anySingle -- skip the operator token itself
            lookAhead anySingle
          case fmap lexTokenKind mNext of
            -- If followed by closing delimiters, parse as operator pattern
            Just TkSpecialRParen -> (True,) <$> operatorPatternParser
            Just TkSpecialUnboxedRParen -> (True,) <$> operatorPatternParser
            Just TkSpecialComma -> (True,) <$> operatorPatternParser
            Just TkReservedPipe -> (True,) <$> operatorPatternParser
            -- Otherwise, try parsing as expression (for cases like (x + y))
            _ -> (False,) <$> exprThenReclassify

        -- Parse an operator token as a variable or constructor pattern.
        operatorPatternParser :: TokParser Pattern
        operatorPatternParser = do
          tok' <- anySingle
          let ann = mkAnnotation (lexTokenSpan tok')
          case lexTokenKind tok' of
            TkVarSym op -> pure (PAnn ann (PVar (mkUnqualifiedNameAt tok' NameVarSym op)))
            TkConSym op -> pure (PAnn ann (PCon (qualifyName Nothing (mkUnqualifiedNameAt tok' NameConSym op)) [] []))
            TkQConSym modName op -> pure (PAnn ann (PCon (mkNameAt tok' (Just modName) NameConSym op) [] []))
            TkReservedColon -> pure (PAnn ann (PCon (qualifyName Nothing (mkUnqualifiedNameAt tok' NameConSym ":")) [] []))
            TkReservedAt -> pure (PAnn ann (PVar (mkUnqualifiedNameAt tok' NameVarSym "@")))
            _ ->
              MP.customFailure
                UnexpectedTokenExpecting
                  { unexpectedFound = Just (mkFoundToken tok'),
                    unexpectedExpecting = "operator token",
                    unexpectedContext = []
                  }

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
          PView expr <$> subpatternWithBareViewParser
        Just (Right pat) ->
          pure pat
        Nothing ->
          patternParser

    tupleOrParenPatternParser tupleFlavor closeTok = do
      (isBareOp, first) <- parenPatElementParser
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
                then parenOrSymConParser isBareOp first
                else pure (PTuple Unboxed [first])
        Just () -> do
          (_, second) <- parenPatElementParser
          more <- MP.many (expectedTok TkSpecialComma *> (snd <$> parenPatElementParser))
          expectedTok closeTok
          pure (PTuple tupleFlavor (first : second : more))

    parseUnboxedSumPatLeadingBars closeTok = do
      -- Parse (# | | ... | pat | ... | #) where pattern is not in first slot
      _ <- expectedTok TkReservedPipe
      leadingBars <- MP.many (MP.try (expectedTok TkReservedPipe))
      let altIdx = 1 + length leadingBars
      (_, inner) <- parenPatElementParser
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
