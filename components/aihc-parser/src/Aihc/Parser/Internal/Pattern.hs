{-# LANGUAGE OverloadedStrings #-}

module Aihc.Parser.Internal.Pattern
  ( patternParser,
    simplePatternParser,
    appPatternParser,
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
    Just ty -> pure (PTypeSig (mergeSourceSpans (getSourceSpan pat) (getSourceSpan ty)) pat ty)
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
    then withSpan $ do
      name <- identifierTextParser
      expectedTok TkReservedAt
      inner <- patternAtomParser
      pure (\span' -> PAs span' name inner)
    else appPatternParser

buildInfixPattern :: Pattern -> (Name, Pattern) -> Pattern
buildInfixPattern lhs (op, rhs) =
  PInfix (mergeSourceSpans (getSourceSpan lhs) (getSourceSpan rhs)) lhs op rhs

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

appPatternParser :: TokParser Pattern
appPatternParser = do
  first <- patternAtomParser
  if isPatternAppHead first
    then do
      rest <- MP.many patternAtomParser
      pure (foldl buildPatternApp first rest)
    else pure first

buildPatternApp :: Pattern -> Pattern -> Pattern
buildPatternApp lhs rhs =
  case lhs of
    PCon lSpan name args -> PCon (mergeSourceSpans lSpan (getSourceSpan rhs)) name (args <> [rhs])
    _ -> lhs

patternAtomParser :: TokParser Pattern
patternAtomParser = do
  thFullEnabled <- isExtensionEnabled TemplateHaskell
  tok <- lookAhead anySingle
  case lexTokenKind tok of
    TkPrefixBang -> strictPatternParser
    TkPrefixTilde -> irrefutablePatternParser
    TkVarSym "-" -> negativeLiteralPatternParser
    TkQuasiQuote {} -> quasiQuotePatternParser
    TkTHSplice | thFullEnabled -> thSplicePatternParser
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
    atomAsPatternParser = withSpan $ do
      name <- identifierTextParser
      expectedTok TkReservedAt
      inner <- patternAtomParser
      pure (\span' -> PAs span' name inner)

strictPatternParser :: TokParser Pattern
strictPatternParser = withSpan $ do
  expectedTok TkPrefixBang
  inner <- patternAtomParser
  pure (`PStrict` inner)

irrefutablePatternParser :: TokParser Pattern
irrefutablePatternParser = withSpan $ do
  expectedTok TkPrefixTilde
  inner <- patternAtomParser
  pure (`PIrrefutable` inner)

negativeLiteralPatternParser :: TokParser Pattern
negativeLiteralPatternParser = MP.try $ withSpan $ do
  expectedTok (TkVarSym "-")
  lit <- numericLiteralParser
  pure (`PNegLit` lit)

-- | Parse only numeric literals (integer or float), used for negative literal
-- patterns where GHC only allows @-@ before numeric literals, not strings or chars.
numericLiteralParser :: TokParser Literal
numericLiteralParser = intLiteralParser <|> intBaseLiteralParser <|> floatLiteralParser

wildcardPatternParser :: TokParser Pattern
wildcardPatternParser = withSpan $ do
  expectedTok TkKeywordUnderscore
  pure PWildcard

literalPatternParser :: TokParser Pattern
literalPatternParser = withSpan $ do
  lit <- literalParser
  pure (`PLit` lit)

quasiQuotePatternParser :: TokParser Pattern
quasiQuotePatternParser = withSpan $ do
  (quoter, body) <- tokenSatisfy "quasi quote" $ \tok ->
    case lexTokenKind tok of
      TkQuasiQuote q b -> Just (q, b)
      _ -> Nothing
  pure (\span' -> PQuasiQuote span' quoter body)

literalParser :: TokParser Literal
literalParser = intLiteralParser <|> intBaseLiteralParser <|> floatLiteralParser <|> charLiteralParser <|> stringLiteralParser

intLiteralParser :: TokParser Literal
intLiteralParser = withSpan $ do
  (ctor, n, repr) <- tokenSatisfy "integer literal" $ \tok ->
    case lexTokenKind tok of
      TkInteger i -> Just (LitInt, i, lexTokenText tok)
      TkIntegerHash i txt -> Just (LitIntHash, i, txt)
      _ -> Nothing
  pure (\span' -> ctor span' n repr)

intBaseLiteralParser :: TokParser Literal
intBaseLiteralParser = withSpan $ do
  (ctor, n, repr) <- tokenSatisfy "based integer literal" $ \tok ->
    case lexTokenKind tok of
      TkIntegerBase i txt -> Just (LitIntBase, i, txt)
      TkIntegerBaseHash i txt -> Just (LitIntBaseHash, i, txt)
      _ -> Nothing
  pure (\span' -> ctor span' n repr)

floatLiteralParser :: TokParser Literal
floatLiteralParser = withSpan $ do
  (ctor, n, repr) <- tokenSatisfy "floating literal" $ \tok ->
    case lexTokenKind tok of
      TkFloat x txt -> Just (LitFloat, x, txt)
      TkFloatHash x txt -> Just (LitFloatHash, x, txt)
      _ -> Nothing
  pure (\span' -> ctor span' n repr)

charLiteralParser :: TokParser Literal
charLiteralParser = withSpan $ do
  (ctor, c, repr) <- tokenSatisfy "character literal" $ \tok ->
    case lexTokenKind tok of
      TkChar x -> Just (LitChar, x, lexTokenText tok)
      TkCharHash x txt -> Just (LitCharHash, x, txt)
      _ -> Nothing
  pure (\span' -> ctor span' c repr)

stringLiteralParser :: TokParser Literal
stringLiteralParser = withSpan $ do
  (ctor, s, repr) <- tokenSatisfy "string literal" $ \tok ->
    case lexTokenKind tok of
      TkString x -> Just (LitString, x, lexTokenText tok)
      TkStringHash x txt -> Just (LitStringHash, x, txt)
      _ -> Nothing
  pure (\span' -> ctor span' s repr)

-- | Parse Template Haskell pattern splice: $pat or $(pat)
thSplicePatternParser :: TokParser Pattern
thSplicePatternParser = withSpan $ do
  expectedTok TkTHSplice
  body <- parenSpliceBody <|> bareSpliceBody
  pure (`PSplice` body)
  where
    parenSpliceBody = withSpan $ do
      body <- parens exprParser
      pure (`EParen` body)
    bareSpliceBody = withSpan $ do
      name <- identifierNameParser
      pure (`EVar` name)

simplePatternParser :: TokParser Pattern
simplePatternParser =
  MP.try
    ( withSpan $ do
        name <- identifierTextParser
        expectedTok TkReservedAt
        inner <- patternAtomParser
        pure (\span' -> PAs span' name inner)
    )
    <|> patternAtomParser

varOrConPatternParser :: TokParser Pattern
varOrConPatternParser = withSpan $ do
  name <- identifierNameParser
  mNextTok <- MP.optional (lookAhead anySingle)
  case mNextTok of
    Just nextTok
      | isConLikeName name && lexTokenKind nextTok == TkSpecialLBrace -> do
          (fields, hasWildcard) <- braces recordPatternFieldListParser
          pure (\span' -> PRecord span' name fields hasWildcard)
    _ ->
      pure $ \span' ->
        if isConLikeName name
          then PCon span' name []
          else PVar span' (mkUnqualifiedName (nameType name) (nameText name))

recordFieldPatternParser :: TokParser (Name, Pattern)
recordFieldPatternParser = withSpan $ do
  field <- identifierNameParser
  mEq <- MP.optional (expectedTok TkReservedEquals)
  case mEq of
    Just () -> do
      pat <- patternParser
      pure $ const (field, pat)
    Nothing -> do
      -- NamedFieldPuns: just "field" means "field = field"
      pure $ \srcSpan -> (field, PVar srcSpan (mkUnqualifiedName (nameType field) (nameText field)))

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
listPatternParser = withSpan $ do
  expectedTok TkSpecialLBracket
  elems <- listPatternElementParser `MP.sepBy` expectedTok TkSpecialComma
  expectedTok TkSpecialRBracket
  pure (`PList` elems)
  where
    -- List elements can contain bare view patterns such as [id -> x].
    -- Try the expr -> pattern form first, then fall back to normal patterns.
    listPatternElementParser :: TokParser Pattern
    listPatternElementParser = do
      mView <- MP.optional . MP.try $ do
        expr <- exprParser
        expectedTok TkReservedRightArrow
        inner <- patternParser
        let sp = mergeSourceSpans (getSourceSpan expr) (getSourceSpan inner)
        pure (PView sp expr inner)
      maybe patternParser pure mView

parenOrTuplePatternParser :: TokParser Pattern
parenOrTuplePatternParser = withSpan $ do
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
      -- View patterns like (expr -> pat) produce PView without PParen wrapping,
      -- matching the original parser behavior and the pretty-printer which
      -- adds its own parens for PView.
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
      pure (\span' -> PTuple span' tupleFlavor [])

    -- Try to parse the paren content as a view pattern: expr -> pat.
    -- Uses exprParser which stops before '->', then checks for the arrow.
    -- Returns Nothing if the content is not a view pattern.
    viewPatternParser :: LexTokenKind -> TokParser (Maybe (SourceSpan -> Pattern))
    viewPatternParser closeTok = MP.optional . MP.try $ do
      expr <- exprParser
      expectedTok TkReservedRightArrow
      inner <- patternParser
      expectedTok closeTok
      let sp = mergeSourceSpans (getSourceSpan expr) (getSourceSpan inner)
      pure (const (PView sp expr inner))

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
        operatorPatternParser = withSpan $ do
          tok' <- anySingle
          case lexTokenKind tok' of
            TkVarSym op -> pure (\span' -> PVar span' (mkUnqualifiedName NameVarSym op))
            TkConSym op -> pure (\span' -> PCon span' (qualifyName Nothing (mkUnqualifiedName NameConSym op)) [])
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
          inner <- patternParser
          let sp = mergeSourceSpans (getSourceSpan expr) (getSourceSpan inner)
          pure (PView sp expr inner)
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
              pure (\span' -> PUnboxedSum span' 0 arity first)
            Nothing -> do
              expectedTok closeTok
              if tupleFlavor == Boxed
                then pure (`PParen` first)
                else pure (\span' -> PTuple span' Unboxed [first])
        Just () -> do
          second <- parenPatElementParser
          more <- MP.many (expectedTok TkSpecialComma *> parenPatElementParser)
          expectedTok closeTok
          pure (\span' -> PTuple span' tupleFlavor (first : second : more))

    parseUnboxedSumPatLeadingBars closeTok = do
      -- Parse (# | | ... | pat | ... | #) where pattern is not in first slot
      _ <- expectedTok TkReservedPipe
      leadingBars <- MP.many (MP.try (expectedTok TkReservedPipe))
      let altIdx = 1 + length leadingBars
      inner <- parenPatElementParser
      trailingBars <- MP.many (expectedTok TkReservedPipe)
      expectedTok closeTok
      let arity = altIdx + 1 + length trailingBars
      pure (\span' -> PUnboxedSum span' altIdx arity inner)

isPatternAppHead :: Pattern -> Bool
isPatternAppHead pat =
  case pat of
    PCon {} -> True
    PVar _ name -> isConLikeNameType (unqualifiedNameType name)
    _ -> False
