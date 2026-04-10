{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Properties.PatternRoundTrip
  ( prop_patternPrettyRoundTrip,
  )
where

import Aihc.Parser (ParseResult (..), ParserConfig (..), defaultConfig, parsePattern)
import Aihc.Parser.Lex (isReservedIdentifier)
import Aihc.Parser.Syntax
import Data.Text (Text)
import Data.Text qualified as T
import Prettyprinter (Pretty (..), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Test.Properties.Coverage (assertCtorCoverage)
import Test.Properties.ExprHelpers (normalizeExpr)
import Test.Properties.Identifiers (genIdent, shrinkIdent)
import Test.QuickCheck
import Text.Megaparsec.Error qualified as MPE

span0 :: SourceSpan
span0 = noSourceSpan

newtype GenPattern = GenPattern {unGenPattern :: Pattern}
  deriving (Show)

patternConfig :: ParserConfig
patternConfig =
  defaultConfig
    { parserExtensions = [BlockArguments, UnboxedTuples, UnboxedSums, TemplateHaskell]
    }

prop_patternPrettyRoundTrip :: GenPattern -> Property
prop_patternPrettyRoundTrip (GenPattern pat) =
  let source = renderStrict (layoutPretty defaultLayoutOptions (pretty pat))
      expected = normalizePattern pat
   in checkCoverage $
        assertCtorCoverage ["PAnn"] pat $
          counterexample (T.unpack source) $
            case parsePattern patternConfig source of
              ParseErr err ->
                counterexample (MPE.errorBundlePretty err) False
              ParseOk parsed ->
                let actual = normalizePattern parsed
                 in counterexample ("expected: " <> show expected <> "\nactual: " <> show actual) (expected == actual)

instance Arbitrary GenPattern where
  arbitrary = GenPattern <$> sized (genPattern . min 3)
  shrink (GenPattern pat) = GenPattern <$> shrinkPattern pat

shrinkPattern :: Pattern -> [Pattern]
shrinkPattern pat =
  case pat of
    PAnn _ sub -> shrinkPattern sub
    PVar _ name ->
      [PVar span0 shrunk | shrunk <- shrinkIdent name]
    PWildcard _ -> []
    PLit _ lit ->
      [PLit span0 shrunk | shrunk <- shrinkLiteral lit]
    PQuasiQuote _ quoter body ->
      [PQuasiQuote span0 q body | q <- shrinkQuoterName quoter]
        <> [PQuasiQuote span0 quoter b | b <- map T.pack (shrink (T.unpack body))]
    PTuple _ tupleFlavor elems ->
      shrinkTupleElems tupleFlavor elems
    PList _ elems ->
      [PList span0 elems' | elems' <- shrinkList shrinkPattern elems]
    PCon _ con args ->
      [PCon span0 con [] | not (null args)]
        <> [PCon span0 con args' | args' <- shrinkList (map canonicalPatternAtom . shrinkPattern) args]
    PInfix _ lhs op rhs ->
      [canonicalPatternAtom lhs, canonicalPatternAtom rhs]
        <> [PInfix span0 (canonicalPatternAtom lhs') op (canonicalPatternAtom rhs) | lhs' <- shrinkPattern lhs]
        <> [PInfix span0 (canonicalPatternAtom lhs) op (canonicalPatternAtom rhs') | rhs' <- shrinkPattern rhs]
    PView _ expr inner ->
      [inner]
        <> [PView span0 expr' inner | expr' <- shrinkViewExpr expr]
        <> [PView span0 expr inner' | inner' <- shrinkPattern inner]
    PAs _ name inner ->
      [canonicalPatternAtom inner]
        <> [PAs span0 name' (canonicalPatternAtom inner) | name' <- shrinkIdent name]
        <> [PAs span0 name (canonicalPatternAtom inner') | inner' <- shrinkPattern inner]
    PStrict _ inner ->
      [canonicalPatternAtom inner]
        <> [PStrict span0 (canonicalPatternAtom inner') | inner' <- shrinkPattern inner]
    PIrrefutable _ inner ->
      [canonicalPatternAtom inner]
        <> [PIrrefutable span0 (canonicalPatternAtom inner') | inner' <- shrinkPattern inner]
    PNegLit _ lit ->
      [PLit span0 lit]
        <> [PNegLit span0 shrunk | shrunk <- shrinkNumericLiteral lit]
    PParen _ inner ->
      [inner] <> [PParen span0 inner' | inner' <- shrinkPattern inner]
    PUnboxedSum _ altIdx arity inner ->
      [PUnboxedSum span0 altIdx arity inner' | inner' <- shrinkPattern inner]
    PRecord _ con fields _ ->
      [PRecord span0 con [] False | not (null fields)]
        <> [PRecord span0 con fields' False | fields' <- shrinkList shrinkField fields]
    PTypeSig _ inner ty ->
      [inner]
        <> [PTypeSig span0 inner' ty | inner' <- shrinkPattern inner]
    PSplice {} ->
      []

shrinkTupleElems :: TupleFlavor -> [Pattern] -> [Pattern]
shrinkTupleElems tupleFlavor elems =
  [ candidate
  | shrunk <- shrinkList shrinkPattern elems,
    candidate <- case shrunk of
      [] -> [PTuple span0 tupleFlavor []]
      [_] -> []
      _ -> [PTuple span0 tupleFlavor shrunk]
  ]

shrinkField :: (Text, Pattern) -> [(Text, Pattern)]
shrinkField (fieldName, fieldPat) =
  [(fieldName, shrunk) | shrunk <- shrinkPattern fieldPat]

shrinkLiteral :: Literal -> [Literal]
shrinkLiteral lit =
  case lit of
    LitInt _ value _ -> [mkIntLiteral shrunk | shrunk <- shrinkIntegral value]
    LitIntHash _ value _ -> [LitIntHash span0 shrunk (T.pack (show shrunk) <> "#") | shrunk <- shrinkIntegral value]
    LitIntBase _ value _ -> [mkHexLiteral shrunk | shrunk <- shrinkIntegral value, shrunk >= 0]
    LitIntBaseHash _ value _ -> [LitIntBaseHash span0 shrunk ("0x" <> T.pack (showHex shrunk) <> "#") | shrunk <- shrinkIntegral value, shrunk >= 0]
    LitFloat _ value _ -> [mkFloatLiteral shrunk | shrunk <- shrinkFloat value, shrunk >= 0]
    LitFloatHash _ value _ -> [LitFloatHash span0 shrunk (T.pack (show shrunk) <> "#") | shrunk <- shrinkFloat value, shrunk >= 0]
    LitChar _ c _ -> [mkCharLiteral shrunk | shrunk <- shrink c]
    LitCharHash _ c _ -> [LitCharHash span0 shrunk (T.pack (show shrunk) <> "#") | shrunk <- shrink c]
    LitString _ txt _ -> [mkStringLiteral (T.pack shrunk) | shrunk <- shrink (T.unpack txt)]
    LitStringHash _ txt _ -> [LitStringHash span0 (T.pack shrunk) (T.pack (show shrunk) <> "#") | shrunk <- shrink (T.unpack txt)]

shrinkNumericLiteral :: Literal -> [Literal]
shrinkNumericLiteral lit =
  case lit of
    LitInt {} -> shrinkLiteral lit
    LitIntHash {} -> shrinkLiteral lit
    LitIntBase {} -> shrinkLiteral lit
    LitIntBaseHash {} -> shrinkLiteral lit
    LitFloat {} -> shrinkLiteral lit
    LitFloatHash {} -> shrinkLiteral lit
    _ -> []

shrinkQuoterName :: Text -> [Text]
shrinkQuoterName name =
  [ candidate
  | candidate <- map T.pack (shrink (T.unpack name)),
    isValidQuoterName candidate
  ]

shrinkFloat :: Double -> [Double]
shrinkFloat value =
  [fromInteger shrunk / 10 | shrunk <- shrinkIntegral (round (value * 10 :: Double) :: Integer), shrunk >= 0]

genPattern :: Int -> Gen Pattern
genPattern depth
  | depth <= 0 =
      oneof
        [ PVar span0 <$> genIdent,
          pure (PWildcard span0),
          PLit span0 <$> genLiteral,
          PQuasiQuote span0 <$> genQuoterName <*> genQuasiBody,
          PTuple span0 Boxed <$> elements [[], [PVar span0 "x", PWildcard span0]],
          PTuple span0 Unboxed <$> elements [[], [PVar span0 "x", PWildcard span0]],
          pure (PList span0 []),
          PCon span0 <$> genPatternConName <*> pure [],
          PNegLit span0 <$> genNumericLiteral,
          genUnboxedSumPattern 0
        ]
  | otherwise =
      frequency
        [ (3, PVar span0 <$> genIdent),
          (2, pure (PWildcard span0)),
          (3, PLit span0 <$> genLiteral),
          (2, PQuasiQuote span0 <$> genQuoterName <*> genQuasiBody),
          (2, PTuple span0 Boxed <$> genTupleElems (depth - 1)),
          (1, PTuple span0 Unboxed <$> genTupleElems (depth - 1)),
          (2, PList span0 <$> genListElems (depth - 1)),
          (3, genPatternCon depth),
          (2, genPatternInfix depth),
          (2, PView span0 <$> genViewExpr <*> genPattern (depth - 1)),
          (2, PAs span0 <$> genIdent <*> (canonicalPatternAtom <$> genPattern (depth - 1))),
          (2, PStrict span0 . canonicalPatternAtom <$> genPattern (depth - 1)),
          (2, PIrrefutable span0 . canonicalPatternAtom <$> genPattern (depth - 1)),
          (2, PNegLit span0 <$> genNumericLiteral),
          (2, PParen span0 <$> genPattern (depth - 1)),
          (2, PRecord span0 <$> genPatternConName <*> genRecordFields (depth - 1) <*> pure False),
          (2, genPatternTypeSig depth),
          (1, genUnboxedSumPattern (depth - 1)),
          (2, PSplice span0 <$> genPatSpliceBody)
        ]

genPatternCon :: Int -> Gen Pattern
genPatternCon depth = do
  con <- genPatternConName
  argCount <- chooseInt (0, 3)
  args <- vectorOf argCount (canonicalPatternAtom <$> genPattern (depth - 1))
  pure (PCon span0 con args)

genPatternTypeSig :: Int -> Gen Pattern
genPatternTypeSig depth = do
  inner <- genPattern (depth - 1)
  PParen span0 . PTypeSig span0 inner <$> genPatternType

-- | Generate a simple type for use in pattern type signatures.
genPatternType :: Gen Type
genPatternType =
  oneof
    [ TVar span0 <$> genIdent,
      (\name -> TCon span0 name Unpromoted) <$> genPatternConName
    ]

genPatternInfix :: Int -> Gen Pattern
genPatternInfix depth = do
  lhs <- canonicalPatternAtom <$> genPattern (depth - 1)
  op <- genConOperator
  rhs <- canonicalPatternAtom <$> genPattern (depth - 1)
  pure (PInfix span0 lhs op rhs)

genTupleElems :: Int -> Gen [Pattern]
genTupleElems depth = do
  isUnit <- arbitrary
  if isUnit
    then pure []
    else do
      n <- chooseInt (2, 4)
      vectorOf n (genPattern depth)

genUnboxedSumPattern :: Int -> Gen Pattern
genUnboxedSumPattern depth = do
  arity <- chooseInt (2, 4)
  altIdx <- chooseInt (0, arity - 1)
  inner <- genPattern depth
  pure (PUnboxedSum span0 altIdx arity inner)

genListElems :: Int -> Gen [Pattern]
genListElems depth = do
  n <- chooseInt (0, 4)
  vectorOf n (genPattern depth)

genRecordFields :: Int -> Gen [(Text, Pattern)]
genRecordFields depth = do
  n <- chooseInt (0, 3)
  names <- vectorOf n genFieldName
  pats <- vectorOf n (genPattern depth)
  pure (zip names pats)

genLiteral :: Gen Literal
genLiteral =
  oneof
    [ mkIntLiteral <$> chooseInteger (0, 999),
      mkHexLiteral <$> chooseInteger (0, 255),
      mkFloatLiteral <$> genTenths,
      mkCharLiteral <$> genCharValue,
      mkStringLiteral <$> genStringValue
    ]

genNumericLiteral :: Gen Literal
genNumericLiteral =
  oneof
    [ mkIntLiteral <$> chooseInteger (0, 999),
      mkHexLiteral <$> chooseInteger (0, 255),
      mkFloatLiteral <$> genTenths
    ]

genTenths :: Gen Double
genTenths = do
  whole <- chooseInteger (0, 99)
  frac <- chooseInteger (0, 9)
  pure (fromInteger whole + fromInteger frac / 10)

genCharValue :: Gen Char
genCharValue = elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> " _")

genStringValue :: Gen Text
genStringValue = do
  len <- chooseInt (0, 8)
  T.pack <$> vectorOf len genCharValue

genViewExpr :: Gen Expr
genViewExpr =
  oneof
    [ EVar span0 <$> genIdent,
      mkIntExpr <$> chooseInteger (0, 999),
      EParen span0 . EVar span0 <$> genIdent
    ]

-- | Generate the body of a TH pattern splice: either a bare variable or a parenthesized expression.
genPatSpliceBody :: Gen Expr
genPatSpliceBody =
  oneof
    [ EVar span0 <$> genIdent,
      EParen span0 . EVar span0 <$> genIdent
    ]

shrinkViewExpr :: Expr -> [Expr]
shrinkViewExpr expr =
  case expr of
    EVar _ name -> [EVar span0 shrunk | shrunk <- shrinkIdent name]
    EInt _ value _ -> [mkIntExpr shrunk | shrunk <- shrinkIntegral value]
    EParen _ inner -> inner : [EParen span0 inner' | inner' <- shrinkViewExpr inner]
    _ -> []

genPatternConName :: Gen Text
genPatternConName = do
  first <- elements ['A' .. 'Z']
  restLen <- chooseInt (0, 5)
  rest <- vectorOf restLen (elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "_'"))
  pure (T.pack (first : rest))

genConOperator :: Gen Text
genConOperator = do
  first <- elements ":"
  restLen <- chooseInt (0, 3)
  rest <- vectorOf restLen (elements ":!#$%&*+./<=>?\\^|-~")
  let op = T.pack (first : rest)
  -- :: is not a valid constructor operator in patterns (it's a type signature)
  if op == "::" then genConOperator else pure op

genFieldName :: Gen Text
genFieldName = do
  first <- elements (['a' .. 'z'] <> ['_'])
  restLen <- chooseInt (0, 5)
  rest <- vectorOf restLen (elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "_'"))
  let candidate = T.pack (first : rest)
  if isReservedIdentifier candidate
    then genFieldName
    else pure candidate

genQuoterName :: Gen Text
genQuoterName = do
  first <- elements (['a' .. 'z'] <> ['_'])
  restLen <- chooseInt (0, 4)
  rest <- vectorOf restLen (elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "_'"))
  let candidate = T.pack (first : rest)
  if isValidQuoterName candidate
    then pure candidate
    else genQuoterName

isValidQuoterName :: Text -> Bool
isValidQuoterName name =
  case T.uncons name of
    Just (first, rest) ->
      (first `elem` (['a' .. 'z'] <> ['_']))
        && T.all (`elem` (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "_'.")) rest
        -- Exclude names that clash with TH quote brackets when TemplateHaskell is enabled
        && name `notElem` ["e", "t", "d", "p"]
    Nothing -> False

genQuasiBody :: Gen Text
genQuasiBody = do
  len <- chooseInt (0, 10)
  chars <- vectorOf len (elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> " +-*/_()"))
  pure (T.pack chars)

canonicalPatternAtom :: Pattern -> Pattern
canonicalPatternAtom pat =
  if isPatternAtom pat
    then pat
    else PParen span0 pat

isPatternAtom :: Pattern -> Bool
isPatternAtom pat =
  case pat of
    PVar {} -> True
    PWildcard {} -> True
    PLit {} -> True
    PQuasiQuote {} -> True
    PNegLit {} -> True
    PList {} -> True
    PTuple {} -> True
    PParen {} -> True
    PStrict {} -> True
    PView {} -> True
    PAs {} -> True
    PUnboxedSum {} -> True
    _ -> False

mkIntLiteral :: Integer -> Literal
mkIntLiteral value = LitInt span0 value (T.pack (show value))

mkHexLiteral :: Integer -> Literal
mkHexLiteral value = LitIntBase span0 value ("0x" <> T.pack (showHex value))

mkFloatLiteral :: Double -> Literal
mkFloatLiteral value = LitFloat span0 value (T.pack (show value))

mkCharLiteral :: Char -> Literal
mkCharLiteral value = LitChar span0 value (T.pack (show value))

mkStringLiteral :: Text -> Literal
mkStringLiteral value = LitString span0 value (T.pack (show (T.unpack value)))

mkIntExpr :: Integer -> Expr
mkIntExpr value = EInt span0 value (T.pack (show value))

showHex :: Integer -> String
showHex value
  | value < 16 = [hexDigit value]
  | otherwise = showHex (value `div` 16) <> [hexDigit (value `mod` 16)]
  where
    hexDigit n = "0123456789abcdef" !! fromInteger n

normalizePattern :: Pattern -> Pattern
normalizePattern pat =
  case pat of
    PAnn _ sub -> normalizePattern sub
    PVar _ name -> PVar span0 name
    PWildcard _ -> PWildcard span0
    PLit _ lit -> PLit span0 (normalizeLiteral lit)
    PQuasiQuote _ quoter body -> PQuasiQuote span0 quoter body
    PTuple _ tupleFlavor elems -> PTuple span0 tupleFlavor (map normalizePattern elems)
    PList _ elems -> PList span0 (map normalizePattern elems)
    PCon _ con args -> PCon span0 con (map normalizePattern args)
    PInfix _ lhs op rhs -> PInfix span0 (normalizePattern lhs) op (normalizePattern rhs)
    PView _ expr inner -> PView span0 (normalizeViewExpr expr) (normalizePattern inner)
    PAs _ name inner -> PAs span0 name (normalizeAsInner inner)
    PStrict _ inner -> PStrict span0 (normalizeUnaryInner inner)
    PIrrefutable _ inner -> PIrrefutable span0 (normalizeUnaryInner inner)
    PNegLit _ lit -> PNegLit span0 (normalizeLiteral lit)
    PParen _ inner -> PParen span0 (normalizePattern inner)
    PUnboxedSum _ altIdx arity inner -> PUnboxedSum span0 altIdx arity (normalizePattern inner)
    PRecord _ con fields rwc -> PRecord span0 con [(fieldName, normalizePattern fieldPat) | (fieldName, fieldPat) <- fields] rwc
    PTypeSig _ inner ty -> PTypeSig span0 (normalizePattern inner) (normalizeTypeSpan ty)
    PSplice _ body -> PSplice span0 (normalizeExpr body)

-- | Normalize source spans in a type (reset to noSourceSpan).
normalizeTypeSpan :: Type -> Type
normalizeTypeSpan ty =
  case ty of
    TVar _ name -> TVar span0 name
    TCon _ name promoted -> TCon span0 name promoted
    TImplicitParam _ name inner -> TImplicitParam span0 name (normalizeTypeSpan inner)
    TTypeLit _ lit -> TTypeLit span0 lit
    TStar _ -> TStar span0
    TQuasiQuote _ quoter body -> TQuasiQuote span0 quoter body
    TForall _ binders inner -> TForall span0 (map normalizeTyVarBinderSpan binders) (normalizeTypeSpan inner)
    TApp _ lhs rhs -> TApp span0 (normalizeTypeSpan lhs) (normalizeTypeSpan rhs)
    TFun _ lhs rhs -> TFun span0 (normalizeTypeSpan lhs) (normalizeTypeSpan rhs)
    TTuple _ tupleFlavor promoted elems -> TTuple span0 tupleFlavor promoted (map normalizeTypeSpan elems)
    TList _ promoted elems -> TList span0 promoted (map normalizeTypeSpan elems)
    TParen _ inner -> TParen span0 (normalizeTypeSpan inner)
    TKindSig _ inner kind -> TKindSig span0 (normalizeTypeSpan inner) (normalizeTypeSpan kind)
    TContext _ constraints inner -> TContext span0 (map normalizeTypeSpan constraints) (normalizeTypeSpan inner)
    TUnboxedSum _ elems -> TUnboxedSum span0 (map normalizeTypeSpan elems)
    TSplice _ body -> TSplice span0 (normalizeExpr body)
    TWildcard _ -> TWildcard span0
    TAnn ann sub -> TAnn ann (normalizeTypeSpan sub)

normalizeLiteral :: Literal -> Literal
normalizeLiteral lit =
  case lit of
    LitInt _ value repr -> LitInt span0 value repr
    LitIntHash _ value repr -> LitIntHash span0 value repr
    LitIntBase _ value repr -> LitIntBase span0 value repr
    LitIntBaseHash _ value repr -> LitIntBaseHash span0 value repr
    LitFloat _ value repr -> LitFloat span0 value repr
    LitFloatHash _ value repr -> LitFloatHash span0 value repr
    LitChar _ value repr -> LitChar span0 value repr
    LitCharHash _ value repr -> LitCharHash span0 value repr
    LitString _ value repr -> LitString span0 value repr
    LitStringHash _ value repr -> LitStringHash span0 value repr

normalizeViewExpr :: Expr -> Expr
normalizeViewExpr expr =
  case expr of
    EVar _ name -> EVar span0 name
    EInt _ value repr -> EInt span0 value repr
    EParen _ inner -> EParen span0 (normalizeViewExpr inner)
    _ -> expr

normalizeUnaryInner :: Pattern -> Pattern
normalizeUnaryInner pat =
  case normalizePattern pat of
    PParen _ inner@(PNegLit {}) -> inner
    PParen _ inner@(PStrict {}) -> inner
    PParen _ inner@(PIrrefutable {}) -> inner
    other -> other

-- | Normalize the inner pattern of an as-pattern.
-- The pretty-printer adds parens around negative literals after @ for safety (a@-0 is invalid),
-- and around strict/irrefutable patterns to avoid lexing @!/@~ as symbolic operators,
-- so we strip those parens to get the canonical form.
normalizeAsInner :: Pattern -> Pattern
normalizeAsInner pat =
  case normalizePattern pat of
    PParen _ inner@(PNegLit {}) -> inner
    PParen _ inner@(PStrict {}) -> inner
    PParen _ inner@(PIrrefutable {}) -> inner
    other -> other

normalizeTyVarBinderSpan :: TyVarBinder -> TyVarBinder
normalizeTyVarBinderSpan tvb =
  tvb
    { tyVarBinderSpan = span0,
      tyVarBinderKind = fmap normalizeTypeSpan (tyVarBinderKind tvb)
    }
