{-# LANGUAGE OverloadedStrings #-}

module Test.Properties.PatternRoundTrip
  ( prop_patternPrettyRoundTrip,
  )
where

import AIhc.Parser.Syntax
import Aihc.Parser (ParseResult (..), defaultConfig, errorBundlePretty, parsePattern)
import Aihc.Parser.Lex (isReservedIdentifier)
import Data.Data (dataTypeConstrs, dataTypeOf, showConstr, toConstr)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Prettyprinter (Pretty (..), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Test.Properties.Identifiers (genIdent, shrinkIdent)
import Test.QuickCheck

span0 :: SourceSpan
span0 = noSourceSpan

newtype GenPattern = GenPattern {unGenPattern :: Pattern}
  deriving (Show)

prop_patternPrettyRoundTrip :: GenPattern -> Property
prop_patternPrettyRoundTrip (GenPattern pat) =
  let source = renderStrict (layoutPretty defaultLayoutOptions (pretty pat))
      expected = normalizePattern pat
   in checkCoverage $
        applyCoverage (patternCtorCoverage pat) $
          counterexample (T.unpack source) $
            case parsePattern defaultConfig source of
              ParseErr err ->
                counterexample (errorBundlePretty err) False
              ParseOk parsed ->
                let actual = normalizePattern parsed
                 in counterexample ("expected: " <> show expected <> "\nactual: " <> show actual) (expected == actual)

patternCtorCoverage :: Pattern -> [Property -> Property]
patternCtorCoverage pat =
  let allCtors = map showConstr (dataTypeConstrs (dataTypeOf (undefined :: Pattern)))
      seenCtors = patternCtorNames pat
   in [cover 1 (ctor `Set.member` seenCtors) ctor | ctor <- allCtors]

applyCoverage :: [Property -> Property] -> Property -> Property
applyCoverage wrappers prop = foldr (\wrap acc -> wrap acc) prop wrappers

patternCtorNames :: Pattern -> Set.Set String
patternCtorNames pat =
  let here = Set.singleton (showConstr (toConstr pat))
   in case pat of
        PVar {} -> here
        PWildcard {} -> here
        PLit {} -> here
        PQuasiQuote {} -> here
        PTuple _ elems -> here <> mconcat (map patternCtorNames elems)
        PList _ elems -> here <> mconcat (map patternCtorNames elems)
        PCon _ _ args -> here <> mconcat (map patternCtorNames args)
        PInfix _ lhs _ rhs -> here <> patternCtorNames lhs <> patternCtorNames rhs
        PView _ _ inner -> here <> patternCtorNames inner
        PAs _ _ inner -> here <> patternCtorNames inner
        PStrict _ inner -> here <> patternCtorNames inner
        PIrrefutable _ inner -> here <> patternCtorNames inner
        PNegLit {} -> here
        PParen _ inner -> here <> patternCtorNames inner
        PRecord _ _ fields -> here <> mconcat [patternCtorNames fieldPat | (_, fieldPat) <- fields]

instance Arbitrary GenPattern where
  arbitrary = GenPattern <$> sized (genPattern . min 3)
  shrink (GenPattern pat) = GenPattern <$> shrinkPattern pat

shrinkPattern :: Pattern -> [Pattern]
shrinkPattern pat =
  case pat of
    PVar _ name ->
      [PVar span0 shrunk | shrunk <- shrinkIdent name]
    PWildcard _ -> []
    PLit _ lit ->
      [PLit span0 shrunk | shrunk <- shrinkLiteral lit]
    PQuasiQuote _ quoter body ->
      [PQuasiQuote span0 q body | q <- shrinkQuoterName quoter]
        <> [PQuasiQuote span0 quoter b | b <- map T.pack (shrink (T.unpack body))]
    PTuple _ elems ->
      shrinkTupleElems elems
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
    PRecord _ con fields ->
      [PRecord span0 con [] | not (null fields)]
        <> [PRecord span0 con fields' | fields' <- shrinkList shrinkField fields]

shrinkTupleElems :: [Pattern] -> [Pattern]
shrinkTupleElems elems =
  [ candidate
  | shrunk <- shrinkList shrinkPattern elems,
    candidate <- case shrunk of
      [] -> [PTuple span0 []]
      [_] -> []
      _ -> [PTuple span0 shrunk]
  ]

shrinkField :: (Text, Pattern) -> [(Text, Pattern)]
shrinkField (fieldName, fieldPat) =
  [(fieldName, shrunk) | shrunk <- shrinkPattern fieldPat]

shrinkLiteral :: Literal -> [Literal]
shrinkLiteral lit =
  case lit of
    LitInt _ value _ -> [mkIntLiteral shrunk | shrunk <- shrinkIntegral value]
    LitIntBase _ value _ -> [mkHexLiteral shrunk | shrunk <- shrinkIntegral value, shrunk >= 0]
    LitFloat _ value _ -> [mkFloatLiteral shrunk | shrunk <- shrinkFloat value, shrunk >= 0]
    LitChar _ c _ -> [mkCharLiteral shrunk | shrunk <- shrink c]
    LitString _ txt _ -> [mkStringLiteral (T.pack shrunk) | shrunk <- shrink (T.unpack txt)]

shrinkNumericLiteral :: Literal -> [Literal]
shrinkNumericLiteral lit =
  case lit of
    LitInt {} -> shrinkLiteral lit
    LitIntBase {} -> shrinkLiteral lit
    LitFloat {} -> shrinkLiteral lit
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
          PTuple span0 <$> elements [[], [PVar span0 "x", PWildcard span0]],
          pure (PList span0 []),
          PCon span0 <$> genPatternConName <*> pure [],
          PNegLit span0 <$> genNumericLiteral
        ]
  | otherwise =
      frequency
        [ (3, PVar span0 <$> genIdent),
          (2, pure (PWildcard span0)),
          (3, PLit span0 <$> genLiteral),
          (2, PQuasiQuote span0 <$> genQuoterName <*> genQuasiBody),
          (2, PTuple span0 <$> genTupleElems (depth - 1)),
          (2, PList span0 <$> genListElems (depth - 1)),
          (3, genPatternCon depth),
          (2, genPatternInfix depth),
          (2, PView span0 <$> genViewExpr <*> genPattern (depth - 1)),
          (2, PAs span0 <$> genIdent <*> (canonicalPatternAtom <$> genPattern (depth - 1))),
          (2, PStrict span0 . canonicalPatternAtom <$> genPattern (depth - 1)),
          (2, PIrrefutable span0 . canonicalPatternAtom <$> genPattern (depth - 1)),
          (2, PNegLit span0 <$> genNumericLiteral),
          (2, PParen span0 <$> genPattern (depth - 1)),
          (2, PRecord span0 <$> genPatternConName <*> genRecordFields (depth - 1))
        ]

genPatternCon :: Int -> Gen Pattern
genPatternCon depth = do
  con <- genPatternConName
  argCount <- chooseInt (0, 3)
  args <- vectorOf argCount (canonicalPatternAtom <$> genPattern (depth - 1))
  pure (PCon span0 con args)

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
  pure (T.pack (first : rest))

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
    PVar _ name -> PVar span0 name
    PWildcard _ -> PWildcard span0
    PLit _ lit -> PLit span0 (normalizeLiteral lit)
    PQuasiQuote _ quoter body -> PQuasiQuote span0 quoter body
    PTuple _ elems -> PTuple span0 (map normalizePattern elems)
    PList _ elems -> PList span0 (map normalizePattern elems)
    PCon _ con args -> PCon span0 con (map normalizePattern args)
    PInfix _ lhs op rhs -> PInfix span0 (normalizePattern lhs) op (normalizePattern rhs)
    PView _ expr inner -> PView span0 (normalizeViewExpr expr) (normalizePattern inner)
    PAs _ name inner -> PAs span0 name (normalizeAsInner inner)
    PStrict _ inner -> PStrict span0 (normalizeUnaryInner inner)
    PIrrefutable _ inner -> PIrrefutable span0 (normalizeUnaryInner inner)
    PNegLit _ lit -> PNegLit span0 (normalizeLiteral lit)
    PParen _ inner -> PParen span0 (normalizePattern inner)
    PRecord _ con fields -> PRecord span0 con [(fieldName, normalizePattern fieldPat) | (fieldName, fieldPat) <- fields]

normalizeLiteral :: Literal -> Literal
normalizeLiteral lit =
  case lit of
    LitInt _ value repr -> LitInt span0 value repr
    LitIntBase _ value repr -> LitIntBase span0 value repr
    LitFloat _ value repr -> LitFloat span0 value repr
    LitChar _ value repr -> LitChar span0 value repr
    LitString _ value repr -> LitString span0 value repr

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
