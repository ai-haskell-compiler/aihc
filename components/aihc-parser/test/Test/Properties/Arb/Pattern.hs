{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Properties.Arb.Pattern
  ( genPattern,
    shrinkPattern,
    canonicalPatternAtom,
  )
where

import Aihc.Parser.Lex (isReservedIdentifier)
import Aihc.Parser.Syntax
import Data.Text (Text)
import Data.Text qualified as T
import Test.Properties.Arb.Identifiers (genIdent, shrinkIdent)
import Test.QuickCheck

span0 :: SourceSpan
span0 = noSourceSpan

instance Arbitrary Pattern where
  arbitrary = sized (genPattern . min 3)
  shrink = shrinkPattern

genPattern :: Int -> Gen Pattern
genPattern depth
  | depth <= 0 =
      oneof
        [ PVar span0 <$> genPatternUnqualVarName,
          pure (PWildcard span0),
          PLit span0 <$> genLiteral,
          PQuasiQuote span0 <$> genQuoterName <*> genQuasiBody,
          PTuple span0 Boxed <$> elements [[], [PVar span0 (mkUnqualifiedName NameVarId "x"), PWildcard span0]],
          PTuple span0 Unboxed <$> elements [[], [PVar span0 (mkUnqualifiedName NameVarId "x"), PWildcard span0]],
          pure (PList span0 []),
          PCon span0 <$> genPatternConAstName <*> pure [],
          PNegLit span0 <$> genNumericLiteral,
          genUnboxedSumPattern 0
        ]
  | otherwise =
      frequency
        [ (3, PVar span0 <$> genPatternUnqualVarName),
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
          (2, PRecord span0 <$> genPatternConAstName <*> genRecordFields (depth - 1) <*> pure False),
          (2, genPatternTypeSig depth),
          (1, genUnboxedSumPattern (depth - 1)),
          (2, PSplice span0 <$> genPatSpliceBody)
        ]

genPatternCon :: Int -> Gen Pattern
genPatternCon depth = do
  con <- genPatternConAstName
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
    [ TVar span0 . mkUnqualifiedName NameVarId <$> genIdent,
      (\name -> TCon span0 name Unpromoted) <$> genPatternConAstName
    ]

genPatternInfix :: Int -> Gen Pattern
genPatternInfix depth = do
  lhs <- canonicalPatternAtom <$> genPattern (depth - 1)
  op <- genConOperatorName
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

genRecordFields :: Int -> Gen [(Name, Pattern)]
genRecordFields depth = do
  n <- chooseInt (0, 3)
  names <- vectorOf n genFieldName
  pats <- vectorOf n (genPattern depth)
  pure (zip (map (qualifyName Nothing . mkUnqualifiedName NameVarId) names) pats)

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
    [ EVar span0 <$> genPatternVarName,
      (\v -> EInt span0 v (T.pack (show v))) <$> chooseInteger (0, 999),
      EParen span0 . EVar span0 <$> genPatternVarName
    ]

-- | Generate the body of a TH pattern splice: either a bare variable or a parenthesized expression.
genPatSpliceBody :: Gen Expr
genPatSpliceBody =
  oneof
    [ EVar span0 <$> genPatternVarName,
      EParen span0 . EVar span0 <$> genPatternVarName
    ]

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

genPatternVarName :: Gen Name
genPatternVarName = qualifyName Nothing . mkUnqualifiedName NameVarId <$> genIdent

genPatternUnqualVarName :: Gen UnqualifiedName
genPatternUnqualVarName = mkUnqualifiedName NameVarId <$> genIdent

genPatternConAstName :: Gen Name
genPatternConAstName = qualifyName Nothing . mkUnqualifiedName NameConId <$> genPatternConName

genConOperatorName :: Gen Name
genConOperatorName = qualifyName Nothing . mkUnqualifiedName NameConSym <$> genConOperator

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

showHex :: Integer -> String
showHex value
  | value < 16 = [hexDigit value]
  | otherwise = showHex (value `div` 16) <> [hexDigit (value `mod` 16)]
  where
    hexDigit n = "0123456789abcdef" !! fromInteger n

shrinkPattern :: Pattern -> [Pattern]
shrinkPattern pat =
  case pat of
    PAnn _ sub -> shrinkPattern sub
    PVar _ name ->
      [PVar span0 (name {unqualifiedNameText = shrunk}) | shrunk <- shrinkIdent (unqualifiedNameText name)]
    PWildcard _ -> []
    PLit _ lit ->
      [PLit span0 shrunk | shrunk <- shrinkLiteral lit]
    PQuasiQuote _ quoter body ->
      [PQuasiQuote span0 q body | q <- shrinkQuoterName quoter]
        <> [PQuasiQuote span0 quoter b | b <- map T.pack (shrink (T.unpack body))]
    PTuple _ tupleFlavor elems ->
      shrinkPatternTupleElems tupleFlavor elems
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

shrinkPatternTupleElems :: TupleFlavor -> [Pattern] -> [Pattern]
shrinkPatternTupleElems tupleFlavor elems =
  [ candidate
  | shrunk <- shrinkList shrinkPattern elems,
    candidate <- case shrunk of
      [] -> [PTuple span0 tupleFlavor []]
      [_] -> []
      _ -> [PTuple span0 tupleFlavor shrunk]
  ]

shrinkField :: (Name, Pattern) -> [(Name, Pattern)]
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

shrinkFloat :: Double -> [Double]
shrinkFloat value =
  [fromInteger shrunk / 10 | shrunk <- shrinkIntegral (round (value * 10 :: Double) :: Integer), shrunk >= 0]

shrinkQuoterName :: Text -> [Text]
shrinkQuoterName name =
  [ candidate
  | candidate <- map T.pack (shrink (T.unpack name)),
    isValidQuoterName candidate
  ]

shrinkViewExpr :: Expr -> [Expr]
shrinkViewExpr expr =
  case expr of
    EVar _ name -> [EVar span0 (name {nameText = shrunk}) | shrunk <- shrinkIdent (nameText name)]
    EInt _ value _ -> [EInt span0 shrunk (T.pack (show shrunk)) | shrunk <- shrinkIntegral value]
    EParen _ inner -> inner : [EParen span0 inner' | inner' <- shrinkViewExpr inner]
    _ -> []
