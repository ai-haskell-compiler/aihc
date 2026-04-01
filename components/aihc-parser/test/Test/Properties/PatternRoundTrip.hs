{-# LANGUAGE OverloadedStrings #-}

module Test.Properties.PatternRoundTrip
  ( prop_patternPrettyRoundTrip,
  )
where

import Aihc.Parser (ParseResult (..), ParserConfig (..), defaultConfig, errorBundlePretty, parsePattern)
import Aihc.Parser.Lex (isReservedIdentifier)
import Aihc.Parser.Syntax (Extension (TemplateHaskell, UnboxedSums, UnboxedTuples))
import Data.Text (Text)
import qualified Data.Text as T
import Prettyprinter (Pretty (..), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Test.Properties.BareSyntax
import Test.Properties.ExprHelpers (normalizeExpr)
import Test.Properties.Identifiers (genIdent, shrinkIdent)
import Test.QuickCheck

newtype GenPattern = GenPattern {unGenPattern :: Pattern}
  deriving (Show)

patternConfig :: ParserConfig
patternConfig =
  defaultConfig
    { parserExtensions = [UnboxedTuples, UnboxedSums, TemplateHaskell]
    }

prop_patternPrettyRoundTrip :: GenPattern -> Property
prop_patternPrettyRoundTrip (GenPattern pat) =
  let source = renderStrict (layoutPretty defaultLayoutOptions (pretty (toSyntaxPattern pat)))
      expected = normalizePattern pat
   in counterexample (T.unpack source) $
        case parsePattern patternConfig source of
          ParseErr err ->
            counterexample (errorBundlePretty (Just source) err) False
          ParseOk parsed ->
            let actual = normalizePattern (erasePattern parsed)
             in counterexample ("expected: " <> show expected <> "\nactual: " <> show actual) (expected == actual)

instance Arbitrary GenPattern where
  arbitrary = GenPattern <$> sized (genPattern . min 3)
  shrink (GenPattern pat) = GenPattern <$> shrinkPattern pat

shrinkPattern :: Pattern -> [Pattern]
shrinkPattern pat =
  case pat of
    PVar name ->
      [PVar shrunk | shrunk <- shrinkIdent name]
    PWildcard -> []
    PLit lit ->
      [PLit shrunk | shrunk <- shrinkLiteral lit]
    PQuasiQuote quoter body ->
      [PQuasiQuote q body | q <- shrinkQuoterName quoter]
        <> [PQuasiQuote quoter b | b <- map T.pack (shrink (T.unpack body))]
    PTuple tupleFlavor elems ->
      shrinkTupleElems tupleFlavor elems
    PList elems ->
      [PList elems' | elems' <- shrinkList shrinkPattern elems]
    PCon con args ->
      [PCon con [] | not (null args)]
        <> [PCon con args' | args' <- shrinkList (map canonicalPatternAtom . shrinkPattern) args]
    PInfix lhs op rhs ->
      [canonicalPatternAtom lhs, canonicalPatternAtom rhs]
        <> [PInfix (canonicalPatternAtom lhs') op (canonicalPatternAtom rhs) | lhs' <- shrinkPattern lhs]
        <> [PInfix (canonicalPatternAtom lhs) op (canonicalPatternAtom rhs') | rhs' <- shrinkPattern rhs]
    PView expr inner ->
      [inner]
        <> [PView expr' inner | expr' <- shrinkViewExpr expr]
        <> [PView expr inner' | inner' <- shrinkPattern inner]
    PAs name inner ->
      [canonicalPatternAtom inner]
        <> [PAs name' (canonicalPatternAtom inner) | name' <- shrinkIdent name]
        <> [PAs name (canonicalPatternAtom inner') | inner' <- shrinkPattern inner]
    PStrict inner ->
      [canonicalPatternAtom inner]
        <> [PStrict (canonicalPatternAtom inner') | inner' <- shrinkPattern inner]
    PIrrefutable inner ->
      [canonicalPatternAtom inner]
        <> [PIrrefutable (canonicalPatternAtom inner') | inner' <- shrinkPattern inner]
    PNegLit lit ->
      [PLit lit]
        <> [PNegLit shrunk | shrunk <- shrinkNumericLiteral lit]
    PParen inner ->
      [inner] <> [PParen inner' | inner' <- shrinkPattern inner]
    PUnboxedSum altIdx arity inner ->
      [PUnboxedSum altIdx arity inner' | inner' <- shrinkPattern inner]
    PRecord con fields _ ->
      [PRecord con [] False | not (null fields)]
        <> [PRecord con fields' False | fields' <- shrinkList shrinkField fields]
    PTypeSig inner ty ->
      [inner]
        <> [PTypeSig inner' ty | inner' <- shrinkPattern inner]
    PSplice {} ->
      []

shrinkTupleElems :: TupleFlavor -> [Pattern] -> [Pattern]
shrinkTupleElems tupleFlavor elems =
  [ candidate
  | shrunk <- shrinkList shrinkPattern elems,
    candidate <- case shrunk of
      [] -> [PTuple tupleFlavor []]
      [_] -> []
      _ -> [PTuple tupleFlavor shrunk]
  ]

shrinkField :: (Text, Pattern) -> [(Text, Pattern)]
shrinkField (fieldName, fieldPat) =
  [(fieldName, shrunk) | shrunk <- shrinkPattern fieldPat]

shrinkLiteral :: Literal -> [Literal]
shrinkLiteral lit =
  case lit of
    LitInt value _ -> [mkIntLiteral shrunk | shrunk <- shrinkIntegral value]
    LitIntHash value _ -> [LitIntHash shrunk (T.pack (show shrunk) <> "#") | shrunk <- shrinkIntegral value]
    LitIntBase value _ -> [mkHexLiteral shrunk | shrunk <- shrinkIntegral value, shrunk >= 0]
    LitIntBaseHash value _ -> [LitIntBaseHash shrunk ("0x" <> T.pack (showHex shrunk) <> "#") | shrunk <- shrinkIntegral value, shrunk >= 0]
    LitFloat value _ -> [mkFloatLiteral shrunk | shrunk <- shrinkFloat value, shrunk >= 0]
    LitFloatHash value _ -> [LitFloatHash shrunk (T.pack (show shrunk) <> "#") | shrunk <- shrinkFloat value, shrunk >= 0]
    LitChar c _ -> [mkCharLiteral shrunk | shrunk <- shrink c]
    LitCharHash c _ -> [LitCharHash shrunk (T.pack (show shrunk) <> "#") | shrunk <- shrink c]
    LitString txt _ -> [mkStringLiteral (T.pack shrunk) | shrunk <- shrink (T.unpack txt)]
    LitStringHash txt _ -> [LitStringHash (T.pack shrunk) (T.pack (show shrunk) <> "#") | shrunk <- shrink (T.unpack txt)]

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
        [ PVar <$> genIdent,
          pure PWildcard,
          PLit <$> genLiteral,
          PQuasiQuote <$> genQuoterName <*> genQuasiBody,
          PTuple Boxed <$> elements [[], [PVar "x", PWildcard]],
          PTuple Unboxed <$> elements [[], [PVar "x", PWildcard]],
          pure (PList []),
          PCon <$> genPatternConName <*> pure [],
          PNegLit <$> genNumericLiteral,
          genUnboxedSumPattern 0
        ]
  | otherwise =
      frequency
        [ (3, PVar <$> genIdent),
          (2, pure PWildcard),
          (3, PLit <$> genLiteral),
          (2, PQuasiQuote <$> genQuoterName <*> genQuasiBody),
          (2, PTuple Boxed <$> genTupleElems (depth - 1)),
          (1, PTuple Unboxed <$> genTupleElems (depth - 1)),
          (2, PList <$> genListElems (depth - 1)),
          (3, genPatternCon depth),
          (2, genPatternInfix depth),
          (2, PView <$> genViewExpr <*> genPattern (depth - 1)),
          (2, PAs <$> genIdent <*> (canonicalPatternAtom <$> genPattern (depth - 1))),
          (2, PStrict . canonicalPatternAtom <$> genPattern (depth - 1)),
          (2, PIrrefutable . canonicalPatternAtom <$> genPattern (depth - 1)),
          (2, PNegLit <$> genNumericLiteral),
          (2, PParen <$> genPattern (depth - 1)),
          (2, PRecord <$> genPatternConName <*> genRecordFields (depth - 1) <*> pure False),
          (2, genPatternTypeSig depth),
          (1, genUnboxedSumPattern (depth - 1)),
          (2, PSplice <$> genPatSpliceBody)
        ]

genPatternCon :: Int -> Gen Pattern
genPatternCon depth = do
  con <- genPatternConName
  argCount <- chooseInt (0, 3)
  args <- vectorOf argCount (canonicalPatternAtom <$> genPattern (depth - 1))
  pure (PCon con args)

genPatternTypeSig :: Int -> Gen Pattern
genPatternTypeSig depth = do
  inner <- genPattern (depth - 1)
  PParen . PTypeSig inner <$> genPatternType

genPatternType :: Gen Type
genPatternType =
  oneof
    [ TVar <$> genIdent,
      (`TCon` Unpromoted) <$> genPatternConName
    ]

genPatternInfix :: Int -> Gen Pattern
genPatternInfix depth = do
  lhs <- canonicalPatternAtom <$> genPattern (depth - 1)
  op <- genConOperator
  rhs <- canonicalPatternAtom <$> genPattern (depth - 1)
  pure (PInfix lhs op rhs)

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
  pure (PUnboxedSum altIdx arity inner)

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
    [ EVar <$> genIdent,
      mkIntExpr <$> chooseInteger (0, 999),
      EParen . EVar <$> genIdent
    ]

genPatSpliceBody :: Gen Expr
genPatSpliceBody =
  oneof
    [ EVar <$> genIdent,
      EParen . EVar <$> genIdent
    ]

shrinkViewExpr :: Expr -> [Expr]
shrinkViewExpr expr =
  case expr of
    EVar name -> [EVar shrunk | shrunk <- shrinkIdent name]
    EInt value _ -> [mkIntExpr shrunk | shrunk <- shrinkIntegral value]
    EParen inner -> inner : [EParen inner' | inner' <- shrinkViewExpr inner]
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
    else PParen pat

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
mkIntLiteral value = LitInt value (T.pack (show value))

mkHexLiteral :: Integer -> Literal
mkHexLiteral value = LitIntBase value ("0x" <> T.pack (showHex value))

mkFloatLiteral :: Double -> Literal
mkFloatLiteral value = LitFloat value (T.pack (show value))

mkCharLiteral :: Char -> Literal
mkCharLiteral value = LitChar value (T.pack (show value))

mkStringLiteral :: Text -> Literal
mkStringLiteral value = LitString value (T.pack (show (T.unpack value)))

mkIntExpr :: Integer -> Expr
mkIntExpr value = EInt value (T.pack (show value))

showHex :: Integer -> String
showHex value
  | value < 16 = [hexDigit value]
  | otherwise = showHex (value `div` 16) <> [hexDigit (value `mod` 16)]
  where
    hexDigit n = "0123456789abcdef" !! fromInteger n

normalizePattern :: Pattern -> Pattern
normalizePattern pat =
  case pat of
    PVar name -> PVar name
    PWildcard -> PWildcard
    PLit lit -> PLit (normalizeLiteral lit)
    PQuasiQuote quoter body -> PQuasiQuote quoter body
    PTuple tupleFlavor elems -> PTuple tupleFlavor (map normalizePattern elems)
    PList elems -> PList (map normalizePattern elems)
    PCon con args -> PCon con (map normalizePattern args)
    PInfix lhs op rhs -> PInfix (normalizePattern lhs) op (normalizePattern rhs)
    PView expr inner -> PView (normalizeViewExpr expr) (normalizePattern inner)
    PAs name inner -> PAs name (normalizeAsInner inner)
    PStrict inner -> PStrict (normalizeUnaryInner inner)
    PIrrefutable inner -> PIrrefutable (normalizeUnaryInner inner)
    PNegLit lit -> PNegLit (normalizeLiteral lit)
    PParen inner -> PParen (normalizePattern inner)
    PUnboxedSum altIdx arity inner -> PUnboxedSum altIdx arity (normalizePattern inner)
    PRecord con fields rwc -> PRecord con [(fieldName, normalizePattern fieldPat) | (fieldName, fieldPat) <- fields] rwc
    PTypeSig inner ty -> PTypeSig (normalizePattern inner) (normalizeTypeSpan ty)
    PSplice body -> PSplice (normalizeExpr body)

normalizeTypeSpan :: Type -> Type
normalizeTypeSpan ty =
  case ty of
    TVar name -> TVar name
    TCon name promoted -> TCon name promoted
    TTypeLit lit -> TTypeLit lit
    TStar -> TStar
    TQuasiQuote quoter body -> TQuasiQuote quoter body
    TForall binders inner -> TForall binders (normalizeTypeSpan inner)
    TApp lhs rhs -> TApp (normalizeTypeSpan lhs) (normalizeTypeSpan rhs)
    TFun lhs rhs -> TFun (normalizeTypeSpan lhs) (normalizeTypeSpan rhs)
    TTuple tupleFlavor promoted elems -> TTuple tupleFlavor promoted (map normalizeTypeSpan elems)
    TList promoted inner -> TList promoted (normalizeTypeSpan inner)
    TParen inner -> TParen (normalizeTypeSpan inner)
    TContext constraints inner -> TContext constraints (normalizeTypeSpan inner)
    TUnboxedSum elems -> TUnboxedSum (map normalizeTypeSpan elems)
    TSplice body -> TSplice (normalizeExpr body)

normalizeLiteral :: Literal -> Literal
normalizeLiteral lit =
  case lit of
    LitInt value repr -> LitInt value repr
    LitIntHash value repr -> LitIntHash value repr
    LitIntBase value repr -> LitIntBase value repr
    LitIntBaseHash value repr -> LitIntBaseHash value repr
    LitFloat value repr -> LitFloat value repr
    LitFloatHash value repr -> LitFloatHash value repr
    LitChar value repr -> LitChar value repr
    LitCharHash value repr -> LitCharHash value repr
    LitString value repr -> LitString value repr
    LitStringHash value repr -> LitStringHash value repr

normalizeViewExpr :: Expr -> Expr
normalizeViewExpr expr =
  case expr of
    EVar name -> EVar name
    EInt value repr -> EInt value repr
    EParen inner -> EParen (normalizeViewExpr inner)
    _ -> expr

normalizeUnaryInner :: Pattern -> Pattern
normalizeUnaryInner pat =
  case normalizePattern pat of
    PParen inner@(PNegLit {}) -> inner
    PParen inner@(PStrict {}) -> inner
    PParen inner@(PIrrefutable {}) -> inner
    other -> other

normalizeAsInner :: Pattern -> Pattern
normalizeAsInner pat =
  case normalizePattern pat of
    PParen inner@(PNegLit {}) -> inner
    PParen inner@(PStrict {}) -> inner
    PParen inner@(PIrrefutable {}) -> inner
    other -> other
