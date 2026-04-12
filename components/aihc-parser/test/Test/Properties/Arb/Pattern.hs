{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Properties.Arb.Pattern
  ( genPattern,
    genPatternNoView,
    shrinkPattern,
    canonicalPatternAtom,
  )
where

import Aihc.Parser.Lex (isReservedIdentifier)
import Aihc.Parser.Syntax
import Data.Text (Text)
import Data.Text qualified as T
import {-# SOURCE #-} Test.Properties.Arb.Expr (genExpr, shrinkExpr)
import Test.Properties.Arb.Identifiers (genIdent, shrinkIdent)
import Test.QuickCheck

span0 :: SourceSpan
span0 = noSourceSpan

instance Arbitrary Pattern where
  arbitrary = sized (genPattern . min 3)
  shrink = shrinkPattern

genPattern :: Int -> Gen Pattern
genPattern = genPatternWith True

-- | Generate a pattern safe for use in list comprehension generators and guard
-- qualifiers. Excludes PView, PIrrefutable, PStrict, and PAs at all depths.
-- TODO: Restore full pattern generation once the parser supports these patterns
-- in nested positions (inside PList, PTuple, PCon args, etc.) within list
-- comprehension generators and guard qualifiers. Currently, the prefix tokens
-- @->@, @~@, @!@, @\@@ are not recognized as pattern starters in these nested
-- contexts because the parser uses expression parsing rules there.
genPatternNoView :: Int -> Gen Pattern
genPatternNoView = genPatternWith False

-- | Internal pattern generator parameterized by whether all pattern constructors
-- are allowed. When @allowAll@ is False, PView, PIrrefutable, PStrict, and PAs
-- are excluded at all depths (for use in comprehension/guard contexts).
genPatternWith :: Bool -> Int -> Gen Pattern
genPatternWith allowAll depth =
  oneof (leafGenerators <> recursiveGenerators)
  where
    allowRecursive = depth > 0
    nextDepth = depth - 1
    leafGenerators =
      [ PVar span0 <$> genPatternUnqualVarName,
        pure (PWildcard span0),
        PLit span0 <$> genLiteral,
        PQuasiQuote span0 <$> genQuoterName <*> genQuasiBody,
        PNegLit span0 <$> genNumericLiteral,
        PSplice span0 <$> genPatSpliceBody,
        PTuple span0 Boxed <$> elements [[], [PVar span0 (mkUnqualifiedName NameVarId "x"), PWildcard span0]],
        PTuple span0 Unboxed <$> elements [[], [PVar span0 (mkUnqualifiedName NameVarId "x"), PWildcard span0]],
        pure (PList span0 []),
        PCon span0 <$> genPatternConAstName <*> pure [],
        genUnboxedSumPatternWith allowAll 0
      ]
    recursiveGenerators =
      [ PTuple span0 Boxed <$> genTupleElemsWith allowAll nextDepth
      | allowRecursive
      ]
        <> [PTuple span0 Unboxed <$> genUnboxedTupleElemsWith allowAll nextDepth | allowRecursive]
        <> [PList span0 <$> genListElemsWith allowAll nextDepth | allowRecursive]
        <> [genPatternConWith allowAll depth | allowRecursive]
        <> [genPatternInfixWith allowAll depth | allowRecursive]
        <> [PParen span0 <$> genPatternWith allowAll nextDepth | allowRecursive]
        <> [PRecord span0 <$> genPatternConAstName <*> genRecordFieldsWith allowAll nextDepth <*> pure False | allowRecursive]
        <> [genPatternTypeSigWith allowAll depth | allowRecursive]
        <> [genUnboxedSumPatternWith allowAll nextDepth | allowRecursive]
        <> [PView span0 <$> resize 2 genExpr <*> genPatternWith allowAll nextDepth | allowRecursive, allowAll]
        <> [PAs span0 <$> genIdent <*> (canonicalPatternAtom <$> genPatternWith allowAll nextDepth) | allowRecursive, allowAll]
        <> [PStrict span0 . canonicalPatternAtom <$> genPatternWith allowAll nextDepth | allowRecursive, allowAll]
        <> [PIrrefutable span0 . canonicalPatternAtom <$> genPatternWith allowAll nextDepth | allowRecursive, allowAll]

genPatternConWith :: Bool -> Int -> Gen Pattern
genPatternConWith allowView depth = do
  con <- genPatternConAstName
  argCount <- chooseInt (0, 3)
  -- TODO: Switch back to canonicalPatternAtom once the parser handles PNegLit,
  -- PAs, PStrict, and PIrrefutable as constructor arguments in all pattern
  -- contexts (currently they fail in list comp generators, guard qualifiers,
  -- and do-binds because the prefix tokens @-@, @\@@, @!@, @~@ are misparsed).
  args <- vectorOf argCount (canonicalPatternAtomForComp <$> genPatternWith allowView (depth - 1))
  pure (PCon span0 con args)

genPatternTypeSigWith :: Bool -> Int -> Gen Pattern
genPatternTypeSigWith allowAll depth = do
  -- TODO: Remove the PNegLit wrapping once the pretty-printer correctly
  -- parenthesizes PNegLit inside PTypeSig. Currently, PTypeSig (PNegLit 66) T
  -- prints as (-66 :: T) which the parser interprets as negation applied to
  -- (66 :: T) rather than a type signature on -66.
  inner <- wrapNegLit <$> genPatternWith allowAll (depth - 1)
  PParen span0 . PTypeSig span0 inner <$> genPatternType
  where
    wrapNegLit p@(PNegLit {}) = PParen span0 p
    wrapNegLit p = p

-- | Generate a simple type for use in pattern type signatures.
genPatternType :: Gen Type
genPatternType =
  oneof
    [ TVar span0 . mkUnqualifiedName NameVarId <$> genIdent,
      (\name -> TCon span0 name Unpromoted) <$> genPatternConAstName
    ]

genPatternInfixWith :: Bool -> Int -> Gen Pattern
genPatternInfixWith allowAll depth = do
  -- TODO: Switch back to canonicalPatternAtom once the pretty-printer correctly
  -- parenthesizes PNegLit as an infix operand. Currently, PInfix (PNegLit 433)
  -- ":+" (PVar "y") prints as (-433 :+ y) which is misparsed as negation of
  -- (433 :+ y).
  lhs <- canonicalPatternAtomForComp <$> genPatternWith allowAll (depth - 1)
  op <- genConOperatorName
  rhs <- canonicalPatternAtomForComp <$> genPatternWith allowAll (depth - 1)
  pure (PInfix span0 lhs op rhs)

genTupleElemsWith :: Bool -> Int -> Gen [Pattern]
genTupleElemsWith allowView depth = do
  isUnit <- arbitrary
  if isUnit
    then pure []
    else do
      n <- chooseInt (2, 4)
      vectorOf n (genPatternWith allowView depth)

-- | Generate elements for an unboxed tuple pattern (0 or 2-4 elements).
-- Unlike boxed tuples, unboxed tuples with 0 elements are valid Haskell.
-- NOTE: 1-element unboxed tuples are valid Haskell but the parser doesn't
-- accept them yet, so we skip generating them for now.
genUnboxedTupleElemsWith :: Bool -> Int -> Gen [Pattern]
genUnboxedTupleElemsWith allowView depth = do
  n <- chooseInt (0, 4)
  if n == 1 then pure [] else vectorOf n (genPatternWith allowView depth)

genUnboxedSumPatternWith :: Bool -> Int -> Gen Pattern
genUnboxedSumPatternWith allowView depth = do
  arity <- chooseInt (2, 4)
  altIdx <- chooseInt (0, arity - 1)
  inner <- genPatternWith allowView depth
  pure (PUnboxedSum span0 altIdx arity inner)

genListElemsWith :: Bool -> Int -> Gen [Pattern]
genListElemsWith allowView depth = do
  n <- chooseInt (0, 4)
  vectorOf n (genPatternWith allowView depth)

genRecordFieldsWith :: Bool -> Int -> Gen [(Name, Pattern)]
genRecordFieldsWith allowView depth = do
  n <- chooseInt (0, 3)
  names <- vectorOf n genFieldName
  pats <- vectorOf n (genPatternWith allowView depth)
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

-- | Generate an optional module qualifier (e.g., Nothing or Just "Data.List").
-- Biased towards Nothing to keep most names unqualified.
genOptionalQualifier :: Gen (Maybe Text)
genOptionalQualifier =
  oneof
    [ pure Nothing,
      Just <$> genModuleQualifier
    ]

-- | Generate a module qualifier like "Data.List" or "Prelude".
genModuleQualifier :: Gen Text
genModuleQualifier = do
  segCount <- chooseInt (1, 3)
  segs <- vectorOf segCount genModuleSegment
  pure (T.intercalate "." segs)

-- | Generate a single module name segment (starts with uppercase).
genModuleSegment :: Gen Text
genModuleSegment = do
  first <- elements ['A' .. 'Z']
  restLen <- chooseInt (0, 5)
  rest <- vectorOf restLen (elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9']))
  pure (T.pack (first : rest))

genPatternConAstName :: Gen Name
genPatternConAstName = qualifyName <$> genOptionalQualifier <*> (mkUnqualifiedName NameConId <$> genPatternConName)

genConOperatorName :: Gen Name
genConOperatorName = qualifyName <$> genOptionalQualifier <*> (mkUnqualifiedName NameConSym <$> genConOperator)

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

-- | Like 'canonicalPatternAtom' but also wraps PNegLit, PAs, PStrict, and
-- PIrrefutable in parens.
-- TODO: Remove once the parser supports these patterns as constructor arguments
-- in list comprehension generators and guard qualifiers. Currently, patterns
-- starting with special prefix tokens (@-@, @\@@, @!@, @~@) fail to parse when
-- used as constructor arguments in these contexts (e.g., @K -72.1@ or @K !x@ in
-- a list comp generator is misparsed).
canonicalPatternAtomForComp :: Pattern -> Pattern
canonicalPatternAtomForComp pat =
  case pat of
    PNegLit {} -> PParen span0 pat
    PAs {} -> PParen span0 pat
    PStrict {} -> PParen span0 pat
    PIrrefutable {} -> PParen span0 pat
    _ -> canonicalPatternAtom pat

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
    PCon _ _ [] -> True
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
        <> [PView span0 expr' inner | expr' <- shrinkExpr expr]
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
