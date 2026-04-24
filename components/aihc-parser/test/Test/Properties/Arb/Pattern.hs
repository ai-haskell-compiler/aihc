{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Properties.Arb.Pattern
  ( genPattern,
    shrinkPattern,
  )
where

import Aihc.Parser.Syntax
import Data.Text (Text)
import Data.Text qualified as T
import {-# SOURCE #-} Test.Properties.Arb.Expr (genExpr, shrinkExpr)
import Test.Properties.Arb.Identifiers
  ( genCharValue,
    genConName,
    genFieldName,
    genOptionalQualifier,
    genQuasiBody,
    genQuoterName,
    genStringValue,
    genTenths,
    genVarId,
    genVarName,
    isValidQuoterName,
    showHex,
    shrinkFloat,
    shrinkIdent,
    shrinkName,
  )
import Test.Properties.Arb.Type (shrinkType)
import Test.QuickCheck

instance Arbitrary Pattern where
  arbitrary = scale (min 3) genPattern
  shrink = shrinkPattern

genPattern :: Gen Pattern
genPattern = scale (`div` 2) $ do
  n <- getSize
  if n <= 0
    then oneof leafGenerators
    else oneof (leafGenerators <> recursiveGenerators)
  where
    leafGenerators =
      [ PVar <$> genPatternUnqualVarName,
        pure PWildcard,
        PLit <$> genLiteral,
        PQuasiQuote <$> genQuoterName <*> genQuasiBody,
        PNegLit <$> genNumericLiteral,
        PSplice <$> genPatSpliceBody,
        PTuple Boxed <$> elements [[], [PVar (mkUnqualifiedName NameVarId "x"), PWildcard]],
        PTuple Unboxed <$> elements [[], [PVar (mkUnqualifiedName NameVarId "x")], [PVar (mkUnqualifiedName NameVarId "x"), PWildcard]],
        pure (PList []),
        PCon <$> genConName <*> pure [] <*> pure []
      ]
    recursiveGenerators =
      [ PTuple Boxed <$> genTupleElemsWith,
        PTuple Unboxed <$> genUnboxedTupleElemsWith,
        PList <$> genListElemsWith,
        genPatternConWith,
        genPatternInfixWith,
        PParen <$> genPattern,
        genRecordPatternWith,
        genPatternTypeSigWith,
        genUnboxedSumPatternWith,
        PView <$> resize 2 genExpr <*> genPattern,
        PAs <$> genVarId <*> genPattern,
        PStrict <$> genPattern,
        PIrrefutable <$> genPattern
      ]

genPatternConWith :: Gen Pattern
genPatternConWith = do
  con <- genConName
  argCount <- chooseInt (0, 3)
  args <- vectorOf argCount genPattern
  pure (PCon con [] args)

genPatternTypeSigWith :: Gen Pattern
genPatternTypeSigWith = do
  -- TODO: Remove the PNegLit wrapping once the pretty-printer correctly
  -- parenthesizes PNegLit inside PTypeSig. Currently, PTypeSig (PNegLit 66) T
  -- prints as (-66 :: T) which the parser interprets as negation applied to
  -- (66 :: T) rather than a type signature on -66.
  inner <- wrapNegLit <$> genPattern
  PParen . PTypeSig inner <$> genPatternType
  where
    -- FIXME: This is a hack to get the pretty-printer to correctly parenthesize PNegLit inside PTypeSig. Remove!
    wrapNegLit p@(PNegLit {}) = PParen p
    wrapNegLit p = p

-- | Generate a simple type for use in pattern type signatures.
genPatternType :: Gen Type
genPatternType =
  oneof
    [ TVar . mkUnqualifiedName NameVarId <$> genVarId,
      (`TCon` Unpromoted) <$> genConName
    ]

genPatternInfixWith :: Gen Pattern
genPatternInfixWith = do
  lhs <- genPattern
  op <- genConName
  PInfix lhs op <$> genPattern

genTupleElemsWith :: Gen [Pattern]
genTupleElemsWith = do
  isUnit <- arbitrary
  if isUnit
    then pure []
    else do
      n <- chooseInt (2, 4)
      vectorOf n genPattern

-- | Generate elements for an unboxed tuple pattern (0-4 elements).
-- Unlike boxed tuples, unboxed tuples with 0 elements are valid Haskell.
genUnboxedTupleElemsWith :: Gen [Pattern]
genUnboxedTupleElemsWith = do
  n <- chooseInt (0, 4)
  vectorOf n genPattern

genUnboxedSumPatternWith :: Gen Pattern
genUnboxedSumPatternWith = do
  arity <- chooseInt (2, 4)
  altIdx <- chooseInt (0, arity - 1)
  PUnboxedSum altIdx arity <$> genPattern

genListElemsWith :: Gen [Pattern]
genListElemsWith = do
  n <- chooseInt (0, 4)
  vectorOf n genPattern

genRecordPatternWith :: Gen Pattern
genRecordPatternWith = do
  con <- genConName
  fields <- genRecordFieldsWith
  pure (PRecord con fields False)

genRecordFieldsWith :: Gen [(Name, Pattern)]
genRecordFieldsWith = do
  n <- chooseInt (0, 3)
  names <- vectorOf n genFieldName
  pats <- vectorOf n genPattern
  quals <- vectorOf n genOptionalQualifier
  let qualifiedNames = zipWith (\q name -> qualifyName q (mkUnqualifiedName NameVarId name)) quals names
  pure (zip qualifiedNames pats)

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

-- | Generate the body of a TH pattern splice: either a bare variable or a parenthesized expression.
genPatSpliceBody :: Gen Expr
genPatSpliceBody =
  oneof
    [ EVar <$> genVarName,
      EParen . EVar <$> genVarName
    ]

genPatternUnqualVarName :: Gen UnqualifiedName
genPatternUnqualVarName = mkUnqualifiedName NameVarId <$> genVarId

mkIntLiteral :: Integer -> Literal
mkIntLiteral value = LitInt value TInteger (T.pack (show value))

mkHexLiteral :: Integer -> Literal
mkHexLiteral value = LitInt value TInteger ("0x" <> T.pack (showHex value))

mkFloatLiteral :: Rational -> Literal
mkFloatLiteral value = LitFloat value TFractional (renderFloat value)

mkCharLiteral :: Char -> Literal
mkCharLiteral value = LitChar value (T.pack (show value))

mkStringLiteral :: Text -> Literal
mkStringLiteral value = LitString value (T.pack (show (T.unpack value)))

renderFloat :: Rational -> T.Text
renderFloat value = T.pack (show (fromRational value :: Double))

shrinkPattern :: Pattern -> [Pattern]
shrinkPattern pat = filter (/= pat) (shrinkPatternCandidates pat)

shrinkPatternCandidates :: Pattern -> [Pattern]
shrinkPatternCandidates pat =
  case pat of
    PAnn _ sub -> shrinkPattern sub
    PVar name ->
      [PVar (name {unqualifiedNameText = shrunk}) | shrunk <- shrinkIdent (unqualifiedNameText name)]
    PTypeBinder binder -> [PTypeBinder binder' | binder' <- shrinkTyVarBinder binder]
    PTypeSyntax form ty -> [PTypeSyntax form ty' | ty' <- shrinkType ty]
    PWildcard -> []
    PLit lit ->
      [PLit shrunk | shrunk <- shrinkLiteral lit]
    PQuasiQuote quoter body ->
      [PQuasiQuote q body | q <- shrinkQuoterName quoter]
        <> [PQuasiQuote quoter b | b <- map T.pack (shrink (T.unpack body))]
    PTuple tupleFlavor elems ->
      shrinkPatternTupleElems tupleFlavor elems
    PList elems ->
      [PList elems' | elems' <- shrinkList shrinkPattern elems]
    PCon con typeArgs args ->
      [PCon con' typeArgs args | con' <- shrinkName con]
        <> [PCon con typeArgs [] | not (null args)]
        <> [PCon con typeArgs args' | args' <- shrinkList shrinkPattern args]
    PInfix lhs op rhs ->
      [lhs, rhs]
        <> [PInfix lhs' op rhs | lhs' <- shrinkPattern lhs]
        <> [PInfix lhs op rhs' | rhs' <- shrinkPattern rhs]
    PView expr inner ->
      [inner]
        <> [PView expr' inner | expr' <- shrinkExpr expr]
        <> [PView expr inner' | inner' <- shrinkPattern inner]
    PAs name inner ->
      [inner]
        <> [PAs name' inner | name' <- shrinkIdent name]
        <> [PAs name inner' | inner' <- shrinkPattern inner]
    PStrict inner ->
      [inner]
        <> [PStrict inner' | inner' <- shrinkPattern inner]
    PIrrefutable inner ->
      [inner]
        <> [PIrrefutable inner' | inner' <- shrinkPattern inner]
    PNegLit lit ->
      [PLit lit]
        <> [PNegLit shrunk | shrunk <- shrinkNumericLiteral lit]
    PParen inner ->
      [inner] <> [PParen inner' | inner' <- shrinkPattern inner]
    PUnboxedSum altIdx arity inner ->
      [PUnboxedSum altIdx arity inner' | inner' <- shrinkPattern inner]
    PRecord con fields _ ->
      [PRecord con' fields False | con' <- shrinkName con]
        <> [PRecord con [] False | not (null fields)]
        <> [PRecord con fields' False | fields' <- shrinkList shrinkField fields]
    PTypeSig inner ty ->
      [inner]
        <> [PTypeSig inner' ty | inner' <- shrinkPattern inner]
    PSplice {} ->
      []

shrinkTyVarBinder :: TyVarBinder -> [TyVarBinder]
shrinkTyVarBinder tvb =
  [tvb {tyVarBinderName = name'} | name' <- shrinkIdent (tyVarBinderName tvb)]

shrinkPatternTupleElems :: TupleFlavor -> [Pattern] -> [Pattern]
shrinkPatternTupleElems tupleFlavor elems =
  -- For a unit boxed tuple (), try a simple variable as a simpler outer pattern
  ( case (tupleFlavor, elems) of
      (Boxed, []) -> [PVar (mkUnqualifiedName NameVarId "x")]
      _ -> []
  )
    -- For a single-element unboxed tuple, try extracting the element directly
    <> ( case (tupleFlavor, elems) of
           (Unboxed, [e]) -> [e]
           _ -> []
       )
    <> [ candidate
       | shrunk <- shrinkList shrinkPattern elems,
         candidate <- case shrunk of
           [] -> [PTuple tupleFlavor []]
           [_] -> [PTuple tupleFlavor shrunk | tupleFlavor == Unboxed]
           _ -> [PTuple tupleFlavor shrunk]
       ]

shrinkField :: (Name, Pattern) -> [(Name, Pattern)]
shrinkField (fieldName, fieldPat) =
  [(fieldName', fieldPat) | fieldName' <- shrinkName fieldName]
    <> [(fieldName, shrunk) | shrunk <- shrinkPattern fieldPat]

shrinkLiteral :: Literal -> [Literal]
shrinkLiteral lit =
  case peelLiteralAnn lit of
    LitInt value _ _ -> [mkIntLiteral shrunk | shrunk <- shrinkIntegral value]
    LitFloat value _ _ -> [mkFloatLiteral shrunk | shrunk <- shrinkFloat value, shrunk >= 0]
    LitChar c _ -> [mkCharLiteral shrunk | shrunk <- shrink c]
    LitCharHash c _ -> [LitCharHash shrunk (T.pack (show shrunk) <> "#") | shrunk <- shrink c]
    LitString txt _ -> [mkStringLiteral (T.pack shrunk) | shrunk <- shrink (T.unpack txt)]
    LitStringHash txt _ -> [LitStringHash (T.pack shrunk) (T.pack (show shrunk) <> "#") | shrunk <- shrink (T.unpack txt)]
    LitAnn {} -> error "unreachable"

shrinkNumericLiteral :: Literal -> [Literal]
shrinkNumericLiteral lit =
  case lit of
    LitInt {} -> shrinkLiteral lit
    LitFloat {} -> shrinkLiteral lit
    _ -> []

shrinkQuoterName :: Text -> [Text]
shrinkQuoterName name =
  [ candidate
  | candidate <- map T.pack (shrink (T.unpack name)),
    isValidQuoterName candidate
  ]
