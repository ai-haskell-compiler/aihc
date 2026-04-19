{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Properties.Arb.Pattern
  ( genPattern,
    shrinkPattern,
    canonicalPatternAtom,
  )
where

import Aihc.Parser.Syntax
import Data.Text (Text)
import Data.Text qualified as T
import {-# SOURCE #-} Test.Properties.Arb.Expr (genExpr, shrinkExpr)
import Test.Properties.Arb.Identifiers
  ( genCharValue,
    genConIdent,
    genConSym,
    genFieldName,
    genIdent,
    genOptionalQualifier,
    genQuasiBody,
    genQuoterName,
    genStringValue,
    genTenths,
    isValidQuoterName,
    showHex,
    shrinkFloat,
    shrinkIdent,
  )
import Test.Properties.Arb.Type (shrinkType)
import Test.QuickCheck

instance Arbitrary Pattern where
  arbitrary = sized (genPattern . min 3)
  shrink = shrinkPattern

genPattern :: Int -> Gen Pattern
genPattern depth =
  oneof (leafGenerators <> recursiveGenerators)
  where
    allowRecursive = depth > 0
    nextDepth = depth - 1
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
        (\name -> PCon name [] []) <$> genPatternConAstName,
        genUnboxedSumPatternWith 0
      ]
    recursiveGenerators =
      [ PTuple Boxed <$> genTupleElemsWith nextDepth
      | allowRecursive
      ]
        <> [PTuple Unboxed <$> genUnboxedTupleElemsWith nextDepth | allowRecursive]
        <> [PList <$> genListElemsWith nextDepth | allowRecursive]
        <> [genPatternConWith depth | allowRecursive]
        <> [genPatternInfixWith depth | allowRecursive]
        <> [PParen <$> genPattern nextDepth | allowRecursive]
        <> [genRecordPatternWith nextDepth | allowRecursive]
        <> [genPatternTypeSigWith depth | allowRecursive]
        <> [genUnboxedSumPatternWith nextDepth | allowRecursive]
        <> [PView <$> resize 2 genExpr <*> genPattern nextDepth | allowRecursive]
        <> [PAs <$> genIdent <*> (canonicalPatternAtom <$> genPattern nextDepth) | allowRecursive]
        <> [PStrict . canonicalPatternAtom <$> genPattern nextDepth | allowRecursive]
        <> [PIrrefutable . canonicalPatternAtom <$> genPattern nextDepth | allowRecursive]

genPatternConWith :: Int -> Gen Pattern
genPatternConWith depth = do
  con <- genPatternConAstName
  argCount <- chooseInt (0, 3)
  args <- vectorOf argCount (canonicalPatternAtom <$> genPattern (depth - 1))
  pure (PCon con [] args)

genPatternTypeSigWith :: Int -> Gen Pattern
genPatternTypeSigWith depth = do
  -- TODO: Remove the PNegLit wrapping once the pretty-printer correctly
  -- parenthesizes PNegLit inside PTypeSig. Currently, PTypeSig (PNegLit 66) T
  -- prints as (-66 :: T) which the parser interprets as negation applied to
  -- (66 :: T) rather than a type signature on -66.
  inner <- wrapNegLit <$> genPattern (depth - 1)
  PParen . PTypeSig inner <$> genPatternType
  where
    -- FIXME: This is a hack to get the pretty-printer to correctly parenthesize PNegLit inside PTypeSig. Remove!
    wrapNegLit p@(PNegLit {}) = PParen p
    wrapNegLit p = p

-- | Generate a simple type for use in pattern type signatures.
genPatternType :: Gen Type
genPatternType =
  oneof
    [ TVar . mkUnqualifiedName NameVarId <$> genIdent,
      (`TCon` Unpromoted) <$> genPatternConAstName
    ]

genPatternInfixWith :: Int -> Gen Pattern
genPatternInfixWith depth = do
  -- TODO: Switch back to canonicalPatternAtom once the pretty-printer correctly
  -- parenthesizes PNegLit as an infix operand. Currently, PInfix (PNegLit 433)
  -- ":+" (PVar "y") prints as (-433 :+ y) which is misparsed as negation of
  -- (433 :+ y).
  lhs <- canonicalPatternAtom <$> genPattern (depth - 1)
  op <- genConOperatorName
  rhs <- canonicalPatternAtom <$> genPattern (depth - 1)
  pure (PInfix lhs op rhs)

genTupleElemsWith :: Int -> Gen [Pattern]
genTupleElemsWith depth = do
  isUnit <- arbitrary
  if isUnit
    then pure []
    else do
      n <- chooseInt (2, 4)
      vectorOf n (genPattern depth)

-- | Generate elements for an unboxed tuple pattern (0-4 elements).
-- Unlike boxed tuples, unboxed tuples with 0 elements are valid Haskell.
genUnboxedTupleElemsWith :: Int -> Gen [Pattern]
genUnboxedTupleElemsWith depth = do
  n <- chooseInt (0, 4)
  vectorOf n (genPattern depth)

genUnboxedSumPatternWith :: Int -> Gen Pattern
genUnboxedSumPatternWith depth = do
  arity <- chooseInt (2, 4)
  altIdx <- chooseInt (0, arity - 1)
  inner <- genPattern depth
  pure (PUnboxedSum altIdx arity inner)

genListElemsWith :: Int -> Gen [Pattern]
genListElemsWith depth = do
  n <- chooseInt (0, 4)
  vectorOf n (genPattern depth)

genRecordPatternWith :: Int -> Gen Pattern
genRecordPatternWith depth = do
  con <- genPatternConAstName
  fields <- genRecordFieldsWith depth
  pure (PRecord con fields False)

genRecordFieldsWith :: Int -> Gen [(Name, Pattern)]
genRecordFieldsWith depth = do
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

-- | Generate the body of a TH pattern splice: either a bare variable or a parenthesized expression.
genPatSpliceBody :: Gen Expr
genPatSpliceBody =
  oneof
    [ EVar <$> genPatternVarName,
      EParen . EVar <$> genPatternVarName
    ]

genConOperatorName :: Gen Name
genConOperatorName = qualifyName <$> genOptionalQualifier <*> (mkUnqualifiedName NameConSym <$> genConSym)

genPatternVarName :: Gen Name
genPatternVarName = qualifyName Nothing . mkUnqualifiedName NameVarId <$> genIdent

genPatternUnqualVarName :: Gen UnqualifiedName
genPatternUnqualVarName = mkUnqualifiedName NameVarId <$> genIdent

genPatternConAstName :: Gen Name
genPatternConAstName = qualifyName <$> genOptionalQualifier <*> (mkUnqualifiedName NameConId <$> genConIdent)

-- FIXME: This should be removed.
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
    PCon _ [] [] -> True
    _ -> False

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
shrinkPattern pat =
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
      [PCon con typeArgs [] | not (null args)]
        <> [PCon con typeArgs args' | args' <- shrinkList (map canonicalPatternAtom . shrinkPattern) args]
    PInfix lhs op rhs ->
      [canonicalPatternAtom lhs, canonicalPatternAtom rhs]
        <> [PInfix (canonicalPatternAtom lhs') op (canonicalPatternAtom rhs) | lhs' <- shrinkPattern lhs]
        <> [PInfix (canonicalPatternAtom lhs) op (canonicalPatternAtom rhs') | rhs' <- shrinkPattern rhs]
    PView expr inner ->
      [inner]
        <> [PView expr' inner | expr' <- shrinkExpr expr]
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

shrinkTyVarBinder :: TyVarBinder -> [TyVarBinder]
shrinkTyVarBinder tvb =
  [tvb {tyVarBinderName = name'} | name' <- shrinkIdent (tyVarBinderName tvb)]

shrinkPatternTupleElems :: TupleFlavor -> [Pattern] -> [Pattern]
shrinkPatternTupleElems tupleFlavor elems =
  [ candidate
  | shrunk <- shrinkList shrinkPattern elems,
    candidate <- case shrunk of
      [] -> [PTuple tupleFlavor []]
      [_] -> [PTuple tupleFlavor shrunk | tupleFlavor == Unboxed]
      _ -> [PTuple tupleFlavor shrunk]
  ]

shrinkField :: (Name, Pattern) -> [(Name, Pattern)]
shrinkField (fieldName, fieldPat) =
  [(fieldName, shrunk) | shrunk <- shrinkPattern fieldPat]

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
