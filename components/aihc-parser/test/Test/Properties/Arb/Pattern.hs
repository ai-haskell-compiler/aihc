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
genPattern = genPatternWith True

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
        genUnboxedSumPatternWith allowAll 0
      ]
    recursiveGenerators =
      [ PTuple Boxed <$> genTupleElemsWith allowAll nextDepth
      | allowRecursive
      ]
        <> [PTuple Unboxed <$> genUnboxedTupleElemsWith allowAll nextDepth | allowRecursive]
        <> [PList <$> genListElemsWith allowAll nextDepth | allowRecursive]
        <> [genPatternConWith allowAll depth | allowRecursive]
        <> [genPatternInfixWith allowAll depth | allowRecursive]
        <> [PParen <$> genPatternWith allowAll nextDepth | allowRecursive]
        <> [genRecordPatternWith allowAll nextDepth | allowRecursive]
        <> [genPatternTypeSigWith allowAll depth | allowRecursive]
        <> [genUnboxedSumPatternWith allowAll nextDepth | allowRecursive]
        <> [PView <$> resize 2 genExpr <*> genPatternWith allowAll nextDepth | allowRecursive, allowAll]
        <> [PAs <$> genIdent <*> (canonicalPatternAtom <$> genPatternWith allowAll nextDepth) | allowRecursive, allowAll]
        <> [PStrict . canonicalPatternAtom <$> genPatternWith allowAll nextDepth | allowRecursive, allowAll]
        <> [PIrrefutable . canonicalPatternAtom <$> genPatternWith allowAll nextDepth | allowRecursive, allowAll]

genPatternConWith :: Bool -> Int -> Gen Pattern
genPatternConWith allowView depth = do
  con <- genPatternConAstName
  argCount <- chooseInt (0, 3)
  args <- vectorOf argCount (canonicalPatternAtom <$> genPatternWith allowView (depth - 1))
  pure (PCon con [] args)

genPatternTypeSigWith :: Bool -> Int -> Gen Pattern
genPatternTypeSigWith allowAll depth = do
  -- TODO: Remove the PNegLit wrapping once the pretty-printer correctly
  -- parenthesizes PNegLit inside PTypeSig. Currently, PTypeSig (PNegLit 66) T
  -- prints as (-66 :: T) which the parser interprets as negation applied to
  -- (66 :: T) rather than a type signature on -66.
  inner <- wrapNegLit <$> genPatternWith allowAll (depth - 1)
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

genPatternInfixWith :: Bool -> Int -> Gen Pattern
genPatternInfixWith allowAll depth = do
  -- TODO: Switch back to canonicalPatternAtom once the pretty-printer correctly
  -- parenthesizes PNegLit as an infix operand. Currently, PInfix (PNegLit 433)
  -- ":+" (PVar "y") prints as (-433 :+ y) which is misparsed as negation of
  -- (433 :+ y).
  lhs <- canonicalPatternAtom <$> genPatternWith allowAll (depth - 1)
  op <- genConOperatorName
  rhs <- canonicalPatternAtom <$> genPatternWith allowAll (depth - 1)
  pure (PInfix lhs op rhs)

genTupleElemsWith :: Bool -> Int -> Gen [Pattern]
genTupleElemsWith allowView depth = do
  isUnit <- arbitrary
  if isUnit
    then pure []
    else do
      n <- chooseInt (2, 4)
      vectorOf n (genPatternWith allowView depth)

-- | Generate elements for an unboxed tuple pattern (0-4 elements).
-- Unlike boxed tuples, unboxed tuples with 0 elements are valid Haskell.
genUnboxedTupleElemsWith :: Bool -> Int -> Gen [Pattern]
genUnboxedTupleElemsWith allowView depth = do
  n <- chooseInt (0, 4)
  vectorOf n (genPatternWith allowView depth)

genUnboxedSumPatternWith :: Bool -> Int -> Gen Pattern
genUnboxedSumPatternWith allowView depth = do
  arity <- chooseInt (2, 4)
  altIdx <- chooseInt (0, arity - 1)
  inner <- genPatternWith allowView depth
  pure (PUnboxedSum altIdx arity inner)

genListElemsWith :: Bool -> Int -> Gen [Pattern]
genListElemsWith allowView depth = do
  n <- chooseInt (0, 4)
  vectorOf n (genPatternWith allowView depth)

genRecordPatternWith :: Bool -> Int -> Gen Pattern
genRecordPatternWith allowAll depth = do
  con <- genPatternConAstName
  fields <- genRecordFieldsWith allowAll depth
  pure (PRecord con fields False)

genRecordFieldsWith :: Bool -> Int -> Gen [(Name, Pattern)]
genRecordFieldsWith allowView depth = do
  n <- chooseInt (0, 3)
  names <- vectorOf n genFieldName
  pats <- vectorOf n (genRecordFieldPatternWith allowView depth)
  pure (zip (map (qualifyName Nothing . mkUnqualifiedName NameVarId) names) pats)

genRecordFieldPatternWith :: Bool -> Int -> Gen Pattern
genRecordFieldPatternWith allowAll depth
  | allowAll && depth > 0 =
      frequency
        [ (3, genPatternWith allowAll depth),
          (1, PView <$> resize 2 genExpr <*> genPatternWith allowAll (depth - 1))
        ]
  | otherwise = genPatternWith allowAll depth

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
mkIntLiteral value = LitInt value (T.pack (show value))

mkHexLiteral :: Integer -> Literal
mkHexLiteral value = LitIntBase value ("0x" <> T.pack (showHex value))

mkFloatLiteral :: Double -> Literal
mkFloatLiteral value = LitFloat value (T.pack (show value))

mkCharLiteral :: Char -> Literal
mkCharLiteral value = LitChar value (T.pack (show value))

mkStringLiteral :: Text -> Literal
mkStringLiteral value = LitString value (T.pack (show (T.unpack value)))

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
    LitAnn {} -> error "unreachable"

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
