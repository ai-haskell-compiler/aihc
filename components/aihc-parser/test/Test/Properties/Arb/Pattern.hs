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
    span0,
  )
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
    pat0 = patternAnnSpan span0
    leafGenerators =
      [ pat0 . PVar <$> genPatternUnqualVarName,
        pure (pat0 PWildcard),
        pat0 . PLit <$> genLiteral,
        (\q b -> pat0 (PQuasiQuote q b)) <$> genQuoterName <*> genQuasiBody,
        pat0 . PNegLit <$> genNumericLiteral,
        pat0 . PSplice <$> genPatSpliceBody,
        pat0 . PTuple Boxed <$> elements [[], [pat0 (PVar (mkUnqualifiedName NameVarId "x")), pat0 PWildcard]],
        pat0 . PTuple Unboxed <$> elements [[], [pat0 (PVar (mkUnqualifiedName NameVarId "x"))], [pat0 (PVar (mkUnqualifiedName NameVarId "x")), pat0 PWildcard]],
        pure (pat0 (PList [])),
        (\name -> pat0 (PCon name [])) <$> genPatternConAstName,
        genUnboxedSumPatternWith allowAll 0
      ]
    recursiveGenerators =
      [ pat0 . PTuple Boxed <$> genTupleElemsWith allowAll nextDepth
      | allowRecursive
      ]
        <> [pat0 . PTuple Unboxed <$> genUnboxedTupleElemsWith allowAll nextDepth | allowRecursive]
        <> [pat0 . PList <$> genListElemsWith allowAll nextDepth | allowRecursive]
        <> [genPatternConWith allowAll depth | allowRecursive]
        <> [genPatternInfixWith allowAll depth | allowRecursive]
        <> [pat0 . PParen <$> genPatternWith allowAll nextDepth | allowRecursive]
        <> [(\name fields -> pat0 (PRecord name fields False)) <$> genPatternConAstName <*> genRecordFieldsWith allowAll nextDepth | allowRecursive]
        <> [genPatternTypeSigWith allowAll depth | allowRecursive]
        <> [genUnboxedSumPatternWith allowAll nextDepth | allowRecursive]
        <> [(\e p -> pat0 (PView e p)) <$> resize 2 genExpr <*> genPatternWith allowAll nextDepth | allowRecursive, allowAll]
        <> [(\name p -> pat0 (PAs name p)) <$> genIdent <*> (canonicalPatternAtom <$> genPatternWith allowAll nextDepth) | allowRecursive, allowAll]
        <> [pat0 . PStrict . canonicalPatternAtom <$> genPatternWith allowAll nextDepth | allowRecursive, allowAll]
        <> [pat0 . PIrrefutable . canonicalPatternAtom <$> genPatternWith allowAll nextDepth | allowRecursive, allowAll]

genPatternConWith :: Bool -> Int -> Gen Pattern
genPatternConWith allowView depth = do
  con <- genPatternConAstName
  argCount <- chooseInt (0, 3)
  args <- vectorOf argCount (canonicalPatternAtom <$> genPatternWith allowView (depth - 1))
  pure (patternAnnSpan span0 (PCon con args))

genPatternTypeSigWith :: Bool -> Int -> Gen Pattern
genPatternTypeSigWith allowAll depth = do
  -- TODO: Remove the PNegLit wrapping once the pretty-printer correctly
  -- parenthesizes PNegLit inside PTypeSig. Currently, PTypeSig (PNegLit 66) T
  -- prints as (-66 :: T) which the parser interprets as negation applied to
  -- (66 :: T) rather than a type signature on -66.
  inner <- wrapNegLit <$> genPatternWith allowAll (depth - 1)
  patternAnnSpan span0 . PParen . patternAnnSpan span0 . PTypeSig inner <$> genPatternType
  where
    wrapNegLit p@(PNegLit {}) = patternAnnSpan span0 (PParen p)
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
  lhs <- canonicalPatternAtom <$> genPatternWith allowAll (depth - 1)
  op <- genConOperatorName
  rhs <- canonicalPatternAtom <$> genPatternWith allowAll (depth - 1)
  pure (patternAnnSpan span0 (PInfix lhs op rhs))

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
  pure (patternAnnSpan span0 (PUnboxedSum altIdx arity inner))

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

-- | Generate the body of a TH pattern splice: either a bare variable or a parenthesized expression.
genPatSpliceBody :: Gen Expr
genPatSpliceBody =
  oneof
    [ exprAnnSpan span0 . EVar <$> genPatternVarName,
      exprAnnSpan span0 . EParen . exprAnnSpan span0 . EVar <$> genPatternVarName
    ]

genConOperatorName :: Gen Name
genConOperatorName = qualifyName <$> genOptionalQualifier <*> (mkUnqualifiedName NameConSym <$> genConSym)

genPatternVarName :: Gen Name
genPatternVarName = qualifyName Nothing . mkUnqualifiedName NameVarId <$> genIdent

genPatternUnqualVarName :: Gen UnqualifiedName
genPatternUnqualVarName = mkUnqualifiedName NameVarId <$> genIdent

genPatternConAstName :: Gen Name
genPatternConAstName = qualifyName <$> genOptionalQualifier <*> (mkUnqualifiedName NameConId <$> genConIdent)

canonicalPatternAtom :: Pattern -> Pattern
canonicalPatternAtom pat =
  if isPatternAtom pat
    then pat
    else patternAnnSpan span0 (PParen pat)

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
    PCon _ [] -> True
    _ -> False

mkIntLiteral :: Integer -> Literal
mkIntLiteral value = literalAnnSpan span0 (LitInt value (T.pack (show value)))

mkHexLiteral :: Integer -> Literal
mkHexLiteral value = literalAnnSpan span0 (LitIntBase value ("0x" <> T.pack (showHex value)))

mkFloatLiteral :: Double -> Literal
mkFloatLiteral value = literalAnnSpan span0 (LitFloat value (T.pack (show value)))

mkCharLiteral :: Char -> Literal
mkCharLiteral value = literalAnnSpan span0 (LitChar value (T.pack (show value)))

mkStringLiteral :: Text -> Literal
mkStringLiteral value = literalAnnSpan span0 (LitString value (T.pack (show (T.unpack value))))

shrinkPattern :: Pattern -> [Pattern]
shrinkPattern pat =
  case pat of
    PAnn _ sub -> shrinkPattern sub
    PVar name ->
      [patternAnnSpan span0 (PVar (name {unqualifiedNameText = shrunk})) | shrunk <- shrinkIdent (unqualifiedNameText name)]
    PWildcard -> []
    PLit lit ->
      [patternAnnSpan span0 (PLit shrunk) | shrunk <- shrinkLiteral lit]
    PQuasiQuote quoter body ->
      [patternAnnSpan span0 (PQuasiQuote q body) | q <- shrinkQuoterName quoter]
        <> [patternAnnSpan span0 (PQuasiQuote quoter b) | b <- map T.pack (shrink (T.unpack body))]
    PTuple tupleFlavor elems ->
      shrinkPatternTupleElems tupleFlavor elems
    PList elems ->
      [patternAnnSpan span0 (PList elems') | elems' <- shrinkList shrinkPattern elems]
    PCon con args ->
      [patternAnnSpan span0 (PCon con []) | not (null args)]
        <> [patternAnnSpan span0 (PCon con args') | args' <- shrinkList (map canonicalPatternAtom . shrinkPattern) args]
    PInfix lhs op rhs ->
      [canonicalPatternAtom lhs, canonicalPatternAtom rhs]
        <> [patternAnnSpan span0 (PInfix (canonicalPatternAtom lhs') op (canonicalPatternAtom rhs)) | lhs' <- shrinkPattern lhs]
        <> [patternAnnSpan span0 (PInfix (canonicalPatternAtom lhs) op (canonicalPatternAtom rhs')) | rhs' <- shrinkPattern rhs]
    PView expr inner ->
      [inner]
        <> [patternAnnSpan span0 (PView expr' inner) | expr' <- shrinkExpr expr]
        <> [patternAnnSpan span0 (PView expr inner') | inner' <- shrinkPattern inner]
    PAs name inner ->
      [canonicalPatternAtom inner]
        <> [patternAnnSpan span0 (PAs name' (canonicalPatternAtom inner)) | name' <- shrinkIdent name]
        <> [patternAnnSpan span0 (PAs name (canonicalPatternAtom inner')) | inner' <- shrinkPattern inner]
    PStrict inner ->
      [canonicalPatternAtom inner]
        <> [patternAnnSpan span0 (PStrict (canonicalPatternAtom inner')) | inner' <- shrinkPattern inner]
    PIrrefutable inner ->
      [canonicalPatternAtom inner]
        <> [patternAnnSpan span0 (PIrrefutable (canonicalPatternAtom inner')) | inner' <- shrinkPattern inner]
    PNegLit lit ->
      [patternAnnSpan span0 (PLit lit)]
        <> [patternAnnSpan span0 (PNegLit shrunk) | shrunk <- shrinkNumericLiteral lit]
    PParen inner ->
      [inner] <> [patternAnnSpan span0 (PParen inner') | inner' <- shrinkPattern inner]
    PUnboxedSum altIdx arity inner ->
      [patternAnnSpan span0 (PUnboxedSum altIdx arity inner') | inner' <- shrinkPattern inner]
    PRecord con fields _ ->
      [patternAnnSpan span0 (PRecord con [] False) | not (null fields)]
        <> [patternAnnSpan span0 (PRecord con fields' False) | fields' <- shrinkList shrinkField fields]
    PTypeSig inner ty ->
      [inner]
        <> [patternAnnSpan span0 (PTypeSig inner' ty) | inner' <- shrinkPattern inner]
    PSplice {} ->
      []

shrinkPatternTupleElems :: TupleFlavor -> [Pattern] -> [Pattern]
shrinkPatternTupleElems tupleFlavor elems =
  [ candidate
  | shrunk <- shrinkList shrinkPattern elems,
    candidate <- case shrunk of
      [] -> [patternAnnSpan span0 (PTuple tupleFlavor [])]
      [_] -> [patternAnnSpan span0 (PTuple tupleFlavor shrunk) | tupleFlavor == Unboxed]
      _ -> [patternAnnSpan span0 (PTuple tupleFlavor shrunk)]
  ]

shrinkField :: (Name, Pattern) -> [(Name, Pattern)]
shrinkField (fieldName, fieldPat) =
  [(fieldName, shrunk) | shrunk <- shrinkPattern fieldPat]

shrinkLiteral :: Literal -> [Literal]
shrinkLiteral lit =
  case peelLiteralAnn lit of
    LitInt value _ -> [mkIntLiteral shrunk | shrunk <- shrinkIntegral value]
    LitIntHash value _ -> [literalAnnSpan span0 (LitIntHash shrunk (T.pack (show shrunk) <> "#")) | shrunk <- shrinkIntegral value]
    LitIntBase value _ -> [mkHexLiteral shrunk | shrunk <- shrinkIntegral value, shrunk >= 0]
    LitIntBaseHash value _ -> [literalAnnSpan span0 (LitIntBaseHash shrunk ("0x" <> T.pack (showHex shrunk) <> "#")) | shrunk <- shrinkIntegral value, shrunk >= 0]
    LitFloat value _ -> [mkFloatLiteral shrunk | shrunk <- shrinkFloat value, shrunk >= 0]
    LitFloatHash value _ -> [literalAnnSpan span0 (LitFloatHash shrunk (T.pack (show shrunk) <> "#")) | shrunk <- shrinkFloat value, shrunk >= 0]
    LitChar c _ -> [mkCharLiteral shrunk | shrunk <- shrink c]
    LitCharHash c _ -> [literalAnnSpan span0 (LitCharHash shrunk (T.pack (show shrunk) <> "#")) | shrunk <- shrink c]
    LitString txt _ -> [mkStringLiteral (T.pack shrunk) | shrunk <- shrink (T.unpack txt)]
    LitStringHash txt _ -> [literalAnnSpan span0 (LitStringHash (T.pack shrunk) (T.pack (show shrunk) <> "#")) | shrunk <- shrink (T.unpack txt)]
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
