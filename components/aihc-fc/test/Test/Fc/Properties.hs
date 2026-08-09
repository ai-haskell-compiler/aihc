{-# LANGUAGE OverloadedStrings #-}

module Test.Fc.Properties
  ( fcPropertyTests,
  )
where

import Aihc.Fc
import Aihc.Tc.Evidence (Coercion (..), EvVar (..))
import Aihc.Tc.Types
  ( Kind (..),
    Levity (..),
    Pred (..),
    RuntimeRep (..),
    TcType (..),
    TyCon (..),
    TyVarId (..),
    Unique (..),
    VecCount (..),
    VecElem (..),
    liftedRuntimeRep,
    setTyConKind,
    setTyVarKind,
  )
import Control.Monad (when)
import Data.ByteString qualified as BS
import Data.List (nubBy)
import Data.Text (Text)
import Data.Text qualified as T
import Hedgehog (Gen, Property, PropertyT, annotate, failure, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

fcPropertyTests :: TestTree
fcPropertyTests =
  testGroup
    "System FC properties"
    [ testProperty "parseProgram . renderProgram = id" prop_programRoundTrip,
      testProperty "parseExpr . renderExpr = id" prop_exprRoundTrip,
      testProperty "parseType . renderType = id" prop_typeRoundTrip,
      testProperty "dependent runtime representations use their binder" prop_dependentRuntimeRep,
      testProperty "package origins distinguish equal symbol names" prop_packageOrigins,
      testProperty "external signatures occur once" prop_externalSignatures,
      testProperty "local definition signatures serve local occurrences" prop_localSignatures,
      testProperty "undeclared external occurrences are rejected" prop_undeclaredExternal,
      testProperty "duplicate external declarations are rejected" prop_duplicateExternal
    ]

prop_programRoundTrip :: Property
prop_programRoundTrip = property $ do
  value <- forAll genProgram
  roundTrip renderProgram parseProgram value

prop_exprRoundTrip :: Property
prop_exprRoundTrip = property $ do
  value <- forAll genExpr
  roundTrip renderExpr parseExpr value

prop_typeRoundTrip :: Property
prop_typeRoundTrip = property $ do
  value <- forAll genType
  roundTrip renderType parseType value

prop_dependentRuntimeRep :: Property
prop_dependentRuntimeRep = property $ do
  let rep = setTyVarKind KRuntimeRep (TyVarId "rep" (Unique 1))
      value = setTyVarKind (KTYPE (RuntimeRepVar (tvUnique rep))) (TyVarId "value" (Unique 2))
      ty = TcForAllTy rep (TcForAllTy value (TcTyVar value))
      rendered = T.pack (renderType ty)
  annotate (T.unpack rendered)
  rendered === "∀ (rep : RuntimeRep) (value : TYPE rep). value"
  roundTrip renderType parseType ty

prop_packageOrigins :: Property
prop_packageOrigins = property $ do
  let ty = TcTyCon (TyCon "Identity" 0) []
      occurrence packageName =
        FcVar
          ( (Var "id" (Unique 1) ty)
              { varResolvedName = Just (FcTopLevelOrigin packageName "Module" "id")
              }
          )
      renderedA = T.pack (renderExpr (occurrence "pkgA"))
      renderedB = T.pack (renderExpr (occurrence "pkgB"))
      externalA = fcExternalVar (FcTopLevelOrigin "pkgA" "Module" "id") ty
      externalB = fcExternalVar (FcTopLevelOrigin "pkgB" "Module" "id") ty
  annotate (T.unpack renderedA)
  annotate (T.unpack renderedB)
  when (renderedA == renderedB) failure
  (varUnique externalA == varUnique externalB) === False
  case parseExpr renderedA of
    Left parseError -> annotate (show parseError) >> failure
    Right (FcVar var) -> varResolvedName var === Just (FcTopLevelOrigin "pkgA" "Module" "id")
    Right parsed -> annotate (show parsed) >> failure

prop_externalSignatures :: Property
prop_externalSignatures = property $ do
  let ty = TcForAllTy typeVariable (TcFunTy (TcTyVar typeVariable) (TcTyVar typeVariable))
      origin = FcTopLevelOrigin "pkg" "Module" "id"
      imported = (Var "id" (Unique 1) ty) {varResolvedName = Just origin}
      result = Var "result" (Unique 2) ty
      program = FcProgram (FcModuleId "test" "Test") [FcExternal origin ty, FcTopBind (FcNonRec result (FcVar imported))]
      rendered = T.pack (renderProgram program)
  annotate (T.unpack rendered)
  T.count "Module.id :" rendered === 1
  T.count "\"pkg\" Module.id" rendered === 2
  roundTrip renderProgram parseProgram program
  where
    typeVariable = setTyVarKind (KTYPE liftedRuntimeRep) (TyVarId "a" (Unique 3))

prop_undeclaredExternal :: Property
prop_undeclaredExternal = property $ do
  let source = "module \"test\" Test where\n\nresult : Int =\n  \"pkg\" Module.value"
  annotate (T.unpack source)
  case parseProgram source of
    Left _ -> pure ()
    Right parsed -> annotate (show parsed) >> failure

prop_localSignatures :: Property
prop_localSignatures = property $ do
  let boolType = TcTyCon (TyCon "Bool" 0) []
      origin = FcTopLevelOrigin "pkg" "Module" "True"
      constructor = (Var "True" (Unique 1) boolType) {varResolvedName = Just origin}
      result = Var "result" (Unique 2) boolType
      program =
        FcProgram
          (FcModuleId "pkg" "Module")
          [ FcData (FcDataDecl (FcTopLevelOrigin "pkg" "Module" "Bool") "Bool" [] [FcDataConDecl origin "True" []]),
            FcTopBind (FcNonRec result (FcVar constructor))
          ]
      rendered = T.pack (renderProgram program)
  annotate (T.unpack rendered)
  T.count "external" rendered === 0
  T.count "True" rendered === 2
  roundTrip renderProgram parseProgram program

prop_duplicateExternal :: Property
prop_duplicateExternal = property $ do
  let source = "module \"test\" Test where\n\nexternal \"pkg\" Module.value : Int\n\nexternal \"pkg\" Module.value : Int"
  annotate (T.unpack source)
  case parseProgram source of
    Left _ -> pure ()
    Right parsed -> annotate (show parsed) >> failure

roundTrip :: (Eq a, Show a, Show error) => (a -> String) -> (Text -> Either error a) -> a -> PropertyT IO ()
roundTrip pretty parse value = do
  let rendered = T.pack (pretty value)
  annotate (T.unpack rendered)
  case parse rendered of
    Left parseError -> annotate (show parseError) >> failure
    -- Compiler uniques are regenerated from lexical scope. The canonical
    -- semantic syntax itself must be a fixed point.
    Right actual -> pretty actual === pretty value

genProgram :: Gen FcProgram
genProgram = FcProgram (FcModuleId "test" "Test") <$> smallList genTopBind

genTopBind :: Gen FcTopBind
genTopBind =
  Gen.choice
    [ FcData <$> genDataDecl,
      FcAxiom <$> genAxiomDecl,
      FcNewtype <$> genNewtypeDecl,
      FcPrimitive <$> genVar <*> genInt,
      FcForeignImport <$> genForeignCall,
      FcTopBind <$> genBindWith genExpr
    ]

genDataDecl :: Gen FcDataDecl
genDataDecl = do
  dataName <- genTypeName
  FcDataDecl (testOrigin dataName) dataName <$> smallList genTyVar <*> smallList genDataConstructor

genDataConstructor :: Gen FcDataConDecl
genDataConstructor = do
  constructorName <- genTypeName
  FcDataConDecl (testOrigin constructorName) constructorName <$> smallList genType

genAxiomDecl :: Gen FcAxiomDecl
genAxiomDecl =
  FcAxiomDecl
    <$> genTypeName
    <*> smallList genTyVar
    <*> Gen.element [FcNominal, FcRepresentational]
    <*> genType
    <*> genType

genNewtypeDecl :: Gen FcNewtypeDecl
genNewtypeDecl =
  do
    newtypeName <- genTypeName
    constructorName <- genTypeName
    FcNewtypeDecl
      (testOrigin newtypeName)
      newtypeName
      <$> smallList genTyVar
      <*> pure (testOrigin constructorName)
      <*> pure constructorName
      <*> genType
      <*> genType

testOrigin :: Text -> FcSymbolOrigin
testOrigin = FcTopLevelOrigin "test" "Test"

genForeignCall :: Gen FcForeignCall
genForeignCall = FcForeignCall <$> genVarName <*> genLiteralText <*> genForeignSignature

genForeignSignature :: Gen FcForeignSignature
genForeignSignature =
  FcForeignSignature <$> smallList genForeignType <*> genForeignType <*> genForeignEffect

genForeignEffect :: Gen FcForeignEffect
genForeignEffect = Gen.element [FcForeignPure, FcForeignRealWorld]

genForeignType :: Gen FcForeignType
genForeignType = Gen.element [FcForeignInt, FcForeignInt32, FcForeignWord64, FcForeignAddr]

genExpr :: Gen FcExpr
genExpr =
  Gen.recursive
    Gen.choice
    [ FcVar <$> genVar,
      FcLit <$> genLiteral
    ]
    [ FcApp <$> genExpr <*> genExpr,
      FcTyApp <$> genExpr <*> genType,
      FcLam <$> genVar <*> genExpr,
      FcTyLam <$> genTyVar <*> genExpr,
      FcLet <$> genBindWith genExpr <*> genExpr,
      FcCase <$> genExpr <*> genVar <*> smallList (genAltWith genExpr),
      FcCast <$> genExpr <*> genCoercion,
      FcCallForeign <$> genForeignCall <*> smallList genExpr
    ]

genBindWith :: Gen FcExpr -> Gen FcBind
genBindWith child =
  Gen.choice
    [ FcNonRec <$> genVar <*> child,
      FcRec . nubBy sameBinder <$> smallList ((,) <$> genVar <*> child)
    ]
  where
    sameBinder (left, _) (right, _) = varUnique left == varUnique right

genAltWith :: Gen FcExpr -> Gen FcAlt
genAltWith child = FcAlt <$> genAltCon <*> smallList genVar <*> child

genAltCon :: Gen FcAltCon
genAltCon = Gen.choice [DataAlt <$> genTypeName, LitAlt <$> genLiteral, pure DefaultAlt]

genVar :: Gen Var
genVar = do
  identifier <- genUnique
  let name = generatedName "v" identifier
  ty <- genType
  resolvedName <- Gen.maybe genSymbolOrigin
  pure ((Var name identifier ty) {varResolvedName = resolvedName})

genSymbolOrigin :: Gen FcSymbolOrigin
genSymbolOrigin =
  Gen.choice
    [ FcTopLevelOrigin <$> genText <*> genTypeName <*> genVarName,
      FcBuiltinOrigin <$> genVarName
    ]

genLiteral :: Gen Literal
genLiteral =
  Gen.choice
    [ LitInt <$> genRuntimeRep <*> genInteger,
      LitChar <$> genRuntimeRep <*> Gen.unicode,
      LitString <$> genLiteralText,
      LitAddr . BS.pack <$> smallList (Gen.word8 Range.constantBounded)
    ]

genType :: Gen TcType
genType =
  Gen.recursive
    Gen.choice
    [ TcTyVar <$> genTyVar,
      TcMetaTv <$> genUnique,
      TcTyCon <$> genTyCon <*> pure []
    ]
    [ TcTyCon <$> genTyCon <*> smallList genType,
      TcFunTy <$> genType <*> genType,
      TcForAllTy <$> genTyVar <*> genType,
      TcQualTy <$> smallList genPred <*> genType,
      TcAppTy <$> genType <*> genType
    ]

genPred :: Gen Pred
genPred = Gen.choice [ClassPred <$> genTypeName <*> smallList genType, EqPred <$> genType <*> genType]

genTyVar :: Gen TyVarId
genTyVar = do
  identifier <- genUnique
  kind <- genKind
  pure (setTyVarKind kind (TyVarId (generatedName "a" identifier) identifier))

genTyCon :: Gen TyCon
genTyCon = setTyConKind <$> genKind <*> (TyCon <$> genTypeName <*> Gen.int (Range.linear 0 4))

genKind :: Gen Kind
genKind =
  Gen.recursive
    Gen.choice
    [ KTYPE <$> genRuntimeRep,
      pure KConstraint,
      pure KRuntimeRep,
      pure KLevity,
      pure KVecCount,
      pure KVecElem,
      KMeta <$> genUnique
    ]
    [KFun <$> genKind <*> genKind]

genRuntimeRep :: Gen RuntimeRep
genRuntimeRep =
  Gen.recursive
    Gen.choice
    [ VecRep <$> genVecCount <*> genVecElem,
      BoxedRep <$> Gen.element [Lifted, Unlifted],
      pure IntRep,
      pure Int8Rep,
      pure Int16Rep,
      pure Int32Rep,
      pure Int64Rep,
      pure WordRep,
      pure Word8Rep,
      pure Word16Rep,
      pure Word32Rep,
      pure Word64Rep,
      pure AddrRep,
      pure FloatRep,
      pure DoubleRep,
      RuntimeRepVar <$> genUnique,
      RuntimeRepMeta <$> genUnique
    ]
    [ TupleRep <$> smallList genRuntimeRep,
      SumRep <$> smallList genRuntimeRep
    ]

genVecCount :: Gen VecCount
genVecCount = Gen.element [Vec2, Vec4, Vec8, Vec16, Vec32, Vec64]

genVecElem :: Gen VecElem
genVecElem =
  Gen.element
    [ Int8ElemRep,
      Int16ElemRep,
      Int32ElemRep,
      Int64ElemRep,
      Word8ElemRep,
      Word16ElemRep,
      Word32ElemRep,
      Word64ElemRep,
      FloatElemRep,
      DoubleElemRep
    ]

genCoercion :: Gen Coercion
genCoercion =
  Gen.recursive
    Gen.choice
    [ CoVar . EvVar <$> genUnique,
      Refl <$> genType,
      AxiomInstCo <$> genTypeName <*> smallList genType
    ]
    [ Sym <$> genCoercion,
      Trans <$> genCoercion <*> genCoercion,
      TyConAppCo <$> genTyCon <*> smallList genCoercion
    ]

genUnique :: Gen Unique
genUnique = Unique <$> genInt

generatedName :: Text -> Unique -> Text
generatedName prefix (Unique identifier)
  | identifier < 0 = prefix <> "_n" <> T.pack (show (abs identifier))
  | otherwise = prefix <> "_" <> T.pack (show identifier)

genInt :: Gen Int
genInt = Gen.int (Range.linear (-1000) 1000)

genInteger :: Gen Integer
genInteger = Gen.integral (Range.linear (-100000) 100000)

genText :: Gen Text
genText = do
  first <- Gen.element (['a' .. 'z'] <> ['A' .. 'Z'] <> "_$")
  rest <- Gen.list (Range.linear 0 11) (Gen.element (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "_$#'"))
  pure (T.pack (first : rest))

genVarName :: Gen Text
genVarName = do
  first <- Gen.element (['a' .. 'z'] <> "_$")
  rest <- Gen.list (Range.linear 0 11) (Gen.element (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "_$#'"))
  pure (T.pack (first : rest))

genTypeName :: Gen Text
genTypeName = do
  first <- Gen.element ['A' .. 'Z']
  rest <- Gen.list (Range.linear 0 11) (Gen.element (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "_$#'"))
  pure (T.pack (first : rest))

genLiteralText :: Gen Text
genLiteralText = Gen.text (Range.linear 0 12) Gen.unicode

smallList :: Gen a -> Gen [a]
smallList = Gen.list (Range.linear 0 3)
