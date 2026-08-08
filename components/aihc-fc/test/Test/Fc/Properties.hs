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
      testProperty "package origins distinguish equal symbol names" prop_packageOrigins
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
  annotate (T.unpack renderedA)
  annotate (T.unpack renderedB)
  when (renderedA == renderedB) failure
  case parseExpr renderedA of
    Left parseError -> annotate (show parseError) >> failure
    Right (FcVar var) -> varResolvedName var === Just (FcTopLevelOrigin "pkgA" "Module" "id")
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
genProgram = FcProgram <$> smallList genTopBind

genTopBind :: Gen FcTopBind
genTopBind =
  Gen.choice
    [ FcData <$> genTypeName <*> smallList genTyVar <*> smallList genDataConstructor,
      FcAxiom <$> genAxiomDecl,
      FcNewtype <$> genNewtypeDecl,
      FcPrimitive <$> genVar <*> genInt,
      FcForeignImport <$> genForeignCall,
      FcTopBind <$> genBindWith genExpr
    ]

genDataConstructor :: Gen (Text, [TcType])
genDataConstructor = (,) <$> genTypeName <*> smallList genType

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
  FcNewtypeDecl
    <$> genTypeName
    <*> smallList genTyVar
    <*> genTypeName
    <*> genType
    <*> genType

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
