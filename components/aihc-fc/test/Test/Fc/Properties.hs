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
import Data.ByteString qualified as BS
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
      testProperty "parseType . renderType = id" prop_typeRoundTrip
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

roundTrip :: (Eq a, Show a, Show error) => (a -> String) -> (Text -> Either error a) -> a -> PropertyT IO ()
roundTrip pretty parse value = do
  let rendered = T.pack (pretty value)
  annotate (T.unpack rendered)
  case parse rendered of
    Left parseError -> annotate (show parseError) >> failure
    -- Several compiler identities have intentionally semantic Eq instances
    -- (for example TyCon ignores a refined kind).  Derived Show contains every
    -- constructor field, so this comparison is the exact syntax-tree oracle.
    Right actual -> show actual === show value

genProgram :: Gen FcProgram
genProgram = FcProgram <$> smallList genTopBind

genTopBind :: Gen FcTopBind
genTopBind =
  Gen.choice
    [ FcData <$> genText <*> smallList genTyVar <*> smallList genDataConstructor,
      FcAxiom <$> genAxiomDecl,
      FcNewtype <$> genNewtypeDecl,
      FcPrimitive <$> genVar <*> genInt,
      FcForeignImport <$> genForeignCall,
      FcTopBind <$> genBindWith genExpr
    ]

genDataConstructor :: Gen (Text, [TcType])
genDataConstructor = (,) <$> genText <*> smallList genType

genAxiomDecl :: Gen FcAxiomDecl
genAxiomDecl =
  FcAxiomDecl
    <$> genText
    <*> smallList genTyVar
    <*> Gen.element [FcNominal, FcRepresentational]
    <*> genType
    <*> genType

genNewtypeDecl :: Gen FcNewtypeDecl
genNewtypeDecl =
  FcNewtypeDecl
    <$> genText
    <*> smallList genTyVar
    <*> genText
    <*> genType
    <*> genType

genForeignCall :: Gen FcForeignCall
genForeignCall = FcForeignCall <$> genText <*> genText <*> genForeignSignature

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
      FcRec <$> smallList ((,) <$> genVar <*> child)
    ]

genAltWith :: Gen FcExpr -> Gen FcAlt
genAltWith child = FcAlt <$> genAltCon <*> smallList genVar <*> child

genAltCon :: Gen FcAltCon
genAltCon = Gen.choice [DataAlt <$> genText, LitAlt <$> genLiteral, pure DefaultAlt]

genVar :: Gen Var
genVar = do
  name <- genText
  identifier <- genUnique
  ty <- genType
  resolvedName <- Gen.maybe genText
  pure ((Var name identifier ty) {varResolvedName = resolvedName})

genLiteral :: Gen Literal
genLiteral =
  Gen.choice
    [ LitInt <$> genRuntimeRep <*> genInteger,
      LitChar <$> genRuntimeRep <*> Gen.unicode,
      LitString <$> genText,
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
genPred = Gen.choice [ClassPred <$> genText <*> smallList genType, EqPred <$> genType <*> genType]

genTyVar :: Gen TyVarId
genTyVar = setTyVarKind <$> genKind <*> (TyVarId <$> genText <*> genUnique)

genTyCon :: Gen TyCon
genTyCon = setTyConKind <$> genKind <*> (TyCon <$> genText <*> Gen.int (Range.linear 0 4))

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
      AxiomInstCo <$> genText <*> smallList genType
    ]
    [ Sym <$> genCoercion,
      Trans <$> genCoercion <*> genCoercion,
      TyConAppCo <$> genTyCon <*> smallList genCoercion
    ]

genUnique :: Gen Unique
genUnique = Unique <$> genInt

genInt :: Gen Int
genInt = Gen.int (Range.linear (-1000) 1000)

genInteger :: Gen Integer
genInteger = Gen.integral (Range.linear (-100000) 100000)

genText :: Gen Text
genText = Gen.text (Range.linear 0 12) Gen.unicode

smallList :: Gen a -> Gen [a]
smallList = Gen.list (Range.linear 0 3)
