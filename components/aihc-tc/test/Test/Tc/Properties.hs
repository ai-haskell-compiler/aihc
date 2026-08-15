{-# LANGUAGE OverloadedStrings #-}

-- | Hedgehog property tests for the type checker.
module Test.Tc.Properties
  ( prop_kindEncodingUsesType,
    prop_reflexiveEq,
    prop_starUsesType,
    prop_zonkIdempotent,
    tcProperties,
  )
where

import Aihc.Parser.Syntax (Type (TStar))
import Aihc.Resolve (PackageId (PackageId))
import Aihc.Tc.Kind (convertSurfaceTypeWithKinds)
import Aihc.Tc.Monad (emptyTcEnv, freshMetaTv, initTcState, runTcM, writeMetaTv)
import Aihc.Tc.Types
import Aihc.Tc.Zonk (zonkType)
import Data.Map.Strict qualified as Map
import Hedgehog (Gen, Property, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

tcProperties :: TestTree
tcProperties =
  testGroup
    "properties"
    [ testProperty "lifted kind encoding uses GHC.Types.Type" prop_kindEncodingUsesType,
      testProperty "star uses GHC.Types.Type" prop_starUsesType,
      testProperty "zonking idempotent" prop_zonkIdempotent,
      testProperty "reflexive equality solved" prop_reflexiveEq
    ]

-- | All lifted kind encoders use the canonical Type constructor.
prop_kindEncodingUsesType :: Property
prop_kindEncodingUsesType = property $ do
  let expected = TcBuiltinTyCon "Type" 0 []
  kindToTcType KType === expected
  kindSchemeFromKind KType === ForAll [] [] expected

-- | A source star becomes the canonical GHC.Types.Type constructor.
prop_starUsesType :: Property
prop_starUsesType = property $
  case runTcM emptyTcEnv initTcState (convertSurfaceTypeWithKinds Map.empty (TStar "*")) of
    Right ((actual, kind), _) -> do
      let expected = TcTyCon (mkTyConWithOrigin (PackageId "aihc-prim") "GHC.Types" "Type" 0 KType) []
      actual === expected
      kind === KType
    Left err -> fail (show err)

-- | Zonking a fully-zonked type is a no-op.
prop_zonkIdempotent :: Property
prop_zonkIdempotent = property $ do
  ty <- forAll genSimpleType
  case runTcM
    emptyTcEnv
    initTcState
    ( do
        z1 <- zonkType ty
        z2 <- zonkType z1
        pure (z1, z2)
    ) of
    Right ((t1, t2), _) -> t1 === t2
    Left err -> fail (show err)

-- | A reflexive equality (a ~ a) should be trivially solvable.
prop_reflexiveEq :: Property
prop_reflexiveEq = property $
  case runTcM
    emptyTcEnv
    initTcState
    ( do
        alpha <- freshMetaTv
        -- Solve alpha := Int
        case alpha of
          TcMetaTv u -> do
            let intTy = TcTyCon (TyCon "Int" 0) []
            writeMetaTv u intTy
            result <- zonkType alpha
            pure (result == intTy)
          _ -> pure False
    ) of
    Right (result, _) -> result === True
    Left err -> fail (show err)

genSimpleType :: Gen TcType
genSimpleType = do
  depth <- Gen.int (Range.linear 0 6)
  genSimpleTypeSized depth

genSimpleTypeSized :: Int -> Gen TcType
genSimpleTypeSized depth =
  if depth <= 0
    then genAtomicType
    else
      Gen.choice
        [ genAtomicType,
          genFunType (depth - 1),
          genAppType (depth - 1)
        ]

genAtomicType :: Gen TcType
genAtomicType =
  Gen.choice
    [ TcTyCon <$> genTyCon <*> pure [],
      TcMetaTv <$> genUnique
    ]

genFunType :: Int -> Gen TcType
genFunType depth = TcFunTy <$> genSimpleTypeSized depth <*> genSimpleTypeSized depth

genAppType :: Int -> Gen TcType
genAppType depth = do
  tc <- genTyCon1
  arg <- genSimpleTypeSized depth
  pure (TcTyCon tc [arg])

genTyCon :: Gen TyCon
genTyCon =
  Gen.element
    [ TyCon "Int" 0,
      TyCon "Bool" 0,
      TyCon "Char" 0,
      TyCon "Double" 0
    ]

genTyCon1 :: Gen TyCon
genTyCon1 =
  Gen.element
    [ TyCon "Maybe" 1,
      TyCon "[]" 1,
      TyCon "IO" 1
    ]

genUnique :: Gen Unique
genUnique = Unique <$> Gen.int (Range.linear 100 199)
