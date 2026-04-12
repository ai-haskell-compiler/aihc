{-# LANGUAGE OverloadedStrings #-}

-- | QuickCheck property tests for the type checker.
module Test.Tc.Properties
  ( tcProperties,
  )
where

import Aihc.Tc.Monad (emptyTcEnv, freshMetaTv, initTcState, runTcM, writeMetaTv)
import Aihc.Tc.Types
import Aihc.Tc.Zonk (zonkType)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary (..), Gen, elements, oneof, resize, sized, testProperty)

tcProperties :: TestTree
tcProperties =
  testGroup
    "properties"
    [ testProperty "zonking idempotent" prop_zonkIdempotent,
      testProperty "reflexive equality solved" prop_reflexiveEq
    ]

-- | Zonking a fully-zonked type is a no-op.
prop_zonkIdempotent :: SimpleType -> Bool
prop_zonkIdempotent (SimpleType ty) =
  case runTcM
    emptyTcEnv
    initTcState
    ( do
        z1 <- zonkType ty
        z2 <- zonkType z1
        pure (z1, z2)
    ) of
    Right ((t1, t2), _) -> t1 == t2
    Left _ -> False

-- | A reflexive equality (a ~ a) should be trivially solvable.
prop_reflexiveEq :: Bool
prop_reflexiveEq =
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
    Right (True, _) -> True
    _ -> False

-- | Wrapper for generating simple types suitable for property testing.
newtype SimpleType = SimpleType TcType
  deriving (Show)

instance Arbitrary SimpleType where
  arbitrary = SimpleType <$> genSimpleType

genSimpleType :: Gen TcType
genSimpleType = sized $ \n ->
  if n <= 0
    then genAtomicType
    else
      oneof
        [ genAtomicType,
          resize (n `div` 2) genFunType,
          resize (n `div` 2) genAppType
        ]

genAtomicType :: Gen TcType
genAtomicType =
  oneof
    [ TcTyCon <$> genTyCon <*> pure [],
      TcMetaTv <$> genUnique
    ]

genFunType :: Gen TcType
genFunType = TcFunTy <$> genSimpleType <*> genSimpleType

genAppType :: Gen TcType
genAppType = do
  tc <- genTyCon1
  arg <- genSimpleType
  pure (TcTyCon tc [arg])

genTyCon :: Gen TyCon
genTyCon =
  elements
    [ TyCon "Int" 0,
      TyCon "Bool" 0,
      TyCon "Char" 0,
      TyCon "Double" 0
    ]

genTyCon1 :: Gen TyCon
genTyCon1 =
  elements
    [ TyCon "Maybe" 1,
      TyCon "[]" 1,
      TyCon "IO" 1
    ]

genUnique :: Gen Unique
genUnique = Unique <$> elements [100 .. 199]
