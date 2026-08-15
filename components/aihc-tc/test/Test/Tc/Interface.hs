{-# LANGUAGE OverloadedStrings #-}

module Test.Tc.Interface (tcInterfaceTests) where

import Aihc.Resolve (PackageId (..))
import Aihc.Tc
import Aihc.Tc.Types (mkTyConWithOrigin)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

tcInterfaceTests :: TestTree
tcInterfaceTests =
  testGroup
    "type interface"
    [ testCase "keeps the source type name during support merges" $ do
        assertEqual "canonical then support" ["List"] (map tciName (tcInterfaceTyCons (canonicalInterface <> supportInterface)))
        assertEqual "support then canonical" ["List"] (map tciName (tcInterfaceTyCons (supportInterface <> canonicalInterface)))
    ]
  where
    listTyCon = mkTyConWithOrigin (PackageId "aihc-prim") "GHC.Types" "[]" 1 (KFun liftedTypeKind liftedTypeKind)
    canonicalInfo = TyConInfo "List" 1 listTyCon DataTyCon Nothing
    supportInfo = TyConInfo "[]" 1 listTyCon DataTyCon Nothing
    canonicalInterface = emptyTcInterface {tcInterfaceTyCons = [canonicalInfo]}
    supportInterface = emptyTcInterface {tcInterfaceTyCons = [supportInfo]}
