{-# LANGUAGE OverloadedStrings #-}

module Test.Tc.Interface (tcInterfaceTests) where

import Aihc.Prim.Wiring (primTcWiring)
import Aihc.Resolve (PackageId (..))
import Aihc.Tc
import Aihc.Tc.Types (mkTyConWithOrigin)
import Control.Exception (ErrorCall, evaluate, try)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

tcInterfaceTests :: TestTree
tcInterfaceTests =
  testGroup
    "type interface"
    [ -- Source text cannot create two interface values for one global type identity.
      -- This test verifies rejection of inconsistent internal artifacts.
      testCase "rejects conflicting type constructor interface values" $ do
        let merged = mergeTcInterface CheckMergedFacts canonicalInterface supportInterface
        result <- try (evaluate (length (tcInterfaceTyCons merged))) :: IO (Either ErrorCall Int)
        case result of
          Left _ -> pure ()
          Right _ -> assertFailure "expected a conflicting interface value exception",
      -- A compile that is not linting trusts the interfaces it merges, so
      -- the same conflict goes unreported and the left side stands.
      testCase "takes the left value without checking for conflicts" $ do
        let merged = mergeTcInterface TrustMergedFacts canonicalInterface supportInterface
        result <- try (evaluate (map tciName (tcInterfaceTyCons merged))) :: IO (Either ErrorCall [Text])
        case result of
          Left _ -> assertFailure "expected an unchecked merge to succeed"
          Right names -> names @?= ["List"]
    ]
  where
    kinds = mkTcKinds (primTcWiring (PackageId "aihc-prim"))
    listTyCon = mkTyConWithOrigin (PackageId "aihc-prim") "GHC.Types" "[]" 1
    listKind = ForAll [] [] (TcFunTy (typeKind kinds) (typeKind kinds))
    canonicalInfo = TyConInfo "List" 1 listTyCon listKind DataTyCon Nothing Nothing
    supportInfo = TyConInfo "[]" 1 listTyCon listKind DataTyCon Nothing Nothing
    canonicalInterface = emptyTcInterface {tcInterfaceTyConMap = Map.singleton (tyConKey listTyCon) canonicalInfo}
    supportInterface = emptyTcInterface {tcInterfaceTyConMap = Map.singleton (tyConKey listTyCon) supportInfo}
