{-# LANGUAGE OverloadedStrings #-}

module Test.Tc.Interface (tcInterfaceTests) where

import Aihc.Resolve (PackageId (..))
import Aihc.Tc
import Aihc.Tc.Monad (addDataType, emptyTcEnv, initTcState, runTcM, tcAbortMessage)
import Aihc.Tc.Types (mkTyConWithOrigin)
import Control.Exception (ErrorCall, displayException, evaluate, try)
import Control.Monad (void)
import Data.List (isInfixOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

tcInterfaceTests :: TestTree
tcInterfaceTests =
  testGroup
    "type interface"
    [ testCase "keeps the source type name during support merges" $ do
        assertEqual "canonical then support" ["List"] (map tciName (tcInterfaceTyCons (canonicalInterface <> supportInterface)))
        assertEqual "support then canonical" ["List"] (map tciName (tcInterfaceTyCons (supportInterface <> canonicalInterface))),
      testCase "rejects a lossy data type interface merge" $
        assertErrorCall "duplicate data type interface key" $
          evaluate (length (tcInterfaceDataTypes (interfaceA <> interfaceB))),
      testCase "rejects a duplicate data type state key" $
        case runTcM emptyTcEnv initTcState (addDataType dataTypeA >> addDataType dataTypeB) of
          Left abort -> assertBool "duplicate state key message" ("duplicate data type state key" `isInfixOf` tcAbortMessage abort)
          Right _ -> assertFailure "duplicate data type state key did not fail"
    ]
  where
    listTyCon = mkTyConWithOrigin (PackageId "aihc-prim") "GHC.Types" "[]" 1 (KFun liftedTypeKind liftedTypeKind)
    canonicalInfo = TyConInfo "List" 1 listTyCon DataTyCon Nothing
    supportInfo = TyConInfo "[]" 1 listTyCon DataTyCon Nothing
    canonicalInterface = emptyTcInterface {tcInterfaceTyCons = [canonicalInfo]}
    supportInterface = emptyTcInterface {tcInterfaceTyCons = [supportInfo]}
    tyConA = mkTyConWithOrigin (PackageId "main") "A" "Ty" 0 liftedTypeKind
    tyConB = mkTyConWithOrigin (PackageId "main") "B" "Ty" 0 liftedTypeKind
    dataTypeA = DataTypeInfo "Ty" tyConA [] liftedTypeKind DataTyCon []
    dataTypeB = DataTypeInfo "Ty" tyConB [] liftedTypeKind DataTyCon []
    interfaceA = emptyTcInterface {tcInterfaceDataTypes = [dataTypeA]}
    interfaceB = emptyTcInterface {tcInterfaceDataTypes = [dataTypeB]}

assertErrorCall :: String -> IO a -> IO ()
assertErrorCall expected action = do
  result <- try (void action) :: IO (Either ErrorCall ())
  case result of
    Left exception -> assertBool "exception message" (expected `isInfixOf` displayException exception)
    Right () -> assertFailure "expected an exception"
