{-# LANGUAGE OverloadedStrings #-}

module Test.Native.Compiler
  ( tests,
  )
where

import Aihc.Native (NativeTarget (Llvm), backendCompiler, renderLinkedFunctionSymbol)
import Data.Text qualified as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

tests :: TestTree
tests =
  testGroup
    "backend compiler"
    [ testCase "optimizes LLVM IR before object emission" $ do
        (compiler, arguments) <- backendCompiler Llvm
        assertEqual "LLVM compiler" "clang" compiler
        assertBool "LLVM optimization flag" ("-O2" `elem` arguments)
        assertEqual "module-warning flag count" 1 (length (filter (== "-Wno-override-module") arguments)),
      testCase "renders common linker identities readably" $ do
        assertEqual
          "library symbol"
          "aihc_base_4_21_2_0_dephash_Data_Foldable_toList"
          (renderLinkedFunctionSymbol (T.intercalate "\0" ["aihc", "base", "4", "21", "2", "0", "dephash", "Data", "Foldable", "toList"]))
        assertEqual "executable symbol" "exe_Main_main" (renderLinkedFunctionSymbol (T.intercalate "\0" ["exe", "Main", "main"])),
      testCase "escapes unsafe symbol bytes without collisions" $ do
        assertEqual "underscore escape" "aihc_entry_foo__ubar" (renderLinkedFunctionSymbol "foo_bar")
        assertEqual "punctuation escape" "aihc_entry_foo__x2ebar" (renderLinkedFunctionSymbol "foo.bar")
        assertEqual "low byte escape" "aihc_entry_a__x09b" (renderLinkedFunctionSymbol "a\tb")
        assertEqual
          "one escape per utf-8 byte"
          "aihc_entry_caf__xc3__xa9"
          (renderLinkedFunctionSymbol "caf\233")
        assertBool
          "component boundaries cannot imitate escapes"
          (renderLinkedFunctionSymbol (T.intercalate "\0" ["foo", "x2e", "bar"]) /= renderLinkedFunctionSymbol "foo.bar")
    ]
