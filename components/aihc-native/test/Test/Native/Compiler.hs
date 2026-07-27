module Test.Native.Compiler
  ( tests,
  )
where

import Aihc.Native (NativeTarget (Llvm), backendCompiler)
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
        assertEqual "module-warning flag count" 1 (length (filter (== "-Wno-override-module") arguments))
    ]
