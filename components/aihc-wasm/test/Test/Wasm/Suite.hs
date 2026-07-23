{-# LANGUAGE OverloadedStrings #-}

module Test.Wasm.Suite (tests) where

import Aihc.Grin (lowerGc, toCpsGrin)
import Aihc.Grin.Syntax
import Aihc.Tc.Types (Levity (..), RuntimeRep (..))
import Aihc.Wasm (WasmError (..), compileProgram, validatePrimitiveNames, validateProgramPrimitives)
import Data.Text qualified as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck (elements, forAll, testProperty)

tests :: TestTree
tests =
  testGroup
    "Direct WebAssembly backend"
    [ testCase "emits WebAssembly assembly without C or LLVM IR" testDirectModule,
      testCase "rejects a missing entry point" testMissingEntry,
      testCase "rejects unsupported primitives" testUnsupportedPrimitive,
      testProperty "accepts supported primitives" $
        forAll (elements supportedPrimitives) $ \name ->
          validatePrimitiveNames [name] == Right ()
    ]

testDirectModule :: IO ()
testDirectModule =
  case toCpsGrin program of
    Left err -> assertFailure (show err)
    Right cps ->
      case compileProgram "main" (lowerGc cps) of
        Left err -> assertFailure (show err)
        Right source -> do
          assertBool "WebAssembly instructions" ("\t.functype\t" `T.isInfixOf` source && "call\taihc_alloc_locals" `T.isInfixOf` source)
          assertBool "generated entry" ("aihc_function_0:" `T.isInfixOf` source)
          assertBool "not portable C" (not ("#include" `T.isInfixOf` source))
          assertBool "not LLVM IR" (not ("target triple" `T.isInfixOf` source))

testMissingEntry :: IO ()
testMissingEntry =
  case toCpsGrin program of
    Left err -> assertFailure (show err)
    Right cps -> assertEqual "missing entry" (Left (WasmMissingEntry "missing")) (compileProgram "missing" (lowerGc cps))

testUnsupportedPrimitive :: IO ()
testUnsupportedPrimitive =
  assertEqual
    "unsupported primitive"
    (Left (WasmUnsupportedPrimitive "unsupported#"))
    (validateProgramPrimitives program {grinPrimitives = [(GrinVar "unsupported#" 30 IntRep, 1)]})

program :: GrinProgram
program =
  GrinProgram
    { grinConstructors = [],
      grinPrimitives = [],
      grinForeignCalls = [],
      grinExternalGlobals = [],
      grinExternalFunctions = [],
      grinWhnfGlobals =
        [ ( GrinVar "main" 1 (BoxedRep Lifted),
            GrinNode (GrinClosure mainFunction [[]]) []
          )
        ],
      grinCafs = [],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = mainFunction,
              grinFunctionLinkName = Nothing,
              grinFunctionParameters = [],
              grinFunctionResultRep = IntRep,
              grinFunctionBody = GrinConstant [GrinLitValue (GrinLitInt IntRep 42)]
            }
        ]
    }

mainFunction :: FunctionName
mainFunction = FunctionName "$main"

supportedPrimitives :: [T.Text]
supportedPrimitives = ["+#", "awaitIO#", "fork#", "realWorld#", "yield#"]
