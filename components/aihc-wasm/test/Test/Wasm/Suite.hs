{-# LANGUAGE OverloadedStrings #-}

module Test.Wasm.Suite (tests) where

import Aihc.Grin (lowerGc, toCpsGrin)
import Aihc.Grin.Syntax
import Aihc.Native (LinkLayout (..), buildLinkLayout)
import Aihc.Tc.Types (Levity (..), RuntimeRep (..))
import Aihc.Wasm (WasmError (..), compileModule, compileProgram, compileProgramWithDependencies, validatePrimitiveNames, validateProgramPrimitives)
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
      testCase "traps dormant unsupported primitives in dependency modules" testDormantPrimitive,
      testCase "emits relocatable dependency modules" testIncrementalModule,
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
          assertBool "generated entry" (".Laihc_wasm_function_0:" `T.isInfixOf` source)
          assertBool "generated entry is object-local" (not (".globl\t.Laihc_wasm_function_0" `T.isInfixOf` source))
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

testDormantPrimitive :: IO ()
testDormantPrimitive =
  case toCpsGrin dormantPrimitiveProgram of
    Left err -> assertFailure (show err)
    Right cps ->
      case compileModule (buildLinkLayout [dormantPrimitiveProgram]) "_aihc_init_dormant" (lowerGc cps) of
        Left err -> assertFailure (show err)
        Right source -> assertBool "emits the runtime trap" ("call\taihc_unsupported_primitive" `T.isInfixOf` source)

testIncrementalModule :: IO ()
testIncrementalModule =
  case (toCpsGrin dependencyProgram, toCpsGrin program) of
    (Right dependencyCps, Right mainCps) -> do
      let layout = buildLinkLayout [dependencyProgram, program]
      case compileModule layout "_aihc_init_test" (lowerGc dependencyCps) of
        Left err -> assertFailure (show err)
        Right source -> do
          assertBool "exports dependency initializer" ("_aihc_init_test:" `T.isInfixOf` source)
          assertBool "does not emit executable entry" (not ("aihc_wasm_program_initialize:" `T.isInfixOf` source))
          assertBool "does not define shared arguments" (not ("aihc_arguments:" `T.isInfixOf` source))
      case compileProgramWithDependencies layout ["_aihc_init_test"] "main" (lowerGc mainCps) of
        Left err -> assertFailure (show err)
        Right source -> do
          assertBool "calls dependency initializer" ("call\t_aihc_init_test" `T.isInfixOf` source)
          let expectedAllocation = "\ti64.const\t" <> T.pack (show (length (linkGlobalNames layout))) <> "\n\tcall\taihc_machine_new"
          assertBool "uses combined global layout" (expectedAllocation `T.isInfixOf` source)
    (Left err, _) -> assertFailure (show err)
    (_, Left err) -> assertFailure (show err)

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

dependencyProgram :: GrinProgram
dependencyProgram =
  program
    { grinWhnfGlobals =
        [ ( GrinVar "dependency" 2 (BoxedRep Lifted),
            GrinNode (GrinClosure dependencyFunction [[]]) []
          )
        ],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = dependencyFunction,
              grinFunctionLinkName = Just "Demo.dependency",
              grinFunctionParameters = [],
              grinFunctionResultRep = IntRep,
              grinFunctionBody = GrinConstant [GrinLitValue (GrinLitInt IntRep 7)]
            }
        ]
    }

dependencyFunction :: FunctionName
dependencyFunction = FunctionName "$dependency"

dormantPrimitiveProgram :: GrinProgram
dormantPrimitiveProgram =
  GrinProgram
    { grinConstructors = [],
      grinPrimitives = [(GrinVar "unsupported#" 40 IntRep, 1)],
      grinForeignCalls = [],
      grinExternalGlobals = [],
      grinExternalFunctions = [],
      grinWhnfGlobals = [],
      grinCafs = [],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = FunctionName "$dormant_unsupported",
              grinFunctionLinkName = Nothing,
              grinFunctionParameters = [],
              grinFunctionResultRep = IntRep,
              grinFunctionBody =
                GrinBind
                  [result]
                  (GrinPrimitiveCall IntRep "unsupported#" [GrinLitValue (GrinLitInt IntRep 1)])
                  (GrinConstant [GrinVarValue result])
            }
        ]
    }
  where
    result = GrinVar "result" 41 IntRep

supportedPrimitives :: [T.Text]
supportedPrimitives = ["+#", "awaitIO#", "fork#", "realWorld#", "yield#"]
