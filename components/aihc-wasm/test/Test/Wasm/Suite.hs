{-# LANGUAGE OverloadedStrings #-}

module Test.Wasm.Suite (tests) where

import Aihc.Grin (lowerGc, toCpsGrin)
import Aihc.Grin.Syntax
import Aihc.Native (LinkLayout (..), buildLinkLayout, supportedNativePrimitiveNames)
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
      testCase "keeps GRIN variables in WebAssembly locals" testWasmLocals,
      testCase "stages only explicit moving-GC roots in memory" testGcRootStaging,
      testCase "passes direct-call arguments through the machine transfer vector" testDirectCallArguments,
      testCase "rejects a missing entry point" testMissingEntry,
      testCase "rejects unsupported primitives" testUnsupportedPrimitive,
      testCase "traps dormant unsupported primitives in dependency modules" testDormantPrimitive,
      testCase "emits relocatable dependency modules" testIncrementalModule,
      testCase "lowers byte-array primitives through the runtime ABI" testByteArrayPrimitives,
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
          assertBool "WebAssembly instructions" ("\t.functype\t" `T.isInfixOf` source && "local.set\t" `T.isInfixOf` source)
          assertBool "generated entry" (".Laihc_wasm_function_0:" `T.isInfixOf` source)
          assertBool "generated entry is object-local" (not (".globl\t.Laihc_wasm_function_0" `T.isInfixOf` source))
          assertBool "not portable C" (not ("#include" `T.isInfixOf` source))
          assertBool "not LLVM IR" (not ("target triple" `T.isInfixOf` source))

testWasmLocals :: IO ()
testWasmLocals =
  case toCpsGrin directCallProgram of
    Left err -> assertFailure (show err)
    Right cps ->
      case compileModule (buildLinkLayout [directCallProgram]) "_aihc_init_wasm_locals" (lowerGc cps) of
        Left err -> assertFailure (show err)
        Right source -> do
          assertBool "loads the incoming parameter directly" ("i64.load\t0" `T.isInfixOf` source)
          assertBool "assigns and reads WebAssembly locals" ("local.set\t3" `T.isInfixOf` source && "local.get\t3" `T.isInfixOf` source)
          assertBool "does not allocate runtime local storage" (not ("call\taihc_alloc_locals" `T.isInfixOf` source))
          assertBool "does not use C slot accessors" (not ("aihc_wasm_slot_" `T.isInfixOf` source))

testGcRootStaging :: IO ()
testGcRootStaging =
  case toCpsGrin gcRootProgram of
    Left err -> assertFailure (show err)
    Right cps ->
      case compileModule (buildLinkLayout [gcRootProgram]) "_aihc_init_gc_roots" (lowerGc cps) of
        Left err -> assertFailure (show err)
        Right source -> do
          let (beforeCollection, fromCollection) = T.breakOn "call\taihc_ensure_heap" source
          assertBool "emits an explicit collection safepoint" (not (T.null fromCollection))
          assertBool "stages the live root before collection" ("i64.store\t0" `T.isInfixOf` beforeCollection)
          assertBool "reloads the relocated root after collection" ("i64.load\t0" `T.isInfixOf` fromCollection)

testDirectCallArguments :: IO ()
testDirectCallArguments =
  case toCpsGrin directCallProgram of
    Left err -> assertFailure (show err)
    Right cps ->
      case compileModule (buildLinkLayout [directCallProgram]) "_aihc_init_direct_call" (lowerGc cps) of
        Left err -> assertFailure (show err)
        Right source -> do
          assertBool "direct transfer receives machine, entry, count, and values" ("\t.functype\taihc_wasm_transfer_direct (i32, i32, i64, i32) -> ()" `T.isInfixOf` source)
          assertBool "materializes direct-call arguments in module scratch space" (".Laihc_wasm_scratch" `T.isInfixOf` source && "i64.store\t0" `T.isInfixOf` source)
          assertBool "does not use a fixed shared argument buffer" (not ("aihc_arguments" `T.isInfixOf` source))

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

testByteArrayPrimitives :: IO ()
testByteArrayPrimitives =
  case toCpsGrin byteArrayProgram of
    Left err -> assertFailure (show err)
    Right cps ->
      case compileModule (buildLinkLayout [byteArrayProgram]) "_aihc_init_byte_array" (lowerGc cps) of
        Left err -> assertFailure (show err)
        Right source -> do
          assertBool "allocates a pinned byte array" ("call\taihc_byte_array_new_pinned" `T.isInfixOf` source)
          assertBool "copies an address into the byte array" ("call\taihc_byte_array_copy_from_addr" `T.isInfixOf` source)
          assertBool "obtains the byte-array payload" ("call\taihc_byte_array_contents" `T.isInfixOf` source)

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

directCallProgram :: GrinProgram
directCallProgram =
  GrinProgram
    { grinConstructors = [],
      grinPrimitives = [],
      grinForeignCalls = [],
      grinExternalGlobals = [],
      grinExternalFunctions =
        [ GrinCodeInfo
            { grinCodeSourceName = "identity",
              grinCodeFunctionName = identityFunction,
              grinCodeParameterLayouts = [[BoxedRep Lifted]],
              grinCodeResultRep = BoxedRep Lifted
            }
        ],
      grinWhnfGlobals = [],
      grinCafs = [],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = FunctionName "$caller",
              grinFunctionLinkName = Just "caller",
              grinFunctionParameters = [argument],
              grinFunctionResultRep = BoxedRep Lifted,
              grinFunctionBody = GrinCall (BoxedRep Lifted) identityFunction [GrinVarValue argument]
            }
        ]
    }
  where
    argument = GrinVar "argument" 50 (BoxedRep Lifted)
    identityFunction = FunctionName "$identity"

gcRootProgram :: GrinProgram
gcRootProgram =
  GrinProgram
    { grinConstructors = [("Box", [[BoxedRep Lifted]])],
      grinPrimitives = [],
      grinForeignCalls = [],
      grinExternalGlobals = [],
      grinExternalFunctions = [],
      grinWhnfGlobals = [],
      grinCafs = [],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = FunctionName "$allocate_box",
              grinFunctionLinkName = Just "allocate_box",
              grinFunctionParameters = [root],
              grinFunctionResultRep = BoxedRep Lifted,
              grinFunctionBody = GrinStore (GrinNode (GrinConstructor "Box" 0) [GrinVarValue root])
            }
        ]
    }
  where
    root = GrinVar "root" 55 (BoxedRep Lifted)

byteArrayProgram :: GrinProgram
byteArrayProgram =
  GrinProgram
    { grinConstructors = [],
      grinPrimitives =
        [ (GrinVar "newPinnedByteArray#" 60 (BoxedRep Unlifted), 2),
          (GrinVar "copyAddrToByteArray#" 61 (TupleRep []), 5),
          (GrinVar "mutableByteArrayContents#" 62 AddrRep, 1)
        ],
      grinForeignCalls = [],
      grinExternalGlobals = [],
      grinExternalFunctions = [],
      grinWhnfGlobals = [],
      grinCafs = [],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = FunctionName "$byte_array",
              grinFunctionLinkName = Just "byte_array",
              grinFunctionParameters = [],
              grinFunctionResultRep = AddrRep,
              grinFunctionBody =
                GrinBind [array] (GrinPrimitiveCall (BoxedRep Unlifted) "newPinnedByteArray#" [intValue 1]) $
                  GrinBind [] (GrinPrimitiveCall (TupleRep []) "copyAddrToByteArray#" [GrinLitValue (GrinLitAddr "x"), GrinVarValue array, intValue 0, intValue 1]) $
                    GrinBind [contents] (GrinPrimitiveCall AddrRep "mutableByteArrayContents#" [GrinVarValue array]) $
                      GrinConstant [GrinVarValue contents]
            }
        ]
    }
  where
    array = GrinVar "array" 63 (BoxedRep Unlifted)
    contents = GrinVar "contents" 64 AddrRep
    intValue = GrinLitValue . GrinLitInt IntRep

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
supportedPrimitives = supportedNativePrimitiveNames
