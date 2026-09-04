{-# LANGUAGE OverloadedStrings #-}

module Test.Wasm.Suite (tests) where

import Aihc.Grin (lowerGc, toCpsGrin)
import Aihc.Grin.Syntax
import Aihc.Native (executableEntryName, renderLinkedGlobalSymbol, supportedNativePrimitiveNames)
import Aihc.Wasm (WasmError (..), compileEntry, compileModule, validatePrimitiveNames, validateProgramPrimitives)
import Control.Monad (forM_)
import Data.Text qualified as T
import Hedgehog (forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  testGroup
    "Direct WebAssembly backend"
    [ testCase "emits WebAssembly assembly without C or LLVM IR" testDirectModule,
      testCase "keeps GRIN variables in WebAssembly locals" testWasmLocals,
      testCase "declares locals after repeated parameters" testRepeatedParameterLocals,
      testCase "compares literal alternatives against the case scrutinee" testLiteralCaseScrutinee,
      testCase "stages only explicit moving-GC roots in memory" testGcRootStaging,
      testCase "passes known direct-call arguments through typed tail calls" testDirectCallArguments,
      testCase "loads captured values through object-entry adapters" testObjectEntryAdapters,
      testCase "lowers synchronous exception transfers" testSynchronousException,
      testCase "rejects unsupported primitives" testUnsupportedPrimitive,
      testCase "traps dormant unsupported primitives in dependency modules" testDormantPrimitive,
      testCase "emits relocatable dependency modules" testIncrementalModule,
      testCase "lowers byte-array primitives through the runtime ABI" testByteArrayPrimitives,
      testCase "lowers Integer arithmetic primitives without fallback traps" testIntegerPrimitives,
      testCase "lowers MVar primitives through scheduler transfers" testMVarPrimitives,
      testCase "lowers native-width Int foreign calls" testForeignInt,
      testCase "lowers the Prelude Int primitive API" testIntPrimitives,
      testProperty "accepts supported primitives" . property $ do
        name <- forAll (Gen.element supportedPrimitives)
        validatePrimitiveNames [name] === Right ()
    ]

testDirectModule :: IO ()
testDirectModule =
  case toCpsGrin (withExecutableEntry program) of
    Left err -> assertFailure (show err)
    Right cps ->
      case (compileModule (lowerGc cps), compileEntry) of
        (Left err, _) -> assertFailure (show err)
        (_, Left err) -> assertFailure (show err)
        (Right moduleSource, Right entrySource) -> do
          let source = moduleSource <> entrySource
          assertBool "WebAssembly instructions" ("\t.functype\t" `T.isInfixOf` source && "local.set\t" `T.isInfixOf` source)
          assertBool "generated entry" (".Laihc_wasm_function_0:" `T.isInfixOf` source)
          assertBool "generated entry is object-local" (not (".globl\t.Laihc_wasm_function_0" `T.isInfixOf` source))
          assertBool "does not emit C source" (not ("#include" `T.isInfixOf` source))
          assertBool "not LLVM IR" (not ("target triple" `T.isInfixOf` source))
          assertBool "declares stable-name allocation" (".functype\taihc_stable_name_make (i32, i32) -> (i32)" `T.isInfixOf` source)
          assertBool "declares stable-name equality" (".functype\taihc_stable_name_equal (i32, i32) -> (i64)" `T.isInfixOf` source)
          assertBool "declares stable-name hashing" (".functype\taihc_stable_name_hash (i32) -> (i64)" `T.isInfixOf` source)
          assertBool "stores closure kind in the shared info-table ABI" ("\t.int64\t3\n\t.int64\t1\n\t.int32\t0\n\t.skip\t4\n\t.size\t.Laihc_wasm_update_info, 64" `T.isInfixOf` source)
          assertBool "emits stop continuation frame metadata" ("\t.int64\t5\n\t.int64\t1\n\t.int32\t0\n\t.skip\t4\n\t.size\t.Laihc_wasm_final_info, 64" `T.isInfixOf` source)

testWasmLocals :: IO ()
testWasmLocals =
  case toCpsGrin directCallProgram of
    Left err -> assertFailure (show err)
    Right cps ->
      case compileModule (lowerGc cps) of
        Left err -> assertFailure (show err)
        Right source -> do
          let (_, fromFastEntry) = T.breakOn ".Laihc_wasm_function_0:" source
              fastEntry = fst (T.breakOn "\tend_function" fromFastEntry)
          assertBool "reads the incoming parameter directly" ("local.get\t1" `T.isInfixOf` fastEntry)
          assertBool "does not copy fast-entry parameters from memory" (not ("i64.load" `T.isInfixOf` fastEntry))
          assertBool "does not allocate runtime local storage" (not ("call\taihc_alloc_locals" `T.isInfixOf` source))
          assertBool "does not use C slot accessors" (not ("aihc_wasm_slot_" `T.isInfixOf` source))

testRepeatedParameterLocals :: IO ()
testRepeatedParameterLocals =
  case toCpsGrin repeatedParameterProgram of
    Left err -> assertFailure (show err)
    Right cps ->
      case compileModule (lowerGc cps) of
        Left err -> assertFailure (show err)
        Right source -> do
          let label = "\n.Laihc_wasm_function_0:\n"
              (_, fromFunction) = T.breakOn label source
              function = fst (T.breakOn "\tend_function" fromFunction)
          assertBool "uses the bound result local" ("local.set\t" `T.isInfixOf` function)
          assertEqual
            "declares the case local and every allocated value local"
            ["\t.local\ti64, i64"]
            (filter (T.isPrefixOf "\t.local\t") (T.lines function))

testLiteralCaseScrutinee :: IO ()
testLiteralCaseScrutinee =
  case toCpsGrin literalCaseProgram of
    Left err -> assertFailure (show err)
    Right cps ->
      case compileModule (lowerGc cps) of
        Left err -> assertFailure (show err)
        Right source ->
          assertBool
            "compares the saved scrutinee rather than an unrelated GRIN local"
            ("local.get\t3\n\ti64.const\t7\n\ti64.eq" `T.isInfixOf` source)

testGcRootStaging :: IO ()
testGcRootStaging =
  case toCpsGrin gcRootProgram of
    Left err -> assertFailure (show err)
    Right cps ->
      case compileModule (lowerGc cps) of
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
      case compileModule (lowerGc cps) of
        Left err -> assertFailure (show err)
        Right source -> do
          assertBool "declares the exact local fast-entry signature" ("\t.functype\t.Laihc_wasm_function_1 (i32, i64, i64) -> ()" `T.isInfixOf` source)
          assertBool "tail-calls the known entry" ("return_call\t.Laihc_wasm_function_1" `T.isInfixOf` source)
          assertBool "does not route known calls through C" (not ("call\taihc_wasm_transfer_direct" `T.isInfixOf` source))

testObjectEntryAdapters :: IO ()
testObjectEntryAdapters =
  case toCpsGrin capturedThunkProgram of
    Left err -> assertFailure (show err)
    Right cps ->
      case compileModule (lowerGc cps) of
        Left err -> assertFailure (show err)
        Right source -> do
          assertBool "emits a uniform object entry" ("_enter:\n\t.functype" `T.isInfixOf` source && "(i32, i64, i32, i64) -> ()" `T.isInfixOf` source)
          assertBool "loads a captured field directly from the thunk" ("local.get\t1\n\ti32.wrap_i64\n\ti64.load\t8" `T.isInfixOf` source)
          assertBool "passes the update continuation directly" ("local.get\t3\n\treturn_call" `T.isInfixOf` source)
          assertBool "object entry tail-calls the typed fast entry" ("return_call\t.Laihc_wasm_function_0" `T.isInfixOf` source)
          assertBool "publishes the object entry in runtime info" ("_enter\n\t.skip\t4" `T.isInfixOf` source)

testSynchronousException :: IO ()
testSynchronousException =
  case toCpsGrin (withExecutableEntry exceptionProgram) of
    Left err -> assertFailure (show err)
    Right cps ->
      case (compileModule (lowerGc cps), compileEntry) of
        (Left err, _) -> assertFailure (show err)
        (_, Left err) -> assertFailure (show err)
        (Right moduleSource, Right entrySource) -> do
          let source = moduleSource <> entrySource
          assertBool "calls the shared raise transfer" ("call\taihc_wasm_transfer_raise" `T.isInfixOf` source)
          assertBool "emits catch frame metadata" ("\t.int64\t2\n\t.int64\t1\n\t.int32\t0\n\t.skip\t4\n\t.size\t" `T.isInfixOf` source)

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
      case compileModule (lowerGc cps) of
        Left err -> assertFailure (show err)
        Right source -> assertBool "emits the runtime trap" ("call\taihc_unsupported_primitive" `T.isInfixOf` source)

testIncrementalModule :: IO ()
testIncrementalModule =
  case (toCpsGrin dependencyProgram, toCpsGrin (withExecutableEntry program)) of
    (Right dependencyCps, Right mainCps) -> do
      case compileModule (lowerGc dependencyCps) of
        Left err -> assertFailure (show err)
        Right source -> do
          assertBool "exports dependency global" (".globl\taihc_entry_dependency" `T.isInfixOf` source)
          assertBool "does not emit executable entry" (not ("aihc_wasm_program_initialize:" `T.isInfixOf` source))
          assertBool "does not define shared arguments" (not ("aihc_arguments:" `T.isInfixOf` source))
      case (compileModule (lowerGc mainCps), compileEntry) of
        (Left err, _) -> assertFailure (show err)
        (_, Left err) -> assertFailure (show err)
        (Right _, Right source) -> do
          assertBool "references entry global" (("i32.const\t" <> renderLinkedGlobalSymbol executableEntryName) `T.isInfixOf` source)
          assertBool "allocates no global slots" ("i64.const\t0\n\tcall\taihc_machine_new" `T.isInfixOf` source)
    (Left err, _) -> assertFailure (show err)
    (_, Left err) -> assertFailure (show err)

withExecutableEntry :: GrinProgram -> GrinProgram
withExecutableEntry input =
  input
    { grinGlobals =
        [ (if name == "main" then executableEntryName else name, node)
        | (name, node) <- grinGlobals input
        ]
    }

testByteArrayPrimitives :: IO ()
testByteArrayPrimitives =
  case toCpsGrin byteArrayProgram of
    Left err -> assertFailure (show err)
    Right cps ->
      case compileModule (lowerGc cps) of
        Left err -> assertFailure (show err)
        Right source -> do
          assertBool "allocates a pinned byte array" ("call\taihc_byte_array_new_pinned" `T.isInfixOf` source)
          assertBool "copies an address into the byte array" ("call\taihc_byte_array_copy_from_addr" `T.isInfixOf` source)
          assertBool "obtains the byte-array payload" ("call\taihc_byte_array_contents" `T.isInfixOf` source)

testIntegerPrimitives :: IO ()
testIntegerPrimitives = forM_ integerPrimitiveCases $ \primitiveCase ->
  case toCpsGrin (primitiveProgram primitiveCase) of
    Left err -> assertFailure (T.unpack (primitiveCaseName primitiveCase) <> ": " <> show err)
    Right cps ->
      case compileModule (lowerGc cps) of
        Left err -> assertFailure (T.unpack (primitiveCaseName primitiveCase) <> ": " <> show err)
        Right source -> do
          assertBool
            (T.unpack (primitiveCaseName primitiveCase) <> " does not use the unsupported-primitive fallback")
            (not ("call\taihc_unsupported_primitive" `T.isInfixOf` source))
          forM_ (primitiveCaseInstructions primitiveCase) $ \instruction ->
            assertBool
              (T.unpack (primitiveCaseName primitiveCase) <> " emits " <> T.unpack instruction)
              (instruction `T.isInfixOf` source)

data PrimitiveCase = PrimitiveCase
  { primitiveCaseName :: !T.Text,
    primitiveCaseArguments :: ![GrinValue],
    primitiveCaseResults :: ![GrinRep],
    primitiveCaseInstructions :: ![T.Text]
  }

integerPrimitiveCases :: [PrimitiveCase]
integerPrimitiveCases =
  [ intBinary "+#" "i64.add",
    intBinary "-#" "i64.sub",
    intBinary "*#" "i64.mul",
    intComparison "<#" "i64.lt_s",
    intComparison "==#" "i64.eq",
    intComparison "compareInt#" "i64.gt_s",
    intCarry "addIntC#" "i64.add",
    intCarry "subIntC#" "i64.sub",
    wordBinary "plusWord#" "i64.add",
    wordBinary "minusWord#" "i64.sub",
    wordBinary "timesWord#" "i64.mul",
    wordCarry "addWordC#" "i64.add",
    wordCarry "subWordC#" "i64.sub",
    PrimitiveCase "timesWord2#" wordArguments [WordRep, WordRep] ["call\taihc_wasm_times_word2_high", "i64.mul"],
    wordBinary "quotWord#" "i64.div_u",
    wordBinary "remWord#" "i64.rem_u",
    PrimitiveCase "quotRemWord#" wordArguments [WordRep, WordRep] ["i64.div_u", "i64.rem_u"],
    PrimitiveCase "quotRemWord2#" [primitiveWordValue 1, primitiveWordValue 2, primitiveWordValue 3] [WordRep, WordRep] ["call\taihc_wasm_quot_rem_word2_quotient"],
    wordBinary "and#" "i64.and",
    wordBinary "or#" "i64.or",
    wordBinary "xor#" "i64.xor",
    PrimitiveCase "not#" [primitiveWordValue 1] [WordRep] ["i64.xor"],
    PrimitiveCase "uncheckedShiftL#" [primitiveWordValue 1, primitiveIntValue 2] [WordRep] ["i64.shl"],
    PrimitiveCase "uncheckedShiftRL#" [primitiveWordValue 1, primitiveIntValue 2] [WordRep] ["i64.shr_u"],
    PrimitiveCase "int2Word#" [primitiveIntValue 1] [WordRep] ["local.set"],
    PrimitiveCase "word2Int#" [primitiveWordValue 1] [IntRep] ["local.set"],
    wordComparison "eqWord#" "i64.eq",
    wordComparison "neWord#" "i64.ne",
    wordComparison "ltWord#" "i64.lt_u",
    wordComparison "leWord#" "i64.le_u",
    wordComparison "gtWord#" "i64.gt_u",
    wordComparison "geWord#" "i64.ge_u",
    PrimitiveCase "clz#" [primitiveWordValue 1] [WordRep] ["i64.clz"],
    PrimitiveCase "ctz#" [primitiveWordValue 1] [WordRep] ["i64.ctz"],
    PrimitiveCase "popCnt#" [primitiveWordValue 1] [WordRep] ["i64.popcnt"]
  ]
  where
    intBinary name instruction = PrimitiveCase name intArguments [IntRep] [instruction]
    intComparison name instruction = PrimitiveCase name intArguments [IntRep] [instruction]
    intCarry name instruction = PrimitiveCase name intArguments [IntRep, IntRep] [instruction, "i64.shr_u"]
    wordBinary name instruction = PrimitiveCase name wordArguments [WordRep] [instruction]
    wordCarry name instruction = PrimitiveCase name wordArguments [WordRep, IntRep] [instruction, "i64.lt_u"]
    wordComparison name instruction = PrimitiveCase name wordArguments [IntRep] [instruction]
    intArguments = [primitiveIntValue 7, primitiveIntValue 3]
    wordArguments = [primitiveWordValue 7, primitiveWordValue 3]

primitiveProgram :: PrimitiveCase -> GrinProgram
primitiveProgram primitiveCase =
  GrinProgram
    { grinConstructors = [],
      grinPrimitives = [(GrinVar name 80 resultRep, length arguments)],
      grinForeignCalls = [],
      grinGlobals = [],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = FunctionName ("$primitive_" <> name),
              grinFunctionParameters = [],
              grinFunctionResultRep = resultRep,
              grinFunctionBody =
                GrinBind
                  results
                  (GrinPrimitiveCall resultRep name arguments)
                  (GrinConstant (map GrinVarValue results))
            }
        ]
    }
  where
    name = primitiveCaseName primitiveCase
    arguments = primitiveCaseArguments primitiveCase
    resultReps = primitiveCaseResults primitiveCase
    resultRep = case resultReps of
      [single] -> single
      _ -> TupleRep resultReps
    results = [GrinVar ("result" <> T.pack (show index)) (81 + index) runtimeRep | (index, runtimeRep) <- zip [0 ..] resultReps]

primitiveIntValue :: Integer -> GrinValue
primitiveIntValue = GrinLitValue . GrinLitInt IntRep

primitiveWordValue :: Integer -> GrinValue
primitiveWordValue = GrinLitValue . GrinLitInt WordRep

testMVarPrimitives :: IO ()
testMVarPrimitives =
  case toCpsGrin mvarProgram of
    Left err -> assertFailure (show err)
    Right cps ->
      case compileModule (lowerGc cps) of
        Left err -> assertFailure (show err)
        Right source ->
          mapM_
            (\operation -> assertBool ("calls " <> T.unpack operation) (("call\t" <> operation) `T.isInfixOf` source))
            [ "aihc_wasm_transfer_new_mvar",
              "aihc_wasm_transfer_put_mvar",
              "aihc_wasm_transfer_read_mvar",
              "aihc_wasm_transfer_take_mvar"
            ]

testForeignInt :: IO ()
testForeignInt =
  case toCpsGrin foreignIntProgram of
    Left err -> assertFailure (show err)
    Right cps ->
      case compileModule (lowerGc cps) of
        Left err -> assertFailure (show err)
        Right source -> do
          assertBool "declares the Int ABI as i64" (".functype\taihc_io_take_result (i32) -> (i64)" `T.isInfixOf` source)
          assertBool "passes the address and keeps the i64 result" ("i32.wrap_i64\n\tcall\taihc_io_take_result\n\tlocal.set" `T.isInfixOf` source)

testIntPrimitives :: IO ()
testIntPrimitives =
  case toCpsGrin intPrimitiveProgram of
    Left err -> assertFailure (show err)
    Right cps ->
      case compileModule (lowerGc cps) of
        Left err -> assertFailure (show err)
        Right source ->
          mapM_
            (\instruction -> assertBool ("emits " <> T.unpack instruction) (instruction `T.isInfixOf` source))
            ["i64.sub", "i64.mul", "i64.lt_s", "i64.eq", "i64.gt_s", "i32.sub", "i64.extend_i32_s"]

program :: GrinProgram
program =
  GrinProgram
    { grinConstructors = [],
      grinPrimitives = [],
      grinForeignCalls = [],
      grinGlobals =
        [ ( "main",
            GrinNode (GrinClosure mainFunction [[]]) []
          )
        ],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = mainFunction,
              grinFunctionParameters = [],
              grinFunctionResultRep = IntRep,
              grinFunctionBody = GrinConstant [GrinLitValue (GrinLitInt IntRep 42)]
            }
        ]
    }

mainFunction :: FunctionName
mainFunction = FunctionName "$main"

exceptionProgram :: GrinProgram
exceptionProgram =
  GrinProgram
    { grinConstructors = [("Exception", [])],
      grinPrimitives = [],
      grinForeignCalls = [],
      grinGlobals =
        [ (grinVarName exceptionMainClosure, GrinNode (GrinClosure exceptionMainFunction [[]]) []),
          (grinVarName exceptionActionClosure, GrinNode (GrinClosure exceptionActionFunction [[]]) []),
          (grinVarName exceptionHandlerClosure, GrinNode (GrinClosure exceptionHandlerFunction [[lifted]]) [])
        ],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = exceptionMainFunction,
              grinFunctionParameters = [],
              grinFunctionResultRep = lifted,
              grinFunctionBody =
                GrinCatch
                  lifted
                  (GrinVarValue exceptionActionClosure)
                  (GrinVarValue exceptionHandlerClosure)
                  []
            },
          GrinFunction
            { grinFunctionName = exceptionActionFunction,
              grinFunctionParameters = [],
              grinFunctionResultRep = lifted,
              grinFunctionBody =
                GrinBind
                  [exceptionValue]
                  (GrinStore (GrinNode (GrinConstructor "Exception" 0) []))
                  (GrinThrow (GrinVarValue exceptionValue))
            },
          GrinFunction
            { grinFunctionName = exceptionHandlerFunction,
              grinFunctionParameters = [caughtException],
              grinFunctionResultRep = lifted,
              grinFunctionBody = GrinConstant [GrinVarValue caughtException]
            }
        ]
    }
  where
    lifted = BoxedRep Lifted
    exceptionMainFunction = FunctionName "$wasm_exception_main"
    exceptionActionFunction = FunctionName "$wasm_exception_action"
    exceptionHandlerFunction = FunctionName "$wasm_exception_handler"
    exceptionMainClosure = GrinVar "main" 200 lifted
    exceptionActionClosure = GrinVar "wasm_exception_action" 201 lifted
    exceptionHandlerClosure = GrinVar "wasm_exception_handler" 202 lifted
    exceptionValue = GrinVar "wasm_exception" 203 lifted
    caughtException = GrinVar "wasm_caught_exception" 204 lifted

dependencyProgram :: GrinProgram
dependencyProgram =
  program
    { grinGlobals =
        [ ( "dependency",
            GrinNode (GrinClosure dependencyFunction [[]]) []
          )
        ],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = dependencyFunction,
              grinFunctionParameters = [],
              grinFunctionResultRep = IntRep,
              grinFunctionBody = GrinConstant [GrinLitValue (GrinLitInt IntRep 7)]
            }
        ]
    }

dependencyFunction :: FunctionName
dependencyFunction = FunctionName "$dependency"

capturedThunkProgram :: GrinProgram
capturedThunkProgram =
  GrinProgram
    { grinConstructors = [],
      grinPrimitives = [],
      grinForeignCalls = [],
      grinGlobals =
        [ ( "thunk",
            GrinNode (GrinThunk thunkFunction) [GrinLitValue (GrinLitInt IntRep 41)]
          )
        ],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = thunkFunction,
              grinFunctionParameters = [captured],
              grinFunctionResultRep = IntRep,
              grinFunctionBody = GrinConstant [GrinVarValue captured]
            }
        ]
    }
  where
    thunkFunction = FunctionName "$captured_thunk"
    captured = GrinVar "captured" 4 IntRep

directCallProgram :: GrinProgram
directCallProgram =
  GrinProgram
    { grinConstructors = [],
      grinPrimitives = [],
      grinForeignCalls = [],
      grinGlobals = [],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = FunctionName "$caller",
              grinFunctionParameters = [argument],
              grinFunctionResultRep = BoxedRep Lifted,
              grinFunctionBody = GrinCall (BoxedRep Lifted) identityFunction [GrinVarValue argument]
            },
          GrinFunction
            { grinFunctionName = identityFunction,
              grinFunctionParameters = [identityArgument],
              grinFunctionResultRep = BoxedRep Lifted,
              grinFunctionBody = GrinConstant [GrinVarValue identityArgument]
            }
        ]
    }
  where
    argument = GrinVar "argument" 50 (BoxedRep Lifted)
    identityArgument = GrinVar "identity_argument" 51 (BoxedRep Lifted)
    identityFunction = FunctionName "$identity"

repeatedParameterProgram :: GrinProgram
repeatedParameterProgram =
  GrinProgram
    { grinConstructors = [],
      grinPrimitives = [(GrinVar "+#" 51 IntRep, 2)],
      grinForeignCalls = [],
      grinGlobals = [],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = FunctionName "$repeated_parameters",
              grinFunctionParameters = [argument, argument],
              grinFunctionResultRep = IntRep,
              grinFunctionBody =
                GrinBind
                  [result]
                  (GrinPrimitiveCall IntRep "+#" [GrinVarValue argument, GrinVarValue argument])
                  (GrinConstant [GrinVarValue result])
            }
        ]
    }
  where
    argument = GrinVar "argument" 52 IntRep
    result = GrinVar "result" 53 IntRep

gcRootProgram :: GrinProgram
gcRootProgram =
  GrinProgram
    { grinConstructors = [("Box", [[BoxedRep Lifted]])],
      grinPrimitives = [],
      grinForeignCalls = [],
      grinGlobals = [],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = FunctionName "$allocate_box",
              grinFunctionParameters = [root],
              grinFunctionResultRep = BoxedRep Lifted,
              grinFunctionBody = GrinStore (GrinNode (GrinConstructor "Box" 0) [GrinVarValue root])
            }
        ]
    }
  where
    root = GrinVar "root" 55 (BoxedRep Lifted)

literalCaseProgram :: GrinProgram
literalCaseProgram =
  GrinProgram
    { grinConstructors = [],
      grinPrimitives = [],
      grinForeignCalls = [],
      grinGlobals = [],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = FunctionName "$literal_case",
              grinFunctionParameters = [input],
              grinFunctionResultRep = IntRep,
              grinFunctionBody =
                GrinCase
                  (GrinVarValue input)
                  binder
                  [ GrinAlt (GrinLitAlt (GrinLitInt IntRep 7)) [] (GrinConstant [GrinLitValue (GrinLitInt IntRep 1)]),
                    GrinAlt GrinDefaultAlt [] (GrinConstant [GrinLitValue (GrinLitInt IntRep 0)])
                  ]
            }
        ]
    }
  where
    input = GrinVar "input" 70 IntRep
    binder = GrinVar "binder" 71 IntRep

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
      grinGlobals = [],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = FunctionName "$byte_array",
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

foreignIntProgram :: GrinProgram
foreignIntProgram =
  GrinProgram
    { grinConstructors = [],
      grinPrimitives = [],
      grinForeignCalls = [takeResultCall],
      grinGlobals = [],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = FunctionName "$foreign_int",
              grinFunctionParameters = [request],
              grinFunctionResultRep = IntRep,
              grinFunctionBody = GrinForeignCallExpr takeResultCall [GrinVarValue request]
            }
        ]
    }
  where
    request = GrinVar "request" 80 AddrRep
    takeResultCall =
      GrinForeignCall
        { grinForeignCallName = "$ffi$takeResult",
          grinForeignCallSymbol = "aihc_io_take_result",
          grinForeignCallTarget = GrinForeignFunction,
          grinForeignCallSignature = GrinForeignSignature [GrinForeignAddr] GrinForeignInt GrinForeignRealWorld
        }

mvarProgram :: GrinProgram
mvarProgram =
  GrinProgram
    { grinConstructors = [],
      grinPrimitives =
        [ (GrinVar "newMVar#" 70 (BoxedRep Unlifted), 1),
          (GrinVar "putMVar#" 71 (TupleRep []), 3),
          (GrinVar "readMVar#" 72 (BoxedRep Lifted), 2),
          (GrinVar "takeMVar#" 73 (BoxedRep Lifted), 2)
        ],
      grinForeignCalls = [],
      grinGlobals = [],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = FunctionName "$mvars",
              grinFunctionParameters = [],
              grinFunctionResultRep = BoxedRep Lifted,
              grinFunctionBody =
                GrinBind [mvar] (GrinPrimitiveCall (BoxedRep Unlifted) "newMVar#" []) $
                  GrinBind [] (GrinPrimitiveCall (TupleRep []) "putMVar#" [GrinVarValue mvar, value]) $
                    GrinBind [readValue] (GrinPrimitiveCall (BoxedRep Lifted) "readMVar#" [GrinVarValue mvar]) $
                      GrinBind [takenValue] (GrinPrimitiveCall (BoxedRep Lifted) "takeMVar#" [GrinVarValue mvar]) $
                        GrinConstant [GrinVarValue takenValue]
            }
        ]
    }
  where
    mvar = GrinVar "mvar" 74 (BoxedRep Unlifted)
    readValue = GrinVar "read" 75 (BoxedRep Lifted)
    takenValue = GrinVar "taken" 76 (BoxedRep Lifted)
    value = GrinLitValue (GrinLitString "value")

intPrimitiveProgram :: GrinProgram
intPrimitiveProgram =
  GrinProgram
    { grinConstructors = [],
      grinPrimitives = [(GrinVar name unique IntRep, 2) | (name, unique) <- zip primitiveNames [90 ..]],
      grinForeignCalls = [],
      grinGlobals = [],
      grinFunctions = zipWith primitiveFunction primitiveNames [100 ..]
    }
  where
    primitiveNames = ["-#", "*#", "<#", "==#", "compareInt#"]
    primitiveFunction name unique =
      GrinFunction
        { grinFunctionName = FunctionName ("$" <> name),
          grinFunctionParameters = [left unique, right unique],
          grinFunctionResultRep = IntRep,
          grinFunctionBody = GrinPrimitiveCall IntRep name [GrinVarValue (left unique), GrinVarValue (right unique)]
        }
    left unique = GrinVar "left" (unique * 2) IntRep
    right unique = GrinVar "right" (unique * 2 + 1) IntRep

dormantPrimitiveProgram :: GrinProgram
dormantPrimitiveProgram =
  GrinProgram
    { grinConstructors = [],
      grinPrimitives = [(GrinVar "unsupported#" 40 IntRep, 1)],
      grinForeignCalls = [],
      grinGlobals = [],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = FunctionName "$dormant_unsupported",
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
