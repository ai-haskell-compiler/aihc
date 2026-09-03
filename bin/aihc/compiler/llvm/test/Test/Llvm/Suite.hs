{-# LANGUAGE OverloadedStrings #-}

module Test.Llvm.Suite (tests) where

import Aihc.Grin (GcGrinProgram, lintProgram, lowerGc, toCpsGrin)
import Aihc.Grin.Gc (gcGrinProgram)
import Aihc.Grin.Syntax
import Aihc.Llvm (compileEntry, compileModule, validatePrimitiveNames)
import Aihc.Native
  ( NativeTarget (Llvm),
    RuntimeGarbageCollector (..),
    RuntimePlan (..),
    executableEntryName,
    runtimePlan,
    supportedNativePrimitiveNames,
  )
import Aihc.Testing.ExceptionProgram (synchronousExceptionProgram)
import Aihc.Testing.SchedulerProgram (schedulerProgram, stdioSchedulerProgram)
import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (createDirectory, getTemporaryDirectory, removeDirectoryRecursive, removeFile)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "LLVM backend"
    [ testCase "emits verified guaranteed tail calls" testGuaranteedTailCalls,
      testCase "accepts every shared native primitive" $
        assertEqual "LLVM primitive coverage" (Right ()) (validatePrimitiveNames supportedNativePrimitiveNames),
      testCase "lowers integer and word primitives without a fallback" testIntegerPrimitives,
      testCase "lowers byte-array primitives" testByteArrayPrimitives,
      testCase "executes platform Int foreign calls" (testProgram "L" foreignIntProgram),
      testCase "executes Int# addition" (testProgram "*" intAddProgram),
      testCase "preserves first-match case semantics" (testProgram "F" firstMatchCaseProgram),
      testCase "executes thunk entry updates" (testProgram "T" thunkEntryProgram),
      testCase "keeps static root entries in llvm.used" testStaticRootEntries,
      testCase "emits static reference tables and publishes them on entry" testStaticReferenceTables,
      testCase "traces an updated static thunk across collections" (testProgramWith RuntimeGcSemispace ["-DAIHC_SEMISPACE_BYTES=1024"] "S" staticRootProgram),
      testCase "executes cooperative scheduling" (testProgram "PCAB" schedulerProgram),
      testCase "executes synchronous exception unwinding" (testProgram "E" synchronousExceptionProgram),
      testCase "exits directly with an unboxed process status" testProcessExit
    ]

testGuaranteedTailCalls :: IO ()
testGuaranteedTailCalls = do
  sources <- compile schedulerProgram
  let source = T.unlines sources
  forM_
    [ "define internal tailcc void",
      "define i32 @main(i32 %argc, ptr %argv)",
      "call void @aihc_program_arguments_initialize(i32 %argc, ptr %argv)",
      "musttail call tailcc void",
      "ptr %closure, ptr %continuation",
      "getelementptr i64, ptr %closure",
      "define internal tailcc void @aihc_llvm_apply_1",
      "@aihc_llvm_resume"
    ]
    (\needle -> assertBool ("missing generated LLVM fragment: " <> T.unpack needle) (needle `T.isInfixOf` source))
  assertBool "LLVM backend does not use a global argument buffer" (not ("aihc_llvm_arguments" `T.isInfixOf` source))
  mapM_ verifyModule sources

testByteArrayPrimitives :: IO ()
testByteArrayPrimitives = do
  sources <- compile stdioSchedulerProgram
  let source = T.unlines sources
  forM_
    [ "declare ptr @aihc_byte_array_new_pinned(i64)",
      "declare ptr @aihc_byte_array_contents(ptr)",
      "call ptr @aihc_byte_array_new_pinned",
      "call ptr @aihc_byte_array_contents"
    ]
    (\needle -> assertBool ("missing generated LLVM fragment: " <> T.unpack needle) (needle `T.isInfixOf` source))
  mapM_ verifyModule sources

testIntegerPrimitives :: IO ()
testIntegerPrimitives = forM_ integerPrimitiveCases $ \primitiveCase -> do
  let program = primitiveProgram primitiveCase
  case toCpsGrin program of
    Left err -> assertFailure (T.unpack (primitiveCaseName primitiveCase) <> ": " <> show err)
    Right cps ->
      case compileModule (lowerGc cps) of
        Left err -> assertFailure (T.unpack (primitiveCaseName primitiveCase) <> ": " <> show err)
        Right source -> do
          assertBool
            (T.unpack (primitiveCaseName primitiveCase) <> " uses the unsupported-primitive fallback")
            (not ("call void @aihc_unsupported_primitive" `T.isInfixOf` source))
          forM_ (primitiveCaseInstructions primitiveCase) $ \instruction ->
            assertBool
              (T.unpack (primitiveCaseName primitiveCase) <> " does not emit " <> T.unpack instruction)
              (instruction `T.isInfixOf` source)
          verifyModule source

data PrimitiveCase = PrimitiveCase
  { primitiveCaseName :: !T.Text,
    primitiveCaseArguments :: ![GrinValue],
    primitiveCaseResults :: ![GrinRep],
    primitiveCaseInstructions :: ![T.Text]
  }

integerPrimitiveCases :: [PrimitiveCase]
integerPrimitiveCases =
  [ intBinary "+#" " = add i64",
    intBinary "-#" " = sub i64",
    intBinary "*#" " = mul i64",
    intComparison "<#" "icmp slt i64",
    intComparison "==#" "icmp eq i64",
    intComparison "compareInt#" "icmp sgt i64",
    intCarry "addIntC#" " = add i64",
    intCarry "subIntC#" " = sub i64",
    wordBinary "plusWord#" " = add i64",
    wordBinary "minusWord#" " = sub i64",
    wordBinary "timesWord#" " = mul i64",
    wordCarry "addWordC#" " = add i64",
    wordCarry "subWordC#" " = sub i64",
    PrimitiveCase "timesWord2#" wordArguments [WordRep, WordRep] [" = mul i128", " = lshr i128"],
    wordBinary "quotWord#" " = udiv i64",
    wordBinary "remWord#" " = urem i64",
    PrimitiveCase "quotRemWord#" wordArguments [WordRep, WordRep] [" = udiv i64", " = urem i64"],
    PrimitiveCase "quotRemWord2#" [wordValue 1, wordValue 2, wordValue 3] [WordRep, WordRep] [" = udiv i128", " = urem i128"],
    wordBinary "and#" " = and i64",
    wordBinary "or#" " = or i64",
    wordBinary "xor#" " = xor i64",
    PrimitiveCase "not#" [wordValue 1] [WordRep] [" = xor i64"],
    PrimitiveCase "uncheckedShiftL#" [wordValue 1, intValue 2] [WordRep] [" = shl i64"],
    PrimitiveCase "uncheckedShiftRL#" [wordValue 1, intValue 2] [WordRep] [" = lshr i64"],
    PrimitiveCase "int2Word#" [intValue 1] [WordRep] [],
    PrimitiveCase "word2Int#" [wordValue 1] [IntRep] [],
    PrimitiveCase "ord#" [charValue 'a'] [IntRep] [],
    PrimitiveCase "chr#" [intValue 97] [WordRep] [],
    wordComparison "eqWord#" "icmp eq i64",
    wordComparison "neWord#" "icmp ne i64",
    wordComparison "ltWord#" "icmp ult i64",
    wordComparison "leWord#" "icmp ule i64",
    wordComparison "gtWord#" "icmp ugt i64",
    wordComparison "geWord#" "icmp uge i64",
    PrimitiveCase "clz#" [wordValue 1] [WordRep] ["call i64 @aihc_word_clz"],
    PrimitiveCase "ctz#" [wordValue 1] [WordRep] ["call i64 @aihc_word_ctz"],
    PrimitiveCase "popCnt#" [wordValue 1] [WordRep] ["call i64 @aihc_word_popcount"]
  ]
  where
    intBinary name instruction = PrimitiveCase name intArguments [IntRep] [instruction]
    intComparison name instruction = PrimitiveCase name intArguments [IntRep] [instruction]
    intCarry name instruction = PrimitiveCase name intArguments [IntRep, IntRep] [instruction, " = lshr i64"]
    wordBinary name instruction = PrimitiveCase name wordArguments [WordRep] [instruction]
    wordCarry name instruction = PrimitiveCase name wordArguments [WordRep, IntRep] [instruction, "icmp ult i64"]
    wordComparison name instruction = PrimitiveCase name wordArguments [IntRep] [instruction]
    intArguments = [intValue 7, intValue 3]
    wordArguments = [wordValue 7, wordValue 3]

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

intValue :: Integer -> GrinValue
intValue = GrinLitValue . GrinLitInt IntRep

wordValue :: Integer -> GrinValue
wordValue = GrinLitValue . GrinLitInt WordRep

charValue :: Char -> GrinValue
charValue = GrinLitValue . GrinLitChar WordRep

verifyModule :: T.Text -> IO ()
verifyModule source =
  withTempDirectory "aihc-llvm-verify" $ \directory -> do
    let sourcePath = directory </> "program.ll"
        objectPath = directory </> "program.o"
    TIO.writeFile sourcePath source
    (clangExit, _clangOut, clangErr) <-
      readProcessWithExitCode
        "clang"
        ["-Wno-override-module", "-c", sourcePath, "-o", objectPath]
        ""
    assertEqual ("clang rejected generated LLVM IR:\n" <> clangErr) ExitSuccess clangExit

intAddProgram :: GrinProgram
intAddProgram =
  GrinProgram
    { grinConstructors = [("()", [])],
      grinPrimitives = [(GrinVar "+#" 30 IntRep, 2)],
      grinForeignCalls = [putcharCall],
      grinGlobals = [(grinVarName mainClosure, GrinNode (GrinClosure mainFunction [[]]) [])],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = mainFunction,
              grinFunctionParameters = [],
              grinFunctionResultRep = lifted,
              grinFunctionBody =
                GrinBind [sumValue] (GrinPrimitiveCall IntRep "+#" [intLiteral 40, intLiteral 2]) $
                  GrinCase
                    (GrinVarValue sumValue)
                    caseBinder
                    [ outputAlternative (GrinLitAlt (GrinLitInt IntRep 42)) '*' successOutput,
                      outputAlternative GrinDefaultAlt '?' failureOutput
                    ]
            }
        ]
    }
  where
    lifted = BoxedRep Lifted
    mainFunction = FunctionName "$int_add_main"
    mainClosure = GrinVar "main" 31 lifted
    sumValue = GrinVar "sum" 32 IntRep
    caseBinder = GrinVar "case_binder" 33 IntRep
    successOutput = GrinVar "success_output" 34 Int32Rep
    failureOutput = GrinVar "failure_output" 35 Int32Rep
    unitValue = GrinVar "()" 36 lifted
    intLiteral = GrinLitValue . GrinLitInt IntRep
    outputAlternative constructor character output =
      GrinAlt
        { grinAltCon = constructor,
          grinAltBinders = [],
          grinAltRhs =
            GrinBind [output] (GrinForeignCallExpr putcharCall [GrinLitValue (GrinLitInt Int32Rep (toInteger (fromEnum character)))]) $
              GrinConstant [GrinGlobalValue (grinVarName unitValue)]
        }

firstMatchCaseProgram :: GrinProgram
firstMatchCaseProgram =
  GrinProgram
    { grinConstructors = [("()", [])],
      grinPrimitives = [],
      grinForeignCalls = [putcharCall],
      grinGlobals = [(grinVarName mainClosure, GrinNode (GrinClosure mainFunction [[]]) [])],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = mainFunction,
              grinFunctionParameters = [],
              grinFunctionResultRep = lifted,
              grinFunctionBody =
                GrinCase
                  (intLiteral 1)
                  caseBinder
                  [ outputAlternative (GrinLitAlt (GrinLitInt IntRep 1)) 'F' firstOutput,
                    outputAlternative (GrinLitAlt (GrinLitInt IntRep 1)) 'S' secondOutput,
                    outputAlternative GrinDefaultAlt '?' failureOutput
                  ]
            }
        ]
    }
  where
    lifted = BoxedRep Lifted
    mainFunction = FunctionName "$first_match_case_main"
    mainClosure = GrinVar "main" 100 lifted
    caseBinder = GrinVar "case_binder" 101 IntRep
    firstOutput = GrinVar "first_output" 102 Int32Rep
    secondOutput = GrinVar "second_output" 103 Int32Rep
    failureOutput = GrinVar "failure_output" 104 Int32Rep
    unitValue = GrinVar "()" 105 lifted
    intLiteral = GrinLitValue . GrinLitInt IntRep
    outputAlternative constructor character output =
      GrinAlt
        { grinAltCon = constructor,
          grinAltBinders = [],
          grinAltRhs =
            GrinBind [output] (GrinForeignCallExpr putcharCall [GrinLitValue (GrinLitInt Int32Rep (toInteger (fromEnum character)))]) $
              GrinConstant [GrinGlobalValue (grinVarName unitValue)]
        }

thunkEntryProgram :: GrinProgram
thunkEntryProgram =
  GrinProgram
    { grinConstructors = [("()", [])],
      grinPrimitives = [],
      grinForeignCalls = [putcharCall],
      grinGlobals = [(grinVarName mainThunk, GrinNode (GrinThunk mainThunkFunction) [])],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = mainThunkFunction,
              grinFunctionParameters = [],
              grinFunctionResultRep = lifted,
              grinFunctionBody = GrinStore (GrinNode (GrinClosure mainActionFunction [[]]) [])
            },
          GrinFunction
            { grinFunctionName = mainActionFunction,
              grinFunctionParameters = [],
              grinFunctionResultRep = lifted,
              grinFunctionBody =
                GrinBind
                  [output]
                  (GrinForeignCallExpr putcharCall [GrinLitValue (GrinLitInt Int32Rep (toInteger (fromEnum 'T')))])
                  (GrinConstant [GrinGlobalValue (grinVarName unitValue)])
            }
        ]
    }
  where
    lifted = BoxedRep Lifted
    mainThunkFunction = FunctionName "$thunk_entry_main_thunk"
    mainActionFunction = FunctionName "$thunk_entry_main_action"
    mainThunk = GrinVar "main" 90 lifted
    output = GrinVar "output" 91 Int32Rep
    unitValue = GrinVar "()" 92 lifted

-- | Root entries are private constants that no code references. Without
-- @llvm.used@, optimization removes them and the collector gets an empty root
-- section.
testStaticRootEntries :: IO ()
testStaticRootEntries = do
  sources <- compile thunkEntryProgram
  case sources of
    [moduleSource, _entrySource] -> do
      let usedLines = filter ("@llvm.used = appending global" `T.isPrefixOf`) (T.lines moduleSource)
      assertEqual "one llvm.used declaration" 1 (length usedLines)
      assertBool "llvm.used lists the main root" (any ("_root" `T.isInfixOf`) usedLines)
      assertBool "llvm.used lives in llvm.metadata" (any ("section \"llvm.metadata\"" `T.isInfixOf`) usedLines)
    _ -> assertFailure "LLVM compilation did not return two units"

-- | Every compiled function publishes its own table on entry, the tables name
-- the static objects their code reaches, and the info tables of a function's
-- closures point at the same record.
testStaticReferenceTables :: IO ()
testStaticReferenceTables = do
  sources <- compile staticRootProgram
  case sources of
    [moduleSource, _entrySource] -> do
      let sourceLines = T.lines moduleSource
          tableLines = filter ("@aihc_llvm_srt_" `T.isPrefixOf`) sourceLines
          storeLines = filter (", ptr @aihc_current_srt, align 8" `T.isSuffixOf`) sourceLines
          definitionLines =
            [ line
            | line <- sourceLines,
              "define " `T.isPrefixOf` line,
              "@aihc_llvm_function_" `T.isInfixOf` line,
              not ("_info_" `T.isInfixOf` line)
            ]
      assertBool "emits at least one table" (not (null tableLines))
      assertBool
        "a table names the static thunk"
        (any (("ptrtoint (ptr @" <> cafSymbol <> " to i64)") `T.isInfixOf`) tableLines)
      assertBool
        "a table names another table as its child"
        (any (\line -> "ptrtoint (ptr @aihc_llvm_srt_" `T.isInfixOf` T.drop 1 (snd (T.breakOn "[" line))) tableLines)
      assertBool
        "an info table points at a table"
        (any (\line -> "constant %AihcInfo" `T.isInfixOf` line && ", ptr @aihc_llvm_srt_" `T.isInfixOf` line) sourceLines)
      assertEqual
        "every compiled function publishes a table on entry"
        (length definitionLines)
        (length storeLines)
    _ -> assertFailure "LLVM compilation did not return two units"
  where
    cafSymbol = "aihc_entry_caf"

-- | The static @caf@ thunk is updated with a heap box. The action then
-- allocates enough garbage for several collections and reads the box through
-- the static thunk again. The output is wrong or the program stops when the
-- collector does not trace the static thunk.
staticRootProgram :: GrinProgram
staticRootProgram =
  GrinProgram
    { grinConstructors = [("()", []), ("Box", [[Int32Rep]]), ("Pad", replicate 4 [IntRep])],
      grinPrimitives = [],
      grinForeignCalls = [putcharCall],
      grinGlobals =
        [ (grinVarName mainClosure, GrinNode (GrinClosure mainFunction [[]]) []),
          (grinVarName cafThunk, GrinNode (GrinThunk cafFunction) [])
        ],
      grinFunctions =
        [ function cafFunction $
            GrinStore (GrinNode (GrinConstructor "Box" 0) [GrinLitValue (GrinLitInt Int32Rep (toInteger (fromEnum 'S')))]),
          function mainFunction $
            GrinBind [first] (GrinEval lifted (GrinGlobalValue (grinVarName cafThunk))) $
              GrinCall lifted (chainFunction 0) []
        ]
          <> [ function (chainFunction index) $
                 GrinBind [padding index] (GrinStore (GrinNode (GrinConstructor "Pad" 0) (replicate 4 (intValue 0)))) $
                   GrinCall lifted (chainFunction (index + 1)) []
             | index <- [0 .. chainLength - 1]
             ]
          <> [ function (chainFunction chainLength) $
                 GrinBind [second] (GrinEval lifted (GrinGlobalValue (grinVarName cafThunk))) $
                   GrinCase
                     (GrinVarValue second)
                     caseBinder
                     [ GrinAlt (GrinDataAlt "Box") [character] $
                         GrinBind [output] (GrinForeignCallExpr putcharCall [GrinVarValue character]) $
                           GrinConstant [GrinGlobalValue (grinVarName unitValue)],
                       GrinAlt GrinDefaultAlt [] $
                         GrinBind [output] (GrinForeignCallExpr putcharCall [GrinLitValue (GrinLitInt Int32Rep (toInteger (fromEnum '?')))]) $
                           GrinConstant [GrinGlobalValue (grinVarName unitValue)]
                     ]
             ]
    }
  where
    lifted = BoxedRep Lifted
    chainLength = 100
    function name body =
      GrinFunction
        { grinFunctionName = name,
          grinFunctionParameters = [],
          grinFunctionResultRep = lifted,
          grinFunctionBody = body
        }
    cafFunction = FunctionName "$static_root_caf"
    mainFunction = FunctionName "$static_root_main"
    chainFunction index = FunctionName ("$static_root_chain_" <> T.pack (show (index :: Int)))
    mainClosure = GrinVar "main" 300 lifted
    cafThunk = GrinVar "caf" 301 lifted
    padding index = GrinVar "padding" (500 + index) lifted
    first = GrinVar "first" 700 lifted
    second = GrinVar "second" 701 lifted
    caseBinder = GrinVar "case_binder" 702 lifted
    character = GrinVar "character" 703 Int32Rep
    output = GrinVar "output" 704 Int32Rep
    unitValue = GrinVar "()" 705 lifted

putcharCall :: GrinForeignCall
putcharCall =
  GrinForeignCall
    { grinForeignCallName = "$ffi$putchar",
      grinForeignCallSymbol = "putchar",
      grinForeignCallSignature =
        GrinForeignSignature
          { grinForeignArgumentTypes = [GrinForeignInt32],
            grinForeignResultType = GrinForeignInt32,
            grinForeignEffect = GrinForeignPure
          }
    }

foreignIntProgram :: GrinProgram
foreignIntProgram =
  GrinProgram
    { grinConstructors = [("()", [])],
      grinPrimitives = [],
      grinForeignCalls = [labsCall, putcharCall],
      grinGlobals = [(grinVarName mainClosure, GrinNode (GrinClosure mainFunction [[]]) [])],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = mainFunction,
              grinFunctionParameters = [],
              grinFunctionResultRep = lifted,
              grinFunctionBody =
                GrinBind [absoluteValue] (GrinForeignCallExpr labsCall [intLiteral (-76)]) $
                  GrinCase
                    (GrinVarValue absoluteValue)
                    caseBinder
                    [ outputAlternative (GrinLitAlt (GrinLitInt IntRep 76)) 'L' successOutput,
                      outputAlternative GrinDefaultAlt '?' failureOutput
                    ]
            }
        ]
    }
  where
    lifted = BoxedRep Lifted
    mainFunction = FunctionName "$foreign_int_main"
    mainClosure = GrinVar "main" 43 lifted
    absoluteValue = GrinVar "absolute_value" 44 IntRep
    caseBinder = GrinVar "case_binder" 45 IntRep
    successOutput = GrinVar "success_output" 46 Int32Rep
    failureOutput = GrinVar "failure_output" 47 Int32Rep
    unitValue = GrinVar "()" 48 lifted
    intLiteral = GrinLitValue . GrinLitInt IntRep
    outputAlternative constructor character output =
      GrinAlt
        { grinAltCon = constructor,
          grinAltBinders = [],
          grinAltRhs =
            GrinBind [output] (GrinForeignCallExpr putcharCall [GrinLitValue (GrinLitInt Int32Rep (toInteger (fromEnum character)))]) $
              GrinConstant [GrinGlobalValue (grinVarName unitValue)]
        }

labsCall :: GrinForeignCall
labsCall =
  GrinForeignCall
    { grinForeignCallName = "$ffi$labs",
      grinForeignCallSymbol = "labs",
      grinForeignCallSignature =
        GrinForeignSignature
          { grinForeignArgumentTypes = [GrinForeignInt],
            grinForeignResultType = GrinForeignInt,
            grinForeignEffect = GrinForeignPure
          }
    }

testProcessExit :: IO ()
testProcessExit = do
  sources <- compile processExitProgram
  let source = T.unlines sources
  assertBool
    "LLVM exit lowering must call the non-returning host operation"
    ("call void @aihc_exit_process(i64 7)" `T.isInfixOf` source)
  assertBool
    "LLVM main must not recover an exit status after the tail-call chain"
    (not ("call i64 @aihc_get_exit_status" `T.isInfixOf` source))
  withTempDirectory "aihc-llvm-exit" $ \directory -> do
    runtimeArguments <- llvmRuntimeArguments RuntimeGcCalloc
    let modulePath = directory </> "program.ll"
        entryPath = directory </> "entry.ll"
        executablePath = directory </> "program"
    case sources of
      [moduleSource, entrySource] -> do
        TIO.writeFile modulePath moduleSource
        TIO.writeFile entryPath entrySource
      _ -> assertFailure "LLVM compilation did not return two units"
    (clangExit, _clangOut, clangErr) <-
      readProcessWithExitCode
        "clang"
        ( ["-std=c11", "-Wall", "-Wextra", "-Werror", "-Wno-override-module", "-O2"]
            <> runtimeArguments
            <> [modulePath, entryPath, "-o", executablePath]
        )
        ""
    assertEqual ("clang rejected generated LLVM IR:\n" <> clangErr) ExitSuccess clangExit
    (programExit, programOut, programErr) <- readProcessWithExitCode executablePath [] ""
    assertEqual "generated process exit" (ExitFailure 7) programExit
    assertEqual "generated process stdout" "" programOut
    assertEqual "generated process stderr" "" programErr

processExitProgram :: GrinProgram
processExitProgram =
  GrinProgram
    { grinConstructors = [],
      grinPrimitives = [],
      grinForeignCalls = [],
      grinGlobals = [(grinVarName mainClosure, GrinNode (GrinClosure mainFunction [[]]) [])],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = mainFunction,
              grinFunctionParameters = [],
              grinFunctionResultRep = IntRep,
              grinFunctionBody = GrinExit (intValue 7)
            }
        ]
    }
  where
    mainFunction = FunctionName "$process_exit_main"
    mainClosure = GrinVar "main" 120 (BoxedRep Lifted)

testProgram :: String -> GrinProgram -> IO ()
testProgram = testProgramWith RuntimeGcCalloc []

-- | Compile and run one program against the selected collector. The extra
-- arguments reach the C compiler for the runtime and the generated modules.
testProgramWith :: RuntimeGarbageCollector -> [String] -> String -> GrinProgram -> IO ()
testProgramWith collector extraArguments expected program = do
  sources <- compile program
  withTempDirectory "aihc-llvm" $ \directory -> do
    runtimeArguments <- llvmRuntimeArguments collector
    let modulePath = directory </> "program.ll"
        entryPath = directory </> "entry.ll"
        executablePath = directory </> "program"
    case sources of
      [moduleSource, entrySource] -> do
        TIO.writeFile modulePath moduleSource
        TIO.writeFile entryPath entrySource
      _ -> assertFailure "LLVM compilation did not return two units"
    (clangExit, _clangOut, clangErr) <-
      readProcessWithExitCode
        "clang"
        ( ["-std=c11", "-Wall", "-Wextra", "-Werror", "-Wno-override-module", "-O2"]
            <> extraArguments
            <> runtimeArguments
            <> [modulePath, entryPath, "-o", executablePath]
        )
        ""
    assertEqual ("clang rejected generated LLVM IR:\n" <> clangErr) ExitSuccess clangExit
    (programExit, programOut, programErr) <- readProcessWithExitCode executablePath [] ""
    assertEqual ("generated program stderr: " <> programErr) ExitSuccess programExit
    assertEqual "generated program stdout" expected programOut

llvmRuntimeArguments :: RuntimeGarbageCollector -> IO [String]
llvmRuntimeArguments collector = do
  plan <- runtimePlan Llvm collector
  pure
    ( ["-I" <> directory | directory <- runtimeIncludeDirectories plan]
        <> runtimeSources plan
    )

compile :: GrinProgram -> IO [T.Text]
compile program = do
  assertEqual "direct GRIN lint" [] (lintProgram program)
  let linkedProgram =
        program
          { grinGlobals =
              [ (if name == "main" then executableEntryName else name, node)
              | (name, node) <- grinGlobals program
              ]
          }
      gcProgram = expectGcGrin linkedProgram
  assertEqual "GC-GRIN lint" [] (lintProgram (gcGrinProgram gcProgram))
  moduleSource <- either (assertFailure . show) pure (compileModule gcProgram)
  entrySource <- either (assertFailure . show) pure compileEntry
  pure [moduleSource, entrySource]

expectGcGrin :: GrinProgram -> GcGrinProgram
expectGcGrin program =
  case toCpsGrin program of
    Right cpsProgram -> lowerGc cpsProgram
    Left err -> error ("test GRIN failed CPS conversion: " <> show err)

withTempDirectory :: String -> (FilePath -> IO value) -> IO value
withTempDirectory template = bracket acquire removeDirectoryRecursive
  where
    acquire = do
      temporary <- getTemporaryDirectory
      (path, handle) <- openTempFile temporary template
      hClose handle
      removeFile path
      createDirectory path
      pure path
