{-# LANGUAGE OverloadedStrings #-}

module Test.Llvm.Suite (tests) where

import Aihc.Grin (GcGrinProgram, lintProgram, lowerGc, toCpsGrin)
import Aihc.Grin.Gc (gcGrinProgram)
import Aihc.Grin.Syntax
import Aihc.Llvm (compileModule, compileProgram, validatePrimitiveNames)
import Aihc.Native
  ( NativeTarget (Llvm),
    RuntimeGarbageCollector (RuntimeGcCalloc),
    RuntimePlan (..),
    buildLinkLayout,
    runtimePlan,
    supportedNativePrimitiveNames,
  )
import Aihc.Tc.Types (Levity (..), RuntimeRep (..))
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
      testCase "executes cooperative scheduling" (testProgram "PCAB" schedulerProgram),
      testCase "executes synchronous exception unwinding" (testProgram "E" synchronousExceptionProgram),
      testCase "exits directly with an unboxed process status" testProcessExit
    ]

testGuaranteedTailCalls :: IO ()
testGuaranteedTailCalls = do
  source <- compile schedulerProgram
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
  verifyModule source

testByteArrayPrimitives :: IO ()
testByteArrayPrimitives = do
  source <- compile stdioSchedulerProgram
  forM_
    [ "declare ptr @aihc_byte_array_new_pinned(i64)",
      "declare ptr @aihc_byte_array_contents(ptr)",
      "call ptr @aihc_byte_array_new_pinned",
      "call ptr @aihc_byte_array_contents"
    ]
    (\needle -> assertBool ("missing generated LLVM fragment: " <> T.unpack needle) (needle `T.isInfixOf` source))
  verifyModule source

testIntegerPrimitives :: IO ()
testIntegerPrimitives = forM_ integerPrimitiveCases $ \primitiveCase -> do
  let program = primitiveProgram primitiveCase
  case toCpsGrin program of
    Left err -> assertFailure (T.unpack (primitiveCaseName primitiveCase) <> ": " <> show err)
    Right cps ->
      case compileModule (buildLinkLayout [program]) "aihc_init_integer_primitive" (lowerGc cps) of
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
    primitiveCaseResults :: ![RuntimeRep],
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
      grinExternalGlobals = [],
      grinExternalFunctions = [],
      grinWhnfGlobals = [],
      grinCafs = [],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = FunctionName ("$primitive_" <> name),
              grinFunctionLinkName = Just ("primitive." <> name),
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
      grinExternalGlobals = [],
      grinExternalFunctions = [],
      grinWhnfGlobals = [(mainClosure, GrinNode (GrinClosure mainFunction [[]]) [])],
      grinCafs = [],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = mainFunction,
              grinFunctionLinkName = Nothing,
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
              GrinConstant [GrinVarValue unitValue]
        }

firstMatchCaseProgram :: GrinProgram
firstMatchCaseProgram =
  GrinProgram
    { grinConstructors = [("()", [])],
      grinPrimitives = [],
      grinForeignCalls = [putcharCall],
      grinExternalGlobals = [],
      grinExternalFunctions = [],
      grinWhnfGlobals = [(mainClosure, GrinNode (GrinClosure mainFunction [[]]) [])],
      grinCafs = [],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = mainFunction,
              grinFunctionLinkName = Nothing,
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
              GrinConstant [GrinVarValue unitValue]
        }

thunkEntryProgram :: GrinProgram
thunkEntryProgram =
  GrinProgram
    { grinConstructors = [("()", [])],
      grinPrimitives = [],
      grinForeignCalls = [putcharCall],
      grinExternalGlobals = [],
      grinExternalFunctions = [],
      grinWhnfGlobals = [],
      grinCafs = [(mainThunk, GrinNode (GrinThunk mainThunkFunction) [])],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = mainThunkFunction,
              grinFunctionLinkName = Nothing,
              grinFunctionParameters = [],
              grinFunctionResultRep = lifted,
              grinFunctionBody = GrinStore (GrinNode (GrinClosure mainActionFunction [[]]) [])
            },
          GrinFunction
            { grinFunctionName = mainActionFunction,
              grinFunctionLinkName = Nothing,
              grinFunctionParameters = [],
              grinFunctionResultRep = lifted,
              grinFunctionBody =
                GrinBind
                  [output]
                  (GrinForeignCallExpr putcharCall [GrinLitValue (GrinLitInt Int32Rep (toInteger (fromEnum 'T')))])
                  (GrinConstant [GrinVarValue unitValue])
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
      grinExternalGlobals = [],
      grinExternalFunctions = [],
      grinWhnfGlobals = [(mainClosure, GrinNode (GrinClosure mainFunction [[]]) [])],
      grinCafs = [],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = mainFunction,
              grinFunctionLinkName = Nothing,
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
              GrinConstant [GrinVarValue unitValue]
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
  source <- compile processExitProgram
  assertBool
    "LLVM exit lowering must call the non-returning host operation"
    ("call void @aihc_exit_process(i64 7)" `T.isInfixOf` source)
  assertBool
    "LLVM main must not recover an exit status after the tail-call chain"
    (not ("call i64 @aihc_get_exit_status" `T.isInfixOf` source))
  withTempDirectory "aihc-llvm-exit" $ \directory -> do
    runtimeArguments <- llvmRuntimeArguments
    let sourcePath = directory </> "program.ll"
        executablePath = directory </> "program"
    TIO.writeFile sourcePath source
    (clangExit, _clangOut, clangErr) <-
      readProcessWithExitCode
        "clang"
        ( ["-std=c11", "-Wall", "-Wextra", "-Werror", "-Wno-override-module", "-O2"]
            <> runtimeArguments
            <> [sourcePath, "-o", executablePath]
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
      grinExternalGlobals = [],
      grinExternalFunctions = [],
      grinWhnfGlobals = [(mainClosure, GrinNode (GrinClosure mainFunction [[]]) [])],
      grinCafs = [],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = mainFunction,
              grinFunctionLinkName = Nothing,
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
testProgram expected program = do
  source <- compile program
  withTempDirectory "aihc-llvm" $ \directory -> do
    runtimeArguments <- llvmRuntimeArguments
    let sourcePath = directory </> "program.ll"
        executablePath = directory </> "program"
    TIO.writeFile sourcePath source
    (clangExit, _clangOut, clangErr) <-
      readProcessWithExitCode
        "clang"
        ( ["-std=c11", "-Wall", "-Wextra", "-Werror", "-Wno-override-module", "-O2"]
            <> runtimeArguments
            <> [sourcePath, "-o", executablePath]
        )
        ""
    assertEqual ("clang rejected generated LLVM IR:\n" <> clangErr) ExitSuccess clangExit
    (programExit, programOut, programErr) <- readProcessWithExitCode executablePath [] ""
    assertEqual ("generated program stderr: " <> programErr) ExitSuccess programExit
    assertEqual "generated program stdout" expected programOut

llvmRuntimeArguments :: IO [String]
llvmRuntimeArguments = do
  plan <- runtimePlan Llvm RuntimeGcCalloc
  pure
    ( ["-I" <> directory | directory <- runtimeIncludeDirectories plan]
        <> runtimeSources plan
    )

compile :: GrinProgram -> IO T.Text
compile program = do
  assertEqual "direct GRIN lint" [] (lintProgram program)
  let gcProgram = expectGcGrin program
  assertEqual "GC-GRIN lint" [] (lintProgram (gcGrinProgram gcProgram))
  case compileProgram "main" gcProgram of
    Right source -> pure source
    Left err -> assertFailure ("LLVM lowering failed: " <> show err)

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
