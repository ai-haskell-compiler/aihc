{-# LANGUAGE OverloadedStrings #-}

-- | The Lir pipeline on AArch64: the Lir evaluation fixtures, the GRIN heap
-- snapshot fixtures lowered through Lir, and the scheduler programs linked
-- with the C runtime.
module Test.Arm64.LirSuite
  ( tests,
  )
where

import Aihc.Arm64.Lir (compileLirObject)
import Aihc.Grin hiding (renderParseError)
import Aihc.Grin qualified as Grin
import Aihc.Lir
import Aihc.Lir.Lower (lowerEntry, lowerModule)
import Aihc.Native
  ( NativeTarget (AppleArm64),
    RuntimeGarbageCollector (..),
    RuntimePlan (..),
    executableEntryName,
    runtimePlan,
  )
import Aihc.Testing.ExceptionProgram (synchronousExceptionProgram)
import Aihc.Testing.SchedulerProgram (blackholeSchedulerProgram, schedulerProgram, stdioSchedulerProgram)
import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (forM, when)
import Data.Aeson (FromJSON (..), withObject, (.:), (.:?))
import Data.ByteString.Lazy qualified as BL
import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Word (Word64)
import Data.Yaml qualified as Y
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import System.Directory (createDirectory, getTemporaryDirectory, listDirectory, removeDirectoryRecursive, removeFile)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, takeExtension, (</>))
import System.IO (hClose, hFlush, hPutStr, openTempFile)
import System.Info (arch, os)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, readProcessWithExitCode, waitForProcess)
import Test.Arm64.LirObserved (lowerObservedProgram)
import Test.Native.Observed (snapshotSourcePath)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

tests :: IO TestTree
tests = do
  root <- fromMaybe "." <$> lookupEnv "AIHC_TEST_ROOT"
  let directory = root </> "bin" </> "aihc" </> "compiler" </> "lir" </> "test" </> "Test" </> "Fixtures" </> "lir" </> "eval"
      snapshotDirectory = root </> "bin" </> "aihc" </> "compiler" </> "grin" </> "test" </> "Test" </> "Fixtures" </> "grin-snapshot"
  names <- sort . filter ((== ".lir") . takeExtension) <$> listDirectory directory
  snapshots <- sort . filter ((== ".yaml") . takeExtension) <$> listDirectory snapshotDirectory
  pure
    ( testGroup
        "Lir pipeline"
        [ testGroup "Lir evaluation fixtures on AArch64" (map (fixtureTest directory) names),
          testGroup "GRIN heap snapshots through Lir" (map (snapshotTest snapshotDirectory) snapshots),
          testGroup
            "programs through Lir"
            [ testGroup
                (collectorName collector)
                [ testCase "runs fork# and yield# with FIFO scheduling" (programTest collector "PCAB" schedulerProgram),
                  testCase "catches a synchronous exception" (programTest collector "E" synchronousExceptionProgram),
                  testCase "blocks and wakes threads that enter a shared blackhole" (programTest collector "TA" blackholeSchedulerProgram),
                  testCase "waits for stdin and resumes an async stdio continuation" (stdioTest collector)
                ]
            | collector <- [RuntimeGcSemispace]
            ]
        ]
    )

-- | The backend does not check memory alignment or read-only data, so these
-- interpreter traps have no native counterpart.
uncheckedTraps :: [FilePath]
uncheckedTraps = ["trap-misaligned.lir", "trap-read-only.lir"]

nativeHost :: Bool
nativeHost = arch == "aarch64" && os == "darwin"

fixtureTest :: FilePath -> FilePath -> TestTree
fixtureTest directory name = testCase name $ do
  source <- TIO.readFile (directory </> name)
  lirModule <- either (assertFailure . renderParseError) pure (parseModule source)
  let resultTypes = concat [functionResults function | ItemFunction function <- moduleItems lirModule, functionName function == Symbol "main"]
      wrapped = Module (moduleItems lirModule <> [ItemFunction (testWrapper resultTypes)])
  object <- either (assertFailure . show) pure (compileLirObject wrapped)
  when (nativeHost && name `notElem` uncheckedTraps) $ do
    (exit, out, err) <- runObject object
    case (headerValues "expect" source, headerValues "expect-trap" source) of
      ([expected], []) -> do
        assertEqual "exit status" ExitSuccess exit
        words' <- mapM parseWord (lines out)
        let values = zipWith decode resultTypes words'
            actual = T.splitOn ", " (renderValues resultTypes values)
            -- Addresses differ between the interpreter and the native run.
            comparable = [(want, got) | (ty, want, got) <- zip3 resultTypes (T.splitOn ", " expected) actual, ty `notElem` [Ptr, Code]]
        assertEqual "result count" (length resultTypes) (length words')
        assertEqual "results" (map fst comparable) (map snd comparable)
      ([], [expectedTrap]) -> do
        assertBool "trap exit status" (exit /= ExitSuccess)
        assertEqual "trap message" (expectedTrap <> "\n") (T.pack err)
      _ -> assertFailure "fixture has no single expectation"
  where
    parseWord line =
      case reads line of
        [(value, "")] -> pure (value :: Word64)
        _ -> assertFailure ("unexpected driver output: " <> line)

-- | Decode one raw result word with the type of the result.
decode :: Type -> Word64 -> Value
decode ty word =
  case ty of
    F32 -> VF32 (castWord32ToFloat (fromIntegral word))
    F64 -> VF64 (castWord64ToDouble word)
    Ptr -> VPtr word
    Code -> VCode word
    _ -> VInt word

-- | A C function that calls @main@ and stores every result in one word of
-- the output buffer. The driver prints the words.
testWrapper :: [Type] -> Function
testWrapper resultTypes =
  Function
    { functionName = Symbol "aihc_lir_test_main",
      functionLinkage = Export,
      functionParameters = [(Var "out", Ptr)],
      functionResults = [I64],
      functionConvention = CConvention,
      functionBlocks =
        [ Block
            { blockLabel = Label "entry",
              blockParameters = [],
              blockInstructions =
                Instruction results (Call (Symbol "main") [])
                  : [ Instruction [] (Store ty (OperandVar var) (Address (OperandVar (Var "out")) (8 * index)) 1)
                    | (index, var, ty) <- zip3 [0 ..] results resultTypes
                    ],
              blockTerminator = Return [OperandLiteral (LitInt (toInteger (length resultTypes)))]
            }
        ]
    }
  where
    results = [Var ("result" <> T.pack (show index)) | index <- [0 .. length resultTypes - 1]]

headerValues :: Text -> Text -> [Text]
headerValues key source = mapMaybe (T.stripPrefix ("; " <> key <> ": ")) (T.lines source)

driverSource :: String
driverSource =
  unlines
    [ "#include <stdint.h>",
      "#include <stdio.h>",
      "#include <string.h>",
      "extern int64_t aihc_lir_test_main(uint64_t *out);",
      "int main(void) {",
      "  uint64_t out[16];",
      "  memset(out, 0, sizeof out);",
      "  int64_t count = aihc_lir_test_main(out);",
      "  for (int64_t index = 0; index < count; ++index) {",
      "    printf(\"%llu\\n\", (unsigned long long)out[index]);",
      "  }",
      "  return 0;",
      "}"
    ]

runObject :: BL.ByteString -> IO (ExitCode, String, String)
runObject object =
  withTempDirectory "aihc-lir-fixture" $ \directory -> do
    let objectPath = directory </> "fixture.o"
        driverPath = directory </> "driver.c"
        executable = directory </> "fixture"
    BL.writeFile objectPath object
    writeFile driverPath driverSource
    (clangExit, _, clangErr) <-
      readProcessWithExitCode "clang" ["--target=arm64-apple-darwin", "-std=c11", driverPath, objectPath, "-o", executable] ""
    assertEqual ("clang failed to link the fixture:\n" <> clangErr) ExitSuccess clangExit
    readProcessWithExitCode executable [] ""

-- GRIN heap snapshots

data SnapshotFixture = SnapshotFixture
  { snapshotFixtureEntry :: !Text,
    snapshotFixtureProgram :: !Text,
    snapshotFixtureReturn :: !(Maybe Text),
    snapshotFixtureHeap :: !(Maybe Text),
    snapshotFixtureError :: !(Maybe Text),
    snapshotFixtureAllocations :: !(Maybe (Map.Map Text Word64)),
    snapshotFixtureStatus :: !Text
  }

instance FromJSON SnapshotFixture where
  parseJSON =
    withObject "GRIN snapshot fixture" $ \object ->
      SnapshotFixture
        <$> object .: "entry"
        <*> object .: "program"
        <*> object .:? "return"
        <*> object .:? "heap"
        <*> object .:? "error"
        <*> object .:? "allocations"
        <*> object .: "status"

-- | Lower the fixture program through Lir, check the Lir with the linter,
-- and compare the native heap snapshot with the fixture.
snapshotTest :: FilePath -> FilePath -> TestTree
snapshotTest directory name = testCase name $ do
  fixture <- either (assertFailure . Y.prettyPrintParseException) pure =<< Y.decodeFileEither (directory </> name)
  assertEqual "fixture status" "pass" (snapshotFixtureStatus fixture)
  program <- either (assertFailure . Grin.renderParseError) pure (parseProgram (snapshotFixtureProgram fixture))
  gc <- either (assertFailure . show) (pure . lowerGc) (toCpsGrin program)
  (lirModule, metadata) <- either (assertFailure . show) pure (lowerObservedProgram (FunctionName (snapshotFixtureEntry fixture)) gc)
  assertEqual "Lir lint" [] (map renderLintError (lintModule lirModule))
  reparsed <- either (assertFailure . renderParseError) pure (parseModule (renderModule lirModule))
  assertEqual "Lir pretty-printer round-trip" lirModule reparsed
  object <- either (assertFailure . show) pure (compileLirObject lirModule)
  when nativeHost $ do
    native <- runObservedObject object metadata
    case (snapshotFixtureReturn fixture, snapshotFixtureHeap fixture, snapshotFixtureError fixture, native) of
      (Just returnValue, Just heapValue, Nothing, Right snapshot) -> do
        allocations <- maybe (assertFailure "fixture has no macos-arm64 allocation count") pure (snapshotFixtureAllocations fixture >>= Map.lookup "macos-arm64")
        let heap = T.stripEnd heapValue
            expected
              | heap == "[]" = "return: " <> returnValue <> "\nheap: []"
              | otherwise = "return: " <> returnValue <> "\nheap:\n" <> T.unlines (map ("  " <>) (T.lines heap))
        assertEqual "native snapshot" (T.stripEnd expected <> "\nallocations: " <> T.pack (show allocations)) (T.stripEnd snapshot)
      (Nothing, Nothing, Just err, Left message) -> assertEqual "native error" (T.strip err) message
      (_, _, _, Left message) -> assertFailure ("native snapshot failed: " <> T.unpack message)
      (_, _, _, Right snapshot) -> assertFailure ("native snapshot unexpectedly succeeded:\n" <> T.unpack snapshot)

runObservedObject :: BL.ByteString -> Text -> IO (Either Text Text)
runObservedObject object metadata =
  withTempDirectory "aihc-lir-snapshot" $ \directory -> do
    runtimeArguments <- nativeRuntimeArguments RuntimeGcSemispace
    snapshotRuntime <- snapshotSourcePath
    let objectPath = directory </> "snapshot.o"
        metadataPath = directory </> "snapshot_metadata.c"
        executablePath = directory </> "snapshot"
    BL.writeFile objectPath object
    TIO.writeFile metadataPath metadata
    (clangExit, _, clangErr) <-
      readProcessWithExitCode
        "clang"
        ( ["--target=arm64-apple-darwin", "-std=c11", "-Wall", "-Wextra", "-Werror", "-I", takeDirectory snapshotRuntime]
            <> runtimeArguments
            <> [snapshotRuntime, metadataPath, objectPath, "-o", executablePath]
        )
        ""
    assertEqual ("clang failed to link the observed program:\n" <> clangErr) ExitSuccess clangExit
    (programExit, programOut, programErr) <- readProcessWithExitCode executablePath [] ""
    case programExit of
      ExitSuccess -> do
        assertEqual "native stderr" "" programErr
        pure (Right (T.pack programOut))
      ExitFailure _ -> do
        assertEqual "native stdout" "" programOut
        let message = T.strip (T.pack programErr)
        pure (Left (fromMaybe message (T.stripPrefix "aihc runtime: " message)))

-- Programs

-- | Lower a program as a library module and link it with the Lir entry unit
-- and the C runtime.
compileProgramObjects :: GrinProgram -> IO [BL.ByteString]
compileProgramObjects program = do
  let linkedProgram =
        program
          { grinGlobals =
              [ (if name == "main" then executableEntryName else name, node)
              | (name, node) <- grinGlobals program
              ]
          }
  assertEqual "direct GRIN lint" [] (lintProgram linkedProgram)
  gc <- either (assertFailure . show) (pure . lowerGc) (toCpsGrin linkedProgram)
  moduleLir <- either (assertFailure . show) pure (lowerModule gc)
  entryLir <- either (assertFailure . show) pure lowerEntry
  assertEqual "module Lir lint" [] (map renderLintError (lintModule moduleLir))
  assertEqual "entry Lir lint" [] (map renderLintError (lintModule entryLir))
  moduleObject <- either (assertFailure . show) pure (compileLirObject moduleLir)
  entryObject <- either (assertFailure . show) pure (compileLirObject entryLir)
  pure [moduleObject, entryObject]

collectorName :: RuntimeGarbageCollector -> String
collectorName collector =
  case collector of
    RuntimeGcSemispace -> "semispace collector"

programTest :: RuntimeGarbageCollector -> String -> GrinProgram -> IO ()
programTest collector expected program = do
  objects <- compileProgramObjects program
  when nativeHost $
    withProgramExecutable collector objects $ \executablePath -> do
      (programExit, programOut, programErr) <- readProcessWithExitCode executablePath [] ""
      assertEqual ("native stderr: " <> programErr) ExitSuccess programExit
      assertEqual "program stdout" expected programOut

stdioTest :: RuntimeGarbageCollector -> IO ()
stdioTest collector = do
  objects <- compileProgramObjects stdioSchedulerProgram
  when nativeHost $
    withProgramExecutable collector objects $ \executablePath -> do
      (Just childInput, Just childOutput, Just childError, processHandle) <-
        createProcess (proc executablePath []) {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
      threadDelay 50000
      hPutStr childInput "Buffered async IO\n"
      hFlush childInput
      hClose childInput
      programOut <- TIO.hGetContents childOutput
      programErr <- TIO.hGetContents childError
      programExit <- waitForProcess processHandle
      assertEqual ("native stderr: " <> T.unpack programErr) ExitSuccess programExit
      assertEqual "async stdout" "Buffered async IO\n" programOut

withProgramExecutable :: RuntimeGarbageCollector -> [BL.ByteString] -> (FilePath -> IO ()) -> IO ()
withProgramExecutable collector objects action =
  withTempDirectory "aihc-lir-program" $ \directory -> do
    runtimeArguments <- nativeRuntimeArguments collector
    objectPaths <- forM (zip [0 :: Int ..] objects) $ \(index, object) -> do
      let objectPath = directory </> "program-" <> show index <> ".o"
      BL.writeFile objectPath object
      pure objectPath
    let executablePath = directory </> "program"
    (clangExit, _, clangErr) <-
      readProcessWithExitCode "clang" (["--target=arm64-apple-darwin", "-std=c11", "-Wall", "-Wextra", "-Werror"] <> runtimeArguments <> objectPaths <> ["-o", executablePath]) ""
    assertEqual ("clang failed to link the program:\n" <> clangErr) ExitSuccess clangExit
    action executablePath

nativeRuntimeArguments :: RuntimeGarbageCollector -> IO [String]
nativeRuntimeArguments garbageCollector = do
  plan <- runtimePlan AppleArm64 garbageCollector
  pure (["-I" <> directory | directory <- runtimeIncludeDirectories plan] <> runtimeSources plan)

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
