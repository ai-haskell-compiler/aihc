{-# LANGUAGE OverloadedStrings #-}

-- | The Lir pipeline on a native backend: the Lir evaluation fixtures, the
-- GRIN heap snapshot fixtures lowered through Lir, and the scheduler
-- programs linked with the C runtime. The backend produces an object or a
-- source file that Clang compiles.
module Test.Lir.NativeSuite
  ( NativeBackend (..),
    tests,
    uncheckedTraps,
  )
where

import Aihc.Cli.Backend (BackendOutput (..), compileGrinTo, compileLirTo)
import Aihc.Grin hiding (renderParseError)
import Aihc.Grin qualified as Grin
import Aihc.Lir
import Aihc.Lir.Lower (LowerTarget, lowerEntry, lowerModule)
import Aihc.Native (NativeTarget (..), executableEntryName)
import Aihc.Parser.Syntax (Extension (ExtendedLiterals, MagicHash, UnboxedSums, UnboxedTuples))
import Aihc.Testing.ExceptionProgram (synchronousExceptionProgram)
import Aihc.Testing.RuntimeArchive (RuntimeBuild (..), RuntimeSources (..), cachedRuntimeArchive, runtimeSources, withFixtureRuntimeUnits)
import Aihc.Testing.SchedulerProgram (blackholeSchedulerProgram, schedulerProgram, stdioSchedulerProgram)
import Control.Concurrent (threadDelay)
import Control.Exception (bracket, evaluate)
import Control.Monad (forM, forM_, when, (<=<))
import Data.Aeson (FromJSON (..), withObject, (.!=), (.:), (.:?))
import Data.ByteString qualified as BS
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
import GrinGolden qualified
import System.Directory (createDirectory, getTemporaryDirectory, listDirectory, removeDirectoryRecursive, removeFile)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, takeExtension, (</>))
import System.IO (hClose, hFlush, hPutStr, openTempFile)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, readProcessWithExitCode, waitForProcess)
import Test.Lir.Observed (lowerObservedProgram)
import Test.Native.Observed (snapshotSourcePath)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

-- | One native backend under test.
data NativeBackend = NativeBackend
  { backendName :: !String,
    backendTarget :: !NativeTarget,
    backendLowerTarget :: !LowerTarget,
    -- | The Clang arguments that select the target.
    backendClangArguments :: ![String],
    -- | Whether this host can run the linked programs.
    backendRuns :: !Bool,
    -- | The allocated-byte key of the snapshot fixtures.
    backendAllocationKey :: !Text,
    -- | The extension of a source output.
    backendSourceExtension :: !String,
    backendCompile :: !(Module -> Either String BackendOutput)
  }

tests :: NativeBackend -> IO TestTree
tests backend = do
  root <- fromMaybe "." <$> lookupEnv "AIHC_TEST_ROOT"
  let directory = root </> "bin" </> "aihc" </> "compiler" </> "lir" </> "test" </> "Test" </> "Fixtures" </> "lir" </> "eval"
      snapshotDirectory = root </> "bin" </> "aihc" </> "compiler" </> "grin" </> "test" </> "Test" </> "Fixtures" </> "grin-snapshot"
  names <- sort . filter ((== ".lir") . takeExtension) <$> listDirectory directory
  sourceSnapshots <- sort . filter ((== ".yaml") . takeExtension) <$> listDirectory (root </> "bin/aihc/compiler/native/test/Test/Fixtures/source-snapshot")
  snapshots <- sort . filter ((== ".yaml") . takeExtension) <$> listDirectory snapshotDirectory
  pure
    ( testGroup
        (backendName backend)
        [ testGroup "Lir evaluation fixtures" (map (fixtureTest backend directory) names),
          -- The exports are read from the aihc-rts sources when a snapshot
          -- test first runs, not while the tree is built: the tree is built
          -- where those sources may be absent, such as a check that only
          -- compiles the tests.
          withResource (runtimeExports (backendTarget backend)) (const (pure ())) $ \getExports ->
            testGroup
              "heap snapshots through Lir"
              [ testGroup "GRIN" (map (snapshotTest backend getExports snapshotDirectory) snapshots),
                testGroup "source" (map (snapshotTest backend getExports (root </> "bin/aihc/compiler/native/test/Test/Fixtures/source-snapshot")) sourceSnapshots)
              ],
          testGroup
            "programs through Lir"
            [ testCase "runs fork# and yield# with FIFO scheduling" (programTest backend "PCAB" schedulerProgram),
              testCase "catches a synchronous exception" (programTest backend "E" synchronousExceptionProgram),
              testCase "blocks and wakes threads that enter a shared blackhole" (programTest backend "TA" blackholeSchedulerProgram),
              testCase "waits for stdin and resumes an async stdio continuation" (stdioTest backend)
            ]
        ]
    )

-- | No backend checks memory alignment or read-only data: neither a native
-- target nor WebAssembly faults on an unaligned access or on a store to a
-- read-only section. These interpreter traps therefore have no counterpart
-- in generated code, and every backend suite skips them.
uncheckedTraps :: [FilePath]
uncheckedTraps = ["trap-misaligned.lir", "trap-read-only.lir"]

-- | Write the backend output into the directory and return the path that
-- Clang links.
writeUnit :: NativeBackend -> FilePath -> String -> BackendOutput -> IO FilePath
writeUnit backend directory base output =
  case output of
    BackendObject object -> do
      let path = directory </> base <> ".o"
      BL.writeFile path object
      pure path
    BackendSource source -> do
      let path = directory </> base <> backendSourceExtension backend
      TIO.writeFile path source
      pure path

compileUnit :: NativeBackend -> Module -> IO BackendOutput
compileUnit backend lirModule = do
  output <- either (assertFailure . ("backend failed: " <>)) pure (backendCompile backend lirModule)
  when (backendTarget backend `elem` [AppleArm64, LinuxAmd64]) $
    withTempDirectory "aihc-lir-object" $ \directory -> do
      let path = directory </> "stream.o"
      source <- compileLirTo True (backendTarget backend) lirModule path
      assertEqual "native object output" Nothing source
      bytes <- BL.readFile path
      case output of
        BackendObject expected -> assertEqual "incremental object bytes" expected bytes
        BackendSource _ -> assertFailure "Native output is not an object."
  pure output

fixtureTest :: NativeBackend -> FilePath -> FilePath -> TestTree
fixtureTest backend directory name = testCase name $ do
  source <- TIO.readFile (directory </> name)
  parsed <- either (assertFailure . renderParseError) pure (parseModule source)
  expanded <- either (assertFailure . renderLoadError) pure =<< expandIncludes TIO.readFile (directory </> name) parsed
  lirModule <- withFixtureRuntimeUnits source expanded
  let resultTypes = concat [functionResults function | ItemFunction function <- moduleItems lirModule, functionName function == Symbol "main"]
      wrapped = Module (moduleItems lirModule <> [ItemFunction (testWrapper resultTypes)])
  output <- compileUnit backend wrapped
  when (backendTarget backend == AppleArm64) $
    forM_ (headerValues "max-arm64-object-bytes" source) $ \limit -> do
      maximumBytes <- case reads (T.unpack limit) of
        [(value, "")] -> pure value
        _ -> assertFailure "invalid ARM64 object size limit"
      case output of
        BackendObject bytes ->
          assertBool
            ("ARM64 object size " <> show (BL.length bytes) <> " exceeds " <> show maximumBytes)
            (BL.length bytes <= maximumBytes)
        BackendSource _ -> assertFailure "ARM64 output is not an object"
  when (backendRuns backend && name `notElem` uncheckedTraps) $ do
    (exit, out, err) <- runFixture backend output
    case (headerValues "expect" source, headerValues "expect-trap" source) of
      ([expected], []) -> do
        assertEqual ("exit status, stderr: " <> err) ExitSuccess exit
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
                  : [ Instruction [] (Store ty (OperandVar var) (byteAddress (OperandVar (Var "out")) (8 * index)) (byteAlignment 1))
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

runFixture :: NativeBackend -> BackendOutput -> IO (ExitCode, String, String)
runFixture backend output =
  withTempDirectory "aihc-lir-fixture" $ \directory -> do
    unit <- writeUnit backend directory "fixture" output
    let driverPath = directory </> "driver.c"
        executable = directory </> "fixture"
    writeFile driverPath driverSource
    (clangExit, _, clangErr) <-
      -- glibc keeps libm apart from libc, so a fixture that calls one of its
      -- functions needs -lm. The other two links here already carry it.
      readProcessWithExitCode "clang" (backendClangArguments backend <> ["-std=c11", driverPath, unit, "-lm", "-o", executable]) ""
    assertEqual ("clang failed to link the fixture:\n" <> clangErr) ExitSuccess clangExit
    readProcessWithExitCode executable [] ""

-- GRIN heap snapshots

data SnapshotFixture = SnapshotFixture
  { snapshotFixtureEntry :: !Text,
    snapshotFixtureProgram :: !(Maybe Text),
    snapshotFixtureSource :: !(Maybe Text),
    snapshotFixtureReturn :: !(Maybe Text),
    snapshotFixtureHeap :: !(Maybe Text),
    snapshotFixtureError :: !(Maybe Text),
    snapshotFixtureAllocatedBytes :: !(Maybe (Map.Map Text Word64)),
    snapshotFixtureGcStress :: !Bool,
    snapshotFixtureRtsArguments :: ![String],
    snapshotFixtureRequireGc :: !Bool,
    snapshotFixtureStatus :: !Text
  }

instance FromJSON SnapshotFixture where
  parseJSON =
    withObject "GRIN snapshot fixture" $ \object ->
      SnapshotFixture
        <$> object .: "entry"
        <*> object .:? "program"
        <*> object .:? "source"
        <*> object .:? "return"
        <*> object .:? "heap"
        <*> object .:? "error"
        <*> object .:? "allocated-bytes"
        <*> object .:? "gc-stress" .!= False
        <*> object .:? "rts-arguments" .!= []
        <*> object .:? "gc" .!= False
        <*> object .: "status"

-- | Lower the fixture program through Lir, check the Lir with the linter,
-- and compare the native heap snapshot with the fixture.
-- | The functions the Lir units of the runtime export, by name.
runtimeExports :: NativeTarget -> IO (Map.Map Symbol Signature)
runtimeExports target = do
  sources <- runtimeSources target
  runtimeModules <- mapM (either (assertFailure . renderLoadError) pure <=< loadModule) (runtimeLirSources sources)
  pure (Map.fromList [(functionName function, functionSignature function) | runtimeModule <- runtimeModules, ItemFunction function <- moduleItems runtimeModule, functionLinkage function == Export])

snapshotTest :: NativeBackend -> IO (Map.Map Symbol Signature) -> FilePath -> FilePath -> TestTree
snapshotTest backend getExports directory name = testCase name $ do
  exports <- getExports
  fixture <- either (assertFailure . Y.prettyPrintParseException) pure =<< Y.decodeFileEither (directory </> name)
  assertEqual "fixture status" "pass" (snapshotFixtureStatus fixture)
  program <- either assertFailure pure (snapshotProgram fixture)
  gc <- either (assertFailure . show) (pure . lowerGc) (toCpsGrin program)
  (lirModule, metadata) <- either (assertFailure . show) pure (lowerObservedProgram (backendLowerTarget backend) (snapshotFixtureGcStress fixture) (FunctionName (snapshotFixtureEntry fixture)) gc)
  assertEqual "Lir lint" [] (map renderLintError (lintModule lirModule))
  -- Every fixture must use the runtime exports without local copies.
  let localRuntimeFunctions = [functionName function | ItemFunction function <- moduleItems lirModule, Map.member (functionName function) exports]
  assertEqual "local copies of runtime functions" [] localRuntimeFunctions
  forM_ [external | ItemExternFunction external <- moduleItems lirModule, "aihc_lir_" `T.isPrefixOf` unSymbol (externFunctionName external)] $ \external ->
    assertEqual
      ("runtime helper signature: " <> T.unpack (unSymbol (externFunctionName external)))
      (Just (externFunctionSignature external))
      (Map.lookup (externFunctionName external) exports)
  reparsed <- either (assertFailure . renderParseError) pure (parseModule (renderModule lirModule))
  assertEqual "Lir pretty-printer round-trip" lirModule reparsed
  output <- compileUnit backend lirModule
  when (backendRuns backend) $ do
    native <- runObservedUnit backend fixture output metadata
    case (snapshotFixtureReturn fixture, snapshotFixtureHeap fixture, snapshotFixtureError fixture, native) of
      (Just returnValue, Just heapValue, Nothing, Right snapshot) -> do
        allocatedBytes <- maybe (assertFailure ("fixture has no " <> T.unpack (backendAllocationKey backend) <> " allocated byte count")) pure (snapshotFixtureAllocatedBytes fixture >>= Map.lookup (backendAllocationKey backend))
        let heap = T.stripEnd heapValue
            expected
              | heap == "[]" = "return: " <> returnValue <> "\nheap: []"
              | otherwise = "return: " <> returnValue <> "\nheap:\n" <> T.unlines (map ("  " <>) (T.lines heap))
        assertEqual "native snapshot" (T.stripEnd expected <> "\nallocated bytes: " <> T.pack (show allocatedBytes)) (T.stripEnd snapshot)
      (Nothing, Nothing, Just err, Left message) -> assertEqual "native error" (T.strip err) message
      (_, _, _, Left message) -> assertFailure ("native snapshot failed: " <> T.unpack message)
      (_, _, _, Right snapshot) -> assertFailure ("native snapshot unexpectedly succeeded:\n" <> T.unpack snapshot)

-- | Source fixtures declare one boxed value named @value@ in module @Test@.
snapshotProgram :: SnapshotFixture -> Either String GrinProgram
snapshotProgram fixture =
  case (snapshotFixtureProgram fixture, snapshotFixtureSource fixture) of
    (Just program, Nothing) -> either (Left . Grin.renderParseError) Right (parseProgram program)
    (Nothing, Just source) -> do
      programs <- GrinGolden.buildFcPrograms [MagicHash, UnboxedSums, UnboxedTuples, ExtendedLiterals] [source]
      case programs of
        [fc] -> do
          program <- Grin.lowerProgram fc
          let name = grinScopedName "" "Test" "value"
              entry = GrinFunction (FunctionName (snapshotFixtureEntry fixture)) [] liftedResultRep (GrinEval liftedGrinRep (GrinGlobalValue name))
              result = program {grinFunctions = entry : grinFunctions program}
          if any ((== name) . grinGlobalName) (grinGlobals program) && null (lintProgram result)
            then Right result
            else Left "source snapshot requires a valid boxed value named Test.value"
        _ -> Left "source snapshot requires one module"
    _ -> Left "snapshot requires either a GRIN program or a source module"

runObservedUnit :: NativeBackend -> SnapshotFixture -> BackendOutput -> Text -> IO (Either Text Text)
runObservedUnit backend fixture output metadata =
  withTempDirectory "aihc-lir-snapshot" $ \directory -> do
    runtimeBuild <-
      cachedRuntimeArchive
        (backendTarget backend)
        (["-std=c11", "-Wall", "-Wextra", "-Werror"] <> ["-DAIHC_SEMISPACE_BYTES=128" | snapshotFixtureGcStress fixture])
    snapshotRuntime <- snapshotSourcePath
    unit <- writeUnit backend directory "snapshot" output
    let metadataPath = directory </> "snapshot_metadata.c"
        executablePath = directory </> "snapshot"
    TIO.writeFile metadataPath ((if snapshotFixtureRequireGc fixture || snapshotFixtureGcStress fixture then "#define AIHC_SNAPSHOT_REQUIRE_GC\n" else "") <> metadata)
    (clangExit, _, clangErr) <-
      readProcessWithExitCode
        "clang"
        ( backendClangArguments backend
            <> ["-std=c11", "-Wall", "-Wextra", "-Werror", "-I", takeDirectory snapshotRuntime]
            <> runtimeIncludeArguments runtimeBuild
            <> [snapshotRuntime, metadataPath, unit, runtimeBuildArchive runtimeBuild, "-lm", "-o", executablePath]
        )
        ""
    assertEqual ("clang failed to link the observed program:\n" <> clangErr) ExitSuccess clangExit
    (programExit, programOut, programErr) <- readProcessWithExitCode executablePath (snapshotFixtureRtsArguments fixture) ""
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
compileProgramUnits :: NativeBackend -> GrinProgram -> IO [BackendOutput]
compileProgramUnits backend program = do
  let linkedProgram =
        program
          { grinGlobals =
              [ global {grinGlobalName = if grinGlobalName global == "main" then executableEntryName else grinGlobalName global}
              | global <- grinGlobals program
              ]
          }
  assertEqual "direct GRIN lint" [] (lintProgram linkedProgram)
  gc <- either (assertFailure . show) (pure . lowerGc) (toCpsGrin linkedProgram)
  moduleLir <- either (assertFailure . show) pure (lowerModule (backendLowerTarget backend) False gc)
  entryLir <- either (assertFailure . show) pure (lowerEntry (backendLowerTarget backend))
  assertEqual "module Lir lint" [] (map renderLintError (lintModule moduleLir))
  assertEqual "entry Lir lint" [] (map renderLintError (lintModule entryLir))
  moduleUnit <-
    if backendTarget backend `elem` [AppleArm64, LinuxAmd64]
      then withTempDirectory "aihc-grin-object" $ \directory -> do
        let path = directory </> "stream.o"
            dump = directory </> "stream.lir"
        source <- compileGrinTo True False (backendTarget backend) (Just dump) gc path
        assertEqual "native object output" Nothing source
        dumped <- loadModule dump >>= either (assertFailure . renderLoadError) pure
        assertEqual "incremental Lir dump lint" [] (map renderLintError (lintModule dumped))
        bytes <- BL.readFile path
        _ <- evaluate (BL.length bytes)
        when (backendTarget backend == LinuxAmd64) $
          assertBool "ELF stack section is absent." (".note.GNU-stack\0" `BS.isInfixOf` BL.toStrict bytes)
        pure (BackendObject bytes)
      else compileUnit backend moduleLir
  entryUnit <- compileUnit backend entryLir
  pure [moduleUnit, entryUnit]

programTest :: NativeBackend -> String -> GrinProgram -> IO ()
programTest backend expected program = do
  units <- compileProgramUnits backend program
  when (backendRuns backend) $
    withProgramExecutable backend units $ \executablePath -> do
      (programExit, programOut, programErr) <- readProcessWithExitCode executablePath [] ""
      assertEqual ("native stderr: " <> programErr) ExitSuccess programExit
      assertEqual "program stdout" expected programOut

stdioTest :: NativeBackend -> IO ()
stdioTest backend = do
  units <- compileProgramUnits backend stdioSchedulerProgram
  when (backendRuns backend) $
    withProgramExecutable backend units $ \executablePath -> do
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

withProgramExecutable :: NativeBackend -> [BackendOutput] -> (FilePath -> IO ()) -> IO ()
withProgramExecutable backend units action =
  withTempDirectory "aihc-lir-program" $ \directory -> do
    runtimeBuild <- nativeRuntimeBuild backend
    unitPaths <- forM (zip [0 :: Int ..] units) $ \(index, unit) -> writeUnit backend directory ("program-" <> show index) unit
    let executablePath = directory </> "program"
    (clangExit, _, clangErr) <-
      -- This step links the units against the runtime archive and compiles no
      -- C, so it carries no C compile flags: a toolchain that injects its own
      -- preprocessor flags would report every one of them as unused.
      readProcessWithExitCode "clang" (backendClangArguments backend <> unitPaths <> [runtimeBuildArchive runtimeBuild, "-lm", "-o", executablePath]) ""
    assertEqual ("clang failed to link the program:\n" <> clangErr) ExitSuccess clangExit
    action executablePath

-- | The runtime archive of one link. Every link with the same target
-- shares one archive, and takes the include directories first and the
-- archive last, so the test stays independent of how the runtime is put
-- together.
nativeRuntimeBuild :: NativeBackend -> IO RuntimeBuild
nativeRuntimeBuild backend =
  cachedRuntimeArchive (backendTarget backend) ["-std=c11", "-Wall", "-Wextra", "-Werror"]

runtimeIncludeArguments :: RuntimeBuild -> [String]
runtimeIncludeArguments build =
  ["-I" <> include | include <- runtimeBuildIncludeDirectories build]

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
