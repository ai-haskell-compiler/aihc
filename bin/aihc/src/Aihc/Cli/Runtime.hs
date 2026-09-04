{-# LANGUAGE NamedFieldPuns #-}

-- | Build support archives once, install them in the aihc store, and let
-- ordinary program links consume those immutable artifacts.
module Aihc.Cli.Runtime
  ( prepareEntryArchive,
    prepareRuntimeArchive,
    readWasmClangProcessWithExitCode,
    runPrepareRuntime,
    runtimeGarbageCollector,
    wasmClangCommand,
    wasmOptArguments,
  )
where

import Aihc.Cli.Backend (BackendOutput (..), compileLir, lowerTargetFor, nativeSourceExtension)
import Aihc.Cli.Options (GarbageCollector (..), PrepareRuntimeOptions (..))
import Aihc.Cli.Store (defaultStoreRoot, installedEntryArchivePath, installedRuntimeArchivePath)
import Aihc.Lir.Lower qualified as Lir
import Aihc.Native
  ( NativeTarget (..),
    RuntimeGarbageCollector (..),
    RuntimePlan (..),
    backendArchiver,
    backendCompiler,
    nativeTargetTriple,
    runtimePlan,
  )
import Aihc.Wasm qualified as Wasm
import Control.Exception (bracket)
import Control.Monad (forM)
import Data.ByteString.Lazy qualified as BL
import Data.Maybe (fromMaybe)
import Data.Text.IO qualified as TIO
import System.Directory (createDirectory, createDirectoryIfMissing, removeDirectoryRecursive, removeFile, renameFile)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.IO (hClose, openTempFile)
import System.IO.Error (tryIOError)
import System.Process (readProcessWithExitCode)

prepareEntryArchive :: FilePath -> NativeTarget -> IO FilePath
prepareEntryArchive storeRoot target = do
  let destination = installedEntryArchivePath storeRoot target
      destinationDirectory = takeDirectory destination
  createDirectoryIfMissing True destinationDirectory
  withTemporaryDirectory destinationDirectory "entry-build" $ \directory -> do
    let object = directory </> "entry.o"
        archive = directory </> "entry.a"
    entryModule <- either (ioError . userError . ("Lir entry generation failed: " <>) . show) pure (Lir.lowerEntry (lowerTargetFor target))
    output <- either (ioError . userError . ("Lir backend failed: " <>)) pure (compileLir target entryModule)
    case output of
      BackendObject bytes -> BL.writeFile object bytes
      BackendSource source -> do
        let sourcePath = directory </> "entry" <> nativeSourceExtension target
        TIO.writeFile sourcePath source
        (compiler, arguments) <- backendCompiler target
        runTool compiler (arguments <> ["-c", sourcePath, "-o", object])
    archiver <- backendArchiver target
    runTool archiver ["rcs", archive, object]
    renameFile archive destination
  pure destination

runPrepareRuntime :: PrepareRuntimeOptions -> IO ()
runPrepareRuntime options = do
  storeRoot <- maybe defaultStoreRoot pure (prepareRuntimeStoreRoot options)
  entry <- prepareEntryArchive storeRoot (prepareRuntimeTarget options)
  archive <-
    prepareRuntimeArchive
      storeRoot
      (prepareRuntimeTarget options)
      (prepareRuntimeGarbageCollector options)
  putStrLn ("entry: " <> entry)
  putStrLn ("runtime: " <> archive)

prepareRuntimeArchive :: FilePath -> NativeTarget -> GarbageCollector -> IO FilePath
prepareRuntimeArchive storeRoot target garbageCollector = do
  let destination = installedRuntimeArchivePath storeRoot target (runtimeGarbageCollector garbageCollector)
      destinationDirectory = takeDirectory destination
  createDirectoryIfMissing True destinationDirectory
  withTemporaryDirectory destinationDirectory "runtime-build" $ \directory -> do
    objects <-
      case target of
        Wasm32Wasip3 -> buildWasip3RuntimeObjects garbageCollector directory
        _ -> buildNativeRuntimeObjects target garbageCollector directory
    let archive = directory </> "runtime.a"
    archiver <- backendArchiver target
    runTool archiver (["rcs", archive] <> objects)
    renameFile archive destination
  pure destination

buildNativeRuntimeObjects :: NativeTarget -> GarbageCollector -> FilePath -> IO [FilePath]
buildNativeRuntimeObjects target garbageCollector directory = do
  RuntimePlan {runtimeSources, runtimeIncludeDirectories} <- runtimePlan target (runtimeGarbageCollector garbageCollector)
  (compiler, targetArguments) <- backendCompiler target
  let commonArguments =
        targetArguments
          <> ["-std=c11", "-Wall", "-Wextra", "-Werror"]
          <> ["-I" <> includeDirectory | includeDirectory <- runtimeIncludeDirectories]
  forM (zip [0 :: Int ..] runtimeSources) $ \(index, source) -> do
    let object = directory </> "runtime-" <> show index <> ".o"
    runTool compiler (commonArguments <> ["-c", source, "-o", object])
    pure object

buildWasip3RuntimeObjects :: GarbageCollector -> FilePath -> IO [FilePath]
buildWasip3RuntimeObjects garbageCollector directory = do
  RuntimePlan {runtimeSources, runtimeIncludeDirectories} <- runtimePlan Wasm32Wasip3 (runtimeGarbageCollector garbageCollector)
  wasmRuntimeSources <- Wasm.wasip3RuntimeSourcePaths
  driver <- Wasm.wasip3RuntimeSourcePath
  world <- Wasm.wasip3WorldPath
  clangOverride <- lookupEnv "AIHC_WASM_CLANG"
  let bindingsSource = directory </> "command.c"
      bindingsObject = directory </> "bindings.o"
      componentTypeObject = directory </> "command_component_type.o"
      (clang, clangTargetArguments) = wasmClangCommand clangOverride
      includeArguments =
        [ "-I" <> (takeDirectory driver </> "include"),
          "-I" <> takeDirectory driver,
          "-I" <> directory
        ]
          <> ["-I" <> includeDirectory | includeDirectory <- runtimeIncludeDirectories]
      cArguments =
        [ "-O2",
          "-std=c11",
          "-ffreestanding",
          "-fno-builtin",
          "-nostdlib",
          "-Wall",
          "-Wextra",
          "-Werror"
        ]
          <> includeArguments
  runTool "wit-bindgen" ["c", "--world", "command", "--out-dir", directory, world]
  runtimeObjects <-
    forM (zip [0 :: Int ..] (runtimeSources <> wasmRuntimeSources)) $ \(index, source) -> do
      let object = directory </> "runtime-" <> show index <> ".o"
      runWasmClangTool clang (clangTargetArguments <> cArguments <> ["-c", source, "-o", object])
      pure object
  runWasmClangTool clang (clangTargetArguments <> cArguments <> ["-c", bindingsSource, "-o", bindingsObject])
  pure (runtimeObjects <> [bindingsObject, componentTypeObject])

runtimeGarbageCollector :: GarbageCollector -> RuntimeGarbageCollector
runtimeGarbageCollector garbageCollector =
  case garbageCollector of
    GcSemispace -> RuntimeGcSemispace

-- | Select the ordinary Clang driver used for WebAssembly objects. Nix can
-- override only the executable to bypass its host-target compiler wrapper.
wasmClangCommand :: Maybe FilePath -> (FilePath, [String])
wasmClangCommand override =
  (fromMaybe "clang" override, ["--target=" <> nativeTargetTriple Wasm32Wasip3])

wasmOptArguments :: FilePath -> FilePath -> [String]
wasmOptArguments input output =
  [input, "-O3", "--enable-tail-call", "--emit-target-features", "-o", output]

-- | Run Clang and, after a WebAssembly compilation failure, inspect its
-- registered targets so a target-limited installation gets an actionable
-- diagnostic without obscuring Clang's original error.
readWasmClangProcessWithExitCode :: FilePath -> [String] -> IO (ExitCode, String, String)
readWasmClangProcessWithExitCode clang arguments = do
  result@(exitCode, stdout, stderr) <- readProcessWithExitCode clang arguments ""
  case exitCode of
    ExitSuccess -> pure result
    ExitFailure _ -> do
      targetsResult <- tryIOError (readProcessWithExitCode clang ["-print-targets"] "")
      pure
        ( exitCode,
          stdout,
          case targetsResult of
            Right (ExitSuccess, targets, _targetsStderr)
              | not (hasWasm32Target targets) -> appendWasm32TargetNotice stderr
            _ -> stderr
        )

hasWasm32Target :: String -> Bool
hasWasm32Target = any lineIsWasm32Target . lines
  where
    lineIsWasm32Target line =
      case words line of
        target : _ -> target == "wasm32"
        [] -> False

appendWasm32TargetNotice :: String -> String
appendWasm32TargetNotice originalError =
  originalError
    <> separator
    <> unlines
      [ "AIHC notice: this Clang installation does not include the wasm32 target.",
        "The default Clang shipped with macOS omits WebAssembly support. Install LLVM Clang",
        "with Homebrew (`brew install llvm`) or Nix",
        "(`nix shell nixpkgs#llvmPackages.clang-unwrapped`), then set AIHC_WASM_CLANG",
        "to that Clang executable."
      ]
  where
    separator
      | null originalError = ""
      | last originalError == '\n' = "\n"
      | otherwise = "\n\n"

runWasmClangTool :: FilePath -> [String] -> IO ()
runWasmClangTool clang arguments = do
  (exitCode, _stdout, stderr) <- readWasmClangProcessWithExitCode clang arguments
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure _ -> ioError (userError (clang <> " failed (" <> show exitCode <> "): " <> stderr))

runTool :: FilePath -> [String] -> IO ()
runTool tool arguments = do
  (exitCode, _stdout, stderr) <- readProcessWithExitCode tool arguments ""
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure _ -> ioError (userError (tool <> " failed (" <> show exitCode <> "): " <> stderr))

withTemporaryDirectory :: FilePath -> String -> (FilePath -> IO value) -> IO value
withTemporaryDirectory parent template = bracket acquire removeDirectoryRecursive
  where
    acquire = do
      createDirectoryIfMissing True parent
      (path, handle) <- openTempFile parent template
      hClose handle
      removeFile path
      createDirectory path
      pure path
