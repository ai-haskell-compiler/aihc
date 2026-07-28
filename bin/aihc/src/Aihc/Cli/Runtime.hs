{-# LANGUAGE NamedFieldPuns #-}

-- | Build runtime archives once, install them in the aihc store, and let
-- ordinary program links consume those immutable artifacts.
module Aihc.Cli.Runtime
  ( prepareRuntimeArchive,
    runPrepareRuntime,
    runtimeGarbageCollector,
    wasmClangCommand,
    wasmOptArguments,
  )
where

import Aihc.Cli.Options (GarbageCollector (..), PrepareRuntimeOptions (..))
import Aihc.Cli.Store (defaultStoreRoot, installedRuntimeArchivePath)
import Aihc.Native
  ( NativeTarget (..),
    RuntimeGarbageCollector (..),
    RuntimePlan (..),
    backendCompiler,
    nativeTargetTriple,
    runtimePlan,
  )
import Aihc.Wasm qualified as Wasm
import Control.Exception (bracket)
import Control.Monad (forM)
import Data.Maybe (fromMaybe)
import System.Directory (createDirectory, createDirectoryIfMissing, removeDirectoryRecursive, removeFile, renameFile)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.IO (hClose, openTempFile)
import System.Process (readProcessWithExitCode)

runPrepareRuntime :: PrepareRuntimeOptions -> IO ()
runPrepareRuntime options = do
  storeRoot <- maybe defaultStoreRoot pure (prepareRuntimeStoreRoot options)
  archive <-
    prepareRuntimeArchive
      storeRoot
      (prepareRuntimeTarget options)
      (prepareRuntimeGarbageCollector options)
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
    runTool "ar" (["rcs", archive] <> objects)
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
      runTool clang (clangTargetArguments <> cArguments <> ["-c", source, "-o", object])
      pure object
  runTool clang (clangTargetArguments <> cArguments <> ["-c", bindingsSource, "-o", bindingsObject])
  pure (runtimeObjects <> [bindingsObject, componentTypeObject])

runtimeGarbageCollector :: GarbageCollector -> RuntimeGarbageCollector
runtimeGarbageCollector garbageCollector =
  case garbageCollector of
    GcCalloc -> RuntimeGcCalloc
    GcSemispace -> RuntimeGcSemispace

-- | Select the ordinary Clang driver used for WebAssembly objects. Nix can
-- override only the executable to bypass its host-target compiler wrapper.
wasmClangCommand :: Maybe FilePath -> (FilePath, [String])
wasmClangCommand override =
  (fromMaybe "clang" override, ["--target=" <> nativeTargetTriple Wasm32Wasip3])

wasmOptArguments :: FilePath -> FilePath -> [String]
wasmOptArguments input output =
  [input, "-O3", "--enable-tail-call", "--emit-target-features", "-o", output]

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
