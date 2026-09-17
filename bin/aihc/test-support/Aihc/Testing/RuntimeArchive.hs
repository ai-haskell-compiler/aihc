{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | The runtime as one archive outside the store, for the tests that
-- instrument it or resize its semispace.
--
-- The runtime is the @aihc-rts@ package under @core-libs@, which a program
-- links through the ordinary package plan. A test that needs its own
-- runtime -- an instrumented one, or one with a smaller semispace -- builds
-- an archive here from the same Cabal file, with extra C arguments, instead
-- of naming the runtime sources. Moving a unit from C to Lir then changes no
-- test. See the "Runtime units" section of @docs/lir.md@.
--
-- Building that archive once per link would compile the whole runtime once
-- per link, so the archives live in one directory for the lifetime of the
-- test executable and 'releaseCachedRuntimeArchives' removes them at the end.
module Aihc.Testing.RuntimeArchive
  ( RuntimeBuild (..),
    RuntimeSources (..),
    runtimeSources,
    runtimeSourceRoot,
    buildRuntimeArchive,
    cachedRuntimeArchive,
    releaseCachedRuntimeArchives,
  )
where

import Aihc.Cli.Backend (compileLirObject, lirModuleDefinesCode)
import Aihc.Cli.CompilerHeaders (cabalPlatformForTarget)
import Aihc.Hackage.Cabal qualified as HackageCabal
import Aihc.Lir.Resolve (loadModule, renderLoadError)
import Aihc.Native
  ( NativeTarget (..),
    WasmSysroot (..),
    backendArchiver,
    backendCompiler,
    handwrittenCArguments,
    runtimeOptimizationLevel,
    wasmSysroot,
  )
import Aihc.PackagePlan (aihcRtsProvider, coreProviderSourcePath, parseSourcePackageDescription)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newEmptyMVar, newMVar, putMVar, readMVar)
import Control.Exception (SomeException, throwIO, try)
import Control.Monad (forM)
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import System.Directory (removeDirectoryRecursive)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Error (tryIOError)
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcessWithExitCode)

-- | One runtime archive outside the store, and the include directories a
-- caller needs for the runtime headers.
data RuntimeBuild = RuntimeBuild
  { runtimeBuildArchive :: !FilePath,
    runtimeBuildIncludeDirectories :: ![FilePath]
  }
  deriving (Eq, Show)

-- | What the Cabal file of @aihc-rts@ names for one target.
data RuntimeSources = RuntimeSources
  { runtimeCSources :: ![FilePath],
    -- | The Lir units. Every target compiles them with its own Lir backend
    -- instead of a C compiler; see @docs/lir.md@.
    runtimeLirSources :: ![FilePath],
    runtimeIncludeDirectories :: ![FilePath],
    -- | The @cc-options@ of the package, which every C source takes.
    runtimeCcOptions :: ![String]
  }
  deriving (Eq, Show)

-- | The directory of the @aihc-rts@ package.
runtimeSourceRoot :: IO FilePath
runtimeSourceRoot = coreProviderSourcePath aihcRtsProvider

-- | Read the sources of the runtime for a target from the Cabal file of
-- @aihc-rts@, the same way an install reads them.
runtimeSources :: NativeTarget -> IO RuntimeSources
runtimeSources target = do
  root <- runtimeSourceRoot
  gpd <- parseSourcePackageDescription root
  let (os, arch) = cabalPlatformForTarget target
      info = HackageCabal.collectLibraryCCompileInfoFor os arch gpd root
  pure
    RuntimeSources
      { runtimeCSources = HackageCabal.cCompileSources info,
        runtimeLirSources = HackageCabal.cCompileLirSources info,
        runtimeIncludeDirectories = HackageCabal.cCompileIncludeDirs info,
        runtimeCcOptions = HackageCabal.cCompileCcOptions info
      }

-- | Build one runtime archive in @directory@ and return it with the include
-- directories of the runtime headers.
--
-- @extraCArguments@ joins the arguments of every C source, so a caller can
-- instrument the runtime or resize its semispace. The Lir units go through
-- the Lir backend of the target and take no C arguments. A caller links the
-- finished archive and stays independent of which units are C and which are
-- Lir. Place the archive after the objects that reference it: a linker takes
-- only the members that resolve a symbol it has already seen.
buildRuntimeArchive :: NativeTarget -> [String] -> FilePath -> IO RuntimeBuild
buildRuntimeArchive target extraCArguments directory = do
  RuntimeSources {runtimeCSources, runtimeLirSources, runtimeIncludeDirectories, runtimeCcOptions} <- runtimeSources target
  (compiler, targetArguments) <- backendCompiler target
  sysrootIncludes <-
    case target of
      Wasm32Wasip3 -> do
        sysroot <- wasmSysroot
        pure ["-isystem" <> wasmSysrootInclude sysroot]
      _ -> pure []
  let commonArguments =
        targetArguments
          <> handwrittenCArguments runtimeOptimizationLevel
          <> runtimeCcOptions
          <> extraCArguments
          <> sysrootIncludes
          <> ["-I" <> includeDirectory | includeDirectory <- runtimeIncludeDirectories]
  cObjects <- forM (zip [0 :: Int ..] runtimeCSources) $ \(index, source) -> do
    let object = directory </> "runtime-" <> show index <> ".o"
    runTool compiler (commonArguments <> ["-c", source, "-o", object])
    pure object
  lirObjects <- forM (zip [0 :: Int ..] runtimeLirSources) $ \(index, source) -> do
    let name = "runtime-lir-" <> show index
        object = directory </> name <> ".o"
    lirModule <- either (ioError . userError . renderLoadError) pure =<< loadModule source
    if lirModuleDefinesCode lirModule
      then compileLirObject target name lirModule directory object >> pure (Just object)
      else pure Nothing
  let archive = directory </> "runtime.a"
  archiver <- backendArchiver target
  runTool archiver (["rcs", archive] <> cObjects <> catMaybes lirObjects)
  pure RuntimeBuild {runtimeBuildArchive = archive, runtimeBuildIncludeDirectories = runtimeIncludeDirectories}

-- | What makes two runtime archives interchangeable.
type ArchiveKey = (NativeTarget, [String])

-- | The archive of each key, or the failure that building it raised. A key
-- that is being built holds an empty cell, so a second caller waits for the
-- first rather than building the same archive again.
archiveCache :: MVar (Map ArchiveKey (MVar (Either SomeException RuntimeBuild)))
archiveCache = unsafePerformIO (newMVar Map.empty)
{-# NOINLINE archiveCache #-}

-- | The directory that holds every cached archive, created on first use.
archiveRoot :: MVar (Maybe FilePath)
archiveRoot = unsafePerformIO (newMVar Nothing)
{-# NOINLINE archiveRoot #-}

-- | The runtime archive for one target and set of extra C arguments. The
-- first caller of a key builds it and every later caller of
-- that key gets the same archive. A build that fails is remembered, so a
-- broken toolchain raises once per key instead of once per link.
cachedRuntimeArchive :: NativeTarget -> [String] -> IO RuntimeBuild
cachedRuntimeArchive target extraCArguments = do
  let key = (target, extraCArguments)
  slot <-
    modifyMVar archiveCache $ \cache ->
      case Map.lookup key cache of
        Just existing -> pure (cache, Right existing)
        Nothing -> do
          fresh <- newEmptyMVar
          pure (Map.insert key fresh cache, Left fresh)
  case slot of
    Right ready -> readMVar ready >>= either throwIO pure
    Left fresh -> do
      result <- try (buildForKey key)
      putMVar fresh result
      either throwIO pure result

buildForKey :: ArchiveKey -> IO RuntimeBuild
buildForKey (target, extraCArguments) = do
  root <- runtimeArchiveRoot
  directory <- createTempDirectory root "archive"
  buildRuntimeArchive target extraCArguments directory

runtimeArchiveRoot :: IO FilePath
runtimeArchiveRoot =
  modifyMVar archiveRoot $ \case
    Just root -> pure (Just root, root)
    Nothing -> do
      temporary <- getCanonicalTemporaryDirectory
      root <- createTempDirectory temporary "aihc-runtime-archives"
      pure (Just root, root)

-- | Forget and remove the cached archives. The test executable calls this
-- when it exits.
releaseCachedRuntimeArchives :: IO ()
releaseCachedRuntimeArchives = do
  modifyMVar_ archiveCache (const (pure Map.empty))
  root <- modifyMVar archiveRoot (\current -> pure (Nothing, current))
  traverse_ (tryIOError . removeDirectoryRecursive) root

runTool :: FilePath -> [String] -> IO ()
runTool tool arguments = do
  (exitCode, _stdout, stderr) <- readProcessWithExitCode tool arguments ""
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure _ -> ioError (userError (tool <> " failed (" <> show exitCode <> "): " <> stderr))
