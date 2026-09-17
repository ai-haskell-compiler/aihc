{-# LANGUAGE NamedFieldPuns #-}

-- | The runtime system as a build input outside the store.
--
-- The runtime is the @aihc-rts@ package under @core-libs@: C sources, Lir
-- units, and headers, named by its Cabal file. A program takes the runtime
-- through the ordinary package link, since @aihc-prim@ depends on it, and
-- the store fingerprints it like any other package. What is here serves
-- the callers that need the runtime as one archive outside the store: the
-- test harnesses that instrument it or resize its semispace.
module Aihc.Cli.Runtime
  ( RuntimeBuild (..),
    RuntimeSources (..),
    runtimeSources,
    runtimeSourceRoot,
    buildRuntimeArchive,
    compileEntryObject,
    readWasmClangProcessWithExitCode,
    wasmClangCommand,
  )
where

import Aihc.Cli.Backend (compileLirObject, lirModuleDefinesCode, lowerTargetFor)
import Aihc.Cli.CompilerHeaders (cabalPlatformForTarget)
import Aihc.Hackage.Cabal qualified as HackageCabal
import Aihc.Lir.Lower qualified as Lir
import Aihc.Lir.Resolve (loadModule, renderLoadError)
import Aihc.Native
  ( NativeTarget (..),
    WasmSysroot (..),
    backendArchiver,
    backendCompiler,
    handwrittenCArguments,
    nativeTargetTriple,
    runtimeOptimizationLevel,
    wasmSysroot,
  )
import Aihc.PackagePlan (aihcRtsProvider, coreProviderSourcePath, parseSourcePackageDescription)
import Control.Monad (forM)
import Data.Maybe (catMaybes, fromMaybe)
import System.Exit (ExitCode (..))
import System.FilePath (takeBaseName, (</>))
import System.IO.Error (tryIOError)
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

-- | Compile the entry unit of an executable to @object@. The entry starts
-- the runtime and enters the program; the entry of every executable is the
-- same, so it is generated rather than read from a source.
compileEntryObject :: NativeTarget -> FilePath -> FilePath -> IO ()
compileEntryObject target directory object = do
  entryModule <- either (ioError . userError . ("Lir entry generation failed: " <>) . show) pure (Lir.lowerEntry (lowerTargetFor target))
  compileLirObject target (takeBaseName object) entryModule directory object

-- | Select the ordinary Clang driver used for WebAssembly objects. Nix can
-- override only the executable to bypass its host-target compiler wrapper.
-- The sysroot is not part of this: an assembly input needs no headers, and
-- the C compilations add it themselves.
wasmClangCommand :: Maybe FilePath -> (FilePath, [String])
wasmClangCommand override =
  (fromMaybe "clang" override, ["--target=" <> nativeTargetTriple Wasm32Wasip3])

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

runTool :: FilePath -> [String] -> IO ()
runTool tool arguments = do
  (exitCode, _stdout, stderr) <- readProcessWithExitCode tool arguments ""
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure _ -> ioError (userError (tool <> " failed (" <> show exitCode <> "): " <> stderr))
