module Aihc.Cli.Options
  ( Command (..),
    BuildModuleOptions (..),
    BuildOptions (..),
    GarbageCollector (..),
    InstallErrorFormat (..),
    InstallOptions (..),
    LinkExeOptions (..),
    PrepareRuntimeOptions (..),
    parseCommandIO,
    parseCommandPure,
    parserInfo,
  )
where

import Aihc.Native (NativeTarget, OptimizationLevel, defaultOptimizationLevel, parseNativeTarget, parseOptimizationLevel, renderOptimizationLevel)
import Options.Applicative qualified as OA

data Command
  = CmdBuild !BuildOptions
  | CmdBuildModule !BuildModuleOptions
  | CmdInstall !InstallOptions
  | CmdLinkExe !LinkExeOptions
  | CmdPrepareRuntime !PrepareRuntimeOptions
  deriving (Eq, Show)

data GarbageCollector
  = GcSemispace
  deriving (Eq, Show)

-- | Build every executable of a Cabal package.
data BuildOptions = BuildOptions
  { -- | A local package directory, or a Hackage package name with an
    -- optional version, as @install@ takes it.
    buildPackageTarget :: !String,
    buildTarget :: !NativeTarget,
    buildGarbageCollector :: !GarbageCollector,
    buildStoreRoot :: !(Maybe FilePath),
    buildBuildRoot :: !(Maybe FilePath),
    buildLint :: !Bool,
    buildOptimization :: !OptimizationLevel,
    buildNoLink :: !Bool,
    buildVerbose :: !Bool,
    -- | Where the executables go; the default is @bin@ under the build
    -- directory of the target.
    buildOutputDirectory :: !(Maybe FilePath)
  }
  deriving (Eq, Show)

-- | Build one executable from a main module and the source directories its
-- imports are found in.
data BuildModuleOptions = BuildModuleOptions
  { buildModuleSourceFile :: !FilePath,
    buildModuleSourceDirectories :: ![FilePath],
    buildModulePackageConstraints :: ![String],
    buildModuleTarget :: !NativeTarget,
    buildModuleGarbageCollector :: !GarbageCollector,
    buildModuleStoreRoot :: !(Maybe FilePath),
    buildModuleBuildRoot :: !(Maybe FilePath),
    buildModuleWorkspace :: !(Maybe FilePath),
    buildModuleLint :: !Bool,
    buildModuleOptimization :: !OptimizationLevel,
    buildModuleNoLink :: !Bool,
    buildModuleOutputFile :: !(Maybe FilePath)
  }
  deriving (Eq, Show)

-- | Link an executable from a bundle that @build --no-link@ or
-- @build-module --no-link@ wrote.
data LinkExeOptions = LinkExeOptions
  { linkExeBundle :: !FilePath,
    linkExeOutputFile :: !FilePath
  }
  deriving (Eq, Show)

data PrepareRuntimeOptions = PrepareRuntimeOptions
  { prepareRuntimeTarget :: !NativeTarget,
    prepareRuntimeGarbageCollector :: !GarbageCollector,
    prepareRuntimeStoreRoot :: !(Maybe FilePath)
  }
  deriving (Eq, Show)

data InstallOptions = InstallOptions
  { installPackageTarget :: !String,
    installStoreRoot :: !(Maybe FilePath),
    installBuildRoot :: !(Maybe FilePath),
    installImmutable :: !Bool,
    installKeepCore :: !Bool,
    installKeepGrin :: !Bool,
    installKeepNative :: !Bool,
    installLint :: !Bool,
    installOptimization :: !OptimizationLevel,
    installReinstall :: !Bool,
    installNoCode :: !Bool,
    installVerbose :: !Bool,
    installPrintTimings :: !Bool,
    installTarget :: !NativeTarget
  }
  deriving (Eq, Show)

data InstallErrorFormat
  = InstallErrorsJson
  | InstallErrorsHuman
  deriving (Eq, Show)

parseCommandIO :: IO Command
parseCommandIO = OA.execParser parserInfo

parseCommandPure :: [String] -> Either String Command
parseCommandPure args =
  case OA.execParserPure OA.defaultPrefs parserInfo args of
    OA.Success command -> Right command
    OA.Failure failure ->
      let (message, _) = OA.renderFailure failure "aihc"
       in Left message
    OA.CompletionInvoked _ -> Left "completion invoked"

parserInfo :: OA.ParserInfo Command
parserInfo =
  OA.info
    (commandParser OA.<**> OA.helper)
    ( OA.fullDesc
        <> OA.header "aihc - command-line interface for the aihc compiler"
    )

commandParser :: OA.Parser Command
commandParser =
  OA.subparser
    ( OA.command
        "build"
        ( OA.info
            (CmdBuild <$> buildOptionsParser OA.<**> OA.helper)
            (OA.progDesc "Build every executable of one Cabal package from a local directory or Hackage")
        )
        <> OA.command
          "build-module"
          ( OA.info
              (CmdBuildModule <$> buildModuleOptionsParser OA.<**> OA.helper)
              (OA.progDesc "Build one Haskell executable from a main module")
          )
        <> OA.command
          "install"
          ( OA.info
              (CmdInstall <$> installOptionsParser OA.<**> OA.helper)
              (OA.progDesc "Build and install one Cabal library from a local directory or Hackage")
          )
        <> OA.command
          "link-exe"
          ( OA.info
              (CmdLinkExe <$> linkExeOptionsParser OA.<**> OA.helper)
              (OA.progDesc "Link one Haskell executable from a bundle written by build --no-link or build-module --no-link")
          )
        <> OA.command
          "prepare-runtime"
          ( OA.info
              (CmdPrepareRuntime <$> prepareRuntimeOptionsParser OA.<**> OA.helper)
              (OA.progDesc "Compile and install target entry and runtime archives")
          )
    )

buildOptionsParser :: OA.Parser BuildOptions
buildOptionsParser =
  BuildOptions
    <$> OA.strArgument
      ( OA.metavar "PACKAGE"
          <> OA.help "Local Cabal package directory, or a Hackage package name with an optional version (NAME[-VERSION])"
      )
    <*> nativeTargetOption
    <*> garbageCollectorOption
    <*> storeRootOption "Override the aihc store root"
    <*> buildRootOption "Build the package and its executables under DIR instead of its .aihc-target directory"
    <*> lintOption
    <*> optimizationOption
    <*> OA.switch
      ( OA.long "no-link"
          <> OA.help "Compile only: write a link bundle directory for each executable instead of linking it"
      )
    <*> OA.switch
      ( OA.long "verbose"
          <> OA.short 'v'
          <> OA.help "Print each build step"
      )
    <*> OA.optional
      ( OA.strOption
          ( OA.long "output-dir"
              <> OA.short 'o'
              <> OA.metavar "DIR"
              <> OA.help "Write the executables, or their link bundles with --no-link, under DIR instead of bin under the target's build directory"
          )
      )

buildModuleOptionsParser :: OA.Parser BuildModuleOptions
buildModuleOptionsParser =
  BuildModuleOptions
    <$> OA.strArgument
      ( OA.metavar "MODULE"
          <> OA.help "Main Haskell module"
      )
    <*> sourceDirectoryOptions
    <*> OA.many
      ( OA.strOption
          ( OA.long "package"
              <> OA.short 'p'
              <> OA.metavar "CONSTRAINT"
              <> OA.help "Add an installed package constraint"
          )
      )
    <*> nativeTargetOption
    <*> garbageCollectorOption
    <*> storeRootOption "Override the aihc store root"
    <*> buildRootOption "Write the executable's module artifacts under DIR instead of .aihc-target"
    <*> OA.optional
      ( OA.strOption
          ( OA.long "workspace"
              <> OA.metavar "DIR"
              <> OA.help "Take the sources of a package constraint from DIR/NAME before Hackage"
          )
      )
    <*> lintOption
    <*> optimizationOption
    <*> OA.switch
      ( OA.long "no-link"
          <> OA.help "Compile only: write the objects, archives, and a link.json manifest to the output directory instead of linking"
      )
    <*> OA.optional
      ( OA.strOption
          ( OA.long "output"
              <> OA.short 'o'
              <> OA.metavar "FILE"
              <> OA.help "Write the executable to FILE, or the link bundle to the directory FILE with --no-link"
          )
      )

linkExeOptionsParser :: OA.Parser LinkExeOptions
linkExeOptionsParser =
  LinkExeOptions
    <$> OA.strArgument
      ( OA.metavar "BUNDLE"
          <> OA.help "Directory holding the link.json manifest written by build --no-link or build-module --no-link"
      )
    <*> OA.strOption
      ( OA.long "output"
          <> OA.short 'o'
          <> OA.metavar "FILE"
          <> OA.help "Write the executable to FILE"
      )

sourceDirectoryOptions :: OA.Parser [FilePath]
sourceDirectoryOptions =
  defaultDirectory
    <$> OA.many
      ( OA.strOption
          ( OA.long "source-dir"
              <> OA.short 'i'
              <> OA.metavar "DIR"
              <> OA.help "Add a source directory. The default directory is ."
          )
      )
  where
    defaultDirectory [] = ["."]
    defaultDirectory directories = directories

lintOption :: OA.Parser Bool
lintOption =
  OA.switch
    ( OA.long "lint"
        <> OA.help "Run compiler intermediate-language lint checks"
    )

-- | @-O0@, @-O1@, @-O2@ or @-Os@, the level Clang receives for C sources
-- and LLVM output.
-- The level is part of the identity of an installed package, so the
-- packages of a build share its level.
optimizationOption :: OA.Parser OptimizationLevel
optimizationOption =
  OA.option
    (OA.eitherReader parseOptimizationLevel)
    ( OA.short 'O'
        <> OA.metavar "LEVEL"
        <> OA.value defaultOptimizationLevel
        <> OA.showDefaultWith renderOptimizationLevel
        <> OA.help "Optimization level for C sources and LLVM output: 0, 1, 2 or s"
    )

parseGarbageCollector :: String -> Either String GarbageCollector
parseGarbageCollector value =
  case value of
    "semispace" -> Right GcSemispace
    _ -> Left "expected semispace"

prepareRuntimeOptionsParser :: OA.Parser PrepareRuntimeOptions
prepareRuntimeOptionsParser =
  PrepareRuntimeOptions
    <$> nativeTargetOption
    <*> garbageCollectorOption
    <*> storeRootOption "Install the prepared archives into DIR"

nativeTargetOption :: OA.Parser NativeTarget
nativeTargetOption =
  OA.option
    (OA.eitherReader parseNativeTarget)
    ( OA.long "target"
        <> OA.metavar "TARGET"
        <> OA.help "Target: apple-arm64, linux-amd64, llvm, or wasm32-wasip3"
    )

garbageCollectorOption :: OA.Parser GarbageCollector
garbageCollectorOption =
  OA.option
    (OA.eitherReader parseGarbageCollector)
    ( OA.long "gc"
        <> OA.metavar "semispace"
        <> OA.value GcSemispace
        <> OA.showDefaultWith (const "semispace")
        <> OA.help "Select the garbage collector"
    )

buildRootOption :: String -> OA.Parser (Maybe FilePath)
buildRootOption description =
  OA.optional
    ( OA.strOption
        ( OA.long "build-root"
            <> OA.metavar "DIR"
            <> OA.help description
        )
    )

storeRootOption :: String -> OA.Parser (Maybe FilePath)
storeRootOption description =
  OA.optional
    ( OA.strOption
        ( OA.long "store"
            <> OA.metavar "DIR"
            <> OA.help description
        )
    )

installOptionsParser :: OA.Parser InstallOptions
installOptionsParser =
  InstallOptions
    <$> OA.strArgument
      ( OA.metavar "PACKAGE"
          <> OA.help "Local Cabal package directory, or a Hackage package name with an optional version (NAME[-VERSION])"
      )
    <*> storeRootOption "Override the aihc store root"
    <*> buildRootOption "Build a local package under DIR instead of its .aihc-target directory"
    <*> OA.switch
      ( OA.long "immutable"
          <> OA.help "Install a local package into the store, as if it were a Hackage release"
      )
    <*> OA.switch
      ( OA.long "keep-core"
          <> OA.help "Retain Core (System FC) files"
      )
    <*> OA.switch
      ( OA.long "keep-grin"
          <> OA.help "Retain GRIN files"
      )
    <*> OA.switch
      ( OA.long "keep-native"
          <> OA.help "Retain native output files"
      )
    <*> lintOption
    <*> optimizationOption
    <*> OA.switch
      ( OA.long "reinstall"
          <> OA.help "Build the package again when it exists"
      )
    <*> OA.switch
      ( OA.long "no-code"
          <> OA.help "Do not generate compiler or native code"
      )
    <*> OA.switch
      ( OA.long "verbose"
          <> OA.short 'v'
          <> OA.help "Print each installation step"
      )
    <*> OA.switch
      ( OA.long "print-timings"
          <> OA.help "Print compiler stage timings"
      )
    <*> nativeTargetOption
