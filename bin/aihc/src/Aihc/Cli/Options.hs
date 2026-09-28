module Aihc.Cli.Options
  ( Command (..),
    BuildOptions (..),
    InstallOptions (..),
    LinkExeOptions (..),
    PlanCommandOptions (..),
    PlanOptions (..),
    defaultPlanOptions,
    parseCommandIO,
    parseCommandPure,
    parserInfo,
  )
where

import Aihc.Native (NativeTarget, OptimizationLevel, defaultOptimizationLevel, parseNativeTarget, parseOptimizationLevel, renderOptimizationLevel)
import Options.Applicative qualified as OA

data Command
  = CmdBuild !BuildOptions
  | CmdInstall !InstallOptions
  | CmdLinkExe !LinkExeOptions
  | CmdPlan !PlanCommandOptions
  deriving (Eq, Show)

-- | Build an executable, or every executable of a Cabal package.
--
-- What is built follows from the input: a Haskell source file is the main
-- module of one executable, a directory is a local Cabal package, and
-- anything else is the name of a Hackage package with an optional version,
-- as @install@ takes it.
data BuildOptions = BuildOptions
  { buildInput :: !String,
    -- | Where the imports of a main module are found. A Cabal package
    -- names its own source directories.
    buildSourceDirectories :: ![FilePath],
    -- | The installed packages a main module is built against. A Cabal
    -- package names its own in @build-depends@.
    buildPackageConstraints :: ![String],
    buildTarget :: !NativeTarget,
    buildStoreRoot :: !(Maybe FilePath),
    buildBuildRoot :: !(Maybe FilePath),
    buildWorkspace :: !(Maybe FilePath),
    -- | Retain the intermediate output of each phase beside the object of
    -- the module it belongs to. Only the modules of the executable keep
    -- them: an installed dependency is built as @install@ builds it.
    buildKeepCore :: !Bool,
    buildKeepGrin :: !Bool,
    buildKeepLir :: !Bool,
    buildKeepNative :: !Bool,
    buildLint :: !Bool,
    buildCheckPrimBounds :: !Bool,
    buildLto :: !Bool,
    buildOptimization :: !OptimizationLevel,
    buildNoLink :: !Bool,
    buildVerbose :: !Bool,
    -- | The executable of a main module, or the directory the executables
    -- of a package go under; a link bundle takes the place of an executable
    -- with @--no-link@.
    buildOutput :: !(Maybe FilePath),
    -- | The executables of a package to plan and build. An empty list
    -- selects every buildable executable.
    buildExecutables :: ![String],
    buildPlanOptions :: !PlanOptions
  }
  deriving (Eq, Show)

-- | Solve the dependency plan of a package as @build@ does, and print it
-- without a build. The plan is always solved again, without a lock.
data PlanCommandOptions = PlanCommandOptions
  { planCommandInput :: !String,
    planCommandTarget :: !NativeTarget,
    planCommandWorkspace :: !(Maybe FilePath),
    -- | The executables of the package whose dependencies the plan takes.
    -- An empty list takes every executable.
    planCommandExecutables :: ![String],
    planCommandVerbose :: !Bool,
    -- | @NAME RANGE@, @NAME +flag@ or @NAME -flag@ restrictions on the
    -- plan. The plan does not read or write @aihc.lock@.
    planCommandConstraints :: ![String]
  }
  deriving (Eq, Show)

-- | How the dependency plan is solved and kept, see
-- @docs/dependency-resolution.md@.
data PlanOptions = PlanOptions
  { -- | @NAME RANGE@, @NAME +flag@ or @NAME -flag@ restrictions on the
    -- plan.
    planConstraints :: ![String],
    -- | Fail instead of rewriting a stale or absent @aihc.lock@.
    planLocked :: !Bool,
    -- | Ignore the lock for every package and rewrite it.
    planUpdate :: !Bool,
    -- | Ignore the lock for these packages and their dependents.
    planUpdatePackages :: ![String]
  }
  deriving (Eq, Show)

-- | Take the lock as it is, or solve and write it.
defaultPlanOptions :: PlanOptions
defaultPlanOptions = PlanOptions [] False False []

-- | Link an executable from a bundle that @build --no-link@ wrote.
data LinkExeOptions = LinkExeOptions
  { linkExeBundle :: !FilePath,
    linkExeOutputFile :: !FilePath
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
    installCheckPrimBounds :: !Bool,
    installLto :: !Bool,
    installOptimization :: !OptimizationLevel,
    installReinstall :: !Bool,
    installNoCode :: !Bool,
    installVerbose :: !Bool,
    installPrintTimings :: !Bool,
    installTarget :: !NativeTarget,
    installPlanOptions :: !PlanOptions
  }
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
            (OA.progDesc "Build one Haskell executable from a main module, or every executable of a Cabal package from a local directory or Hackage")
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
              (OA.progDesc "Link one Haskell executable from a bundle written by build --no-link")
          )
        <> OA.command
          "plan"
          ( OA.info
              (CmdPlan <$> planCommandOptionsParser OA.<**> OA.helper)
              (OA.progDesc "Solve the dependency plan of a Cabal package, and print one line for each package in dependency order")
          )
    )

buildOptionsParser :: OA.Parser BuildOptions
buildOptionsParser =
  BuildOptions
    <$> OA.strArgument
      ( OA.metavar "INPUT"
          <> OA.help "Main Haskell module, local Cabal package directory, or a Hackage package name with an optional version (NAME[-VERSION])"
      )
    <*> sourceDirectoryOptions
    <*> OA.many
      ( OA.strOption
          ( OA.long "package"
              <> OA.short 'p'
              <> OA.metavar "CONSTRAINT"
              <> OA.help "Add an installed package constraint for a main module"
          )
      )
    <*> nativeTargetOption
    <*> storeRootOption "Override the aihc store root"
    <*> buildRootOption "Write the module artifacts under DIR instead of .aihc-target"
    <*> workspaceOption
    <*> keepCoreOption
    <*> keepGrinOption
    <*> keepLirOption
    <*> keepNativeOption
    <*> lintOption
    <*> checkPrimBoundsOption
    <*> ltoOption
    <*> optimizationOption
    <*> OA.switch
      ( OA.long "no-link"
          <> OA.help "Compile only: write the objects, archives, and a link.json manifest to a bundle directory instead of linking"
      )
    <*> OA.switch
      ( OA.long "verbose"
          <> OA.short 'v'
          <> OA.help "Print each build step"
      )
    <*> OA.optional
      ( OA.strOption
          ( OA.long "output"
              <> OA.short 'o'
              <> OA.metavar "PATH"
              <> OA.help "Write the executable of a main module to PATH, or the executables of a package under the directory PATH; with --no-link, the link bundles take their place"
          )
      )
    <*> executableOptions "Build only the executable NAME of a package, and plan only its dependencies. Give the option again to add an executable"
    <*> planOptionsParser

planCommandOptionsParser :: OA.Parser PlanCommandOptions
planCommandOptionsParser =
  PlanCommandOptions
    <$> OA.strArgument
      ( OA.metavar "INPUT"
          <> OA.help "Local Cabal package directory, or a Hackage package name with an optional version (NAME[-VERSION])"
      )
    <*> nativeTargetOption
    <*> workspaceOption
    <*> executableOptions "Plan only the dependencies of the executable NAME, and of the library. Give the option again to add an executable"
    <*> OA.switch
      ( OA.long "verbose"
          <> OA.short 'v'
          <> OA.help "Print each planning step"
      )
    <*> constraintOptions

constraintOptions :: OA.Parser [String]
constraintOptions =
  OA.many
    ( OA.strOption
        ( OA.long "constraint"
            <> OA.metavar "CONSTRAINT"
            <> OA.help "Restrict the dependency plan: NAME RANGE fixes the versions of a package, NAME +flag and NAME -flag fix one of its cabal flags"
        )
    )

executableOptions :: String -> OA.Parser [String]
executableOptions description =
  OA.many
    ( OA.strOption
        ( OA.long "executable"
            <> OA.metavar "NAME"
            <> OA.help description
        )
    )

workspaceOption :: OA.Parser (Maybe FilePath)
workspaceOption =
  OA.optional
    ( OA.strOption
        ( OA.long "workspace"
            <> OA.metavar "DIR"
            <> OA.help "Take the sources of a dependency from DIR/NAME before Hackage"
        )
    )

planOptionsParser :: OA.Parser PlanOptions
planOptionsParser =
  PlanOptions
    <$> constraintOptions
    <*> OA.switch
      ( OA.long "locked"
          <> OA.help "Take the plan from aihc.lock and fail if the lock is absent or stale, instead of solving and rewriting it"
      )
    <*> OA.switch
      ( OA.long "update"
          <> OA.help "Ignore aihc.lock, solve the plan afresh, and rewrite the lock"
      )
    <*> OA.many
      ( OA.strOption
          ( OA.long "update-package"
              <> OA.metavar "NAME"
              <> OA.help "Ignore what aihc.lock says about NAME and the packages that depend on it, and rewrite the lock"
          )
      )

linkExeOptionsParser :: OA.Parser LinkExeOptions
linkExeOptionsParser =
  LinkExeOptions
    <$> OA.strArgument
      ( OA.metavar "BUNDLE"
          <> OA.help "Directory holding the link.json manifest written by build --no-link"
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

-- | The flags that keep the output of a compiler phase beside the object of
-- the module. Each phase writes its own files: @core@ for System FC, the
-- three @grin@ files for GRIN, @.lir@ for Lir, and the source the C driver
-- of the target compiles. A target whose object the backend writes itself
-- has no such source, so its @--keep-native@ output is the Lir text.
keepCoreOption :: OA.Parser Bool
keepCoreOption =
  OA.switch
    ( OA.long "keep-core"
        <> OA.help "Retain Core (System FC) files"
    )

keepGrinOption :: OA.Parser Bool
keepGrinOption =
  OA.switch
    ( OA.long "keep-grin"
        <> OA.help "Retain GRIN files"
    )

keepLirOption :: OA.Parser Bool
keepLirOption =
  OA.switch
    ( OA.long "keep-lir"
        <> OA.help "Retain Lir files"
    )

keepNativeOption :: OA.Parser Bool
keepNativeOption =
  OA.switch
    ( OA.long "keep-native"
        <> OA.help "Retain native output files"
    )

lintOption :: OA.Parser Bool
lintOption =
  OA.switch
    ( OA.long "lint"
        <> OA.help "Run compiler intermediate-language lint checks"
    )

-- | Check the index of every array primitive, as GHC does under
-- @-fcheck-prim-bounds@. The checks change the generated code, so the flag
-- is part of the identity of an installed package.
checkPrimBoundsOption :: OA.Parser Bool
checkPrimBoundsOption =
  OA.switch
    ( OA.long "check-prim-bounds"
        <> OA.help "Check the index of every array primitive against the array length and abort on an out-of-bounds access (GHC -fcheck-prim-bounds)"
    )

-- | Compile each module to System FC only. @build@ merges the System FC of
-- every module of the program and compiles the merged program once.
-- @-O2@ and @-Os@ imply the flag, which selects the same build at @-O0@
-- and @-O1@. The build is part of the identity of an installed package, so
-- its packages are separate store entries.
ltoOption :: OA.Parser Bool
ltoOption =
  OA.switch
    ( OA.long "lto"
        <> OA.help "Compile each module to System FC only and compile the merged program of the executable once. -O2 and -Os imply this"
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
        <> OA.help "Optimization level: 0, 1, 2 or s. Level 0 runs no System FC pass, s runs the shrinking inliner, 1 and 2 also run the growing one. Levels 2 and s compile the whole program at once, and every level is the level Clang receives for C sources and LLVM output"
    )

nativeTargetOption :: OA.Parser NativeTarget
nativeTargetOption =
  OA.option
    (OA.eitherReader parseNativeTarget)
    ( OA.long "target"
        <> OA.metavar "TARGET"
        <> OA.help "Target: apple-arm64, linux-amd64, llvm, or wasm32-wasip3"
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
    <*> keepCoreOption
    <*> keepGrinOption
    <*> keepNativeOption
    <*> lintOption
    <*> checkPrimBoundsOption
    <*> ltoOption
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
    <*> planOptionsParser
