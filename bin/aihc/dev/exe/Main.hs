module Main (main) where

import Aihc.Cli.Lto (demoteToEntry, entryName, readPrograms)
import Aihc.Cli.OptimizationPlan (OptimizationPlan (..), optimizationPlan)
import Aihc.Dev.Explore (ExploreOptions (..), runExplore)
import Aihc.Dev.ExtractHi (extractPackage)
import Aihc.Dev.ExtractHi.Compare (comparePackageSubset, renderCoreLibProgressReports, renderInterfaceMismatch, runCoreLibApiDivergences, runCoreLibProgressReports)
import Aihc.Dev.ExtractHi.ToResolveIface (toResolveIface)
import Aihc.Dev.Frontend (FrontendOptions (..), runFrontend)
import Aihc.Dev.Fuzz qualified as Fuzz
import Aihc.Dev.Fuzz.CLI qualified as FuzzCLI
import Aihc.Dev.PipelineExamples (PipelineExamplesOptions (..), runPipelineExamples)
import Aihc.Fc qualified as Fc
import Aihc.Native (NativeTarget, OptimizationLevel (..), hostNativeTarget, parseNativeTarget, parseOptimizationLevel)
import Control.Monad (foldM_, unless, when)
import Data.Aeson (encode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Yaml qualified as Yaml
import Options.Applicative
import System.Directory (createDirectoryIfMissing)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory)
import System.IO (stderr)

main :: IO ()
main = do
  cmd <- execParser opts
  runCommand cmd
  where
    opts =
      info
        (commandParser <**> helper)
        ( fullDesc
            <> header "aihc-dev - developer tools for the aihc compiler"
        )

-- | Top-level command type. New subcommands are added here.
data Command
  = ExtractHi ExtractHiOpts
  | CompareHiSubset CompareHiSubsetOpts
  | CoreLibsProgress CoreLibsProgressOpts
  | ExtractResolveIface ExtractResolveIfaceOpts
  | Fuzz FuzzCLI.Command
  | Frontend FrontendOptions
  | PipelineExamples PipelineExamplesOptions
  | FcPrint FilePath
  | FcPasses [FilePath] OptimizationLevel [String]
  | Explore FilePath OptimizationLevel (Maybe NativeTarget)

data ExtractHiOpts = ExtractHiOpts
  { ehPackage :: String,
    ehFormat :: OutputFormat
  }

data ExtractResolveIfaceOpts = ExtractResolveIfaceOpts
  { eriPackage :: String,
    eriOutput :: FilePath
  }

data CompareHiSubsetOpts = CompareHiSubsetOpts
  { chsCandidate :: String,
    chsOracle :: String
  }

newtype CoreLibsProgressOpts = CoreLibsProgressOpts
  { clpDivergences :: Bool
  }

data OutputFormat = YAML | JSON
  deriving (Show)

commandParser :: Parser Command
commandParser =
  subparser
    ( command
        "extract-hi"
        ( info
            (ExtractHi <$> extractHiParser <**> helper)
            (progDesc "Extract scoping and typing information from .hi interface files")
        )
        <> command
          "compare-hi-subset"
          ( info
              (CompareHiSubset <$> compareHiSubsetParser <**> helper)
              (progDesc "Check that a candidate .hi interface is a subset of an oracle package")
          )
        <> command
          "core-libs-progress"
          ( info
              (CoreLibsProgress <$> coreLibsProgressParser <**> helper)
              (progDesc "Report ghc-prim/base API coverage for aihc-prim/aihc-base")
          )
        <> command
          "extract-resolve-iface"
          ( info
              (ExtractResolveIface <$> extractResolveIfaceParser <**> helper)
              (progDesc "Extract minimal resolver interface (names only) from .hi files")
          )
        <> command
          "fuzz"
          ( info
              (Fuzz <$> FuzzCLI.commandParser <**> helper)
              (progDesc "Continuously run Hedgehog properties in parallel")
          )
        <> command
          "frontend"
          ( info
              (Frontend <$> frontendParser <**> helper)
              (progDesc "Preprocess, parse, resolve and type check packages one phase at a time, timing each phase")
          )
        <> command
          "pipeline-examples"
          ( info
              (PipelineExamples <$> pipelineExamplesParser <**> helper)
              (progDesc "Compile the pipeline examples of the AIHC Manual and write their System FC, GRIN and Lir programs")
          )
        <> command
          "fc-print"
          ( info
              (FcPrint <$> strArgument (metavar "FILE" <> help "A core file of the store or of a build root") <**> helper)
              (progDesc "Print a binary System FC file in the System FC text format")
          )
        <> command
          "fc-passes"
          ( info
              (fcPassesParser <**> helper)
              (progDesc "Run the System FC passes of a level on a core file, lint after each pass, and print the declarations that fail the lint")
          )
        <> command
          "explore"
          ( info
              (exploreParser <**> helper)
              (progDesc "Build a program at each optimization level and show its Haskell source next to its System FC and GRIN in a terminal explorer")
          )
    )

fcPassesParser :: Parser Command
fcPassesParser =
  FcPasses
    <$> some (strArgument (metavar "FILE" <> help "Core files of a build root. Several files merge into one whole program, as the --lto link merges them"))
    <*> option
      (eitherReader parseOptimizationLevel)
      (short 'O' <> metavar "LEVEL" <> value O2 <> help "The optimization level whose passes run: 0, 1, 2 or s (default: 2)")
    <*> many (strOption (long "show" <> metavar "NAME" <> help "Print the declaration NAME before the passes and after each pass"))

exploreParser :: Parser Command
exploreParser =
  Explore
    <$> strArgument (metavar "INPUT" <> help "Main Haskell module or local Cabal package directory")
    <*> option
      (eitherReader parseOptimizationLevel)
      (short 'O' <> metavar "LEVEL" <> value O2 <> help "The first optimization level to show: 0, 1, 2 or s (default: 2)")
    <*> optional (option (eitherReader parseNativeTarget) (long "target" <> metavar "TARGET" <> help "Target: apple-arm64, linux-amd64, llvm, or wasm32-wasip3 (default: the host)"))

pipelineExamplesParser :: Parser PipelineExamplesOptions
pipelineExamplesParser =
  PipelineExamplesOptions
    <$> strArgument
      ( metavar "EXAMPLES_DIR"
          <> help "Directory with one directory for each example, each with Example.hs and description.md"
      )
    <*> strOption
      ( long "output"
          <> metavar "DIR"
          <> help "Directory to replace with the generated programs and Markdown"
      )

frontendParser :: Parser FrontendOptions
frontendParser =
  FrontendOptions
    <$> some
      ( strArgument
          ( metavar "PACKAGE..."
              <> help "A package directory or a versioned Hackage package (NAME-VERSION), processed in order; each is checked against the ones before it, and the core libraries are not added"
          )
      )
    <*> optional
      ( option
          auto
          ( long "jobs"
              <> short 'j'
              <> metavar "N"
              <> help "Work on N units at once within a phase (default: the capabilities of the process)"
          )
      )
    <*> switch
      ( long "verbose"
          <> help "Print what each phase does"
      )
    <*> optional
      ( option
          (eitherReader parseNativeTarget)
          ( long "target"
              <> metavar "TARGET"
              <> help "Target whose headers the preprocessor sees: apple-arm64, linux-amd64, llvm, or wasm32-wasip3 (default: the host)"
          )
      )

extractHiParser :: Parser ExtractHiOpts
extractHiParser =
  ExtractHiOpts
    <$> strArgument
      ( metavar "PACKAGE"
          <> help "Package name to extract (e.g. 'base', 'containers')"
      )
    <*> flag
      YAML
      JSON
      ( long "json"
          <> help "Output JSON instead of YAML"
      )

extractResolveIfaceParser :: Parser ExtractResolveIfaceOpts
extractResolveIfaceParser =
  ExtractResolveIfaceOpts
    <$> strOption
      ( long "package"
          <> metavar "PACKAGE"
          <> help "Package name to extract (e.g. 'base', 'ghc-prim')"
      )
    <*> strOption
      ( long "output"
          <> metavar "FILE"
          <> help "Output file path for the JSON interface"
      )

coreLibsProgressParser :: Parser CoreLibsProgressOpts
coreLibsProgressParser =
  CoreLibsProgressOpts
    <$> switch
      ( long "divergences"
          <> help "Also list every export of a module shared with ghc-prim or base that GHC does not provide, and fail if there are any"
      )

compareHiSubsetParser :: Parser CompareHiSubsetOpts
compareHiSubsetParser =
  CompareHiSubsetOpts
    <$> strOption
      ( long "candidate"
          <> metavar "PACKAGE"
          <> help "Candidate package that must be a subset"
      )
    <*> strOption
      ( long "oracle"
          <> metavar "PACKAGE"
          <> help "Oracle package that defines the compatible API"
      )

runCommand :: Command -> IO ()
runCommand (ExtractHi opts) = do
  pkg <- extractPackage (ehPackage opts)
  case ehFormat opts of
    YAML -> BL.putStr (BL.fromStrict (Yaml.encode pkg))
    JSON -> BL.putStr (encode pkg)
runCommand (CompareHiSubset opts) = do
  candidate <- extractPackage (chsCandidate opts)
  oracle <- extractPackage (chsOracle opts)
  let mismatches = comparePackageSubset candidate oracle
  if null mismatches
    then putStrLn "OK"
    else do
      mapM_ (putStrLn . renderInterfaceMismatch) mismatches
      exitFailure
runCommand (CoreLibsProgress opts) = do
  putStr . renderCoreLibProgressReports =<< runCoreLibProgressReports
  when (clpDivergences opts) $ do
    divergences <- runCoreLibApiDivergences
    unless (null divergences) $ do
      mapM_ (putStrLn . renderInterfaceMismatch) divergences
      exitFailure
runCommand (ExtractResolveIface opts) = do
  pkg <- extractPackage (eriPackage opts)
  let resolveIface = toResolveIface pkg
      outputPath = eriOutput opts
  createDirectoryIfMissing True (takeDirectory outputPath)
  BL.writeFile outputPath (encodePretty resolveIface)
runCommand (Fuzz fuzzCommand) =
  Fuzz.runCommand fuzzCommand
runCommand (Frontend options) =
  runFrontend options
runCommand (PipelineExamples options) =
  runPipelineExamples options
runCommand (Explore input level target) = do
  resolved <- case target <|> hostNativeTarget of
    Just found -> pure found
    Nothing -> do
      TIO.hPutStrLn stderr "This host is not a supported target; pass --target"
      exitFailure
  runExplore ExploreOptions {exploreInput = input, exploreLevel = level, exploreTarget = resolved}
runCommand (FcPasses paths level shown) = do
  (program, roots) <- case paths of
    [path] -> do
      loaded <- Fc.readProgramFile path
      case loaded of
        Left message -> TIO.hPutStrLn stderr message >> exitFailure
        Right program -> pure (program, Nothing)
    _ -> do
      programs <- readPrograms paths
      let merged = Fc.pruneProgram [entryName] (demoteToEntry entryName (Fc.mergePrograms programs))
      TIO.hPutStrLn stderr ("merged " <> T.pack (show (length programs)) <> " modules, " <> T.pack (show (length (Fc.programDecls merged))) <> " reachable declarations")
      pure (merged, Just [entryName])
  let passes = planPasses (optimizationPlan True level)
      showDecls label next =
        mapM_
          (\(name, text) -> when (maybe False ((`elem` map T.pack shown) . Fc.nameText) name) (TIO.putStrLn ("-- " <> label) >> TIO.putStrLn text))
          (Fc.renderProgramSections next)
      check label next = do
        showDecls label next
        -- An unused import is not a defect of the program: the merge keeps
        -- the imports of every module.
        let errors = [lintError | lintError <- Fc.lintProgram next, not (isUnusedImport lintError)]
            isUnusedImport lintError = case lintError of
              Fc.UnusedImport {} -> True
              _ -> False
        unless (null errors) $ do
          TIO.hPutStrLn stderr ("lint failed " <> label <> ":")
          mapM_ (TIO.hPutStrLn stderr . ("  " <>) . T.pack . show) errors
          let failing = [T.pack (takeWhile (/= ':') context) | Fc.UnevaluatedStrictField context _ _ <- errors]
          mapM_
            (\(name, text) -> when (maybe False ((`elem` failing) . Fc.nameText) name) (TIO.putStrLn text))
            (Fc.renderProgramSections next)
      step current pass = do
        let (next, report) = Fc.runPass roots pass current
        TIO.hPutStrLn stderr (Fc.reportPass report <> ": size " <> T.pack (show (Fc.reportBefore report)) <> " -> " <> T.pack (show (Fc.reportAfter report)))
        check ("after " <> Fc.reportPass report) next
        pure next
  check "before the passes" program
  foldM_ step program passes
runCommand (FcPrint path) = do
  loaded <- Fc.readProgramFile path
  case loaded of
    Left message -> do
      TIO.hPutStrLn stderr message
      exitFailure
    Right program -> TIO.putStrLn (Fc.renderProgram program)
