module Main (main) where

import Aihc.Dev.ExtractHi (extractPackage)
import Aihc.Dev.ExtractHi.Compare (comparePackageSubset, renderCoreLibProgressReports, renderInterfaceMismatch, runCoreLibProgressReports)
import Aihc.Dev.ExtractHi.ToResolveIface (toResolveIface)
import Aihc.Dev.Fuzz qualified as Fuzz
import Aihc.Dev.Fuzz.CLI qualified as FuzzCLI
import Aihc.Dev.Golden.Update qualified as GoldenUpdate
import Aihc.Dev.HackageTester.Run qualified as HackageTesterRun
import Aihc.Dev.Parser.Bench.CLI qualified as ParserBenchCLI
import Aihc.Dev.Parser.Bench.Run qualified as ParserBenchRun
import Aihc.Dev.Parser.CLI qualified as ParserCLI
import Aihc.Dev.Parser.Run qualified as ParserRun
import Aihc.Dev.Snippet (SnippetOpts (..), parseExtensionSettingArg, runSnippet)
import Aihc.Parser.Syntax (ExtensionSetting)
import Data.Aeson (encode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy qualified as BL
import Data.Yaml qualified as Yaml
import HackageProgress.CLI qualified as ParserHackageProgressCLI
import HackageProgress.Run qualified as ParserHackageProgressRun
import HackageTester.CLI qualified as HackageTesterCLI
import Options.Applicative
import ResolvePackage qualified as RP
import ResolveStackageProgress qualified as RSP
import StackageProgress.CLI qualified as ParserStackageProgressCLI
import StackageProgress.Run qualified as ParserStackageProgressRun
import System.Directory (createDirectoryIfMissing)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory)
import TcStackageProgress qualified as TSP

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
  | CoreLibsProgress
  | ExtractResolveIface ExtractResolveIfaceOpts
  | Fuzz FuzzCLI.Command
  | Snippet SnippetOpts
  | Parser ParserRun.Options
  | ParserBench ParserBenchCLI.Options
  | HackageTester HackageTesterCLI.Options
  | ParserStackageProgress ParserStackageProgressCLI.Options
  | ParserHackageProgress ParserHackageProgressCLI.Options
  | ResolvePackage RP.Options
  | ResolveStackageProgress RSP.Options
  | TcStackageProgress TSP.Options
  | UpdateGoldens GoldenUpdate.Options

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
              (pure CoreLibsProgress <**> helper)
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
              (progDesc "Continuously run QuickCheck properties in parallel")
          )
        <> command
          "snippet"
          ( info
              (Snippet <$> snippetParser <**> helper)
              (progDesc "Analyze a Haskell snippet using GHC and aihc-parser")
          )
        <> command
          "parser"
          ( info
              (Parser <$> ParserCLI.optionsParser <**> helper)
              (progDesc "Parse, lex, or preprocess Haskell source")
          )
        <> command
          "parser-bench"
          ( info
              (ParserBench <$> ParserBenchCLI.optionsParser <**> helper)
              (progDesc "Generate parser benchmark tarballs and run parser benchmarks")
          )
        <> command
          "hackage-tester"
          ( info
              (HackageTester <$> HackageTesterCLI.optionsParser <**> helper)
              (progDesc "Test parser behavior on a Hackage package")
          )
        <> command
          "parser-stackage-progress"
          ( info
              (ParserStackageProgress <$> ParserStackageProgressCLI.optionsParser <**> helper)
              (progDesc "Test parser on Stackage snapshot packages")
          )
        <> command
          "parser-hackage-progress"
          ( info
              (ParserHackageProgress <$> ParserHackageProgressCLI.optionsParser <**> helper)
              (progDesc "Test parser on all packages in Hackage")
          )
        <> command
          "resolve"
          ( info
              (ResolvePackage <$> RP.optionsParser <**> helper)
              (progDesc "Resolve names in a Stackage package and print its resolver interface")
          )
        <> command
          "resolve-stackage-progress"
          ( info
              (ResolveStackageProgress <$> RSP.optionsParser <**> helper)
              (progDesc "Test name resolver on Stackage snapshot packages")
          )
        <> command
          "tc-stackage-progress"
          ( info
              (TcStackageProgress <$> TSP.optionsParser <**> helper)
              (progDesc "Test type checker on Stackage snapshot packages")
          )
        <> command
          "update-goldens"
          ( info
              (UpdateGoldens <$> GoldenUpdate.optionsParser <**> helper)
              (progDesc "Refresh golden fixture outputs from the current implementation")
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

snippetParser :: Parser SnippetOpts
snippetParser =
  SnippetOpts
    <$> many
      ( option
          parseExtensionSetting
          ( short 'X'
              <> metavar "EXTENSION"
              <> help "Enable a language extension (for example, -XTypeApplications)"
          )
      )
    <*> optional
      ( strArgument
          ( metavar "FILE"
              <> help "Snippet file to analyze (reads stdin if omitted)"
          )
      )

parseExtensionSetting :: ReadM ExtensionSetting
parseExtensionSetting =
  eitherReader parseExtensionSettingArg

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
runCommand CoreLibsProgress =
  putStr . renderCoreLibProgressReports =<< runCoreLibProgressReports
runCommand (ExtractResolveIface opts) = do
  pkg <- extractPackage (eriPackage opts)
  let resolveIface = toResolveIface pkg
      outputPath = eriOutput opts
  createDirectoryIfMissing True (takeDirectory outputPath)
  BL.writeFile outputPath (encodePretty resolveIface)
runCommand (Fuzz fuzzCommand) =
  Fuzz.runCommand fuzzCommand
runCommand (Snippet opts) =
  runSnippet opts
runCommand (Parser opts) =
  ParserCLI.run opts
runCommand (ParserBench opts) =
  ParserBenchRun.run opts
runCommand (HackageTester opts) =
  HackageTesterRun.run opts
runCommand (ParserStackageProgress opts) =
  ParserStackageProgressRun.run opts
runCommand (ParserHackageProgress opts) =
  ParserHackageProgressRun.run opts
runCommand (ResolvePackage opts) =
  RP.run opts
runCommand (ResolveStackageProgress opts) =
  RSP.run opts
runCommand (TcStackageProgress opts) =
  TSP.run opts
runCommand (UpdateGoldens opts) =
  GoldenUpdate.run opts
