{-# LANGUAGE OverloadedStrings #-}

module Aihc.Fmt.CLI
  ( Mode (..),
    Options (..),
    CLIResult (..),
    main,
    runCLI,
  )
where

import Aihc.Fmt
import Aihc.Parser.Syntax (ExtensionSetting, parseExtensionSettingName)
import Control.Exception (IOException, try)
import Control.Monad (foldM)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Options.Applicative
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import System.IO qualified as IO

data Mode
  = ModeStdout
  | ModeInplace
  | ModeCheck
  deriving (Eq, Show)

data Options = Options
  { optMode :: !Mode,
    optExtensions :: ![ExtensionSetting],
    optFiles :: ![FilePath]
  }
  deriving (Eq, Show)

data CLIResult = CLIResult
  { cliExitCode :: !ExitCode,
    cliStdout :: !Text,
    cliStderr :: !Text
  }
  deriving (Eq, Show)

main :: IO ()
main = do
  args <- getArgs
  parsed <- parseArgs args
  case parsed of
    Left result -> finish result
    Right opts -> do
      stdin <-
        if null (optFiles opts)
          then TIO.getContents
          else pure ""
      result <- runCLI TIO.readFile TIO.writeFile opts stdin
      finish result

finish :: CLIResult -> IO ()
finish result = do
  TIO.putStr (cliStdout result)
  TIO.hPutStr IO.stderr (cliStderr result)
  exitWith (cliExitCode result)

runCLI ::
  (FilePath -> IO Text) ->
  (FilePath -> Text -> IO ()) ->
  Options ->
  Text ->
  IO CLIResult
runCLI readFileText writeFileText opts stdin =
  case optFiles opts of
    [] -> runStdin opts stdin
    files -> runFiles readFileText writeFileText opts files

runStdin :: Options -> Text -> IO CLIResult
runStdin opts stdin =
  case formatText formatOpts "<stdin>" stdin of
    Left err -> pure (CLIResult (ExitFailure 1) "" (formatErrorMessage err <> "\n"))
    Right formatted ->
      case optMode opts of
        ModeStdout -> pure (CLIResult ExitSuccess formatted "")
        ModeCheck ->
          if formatted == stdin
            then pure (CLIResult ExitSuccess "" "")
            else pure (CLIResult (ExitFailure 1) "" "<stdin>: needs formatting\n")
        ModeInplace ->
          pure (CLIResult (ExitFailure 1) "" "aihc-fmt: --mode inplace requires file arguments\n")
  where
    formatOpts = FormatOptions {formatExtensions = optExtensions opts}

runFiles ::
  (FilePath -> IO Text) ->
  (FilePath -> Text -> IO ()) ->
  Options ->
  [FilePath] ->
  IO CLIResult
runFiles readFileText writeFileText opts =
  foldM
    (runOneFile readFileText writeFileText opts)
    (CLIResult ExitSuccess "" "")

runOneFile ::
  (FilePath -> IO Text) ->
  (FilePath -> Text -> IO ()) ->
  Options ->
  CLIResult ->
  FilePath ->
  IO CLIResult
runOneFile readFileText writeFileText opts acc path = do
  eInput <- try (readFileText path)
  case eInput of
    Left err -> pure (appendIOException path err acc)
    Right input -> do
      let formatOpts = FormatOptions {formatExtensions = optExtensions opts}
      case formatText formatOpts path input of
        Left err ->
          pure
            acc
              { cliExitCode = ExitFailure 1,
                cliStderr = cliStderr acc <> formatErrorMessage err <> "\n"
              }
        Right formatted ->
          case optMode opts of
            ModeStdout ->
              pure acc {cliStdout = cliStdout acc <> formatted}
            ModeCheck ->
              if formatted == input
                then pure acc
                else
                  pure
                    acc
                      { cliExitCode = ExitFailure 1,
                        cliStderr = cliStderr acc <> T.pack path <> ": needs formatting\n"
                      }
            ModeInplace ->
              if formatted == input
                then pure acc
                else do
                  eWrite <- try (writeFileText path formatted)
                  case eWrite of
                    Left err -> pure (appendIOException path err acc)
                    Right () -> pure acc

appendIOException :: FilePath -> IOException -> CLIResult -> CLIResult
appendIOException path err acc =
  acc
    { cliExitCode = ExitFailure 1,
      cliStderr = cliStderr acc <> T.pack path <> ": " <> T.pack (show err) <> "\n"
    }

parseArgs :: [String] -> IO (Either CLIResult Options)
parseArgs args =
  case execParserPure defaultPrefs optionsParser args of
    Success opts -> pure (Right opts)
    Failure failure ->
      let (msg, exitCode) = renderFailure failure "aihc-fmt"
          msgText = T.pack msg <> "\n"
       in if exitCode == ExitSuccess
            then pure (Left (CLIResult exitCode msgText ""))
            else pure (Left (CLIResult exitCode "" msgText))
    CompletionInvoked completion -> do
      output <- execCompletion completion "aihc-fmt"
      pure (Left (CLIResult ExitSuccess (T.pack output) ""))

optionsParser :: ParserInfo Options
optionsParser =
  info
    (optionsP <**> helper)
    ( fullDesc
        <> progDesc "Format Haskell source with aihc-parser"
        <> header "aihc-fmt - Haskell formatter"
    )

optionsP :: Parser Options
optionsP =
  Options
    <$> modeOption
    <*> many extensionOption
    <*> many (argument str (metavar "FILE"))

modeOption :: Parser Mode
modeOption =
  option
    parseMode
    ( long "mode"
        <> metavar "MODE"
        <> value ModeStdout
        <> showDefaultWith renderMode
        <> help "Output mode: stdout, inplace, or check"
    )

parseMode :: ReadM Mode
parseMode = eitherReader $ \raw ->
  case raw of
    "stdout" -> Right ModeStdout
    "inplace" -> Right ModeInplace
    "check" -> Right ModeCheck
    _ -> Left ("unknown mode: " <> raw)

renderMode :: Mode -> String
renderMode mode =
  case mode of
    ModeStdout -> "stdout"
    ModeInplace -> "inplace"
    ModeCheck -> "check"

extensionOption :: Parser ExtensionSetting
extensionOption =
  option
    parseExtensionSetting
    ( short 'X'
        <> metavar "EXTENSION"
        <> help "Enable a language extension"
    )

parseExtensionSetting :: ReadM ExtensionSetting
parseExtensionSetting = eitherReader $ \raw ->
  case parseExtensionSettingName (T.pack raw) of
    Just setting -> Right setting
    Nothing -> Left ("unknown extension: " <> raw)
