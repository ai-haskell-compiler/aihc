{-# LANGUAGE OverloadedStrings #-}

-- | Lexer runner with full command-line argument processing.
-- This module provides a pure function that handles argument parsing,
-- enabling complete CLI testing without IO.
module Aihc.Parser.Run.Lexer
  ( runLexer,
    CLIResult (..),
  )
where

import Aihc.Parser.Lex (lexModuleTokensWithExtensions)
import Aihc.Parser.Shorthand (Shorthand (..))
import Aihc.Parser.Syntax (Extension, ExtensionSetting (..), parseExtensionSettingName)
import Data.Text (Text)
import Data.Text qualified as T
import Options.Applicative
import System.Exit (ExitCode (..))

-- | Result of running a CLI command.
data CLIResult = CLIResult
  { cliExitCode :: !ExitCode,
    cliStdout :: !Text,
    cliStderr :: !Text
  }
  deriving (Eq, Show)

newtype Options = Options
  { optExtensions :: [Extension]
  }

-- | Run the lexer CLI with given arguments and stdin.
-- Returns exit code, stdout, and stderr.
-- This is a pure function that can be tested without IO.
--
-- Note: File arguments are parsed but ignored; input is always from stdin parameter.
runLexer :: [String] -> Text -> CLIResult
runLexer args stdin =
  case execParserPure defaultPrefs optionsParser args of
    Success opts ->
      let extensions = optExtensions opts
          tokens = lexModuleTokensWithExtensions extensions stdin
          output = T.unlines (map (T.pack . show . shorthand) tokens)
       in CLIResult ExitSuccess output ""
    Failure failure ->
      let (msg, _exitCode) = renderFailure failure "aihc-lexer"
       in CLIResult (ExitFailure 1) "" (T.pack msg)
    CompletionInvoked _ ->
      CLIResult ExitSuccess "" ""

optionsParser :: ParserInfo Options
optionsParser =
  info
    (optionsP <**> helper)
    ( fullDesc
        <> progDesc "Lex Haskell source code and pretty-print the token stream"
        <> header "aihc-lexer - Haskell lexer"
    )

optionsP :: Parser Options
optionsP =
  Options
    <$> many extensionOption
    <* optional (argument (str :: ReadM String) (metavar "FILE" <> help "Input file (reads stdin if omitted)"))

extensionOption :: Parser Extension
extensionOption =
  option
    parseExtension
    ( short 'X'
        <> metavar "EXTENSION"
        <> help "Enable a language extension (e.g., -XNegativeLiterals)"
    )

parseExtension :: ReadM Extension
parseExtension = eitherReader $ \s ->
  case parseExtensionSettingName (T.pack s) of
    Just (EnableExtension ext) -> Right ext
    Just (DisableExtension _) -> Left ("Cannot disable extension with -X: " <> s)
    Nothing -> Left ("Unknown extension: " <> s)
