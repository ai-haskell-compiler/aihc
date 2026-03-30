{-# LANGUAGE OverloadedStrings #-}

-- | Unified CLI runner for aihc-parser with lexing and parsing modes.
--
-- This module provides a pure function that handles argument parsing,
-- enabling complete CLI testing without IO. By default, the CLI parses
-- Haskell source code. Use @--lex@ to switch to lexer-only mode.
module Aihc.Parser.Run
  ( runCLI,
    CLIResult (..),
  )
where

import Aihc.Parser (ParseResult (..), ParserConfig (..), defaultConfig, errorBundlePretty, parseModule)
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

-- | CLI mode: parse (default) or lex.
data Mode = ModeParse | ModeLex
  deriving (Eq, Show)

data Options = Options
  { optMode :: !Mode,
    optExtensions :: ![Extension]
  }

-- | Run the CLI with given arguments and stdin.
-- Returns exit code, stdout, and stderr.
-- This is a pure function that can be tested without IO.
--
-- By default, parses Haskell source code. Use @--lex@ to switch to lexer mode.
--
-- Note: File arguments are parsed but ignored; input is always from stdin parameter.
runCLI :: [String] -> Text -> CLIResult
runCLI args stdin =
  case execParserPure defaultPrefs optionsParser args of
    Success opts ->
      let extensions = optExtensions opts
       in case optMode opts of
            ModeParse -> runParseMode extensions stdin
            ModeLex -> runLexMode extensions stdin
    Failure failure ->
      let (msg, exitCode) = renderFailure failure "aihc-parser"
       in if exitCode == ExitSuccess
            then CLIResult exitCode (T.pack msg) ""
            else CLIResult exitCode "" (T.pack msg)
    CompletionInvoked _ ->
      CLIResult ExitSuccess "" ""

-- | Run in parse mode: parse a Haskell module and output the AST.
runParseMode :: [Extension] -> Text -> CLIResult
runParseMode extensions stdin =
  let cfg = defaultConfig {parserExtensions = extensions}
   in case parseModule cfg stdin of
        ParseOk modu ->
          CLIResult ExitSuccess (T.pack (show (shorthand modu)) <> "\n") ""
        ParseErr bundle ->
          CLIResult (ExitFailure 1) "" (T.pack (errorBundlePretty (Just stdin) bundle))

-- | Run in lex mode: tokenize Haskell source and output the token stream.
runLexMode :: [Extension] -> Text -> CLIResult
runLexMode extensions stdin =
  let tokens = lexModuleTokensWithExtensions extensions stdin
      output = T.unlines (map (T.pack . show . shorthand) tokens)
   in CLIResult ExitSuccess output ""

optionsParser :: ParserInfo Options
optionsParser =
  info
    (optionsP <**> helper)
    ( fullDesc
        <> progDesc "Parse or lex Haskell source code"
        <> header "aihc-parser - Haskell parser and lexer"
    )

optionsP :: Parser Options
optionsP =
  Options
    <$> modeOption
    <*> many extensionOption
    <* optional (argument (str :: ReadM String) (metavar "FILE" <> help "Input file (reads stdin if omitted)"))

modeOption :: Parser Mode
modeOption =
  flag
    ModeParse
    ModeLex
    ( long "lex"
        <> help "Lex only, do not parse (output token stream)"
    )

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
