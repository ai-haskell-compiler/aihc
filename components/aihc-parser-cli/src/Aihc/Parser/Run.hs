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

import Aihc.Parser (ParserConfig (..), defaultConfig, formatParseErrors, parseModule)
import Aihc.Parser.Lex (lexModuleTokensWithExtensions, readModuleHeaderPragmas)
import Aihc.Parser.Shorthand (Shorthand (..))
import Aihc.Parser.Syntax (ExtensionSetting (..), Module, parseExtensionSettingName)
import Aihc.Parser.Syntax qualified as Syntax
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Options.Applicative
import Prettyprinter (defaultLayoutOptions, layoutPretty, pretty)
import Prettyprinter.Render.String (renderString)
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

-- | Output format: AST shorthand (default) or pretty-printed source.
data OutputFormat = OutputShorthand | OutputPretty
  deriving (Eq, Show)

data Options = Options
  { optMode :: !Mode,
    optOutputFormat :: !OutputFormat,
    optExtensions :: ![ExtensionSetting]
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
            ModeParse -> runParseMode (optOutputFormat opts) extensions stdin
            ModeLex -> runLexMode extensions stdin
    Failure failure ->
      let (msg, exitCode) = renderFailure failure "aihc-parser"
       in if exitCode == ExitSuccess
            then CLIResult exitCode (T.pack msg) ""
            else CLIResult exitCode "" (T.pack msg)
    CompletionInvoked _ ->
      CLIResult ExitSuccess "" ""

-- | Run in parse mode: parse a Haskell module and output the AST.
runParseMode :: OutputFormat -> [ExtensionSetting] -> Text -> CLIResult
runParseMode outputFormat extensionSettings stdin =
  let (errs, modu) = parseModule cfg stdin
   in if null errs
        then CLIResult ExitSuccess (formatOutput outputFormat modu <> "\n") ""
        else CLIResult (ExitFailure 1) "" (T.pack (formatParseErrors (parserSourceName cfg) (Just stdin) errs))
  where
    headerPragmas = readModuleHeaderPragmas stdin
    defaultEdition = fromMaybe Syntax.Haskell2010Edition (Syntax.editionFromExtensionSettings extensionSettings)
    edition = fromMaybe defaultEdition (Syntax.headerLanguageEdition headerPragmas)
    finalExts = Syntax.effectiveExtensions edition (extensionSettings ++ Syntax.headerExtensionSettings headerPragmas)
    cfg = defaultConfig {parserExtensions = finalExts, parserSourceName = "<stdin>"}

formatOutput :: OutputFormat -> Module -> Text
formatOutput OutputShorthand modu = T.pack (show (shorthand modu))
formatOutput OutputPretty modu = T.pack (renderString (layoutPretty defaultLayoutOptions (pretty modu)))

-- | Run in lex mode: tokenize Haskell source and output the token stream.
runLexMode :: [ExtensionSetting] -> Text -> CLIResult
runLexMode extensionSettings stdin =
  let headerPragmas = readModuleHeaderPragmas stdin
      edition = fromMaybe Syntax.Haskell2010Edition (Syntax.headerLanguageEdition headerPragmas)
      finalExts = Syntax.effectiveExtensions edition (extensionSettings ++ Syntax.headerExtensionSettings headerPragmas)
      tokens = lexModuleTokensWithExtensions finalExts stdin
   in CLIResult ExitSuccess (T.unlines (map (T.pack . show . shorthand) tokens)) ""

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
    <*> outputFormatOption
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

outputFormatOption :: Parser OutputFormat
outputFormatOption =
  flag
    OutputShorthand
    OutputPretty
    ( long "pretty"
        <> help "Pretty-print the parsed module (output Haskell source code instead of AST shorthand)"
    )

extensionOption :: Parser ExtensionSetting
extensionOption =
  option
    parseExtensionSetting
    ( short 'X'
        <> metavar "EXTENSION"
        <> help "Enable a language extension (e.g., -XNegativeLiterals)"
    )

parseExtensionSetting :: ReadM ExtensionSetting
parseExtensionSetting = eitherReader $ \s ->
  case parseExtensionSettingName (T.pack s) of
    Just setting -> Right setting
    Nothing -> Left ("Unknown extension: " <> s)
