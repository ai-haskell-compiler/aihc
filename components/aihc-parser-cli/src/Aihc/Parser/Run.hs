{-# LANGUAGE OverloadedStrings #-}

-- | Unified CLI runner for aihc-parser with lexing and parsing modes.
--
-- This module provides a pure function that handles argument parsing,
-- enabling complete CLI testing without IO. By default, the CLI parses
-- Haskell source code. Use @--lex@ to switch to lexer-only mode.
-- Use @--cpp@ to run only the C preprocessor.
module Aihc.Parser.Run
  ( runCLI,
    CLIResult (..),
  )
where

import Aihc.Cpp
  ( Diagnostic (..),
    IncludeKind (..),
    IncludeRequest (..),
    Result (..),
    Severity (..),
    Step (..),
    preprocess,
  )
import Aihc.Cpp qualified as Cpp
import Aihc.Parser (ParserConfig (..), defaultConfig, formatParseErrors, parseModule)
import Aihc.Parser.Lex (lexModuleTokensWithExtensions, readModuleHeaderPragmas)
import Aihc.Parser.Shorthand (Shorthand (..))
import Aihc.Parser.Syntax
  ( Extension (..),
    ExtensionSetting (..),
    Module,
    parseExtensionSettingName,
  )
import Aihc.Parser.Syntax qualified as Syntax
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Options.Applicative
import Prettyprinter (defaultLayoutOptions, layoutPretty, pretty)
import Prettyprinter.Render.String (renderString)
import System.Exit (ExitCode (..))
import System.FilePath (normalise, takeDirectory, (</>))

-- | Result of running a CLI command.
data CLIResult = CLIResult
  { cliExitCode :: !ExitCode,
    cliStdout :: !Text,
    cliStderr :: !Text
  }
  deriving (Eq, Show)

-- | CLI mode: parse (default), lex, or cpp-only.
data Mode = ModeParse | ModeLex | ModeCpp
  deriving (Eq, Show)

-- | Output format: AST shorthand (default) or pretty-printed source.
data OutputFormat = OutputShorthand | OutputPretty
  deriving (Eq, Show)

data Options = Options
  { optMode :: !Mode,
    optOutputFormat :: !OutputFormat,
    optExtensions :: ![ExtensionSetting]
  }

-- | Run the CLI with given arguments, stdin, and include map.
-- Returns exit code, stdout, and stderr.
-- This is a pure function that can be tested without IO.
--
-- By default, parses Haskell source code. Use @--lex@ to switch to lexer mode.
-- Use @--cpp@ to run only the preprocessor.
--
-- The include map should contain preloaded file contents keyed by normalized paths.
-- Files not in the map will fail to resolve during CPP preprocessing.
runCLI :: Map FilePath Text -> [String] -> Text -> CLIResult
runCLI includeMap args stdin =
  case execParserPure defaultPrefs optionsParser args of
    Success opts ->
      let extensions = optExtensions opts
       in case optMode opts of
            ModeCpp -> runCppMode includeMap stdin
            ModeParse -> runParseMode (optOutputFormat opts) extensions includeMap stdin
            ModeLex -> runLexMode extensions stdin
    Failure failure ->
      let (msg, exitCode) = renderFailure failure "aihc-parser"
       in if exitCode == ExitSuccess
            then CLIResult exitCode (T.pack msg) ""
            else CLIResult exitCode "" (T.pack msg)
    CompletionInvoked _ ->
      CLIResult ExitSuccess "" ""

-- | Run in CPP-only mode: preprocess and output the result.
runCppMode :: Map FilePath Text -> Text -> CLIResult
runCppMode includeMap stdin =
  let cfg =
        Cpp.defaultConfig
          { Cpp.configInputFile = "<stdin>",
            Cpp.configMacros = M.empty
          }
      step = preprocess cfg (TE.encodeUtf8 stdin)
      result = resolveCppStep includeMap step
   in if hasCppErrors result
        then CLIResult (ExitFailure 1) (resultOutput result) (formatCppDiagnostics (resultDiagnostics result))
        else CLIResult ExitSuccess (resultOutput result) ""

-- | Run in parse mode: parse a Haskell module and output the AST.
-- If CPP extension is enabled, preprocess first.
runParseMode :: OutputFormat -> [ExtensionSetting] -> Map FilePath Text -> Text -> CLIResult
runParseMode outputFormat extensionSettings includeMap stdin =
  let headerPragmas = readModuleHeaderPragmas stdin
      defaultEdition = fromMaybe Syntax.Haskell2010Edition (Syntax.editionFromExtensionSettings extensionSettings)
      edition = fromMaybe defaultEdition (Syntax.headerLanguageEdition headerPragmas)
      finalExts = Syntax.effectiveExtensions edition (extensionSettings ++ Syntax.headerExtensionSettings headerPragmas)
      cppEnabled = CPP `elem` finalExts
   in if cppEnabled
        then runParseModeWithCpp outputFormat finalExts includeMap stdin
        else runParseModeDirect outputFormat finalExts stdin

-- | Parse directly without CPP preprocessing.
runParseModeDirect :: OutputFormat -> [Extension] -> Text -> CLIResult
runParseModeDirect outputFormat finalExts stdin =
  let (errs, modu) = parseModule cfg stdin
   in if null errs
        then CLIResult ExitSuccess (formatOutput outputFormat modu <> "\n") ""
        else CLIResult (ExitFailure 1) "" (T.pack (formatParseErrors (parserSourceName cfg) (Just stdin) errs))
  where
    cfg = defaultConfig {parserExtensions = finalExts, parserSourceName = "<stdin>"}

-- | Parse with CPP preprocessing first.
runParseModeWithCpp :: OutputFormat -> [Extension] -> Map FilePath Text -> Text -> CLIResult
runParseModeWithCpp outputFormat finalExts includeMap stdin =
  let cfg =
        Cpp.defaultConfig
          { Cpp.configInputFile = "<stdin>",
            Cpp.configMacros = M.empty
          }
      cppStep = preprocess cfg (TE.encodeUtf8 stdin)
      cppResult = resolveCppStep includeMap cppStep
      cppDiagnostics = resultDiagnostics cppResult
      cppErrors = [d | d <- cppDiagnostics, diagSeverity d == Error]
   in if not (null cppErrors)
        then CLIResult (ExitFailure 1) "" (formatCppDiagnostics cppErrors)
        else
          let preprocessed = resultOutput cppResult
              (errs, modu) = parseModule parseCfg preprocessed
           in if null errs
                then CLIResult ExitSuccess (formatOutput outputFormat modu <> "\n") (formatCppDiagnostics [d | d <- cppDiagnostics, diagSeverity d == Warning])
                else CLIResult (ExitFailure 1) "" (T.pack (formatParseErrors (parserSourceName parseCfg) (Just preprocessed) errs))
  where
    parseCfg = defaultConfig {parserExtensions = finalExts, parserSourceName = "<stdin>"}

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

-- | Resolve all include requests in a CPP step, returning the final result.
resolveCppStep :: Map FilePath Text -> Step -> Result
resolveCppStep _ (Done result) = result
resolveCppStep includeMap (NeedInclude req k) =
  let resolved = resolveInclude includeMap req
   in resolveCppStep includeMap (k (fmap TE.encodeUtf8 resolved))

-- | Resolve an include request using the include map.
-- For local includes, first try relative to the including file, then search the map.
-- For system includes, search the map directly.
resolveInclude :: Map FilePath Text -> IncludeRequest -> Maybe Text
resolveInclude includeMap req =
  case includeKind req of
    IncludeLocal ->
      let relativePath = normalise (takeDirectory (includeFrom req) </> includePath req)
       in case M.lookup relativePath includeMap of
            Just contents -> Just contents
            Nothing -> M.lookup (includePath req) includeMap
    IncludeSystem ->
      M.lookup (includePath req) includeMap

-- | Format CPP diagnostics for stderr output.
formatCppDiagnostics :: [Diagnostic] -> Text
formatCppDiagnostics [] = ""
formatCppDiagnostics diags =
  T.unlines (map formatDiagnostic diags)
  where
    formatDiagnostic d =
      T.pack (diagFile d)
        <> ":"
        <> T.pack (show (diagLine d))
        <> ": "
        <> severityText (diagSeverity d)
        <> ": "
        <> diagMessage d
    severityText Error = "error"
    severityText Warning = "warning"

-- | Check if a CPP result contains any errors.
hasCppErrors :: Result -> Bool
hasCppErrors result = any (\d -> diagSeverity d == Error) (resultDiagnostics result)

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
    <* many includeOption
    <* optional (argument (str :: ReadM String) (metavar "FILE" <> help "Input file (reads stdin if omitted)"))

modeOption :: Parser Mode
modeOption =
  flag
    ModeParse
    ModeLex
    ( long "lex"
        <> help "Lex only, do not parse (output token stream)"
    )
    <|> flag'
      ModeCpp
      ( long "cpp"
          <> help "Run C preprocessor only (output preprocessed source)"
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

-- | Parse an --include=path argument.
includeOption :: Parser FilePath
includeOption =
  option
    str
    ( long "include"
        <> metavar "PATH"
        <> help "Add directory to include search path"
    )

parseExtensionSetting :: ReadM ExtensionSetting
parseExtensionSetting = eitherReader $ \s ->
  case parseExtensionSettingName (T.pack s) of
    Just setting -> Right setting
    Nothing -> Left ("Unknown extension: " <> s)
