{-# LANGUAGE OverloadedStrings #-}

module Aihc.Dev.Snippet
  ( SnippetOpts (..),
    SnippetReport (..),
    ReportLine (..),
    ReportTone (..),
    ParseComparison (..),
    parseExtensionSettingArg,
    runSnippet,
    analyzeSnippet,
    buildSnippetReport,
    renderSnippetReport,
    renderSnippetReportColored,
  )
where

import Aihc.Cpp (resultOutput)
import Aihc.Parser (ParserConfig (..), defaultConfig, parseModule)
import Aihc.Parser.Lex (readModuleHeaderPragmas)
import Aihc.Parser.Parens (addModuleParens)
import Aihc.Parser.Syntax
  ( ExtensionSetting,
    LanguageEdition (Haskell2010Edition),
    Module,
    editionFromExtensionSettings,
    effectiveExtensions,
    headerExtensionSettings,
    headerLanguageEdition,
    parseExtensionSettingName,
    stripAnnotations,
  )
import Control.Monad
import CppSupport (preprocessForParserWithoutIncludesIfEnabled)
import Data.List (dropWhileEnd, intercalate)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GhcOracle (oracleModuleAstFingerprint)
import ParserValidation (ValidationError (..), formatDiff, validateParser)
import Prettyprinter (Pretty (..), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import System.Exit (exitFailure)
import System.IO (hIsTerminalDevice, stdout)

data SnippetOpts = SnippetOpts
  { snippetExtensions :: [ExtensionSetting],
    snippetFile :: Maybe FilePath
  }

data ParseComparison
  = BothReject
  | GhcRejectsAihcAccepts
  | GhcAcceptsAihcRejects
  | BothAccept
  deriving (Eq, Show)

data ReportTone
  = ReportBug
  | ReportNonBug
  deriving (Eq, Show)

data ReportLine = ReportLine
  { reportLineTone :: ReportTone,
    reportLineText :: String
  }
  deriving (Eq, Show)

data SnippetReport = SnippetReport
  { reportStatusLines :: [ReportLine],
    reportHasFailure :: Bool
  }
  deriving (Eq, Show)

runSnippet :: SnippetOpts -> IO ()
runSnippet opts = do
  let sourceTag = fromMaybe "<stdin>" (snippetFile opts)
  source <- maybe TIO.getContents TIO.readFile (snippetFile opts)
  let report = analyzeSnippet sourceTag (snippetExtensions opts) source
  useColor <- hIsTerminalDevice stdout
  putStr (renderSnippetReportColored useColor report)
  Control.Monad.when (reportHasFailure report) exitFailure

parseExtensionSettingArg :: String -> Either String ExtensionSetting
parseExtensionSettingArg raw =
  case parseExtensionSettingName (T.pack raw) of
    Just setting -> Right setting
    Nothing -> Left ("Unknown extension: " <> raw)

analyzeSnippet :: FilePath -> [ExtensionSetting] -> Text -> SnippetReport
analyzeSnippet sourceTag cliExtensions source =
  let preprocessed = preprocessForParserWithoutIncludesIfEnabled cliExtensions [] sourceTag [] source
      source' = resultOutput preprocessed
      headerPragmas = readModuleHeaderPragmas source'
      defaultEdition = fromMaybe Haskell2010Edition (editionFromExtensionSettings cliExtensions)
      edition = fromMaybe defaultEdition (headerLanguageEdition headerPragmas)
      extensionSettings = cliExtensions ++ headerExtensionSettings headerPragmas
      ghcAccepts = either (const False) (const True) (oracleModuleAstFingerprint sourceTag edition extensionSettings source)
      parserModule = parseWithAihc sourceTag edition extensionSettings source'
      comparison = compareParseResults ghcAccepts parserModule
      validationFailure =
        case comparison of
          BothAccept -> fmap validationErrorMessage (validateParser sourceTag edition extensionSettings source')
          _ -> Nothing
      parensDiff =
        case comparison of
          BothAccept -> parserModule >>= parsedSnippetParensDiff source
          _ -> Nothing
   in buildSnippetReport comparison validationFailure parensDiff

buildSnippetReport :: ParseComparison -> Maybe String -> Maybe String -> SnippetReport
buildSnippetReport comparison validationFailure parensDiff =
  let statusLines =
        comparisonLines comparison
          ++ roundtripLines comparison validationFailure
          ++ parensLines comparison parensDiff
      hasFailure = comparison /= BothAccept || isJust validationFailure || isJust parensDiff
   in SnippetReport statusLines hasFailure

renderSnippetReport :: SnippetReport -> String
renderSnippetReport = renderSnippetReportColored False

renderSnippetReportColored :: Bool -> SnippetReport -> String
renderSnippetReportColored useColor report =
  intercalate "\n" (map (renderReportLine useColor) (reportStatusLines report))
    <> if null (reportStatusLines report) then "" else "\n"

renderReportLine :: Bool -> ReportLine -> String
renderReportLine useColor line =
  let text = dropWhileEnd (== '\n') (reportLineText line)
   in if useColor
        then colorForTone (reportLineTone line) <> text <> ansiReset
        else text

colorForTone :: ReportTone -> String
colorForTone tone =
  case tone of
    ReportBug -> "\ESC[31m"
    ReportNonBug -> "\ESC[32m"

ansiReset :: String
ansiReset = "\ESC[0m"

compareParseResults :: Bool -> Maybe Module -> ParseComparison
compareParseResults ghcAccepts parserModule =
  case (ghcAccepts, parserModule) of
    (False, Nothing) -> BothReject
    (False, Just _) -> GhcRejectsAihcAccepts
    (True, Nothing) -> GhcAcceptsAihcRejects
    (True, Just _) -> BothAccept

comparisonLines :: ParseComparison -> [ReportLine]
comparisonLines comparison =
  case comparison of
    BothReject ->
      [ nonBugLine "GHC Parses: Parse failure",
        nonBugLine "AIHC Parses: Parse failure (Expected)"
      ]
    GhcRejectsAihcAccepts ->
      [ nonBugLine "GHC Parses: Parse failure",
        bugLine "AIHC Parses: OK (Bug: GHC/AIHC mismatch)"
      ]
    GhcAcceptsAihcRejects ->
      [ nonBugLine "GHC Parses: OK",
        bugLine "AIHC Parses: Parse failure (Bug: GHC/AIHC mismatch)"
      ]
    BothAccept ->
      [ nonBugLine "GHC Parses: OK",
        nonBugLine "AIHC Parses: OK"
      ]

roundtripLines :: ParseComparison -> Maybe String -> [ReportLine]
roundtripLines comparison validationFailure =
  case (comparison, validationFailure) of
    (BothAccept, Nothing) -> []
    (BothAccept, Just message) -> [bugLine ("Roundtrip: Bug found:\n" <> message)]
    (_, _) -> []

parensLines :: ParseComparison -> Maybe String -> [ReportLine]
parensLines comparison parensDiff =
  case comparison of
    BothAccept ->
      case parensDiff of
        Nothing -> [nonBugLine "Minimal Parentheses: OK"]
        Just message -> [bugLine ("Minimal Parentheses: " <> message)]
    _ -> [nonBugLine "Minimal Parentheses: Skipped"]

bugLine :: String -> ReportLine
bugLine = ReportLine ReportBug

nonBugLine :: String -> ReportLine
nonBugLine = ReportLine ReportNonBug

parseWithAihc :: FilePath -> LanguageEdition -> [ExtensionSetting] -> Text -> Maybe Module
parseWithAihc sourceTag edition extensionSettings source =
  let config =
        defaultConfig
          { parserSourceName = sourceTag,
            parserExtensions = effectiveExtensions edition extensionSettings
          }
      (errs, modu) = parseModule config source
   in case errs of
        [] -> Just modu
        _ -> Nothing

parsedSnippetParensDiff :: Text -> Module -> Maybe String
parsedSnippetParensDiff source modu =
  let moduWithParens = addModuleParens modu
   in if stripAnnotations moduWithParens == stripAnnotations modu
        then Nothing
        else
          let renderedWithParens = renderStrict (layoutPretty defaultLayoutOptions (pretty moduWithParens))
           in Just (formatParensDiff source renderedWithParens)

formatParensDiff :: Text -> Text -> String
formatParensDiff before after =
  let header = "Bug found:"
   in case formatDiff before after of
        Nothing -> header
        Just diffChunk ->
          intercalate
            "\n"
            [ header,
              "Changed section:",
              T.unpack (T.stripEnd diffChunk)
            ]
