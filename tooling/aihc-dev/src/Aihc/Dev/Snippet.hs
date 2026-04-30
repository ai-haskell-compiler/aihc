{-# LANGUAGE OverloadedStrings #-}

module Aihc.Dev.Snippet
  ( SnippetOpts (..),
    SnippetReport (..),
    ParseComparison (..),
    parseExtensionSettingArg,
    runSnippet,
    analyzeSnippet,
    buildSnippetReport,
    renderSnippetReport,
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
import Data.List (intercalate)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GhcOracle (oracleModuleAstFingerprint)
import ParserValidation (ValidationError (..), validateParser)
import System.Exit (exitFailure)

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

data SnippetReport = SnippetReport
  { reportStatusLines :: [String],
    reportHasBug :: Bool
  }
  deriving (Eq, Show)

runSnippet :: SnippetOpts -> IO ()
runSnippet opts = do
  let sourceTag = fromMaybe "<stdin>" (snippetFile opts)
  source <- maybe TIO.getContents TIO.readFile (snippetFile opts)
  let report = analyzeSnippet sourceTag (snippetExtensions opts) source
  putStr (renderSnippetReport report)
  Control.Monad.when (reportHasBug report) exitFailure

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
      parensChanged = maybe False parsedSnippetNeedsParens parserModule
   in buildSnippetReport comparison validationFailure parensChanged

buildSnippetReport :: ParseComparison -> Maybe String -> Bool -> SnippetReport
buildSnippetReport comparison validationFailure parensChanged =
  let statusLines =
        comparisonLines comparison
          ++ maybe [] pure validationFailure
          ++ ["Bug found: Parens.addModuleParens adds parentheses to the parsed snippet." | parensChanged]
      hasBug = comparison /= BothReject && comparison /= BothAccept || isJust validationFailure || parensChanged
   in SnippetReport statusLines hasBug

renderSnippetReport :: SnippetReport -> String
renderSnippetReport report =
  intercalate "\n" (reportStatusLines report)
    <> if null (reportStatusLines report) then "" else "\n"

compareParseResults :: Bool -> Maybe Module -> ParseComparison
compareParseResults ghcAccepts parserModule =
  case (ghcAccepts, parserModule) of
    (False, Nothing) -> BothReject
    (False, Just _) -> GhcRejectsAihcAccepts
    (True, Nothing) -> GhcAcceptsAihcRejects
    (True, Just _) -> BothAccept

comparisonLines :: ParseComparison -> [String]
comparisonLines comparison =
  case comparison of
    BothReject ->
      ["Snippet fails to parse with both GHC and aihc-parser."]
    GhcRejectsAihcAccepts ->
      ["Bug found: code rejected by GHC but parsed by aihc-parser."]
    GhcAcceptsAihcRejects ->
      ["Bug found: code rejected by aihc-parser but parsed by GHC."]
    BothAccept -> []

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

parsedSnippetNeedsParens :: Module -> Bool
parsedSnippetNeedsParens modu =
  stripAnnotations (addModuleParens modu) /= stripAnnotations modu
