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
import Aihc.Parser.Compat (dumpGhcAst, sameGhcAst, toGhcHsModuleDecls)
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
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHC.Hs (GhcPs, HsModule (..))
import GHC.Types.SrcLoc (unLoc)
import GhcOracle (parseWithGhcWithExtensions, toGhcExtension)
import Language.Haskell.Syntax.Decls (HsDecl)
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
      ghcModule = parseWithGhc sourceTag edition extensionSettings source'
      parserModule = parseWithAihc sourceTag edition extensionSettings source'
      comparison = compareParseResults (either (const False) (const True) ghcModule) parserModule
      ghcAstFailure =
        case (comparison, ghcModule, parserModule) of
          (BothAccept, Right ghcModule', Just parserModule') -> compareSnippetGhcAst parserModule' ghcModule'
          _ -> Nothing
      validationFailure =
        case comparison of
          BothAccept -> fmap validationErrorMessage (validateParser sourceTag edition extensionSettings source')
          _ -> Nothing
      parensDiff =
        case comparison of
          BothAccept -> parserModule >>= parsedSnippetParensDiff source
          _ -> Nothing
   in buildSnippetReport comparison ghcAstFailure validationFailure parensDiff

buildSnippetReport :: ParseComparison -> Maybe String -> Maybe String -> Maybe String -> SnippetReport
buildSnippetReport comparison ghcAstFailure validationFailure parensDiff =
  let statusLines =
        comparisonLines comparison
          ++ ghcAstLines comparison ghcAstFailure
          ++ roundtripLines comparison validationFailure
          ++ parensLines comparison parensDiff
      hasFailure = comparison /= BothAccept || isJust ghcAstFailure || isJust validationFailure || isJust parensDiff
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

ghcAstLines :: ParseComparison -> Maybe String -> [ReportLine]
ghcAstLines comparison ghcAstFailure =
  case comparison of
    BothAccept ->
      case ghcAstFailure of
        Nothing -> [nonBugLine "GHC AST Match: OK"]
        Just message -> [bugLine ("GHC AST Match: " <> message)]
    _ -> [nonBugLine "GHC AST Match: Skipped"]

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

parseWithGhc :: FilePath -> LanguageEdition -> [ExtensionSetting] -> Text -> Either Text (HsModule GhcPs)
parseWithGhc sourceTag edition extensionSettings source =
  let ghcExtensions = mapMaybe toGhcExtension (effectiveExtensions edition extensionSettings)
   in parseWithGhcWithExtensions sourceTag ghcExtensions source

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

compareSnippetGhcAst :: Module -> HsModule GhcPs -> Maybe String
compareSnippetGhcAst parserModule ghcModule =
  let expectedDecls = map unLoc (hsmodDecls ghcModule)
      actualDecls = toGhcHsModuleDecls parserModule
   in compareGhcDecls expectedDecls actualDecls

compareGhcDecls :: [HsDecl GhcPs] -> [HsDecl GhcPs] -> Maybe String
compareGhcDecls expectedDecls actualDecls
  | length expectedDecls /= length actualDecls =
      Just
        ( intercalate
            "\n"
            [ "Bug found:",
              "Declaration count mismatch: GHC parsed "
                <> show (length expectedDecls)
                <> ", AIHC converted "
                <> show (length actualDecls)
                <> "."
            ]
        )
  | otherwise = firstMismatch (zip3 [1 :: Int ..] expectedDecls actualDecls)
  where
    firstMismatch [] = Nothing
    firstMismatch ((index, expected, actual) : rest)
      | sameGhcAst expected actual = firstMismatch rest
      | otherwise = Just (formatGhcAstMismatch index expected actual)

formatGhcAstMismatch :: Int -> HsDecl GhcPs -> HsDecl GhcPs -> String
formatGhcAstMismatch index expected actual =
  intercalate
    "\n"
    [ "Bug found:",
      "Declaration " <> show index <> " differs after converting the AIHC AST to GHC AST.",
      "GHC parser AST:",
      dropWhileEnd (== '\n') (dumpGhcAst expected),
      "Converted AIHC AST:",
      dropWhileEnd (== '\n') (dumpGhcAst actual)
    ]

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
