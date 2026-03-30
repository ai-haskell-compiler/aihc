{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Aihc.Cpp (resultOutput)
import Aihc.Parser (ParseResult (..))
import qualified Aihc.Parser
import Aihc.Parser.Syntax (Module)
import CppSupport (preprocessForParserWithoutIncludesIfEnabled)
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text.IO.Utf8 as Utf8
import ExtensionSupport
import qualified GHC.LanguageExtensions.Type as GHC
import GhcOracle
  ( extensionNamesToGhcExtensions,
    oracleModuleAstFingerprintWithExtensionsAt,
    oracleParsesModuleWithExtensionsAt,
  )
import Prettyprinter (Pretty (..), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

data SupportStatus = Supported | InProgress deriving (Eq, Show)

data ExtensionResult = ExtensionResult
  { erName :: !String,
    erStatus :: !SupportStatus,
    erPassN :: !Int,
    erXFailN :: !Int,
    erXPassN :: !Int,
    erFailN :: !Int,
    erTotalN :: !Int,
    erOutcomes :: ![(CaseMeta, Outcome, String)]
  }

main :: IO ()
main = do
  args <- getArgs
  let strict = "--strict" `elem` args
      markdown = "--markdown" `elem` args

  cases <- loadOracleCases
  caseOutcomes <- fmap concat (mapM evaluateCaseMaybe cases)

  let grouped = groupByExtension caseOutcomes
      results = map mkExtensionResult (sortOn fst (M.toList grouped))

  if markdown
    then putStrLn (renderMarkdown results)
    else printTextSummary results

  let failN = sum [erFailN result | result <- results]
      xpassN = sum [erXPassN result | result <- results]
  if failN == 0 && (not strict || xpassN == 0)
    then exitSuccess
    else exitFailure

evaluateCaseMaybe :: CaseMeta -> IO [(CaseMeta, Outcome, String)]
evaluateCaseMaybe meta =
  if null (caseExtensions meta)
    then pure []
    else do
      outcome <- evaluateCase meta
      pure [outcome]

evaluateCase :: CaseMeta -> IO (CaseMeta, Outcome, String)
evaluateCase meta = do
  source <- Utf8.readFile (caseSourcePath meta)
  let exts = extensionNamesToGhcExtensions (caseExtensions meta) Nothing
      source' =
        resultOutput
          ( preprocessForParserWithoutIncludesIfEnabled
              (caseExtensions meta)
              []
              (casePath meta)
              source
          )
      parsed = Aihc.Parser.parseModule Aihc.Parser.defaultConfig source'
      oracleOk = oracleParsesModuleWithExtensionsAt "extension-progress" exts source'
      roundtripOk = moduleRoundtripsViaGhc exts source' parsed
  pure (finalizeOutcome meta oracleOk roundtripOk)

moduleRoundtripsViaGhc :: [GHC.Extension] -> Text -> ParseResult Module -> Bool
moduleRoundtripsViaGhc exts source oursResult =
  case oursResult of
    ParseErr _ -> False
    ParseOk parsed ->
      let rendered = renderStrict (layoutPretty defaultLayoutOptions (pretty parsed))
       in case ( oracleModuleAstFingerprintWithExtensionsAt "extension-progress" exts source,
                 oracleModuleAstFingerprintWithExtensionsAt "extension-progress" exts rendered
               ) of
            (Right sourceAst, Right renderedAst) -> sourceAst == renderedAst
            _ -> False

groupByExtension ::
  [(CaseMeta, Outcome, String)] ->
  M.Map String [(CaseMeta, Outcome, String)]
groupByExtension =
  foldl' insertCase M.empty
  where
    insertCase acc caseOutcome@(meta, _, _) =
      foldl'
        (\m ext -> M.insertWith (<>) ext [caseOutcome] m)
        acc
        (caseExtensions meta)

mkExtensionResult :: (String, [(CaseMeta, Outcome, String)]) -> ExtensionResult
mkExtensionResult (name, outcomes) =
  let passN = countOutcome OutcomePass outcomes
      xfailN = countOutcome OutcomeXFail outcomes
      xpassN = countOutcome OutcomeXPass outcomes
      failN = countOutcome OutcomeFail outcomes
      totalN = passN + xfailN + xpassN + failN
      status
        | failN == 0 && xfailN == 0 && xpassN == 0 = Supported
        | otherwise = InProgress
   in ExtensionResult
        { erName = name,
          erStatus = status,
          erPassN = passN,
          erXFailN = xfailN,
          erXPassN = xpassN,
          erFailN = failN,
          erTotalN = totalN,
          erOutcomes = outcomes
        }

countOutcome :: Outcome -> [(CaseMeta, Outcome, String)] -> Int
countOutcome target = length . filter (\(_, outcome, _) -> outcome == target)

printTextSummary :: [ExtensionResult] -> IO ()
printTextSummary results = do
  let supportedN = length [() | result <- results, erStatus result == Supported]
      inProgressN = length [() | result <- results, erStatus result == InProgress]
      totalN = length results
  putStrLn "Haskell parser extension support progress"
  putStrLn "================================="
  putStrLn ("SUPPORTED    " <> show supportedN)
  putStrLn ("IN_PROGRESS  " <> show inProgressN)
  putStrLn ("TOTAL        " <> show totalN)
  putStrLn ""
  mapM_ printExtensionLine results

  let regressions =
        [ (erName result, meta, details)
        | result <- results,
          (meta, OutcomeFail, details) <- erOutcomes result
        ]
      xpasses =
        [ (erName result, meta, details)
        | result <- results,
          (meta, OutcomeXPass, details) <- erOutcomes result
        ]

  mapM_ printRegression regressions
  mapM_ printXPass xpasses

printExtensionLine :: ExtensionResult -> IO ()
printExtensionLine result =
  putStrLn
    ( erName result
        <> "\t"
        <> statusText (erStatus result)
        <> "\tPASS="
        <> show (erPassN result)
        <> " XFAIL="
        <> show (erXFailN result)
        <> " XPASS="
        <> show (erXPassN result)
        <> " FAIL="
        <> show (erFailN result)
    )

printRegression :: (String, CaseMeta, String) -> IO ()
printRegression (ext, meta, details) =
  putStrLn
    ( "FAIL "
        <> ext
        <> "/"
        <> caseId meta
        <> " ["
        <> caseCategory meta
        <> "] "
        <> details
    )

printXPass :: (String, CaseMeta, String) -> IO ()
printXPass (ext, meta, details) =
  putStrLn
    ( "XPASS "
        <> ext
        <> "/"
        <> caseId meta
        <> " ["
        <> caseCategory meta
        <> "] "
        <> details
    )

renderMarkdown :: [ExtensionResult] -> String
renderMarkdown results =
  unlines
    ( [ "# Haskell Parser Extension Support Status",
        "",
        "## Summary",
        "",
        "- Total Extensions: " <> show totalN,
        "- Supported: " <> show supportedN,
        "- In Progress: " <> show inProgressN,
        "",
        "## Extension Status",
        "",
        "| Extension | Status | Tests Passing |",
        "|-----------|--------|---------------|"
      ]
        <> map renderResultRow results
    )
  where
    supportedN = length [() | result <- results, erStatus result == Supported]
    inProgressN = length [() | result <- results, erStatus result == InProgress]
    totalN = length results

renderResultRow :: ExtensionResult -> String
renderResultRow result =
  "| "
    <> erName result
    <> " | "
    <> statusText (erStatus result)
    <> " | "
    <> show (erPassN result)
    <> "/"
    <> show (erTotalN result)
    <> " |"

statusText :: SupportStatus -> String
statusText status =
  case status of
    Supported -> "Supported"
    InProgress -> "In Progress"
