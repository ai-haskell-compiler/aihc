{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Aihc.Parser.Syntax qualified as Syntax
import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import ExtensionSupport
import ParserGolden qualified as PG
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

  -- Load and evaluate oracle cases
  oracleCases <- loadOracleCases
  oracleOutcomes <- fmap concat (mapM evaluateOracleCaseMaybe oracleCases)

  -- Load and evaluate golden module cases
  goldenCases <- PG.loadModuleCases
  let goldenOutcomes = concatMap evaluateGoldenCaseMaybe goldenCases

  -- Combine all outcomes
  let allOutcomes = oracleOutcomes <> goldenOutcomes
      grouped = groupByExtension allOutcomes
      results = map mkExtensionResult (sortOn fst (M.toList grouped))

  if markdown
    then putStrLn (renderMarkdown results)
    else printTextSummary results

  let failN = sum [erFailN result | result <- results]
      xpassN = sum [erXPassN result | result <- results]
  if failN == 0 && (not strict || xpassN == 0)
    then exitSuccess
    else exitFailure

evaluateOracleCaseMaybe :: CaseMeta -> IO [(CaseMeta, Outcome, String)]
evaluateOracleCaseMaybe meta =
  if null (caseExtensions meta)
    then pure []
    else do
      outcome <- evaluateCaseFromFile meta
      pure [outcome]

-- | Evaluate a golden module case and return outcomes for each extension
evaluateGoldenCaseMaybe :: PG.ParserCase -> [(CaseMeta, Outcome, String)]
evaluateGoldenCaseMaybe goldenCase =
  if null (PG.caseExtensions goldenCase)
    then []
    else
      let (pgOutcome, details) = PG.evaluateModuleCase goldenCase
          outcome = convertGoldenOutcome pgOutcome
          meta = goldenCaseToCaseMeta goldenCase
       in [(meta, outcome, details)]

-- | Convert ParserGolden.Outcome to ExtensionSupport.Outcome
convertGoldenOutcome :: PG.Outcome -> Outcome
convertGoldenOutcome pgOutcome =
  case pgOutcome of
    PG.OutcomePass -> OutcomePass
    PG.OutcomeXFail -> OutcomeXFail
    PG.OutcomeXPass -> OutcomeXPass
    PG.OutcomeFail -> OutcomeFail

-- | Convert a golden ParserCase to a CaseMeta for unified reporting
goldenCaseToCaseMeta :: PG.ParserCase -> CaseMeta
goldenCaseToCaseMeta goldenCase =
  CaseMeta
    { caseId = "golden/" <> PG.caseId goldenCase,
      caseCategory = "golden/" <> PG.caseCategory goldenCase,
      casePath = PG.casePath goldenCase,
      caseExpected = convertGoldenStatus (PG.caseStatus goldenCase),
      caseReason = PG.caseReason goldenCase,
      caseExtensions = map Syntax.EnableExtension (PG.caseExtensions goldenCase)
    }

-- | Convert ParserGolden.ExpectedStatus to ExtensionSupport.Expected
convertGoldenStatus :: PG.ExpectedStatus -> Expected
convertGoldenStatus status =
  case status of
    PG.StatusPass -> ExpectPass
    PG.StatusFail -> ExpectPass -- StatusFail means we expect a parse failure (which is a "pass" for the test)
    PG.StatusXFail -> ExpectXFail
    PG.StatusXPass -> ExpectXFail -- StatusXPass is similar to XFail (known issue)

groupByExtension ::
  [(CaseMeta, Outcome, String)] ->
  M.Map Syntax.Extension [(CaseMeta, Outcome, String)]
groupByExtension =
  foldl' insertCase M.empty
  where
    insertCase acc caseOutcome@(meta, _, _) =
      foldl'
        (\m ext -> M.insertWith (<>) ext [caseOutcome] m)
        acc
        [ext | Syntax.EnableExtension ext <- caseExtensions meta]

mkExtensionResult :: (Syntax.Extension, [(CaseMeta, Outcome, String)]) -> ExtensionResult
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
        { erName = T.unpack (Syntax.extensionName name),
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
        renderTableHeader col1W col2W col3W,
        renderTableSep col1W col2W col3W
      ]
        <> map (renderResultRow col1W col2W col3W) results
    )
  where
    supportedN = length [() | result <- results, erStatus result == Supported]
    inProgressN = length [() | result <- results, erStatus result == InProgress]
    totalN = length results
    col1W = maximum $ length ("Extension" :: String) : map (length . erName) results
    col2W = maximum $ length ("Status" :: String) : map (length . statusEmoji) results
    col3W = maximum $ length ("Tests Passing" :: String) : map (length . testsPassingStr) results
    testsPassingStr result = show (erPassN result) <> "/" <> show (erTotalN result)

padRight :: Int -> String -> String
padRight n s = s <> replicate (n - length s) ' '

padCenter :: Int -> String -> String
padCenter n s =
  let total = n - length s
      left = total `div` 2
      right = total - left
   in replicate left ' ' <> s <> replicate right ' '

renderTableHeader :: Int -> Int -> Int -> String
renderTableHeader w1 w2 w3 =
  "| " <> padRight w1 "Extension" <> " | " <> padCenter w2 "Status" <> " | " <> padRight w3 "Tests Passing" <> " |"

renderTableSep :: Int -> Int -> Int -> String
renderTableSep w1 w2 w3 =
  "|" <> replicate (w1 + 2) '-' <> "|:" <> replicate w2 '-' <> ":|" <> replicate (w3 + 2) '-' <> "|"

renderResultRow :: Int -> Int -> Int -> ExtensionResult -> String
renderResultRow w1 w2 w3 result =
  "| "
    <> padRight w1 (erName result)
    <> " | "
    <> padCenter w2 (statusEmoji result)
    <> " | "
    <> padRight w3 (show (erPassN result) <> "/" <> show (erTotalN result))
    <> " |"

statusEmoji :: ExtensionResult -> String
statusEmoji result
  | erPassN result == erTotalN result = "🟢"
  | erTotalN result > 0 && fromIntegral (erPassN result) / fromIntegral (erTotalN result) >= (0.9 :: Double) = "🟡"
  | otherwise = "🔴"

statusText :: SupportStatus -> String
statusText status =
  case status of
    Supported -> "Supported"
    InProgress -> "In Progress"
