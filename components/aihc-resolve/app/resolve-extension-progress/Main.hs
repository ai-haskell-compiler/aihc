{-# LANGUAGE OverloadedStrings #-}

-- | Extension support reporting for the name resolver.
--
-- Groups resolver golden test cases by language extension and
-- produces a markdown report similar to the parser and TC extension support reports.
module Main (main) where

import Aihc.Parser.Syntax qualified as Syntax
import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import ResolverGolden
import System.Exit (exitFailure, exitSuccess)

data SupportStatus = Supported | InProgress deriving (Eq, Show)

data ExtensionResult = ExtensionResult
  { erName :: !String,
    erStatus :: !SupportStatus,
    erPassN :: !Int,
    erXFailN :: !Int,
    erXPassN :: !Int,
    erFailN :: !Int,
    erTotalN :: !Int
  }

main :: IO ()
main = do
  cases <- loadResolverCases
  let evaluated = [(tc, outcome, details) | tc <- cases, let (outcome, details) = evaluateResolverCase tc]
      grouped = groupByExtension evaluated
      results = map mkExtensionResult (sortOn fst (M.toList grouped))

  putStrLn (renderMarkdown results)

  let failN = sum [erFailN result | result <- results]
      xpassN = sum [erXPassN result | result <- results]
  if failN == 0 && xpassN == 0
    then exitSuccess
    else exitFailure

groupByExtension ::
  [(ResolverCase, Outcome, String)] ->
  M.Map Syntax.Extension [(ResolverCase, Outcome, String)]
groupByExtension =
  foldl' insertCase M.empty
  where
    insertCase :: M.Map Syntax.Extension [(ResolverCase, Outcome, String)] -> (ResolverCase, Outcome, String) -> M.Map Syntax.Extension [(ResolverCase, Outcome, String)]
    insertCase acc (tc, outcome, details) =
      foldl' (\m ext -> M.insertWith (++) ext [(tc, outcome, details)] m) acc (caseExtensions tc)

mkExtensionResult :: (Syntax.Extension, [(ResolverCase, Outcome, String)]) -> ExtensionResult
mkExtensionResult (name, outcomes) =
  let passN = countOutcome OutcomePass outcomes
      xfailN = countOutcome OutcomeXFail outcomes
      xpassN = countOutcome OutcomeXPass outcomes
      failN = countOutcome OutcomeFail outcomes
      totalN = passN + xfailN + xpassN + failN
      status
        | failN == 0 && xpassN == 0 = Supported
        | otherwise = InProgress
   in ExtensionResult
        { erName = T.unpack (Syntax.extensionName name),
          erStatus = status,
          erPassN = passN,
          erXFailN = xfailN,
          erXPassN = xpassN,
          erFailN = failN,
          erTotalN = totalN
        }

countOutcome :: Outcome -> [(a, Outcome, String)] -> Int
countOutcome target = length . filter (\(_, outcome, _) -> outcome == target)

renderMarkdown :: [ExtensionResult] -> String
renderMarkdown results =
  unlines
    ( [ "# Name Resolver Extension Support Status",
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
  | erTotalN result == 0 = "⚪"
  | erPassN result == erTotalN result = "🟢"
  | erTotalN result > 0 && fromIntegral (erPassN result) / fromIntegral (erTotalN result) >= (0.9 :: Double) = "🟡"
  | otherwise = "🔴"
