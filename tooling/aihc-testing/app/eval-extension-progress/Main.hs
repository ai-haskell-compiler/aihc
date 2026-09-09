{-# LANGUAGE OverloadedStrings #-}

-- | Extension coverage reporting for the shared evaluation fixtures.
--
-- Groups the eval fixtures by language extension and produces a markdown report
-- shaped like the resolver and type checker extension support reports. Nothing
-- is compiled or evaluated: the counts come from the status each fixture
-- declares for itself.
module Main (main) where

import Aihc.Parser.Syntax qualified as Syntax
import Aihc.Testing.EvalFixtureIndex
  ( EvalFixtureEntry (..),
    FixtureStatus (..),
    loadEvalFixtureIndex,
  )
import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Data.Text qualified as T

data SupportStatus = Supported | InProgress deriving (Eq, Show)

data ExtensionResult = ExtensionResult
  { erName :: !String,
    erStatus :: !SupportStatus,
    erWorkingN :: !Int,
    erTotalN :: !Int
  }

main :: IO ()
main = do
  entries <- loadEvalFixtureIndex
  let grouped = groupByExtension entries
      results = map mkExtensionResult (sortOn fst (M.toList grouped))
  putStr (renderMarkdown results)

groupByExtension :: [EvalFixtureEntry] -> M.Map Syntax.Extension [EvalFixtureEntry]
groupByExtension = foldl' insertEntry M.empty
  where
    insertEntry acc entry =
      foldl' (\m ext -> M.insertWith (++) ext [entry] m) acc (entryExtensions entry)

mkExtensionResult :: (Syntax.Extension, [EvalFixtureEntry]) -> ExtensionResult
mkExtensionResult (name, entries) =
  ExtensionResult
    { erName = T.unpack (Syntax.extensionName name),
      erStatus = if workingN == totalN then Supported else InProgress,
      erWorkingN = workingN,
      erTotalN = totalN
    }
  where
    -- A fixture with status 'fail' asserts an expected error, so it counts as
    -- working. Only the known-bug markers 'xfail' and 'xpass' do not.
    workingN = length [() | entry <- entries, entryStatus entry `elem` [StatusPass, StatusFail]]
    totalN = length entries

renderMarkdown :: [ExtensionResult] -> String
renderMarkdown results =
  unlines
    ( [ "# Eval Test Extension Support Status",
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

testsPassingStr :: ExtensionResult -> String
testsPassingStr result = show (erWorkingN result) <> "/" <> show (erTotalN result)

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
    <> padRight w3 (testsPassingStr result)
    <> " |"

statusEmoji :: ExtensionResult -> String
statusEmoji result
  | erTotalN result == 0 = "⚪"
  | erWorkingN result == erTotalN result = "🟢"
  | fromIntegral (erWorkingN result) / fromIntegral (erTotalN result) >= (0.9 :: Double) = "🟡"
  | otherwise = "🔴"
