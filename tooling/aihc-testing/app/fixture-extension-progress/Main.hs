{-# LANGUAGE OverloadedStrings #-}

-- | Extension coverage reporting for the shared compiler test fixtures.
--
-- Groups the evaluation and System FC golden fixtures by language extension and
-- produces a markdown report shaped like the resolver and type checker extension
-- support reports. Nothing is compiled or evaluated: the counts come from the
-- status each fixture declares for itself.
module Main (main) where

import Aihc.Parser.Syntax qualified as Syntax
import Aihc.Testing.FixtureIndex
  ( FixtureEntry (..),
    FixtureStatus (..),
    FixtureSuite,
    allFixtureSuites,
    loadFixtureIndex,
    suiteName,
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
  entries <- loadFixtureIndex
  let grouped = groupByExtension entries
      results = map mkExtensionResult (sortOn fst (M.toList grouped))
  putStr (renderMarkdown entries results)

groupByExtension :: [FixtureEntry] -> M.Map Syntax.Extension [FixtureEntry]
groupByExtension = foldr insertEntry M.empty
  where
    insertEntry entry acc =
      foldr (\ext m -> M.insertWith (++) ext [entry] m) acc (entryExtensions entry)

mkExtensionResult :: (Syntax.Extension, [FixtureEntry]) -> ExtensionResult
mkExtensionResult (name, entries) =
  ExtensionResult
    { erName = T.unpack (Syntax.extensionName name),
      erStatus = if workingN == totalN then Supported else InProgress,
      erWorkingN = workingN,
      erTotalN = totalN
    }
  where
    workingN = length (filter (isWorking . entryStatus) entries)
    totalN = length entries

-- | A fixture with status @fail@ asserts an expected error, so it counts as
-- working. Only the known-bug markers @xfail@ and @xpass@ do not.
isWorking :: FixtureStatus -> Bool
isWorking status = status == StatusPass || status == StatusFail

renderMarkdown :: [FixtureEntry] -> [ExtensionResult] -> String
renderMarkdown entries results =
  unlines
    ( [ "# Test Fixture Extension Support Status",
        "",
        "## Summary",
        "",
        "- Total Extensions: " <> show totalN,
        "- Supported: " <> show supportedN,
        "- In Progress: " <> show inProgressN
      ]
        <> map suiteSummaryLine allFixtureSuites
        <> [ "",
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
    suiteSummaryLine suite =
      "- Fixtures (" <> suiteName suite <> "): " <> show (suiteFixtureCount entries suite)

suiteFixtureCount :: [FixtureEntry] -> FixtureSuite -> Int
suiteFixtureCount entries suite = length (filter ((== suite) . entrySuite) entries)

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
