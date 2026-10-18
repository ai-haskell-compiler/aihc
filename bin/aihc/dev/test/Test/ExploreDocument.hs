module Test.ExploreDocument (exploreDocumentTests) where

import Aihc.Dev.Explore.Document
import Data.Vector qualified as V
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

exploreDocumentTests :: TestTree
exploreDocumentTests =
  testGroup
    "explore document"
    [ testCase "a lowercase query ignores the case of letters" $
        assertEqual "matches" [(0, 3), (8, 3)] (textMatches "map" "Map x = map"),
      testCase "a query with a capital letter keeps the case of letters" $
        assertEqual "matches" [(0, 3)] (textMatches "Map" "Map x = map"),
      testCase "occurrences do not overlap" $
        assertEqual "matches" [(0, 2), (2, 2)] (textMatches "aa" "aaaaa"),
      testCase "an empty query has no occurrences" $
        assertEqual "matches" [] (textMatches "" "text"),
      testCase "the matching lines are in line order" $
        assertEqual "lines" [0, 2] (matchingLines "f" (document ["f = 1", "g = 2", "h = f"])),
      testCase "the document text has a newline after each line" $
        assertEqual "text" "a\nb\n" (documentText (document ["a", "b"])),
      testCase "a row breaks after the last space that fits" $
        assertEqual "rows" [(0, 7), (7, 4)] (wrapRows 8 "abc de fghi"),
      testCase "a row breaks a word that is longer than the width" $
        assertEqual "rows" [(0, 4), (4, 4), (8, 2)] (wrapRows 4 "abcdefghij"),
      testCase "a row does not break in the indentation" $
        assertEqual "rows" [(0, 6), (6, 2)] (wrapRows 6 "  abcdef"),
      testCase "a continuation row does not start with spaces" $
        assertEqual "rows" [(0, 4), (6, 2)] (wrapRows 4 "abcd  ef")
    ]
  where
    document lines' = Document (V.fromList lines') (V.fromList (map (const []) lines')) []
