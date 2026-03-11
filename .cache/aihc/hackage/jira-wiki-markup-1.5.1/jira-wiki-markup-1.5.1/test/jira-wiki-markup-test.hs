{-|
Module      : Main
Copyright   : © 2019–2023 Albert Krewinkel
License     : MIT

Maintainer  : Albert Krewinkel <tarleb@zeitkraut.de>
Stability   : alpha
Portability : portable

Tests for the jira-wiki-markup package.
-}
module Main (main) where

import Data.Text ()
import Test.Tasty (TestTree, defaultMain, testGroup)
import qualified Text.Jira.ParserTests
import qualified Text.Jira.PrinterTests

-- | Run the tests for jira-wiki-markup.
main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "jira-wiki-markup"
  [ Text.Jira.ParserTests.tests
  , Text.Jira.PrinterTests.tests
  ]
