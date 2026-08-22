{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Aihc.Fc2 qualified as Fc2
import Aihc.Grin (lintProgram, renderProgram)
import Aihc.Grin.Lower2 qualified as Lower2
import Data.Text.IO qualified as TIO
import System.Directory (doesDirectoryExist, getCurrentDirectory)
import System.FilePath (takeDirectory, (</>))
import Test.Grin.Arbitrary (prop_grinPrettyRoundTrip)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)
import Test.Tasty.Hedgehog (testProperty)

main :: IO ()
main =
  defaultMain
    ( testGroup
        "aihc-grin"
        [ testCase "lowers System FC 2 to GRIN" testLowerFc2,
          testProperty "generated GRIN pretty-printer round-trip" prop_grinPrettyRoundTrip
        ]
    )

testLowerFc2 :: IO ()
testLowerFc2 = do
  root <- fixtureRoot
  source <- TIO.readFile (root </> "basic.fc2")
  expected <- readFile (root </> "basic.grin")
  program <-
    case Fc2.parseProgram source of
      Left parseError -> assertFailure (Fc2.renderParseError parseError)
      Right parsed -> pure parsed
  grin <-
    case Lower2.lowerProgram program of
      Left problem -> assertFailure problem
      Right lowered -> pure lowered
  assertEqual "GRIN lint errors" [] (lintProgram grin)
  assertEqual "rendered GRIN" expected (renderProgram grin <> "\n")

fixtureRoot :: IO FilePath
fixtureRoot = getCurrentDirectory >>= findUp
  where
    findUp directory = do
      let candidate = directory </> "test" </> "Test" </> "Fixtures" </> "fc2-grin"
      exists <- doesDirectoryExist candidate
      if exists
        then pure candidate
        else do
          let parent = takeDirectory directory
          if parent == directory
            then assertFailure "System FC 2 GRIN fixture directory does not exist"
            else findUp parent
