{-# LANGUAGE OverloadedStrings #-}

-- | Fixture tests for System FC 2 text.
module Test.Fc2.Suite
  ( fc2FixtureTests,
  )
where

import Aihc.Fc2 (parseProgram, renderParseError, renderProgram)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)

fixtureRoot :: FilePath
fixtureRoot = "test/Test/Fixtures/fc2"

fc2FixtureTests :: IO TestTree
fc2FixtureTests = do
  exists <- doesDirectoryExist fixtureRoot
  if not exists
    then pure (testGroup "SystemFC2 fixtures" [])
    else do
      names <- listDirectory fixtureRoot
      let files = [fixtureRoot </> name | name <- names, takeExtension name == ".fc2"]
      pure (testGroup "SystemFC2 fixtures" (map fixtureTest files))

fixtureTest :: FilePath -> TestTree
fixtureTest path = testCase path $ do
  source <- TIO.readFile path
  case parseProgram source of
    Left parseError -> assertFailure (renderParseError parseError)
    Right program ->
      let printed = T.pack (renderProgram program)
       in case parseProgram printed of
            Left reprintError -> assertFailure ("reprint parse failed: " <> renderParseError reprintError)
            Right reprinted -> do
              assertEqual "parse then pretty then parse" program reprinted
              assertEqual "pretty matches fixture" (normalize source) (normalize printed)

normalize :: Text -> Text
normalize = T.pack . trim . T.unpack
  where
    trim = dropWhileEnd isSpace . dropWhile isSpace
