{-# LANGUAGE OverloadedStrings #-}

-- | Fixture tests for System FC 2 text.
module Test.Fc2.Suite
  ( fc2FixtureTests,
    fc2GoldenTests,
    fc2LintTests,
  )
where

import Aihc.Fc2 (Program, lintPrograms, parseProgram, renderParseError, renderProgram)
import Data.Char (isSpace)
import Data.List (dropWhileEnd, sort)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Fc2Golden (Fc2Case (..), Outcome (..), evaluateFc2Case, loadFc2Cases)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

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

lintRoot :: FilePath
lintRoot = "test/Test/Fixtures/fc2-lint"

fc2LintTests :: IO TestTree
fc2LintTests = do
  pass <- lintFileTests "pass" True (lintRoot </> "pass")
  failCases <- lintFileTests "fail" False (lintRoot </> "fail")
  mutual <- mutualLintTests (lintRoot </> "mutual")
  pure (testGroup "SystemFC2 lint" [pass, failCases, mutual])

lintFileTests :: String -> Bool -> FilePath -> IO TestTree
lintFileTests label expectPass dir = do
  files <- listFc2Files dir
  pure (testGroup label (map (lintFileTest expectPass) files))

lintFileTest :: Bool -> FilePath -> TestTree
lintFileTest expectPass path = testCase path $ do
  program <- loadFc2Program path
  let errors = lintPrograms [program]
  if expectPass
    then assertEqual (path <> " lint errors") [] errors
    else assertBool (path <> " expected lint errors") (not (null errors))

mutualLintTests :: FilePath -> IO TestTree
mutualLintTests dir = do
  files <- listFc2Files dir
  pure
    ( testGroup
        "mutual"
        [ testCase "lint of all files together passes" $ do
            programs <- mapM loadFc2Program files
            assertEqual "mutual lint errors" [] (lintPrograms programs),
          testCase "lint of one file without the other fails" $ do
            programs <- mapM loadFc2Program files
            mapM_
              ( \program ->
                  assertBool
                    "a single mutual file must fail lint"
                    (not (null (lintPrograms [program])))
              )
              programs
        ]
    )

listFc2Files :: FilePath -> IO [FilePath]
listFc2Files dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then pure []
    else do
      names <- sort <$> listDirectory dir
      pure [dir </> name | name <- names, takeExtension name == ".fc2"]

loadFc2Program :: FilePath -> IO Program
loadFc2Program path = do
  source <- TIO.readFile path
  case parseProgram source of
    Left parseError -> assertFailure (path <> ": " <> renderParseError parseError)
    Right program -> pure program

fc2GoldenTests :: IO TestTree
fc2GoldenTests = testGroup "SystemFC2 goldens" . map mkGolden <$> loadFc2Cases

mkGolden :: Fc2Case -> TestTree
mkGolden tc = testCase (caseId tc) $ do
  let (outcome, details) = evaluateFc2Case tc
  case outcome of
    OutcomePass -> pure ()
    OutcomeXFail -> pure ()
    OutcomeXPass -> assertFailure ("unexpected pass (xpass): " <> details)
    OutcomeFail -> assertFailure details
