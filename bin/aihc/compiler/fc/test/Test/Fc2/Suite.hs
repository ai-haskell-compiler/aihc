{-# LANGUAGE OverloadedStrings #-}

-- | Fixture tests for System FC 2 text.
module Test.Fc2.Suite
  ( fc2FixtureTests,
    fc2GoldenTests,
    fc2LintTests,
  )
where

import Aihc.Fc2 (LintError (..), Program, lintProgram, loadScopeClosure, parseProgram, renderParseError, renderProgram, storeModuleLoader)
import Control.Exception (IOException, try)
import Data.Char (isSpace)
import Data.List (dropWhileEnd, isInfixOf, sort)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Fc2Golden (Fc2Case (..), Outcome (..), evaluateFc2Case, loadFc2Cases)
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, getTemporaryDirectory, listDirectory, removeDirectoryRecursive)
import System.FilePath (takeExtension, takeFileName, (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

fixtureRoot :: FilePath
fixtureRoot = "compiler/fc/test/Test/Fixtures/fc2"

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
lintRoot = "compiler/fc/test/Test/Fixtures/fc2-lint"

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
  let errors = lintProgram program
  if expectPass
    then assertEqual (path <> " lint errors") [] errors
    else assertBool (path <> " expected lint errors") (matchesFail path errors)

mutualLintTests :: FilePath -> IO TestTree
mutualLintTests dir = do
  files <- listFc2Files dir
  pure
    ( testGroup
        "mutual"
        [ testCase "each file passes lint" $ do
            programs <- mapM loadFc2Program files
            mapM_
              (assertEqual "single-file lint errors" [] . lintProgram)
              programs,
          scopeLoaderTest
        ]
    )

scopeLoaderTest :: TestTree
scopeLoaderTest = testCase "loadScopeClosure loads a scoped module from the store" $ do
  tmp <- getTemporaryDirectory
  let store = tmp </> "aihc-fc2-lint-scope"
      typesDir = store </> "aihc-prim" </> "GHC" </> "Types"
      primDir = store </> "aihc-prim" </> "GHC" </> "Prim"
  ignoreMissing (removeDirectoryRecursive store)
  createDirectoryIfMissing True typesDir
  createDirectoryIfMissing True primDir
  copyFile (lintRoot </> "mutual" </> "GHC.Types.fc2") (typesDir </> "core-v2")
  copyFile (lintRoot </> "mutual" </> "GHC.Prim.fc2") (primDir </> "core-v2")
  seed <- loadFc2Program (lintRoot </> "mutual" </> "GHC.Types.fc2")
  loaded <- loadScopeClosure (storeModuleLoader store) [seed]
  ignoreMissing (removeDirectoryRecursive store)
  assertEqual "loaded module count" 2 (length loaded)
  assertEqual "seed lint errors" [] (lintProgram seed)

ignoreMissing :: IO () -> IO ()
ignoreMissing action = do
  result <- try action :: IO (Either IOException ())
  case result of
    Left _ -> pure ()
    Right () -> pure ()

matchesFail :: FilePath -> [LintError] -> Bool
matchesFail path errors =
  case failClass (takeFileName path) of
    Just check -> any check errors
    Nothing -> not (null errors)

failClass :: FilePath -> Maybe (LintError -> Bool)
failClass name
  | "unbound" `isInfixOf` name = Just isUnboundName
  | "app-mismatch" `isInfixOf` name = Just isTypeMismatch
  | "case-result" `isInfixOf` name = Just isTypeMismatch
  | "tyapp-kind" `isInfixOf` name = Just isKindMismatch
  | "cast-source" `isInfixOf` name = Just isTypeMismatch
  | "shadowed" `isInfixOf` name = Just isShadowedBinder
  | "unused-import" `isInfixOf` name = Just isUnusedImport
  | "lit-alt" `isInfixOf` name = Just isTypeMismatch
  | "lit-secret" `isInfixOf` name = Just isTypeMismatch
  | "tycon-co-arity" `isInfixOf` name = Just isLintFailure
  | otherwise = Nothing

isUnboundName :: LintError -> Bool
isUnboundName UnboundName {} = True
isUnboundName _ = False

isTypeMismatch :: LintError -> Bool
isTypeMismatch TypeMismatch {} = True
isTypeMismatch _ = False

isKindMismatch :: LintError -> Bool
isKindMismatch KindMismatch {} = True
isKindMismatch _ = False

isShadowedBinder :: LintError -> Bool
isShadowedBinder ShadowedBinder {} = True
isShadowedBinder _ = False

isUnusedImport :: LintError -> Bool
isUnusedImport UnusedImport {} = True
isUnusedImport _ = False

isLintFailure :: LintError -> Bool
isLintFailure LintFailure {} = True
isLintFailure _ = False

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
