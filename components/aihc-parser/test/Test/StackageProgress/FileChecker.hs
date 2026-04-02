{-# LANGUAGE OverloadedStrings #-}

module Test.StackageProgress.FileChecker (stackageProgressFileCheckerTests) where

import Control.Exception (bracket)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import qualified Data.Text.IO as TIO
import HackageSupport (FileInfo (..))
import StackageProgress.CLI (Check (..))
import StackageProgress.FileChecker (checkFile, fileError, fileOursOk)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)

stackageProgressFileCheckerTests :: TestTree
stackageProgressFileCheckerTests =
  testGroup
    "stackage progress file checker"
    [ testCase "parse errors keep the file path source name" test_parseErrorUsesFilePath
    ]

test_parseErrorUsesFilePath :: Assertion
test_parseErrorUsesFilePath = do
  tempRoot <- getTemporaryDirectory
  let dir = tempRoot </> "aihc-stackage-progress-tests"
  bracket (createTestFile dir) cleanupTestFile $ \tempFile -> do
    TIO.writeFile tempFile "module"
    result <- checkFile [CheckParse] dir (FileInfo tempFile [] [] Nothing)
    let err = fromMaybe "" (fileError result)
    assertBool "parser check should fail" (not (fileOursOk result))
    assertBool "error should include the actual file path" (tempFile `isInfixOf` err)
    assertBool "error should not include <input>" (not ("<input>" `isInfixOf` err))
  where
    createTestFile dir = do
      createDirectoryIfMissing True dir
      (tempFile, tempHandle) <- openTempFile dir "Example.hs"
      hClose tempHandle
      pure tempFile

    cleanupTestFile _tempFile = do
      tempRoot <- getTemporaryDirectory
      let dir = tempRoot </> "aihc-stackage-progress-tests"
      removeDirectoryRecursive dir
