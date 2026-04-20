module Test.StackageProgress.FileChecker (stackageProgressFileCheckerTests) where

import Control.Exception (bracket)
import HackageSupport (FileInfo (..))
import StackageProgress.CLI (Parser (..))
import StackageProgress.FileChecker
  ( FileCheckOptions (..),
    FileResult (..),
    PackageFileSummary (..),
    checkFile,
    emptyFileSummary,
    foldFilesForPackage,
    getPackageFileErrors,
  )
import System.Directory (createDirectory, getTemporaryDirectory, removeDirectoryRecursive, removeFile)
import System.IO (hClose, openTempFile)
import Test.Tasty
import Test.Tasty.HUnit

stackageProgressFileCheckerTests :: TestTree
stackageProgressFileCheckerTests =
  testGroup
    "stackage progress file checker"
    [ testCase "failed parse stays failed when detail retention is disabled" test_failedParseWithoutDetail,
      testCase "package fold preserves failure without retained file errors" test_foldWithoutDetail
    ]

test_failedParseWithoutDetail :: Assertion
test_failedParseWithoutDetail =
  withTempDir "stackage-progress-file-checker" $ \root -> do
    let file = root ++ "/Broken.hs"
        info = brokenFileInfo file
        checkOpts =
          FileCheckOptions
            { fileCheckKeepFirstFailure = False,
              fileCheckKeepFileErrors = False,
              fileCheckKeepGhcError = False
            }
    writeFile file "module Broken where\nvalue =\n"
    result <- checkFile checkOpts [ParserAihc] False root info
    assertBool "parse failure must still count as failure" (not (fileOursOk result))
    assertEqual "detail is dropped in count-only mode" Nothing (fileError result)

test_foldWithoutDetail :: Assertion
test_foldWithoutDetail =
  withTempDir "stackage-progress-file-checker" $ \root -> do
    let file = root ++ "/Broken.hs"
        info = brokenFileInfo file
        checkOpts =
          FileCheckOptions
            { fileCheckKeepFirstFailure = False,
              fileCheckKeepFileErrors = False,
              fileCheckKeepGhcError = False
            }
    writeFile file "module Broken where\nvalue =\n"
    summary <- foldFilesForPackage checkOpts [ParserAihc] False root emptyFileSummary [info]
    assertBool "package summary must record parser failure" (not (packageFileOursOk summary))
    assertEqual "file errors remain omitted" [] (getPackageFileErrors summary)

brokenFileInfo :: FilePath -> FileInfo
brokenFileInfo file =
  FileInfo
    { fileInfoPath = file,
      fileInfoExtensions = [],
      fileInfoCppOptions = [],
      fileInfoLanguage = Nothing,
      fileInfoDependencies = []
    }

withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir prefix action = do
  tempRoot <- getTemporaryDirectory
  (tempFile, tempHandle) <- openTempFile tempRoot (prefix ++ "-XXXXXX")
  hClose tempHandle
  removeFile tempFile
  createDirectory tempFile
  bracket
    (pure tempFile)
    removeDirectoryRecursive
    action
