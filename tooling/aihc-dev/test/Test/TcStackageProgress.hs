{-# LANGUAGE OverloadedStrings #-}

module Test.TcStackageProgress
  ( tcStackageProgressTests,
  )
where

import Aihc.Hackage.Cabal (FileInfo (..))
import Control.Exception (bracket)
import Data.Map.Strict qualified as Map
import ResolveStackageProgress (PackageStatus (..))
import ResolveStackageProgress qualified as RSP
import System.Directory (createDirectory, getTemporaryDirectory, removeDirectoryRecursive, removeFile)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import TcStackageProgress (PackageCounts (..), smallestFailingPackages, summarizePackageStatuses)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

tcStackageProgressTests :: TestTree
tcStackageProgressTests =
  testGroup
    "tc stackage progress"
    [ testCase "counts typechecked, failed, and skipped packages" test_countsPackageStatuses,
      testCase "selects the smallest failing packages before applying the top limit" test_smallestFailingPackages
    ]

test_countsPackageStatuses :: IO ()
test_countsPackageStatuses = do
  let statuses =
        Map.fromList
          [ ("base", PkgSuccess Map.empty),
            ("containers", PkgFailed "type error"),
            ("text", PkgSkipped)
          ]
  assertEqual
    "counts"
    PackageCounts
      { countTypechecked = 1,
        countFailed = 1,
        countSkipped = 1,
        countTotal = 3
      }
    (summarizePackageStatuses statuses)

test_smallestFailingPackages :: IO ()
test_smallestFailingPackages =
  withTempDir "tc-stackage-progress" $ \dir -> do
    let tinyFile = dir </> "Tiny.hs"
        mediumFile = dir </> "Medium.hs"
        largeFile = dir </> "Large.hs"
    writeFile tinyFile "module Tiny where\n"
    writeFile mediumFile "module Medium where\nx = 1\n"
    writeFile largeFile "module Large where\nx = 1\ny = 2\nz = 3\n"
    let infos =
          Map.fromList
            [ ("large", packageInfo [largeFile]),
              ("medium", packageInfo [mediumFile]),
              ("tiny", packageInfo [tinyFile])
            ]
        statuses =
          Map.fromList
            [ ("large", PkgFailed "large error"),
              ("medium", PkgFailed "medium error"),
              ("tiny", PkgFailed "tiny error")
            ]
    assertEqual
      "smallest failures"
      [("tiny", 1, "tiny error"), ("medium", 2, "medium error")]
      =<< smallestFailingPackages 2 infos statuses

packageInfo :: [FilePath] -> RSP.PackageInfo
packageInfo paths =
  RSP.PackageInfo
    { RSP.piSrcDir = "",
      RSP.piFiles = map fileInfo paths,
      RSP.piSnapshotDeps = []
    }

fileInfo :: FilePath -> FileInfo
fileInfo path =
  FileInfo
    { fileInfoPath = path,
      fileInfoExtensions = [],
      fileInfoCppOptions = [],
      fileInfoIncludeDirs = [],
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
