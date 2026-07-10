{-# LANGUAGE OverloadedStrings #-}

module Test.TcStackageProgress
  ( tcStackageProgressTests,
  )
where

import Aihc.Cli.Install (DependencyResolver (..), newPackageCheckCache, newPackagePlanCache)
import Aihc.Hackage.Cabal (FileInfo (..))
import Aihc.Hackage.Types (PackageSpec (..))
import Control.Exception (bracket)
import Data.List (isInfixOf)
import Data.Map.Strict qualified as Map
import ResolveStackageProgress (PackageStatus (..))
import ResolveStackageProgress qualified as RSP
import System.Directory (createDirectory, createDirectoryIfMissing, getTemporaryDirectory, removeDirectoryRecursive, removeFile)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import TcStackageProgress (PackageCounts (..), checkOnePackage, smallestFailingPackages, summarizePackageStatuses)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

tcStackageProgressTests :: TestTree
tcStackageProgressTests =
  testGroup
    "tc stackage progress"
    [ testCase "counts typechecked, failed, and skipped packages" test_countsPackageStatuses,
      testCase "selects the smallest failing packages before applying the top limit" test_smallestFailingPackages,
      testCase "skips packages when source download fails" test_skipsSourceDownloadFailure,
      testCase "skips packages when dependency planning fails" test_skipsDependencyPlanningFailure,
      testCase "skips packages when a dependency check fails" test_skipsDependencyCheckFailure,
      testCase "reports failures originating in the root package" test_reportsRootPackageFailure
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

test_skipsSourceDownloadFailure :: IO ()
test_skipsSourceDownloadFailure =
  withTempDir "tc-stackage-progress-download-failure" $ \dir -> do
    let storeRoot = dir </> "store"
        spec = PackageSpec "demo" "0.1.0.0"
        resolver =
          DependencyResolver
            { resolverResolveVersion = \name -> fail ("unexpected dependency " <> name),
              resolverSourcePath = const (fail "download failed")
            }
    planCache <- newPackagePlanCache
    checkCache <- newPackageCheckCache

    (status, sourceFileCount) <- checkOnePackage planCache checkCache resolver storeRoot spec

    assertEqual "source file count" 0 sourceFileCount
    assertSkipped status

test_skipsDependencyPlanningFailure :: IO ()
test_skipsDependencyPlanningFailure =
  withTempDir "tc-stackage-progress-planning-failure" $ \dir -> do
    let sourceRoot = dir </> "source"
        sourceDir = sourceRoot </> "src"
        storeRoot = dir </> "store"
        spec = PackageSpec "demo" "0.1.0.0"
        resolver =
          DependencyResolver
            { resolverResolveVersion = \name -> fail ("missing dependency version for " <> name),
              resolverSourcePath = const (pure sourceRoot)
            }
    createDirectoryIfMissing True sourceDir
    writeFile (sourceRoot </> "demo.cabal") planningFailureCabal
    writeFile (sourceDir </> "Demo.hs") "module Demo where\nx = ()\n"
    planCache <- newPackagePlanCache
    checkCache <- newPackageCheckCache

    (status, sourceFileCount) <- checkOnePackage planCache checkCache resolver storeRoot spec

    assertEqual "source file count" 1 sourceFileCount
    assertSkipped status

test_skipsDependencyCheckFailure :: IO ()
test_skipsDependencyCheckFailure =
  withTempDir "tc-stackage-progress-dependency-check-failure" $ \dir -> do
    let sourceRoot = dir </> "source"
        dependencyRoot = dir </> "dependency"
        storeRoot = dir </> "store"
        spec = PackageSpec "demo" "0.1.0.0"
        resolver =
          DependencyResolver
            { resolverResolveVersion = const (pure "1.0.0"),
              resolverSourcePath = \packageSpec ->
                pure $ if pkgName packageSpec == "demo" then sourceRoot else dependencyRoot
            }
    createPackage sourceRoot "demo" "Demo" ["dependency"] "module Demo where\nx = ()\n"
    createPackage dependencyRoot "dependency" "Dependency" [] "module Dependency where\nx =\n"
    planCache <- newPackagePlanCache
    checkCache <- newPackageCheckCache

    (status, _) <- checkOnePackage planCache checkCache resolver storeRoot spec

    assertSkipped status

test_reportsRootPackageFailure :: IO ()
test_reportsRootPackageFailure =
  withTempDir "tc-stackage-progress-root-failure" $ \dir -> do
    let sourceRoot = dir </> "source"
        storeRoot = dir </> "store"
        spec = PackageSpec "demo" "0.1.0.0"
        resolver =
          DependencyResolver
            { resolverResolveVersion = \name -> fail ("unexpected dependency " <> name),
              resolverSourcePath = const (pure sourceRoot)
            }
    createPackage sourceRoot "demo" "Demo" [] "module Demo where\nx =\n"
    planCache <- newPackagePlanCache
    checkCache <- newPackageCheckCache

    (status, _) <- checkOnePackage planCache checkCache resolver storeRoot spec

    case status of
      PkgFailed message -> assertBool "root parse failure is retained" ("parse errors" `isInfixOf` message)
      _ -> fail "expected root package failure"

assertSkipped :: PackageStatus -> IO ()
assertSkipped PkgSkipped = pure ()
assertSkipped _ = fail "expected package to be skipped"

createPackage :: FilePath -> String -> String -> [String] -> String -> IO ()
createPackage root name moduleName dependencies source = do
  let sourceDir = root </> "src"
  createDirectoryIfMissing True sourceDir
  writeFile (root </> name <> ".cabal") (packageCabal name moduleName dependencies)
  writeFile (sourceDir </> moduleName <> ".hs") source

packageCabal :: String -> String -> [String] -> String
packageCabal name moduleName dependencies =
  unlines $
    [ "cabal-version: 3.0",
      "name: " <> name,
      "version: 0.1.0.0",
      "",
      "library",
      "  exposed-modules: " <> moduleName,
      "  hs-source-dirs: src"
    ]
      <> ["  build-depends: " <> dependency | dependency <- dependencies]
      <> ["  default-language: Haskell2010"]

planningFailureCabal :: String
planningFailureCabal =
  unlines
    [ "cabal-version: 3.0",
      "name: demo",
      "version: 0.1.0.0",
      "",
      "library",
      "  exposed-modules: Demo",
      "  hs-source-dirs: src",
      "  build-depends: missing-dependency",
      "  default-language: Haskell2010"
    ]

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
