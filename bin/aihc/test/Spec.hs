module Main (main) where

import Aihc.Cli.Install
  ( DependencyResolver (..),
    InstallResult (..),
    PackagePlan (..),
    buildDryRunPackagePlanWithResolver,
    buildPackagePlanFromSource,
    buildPackagePlanWithResolver,
    dryRunInstallScaffold,
    writeInstallScaffold,
  )
import Aihc.Cli.Options (Command (..), InstallOptions (..), parseCommandPure)
import Aihc.Cli.Repl (ReplStep (..), evaluateExpression, handleReplInput)
import Aihc.Hackage.Types (PackageSpec (..))
import Control.Exception (bracket)
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.List (isInfixOf, isPrefixOf)
import System.Directory
  ( createDirectory,
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getTemporaryDirectory,
    removeDirectoryRecursive,
    removeFile,
  )
import System.FilePath (takeFileName, (</>))
import System.IO (hClose, openTempFile)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck qualified as QC

main :: IO ()
main =
  defaultMain . testGroup "aihc" $
    [ testGroup
        "cli"
        [ testCase "parses install package" $
            assertEqual
              "command"
              (Right (CmdInstall (InstallOptions "text" Nothing Nothing False False)))
              (parseCommandPure ["install", "text"]),
          testCase "parses install version" $
            assertEqual
              "command"
              (Right (CmdInstall (InstallOptions "text" (Just "2.1") Nothing False False)))
              (parseCommandPure ["install", "text", "--version", "2.1"]),
          testCase "parses install offline and store" $
            assertEqual
              "command"
              (Right (CmdInstall (InstallOptions "text" Nothing (Just "/tmp/aihc-store") True False)))
              (parseCommandPure ["install", "text", "--offline", "--store", "/tmp/aihc-store"]),
          testCase "parses install dry run" $
            assertEqual
              "command"
              (Right (CmdInstall (InstallOptions "text" Nothing Nothing False True)))
              (parseCommandPure ["install", "text", "--dry-run"]),
          testCase "rejects explicit dependency variants" $
            assertLeftContains "dependency" (parseCommandPure ["install", "a", "--dependency", "b=1.0.0:abcdef"]),
          testCase "parses repl" $
            assertEqual "command" (Right CmdRepl) (parseCommandPure ["repl"])
        ],
      testGroup
        "repl"
        [ testCase "quits with long command" $
            assertEqual "step" (ReplExit Nothing) (handleReplInput ":quit"),
          testCase "quits with short command" $
            assertEqual "step" (ReplExit Nothing) (handleReplInput ":q"),
          testCase "prints help" $ do
            assertHelp (handleReplInput ":help")
            assertHelp (handleReplInput ":?"),
          testCase "ignores empty input" $
            assertEqual "step" (ReplContinue Nothing) (handleReplInput "   "),
          testCase "reports unknown commands" $
            assertEqual "step" (ReplContinue (Just "unknown command: :type")) (handleReplInput ":type"),
          testCase "evaluates expressions through the scaffold" $
            assertEqual "step" (ReplContinue (Just "unimplemented")) (handleReplInput "1 + 1"),
          testCase "evaluateExpression is scaffolded" $
            assertEqual "result" "unimplemented" (evaluateExpression "map id []")
        ],
      testGroup
        "install"
        [ testCase "builds stable store paths" test_stableStorePath,
          testCase "recursive dependencies affect store paths" test_recursiveDependenciesAffectStorePaths,
          testCase "writes scaffold artifacts" test_writeInstallScaffold,
          testCase "dry run writes no scaffold artifacts" test_dryRunWritesNoScaffoldArtifacts,
          testCase "dry run planner does not generate source files" test_dryRunPlannerDoesNotGenerateSourceFiles
        ],
      QC.testProperty "dummy quickcheck property" prop_dummy
    ]

prop_dummy :: Bool
prop_dummy = True

assertHelp :: ReplStep -> Assertion
assertHelp (ReplContinue (Just output)) =
  assertBool "help mentions :quit" (":quit" `isInfixOf` output)
assertHelp other =
  assertFailure ("expected help output, got " <> show other)

assertLeftContains :: String -> Either String a -> Assertion
assertLeftContains expected (Left actual) =
  assertBool ("expected error to contain " <> show expected <> ", got " <> show actual) (expected `isInfixOf` actual)
assertLeftContains _ (Right _) =
  assertFailure "expected parse failure"

test_stableStorePath :: Assertion
test_stableStorePath =
  withFixturePackage $ \sourceRoot storeRoot -> do
    plan1 <- buildPackagePlanFromSource storeRoot (PackageSpec "demo" "0.1.0.0") sourceRoot
    plan2 <- buildPackagePlanFromSource storeRoot (PackageSpec "demo" "0.1.0.0") sourceRoot
    assertEqual "store path" (planStorePath plan1) (planStorePath plan2)
    assertBool "store path includes package" ("demo-0_1_0_0" `isInfixOf` takeFileName (planStorePath plan1))
    assertBool "store path starts below root" (storeRoot `isPrefixOf` planStorePath plan1)

test_recursiveDependenciesAffectStorePaths :: Assertion
test_recursiveDependenciesAffectStorePaths =
  withDependencyFixture $ \sourceRoot depV1Root depV2Root storeRoot -> do
    plan1 <-
      buildPackagePlanWithResolver
        (fixtureDependencyResolver sourceRoot [("dep", "1.0.0", depV1Root)])
        storeRoot
        (PackageSpec "demo" "0.1.0.0")
    plan2 <-
      buildPackagePlanWithResolver
        (fixtureDependencyResolver sourceRoot [("dep", "2.0.0", depV2Root)])
        storeRoot
        (PackageSpec "demo" "0.1.0.0")
    assertBool "dependency variant should change store path" (planStorePath plan1 /= planStorePath plan2)

test_writeInstallScaffold :: Assertion
test_writeInstallScaffold =
  withDependencyFixture $ \sourceRoot depRoot _ storeRoot -> do
    plan <-
      buildPackagePlanWithResolver
        (fixtureDependencyResolver sourceRoot [("dep", "1.0.0", depRoot)])
        storeRoot
        (PackageSpec "demo" "0.1.0.0")
    result <- writeInstallScaffold plan
    assertFileExists (resultManifestPath result)
    assertFileExists (resultInterfacePath result)
    assertFileExists (resultFcPath result)

    manifest <- BL8.readFile (resultManifestPath result)
    let renderedManifest = BL8.unpack manifest
    assertBool "manifest records setup phase" ("compile-setup" `isInfixOf` renderedManifest)
    assertBool "manifest omits GHC version" (not ("ghcVersion" `isInfixOf` renderedManifest))
    assertBool "manifest records package key" ("packageKey" `isInfixOf` renderedManifest)
    assertBool "manifest records dependencies" ("dependencies" `isInfixOf` renderedManifest)
    assertBool "manifest records dependency package" ("dep" `isInfixOf` renderedManifest)
    assertBool "manifest records source count" ("sourceFileCount" `isInfixOf` renderedManifest)
    assertBool "manifest records unimplemented phases" ("unimplemented" `isInfixOf` renderedManifest)

    interfaceJson <- BL8.readFile (resultInterfacePath result)
    assertBool "interface placeholder includes fixities" ("fixities" `isInfixOf` BL8.unpack interfaceJson)

    fcJson <- BL8.readFile (resultFcPath result)
    assertBool "fc placeholder includes system-fc" ("system-fc" `isInfixOf` BL8.unpack fcJson)

test_dryRunWritesNoScaffoldArtifacts :: Assertion
test_dryRunWritesNoScaffoldArtifacts =
  withDependencyFixture $ \sourceRoot depRoot _ storeRoot -> do
    plan <-
      buildDryRunPackagePlanWithResolver
        (fixtureDependencyResolver sourceRoot [("dep", "1.0.0", depRoot)])
        storeRoot
        (PackageSpec "demo" "0.1.0.0")
    result <- dryRunInstallScaffold plan

    assertEqual "store path" (planStorePath plan) (resultStorePath result)
    assertFileDoesNotExist (resultManifestPath result)
    assertFileDoesNotExist (resultInterfacePath result)
    assertFileDoesNotExist (resultFcPath result)
    storePathExists <- doesDirectoryExist (resultStorePath result)
    assertBool ("expected dry-run store path not to exist: " <> resultStorePath result) (not storePathExists)

test_dryRunPlannerDoesNotGenerateSourceFiles :: Assertion
test_dryRunPlannerDoesNotGenerateSourceFiles =
  withTempDir "aihc-cli" $ \root -> do
    let sourceRoot = root </> "source"
        storeRoot = root </> "store"
        autogenDir = sourceRoot </> ".aihc-autogen"
    createFixturePackageWithOtherModules sourceRoot "demo" "0.1.0.0" "Demo" ["Paths_demo"] []
    createDirectoryIfMissing True storeRoot

    _ <-
      buildDryRunPackagePlanWithResolver
        (fixtureDependencyResolver sourceRoot [])
        storeRoot
        (PackageSpec "demo" "0.1.0.0")

    autogenExists <- doesDirectoryExist autogenDir
    assertBool ("expected dry-run planner not to create " <> autogenDir) (not autogenExists)

assertFileExists :: FilePath -> Assertion
assertFileExists path = do
  exists <- doesFileExist path
  assertBool ("expected file to exist: " <> path) exists

assertFileDoesNotExist :: FilePath -> Assertion
assertFileDoesNotExist path = do
  exists <- doesFileExist path
  assertBool ("expected file not to exist: " <> path) (not exists)

withFixturePackage :: (FilePath -> FilePath -> IO a) -> IO a
withFixturePackage action =
  withTempDir "aihc-cli" $ \root -> do
    let sourceRoot = root </> "source"
        storeRoot = root </> "store"
        srcDir = sourceRoot </> "src"
    createDirectoryIfMissing True srcDir
    createDirectoryIfMissing True storeRoot
    writeFile (sourceRoot </> "demo.cabal") demoCabal
    writeFile (sourceRoot </> "Setup.hs") "import Distribution.Simple\nmain = defaultMain\n"
    writeFile (srcDir </> "Demo.hs") "module Demo where\nx = ()\n"
    action sourceRoot storeRoot

withDependencyFixture :: (FilePath -> FilePath -> FilePath -> FilePath -> IO a) -> IO a
withDependencyFixture action =
  withTempDir "aihc-cli" $ \root -> do
    let sourceRoot = root </> "source"
        depV1Root = root </> "dep-v1"
        depV2Root = root </> "dep-v2"
        storeRoot = root </> "store"
    createFixturePackage sourceRoot "demo" "0.1.0.0" "Demo" ["dep >=1 && <3"]
    createFixturePackage depV1Root "dep" "1.0.0" "Dep" []
    createFixturePackage depV2Root "dep" "2.0.0" "Dep" []
    createDirectoryIfMissing True storeRoot
    action sourceRoot depV1Root depV2Root storeRoot

fixtureDependencyResolver :: FilePath -> [(String, String, FilePath)] -> DependencyResolver
fixtureDependencyResolver sourceRoot dependencies =
  DependencyResolver
    { resolverResolveVersion = \name ->
        case [version | (dependencyName, version, _) <- dependencies, dependencyName == name] of
          version : _ -> pure version
          [] -> fail ("missing fixture dependency version for " <> name),
      resolverSourcePath = \spec ->
        if pkgName spec == "demo"
          then pure sourceRoot
          else case [path | (dependencyName, version, path) <- dependencies, dependencyName == pkgName spec, version == pkgVersion spec] of
            path : _ -> pure path
            [] -> fail ("missing fixture source for " <> show spec)
    }

createFixturePackage :: FilePath -> String -> String -> String -> [String] -> IO ()
createFixturePackage sourceRoot name version moduleName dependencies = do
  createFixturePackageWithOtherModules sourceRoot name version moduleName [] dependencies

createFixturePackageWithOtherModules :: FilePath -> String -> String -> String -> [String] -> [String] -> IO ()
createFixturePackageWithOtherModules sourceRoot name version moduleName otherModules dependencies = do
  let srcDir = sourceRoot </> "src"
  createDirectoryIfMissing True srcDir
  writeFile (sourceRoot </> name <> ".cabal") (fixtureCabal name version moduleName otherModules dependencies)
  writeFile (sourceRoot </> "Setup.hs") "import Distribution.Simple\nmain = defaultMain\n"
  writeFile (srcDir </> moduleName <> ".hs") ("module " <> moduleName <> " where\nx = ()\n")

fixtureCabal :: String -> String -> String -> [String] -> [String] -> String
fixtureCabal name version moduleName otherModules dependencies =
  unlines $
    [ "cabal-version: 3.0",
      "name: " <> name,
      "version: " <> version,
      "",
      "library",
      "  exposed-modules: " <> moduleName,
      "  hs-source-dirs: src"
    ]
      <> ["  other-modules: " <> unwords otherModules | not (null otherModules)]
      <> ["  build-depends: " <> dependency | dependency <- dependencies]
      <> ["  default-language: Haskell2010"]

demoCabal :: String
demoCabal =
  unlines
    [ "cabal-version: 3.0",
      "name: demo",
      "version: 0.1.0.0",
      "",
      "library",
      "  exposed-modules: Demo",
      "  hs-source-dirs: src",
      "  build-depends: base >=4 && <5",
      "  default-language: Haskell2010"
    ]

withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir prefix action = do
  tempRoot <- getTemporaryDirectory
  (tempFile, tempHandle) <- openTempFile tempRoot (prefix <> "-XXXXXX")
  hClose tempHandle
  removeFile tempFile
  createDirectory tempFile
  bracket
    (pure tempFile)
    removeDirectoryRecursive
    action
