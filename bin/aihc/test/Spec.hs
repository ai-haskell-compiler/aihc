module Main (main) where

import Aihc.Cli.Install
  ( InstallResult (..),
    PackagePlan (..),
    buildPackagePlanFromSource,
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
              (Right (CmdInstall (InstallOptions "text" Nothing Nothing False)))
              (parseCommandPure ["install", "text"]),
          testCase "parses install version" $
            assertEqual
              "command"
              (Right (CmdInstall (InstallOptions "text" (Just "2.1") Nothing False)))
              (parseCommandPure ["install", "text", "--version", "2.1"]),
          testCase "parses install offline and store" $
            assertEqual
              "command"
              (Right (CmdInstall (InstallOptions "text" Nothing (Just "/tmp/aihc-store") True)))
              (parseCommandPure ["install", "text", "--offline", "--store", "/tmp/aihc-store"]),
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
          testCase "writes scaffold artifacts" test_writeInstallScaffold
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

test_stableStorePath :: Assertion
test_stableStorePath =
  withFixturePackage $ \sourceRoot storeRoot -> do
    plan1 <- buildPackagePlanFromSource storeRoot (PackageSpec "demo" "0.1.0.0") sourceRoot
    plan2 <- buildPackagePlanFromSource storeRoot (PackageSpec "demo" "0.1.0.0") sourceRoot
    assertEqual "store path" (planStorePath plan1) (planStorePath plan2)
    assertBool "store path includes package" ("demo-0_1_0_0" `isInfixOf` takeFileName (planStorePath plan1))
    assertBool "store path starts below root" (storeRoot `isPrefixOf` planStorePath plan1)

test_writeInstallScaffold :: Assertion
test_writeInstallScaffold =
  withFixturePackage $ \sourceRoot storeRoot -> do
    plan <- buildPackagePlanFromSource storeRoot (PackageSpec "demo" "0.1.0.0") sourceRoot
    result <- writeInstallScaffold plan
    assertFileExists (resultManifestPath result)
    assertFileExists (resultInterfacePath result)
    assertFileExists (resultFcPath result)

    manifest <- BL8.readFile (resultManifestPath result)
    let renderedManifest = BL8.unpack manifest
    assertBool "manifest records setup phase" ("compile-setup" `isInfixOf` renderedManifest)
    assertBool "manifest omits GHC version" (not ("ghcVersion" `isInfixOf` renderedManifest))
    assertBool "manifest records source count" ("sourceFileCount" `isInfixOf` renderedManifest)
    assertBool "manifest records unimplemented phases" ("unimplemented" `isInfixOf` renderedManifest)

    interfaceJson <- BL8.readFile (resultInterfacePath result)
    assertBool "interface placeholder includes fixities" ("fixities" `isInfixOf` BL8.unpack interfaceJson)

    fcJson <- BL8.readFile (resultFcPath result)
    assertBool "fc placeholder includes system-fc" ("system-fc" `isInfixOf` BL8.unpack fcJson)

assertFileExists :: FilePath -> Assertion
assertFileExists path = do
  exists <- doesFileExist path
  assertBool ("expected file to exist: " <> path) exists

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
