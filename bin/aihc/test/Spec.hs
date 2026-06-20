{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Aihc.Cli.Install
  ( DependencyResolver (..),
    InstallFailure (..),
    InstallResult (..),
    PackagePlan (..),
    PackageVariantKey (..),
    ResolvedDependency (..),
    buildDryRunPackagePlanWithResolver,
    buildPackagePlanFromSource,
    buildPackagePlanWithResolver,
    dryRunInstallScaffold,
    renderInstallFailure,
    renderInstallFailureWithOptions,
    writeInstallScaffold,
  )
import Aihc.Cli.Options (Command (..), InstallErrorFormat (..), InstallOptions (..), ReplOptions (..), parseCommandPure)
import Aihc.Cli.Repl (ReplError (..), ReplSession (..), ReplStep (..), defaultReplSettings, evaluateExpression, handleReplInput, loadReplSession)
import Aihc.Fc (FcProgram (..))
import Aihc.Hackage.Types (PackageSpec (..))
import Aihc.Resolve (Scope (..))
import Control.Exception (bracket)
import Data.Aeson (object, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.IORef (newIORef)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, sort)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import System.Directory
  ( createDirectory,
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getTemporaryDirectory,
    removeDirectoryRecursive,
    removeFile,
  )
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath (takeDirectory, takeFileName, (</>))
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
              (Right (CmdInstall (InstallOptions "text" Nothing Nothing False False False InstallErrorsJson)))
              (parseCommandPure ["install", "text"]),
          testCase "parses install version" $
            assertEqual
              "command"
              (Right (CmdInstall (InstallOptions "text" (Just "2.1") Nothing False False False InstallErrorsJson)))
              (parseCommandPure ["install", "text", "--version", "2.1"]),
          testCase "parses install offline and store" $
            assertEqual
              "command"
              (Right (CmdInstall (InstallOptions "text" Nothing (Just "/tmp/aihc-store") True False False InstallErrorsJson)))
              (parseCommandPure ["install", "text", "--offline", "--store", "/tmp/aihc-store"]),
          testCase "parses install dry run" $
            assertEqual
              "command"
              (Right (CmdInstall (InstallOptions "text" Nothing Nothing False True False InstallErrorsJson)))
              (parseCommandPure ["install", "text", "--dry-run"]),
          testCase "parses first error module and human errors" $
            assertEqual
              "command"
              (Right (CmdInstall (InstallOptions "text" Nothing Nothing False False True InstallErrorsHuman)))
              (parseCommandPure ["install", "text", "--first-error-module", "--human-errors"]),
          testCase "rejects explicit dependency variants" $
            assertLeftContains "dependency" (parseCommandPure ["install", "a", "--dependency", "b=1.0.0:abcdef"]),
          testCase "parses repl" $
            assertEqual "command" (Right (CmdRepl (ReplOptions Nothing))) (parseCommandPure ["repl"]),
          testCase "parses repl store" $
            assertEqual "command" (Right (CmdRepl (ReplOptions (Just "/tmp/aihc-store")))) (parseCommandPure ["repl", "--store", "/tmp/aihc-store"])
        ],
      testGroup
        "repl"
        [ testCase "quits with long command" $ do
            session <- testReplSession
            step <- handleReplInput session ":quit"
            assertEqual "step" (ReplExit Nothing) step,
          testCase "quits with short command" $ do
            session <- testReplSession
            step <- handleReplInput session ":q"
            assertEqual "step" (ReplExit Nothing) step,
          testCase "prints help" $ do
            session <- testReplSession
            handleReplInput session ":help" >>= assertHelp
            handleReplInput session ":?" >>= assertHelp,
          testCase "ignores empty input" $ do
            session <- testReplSession
            step <- handleReplInput session "   "
            assertEqual "step" (ReplContinue Nothing) step,
          testCase "reports unknown commands" $ do
            session <- testReplSession
            step <- handleReplInput session ":type"
            assertEqual "step" (ReplContinue (Just "unknown command: :type")) step,
          testCase "evaluates string expressions through the pipeline" $ do
            session <- testReplSession
            step <- handleReplInput session "\"hello world\""
            assertEqual "step" (ReplContinue (Just "\"hello world\"")) step,
          testCase "evaluates let-bound strings through the pipeline" $ do
            session <- testReplSession
            step <- handleReplInput session "let a = \"hello world\" in a"
            assertEqual "step" (ReplContinue (Just "\"hello world\"")) step,
          testCase "evaluateExpression returns string values" $ do
            session <- testReplSession
            result <- evaluateExpression session "\"hello world\""
            assertEqual "result" (Right "\"hello world\"") result,
          testCase "evaluateExpression reports parse errors" $ do
            session <- testReplSession
            result <- evaluateExpression session "1 +"
            assertEqual "result" (Left ReplParseError) result,
          testCase "sets evaluation detail output from the repl" $ do
            session <- testReplSession
            setStep <- handleReplInput session ":set +pretty"
            assertEqual "set step" (ReplContinue (Just "settings: +parsed-pretty -parsed-shorthand -type -system-fc")) setStep
            setStep2 <- handleReplInput session ":set +shorthand"
            assertEqual "set step" (ReplContinue (Just "settings: +parsed-pretty +parsed-shorthand -type -system-fc")) setStep2
            step <- handleReplInput session "\"hello\""
            case step of
              ReplContinue (Just output) -> do
                assertBool ("expected parsed output, got:\n" <> output) ("parsed:\n\"hello\"" `isInfixOf` output)
                assertBool ("expected shorthand output, got:\n" <> output) ("shorthand:\nEString \"hello\"" `isInfixOf` output)
                assertBool "expected final value" ("\"hello\"" `isSuffixOf` output)
              other -> assertFailure ("expected output, got " <> show other),
          testCase "sets type and system-fc output from the repl" $ do
            session <- testReplSession
            _ <- handleReplInput session ":set +type"
            _ <- handleReplInput session ":set +fc"
            result <- evaluateExpression session "\"hello\""
            case result of
              Right output -> do
                assertBool ("expected type output, got:\n" <> T.unpack output) ("type:\n[Char]" `T.isInfixOf` output)
                assertBool ("expected system-fc output, got:\n" <> T.unpack output) ("system-fc:\n__aihc_repl_it : [Char] =" `T.isInfixOf` output)
                assertBool ("expected desugared char list, got:\n" <> T.unpack output) (not ("LitString" `T.isInfixOf` output))
              Left err -> assertFailure ("expected success, got " <> show err),
          testCase "loads bundled aihc-base Prelude by default" $ do
            session <- loadReplSession Nothing
            assertBundledPreludeNumericScope session
            result <- evaluateExpression session "otherwise"
            assertEqual "result" (Right "True") result,
          testCase "loads installed base interface for Prelude MVP scope" test_loadsInstalledBaseInterfaceForRepl
        ],
      testGroup
        "install"
        [ testCase "builds stable store paths" test_stableStorePath,
          testCase "recursive dependencies affect store paths" test_recursiveDependenciesAffectStorePaths,
          testCase "writes scaffold artifacts" test_writeInstallScaffold,
          testCase "writes dependency scaffold artifacts first" test_writeInstallScaffoldWritesDependencies,
          testCase "reports rename errors and writes no artifacts" test_reportsRenameErrorsAndWritesNoArtifacts,
          testCase "reports type-check errors and writes no artifacts" test_reportsTypeCheckErrorsAndWritesNoArtifacts,
          testCase "limits rendered install errors to first module" test_limitsRenderedInstallErrorsToFirstModule,
          testCase "renders human install errors" test_rendersHumanInstallErrors,
          testCase "does not install dependencies when root package fails" test_failedRootDoesNotInstallDependencies,
          testCase "resolves base dependencies through resolver" test_resolvesBaseDependenciesThroughResolver,
          testCase "uses local provider for ghc-prim dependencies" test_usesLocalProviderForGhcPrim,
          testCase "uses local provider for ghc-internal dependencies" test_usesLocalProviderForGhcInternal,
          testCase "manifest records core provider dependency names" test_manifestRecordsCoreProviderDependencyNames,
          testCase "installs resolved base dependency closure" test_installsResolvedBaseDependencyClosure,
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

testReplSession :: IO ReplSession
testReplSession = do
  settingsRef <- newIORef defaultReplSettings
  pure
    ReplSession
      { replModuleExports = Map.empty,
        replImportedTerms = [],
        replDependencyProgram = FcProgram [],
        replSettings = settingsRef
      }

test_loadsInstalledBaseInterfaceForRepl :: Assertion
test_loadsInstalledBaseInterfaceForRepl =
  withTempDir "aihc-repl" $ \root -> do
    let storeRoot = root </> "store"
        packageRoot = storeRoot </> "0000000000000000-base-4_22_0_0"
        interfacePath = packageRoot </> "interfaces" </> "package-interface.json"
        manifestPath = packageRoot </> "manifest.json"
    createDirectoryIfMissing True (takeDirectory interfacePath)
    BL8.writeFile
      manifestPath
      ( Aeson.encode
          ( object
              [ "package" .= object ["name" .= ("aihc-base" :: String), "version" .= ("4.21.2.0" :: String)],
                "interfacePath" .= interfacePath
              ]
          )
      )
    BL8.writeFile
      interfacePath
      ( Aeson.encode
          ( object
              [ "modules"
                  .= [ object
                         [ "module" .= ("Prelude" :: String),
                           "terms" .= ([] :: [String]),
                           "types" .= ([] :: [String]),
                           "constructors" .= object [],
                           "recordFields" .= object [],
                           "methods" .= object []
                         ]
                     ]
              ]
          )
      )
    session <- loadReplSession (Just storeRoot)
    case Map.lookup "Prelude" (replModuleExports session) of
      Nothing -> assertFailure "Prelude scope not loaded"
      Just preludeScope -> do
        assertBool "Prelude exposes Char" (Map.member "Char" (scopeTypes preludeScope))
        assertBool "Prelude exposes String" (Map.member "String" (scopeTypes preludeScope))
    result <- evaluateExpression session "\"hello\""
    assertEqual "result" (Right "\"hello\"") result
    _ <- handleReplInput session ":set +type"
    typedStringResult <- evaluateExpression session "\"hello\""
    case typedStringResult of
      Right output ->
        assertBool ("expected [Char] type, got:\n" <> T.unpack output) ("type:\n[Char]" `T.isInfixOf` output)
      Left err -> assertFailure ("expected typed string success, got " <> show err)

assertBundledPreludeNumericScope :: ReplSession -> Assertion
assertBundledPreludeNumericScope session =
  case Map.lookup "Prelude" (replModuleExports session) of
    Nothing -> assertFailure "Prelude scope not loaded"
    Just preludeScope -> do
      assertBool "Prelude exposes Integer" (Map.member "Integer" (scopeTypes preludeScope))
      assertBool "Prelude exposes Num" (Map.member "Num" (scopeTypes preludeScope))
      assertEqual "Prelude exposes Num methods" (Just ["+", "-", "*", "negate", "abs", "signum", "fromInteger"]) (Map.lookup "Num" (scopeMethods preludeScope))

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
    result <- expectInstallSuccess (writeInstallScaffold plan)
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
    let renderedInterface = BL8.unpack interfaceJson
    assertBool "interface includes fixities" ("fixities" `isInfixOf` renderedInterface)
    assertBool "interface includes modules" ("modules" `isInfixOf` renderedInterface)
    assertBool "interface includes typecheck data" ("typecheck" `isInfixOf` renderedInterface)
    assertBool "interface is implemented" (not ("unimplemented" `isInfixOf` renderedInterface))

    fcJson <- BL8.readFile (resultFcPath result)
    assertBool "fc placeholder includes system-fc" ("system-fc" `isInfixOf` BL8.unpack fcJson)

test_writeInstallScaffoldWritesDependencies :: Assertion
test_writeInstallScaffoldWritesDependencies =
  withDependencyFixture $ \sourceRoot depRoot _ storeRoot -> do
    plan <-
      buildPackagePlanWithResolver
        (fixtureDependencyResolver sourceRoot [("dep", "1.0.0", depRoot)])
        storeRoot
        (PackageSpec "demo" "0.1.0.0")
    _ <- expectInstallSuccess (writeInstallScaffold plan)
    dependencyPlan <- findPlanByName "dep" (planDependencyPlans plan)
    assertFileExists (planStorePath dependencyPlan </> "manifest.json")
    assertFileExists (planStorePath dependencyPlan </> "interfaces" </> "package-interface.json")
    assertFileExists (planStorePath dependencyPlan </> "fc" </> "package-fc.json")

test_reportsRenameErrorsAndWritesNoArtifacts :: Assertion
test_reportsRenameErrorsAndWritesNoArtifacts =
  withTempDir "aihc-cli" $ \root -> do
    let sourceRoot = root </> "source"
        storeRoot = root </> "store"
    createFixturePackageWithSource sourceRoot "demo" "0.1.0.0" "Demo" [] "module Demo where\nimport Missing.Module\nx = 1\n"
    createDirectoryIfMissing True storeRoot
    plan <- buildPackagePlanFromSource storeRoot (PackageSpec "demo" "0.1.0.0") sourceRoot

    err <- renderInstallFailure <$> expectInstallFailure (writeInstallScaffold plan)
    assertBool "error reports package" ("failed to install demo-0.1.0.0" `isInfixOf` err)
    assertBool "error reports rename phase" ("rename errors" `isInfixOf` err)
    assertBool "error includes missing import" ("Missing.Module" `isInfixOf` err)
    assertFileDoesNotExist (planStorePath plan </> "manifest.json")
    assertFileDoesNotExist (planStorePath plan </> "interfaces" </> "package-interface.json")
    assertFileDoesNotExist (planStorePath plan </> "fc" </> "package-fc.json")

test_reportsTypeCheckErrorsAndWritesNoArtifacts :: Assertion
test_reportsTypeCheckErrorsAndWritesNoArtifacts =
  withTempDir "aihc-cli" $ \root -> do
    let sourceRoot = root </> "source"
        storeRoot = root </> "store"
    createFixturePackageWithSource sourceRoot "demo" "0.1.0.0" "Demo" [] "module Demo where\nx = [1, 'a']\n"
    createDirectoryIfMissing True storeRoot
    plan <- buildPackagePlanFromSource storeRoot (PackageSpec "demo" "0.1.0.0") sourceRoot

    err <- renderInstallFailure <$> expectInstallFailure (writeInstallScaffold plan)
    assertBool "error reports package" ("failed to install demo-0.1.0.0" `isInfixOf` err)
    assertBool "error reports type-check phase" ("type-check errors" `isInfixOf` err)
    assertBool "error includes unification diagnostic" ("UnificationError" `isInfixOf` err)
    assertFileDoesNotExist (planStorePath plan </> "manifest.json")
    assertFileDoesNotExist (planStorePath plan </> "interfaces" </> "package-interface.json")
    assertFileDoesNotExist (planStorePath plan </> "fc" </> "package-fc.json")

test_limitsRenderedInstallErrorsToFirstModule :: Assertion
test_limitsRenderedInstallErrorsToFirstModule = do
  let opts =
        defaultInstallOptionsForTest
          { installFirstErrorModule = True
          }
      err = renderInstallFailureWithOptions opts syntheticInstallFailure
  assertBool "error includes first module" ("selected module failed" `isInfixOf` err)
  assertBool "error omits other module" (not ("other module failed" `isInfixOf` err))

test_rendersHumanInstallErrors :: Assertion
test_rendersHumanInstallErrors = do
  let opts =
        defaultInstallOptionsForTest
          { installErrorFormat = InstallErrorsHuman
          }
      err = renderInstallFailureWithOptions opts syntheticInstallFailure
  assertBool ("error includes human location, got:\n" <> err) ("src/Demo.hs:2:3: error: [Demo] selected module failed" `isInfixOf` err)
  assertBool ("error includes source line, got:\n" <> err) ("  2 | x = selected + problem" `isInfixOf` err)
  assertBool ("error includes caret range, got:\n" <> err) ("    |   ^^^^^^^^" `isInfixOf` err)
  assertBool "error omits JSON object syntax" (not ("{\"" `isInfixOf` err))

defaultInstallOptionsForTest :: InstallOptions
defaultInstallOptionsForTest =
  InstallOptions "demo" Nothing Nothing False False False InstallErrorsJson

syntheticInstallFailure :: InstallFailure
syntheticInstallFailure =
  InstallInterfaceFailure
    (PackageSpec "demo" "0.1.0.0")
    [ ( "type-check",
        [ object
            [ "module" .= ("Demo" :: String),
              "span"
                .= object
                  [ "file" .= ("src/Demo.hs" :: String),
                    "startLine" .= (2 :: Int),
                    "startColumn" .= (3 :: Int),
                    "endLine" .= (2 :: Int),
                    "endColumn" .= (11 :: Int)
                  ],
              "severity" .= ("error" :: String),
              "message" .= ("selected module failed" :: String),
              "sourceLines"
                .= [ object
                       [ "line" .= (2 :: Int),
                         "text" .= ("x = selected + problem" :: String)
                       ]
                   ]
            ],
          object
            [ "module" .= ("Other" :: String),
              "span"
                .= object
                  [ "file" .= ("src/Other.hs" :: String),
                    "startLine" .= (4 :: Int),
                    "startColumn" .= (5 :: Int),
                    "endLine" .= (4 :: Int),
                    "endColumn" .= (9 :: Int)
                  ],
              "severity" .= ("error" :: String),
              "message" .= ("other module failed" :: String),
              "sourceLines"
                .= [ object
                       [ "line" .= (4 :: Int),
                         "text" .= ("y = other" :: String)
                       ]
                   ]
            ]
        ]
      )
    ]

test_failedRootDoesNotInstallDependencies :: Assertion
test_failedRootDoesNotInstallDependencies =
  withTempDir "aihc-cli" $ \root -> do
    let sourceRoot = root </> "source"
        depRoot = root </> "dep"
        storeRoot = root </> "store"
    createFixturePackageWithSource sourceRoot "demo" "0.1.0.0" "Demo" ["dep >=1 && <2"] "module Demo where\nimport Missing.Module\nx = 1\n"
    createFixturePackage depRoot "dep" "1.0.0" "Dep" []
    createDirectoryIfMissing True storeRoot
    plan <-
      buildPackagePlanWithResolver
        (fixtureDependencyResolver sourceRoot [("dep", "1.0.0", depRoot)])
        storeRoot
        (PackageSpec "demo" "0.1.0.0")
    dependencyPlan <- findPlanByName "dep" (planDependencyPlans plan)

    _ <- expectInstallFailure (writeInstallScaffold plan)
    assertFileDoesNotExist (planStorePath plan </> "manifest.json")
    assertFileDoesNotExist (planStorePath dependencyPlan </> "manifest.json")

test_usesLocalProviderForGhcPrim :: Assertion
test_usesLocalProviderForGhcPrim =
  withCoreDependencyFixture "ghc-prim" $ \sourceRoot storeRoot -> do
    plan <-
      buildPackagePlanWithResolver
        (fixtureDependencyResolver sourceRoot [])
        storeRoot
        (PackageSpec "demo" "0.1.0.0")
    assertDependencySpec plan (PackageSpec "aihc-prim" "0.13.0")

test_resolvesBaseDependenciesThroughResolver :: Assertion
test_resolvesBaseDependenciesThroughResolver =
  withBaseDependencyFixture $ \sourceRoot baseRoot storeRoot -> do
    plan <-
      buildPackagePlanWithResolver
        (fixtureDependencyResolver sourceRoot [("base", "4.22.0.0", baseRoot)])
        storeRoot
        (PackageSpec "demo" "0.1.0.0")
    assertDependencySpec plan (PackageSpec "base" "4.22.0.0")

test_usesLocalProviderForGhcInternal :: Assertion
test_usesLocalProviderForGhcInternal =
  withCoreDependencyFixture "ghc-internal" $ \sourceRoot storeRoot -> do
    plan <-
      buildPackagePlanWithResolver
        (fixtureDependencyResolver sourceRoot [])
        storeRoot
        (PackageSpec "demo" "0.1.0.0")
    assertDependencySpec plan (PackageSpec "aihc-internal" "9.1204.0")

test_manifestRecordsCoreProviderDependencyNames :: Assertion
test_manifestRecordsCoreProviderDependencyNames =
  withCoreDependencyFixture "ghc-prim" $ \sourceRoot storeRoot -> do
    plan <-
      buildPackagePlanWithResolver
        (fixtureDependencyResolver sourceRoot [])
        storeRoot
        (PackageSpec "demo" "0.1.0.0")
    result <- expectInstallSuccess (writeInstallScaffold plan)
    manifest <- BL8.readFile (resultManifestPath result)
    let renderedManifest = BL8.unpack manifest
    assertBool "manifest records provider dependency name" ("aihc-prim" `isInfixOf` renderedManifest)
    assertBool "manifest does not retain upstream boot package name" (not ("ghc-prim" `isInfixOf` renderedManifest))

test_installsResolvedBaseDependencyClosure :: Assertion
test_installsResolvedBaseDependencyClosure =
  withBaseDependencyFixture $ \sourceRoot baseRoot storeRoot -> do
    plan <-
      buildPackagePlanWithResolver
        (fixtureDependencyResolver sourceRoot [("base", "4.22.0.0", baseRoot)])
        storeRoot
        (PackageSpec "demo" "0.1.0.0")
    _ <- expectInstallSuccess (writeInstallScaffold plan)
    let plans = flattenPackagePlans plan
        installedNames = sort [pkgName (packageKeySpec (planPackageKey item)) | item <- plans]
    assertEqual "installed packages" ["base", "demo"] installedNames
    mapM_ (assertFileExists . (</> "manifest.json") . planStorePath) plans

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

expectInstallSuccess :: IO (Either InstallFailure a) -> IO a
expectInstallSuccess action = do
  result <- action
  case result of
    Right value -> pure value
    Left failure -> assertFailure ("expected install success, got: " <> renderInstallFailure failure)

expectInstallFailure :: IO (Either InstallFailure a) -> IO InstallFailure
expectInstallFailure action = do
  result <- action
  case result of
    Left failure -> pure failure
    Right _ -> assertFailure "expected install failure"

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

withCoreDependencyFixture :: String -> (FilePath -> FilePath -> IO a) -> IO a
withCoreDependencyFixture dependencyName action =
  withTempDir "aihc-cli" $ \root -> do
    let sourceRoot = root </> "source"
        storeRoot = root </> "store"
        coreLibsRoot = root
    createFixturePackage sourceRoot "demo" "0.1.0.0" "Demo" [dependencyName]
    createCoreProviderFixtures coreLibsRoot
    createDirectoryIfMissing True storeRoot
    withCoreLibsRoot coreLibsRoot (action sourceRoot storeRoot)

withBaseDependencyFixture :: (FilePath -> FilePath -> FilePath -> IO a) -> IO a
withBaseDependencyFixture action =
  withTempDir "aihc-cli" $ \root -> do
    let sourceRoot = root </> "source"
        baseRoot = root </> "base"
        storeRoot = root </> "store"
    createFixturePackage sourceRoot "demo" "0.1.0.0" "Demo" ["base"]
    createFixturePackage baseRoot "base" "4.22.0.0" "Base" []
    createDirectoryIfMissing True storeRoot
    action sourceRoot baseRoot storeRoot

withCoreLibsRoot :: FilePath -> IO a -> IO a
withCoreLibsRoot root action =
  bracket
    (lookupEnv "AIHC_CORE_LIBS_ROOT")
    restoreEnv
    (\_ -> setEnv "AIHC_CORE_LIBS_ROOT" root *> action)
  where
    restoreEnv Nothing = unsetEnv "AIHC_CORE_LIBS_ROOT"
    restoreEnv (Just value) = setEnv "AIHC_CORE_LIBS_ROOT" value

assertDependencySpec :: PackagePlan -> PackageSpec -> Assertion
assertDependencySpec plan expected =
  assertBool ("expected dependency " <> show expected <> " in " <> show dependencySpecs) (expected `elem` dependencySpecs)
  where
    dependencySpecs = map resolvedDependencySpec (packageKeyDependencies (planPackageKey plan))

findPlanByName :: String -> [PackagePlan] -> IO PackagePlan
findPlanByName name plans =
  case [plan | plan <- plans, pkgName (packageKeySpec (planPackageKey plan)) == name] of
    plan : _ -> pure plan
    [] -> assertFailure ("missing dependency plan for " <> name)

flattenPackagePlans :: PackagePlan -> [PackagePlan]
flattenPackagePlans plan =
  concatMap flattenPackagePlans (planDependencyPlans plan) <> [plan]

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

createFixturePackageWithSource :: FilePath -> String -> String -> String -> [String] -> String -> IO ()
createFixturePackageWithSource sourceRoot name version moduleName dependencies source = do
  let srcDir = sourceRoot </> "src"
  createDirectoryIfMissing True srcDir
  writeFile (sourceRoot </> name <> ".cabal") (fixtureCabal name version moduleName [] dependencies)
  writeFile (sourceRoot </> "Setup.hs") "import Distribution.Simple\nmain = defaultMain\n"
  writeFile (srcDir </> moduleName <> ".hs") source

createCoreProviderFixtures :: FilePath -> IO ()
createCoreProviderFixtures root = do
  createCoreProviderPackage
    (root </> "core-libs" </> "aihc-base")
    "aihc-base"
    "4.21.2.0"
    "Prelude"
  createCoreProviderPackage
    (root </> "core-libs" </> "aihc-prim")
    "aihc-prim"
    "0.13.0"
    "GHC.Prim"
  createCoreProviderPackage
    (root </> "core-libs" </> "aihc-internal")
    "aihc-internal"
    "9.1204.0"
    "GHC.Internal.Base"

createCoreProviderPackage :: FilePath -> String -> String -> String -> IO ()
createCoreProviderPackage sourceRoot name version moduleName = do
  let srcDir = sourceRoot </> "src"
      modulePath = srcDir </> map dotToSlash moduleName <> ".hs"
  createDirectoryIfMissing True (takeDirectory modulePath)
  writeFile (sourceRoot </> name <> ".cabal") (fixtureCabal name version moduleName [] [])
  writeFile modulePath ("{-# LANGUAGE NoImplicitPrelude #-}\nmodule " <> moduleName <> " () where\n")
  where
    dotToSlash '.' = '/'
    dotToSlash c = c

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
