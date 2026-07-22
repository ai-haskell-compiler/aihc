{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Aihc.Cli.Compile
  ( CompileEnvironment (..),
    CompileError,
    compileOutputPath,
    compileSourceToAssemblyWithDependencies,
    compileSourceToAssemblyWithDependenciesFor,
    compileSourceToCoreWithDependencies,
    compileSourceToCpsGrinWithDependencies,
    compileSourceToGrinWithDependencies,
    compileSourceToWholeCoreWithDependencies,
    defaultCompileEnvironment,
    runCompileWithEnvironment,
  )
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
    checkPackagePlan,
    dryRunInstallScaffold,
    renderInstallFailure,
    renderInstallFailureWithOptions,
    writeInstallScaffold,
  )
import Aihc.Cli.Options (Command (..), CompileOptions (..), GarbageCollector (..), InstallErrorFormat (..), InstallOptions (..), ReplOptions (..), parseCommandPure)
import Aihc.Cli.Repl (ReplError (..), ReplSession (..), ReplStep (..), defaultReplSettings, evaluateExpression, handleReplInput, loadReplSession, replCompletion)
import Aihc.Fc (FcProgram (..))
import Aihc.Hackage.Types (PackageSpec (..))
import Aihc.Native (NativeTarget (..))
import Aihc.Resolve (Scope (..))
import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (forM_, when, (>=>))
import Data.Aeson (object, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.IORef (newIORef)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, sort)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..))
import System.Console.Haskeline qualified as Haskeline
import System.Directory
  ( createDirectory,
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getCurrentDirectory,
    getModificationTime,
    getTemporaryDirectory,
    listDirectory,
    removeDirectoryRecursive,
    removeFile,
    setModificationTime,
    withCurrentDirectory,
  )
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.IO (hClose, hFlush, hPutStr, openTempFile)
import System.Info (arch, os)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, readProcessWithExitCode, waitForProcess)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck qualified as QC

main :: IO ()
main =
  defaultMain . testGroup "aihc" $
    [ testGroup
        "cli"
        [ testCase "parses compile source" $
            assertEqual
              "command"
              (Right (CmdCompile (CompileOptions "Main.hs" Nothing False False False False Nothing GcCalloc)))
              (parseCommandPure ["compile", "Main.hs"]),
          testCase "parses compile output and keep-asm" $
            assertEqual
              "command"
              (Right (CmdCompile (CompileOptions "Main.hs" (Just "hello") False False True False Nothing GcCalloc)))
              (parseCommandPure ["compile", "Main.hs", "-o", "hello", "--keep-asm"]),
          testCase "parses keep-core" $
            assertEqual
              "command"
              (Right (CmdCompile (CompileOptions "Main.hs" Nothing True False False False Nothing GcCalloc)))
              (parseCommandPure ["compile", "Main.hs", "--keep-core"]),
          testCase "parses keep-grin" $
            assertEqual
              "command"
              (Right (CmdCompile (CompileOptions "Main.hs" Nothing False True False False Nothing GcCalloc)))
              (parseCommandPure ["compile", "Main.hs", "--keep-grin"]),
          testCase "parses whole-program compatibility mode" $
            assertEqual
              "command"
              (Right (CmdCompile (CompileOptions "Main.hs" Nothing False False False True Nothing GcCalloc)))
              (parseCommandPure ["compile", "Main.hs", "--whole-program"]),
          testCase "parses a cross-compilation target" $
            assertEqual
              "command"
              (Right (CmdCompile (CompileOptions "Main.hs" Nothing False False False False (Just LinuxAmd64) GcCalloc)))
              (parseCommandPure ["compile", "Main.hs", "--target", "linux-amd64"]),
          testCase "parses the portable C target" $
            assertEqual
              "command"
              (Right (CmdCompile (CompileOptions "Main.hs" Nothing False False False False (Just PortableC) GcCalloc)))
              (parseCommandPure ["compile", "Main.hs", "--target", "portable-c"]),
          testCase "selects the semispace collector" $
            assertEqual
              "command"
              (Right (CmdCompile (CompileOptions "Main.hs" Nothing False False False False Nothing GcSemispace)))
              (parseCommandPure ["compile", "Main.hs", "--gc", "semispace"]),
          testCase "derives safe default compile output paths" $ do
            assertEqual "Haskell source" "src/Main" (compileOutputPath (CompileOptions "src/Main.hs" Nothing False False False False Nothing GcCalloc))
            assertEqual "extensionless source" "program.out" (compileOutputPath (CompileOptions "program" Nothing False False False False Nothing GcCalloc)),
          testCase "parses install package" $
            assertEqual
              "command"
              (Right (CmdInstall (InstallOptions "text" Nothing Nothing False False False InstallErrorsHuman)))
              (parseCommandPure ["install", "text"]),
          testCase "parses install version" $
            assertEqual
              "command"
              (Right (CmdInstall (InstallOptions "text" (Just "2.1") Nothing False False False InstallErrorsHuman)))
              (parseCommandPure ["install", "text", "--version", "2.1"]),
          testCase "parses install offline and store" $
            assertEqual
              "command"
              (Right (CmdInstall (InstallOptions "text" Nothing (Just "/tmp/aihc-store") True False False InstallErrorsHuman)))
              (parseCommandPure ["install", "text", "--offline", "--store", "/tmp/aihc-store"]),
          testCase "parses install dry run" $
            assertEqual
              "command"
              (Right (CmdInstall (InstallOptions "text" Nothing Nothing False True False InstallErrorsHuman)))
              (parseCommandPure ["install", "text", "--dry-run"]),
          testCase "parses first error module and JSON errors" $
            assertEqual
              "command"
              (Right (CmdInstall (InstallOptions "text" Nothing Nothing False False True InstallErrorsJson)))
              (parseCommandPure ["install", "text", "--first-error-module", "--json-errors"]),
          testCase "rejects explicit dependency variants" $
            assertLeftContains "dependency" (parseCommandPure ["install", "a", "--dependency", "b=1.0.0:abcdef"]),
          testCase "parses repl" $
            assertEqual "command" (Right (CmdRepl (ReplOptions Nothing))) (parseCommandPure ["repl"]),
          testCase "parses repl store" $
            assertEqual "command" (Right (CmdRepl (ReplOptions (Just "/tmp/aihc-store")))) (parseCommandPure ["repl", "--store", "/tmp/aihc-store"])
        ],
      testGroup
        "compile"
        [ testCase "lowers the aihc-base HelloWorld example to native assembly" $
            withTempDir "aihc-compile-example" $ \root -> do
              sourcePath <- helloWorldExamplePath
              source <- TIO.readFile sourcePath
              let repositoryRoot = takeDirectory (takeDirectory (takeDirectory sourcePath))
                  environment = CompileEnvironment (repositoryRoot </> "core-libs") (root </> "cache")
              forM_ [AppleArm64, LinuxAmd64, PortableC] $ \target -> do
                result <- compileSourceToAssemblyWithDependenciesFor target environment sourcePath source
                case result of
                  Left err -> assertFailure ("expected " <> show target <> " compile success, got: " <> show err)
                  Right assembly -> do
                    assertBool "target entry" (targetMainDirective target `T.isInfixOf` assembly)
                    assertBool "dependency initializer call" (targetInitializerCall target `T.isInfixOf` assembly)
                    assertBool "Haskell tail transfer" (targetTailTransfer target `T.isInfixOf` assembly),
          testCase "assembles an executable and honors keep-output flags" test_compileExecutable,
          testCase "assembles an incremental portable C executable" test_compilePortableCExecutable,
          testCase "lowers every example to portable C" test_compilePortableCExamples,
          testCase "compiles and runs the aihc-base green threads example" test_compileGreenThreadsExample,
          testCase "compiles and runs the async stdio example" test_compileAsyncStdioExample,
          testCase "uses the shared XDG cache for compiled dependencies" test_compileDefaultEnvironment,
          testCase "builds and caches implicit core dependencies" test_compileImplicitCoreDependencies,
          testCase "skips default dependencies under NoImplicitPrelude" test_compileNoImplicitPrelude,
          testCase "builds explicit incremental imports under NoImplicitPrelude" test_compileExplicitCoreImport,
          testCase "compiles mutually recursive modules as one SCC unit" test_compileMutuallyRecursiveModules
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
            step <- handleReplInput session ":unknown"
            assertEqual "step" (ReplContinue (Just "unknown command: :unknown")) step,
          testCase "browses exported Prelude types and terms" $ do
            session <- loadReplSession Nothing
            step <- handleReplInput session ":browse Prelude"
            case step of
              ReplContinue (Just output) -> do
                assertOutputLinePrefix "exported type" "type Bool" output
                assertOutputLine "local function" "id ∷ ∀ a. a → a" output
                assertOutputLine "symbolic function" "(++) ∷ ∀ a. [a] → [a] → [a]" output
                assertOutputLine "re-exported function" "not ∷ Bool → Bool" output
                assertBool "private helper should not be browsable" (not ("fmapList ∷" `isInfixOf` output))
              other -> assertFailure ("expected browse output, got " <> show other),
          testCase "reports browse usage and unknown modules" $ do
            session <- testReplSession
            handleReplInput session ":browse"
              >>= assertEqual "missing module" (ReplContinue (Just "usage: :browse <module>"))
            handleReplInput session ":browse Does.Not.Exist"
              >>= assertEqual "unknown module" (ReplContinue (Just "unknown module: Does.Not.Exist")),
          testCase "completes browse module names" $ do
            session <- loadReplSession Nothing
            (_, completions) <- replCompletion session (reverse ":browse Pre", "")
            assertEqual "module completions" ["Prelude"] (map Haskeline.replacement completions)
            (_, unrelatedCompletions) <- replCompletion session (reverse "Pre", "")
            assertEqual "unrelated completions" [] unrelatedCompletions,
          testCase "reports expression types with long and short commands" $ do
            session <- loadReplSession Nothing
            longStep <- handleReplInput session ":type True"
            assertEqual "long command" (ReplContinue (Just "True ∷ Bool")) longStep
            shortStep <- handleReplInput session ":t not"
            assertEqual "short command" (ReplContinue (Just "not ∷ Bool → Bool")) shortStep
            polymorphicStep <- handleReplInput session ":t \\x -> x"
            assertEqual "polymorphic expression" (ReplContinue (Just "\\x -> x ∷ ∀ a. a → a")) polymorphicStep,
          testCase "solves and normalizes type command constraints" $ do
            session <- loadReplSession Nothing
            operatorStep <- handleReplInput session ":t (+)"
            assertEqual "operator" (ReplContinue (Just "(+) ∷ ∀ a. (Num a) ⇒ a → a → a")) operatorStep
            partialStep <- handleReplInput session ":t (+) 1"
            assertEqual "polymorphic partial application" (ReplContinue (Just "(+) 1 ∷ ∀ a. (Num a) ⇒ a → a")) partialStep
            intStep <- handleReplInput session ":t (+) (1::Int)"
            assertEqual "concrete instance" (ReplContinue (Just "(+) (1::Int) ∷ Int → Int")) intStep
            boolStep <- handleReplInput session ":t (+) (1::Bool)"
            assertEqual "missing concrete instance" (ReplContinue (Just "type error: no instance for Num Bool")) boolStep,
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
            result <- evaluateExpression session "otherwise"
            assertEqual "result" (Right "True") result,
          testCase "loads installed base interface for Prelude MVP scope" test_loadsInstalledBaseInterfaceForRepl
        ],
      testGroup
        "install"
        [ testCase "builds stable store paths" test_stableStorePath,
          testCase "recursive dependencies affect store paths" test_recursiveDependenciesAffectStorePaths,
          testCase "plans library components without executables" test_plansLibraryComponentsWithoutExecutables,
          testCase "writes scaffold artifacts" test_writeInstallScaffold,
          testCase "writes dependency scaffold artifacts first" test_writeInstallScaffoldWritesDependencies,
          testCase "reports parse errors and writes no artifacts" test_reportsParseErrorsAndWritesNoArtifacts,
          testCase "reports rename errors and writes no artifacts" test_reportsRenameErrorsAndWritesNoArtifacts,
          testCase "renders preprocessed source for rename errors" test_rendersPreprocessedSourceForRenameErrors,
          testCase "reports type-check errors and writes no artifacts" test_reportsTypeCheckErrorsAndWritesNoArtifacts,
          testCase "reports desugar errors and writes no artifacts" test_reportsDesugarErrorsAndWritesNoArtifacts,
          testCase "limits rendered install errors to first module" test_limitsRenderedInstallErrorsToFirstModule,
          testCase "renders human install errors" test_rendersHumanInstallErrors,
          testCase "renders JSON install errors on request" test_rendersJsonInstallErrors,
          testCase "does not install dependencies when root package fails" test_failedRootDoesNotInstallDependencies,
          testCase "type-checks references to dependency bindings" test_typeChecksReferencesToDependencyBindings,
          testCase "checks cast-style instance methods using dependency id" test_checksCastStyleDependencyId,
          testCase "checks constraint-kinded multi-parameter classes" test_checksConstraintKindedMultiParameterClasses,
          testCase "checks packages without writing install artifacts" test_checkPackagePlanWritesNoArtifacts,
          testCase "uses local provider for base dependencies" test_usesLocalProviderForBase,
          testCase "uses local provider for ghc-prim dependencies" test_usesLocalProviderForGhcPrim,
          testCase "uses local provider for ghc-internal dependencies" test_usesLocalProviderForGhcInternal,
          testCase "uses virtual provider for system-cxx-std-lib dependencies" test_usesVirtualProviderForSystemCxxStdLib,
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

assertOutputLine :: String -> String -> String -> Assertion
assertOutputLine label expected output =
  assertBool (label <> ": expected line " <> show expected <> ", got:\n" <> output) (expected `elem` lines output)

assertOutputLinePrefix :: String -> String -> String -> Assertion
assertOutputLinePrefix label expectedPrefix output =
  assertBool (label <> ": expected line starting with " <> show expectedPrefix <> ", got:\n" <> output) (any (expectedPrefix `isPrefixOf`) (lines output))

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
        replBindingTypes = Map.empty,
        replImportedInstances = [],
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

test_plansLibraryComponentsWithoutExecutables :: Assertion
test_plansLibraryComponentsWithoutExecutables =
  withTempDir "aihc-cli" $ \root -> do
    let sourceRoot = root </> "source"
        depRoot = root </> "dep"
        storeRoot = root </> "store"
    createDirectoryIfMissing True (sourceRoot </> "src")
    createDirectoryIfMissing True (sourceRoot </> "internal" </> "Demo")
    createDirectoryIfMissing True (sourceRoot </> "app")
    createDirectoryIfMissing True storeRoot
    writeFile (sourceRoot </> "demo.cabal") libraryOnlyInstallCabal
    writeFile (sourceRoot </> "src" </> "Demo.hs") "module Demo where\nimport Demo.Internal\nimport Dep\nx = depId internalValue\n"
    writeFile (sourceRoot </> "internal" </> "Demo" </> "Internal.hs") "module Demo.Internal where\ninternalValue = ()\n"
    writeFile (sourceRoot </> "app" </> "Main.hs") "module Main where\nthis executable is intentionally invalid\n"
    createFixturePackageWithSource depRoot "dep" "1.0.0" "Dep" [] "module Dep where\ndepId x = x\n"

    plan <-
      buildPackagePlanWithResolver
        (fixtureDependencyResolver sourceRoot [("dep", "1.0.0", depRoot)])
        storeRoot
        (PackageSpec "demo" "0.1.0.0")

    assertEqual "library source file count" 2 (planSourceFileCount plan)
    assertEqual "external library dependencies" ["dep"] [pkgName (packageKeySpec (planPackageKey dependencyPlan)) | dependencyPlan <- planDependencyPlans plan]
    _ <- expectInstallSuccess (checkPackagePlan plan)
    pure ()

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
    assertBool "manifest records completed desugaring" ("desugar-system-fc" `isInfixOf` renderedManifest && "Generate desugared System-FC data files" `isInfixOf` renderedManifest)

    interfaceJson <- BL8.readFile (resultInterfacePath result)
    let renderedInterface = BL8.unpack interfaceJson
    assertBool "interface includes fixities" ("fixities" `isInfixOf` renderedInterface)
    assertBool "interface includes modules" ("modules" `isInfixOf` renderedInterface)
    assertBool "interface includes typecheck data" ("typecheck" `isInfixOf` renderedInterface)
    assertBool "interface is implemented" (not ("unimplemented" `isInfixOf` renderedInterface))

    fcJson <- BL8.readFile (resultFcPath result)
    let renderedFc = BL8.unpack fcJson
    assertBool "fc artifact includes system-fc" ("system-fc" `isInfixOf` renderedFc)
    assertBool "fc artifact is complete" ("\"status\":\"complete\"" `isInfixOf` renderedFc)
    assertBool "fc artifact contains desugared modules" ("\"program\"" `isInfixOf` renderedFc)

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

test_reportsParseErrorsAndWritesNoArtifacts :: Assertion
test_reportsParseErrorsAndWritesNoArtifacts =
  withTempDir "aihc-cli" $ \root -> do
    let sourceRoot = root </> "source"
        storeRoot = root </> "store"
    createFixturePackageWithSource sourceRoot "demo" "0.1.0.0" "Demo" [] "module Demo where\nx =\n"
    createDirectoryIfMissing True storeRoot
    plan <- buildPackagePlanFromSource storeRoot (PackageSpec "demo" "0.1.0.0") sourceRoot

    err <- renderInstallFailure <$> expectInstallFailure (writeInstallScaffold plan)
    assertBool "error reports parse phase" ("parse errors" `isInfixOf` err)
    assertFileDoesNotExist (planStorePath plan </> "manifest.json")
    assertFileDoesNotExist (planStorePath plan </> "interfaces" </> "package-interface.json")
    assertFileDoesNotExist (planStorePath plan </> "fc" </> "package-fc.json")

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
    createFixturePackageWithSource sourceRoot "demo" "0.1.0.0" "Demo" [] "module Demo where\nx = [(), 'a']\n"
    createDirectoryIfMissing True storeRoot
    plan <- buildPackagePlanFromSource storeRoot (PackageSpec "demo" "0.1.0.0") sourceRoot

    err <- renderInstallFailure <$> expectInstallFailure (writeInstallScaffold plan)
    assertBool "error reports package" ("failed to install demo-0.1.0.0" `isInfixOf` err)
    assertBool "error reports type-check phase" ("type-check errors" `isInfixOf` err)
    assertBool "error includes unification diagnostic" ("UnificationError" `isInfixOf` err)
    assertFileDoesNotExist (planStorePath plan </> "manifest.json")
    assertFileDoesNotExist (planStorePath plan </> "interfaces" </> "package-interface.json")
    assertFileDoesNotExist (planStorePath plan </> "fc" </> "package-fc.json")

test_reportsDesugarErrorsAndWritesNoArtifacts :: Assertion
test_reportsDesugarErrorsAndWritesNoArtifacts =
  withTempDir "aihc-cli" $ \root -> do
    let sourceRoot = root </> "source"
        storeRoot = root </> "store"
        source = "{-# LANGUAGE MagicHash #-}\nmodule Demo where\nforeign import prim mystery# :: Int# -> Int#\n"
    createFixturePackageWithSource sourceRoot "demo" "0.1.0.0" "Demo" [] source
    createDirectoryIfMissing True storeRoot
    plan <- buildPackagePlanFromSource storeRoot (PackageSpec "demo" "0.1.0.0") sourceRoot

    err <- renderInstallFailure <$> expectInstallFailure (writeInstallScaffold plan)
    assertBool "error reports desugar phase" ("desugar errors" `isInfixOf` err)
    assertBool "error includes unknown primitive" ("unknown foreign import prim" `isInfixOf` err)
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
  let err = renderInstallFailure syntheticInstallFailure
  assertBool ("error includes human location, got:\n" <> err) ("src/Demo.hs:2:3: error: [Demo] selected module failed" `isInfixOf` err)
  assertBool ("error includes source line, got:\n" <> err) ("  2 | x = selected + problem" `isInfixOf` err)
  assertBool ("error includes caret range, got:\n" <> err) ("    |   ^^^^^^^^" `isInfixOf` err)
  assertBool "error omits JSON object syntax" (not ("{\"" `isInfixOf` err))

test_rendersJsonInstallErrors :: Assertion
test_rendersJsonInstallErrors = do
  let opts =
        defaultInstallOptionsForTest
          { installErrorFormat = InstallErrorsJson
          }
      err = renderInstallFailureWithOptions opts syntheticInstallFailure
  assertBool ("error includes JSON diagnostic, got:\n" <> err) ("{\"message\":\"selected module failed\"" `isInfixOf` err)

test_rendersPreprocessedSourceForRenameErrors :: Assertion
test_rendersPreprocessedSourceForRenameErrors =
  withTempDir "aihc-cli" $ \root -> do
    let sourceRoot = root </> "source"
        storeRoot = root </> "store"
        source = "{-# LANGUAGE CPP #-}\nmodule Demo where\n#define MISSING missingAfterCpp\nx = MISSING\n"
    createFixturePackageWithSource sourceRoot "demo" "0.1.0.0" "Demo" [] source
    createDirectoryIfMissing True storeRoot
    plan <- buildPackagePlanFromSource storeRoot (PackageSpec "demo" "0.1.0.0") sourceRoot

    err <- renderInstallFailure <$> expectInstallFailure (writeInstallScaffold plan)
    assertBool ("error explains the unbound name, got:\n" <> err) ("unbound term name ‘missingAfterCpp’" `isInfixOf` err)
    assertBool ("error includes postprocessed source, got:\n" <> err) ("| x = missingAfterCpp" `isInfixOf` err)
    assertBool ("error includes a caret, got:\n" <> err) ("^^^^^^^^^^^^^^^" `isInfixOf` err)
    assertBool "error omits preprocessed macro use" (not ("| x = MISSING" `isInfixOf` err))

defaultInstallOptionsForTest :: InstallOptions
defaultInstallOptionsForTest =
  InstallOptions "demo" Nothing Nothing False False False InstallErrorsHuman

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

test_typeChecksReferencesToDependencyBindings :: Assertion
test_typeChecksReferencesToDependencyBindings =
  withTempDir "aihc-cli" $ \root -> do
    let sourceRoot = root </> "source"
        depRoot = root </> "dep"
        storeRoot = root </> "store"
    createFixturePackageWithSource sourceRoot "demo" "0.1.0.0" "Demo" ["dep >=1 && <2"] "module Demo where\nimport Dep\ny = depId ()\n"
    createFixturePackageWithSource depRoot "dep" "1.0.0" "Dep" [] "module Dep where\ndepId :: a -> a\ndepId x = x\n"
    createDirectoryIfMissing True storeRoot
    plan <-
      buildPackagePlanWithResolver
        (fixtureDependencyResolver sourceRoot [("dep", "1.0.0", depRoot)])
        storeRoot
        (PackageSpec "demo" "0.1.0.0")

    _ <- expectInstallSuccess (writeInstallScaffold plan)
    assertFileExists (planStorePath plan </> "manifest.json")

test_checksCastStyleDependencyId :: Assertion
test_checksCastStyleDependencyId =
  withTempDir "aihc-cli" $ \root -> do
    let sourceRoot = root </> "source"
        depRoot = root </> "dep"
        storeRoot = root </> "store"
        source =
          unlines
            [ "{-# LANGUAGE FlexibleInstances #-}",
              "{-# LANGUAGE MultiParamTypeClasses #-}",
              "{-# LANGUAGE NoImplicitPrelude #-}",
              "module Demo where",
              "import Dep (id)",
              "class Cast a b where",
              "  cast :: a -> b",
              "instance Cast a a where",
              "  cast = id"
            ]
    createFixturePackageWithSource sourceRoot "demo" "0.1.0.0" "Demo" ["dep >=1 && <2"] source
    createFixturePackageWithSource depRoot "dep" "1.0.0" "Dep" [] "module Dep where\nid :: a -> a\nid x = x\n"
    createDirectoryIfMissing True storeRoot
    plan <-
      buildPackagePlanWithResolver
        (fixtureDependencyResolver sourceRoot [("dep", "1.0.0", depRoot)])
        storeRoot
        (PackageSpec "demo" "0.1.0.0")

    result <- expectInstallSuccess (writeInstallScaffold plan)
    fcJson <- BL8.readFile (resultFcPath result)
    let renderedFc = BL8.unpack fcJson
    assertBool "FC artifact constructs an ordinary Cast dictionary" ("$Dict$Cast @a @a" `isInfixOf` renderedFc)
    assertBool "FC artifact applies imported id" ("id @a" `isInfixOf` renderedFc)

test_checksConstraintKindedMultiParameterClasses :: Assertion
test_checksConstraintKindedMultiParameterClasses =
  withTempDir "aihc-cli" $ \root -> do
    let sourceRoot = root </> "source"
        storeRoot = root </> "store"
        source =
          unlines
            [ "{-# LANGUAGE ConstraintKinds #-}",
              "{-# LANGUAGE MultiParamTypeClasses #-}",
              "{-# LANGUAGE NoImplicitPrelude #-}",
              "{-# LANGUAGE UndecidableSuperClasses #-}",
              "module Demo where",
              "class Eq a",
              "class c t => Lawful c t",
              "data Bool = False | True",
              "instance Eq Bool",
              "instance Lawful Eq Bool"
            ]
    createFixturePackageWithSource sourceRoot "demo" "0.1.0.0" "Demo" [] source
    createDirectoryIfMissing True storeRoot
    plan <- buildPackagePlanWithResolver (fixtureDependencyResolver sourceRoot []) storeRoot (PackageSpec "demo" "0.1.0.0")

    _ <- expectInstallSuccess (writeInstallScaffold plan)
    pure ()

test_checkPackagePlanWritesNoArtifacts :: Assertion
test_checkPackagePlanWritesNoArtifacts =
  withDependencyFixture $ \sourceRoot depRoot _ storeRoot -> do
    plan <-
      buildPackagePlanWithResolver
        (fixtureDependencyResolver sourceRoot [("dep", "1.0.0", depRoot)])
        storeRoot
        (PackageSpec "demo" "0.1.0.0")
    _ <- expectInstallSuccess (checkPackagePlan plan)
    mapM_ (assertFileDoesNotExist . (</> "manifest.json") . planStorePath) (flattenPackagePlans plan)

test_usesLocalProviderForGhcPrim :: Assertion
test_usesLocalProviderForGhcPrim =
  withCoreDependencyFixture "ghc-prim" $ \sourceRoot storeRoot -> do
    plan <-
      buildPackagePlanWithResolver
        (fixtureDependencyResolver sourceRoot [])
        storeRoot
        (PackageSpec "demo" "0.1.0.0")
    assertDependencySpec plan (PackageSpec "aihc-prim" "0.13.0")

test_usesLocalProviderForBase :: Assertion
test_usesLocalProviderForBase =
  withCoreDependencyFixture "base" $ \sourceRoot storeRoot -> do
    plan <-
      buildPackagePlanWithResolver
        (fixtureDependencyResolver sourceRoot [])
        storeRoot
        (PackageSpec "demo" "0.1.0.0")
    assertDependencySpec plan (PackageSpec "aihc-base" "4.21.2.0")

test_usesLocalProviderForGhcInternal :: Assertion
test_usesLocalProviderForGhcInternal =
  withCoreDependencyFixture "ghc-internal" $ \sourceRoot storeRoot -> do
    plan <-
      buildPackagePlanWithResolver
        (fixtureDependencyResolver sourceRoot [])
        storeRoot
        (PackageSpec "demo" "0.1.0.0")
    assertDependencySpec plan (PackageSpec "aihc-internal" "9.1204.0")

test_usesVirtualProviderForSystemCxxStdLib :: Assertion
test_usesVirtualProviderForSystemCxxStdLib =
  withCoreDependencyFixture "system-cxx-std-lib" $ \sourceRoot storeRoot -> do
    plan <-
      buildPackagePlanWithResolver
        (fixtureDependencyResolver sourceRoot [])
        storeRoot
        (PackageSpec "demo" "0.1.0.0")
    assertDependencySpec plan (PackageSpec "system-cxx-std-lib" "1.0")
    dependencyPlan <- findPlanByName "system-cxx-std-lib" (planDependencyPlans plan)
    assertEqual "virtual package source file count" 0 (planSourceFileCount dependencyPlan)
    _ <- expectInstallSuccess (checkPackagePlan plan)
    pure ()

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
  withCoreDependencyFixture "base" $ \sourceRoot storeRoot -> do
    plan <-
      buildPackagePlanWithResolver
        (fixtureDependencyResolver sourceRoot [])
        storeRoot
        (PackageSpec "demo" "0.1.0.0")
    _ <- expectInstallSuccess (writeInstallScaffold plan)
    let plans = flattenPackagePlans plan
        installedNames = sort [pkgName (packageKeySpec (planPackageKey item)) | item <- plans]
    assertEqual "installed packages" ["aihc-base", "demo"] installedNames
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

test_compileExecutable :: Assertion
test_compileExecutable =
  when (isNativeCodegenHost arch os) $
    withTempDir "aihc-compile" $ \root -> do
      sourcePath <- helloWorldExamplePath
      let repositoryRoot = takeDirectory (takeDirectory (takeDirectory sourcePath))
          keptOutput = root </> "kept"
          temporaryOutput = root </> "temporary"
          environment = CompileEnvironment (repositoryRoot </> "core-libs") (root </> "cache")
          keptOptions = CompileOptions sourcePath (Just keptOutput) True True True False Nothing GcSemispace
          temporaryOptions = CompileOptions sourcePath (Just temporaryOutput) False False False True Nothing GcCalloc
      withCurrentDirectory repositoryRoot $ do
        runCompileWithEnvironment environment keptOptions
        assertFileExists keptOutput
        assertFileExists (keptOutput <> ".core")
        assertFileExists (keptOutput <> ".grin")
        assertFileExists (keptOutput <> ".cps.grin")
        assertFileExists (keptOutput <> ".gc.grin")
        assertFileExists (keptOutput <> ".s")
        core <- TIO.readFile (keptOutput <> ".core")
        grin <- TIO.readFile (keptOutput <> ".grin")
        cpsGrin <- TIO.readFile (keptOutput <> ".cps.grin")
        gcGrin <- TIO.readFile (keptOutput <> ".gc.grin")
        assertBool "core contains main" ("main" `T.isInfixOf` core)
        assertBool "GRIN contains main" ("main" `T.isInfixOf` grin)
        assertBool "CPS-GRIN contains allocated continuations" ("store (P$cps$" `T.isInfixOf` cpsGrin)
        assertBool "GC-GRIN contains explicit heap reservations" ("ensure-heap " `T.isInfixOf` gcGrin)
        assertBool "GC-GRIN contains unchecked allocations" ("store-unchecked " `T.isInfixOf` gcGrin)
        assertBool "GRIN erases the IO constructor" (not ("constructor IO/" `T.isInfixOf` grin))
        assertBool "GRIN erases the CInt constructor" (not ("constructor CInt/" `T.isInfixOf` grin))
        assertBool "GRIN does not allocate putchar globally" (not ("global putchar" `T.isInfixOf` grin || "caf putchar" `T.isInfixOf` grin))
        assertBool "GRIN does not allocate char globally" (not ("global char" `T.isInfixOf` grin || "caf char" `T.isInfixOf` grin))
        assertBool "GRIN uses direct known calls" ("call @(BoxedRep Lifted) $entry$>>" `T.isInfixOf` grin)
        assertBool "GRIN makes evaluation explicit" ("eval @" `T.isInfixOf` grin)
        assertNativeOutput "Hello, world!\n" keptOutput

        runCompileWithEnvironment environment temporaryOptions
        assertFileExists temporaryOutput
        assertFileDoesNotExist (temporaryOutput <> ".core")
        assertFileDoesNotExist (temporaryOutput <> ".grin")
        assertFileDoesNotExist (temporaryOutput <> ".cps.grin")
        assertFileDoesNotExist (temporaryOutput <> ".gc.grin")
        assertFileDoesNotExist (temporaryOutput <> ".s")
        assertNativeOutput "Hello, world!\n" temporaryOutput

test_compileGreenThreadsExample :: Assertion
test_compileGreenThreadsExample =
  when (isNativeCodegenHost arch os) $
    withTempDir "aihc-compile-green-threads" $ \root -> do
      sourcePath <- greenThreadsExamplePath
      let repositoryRoot = takeDirectory (takeDirectory (takeDirectory sourcePath))
          output = root </> "green-threads"
          environment = CompileEnvironment (repositoryRoot </> "core-libs") (root </> "cache")
          options = CompileOptions sourcePath (Just output) False False False False Nothing GcCalloc
      withCurrentDirectory repositoryRoot $ do
        runCompileWithEnvironment environment options
        assertNativeOutput
          "Hello world main green thread\nStill in main\nHello from forked thread\nBack in main\n"
          output

test_compileAsyncStdioExample :: Assertion
test_compileAsyncStdioExample =
  withTempDir "aihc-compile-async-stdio" $ \root -> do
    sourcePath <- asyncStdioExamplePath
    let repositoryRoot = takeDirectory (takeDirectory (takeDirectory sourcePath))
        environment = CompileEnvironment (repositoryRoot </> "core-libs") (root </> "cache")
        targets =
          [PortableC]
            <> [AppleArm64 | arch == "aarch64" && os == "darwin"]
            <> [LinuxAmd64 | arch == "x86_64" && os == "linux"]
    withCurrentDirectory repositoryRoot $
      forM_ targets $ \target -> do
        let output = root </> ("async-stdio-" <> show target)
            options = CompileOptions sourcePath (Just output) False False False False (Just target) GcSemispace
        runCompileWithEnvironment environment options
        (Just childInput, Just childOutput, Just childError, processHandle) <-
          createProcess
            (proc output [])
              { std_in = CreatePipe,
                std_out = CreatePipe,
                std_err = CreatePipe
              }
        threadDelay 50000
        hPutStr childInput "Buffered async IO\n"
        hFlush childInput
        hClose childInput
        programOut <- TIO.hGetContents childOutput
        programErr <- TIO.hGetContents childError
        exitCode <- waitForProcess processHandle
        assertEqual (show target <> " stderr: " <> T.unpack programErr) ExitSuccess exitCode
        assertEqual (show target <> " stdout") "Buffered async IO\n" programOut

test_compilePortableCExecutable :: Assertion
test_compilePortableCExecutable =
  withTempDir "aihc-compile-portable-c" $ \root -> do
    sourcePath <- helloWorldExamplePath
    let repositoryRoot = takeDirectory (takeDirectory (takeDirectory sourcePath))
        output = root </> "hello-portable-c"
        cacheRoot = root </> "cache"
        environment = CompileEnvironment (repositoryRoot </> "core-libs") cacheRoot
        options = CompileOptions sourcePath (Just output) False False True False (Just PortableC) GcCalloc
    withCurrentDirectory repositoryRoot $ do
      runCompileWithEnvironment environment options
      assertFileExists output
      assertFileExists (output <> ".c")
      generatedC <- TIO.readFile (output <> ".c")
      assertBool "portable C main" ("int main(void)" `T.isInfixOf` generatedC)
      assertBool "dependency initializer call" ("_aihc_init_" `T.isInfixOf` generatedC)
      assertNativeOutput "Hello, world!\n" output
      objectFiles <- compileCacheArtifacts ".o" cacheRoot
      archiveFiles <- compileCacheArtifacts ".a" cacheRoot
      assertBool "cached C dependency objects" (not (null objectFiles))
      assertBool "cached C dependency archives" (not (null archiveFiles))
      let oldTimestamp = UTCTime (fromGregorian 2000 1 1) 0
      mapM_ (`setModificationTime` oldTimestamp) (objectFiles <> archiveFiles)
      runCompileWithEnvironment environment options
      mapM_ (getModificationTime >=> assertEqual "cache hit preserves C artifact" oldTimestamp) (objectFiles <> archiveFiles)

test_compilePortableCExamples :: Assertion
test_compilePortableCExamples =
  withTempDir "aihc-compile-portable-c-examples" $ \root -> do
    sourcePaths <- sequence [helloWorldExamplePath, greenThreadsExamplePath, asyncStdioExamplePath, unboxedTailRecursionExamplePath]
    forM_ sourcePaths $ \sourcePath -> do
      source <- TIO.readFile sourcePath
      let repositoryRoot = takeDirectory (takeDirectory (takeDirectory sourcePath))
          environment = CompileEnvironment (repositoryRoot </> "core-libs") (root </> "cache")
      result <- compileSourceToAssemblyWithDependenciesFor PortableC environment sourcePath source
      case result of
        Left err -> assertFailure ("expected portable C compile success for " <> sourcePath <> ", got: " <> show err)
        Right generatedC -> do
          assertBool "includes the portable runtime" ("#include \"aihc_runtime.h\"" `T.isInfixOf` generatedC)
          assertBool "uses trampoline dispatch" ("while (aihc_next_transfer.entry != NULL)" `T.isInfixOf` generatedC)

isNativeCodegenHost :: String -> String -> Bool
isNativeCodegenHost hostArch hostOs =
  (hostArch == "aarch64" && hostOs == "darwin")
    || (hostArch == "x86_64" && hostOs == "linux")

test_compileDefaultEnvironment :: Assertion
test_compileDefaultEnvironment =
  withTempDir "aihc-compile-environment" $ \root -> do
    let workingDirectory = root </> "project"
        cacheHome = root </> "cache"
    createDirectoryIfMissing True workingDirectory
    bracket
      (lookupEnv "XDG_CACHE_HOME")
      restoreCacheHome
      ( \_ -> do
          setEnv "XDG_CACHE_HOME" cacheHome
          withCurrentDirectory workingDirectory $ do
            actualWorkingDirectory <- getCurrentDirectory
            environment <- defaultCompileEnvironment
            assertEqual "core libraries" (actualWorkingDirectory </> "core-libs") (compileCoreLibraryRoot environment)
            assertEqual "compiled dependency cache" (cacheHome </> "aihc" </> "libraries") (compileCacheRoot environment)
      )
  where
    restoreCacheHome Nothing = unsetEnv "XDG_CACHE_HOME"
    restoreCacheHome (Just value) = setEnv "XDG_CACHE_HOME" value

test_compileImplicitCoreDependencies :: Assertion
test_compileImplicitCoreDependencies =
  withTempDir "aihc-compile-dependencies" $ \root -> do
    let sourcePath = "Main.hs"
        coreRoot = root </> "core-libs"
        cacheRoot = root </> "cache"
        environment = CompileEnvironment coreRoot cacheRoot
        implicitSource = implicitDependencySource
    createCompileLibrary coreRoot "aihc-prim" "GHC.Prim" "module GHC.Prim where\n"
    createCompileLibrary coreRoot "aihc-base" "BaseSupport" "module BaseSupport where\nsupport x = x\n"
    createCompileLibrary coreRoot "aihc-base" "Prelude" "module Prelude where\nimport BaseSupport\nid x = support x\n"
    writeFile (coreRoot </> "aihc-base" </> "src" </> "Unused.hs") "module Unused where\nunused = 1\n"

    expectCompileSuccess =<< compileSourceToAssemblyWithDependencies environment sourcePath implicitSource
    cacheFiles <- compileCacheFiles cacheRoot
    assertEqual "one dependency artifact" 1 (length cacheFiles)
    cachePath <- case cacheFiles of
      [path] -> pure path
      paths -> assertFailure ("expected one cache file, got " <> show paths)
    objectFiles <- compileCacheArtifacts ".o" cacheRoot
    archiveFiles <- compileCacheArtifacts ".a" cacheRoot
    assertEqual "one object per dependency module" 3 (length objectFiles)
    assertEqual "one archive per dependency library" 2 (length archiveFiles)

    let oldTimestamp = UTCTime (fromGregorian 2000 1 1) 0
    setModificationTime cachePath oldTimestamp
    mapM_ (`setModificationTime` oldTimestamp) (objectFiles <> archiveFiles)
    expectCompileSuccess =<< compileSourceToAssemblyWithDependencies environment sourcePath implicitSource
    getModificationTime cachePath >>= assertEqual "cache hit preserves artifact" oldTimestamp
    mapM_ (getModificationTime >=> assertEqual "cache hit preserves native artifact" oldTimestamp) (objectFiles <> archiveFiles)

    writeFile (coreRoot </> "aihc-base" </> "src" </> "Unused.hs") "module Unused where\nunused = 2\n"
    expectCompileSuccess =<< compileSourceToAssemblyWithDependencies environment sourcePath implicitSource
    compileCacheFiles cacheRoot >>= assertEqual "unused library input changes the graph key" 2 . length

test_compileNoImplicitPrelude :: Assertion
test_compileNoImplicitPrelude =
  withTempDir "aihc-compile-no-prelude" $ \root -> do
    let environment = CompileEnvironment (root </> "missing-core-libs") (root </> "cache")
    expectCompileSuccess =<< compileSourceToAssemblyWithDependencies environment "Main.hs" noImplicitDependencySource
    cacheExists <- doesDirectoryExist (compileCacheRoot environment)
    assertBool "NoImplicitPrelude should not create a dependency cache" (not cacheExists)

test_compileExplicitCoreImport :: Assertion
test_compileExplicitCoreImport =
  withTempDir "aihc-compile-explicit-dependency" $ \root -> do
    let coreRoot = root </> "core-libs"
        cacheRoot = root </> "cache"
        environment = CompileEnvironment coreRoot cacheRoot
        withImport = T.replace "module Main where\n" "module Main where\n\nimport Demo (identity)\n" noImplicitDependencySource
        importedSource = T.replace "putchar (char 72#Int32)" "putchar (identity (char 72#Int32))" withImport
    createCompileLibrary
      coreRoot
      "demo"
      "Demo"
      ( unlines
          [ "{-# LANGUAGE NoImplicitPrelude #-}",
            "module Demo (identity) where",
            "data UnreachableDependencyType = UnreachableDependencyConstructor",
            "unreachableDependencyFunction = UnreachableDependencyConstructor",
            "dependencyImplementation x = let alias = x in alias",
            "identity x = dependencyImplementation x"
          ]
      )
    core <- expectCompileArtifact =<< compileSourceToCoreWithDependencies environment "Main.hs" importedSource
    grin <- expectCompileArtifact =<< compileSourceToGrinWithDependencies environment "Main.hs" importedSource
    cpsGrin <- expectCompileArtifact =<< compileSourceToCpsGrinWithDependencies environment "Main.hs" importedSource
    wholeCore <- expectCompileArtifact =<< compileSourceToWholeCoreWithDependencies environment "Main.hs" importedSource
    assertBool "dependency reference remains in incremental Core" ("identity" `T.isInfixOf` core)
    assertBool "dependency reference remains in incremental GRIN" ("identity" `T.isInfixOf` grin)
    assertBool "dependency reference remains in incremental CPS-GRIN" ("identity" `T.isInfixOf` cpsGrin)
    assertBool "CPS-GRIN preserves explicit evaluation" ("eval @" `T.isInfixOf` cpsGrin)
    assertBool "CPS-GRIN reifies evaluation continuations" ("store (P$cps$" `T.isInfixOf` cpsGrin)
    assertBool "dependency Core implementation is excluded" (not ("dependencyImplementation" `T.isInfixOf` core))
    assertBool "dependency GRIN implementation is excluded" (not ("dependencyImplementation" `T.isInfixOf` grin))
    assertBool "whole-program Core merges reachable dependency implementations" ("dependencyImplementation" `T.isInfixOf` wholeCore)
    assertBool ("whole-program Core retains a dependency alias:\n" <> T.unpack wholeCore) (not ("alias" `T.isInfixOf` wholeCore))
    assertBool "unreachable dependency function is excluded from Core" (not ("unreachableDependencyFunction" `T.isInfixOf` core))
    assertBool "unreachable dependency type is excluded from Core" (not ("UnreachableDependencyConstructor" `T.isInfixOf` core))
    assertBool "whole-program DCE excludes unreachable dependency functions" (not ("unreachableDependencyFunction" `T.isInfixOf` wholeCore))
    expectCompileSuccess =<< compileSourceToAssemblyWithDependencies environment "Main.hs" importedSource
    compileCacheFiles cacheRoot >>= assertEqual "explicit dependency artifact" 1 . length

test_compileMutuallyRecursiveModules :: Assertion
test_compileMutuallyRecursiveModules =
  withTempDir "aihc-compile-module-scc" $ \root -> do
    let coreRoot = root </> "core-libs"
        cacheRoot = root </> "cache"
        environment = CompileEnvironment coreRoot cacheRoot
        source = T.replace "module Main where\n" "module Main where\n\nimport Cycle.A (Token)\n" noImplicitDependencySource
    createCompileLibrary
      coreRoot
      "cycle"
      "Cycle.A"
      ( unlines
          [ "{-# LANGUAGE NoImplicitPrelude #-}",
            "module Cycle.A (Token, a) where",
            "import Cycle.B (b)",
            "data Token = Token",
            "a :: Token -> Token",
            "a = b"
          ]
      )
    createCompileLibrary
      coreRoot
      "cycle"
      "Cycle.B"
      ( unlines
          [ "{-# LANGUAGE NoImplicitPrelude #-}",
            "module Cycle.B (b) where",
            "import Cycle.A (Token, a)",
            "b :: Token -> Token",
            "b value = value",
            "back :: Token -> Token",
            "back = a"
          ]
      )
    expectCompileSuccess =<< compileSourceToAssemblyWithDependencies environment "Main.hs" source
    objectFiles <- compileCacheArtifacts ".o" cacheRoot
    assertEqual "one object for the two-module SCC" 1 (length objectFiles)

expectCompileSuccess :: Either CompileError T.Text -> Assertion
expectCompileSuccess result =
  case result of
    Left err -> assertFailure ("expected dependency-aware compile to succeed, got: " <> show err)
    Right assembly -> assertBool "native entry" (nativeMainDirective `T.isInfixOf` assembly)

nativeMainDirective :: T.Text
nativeMainDirective
  | arch == "x86_64" && os == "linux" = ".globl main"
  | otherwise = ".globl _main"

targetMainDirective :: NativeTarget -> T.Text
targetMainDirective AppleArm64 = ".globl _main"
targetMainDirective LinuxAmd64 = ".globl main"
targetMainDirective PortableC = "int main(void)"

targetInitializerCall :: NativeTarget -> T.Text
targetInitializerCall AppleArm64 = "bl _aihc_init_"
targetInitializerCall LinuxAmd64 = "call _aihc_init_"
targetInitializerCall PortableC = "_aihc_init_"

targetTailTransfer :: NativeTarget -> T.Text
targetTailTransfer AppleArm64 = "br x9"
targetTailTransfer LinuxAmd64 = "jmp r11"
targetTailTransfer PortableC = "aihc_next_transfer"

expectCompileArtifact :: Either CompileError T.Text -> IO T.Text
expectCompileArtifact result =
  case result of
    Left err -> assertFailure ("expected dependency-aware compile to succeed, got: " <> show err)
    Right core -> pure core

implicitDependencySource :: T.Text
implicitDependencySource =
  T.unlines
    [ "{-# LANGUAGE ExtendedLiterals #-}",
      "{-# LANGUAGE ForeignFunctionInterface #-}",
      "{-# LANGUAGE MagicHash #-}",
      "{-# LANGUAGE UnboxedTuples #-}",
      "module Main where",
      "data State# s",
      "data RealWorld",
      "data Int32 = I32# Int32#",
      "newtype CInt = CInt Int32",
      "newtype IO a = IO (State# RealWorld -> (# State# RealWorld, a #))",
      "foreign import ccall unsafe putchar :: CInt -> IO CInt",
      "char value = CInt (I32# value)",
      "main = id (putchar (char 72#Int32))"
    ]

noImplicitDependencySource :: T.Text
noImplicitDependencySource =
  T.replace
    "main = id (putchar (char 72#Int32))"
    "main = putchar (char 72#Int32)"
    ("{-# LANGUAGE NoImplicitPrelude #-}\n" <> implicitDependencySource)

createCompileLibrary :: FilePath -> FilePath -> FilePath -> String -> IO ()
createCompileLibrary coreRoot library moduleName source = do
  let sourcePath = coreRoot </> library </> "src" </> map dotToSlash moduleName <> ".hs"
  createDirectoryIfMissing True (takeDirectory sourcePath)
  writeFile (coreRoot </> library </> library <> ".cabal") ("name: " <> library <> "\n")
  writeFile sourcePath source
  where
    dotToSlash '.' = '/'
    dotToSlash char = char

compileCacheFiles :: FilePath -> IO [FilePath]
compileCacheFiles = compileCacheArtifacts ".cache"

compileCacheArtifacts :: String -> FilePath -> IO [FilePath]
compileCacheArtifacts extension root = do
  exists <- doesDirectoryExist root
  if not exists
    then pure []
    else do
      entries <- listDirectory root
      concat <$> mapM visit entries
  where
    visit entry = do
      let path = root </> entry
      isDirectory <- doesDirectoryExist path
      if isDirectory
        then compileCacheArtifacts extension path
        else pure [path | extension `isSuffixOf` path]

assertNativeOutput :: String -> FilePath -> Assertion
assertNativeOutput expected executable = do
  (exitCode, stdout, stderr) <- readProcessWithExitCode executable [] ""
  assertEqual ("native stderr: " <> stderr) ExitSuccess exitCode
  assertEqual "native stdout" expected stdout

helloWorldExamplePath :: IO FilePath
helloWorldExamplePath = examplePath ("hello-world" </> "Main.hs")

greenThreadsExamplePath :: IO FilePath
greenThreadsExamplePath = examplePath ("green-threads" </> "Main.hs")

asyncStdioExamplePath :: IO FilePath
asyncStdioExamplePath = examplePath ("async-stdio" </> "Main.hs")

unboxedTailRecursionExamplePath :: IO FilePath
unboxedTailRecursionExamplePath = examplePath ("unboxed-tail-recursion" </> "Main.hs")

examplePath :: FilePath -> IO FilePath
examplePath example = getCurrentDirectory >>= findFrom
  where
    relativePath = "examples" </> example
    findFrom directory = do
      let candidate = directory </> relativePath
      exists <- doesFileExist candidate
      if exists
        then pure candidate
        else do
          let parent = takeDirectory directory
          if parent == directory
            then assertFailure ("could not find " <> relativePath)
            else findFrom parent

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
  createVirtualCoreProviderPackage
    (root </> "core-libs" </> "system-cxx-std-lib")
    "system-cxx-std-lib"
    "1.0"

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

createVirtualCoreProviderPackage :: FilePath -> String -> String -> IO ()
createVirtualCoreProviderPackage sourceRoot name version = do
  createDirectoryIfMissing True sourceRoot
  writeFile
    (sourceRoot </> name <> ".cabal")
    ( unlines
        [ "cabal-version: 3.8",
          "name: " <> name,
          "version: " <> version,
          "build-type: Simple",
          "license: NONE",
          "",
          "library",
          "  default-language: GHC2021"
        ]
    )

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

libraryOnlyInstallCabal :: String
libraryOnlyInstallCabal =
  unlines
    [ "cabal-version: 3.0",
      "name: demo",
      "version: 0.1.0.0",
      "",
      "library demo-internal",
      "  exposed-modules: Demo.Internal",
      "  hs-source-dirs: internal",
      "  default-language: Haskell2010",
      "",
      "library",
      "  exposed-modules: Demo",
      "  hs-source-dirs: src",
      "  build-depends: demo-internal, dep",
      "  default-language: Haskell2010",
      "",
      "executable demo-cli",
      "  main-is: Main.hs",
      "  hs-source-dirs: app",
      "  build-depends: demo, executable-only-dependency",
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
