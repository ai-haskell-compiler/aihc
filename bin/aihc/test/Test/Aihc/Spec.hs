{-# LANGUAGE OverloadedStrings #-}

module Test.Aihc.Spec (tests) where

import Aihc.Cli.BuildExe (runBuildExe)
import Aihc.Cli.Install (InstallResult (..), install, parsePackageTarget)
import Aihc.Cli.Options (BuildExeOptions (..), GarbageCollector (GcSemispace), InstallOptions (..))
import Aihc.Cli.PackageManifest (PackageManifest (..), packageManifestPath, readPackageManifest, writePackageManifest)
import Aihc.Cli.Store (installedEntryArchivePath)
import Aihc.Cli.TypeArtifact (TypeArtifact (..), decodeTypeArtifact)
import Aihc.Fc qualified as Fc
import Aihc.Native (NativeTarget (..), hostNativeTarget, nativeTargetStoreDirectory)
import Aihc.Resolve (PackageId (..))
import Aihc.Tc (TcInterface (..), tcTermKeyIdentifier)
import Control.Concurrent (getNumCapabilities, setNumCapabilities)
import Control.Exception (IOException, bracket, finally, try)
import Control.Monad (forM)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, sort)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.Directory
  ( createDirectory,
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    findExecutable,
    getCurrentDirectory,
    getFileSize,
    getTemporaryDirectory,
    listDirectory,
    removeDirectoryRecursive,
    removeFile,
    withCurrentDirectory,
  )
import System.Environment (lookupEnv)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.IO (IOMode (WriteMode), hClose, hFlush, openTempFile, withFile)
import System.IO qualified as IO
import System.IO.Error (ioeGetErrorString)
import System.Info qualified as System
import System.Process (readProcess, readProcessWithExitCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "aihc"
    [ testGroup
        "build-exe"
        [testCase "builds imported source modules and runs the executable" test_buildExeSourceDirectories],
      testGroup
        "install"
        [ testCase "writes Core files and reuses an installed package" test_installResolveArtifacts,
          testCase "accepts type-check warnings" test_installTypeWarning,
          testCase "loads the implicit Prelude type interface" test_installImplicitPrelude,
          testCase "duplicates re-exported term signatures in type interfaces" test_installTypeReexports,
          testCase "limits instances to the transitive import graph" test_installInstanceVisibility,
          testCase "installs direct local dependencies" test_installLocalDependencies,
          testCase "prints timings independently from verbose output" test_installTimingOutput,
          testCase "reports all frontend errors in stable dependency order" test_installResolveError,
          testCase "writes Core for a ccall import" test_installFcCcall,
          testCase "retains and repairs GRIN only with keep-grin" test_installKeepGrin,
          testCase "writes target-specific objects and library archives" test_installTargetArchives,
          testCase "install writes core for aihc-prim and lints stored programs" test_installAihcPrim,
          testCase "parses Hackage package targets" test_parsePackageTarget
        ]
    ]

test_parsePackageTarget :: Assertion
test_parsePackageTarget = do
  assertEqual "bare name" (Just ("nats", Nothing)) (parsePackageTarget "nats")
  assertEqual "hyphenated name" (Just ("aihc-base", Nothing)) (parsePackageTarget "aihc-base")
  assertEqual "name and version" (Just ("nats", Just "1.1.2")) (parsePackageTarget "nats-1.1.2")
  assertEqual "hyphenated name and version" (Just ("aihc-base", Just "4.21.2.0")) (parsePackageTarget "aihc-base-4.21.2.0")
  assertEqual "path" Nothing (parsePackageTarget "core-libs/aihc-base")
  assertEqual "spaces" Nothing (parsePackageTarget "not a package")

test_buildExeSourceDirectories :: Assertion
test_buildExeSourceDirectories = do
  fixtureRoot <- findFixtureRoot "bin/aihc/test/Test/Fixtures/build-exe/source-directories"
  entryCollisionRoot <- findFixtureRoot "bin/aihc/test/Test/Fixtures/build-exe/generated-entry-collision"
  baseRoot <- findCoreLibraryRoot "aihc-base"
  primitiveRoot <- findCoreLibraryRoot "aihc-prim"
  let target = fromMaybe Llvm hostNativeTarget
  withTempDir "aihc-build-exe" $ \root -> do
    let storeRoot = root </> "store"
        output = root </> "program"
    primitive <- install (InstallOptions primitiveRoot (Just storeRoot) False False False False False False False target)
    installed <- install (InstallOptions baseRoot (Just storeRoot) False False False False False False False target)
    manifestResult <- readPackageManifest (packageManifestPath (installStorePath installed))
    manifest <- either assertFailure pure manifestResult
    assertBool "package manifest contains Prelude" ("Prelude" `elem` packageManifestModules manifest)
    let options =
          BuildExeOptions
            { buildExeSourceFile = fixtureRoot </> "Main.hs",
              buildExeSourceDirectories = [fixtureRoot],
              buildExePackageConstraints = ["aihc-base == 4.21.2.0"],
              buildExeTarget = target,
              buildExeGarbageCollector = GcSemispace,
              buildExeStoreRoot = Just storeRoot,
              buildExeBuildRoot = Nothing,
              buildExeLint = False,
              buildExeOutputFile = Just output
            }
        unusedResolve = installStorePath installed </> "Data" </> "Bool" </> "resolve.cbor"
        unusedType = installStorePath installed </> "Data" </> "Bool" </> "type.cbor"
        requiredFc = installStorePath primitive </> "GHC" </> "Prim" </> "Base" </> "core"
    resolveBytes <- BS.readFile unusedResolve
    BS.writeFile unusedResolve "invalid unused resolve interface"
    withCurrentDirectory root (runBuildExe options)
    assertFileExists (root </> ".aihc-cache" </> nativeTargetStoreDirectory target </> "Main" </> "Main.o")
    assertFileDoesNotExist (root </> ".aihc-cache" </> nativeTargetStoreDirectory target </> "GHC" </> "Base" </> "GHC.Base.o")
    let customBuildRoot = root </> "custom-build-root"
    withCurrentDirectory fixtureRoot (runBuildExe options {buildExeBuildRoot = Just customBuildRoot})
    assertFileExists (customBuildRoot </> nativeTargetStoreDirectory target </> "Main" </> "Main.o")
    BS.writeFile unusedResolve resolveBytes
    typeBytes <- BS.readFile unusedType
    BS.writeFile unusedType "invalid unused type interface"
    withCurrentDirectory root (runBuildExe options)
    BS.writeFile unusedType typeBytes
    fcBytes <- BS.readFile requiredFc
    BS.writeFile requiredFc "invalid required System FC"
    withCurrentDirectory root (runBuildExe options {buildExeLint = True})
    BS.writeFile requiredFc fcBytes
    writeCachedPackage storeRoot target "duplicate-1.0.0-a" "duplicate" "1.0.0" [] ["System.IO"]
    ambiguousModule <-
      try
        ( withCurrentDirectory root $
            runBuildExe
              options
                { buildExePackageConstraints = buildExePackageConstraints options <> ["duplicate == 1.0.0"]
                }
        ) ::
        IO (Either IOException ())
    case ambiguousModule of
      Left err -> assertBool "reports the ambiguous installed module" ("Ambiguous installed module: System.IO" `isInfixOf` ioeGetErrorString err)
      Right () -> assertFailure "expected the installed module import to be ambiguous"
    writeCachedPackage storeRoot target "duplicate-1.0.0-b" "duplicate" "1.0.0" [] []
    ambiguousPackage <-
      try
        ( withCurrentDirectory root $
            runBuildExe
              options
                { buildExePackageConstraints = buildExePackageConstraints options <> ["duplicate == 1.0.0"]
                }
        ) ::
        IO (Either IOException ())
    case ambiguousPackage of
      Left err -> assertBool "reports ambiguous package builds" ("More than one compiled build fulfills the constraint for duplicate" `isInfixOf` ioeGetErrorString err)
      Right () -> assertFailure "expected the compiled package build to be ambiguous"
    writeCachedPackage storeRoot target "shared-1.0.0-a" "shared" "1.0.0" [] []
    writeCachedPackage storeRoot target "shared-1.0.0-b" "shared" "1.0.0" [] []
    writeCachedPackage storeRoot target "root-a-1.0.0" "root-a" "1.0.0" ["shared-1.0.0-a"] []
    writeCachedPackage storeRoot target "root-b-1.0.0" "root-b" "1.0.0" ["shared-1.0.0-b"] []
    conflictingClosure <-
      try
        ( withCurrentDirectory root $
            runBuildExe
              options
                { buildExePackageConstraints = buildExePackageConstraints options <> ["root-a == 1.0.0", "root-b == 1.0.0"]
                }
        ) ::
        IO (Either IOException ())
    case conflictingClosure of
      Left err -> assertBool "reports conflicting dependency builds" ("The dependency plan selects more than one build of shared" `isInfixOf` ioeGetErrorString err)
      Right () -> assertFailure "expected the dependency builds to conflict"
    entryCollision <-
      try
        ( withCurrentDirectory root $
            runBuildExe
              options
                { buildExeSourceFile = entryCollisionRoot </> "Main.hs",
                  buildExeSourceDirectories = [entryCollisionRoot]
                }
        ) ::
        IO (Either IOException ())
    case entryCollision of
      Left err -> assertBool "reports the generated entry collision" ("Source module conflicts with generated module Aihc.Entry" `isInfixOf` ioeGetErrorString err)
      Right () -> assertFailure "expected the generated entry module to conflict"
    entryExists <- doesFileExist (installedEntryArchivePath storeRoot target)
    assertBool "target entry archive exists" entryExists
    (status, stdout, stderr) <- readProcessWithExitCode output [] ""
    assertEqual "executable exit status" ExitSuccess status
    assertEqual "executable stdout" "build-exe works\n" stdout
    assertEqual "executable stderr" "" stderr
    (rtsStatus, rtsStdout, rtsStderr) <-
      readProcessWithExitCode output ["first", "+RTS", "-M1G", "-RTS", "second"] ""
    assertEqual "RTS executable exit status" ExitSuccess rtsStatus
    assertEqual "RTS options are absent from program arguments" "first\nsecond\n" rtsStdout
    assertEqual "RTS executable stderr" "" rtsStderr
    (plainStatus, plainStdout, plainStderr) <-
      readProcessWithExitCode output ["-M1G", "second"] ""
    assertEqual "plain option executable exit status" ExitSuccess plainStatus
    assertEqual "plain option remains a program argument" "-M1G\nsecond\n" plainStdout
    assertEqual "plain option executable stderr" "" plainStderr
    (limitStatus, limitStdout, limitStderr) <-
      readProcessWithExitCode output ["+RTS", "-M1", "-RTS"] ""
    assertBool "heap limit terminates the executable" (limitStatus /= ExitSuccess)
    assertEqual "heap limit stdout" "" limitStdout
    assertEqual "heap limit diagnostic" "aihc runtime: heap limit exceeded\n" limitStderr
    (invalidStatus, invalidStdout, invalidStderr) <-
      readProcessWithExitCode output ["+RTS", "-M1X", "-RTS"] ""
    assertBool "invalid heap size terminates the executable" (invalidStatus /= ExitSuccess)
    assertEqual "invalid heap size stdout" "" invalidStdout
    assertEqual "invalid heap size diagnostic" "aihc runtime: invalid size for RTS option -M\n" invalidStderr

writeCachedPackage :: FilePath -> NativeTarget -> FilePath -> Text -> Text -> [Text] -> [Text] -> IO ()
writeCachedPackage storeRoot target identity name version dependencies modules = do
  let packageRoot = storeRoot </> nativeTargetStoreDirectory target </> identity
      archive = packageRoot </> "lib" </> "lib" <> T.unpack name <> ".a"
  createDirectoryIfMissing True (takeDirectory archive)
  writePackageManifest
    (packageManifestPath packageRoot)
    PackageManifest
      { packageManifestName = name,
        packageManifestVersion = version,
        packageManifestIdentity = T.pack identity,
        packageManifestDependencies = dependencies,
        packageManifestModules = modules
      }
  BS.writeFile archive ""

test_installResolveArtifacts :: Assertion
test_installResolveArtifacts =
  withTempDir "aihc-install" $ \root -> do
    let sourceRoot = root </> "source"
        storeRoot = root </> "store"
        sourceDir = sourceRoot </> "src" </> "Demo"
        options = InstallOptions sourceRoot (Just storeRoot) False False False False False False False AppleArm64
    createDirectoryIfMissing True sourceDir
    writeFile
      (sourceRoot </> "demo.cabal")
      ( unlines
          [ "cabal-version: 3.0",
            "name: demo",
            "version: 0.1.0.0",
            "library",
            "  exposed-modules: Demo.A, Demo.B",
            "  hs-source-dirs: src",
            "  default-language: Haskell2010"
          ]
      )
    writeFile (sourceDir </> "A.hs") "module Demo.A where\nimport Demo.B\na x = x\n"
    writeFile (sourceDir </> "B.hs") "module Demo.B where\nimport Demo.A\nb x = x\n"
    first <- install options
    assertEqual "written modules" ["Demo.A", "Demo.B"] (sort (installWrittenModules first))
    assertFileExists (installStorePath first </> "Demo" </> "A" </> "resolve.cbor")
    assertFileExists (installStorePath first </> "Demo" </> "B" </> "resolve.cbor")
    assertFileExists (installStorePath first </> "Demo" </> "A" </> "type.cbor")
    assertFileExists (installStorePath first </> "Demo" </> "B" </> "type.cbor")
    assertCoreFile (installStorePath first </> "Demo" </> "A" </> "core")
    assertCoreFile (installStorePath first </> "Demo" </> "B" </> "core")
    second <- install options
    assertEqual "reused modules" ["Demo.A", "Demo.B"] (sort (installReusedModules second))
    assertEqual "stable package directory" (installStorePath first) (installStorePath second)
    assertCoreFile (installStorePath second </> "Demo" </> "A" </> "core")
    assertCoreFile (installStorePath second </> "Demo" </> "B" </> "core")
    BS.writeFile (installStorePath second </> "Demo" </> "A" </> "resolve.cbor") "invalid resolve artifact"
    BS.writeFile (installStorePath second </> "Demo" </> "B" </> "type.cbor") "invalid type artifact"
    reinstalled <- install options {installReinstall = True}
    assertEqual "reinstall rebuilds all modules" ["Demo.A", "Demo.B"] (sort (installWrittenModules reinstalled))
    assertEqual "reinstall reuses no modules" [] (installReusedModules reinstalled)
    removeFile (installStorePath first </> "Demo" </> "A" </> "core")
    coreRepaired <- install options {installReinstall = True}
    assertEqual "repairs the complete SCC when core is absent" ["Demo.A", "Demo.B"] (sort (installWrittenModules coreRepaired))
    writeFile (sourceDir </> "B.hs") "module Demo.B where\nimport Demo.A\nb x = (x)\n"
    changed <- install options {installReinstall = True}
    assertEqual "source changes keep the package directory" (installStorePath first) (installStorePath changed)
    assertEqual "source changes rebuild the complete SCC" ["Demo.A", "Demo.B"] (sort (installWrittenModules changed))
    let artifact = installStorePath first </> "Demo" </> "A" </> "resolve.cbor"
    artifactBytes <- BS.readFile artifact
    BS.writeFile artifact (BS.init artifactBytes)
    repaired <- install options {installReinstall = True}
    assertEqual "repairs the complete corrupt SCC" ["Demo.A", "Demo.B"] (sort (installWrittenModules repaired))
    assertEqual "does not reuse a corrupt SCC" [] (installReusedModules repaired)

test_installTimingOutput :: Assertion
test_installTimingOutput = do
  fixtureRoot <- findFixtureRoot "bin/aihc/test/Test/Fixtures/install/keep-grin"
  withTempDir "aihc-install-timings" $ \root -> do
    let baseOptions = InstallOptions fixtureRoot Nothing False False False False True False False AppleArm64
    verboseOutput <-
      captureStdout
        (install baseOptions {installStoreRoot = Just (root </> "verbose"), installVerbose = True})
    timingOutput <-
      captureStdout
        (install baseOptions {installStoreRoot = Just (root </> "timings"), installPrintTimings = True})
    assertBool "verbose output contains an installation step" ("Read Cabal package:" `isInfixOf` verboseOutput)
    assertBool "verbose output does not contain timings" (not ("Compile time:" `isInfixOf` verboseOutput))
    assertBool
      "timing output contains the stage symbols"
      ("▁=parse ▂=resolve ▄=type-check █=backend .=idle" `isInfixOf` timingOutput)
    assertBool "timing output contains frontend time" ("Frontend time:" `isInfixOf` timingOutput)
    assertBool "parse total includes a span" (hasStageSpan "▁ total:" timingOutput)
    assertBool "resolve total includes a span" (hasStageSpan "▂ total:" timingOutput)
    assertBool "type-check total includes a span" (hasStageSpan "▄ total:" timingOutput)
    assertBool "backend total includes a span" (hasStageSpan "█ total:" timingOutput)
    assertBool "timing output contains desugar total" ("desugar total:" `isInfixOf` timingOutput)
    assertBool "timing output contains grin total" ("grin total:" `isInfixOf` timingOutput)
    assertBool "timing output contains native total" ("native total:" `isInfixOf` timingOutput)
    assertBool "timing output contains other total" ("other total:" `isInfixOf` timingOutput)
    assertBool "timing output does not contain verbose output" (not ("Read Cabal package:" `isInfixOf` timingOutput))
    assertBool "redirected timing output does not contain colors" ('\ESC' `notElem` timingOutput)

hasStageSpan :: String -> String -> Bool
hasStageSpan label output =
  any (\line -> label `isInfixOf` line && ", spanning " `isInfixOf` line) (lines output)

captureStdout :: IO value -> IO String
captureStdout action =
  withTempDir "aihc-capture-stdout" $ \root -> do
    let outputPath = root </> "stdout"
    bracket (hDuplicate IO.stdout) hClose $ \savedStdout -> do
      withFile outputPath WriteMode $ \outputHandle -> do
        hFlush IO.stdout
        hDuplicateTo outputHandle IO.stdout
        _ <-
          action
            `finally` do
              hFlush IO.stdout
              hDuplicateTo savedStdout IO.stdout
        pure ()
      T.unpack <$> TIO.readFile outputPath

test_installResolveError :: Assertion
test_installResolveError = do
  fixtureRoot <- findFixtureRoot "bin/aihc/test/Test/Fixtures/install/resolve-error"
  expected <- readFile (fixtureRoot </> "expected.txt")
  withTempDir "aihc-install-resolve-error" $ \root -> do
    actual <-
      bracket getNumCapabilities setNumCapabilities $ \_ ->
        forM [1, 2, 4] $ \workers -> do
          setNumCapabilities workers
          let options = InstallOptions fixtureRoot (Just (root </> "store-" <> show workers)) False False False False False False False AppleArm64
          result <- try (install options) :: IO (Either IOException InstallResult)
          case result of
            Right _ -> assertFailure "expected frontend compilation to fail"
            Left err -> do
              storeEntries <- listDirectory (root </> "store-" <> show workers </> nativeTargetStoreDirectory AppleArm64)
              assertBool "failed install leaves no temporary entry" (not (any (".tmp-" `isPrefixOf`) storeEntries))
              assertBool "failed install leaves no package entry" (not (any ("demo-" `isPrefixOf`) storeEntries))
              pure (T.unpack (T.replace (T.pack fixtureRoot) "<PACKAGE>" (T.pack (ioeGetErrorString err))))
    mapM_ (assertEqual "formatted frontend errors" expected) actual

findFixtureRoot :: FilePath -> IO FilePath
findFixtureRoot fixture = do
  configuredRoot <- lookupEnv "AIHC_TEST_ROOT"
  case configuredRoot of
    Just root -> validate (root </> fixture)
    Nothing -> getCurrentDirectory >>= findUp
  where
    validate candidate = do
      exists <- doesDirectoryExist candidate
      if exists
        then pure candidate
        else assertFailure ("could not find fixture " <> candidate)
    findUp directory = do
      let candidate = directory </> fixture
      exists <- doesDirectoryExist candidate
      if exists
        then pure candidate
        else do
          let parent = takeDirectory directory
          if parent == directory
            then assertFailure ("could not find fixture " <> fixture)
            else findUp parent

test_installKeepGrin :: Assertion
test_installKeepGrin = do
  fixtureRoot <- findFixtureRoot "bin/aihc/test/Test/Fixtures/install/keep-grin"
  withTempDir "aihc-install-keep-grin" $ \root -> do
    withoutGrin <- install (InstallOptions fixtureRoot (Just (root </> "without")) False False False False False False False AppleArm64)
    assertFileDoesNotExist (installStorePath withoutGrin </> "Demo" </> "grin")
    assertFileDoesNotExist (installStorePath withoutGrin </> "Demo" </> "cps.grin")
    assertFileDoesNotExist (installStorePath withoutGrin </> "Demo" </> "gc.grin")
    assertFileDoesNotExist (installStorePath withoutGrin </> "Demo" </> "Demo.o.objdump")
    retained <- install (InstallOptions fixtureRoot (Just (root </> "with")) True False False False False False False AppleArm64)
    let corePath = installStorePath retained </> "Demo" </> "core"
        grinPath = installStorePath retained </> "Demo" </> "grin"
        cpsGrinPath = installStorePath retained </> "Demo" </> "cps.grin"
        gcGrinPath = installStorePath retained </> "Demo" </> "gc.grin"
    assertFileExists grinPath
    assertFileExists cpsGrinPath
    assertFileExists gcGrinPath
    originalCore <- readFile corePath
    removeFile cpsGrinPath
    removeFile gcGrinPath
    repaired <- install (InstallOptions fixtureRoot (Just (root </> "with")) True False False True False False False AppleArm64)
    assertFileExists grinPath
    assertFileExists cpsGrinPath
    assertFileExists gcGrinPath
    repairedCore <- readFile corePath
    assertEqual "GRIN repair keeps Core" originalCore repairedCore
    assertEqual "GRIN repair writes the module" ["Demo"] (installWrittenModules repaired)
    noCode <-
      install
        (InstallOptions fixtureRoot (Just (root </> "no-code")) True True True False True False False AppleArm64)
    let noCodeRoot = installStorePath noCode
    assertFileExists (noCodeRoot </> "Demo" </> "resolve.cbor")
    assertFileExists (noCodeRoot </> "Demo" </> "type.cbor")
    assertFileDoesNotExist (noCodeRoot </> "Demo" </> "core")
    assertFileDoesNotExist (noCodeRoot </> "Demo" </> "grin")
    assertFileDoesNotExist (noCodeRoot </> "Demo" </> "cps.grin")
    assertFileDoesNotExist (noCodeRoot </> "Demo" </> "gc.grin")
    assertFileDoesNotExist (noCodeRoot </> "Demo" </> "Demo.o")
    assertFileDoesNotExist (noCodeRoot </> "Demo" </> "Demo.o.objdump")
    assertFileDoesNotExist (noCodeRoot </> "lib" </> "libdemo.a")

test_installTargetArchives :: Assertion
test_installTargetArchives = do
  fixtureRoot <- findFixtureRoot "bin/aihc/test/Test/Fixtures/install/keep-grin"
  wasmSupported <- clangSupportsWasm
  foreignArchivesSupported <- arSupportsForeignObjects
  withTempDir "aihc-install-targets" $ \root -> do
    let targets =
          [ (AppleArm64, "arm64-macos-apple", ".objdump"),
            (AppleArm64Lir, "arm64-macos-apple-lir", ".lir"),
            (Llvm, "llvm", ".ll")
          ]
            <> [(LinuxAmd64, "amd64-linux-gnu", ".objdump") | foreignArchivesSupported]
            <> [(Wasm32Wasip3, "wasm32-wasip3", ".s") | wasmSupported && foreignArchivesSupported]
    results <- forM targets $ \(target, directory, nativeExtension) -> do
      result <- install (InstallOptions fixtureRoot (Just (root </> "store")) False True False False False False False target)
      let objectPath = installStorePath result </> "Demo" </> "Demo.o"
          nativePath = objectPath <> nativeExtension
          corePath = installStorePath result </> "Demo" </> "core"
          archivePath = installStorePath result </> "lib" </> "libdemo.a"
      assertEqual "target store directory" directory (takeFileName (takeDirectory (installStorePath result)))
      assertFileExists objectPath
      assertFileExists nativePath
      assertFileExists archivePath
      objectHeader <- BS.take 4 <$> BS.readFile objectPath
      case target of
        AppleArm64 -> do
          assertEqual "Mach-O object header" (BS.pack [0xcf, 0xfa, 0xed, 0xfe]) objectHeader
          assertFileDoesNotExist (objectPath <> ".s")
        AppleArm64Lir -> do
          assertEqual "Mach-O object header" (BS.pack [0xcf, 0xfa, 0xed, 0xfe]) objectHeader
          assertFileDoesNotExist (objectPath <> ".s")
        LinuxAmd64 -> do
          assertEqual "ELF object header" (BS.pack [0x7f, 0x45, 0x4c, 0x46]) objectHeader
          assertFileDoesNotExist (objectPath <> ".s")
        _ -> pure ()
      members <- filter (not . ("__.SYMDEF" `isPrefixOf`)) . lines <$> readProcess "ar" ["-t", archivePath] ""
      assertEqual ("archive members for " <> show target) ["Demo.o"] members
      originalCore <- readFile corePath
      removeFile nativePath
      repaired <- install (InstallOptions fixtureRoot (Just (root </> "store")) False True False True False False False target)
      assertFileExists nativePath
      repairedCore <- readFile corePath
      assertEqual "native output repair keeps Core" originalCore repairedCore
      assertEqual "native output repair writes the module" ["Demo"] (installWrittenModules repaired)
      pure result
    case results of
      [] -> assertFailure "no target results"
      first : rest ->
        assertBool
          "package identity is equal for all targets"
          (all ((== takeFileName (installStorePath first)) . takeFileName . installStorePath) rest)

clangSupportsWasm :: IO Bool
clangSupportsWasm = do
  result <- try (readProcess "clang" ["-print-targets"] "") :: IO (Either IOException String)
  pure $ case result of
    Left _ -> False
    Right targets -> any isWasmTarget (lines targets)
  where
    isWasmTarget line =
      case words line of
        target : _ -> target == "wasm32"
        [] -> False

arSupportsForeignObjects :: IO Bool
arSupportsForeignObjects = do
  archiveTool <- findExecutable "ar"
  pure (System.os /= "darwin" || archiveTool /= Just "/usr/bin/ar")

assertCoreFile :: FilePath -> Assertion
assertCoreFile path = do
  assertFileExists path
  core <- TIO.readFile path
  case Fc.parseProgram core of
    Left parseError -> assertFailure ("invalid Core file " <> path <> ": " <> Fc.renderParseError parseError)
    Right _ -> pure ()

test_installFcCcall :: Assertion
test_installFcCcall =
  withTempDir "aihc-install-fc-ccall" $ \root -> do
    let sourceRoot = root </> "source"
        storeRoot = root </> "store"
        sourceDir = sourceRoot </> "src"
        options = InstallOptions sourceRoot (Just storeRoot) False False False False False False False AppleArm64
    createDirectoryIfMissing True sourceDir
    writeFile
      (sourceRoot </> "demo.cabal")
      ( unlines
          [ "cabal-version: 3.0",
            "name: demo",
            "version: 0.1.0.0",
            "library",
            "  exposed-modules: Demo",
            "  hs-source-dirs: src",
            "  default-language: Haskell2010",
            "  default-extensions: ForeignFunctionInterface, MagicHash"
          ]
      )
    writeFile
      (sourceDir </> "Demo.hs")
      "module Demo where\nimport GHC.Prim (Int#)\ndata Int = I# Int#\nforeign import ccall unsafe \"foo\" foo :: Int -> Int\n"
    result <- install options
    assertCoreFile (installStorePath result </> "Demo" </> "core")

test_installAihcPrim :: Assertion
test_installAihcPrim = do
  aihcPrimRoot <- findAihcPrimRoot
  withTempDir "aihc-install-aihc-prim" $ \root -> do
    let storeRoot = root </> "store"
        targetStoreRoot = storeRoot </> nativeTargetStoreDirectory AppleArm64
        options = InstallOptions aihcPrimRoot (Just storeRoot) True False True False False False False AppleArm64
    createDirectoryIfMissing True storeRoot
    caught <- try (install options) :: IO (Either IOException InstallResult)
    result <- case caught of
      Left err -> assertFailure ("install aihc-prim failed: " <> show err)
      Right value -> pure value
    let packageDir = installStorePath result
        packageId = PackageId (T.pack (takeFileName packageDir))
        loader = Fc.storeModuleLoader targetStoreRoot
    assertBool "package artifact version sets the package hash" ("ff25baf152cf478e" `isSuffixOf` packageDir)
    mapM_ (assertTypeArtifactSize packageDir) ["GHC.Tuple", "GHC.Types"]
    mapM_ (assertModuleCore packageDir) aihcPrimLibraryModules
    coreFiles <- listNamedFiles packageDir "core"
    mapM_ assertCoreFile coreFiles
    grinFiles <- listNamedFiles packageDir "grin"
    assertEqual "one GRIN file for each Core file" (length coreFiles) (length grinFiles)
    types <- loadStoredFc loader packageId "GHC.Types"
    prim <- loadStoredFc loader packageId "GHC.Prim"
    assertEqual "GHC.Types lint errors" [] (Fc.lintProgram types)
    assertEqual "GHC.Prim lint errors" [] (Fc.lintProgram prim)
    mapM_ (assertModuleClosureLints loader packageId) (filter (`notElem` ["GHC.Types", "GHC.Prim"]) aihcPrimLibraryModules)

assertTypeArtifactSize :: FilePath -> Text -> Assertion
assertTypeArtifactSize packageDir name = do
  let path = foldl (</>) packageDir (map T.unpack (T.splitOn "." name) ++ ["type.cbor"])
  size <- getFileSize path
  assertBool ("type artifact is less than 1 MiB: " <> path) (size < 1024 * 1024)

aihcPrimLibraryModules :: [Text]
aihcPrimLibraryModules =
  [ "GHC.CString",
    "GHC.Classes",
    "GHC.Debug",
    "GHC.Magic",
    "GHC.Magic.Dict",
    "GHC.Prim",
    "GHC.Prim.Exception",
    "GHC.Prim.Ext",
    "GHC.Prim.Panic",
    "GHC.Prim.PtrEq",
    "GHC.Prim.Unicode",
    "GHC.PrimopWrappers",
    "GHC.Tuple",
    "GHC.Types"
  ]

findAihcPrimRoot :: IO FilePath
findAihcPrimRoot = do
  envRoot <- lookupEnv "AIHC_PRIM_SRC"
  case envRoot of
    Just root -> do
      cabalExists <- doesFileExist (root </> "aihc-prim.cabal")
      if cabalExists
        then pure root
        else assertFailure ("AIHC_PRIM_SRC has no aihc-prim.cabal: " <> root)
    Nothing -> do
      cwd <- getCurrentDirectory
      findUp cwd
  where
    findUp dir = do
      let candidate = dir </> "core-libs" </> "aihc-prim"
      cabalExists <- doesFileExist (candidate </> "aihc-prim.cabal")
      if cabalExists
        then pure candidate
        else do
          let parent = takeDirectory dir
          if parent == dir
            then assertFailure ("could not find core-libs/aihc-prim from " <> dir)
            else findUp parent

findCoreLibraryRoot :: FilePath -> IO FilePath
findCoreLibraryRoot name = do
  configured <- lookupEnv (coreLibraryEnvironment name)
  case configured of
    Just root -> validate root
    Nothing -> getCurrentDirectory >>= findUp
  where
    coreLibraryEnvironment library
      | library == "aihc-base" = "AIHC_BASE_SRC"
      | library == "aihc-prim" = "AIHC_PRIM_SRC"
      | otherwise = "AIHC_CORE_LIBRARY_SRC"
    validate root = do
      exists <- doesFileExist (root </> name <> ".cabal")
      if exists
        then pure root
        else assertFailure (coreLibraryEnvironment name <> " has no " <> name <> ".cabal: " <> root)
    findUp directory = do
      let candidate = directory </> "core-libs" </> name
          cabalFile = candidate </> name <> ".cabal"
      exists <- doesFileExist cabalFile
      if exists
        then pure candidate
        else do
          let parent = takeDirectory directory
          if parent == directory
            then assertFailure ("could not find core-libs/" <> name)
            else findUp parent

moduleCorePath :: FilePath -> Text -> FilePath
moduleCorePath packageDir moduleName =
  foldl (</>) packageDir (map T.unpack (T.splitOn "." moduleName) ++ ["core"])

assertModuleCore :: FilePath -> Text -> Assertion
assertModuleCore packageDir moduleName =
  assertFileExists (moduleCorePath packageDir moduleName)

loadStoredFc :: Fc.ModuleLoader -> PackageId -> Text -> IO Fc.Program
loadStoredFc loader packageId moduleName = do
  loaded <- loader packageId moduleName
  case loaded of
    Nothing -> assertFailure ("store loader did not find " <> T.unpack moduleName)
    Just program -> pure program

assertModuleClosureLints :: Fc.ModuleLoader -> PackageId -> Text -> Assertion
assertModuleClosureLints loader packageId moduleName = do
  program <- loadStoredFc loader packageId moduleName
  assertEqual
    (T.unpack moduleName <> " lint errors")
    []
    (Fc.lintProgram program)

listNamedFiles :: FilePath -> FilePath -> IO [FilePath]
listNamedFiles root name = do
  exists <- doesDirectoryExist root
  if not exists
    then pure []
    else do
      entries <- listDirectory root
      concat <$> mapM (go . (root </>)) entries
  where
    go path = do
      isDir <- doesDirectoryExist path
      if isDir
        then listNamedFiles path name
        else
          if takeFileName path == name
            then pure [path]
            else pure []

test_installTypeWarning :: Assertion
test_installTypeWarning = do
  fixtureRoot <- findFixtureRoot "bin/aihc/test/Test/Fixtures/install/type-warning"
  withTempDir "aihc-install-type-warning" $ \root -> do
    let options = InstallOptions fixtureRoot (Just (root </> "store")) False False False False True False False AppleArm64
    result <- install options
    assertEqual "warning does not prevent installation" ["Demo"] (installWrittenModules result)

test_installImplicitPrelude :: Assertion
test_installImplicitPrelude = do
  fixtureRoot <- findFixtureRoot "bin/aihc/test/Test/Fixtures/install/implicit-prelude"
  withTempDir "aihc-install-implicit-prelude" $ \root -> do
    let sourceRoot = fixtureRoot </> "demo"
        options = InstallOptions sourceRoot (Just (root </> "store")) False False False False True False False AppleArm64
    result <- install options
    assertEqual "implicit Prelude user" ["Demo"] (installWrittenModules result)

test_installTypeReexports :: Assertion
test_installTypeReexports =
  withTempDir "aihc-install-type-reexports" $ \root -> do
    let sourceRoot = root </> "source"
        sourceDir = sourceRoot </> "src" </> "Demo"
        options = InstallOptions sourceRoot (Just (root </> "store")) False False False False False False False AppleArm64
    createDirectoryIfMissing True sourceDir
    writeFile
      (sourceRoot </> "demo.cabal")
      ( unlines
          [ "cabal-version: 3.0",
            "name: demo",
            "version: 0.1.0.0",
            "library",
            "  exposed-modules: Demo.A, Demo.B",
            "  hs-source-dirs: src",
            "  default-language: Haskell2010"
          ]
      )
    writeFile
      (sourceDir </> "A.hs")
      "module Demo.A where\ndata Box a = Box a\nclass Identity a where\n  identity :: a -> a\nfn x = x\n"
    writeFile (sourceDir </> "B.hs") "module Demo.B (module Demo.A) where\nimport Demo.A\n"
    result <- install options
    bytes <- BL.readFile (installStorePath result </> "Demo" </> "B" </> "type.cbor")
    let artifact = decodeTypeArtifact bytes
    let termNames = mapMaybe (tcTermKeyIdentifier . fst) (tcInterfaceTerms (typeArtifactInterface artifact))
    assertBool "re-exported signature" ("fn" `elem` termNames)

test_installLocalDependencies :: Assertion
test_installLocalDependencies = do
  fixtureRoot <- findFixtureRoot "bin/aihc/test/Test/Fixtures/install/local-dependencies"
  withTempDir "aihc-install-local-dependencies" $ \root -> do
    let sourceRoot = fixtureRoot </> "demo"
        storeRoot = root </> "store"
        options = InstallOptions sourceRoot (Just storeRoot) False False False False False False False AppleArm64
    _ <- install options
    let targetStoreRoot = storeRoot </> nativeTargetStoreDirectory AppleArm64
    storeEntries <- listDirectory targetStoreRoot
    assertBool "temporary store directories are absent" (not (any (".tmp-" `isPrefixOf`) storeEntries))
    let dependencyStores = filter ("dep-1.0.0-" `isPrefixOf`) storeEntries
    case dependencyStores of
      [dependencyStore] -> do
        let dependencyStoreRoot = targetStoreRoot </> dependencyStore
            unusedTypePath = dependencyStoreRoot </> "Dep" </> "Unused" </> "type.cbor"
            sentinelPath = dependencyStoreRoot </> "reinstall-sentinel"
        assertFileExists (dependencyStoreRoot </> "Dep" </> "resolve.cbor")
        assertFileExists (dependencyStoreRoot </> "Dep" </> "type.cbor")
        assertFileExists unusedTypePath
        BS.writeFile unusedTypePath "invalid unused type artifact"
        writeFile sentinelPath "dependency was not reinstalled"
        reinstalled <- install options {installReinstall = True}
        assertEqual "reinstall writes the specified package" ["Demo"] (installWrittenModules reinstalled)
        assertFileExists sentinelPath
        unusedTypeBytes <- BS.readFile unusedTypePath
        assertEqual "reinstall does not read or replace the unused module" "invalid unused type artifact" unusedTypeBytes
      _ -> assertFailure ("expected one installed dependency, got " <> show dependencyStores)

test_installInstanceVisibility :: Assertion
test_installInstanceVisibility = do
  fixtureRoot <- findFixtureRoot "bin/aihc/test/Test/Fixtures/install/instance-visibility"
  withTempDir "aihc-install-instance-visibility" $ \root -> do
    let installFixture source store =
          install
            (InstallOptions (fixtureRoot </> source) (Just (root </> store)) False False False False True False False AppleArm64)
    withoutResult <- try (installFixture "without" "without-store") :: IO (Either IOException InstallResult)
    case withoutResult of
      Left _ -> pure ()
      Right _ -> assertFailure "an unrelated module supplied an instance"
    _ <- installFixture "with" "with-store"
    pure ()

assertFileExists :: FilePath -> Assertion
assertFileExists path = do
  exists <- doesFileExist path
  assertBool ("expected file to exist: " <> path) exists

assertFileDoesNotExist :: FilePath -> Assertion
assertFileDoesNotExist path = do
  exists <- doesFileExist path
  assertBool ("expected file not to exist: " <> path) (not exists)

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
