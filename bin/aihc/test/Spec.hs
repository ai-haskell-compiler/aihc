{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Aihc.Cli.BuildExe (runBuildExe)
import Aihc.Cli.InstallV2 (InstallV2Result (..), installV2)
import Aihc.Cli.Options (BuildExeOptions (..), GarbageCollector (GcCalloc), InstallV2Options (..))
import Aihc.Cli.TypeArtifact (TypeArtifact (..), decodeTypeArtifact)
import Aihc.Fc2 qualified as Fc2
import Aihc.Native (NativeTarget (..), hostNativeTarget, nativeTargetStoreDirectory)
import Aihc.Resolve (PackageId (..))
import Aihc.Tc (TcInterface (..), tcTermKeyIdentifier)
import Control.Exception (IOException, bracket, try)
import Control.Monad (forM)
import Data.ByteString qualified as BS
import Data.List (isPrefixOf, sort)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Hedgehog (Property, property, success)
import System.Directory
  ( createDirectory,
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    findExecutable,
    getCurrentDirectory,
    getTemporaryDirectory,
    listDirectory,
    removeDirectoryRecursive,
    removeFile,
  )
import System.Environment (lookupEnv)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.IO (hClose, openTempFile)
import System.IO.Error (ioeGetErrorString)
import System.Info qualified as System
import System.Process (readProcess, readProcessWithExitCode)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.Hedgehog (testProperty)

main :: IO ()
main =
  defaultMain . testGroup "aihc" $
    [ testGroup
        "build-exe"
        [testCase "builds imported source modules and runs the executable" test_buildExeSourceDirectories],
      testGroup
        "install-v2"
        [ testCase "writes no legacy Core files and reuses equal SCC inputs" test_installV2ResolveArtifacts,
          testCase "rebuilds a module when a predecessor resolve artifact changes" test_installV2ResolveDependencies,
          testCase "rebuilds a module when a predecessor type interface changes" test_installV2TypeDependencies,
          testCase "duplicates re-exported term signatures in type interfaces" test_installV2TypeReexports,
          testCase "installs direct local dependencies" test_installV2LocalDependencies,
          testCase "rebuilds stale type artifact schemas" test_installV2StaleTypeArtifact,
          testCase "stops invalidation when a rebuilt scope stays equal" test_installV2StopsAtEqualScope,
          testCase "reports resolve errors with source locations" test_installV2ResolveError,
          testCase "writes Core-v2 for a ccall import" test_installV2Fc2Ccall,
          testCase "retains and repairs GRIN only with keep-grin" test_installV2KeepGrin,
          testCase "writes target-specific objects and library archives" test_installV2TargetArchives,
          testCase "install-v2 writes core-v2 for aihc-prim and lints stored programs" test_installV2AihcPrim
        ],
      testProperty "Hedgehog options" prop_dummy
    ]

prop_dummy :: Property
prop_dummy = property success

test_buildExeSourceDirectories :: Assertion
test_buildExeSourceDirectories = do
  fixtureRoot <- findFixtureRoot "bin/aihc/test/Test/Fixtures/build-exe/source-directories"
  baseRoot <- findCoreLibraryRoot "aihc-base"
  target <- maybe (assertFailure "the host has no native aihc target") pure hostNativeTarget
  withTempDir "aihc-build-exe" $ \root -> do
    let storeRoot = root </> "store"
        output = root </> "program"
    _ <- installV2 (InstallV2Options baseRoot (Just storeRoot) False False False target)
    runBuildExe
      BuildExeOptions
        { buildExeSourceFile = fixtureRoot </> "Main.hs",
          buildExeSourceDirectories = [fixtureRoot],
          buildExePackageConstraints = ["aihc-base == 4.21.2.0"],
          buildExeTarget = target,
          buildExeGarbageCollector = GcCalloc,
          buildExeStoreRoot = Just storeRoot,
          buildExeOutputFile = Just output
        }
    (status, stdout, stderr) <- readProcessWithExitCode output [] ""
    assertEqual "executable exit status" ExitSuccess status
    assertEqual "executable stdout" "build-exe works\n" stdout
    assertEqual "executable stderr" "" stderr

test_installV2ResolveArtifacts :: Assertion
test_installV2ResolveArtifacts =
  withTempDir "aihc-install-v2" $ \root -> do
    let sourceRoot = root </> "source"
        storeRoot = root </> "store"
        sourceDir = sourceRoot </> "src" </> "Demo"
        options = InstallV2Options sourceRoot (Just storeRoot) False False False AppleArm64
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
    first <- installV2 options
    assertEqual "written modules" ["Demo.A", "Demo.B"] (sort (installV2WrittenModules first))
    assertFileExists (installV2StorePath first </> "Demo" </> "A" </> "resolve.cbor")
    assertFileExists (installV2StorePath first </> "Demo" </> "B" </> "resolve.cbor")
    assertFileExists (installV2StorePath first </> "Demo" </> "A" </> "type.cbor")
    assertFileExists (installV2StorePath first </> "Demo" </> "B" </> "type.cbor")
    assertFileDoesNotExist (installV2StorePath first </> "Demo" </> "A" </> "core")
    assertFileDoesNotExist (installV2StorePath first </> "Demo" </> "B" </> "core")
    assertCoreV2File (installV2StorePath first </> "Demo" </> "A" </> "core-v2")
    assertCoreV2File (installV2StorePath first </> "Demo" </> "B" </> "core-v2")
    second <- installV2 options
    assertEqual "reused modules" ["Demo.A", "Demo.B"] (sort (installV2ReusedModules second))
    assertEqual "stable package directory" (installV2StorePath first) (installV2StorePath second)
    assertCoreV2File (installV2StorePath second </> "Demo" </> "A" </> "core-v2")
    assertCoreV2File (installV2StorePath second </> "Demo" </> "B" </> "core-v2")
    removeFile (installV2StorePath first </> "Demo" </> "A" </> "core-v2")
    coreV2Repaired <- installV2 options
    assertEqual "repairs the complete SCC when core-v2 is absent" ["Demo.A", "Demo.B"] (sort (installV2WrittenModules coreV2Repaired))
    writeFile (sourceDir </> "B.hs") "module Demo.B where\nimport Demo.A\nb x = (x)\n"
    changed <- installV2 options
    assertEqual "source changes keep the package directory" (installV2StorePath first) (installV2StorePath changed)
    assertEqual "source changes rebuild the complete SCC" ["Demo.A", "Demo.B"] (sort (installV2WrittenModules changed))
    let artifact = installV2StorePath first </> "Demo" </> "A" </> "resolve.cbor"
    artifactBytes <- BS.readFile artifact
    BS.writeFile artifact (BS.init artifactBytes)
    repaired <- installV2 options
    assertEqual "repairs the complete corrupt SCC" ["Demo.A", "Demo.B"] (sort (installV2WrittenModules repaired))
    assertEqual "does not reuse a corrupt SCC" [] (installV2ReusedModules repaired)

test_installV2ResolveError :: Assertion
test_installV2ResolveError = do
  fixtureRoot <- findFixtureRoot "bin/aihc/test/Test/Fixtures/install-v2/resolve-error"
  expected <- readFile (fixtureRoot </> "expected.txt")
  withTempDir "aihc-install-v2-resolve-error" $ \root -> do
    let options = InstallV2Options fixtureRoot (Just (root </> "store")) False False False AppleArm64
    result <- try (installV2 options) :: IO (Either IOException InstallV2Result)
    case result of
      Right _ -> assertFailure "expected name resolution to fail"
      Left err ->
        assertEqual
          "formatted name resolution error"
          expected
          (T.unpack (T.replace (T.pack fixtureRoot) "<PACKAGE>" (T.pack (ioeGetErrorString err))))

findFixtureRoot :: FilePath -> IO FilePath
findFixtureRoot fixture = do
  cwd <- getCurrentDirectory
  findUp cwd
  where
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

test_installV2KeepGrin :: Assertion
test_installV2KeepGrin = do
  fixtureRoot <- findFixtureRoot "bin/aihc/test/Test/Fixtures/install-v2/keep-grin"
  withTempDir "aihc-install-v2-keep-grin" $ \root -> do
    withoutGrin <- installV2 (InstallV2Options fixtureRoot (Just (root </> "without")) False False False AppleArm64)
    assertFileDoesNotExist (installV2StorePath withoutGrin </> "Demo" </> "grin")
    assertFileDoesNotExist (installV2StorePath withoutGrin </> "Demo" </> "Demo.o.s")
    retained <- installV2 (InstallV2Options fixtureRoot (Just (root </> "with")) True False False AppleArm64)
    let corePath = installV2StorePath retained </> "Demo" </> "core-v2"
        grinPath = installV2StorePath retained </> "Demo" </> "grin"
    assertFileExists grinPath
    originalCore <- readFile corePath
    removeFile grinPath
    repaired <- installV2 (InstallV2Options fixtureRoot (Just (root </> "with")) True False False AppleArm64)
    assertFileExists grinPath
    repairedCore <- readFile corePath
    assertEqual "GRIN repair keeps Core-v2" originalCore repairedCore
    assertEqual "GRIN repair writes the module" ["Demo"] (installV2WrittenModules repaired)

test_installV2TargetArchives :: Assertion
test_installV2TargetArchives = do
  fixtureRoot <- findFixtureRoot "bin/aihc/test/Test/Fixtures/install-v2/keep-grin"
  wasmSupported <- clangSupportsWasm
  foreignArchivesSupported <- arSupportsForeignObjects
  withTempDir "aihc-install-v2-targets" $ \root -> do
    let targets =
          [ (AppleArm64, "arm64-macos-apple", ".s"),
            (Llvm, "llvm", ".ll")
          ]
            <> [(LinuxAmd64, "amd64-linux-gnu", ".s") | foreignArchivesSupported]
            <> [(Wasm32Wasip3, "wasm32-wasip3", ".s") | wasmSupported && foreignArchivesSupported]
    results <- forM targets $ \(target, directory, nativeExtension) -> do
      result <- installV2 (InstallV2Options fixtureRoot (Just (root </> "store")) False True False target)
      let objectPath = installV2StorePath result </> "Demo" </> "Demo.o"
          nativePath = objectPath <> nativeExtension
          corePath = installV2StorePath result </> "Demo" </> "core-v2"
          archivePath = installV2StorePath result </> "lib" </> "libdemo.a"
      assertEqual "target store directory" directory (takeFileName (takeDirectory (installV2StorePath result)))
      assertFileExists objectPath
      assertFileExists nativePath
      assertFileExists archivePath
      members <- filter (not . ("__.SYMDEF" `isPrefixOf`)) . lines <$> readProcess "ar" ["-t", archivePath] ""
      assertEqual ("archive members for " <> show target) ["Demo.o"] members
      originalCore <- readFile corePath
      removeFile nativePath
      repaired <- installV2 (InstallV2Options fixtureRoot (Just (root </> "store")) False True False target)
      assertFileExists nativePath
      repairedCore <- readFile corePath
      assertEqual "native source repair keeps Core-v2" originalCore repairedCore
      assertEqual "native source repair writes the module" ["Demo"] (installV2WrittenModules repaired)
      pure result
    case results of
      [] -> assertFailure "no target results"
      first : rest ->
        assertBool
          "package identity is equal for all targets"
          (all ((== takeFileName (installV2StorePath first)) . takeFileName . installV2StorePath) rest)

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

assertCoreV2File :: FilePath -> Assertion
assertCoreV2File path = do
  assertFileExists path
  core <- TIO.readFile path
  case Fc2.parseProgram core of
    Left parseError -> assertFailure ("invalid Core-v2 file " <> path <> ": " <> Fc2.renderParseError parseError)
    Right _ -> pure ()

test_installV2Fc2Ccall :: Assertion
test_installV2Fc2Ccall =
  withTempDir "aihc-install-v2-fc2-ccall" $ \root -> do
    let sourceRoot = root </> "source"
        storeRoot = root </> "store"
        sourceDir = sourceRoot </> "src"
        options = InstallV2Options sourceRoot (Just storeRoot) False False False AppleArm64
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
    result <- installV2 options
    assertCoreV2File (installV2StorePath result </> "Demo" </> "core-v2")

test_installV2AihcPrim :: Assertion
test_installV2AihcPrim = do
  aihcPrimRoot <- findAihcPrimRoot
  withTempDir "aihc-install-v2-aihc-prim" $ \root -> do
    let storeRoot = root </> "store"
        targetStoreRoot = storeRoot </> nativeTargetStoreDirectory AppleArm64
        options = InstallV2Options aihcPrimRoot (Just storeRoot) True False False AppleArm64
    createDirectoryIfMissing True storeRoot
    caught <- try (installV2 options) :: IO (Either IOException InstallV2Result)
    result <- case caught of
      Left err -> do
        badFiles <- listNamedFiles storeRoot "core-v2.bad"
        assertFailure
          ( "install-v2 aihc-prim failed: "
              <> show err
              <> if null badFiles
                then ""
                else "\nbad core-v2 files:\n" <> unlines badFiles
          )
      Right value -> pure value
    let packageDir = installV2StorePath result
        packageId = PackageId (T.pack (takeFileName packageDir))
        loader = Fc2.storeModuleLoader targetStoreRoot
    mapM_ (assertModuleCoreV2 packageDir) aihcPrimLibraryModules
    coreV2Files <- listNamedFiles packageDir "core-v2"
    mapM_ assertCoreV2File coreV2Files
    grinFiles <- listNamedFiles packageDir "grin"
    assertEqual "one GRIN file for each Core-v2 file" (length coreV2Files) (length grinFiles)
    types <- loadStoredFc2 loader packageId "GHC.Types"
    prim <- loadStoredFc2 loader packageId "GHC.Prim"
    assertBool "GHC.Types lint needs GHC.Prim" (not (null (Fc2.lintPrograms [types])))
    assertBool "GHC.Prim lint needs GHC.Types" (not (null (Fc2.lintPrograms [prim])))
    typesAndPrim <- Fc2.loadScopeClosure loader [types, prim]
    assertEqual "GHC.Types and GHC.Prim closure lint errors" [] (Fc2.lintPrograms typesAndPrim)
    mapM_ (assertModuleClosureLints loader packageId) (filter (`notElem` ["GHC.Types", "GHC.Prim"]) aihcPrimLibraryModules)

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

moduleCoreV2Path :: FilePath -> Text -> FilePath
moduleCoreV2Path packageDir moduleName =
  foldl (</>) packageDir (map T.unpack (T.splitOn "." moduleName) ++ ["core-v2"])

assertModuleCoreV2 :: FilePath -> Text -> Assertion
assertModuleCoreV2 packageDir moduleName =
  assertFileExists (moduleCoreV2Path packageDir moduleName)

loadStoredFc2 :: Fc2.ModuleLoader -> PackageId -> Text -> IO Fc2.Program
loadStoredFc2 loader packageId moduleName = do
  loaded <- loader packageId moduleName
  case loaded of
    Nothing -> assertFailure ("store loader did not find " <> T.unpack moduleName)
    Just program -> pure program

assertModuleClosureLints :: Fc2.ModuleLoader -> PackageId -> Text -> Assertion
assertModuleClosureLints loader packageId moduleName = do
  program <- loadStoredFc2 loader packageId moduleName
  loaded <- Fc2.loadScopeClosure loader [program]
  assertEqual
    (T.unpack moduleName <> " closure lint errors")
    []
    (Fc2.lintPrograms loaded)

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

test_installV2TypeDependencies :: Assertion
test_installV2TypeDependencies =
  withTempDir "aihc-install-v2-type-dependencies" $ \root -> do
    let sourceRoot = root </> "source"
        sourceDir = sourceRoot </> "src" </> "Demo"
        options = InstallV2Options sourceRoot (Just (root </> "store")) False False False AppleArm64
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
    writeFile (sourceDir </> "A.hs") "module Demo.A where\nimport Demo.B\na = b\n"
    writeFile (sourceDir </> "B.hs") "module Demo.B where\nb x = x\n"
    _ <- installV2 options
    writeFile (sourceDir </> "B.hs") "module Demo.B where\nb x y = x\n"
    changed <- installV2 options
    assertEqual "type change and direct dependent" ["Demo.A", "Demo.B"] (sort (installV2WrittenModules changed))
    assertEqual "no reused dependent after type change" [] (installV2ReusedModules changed)

test_installV2TypeReexports :: Assertion
test_installV2TypeReexports =
  withTempDir "aihc-install-v2-type-reexports" $ \root -> do
    let sourceRoot = root </> "source"
        sourceDir = sourceRoot </> "src" </> "Demo"
        options = InstallV2Options sourceRoot (Just (root </> "store")) False False False AppleArm64
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
    result <- installV2 options
    bytes <- BS.readFile (installV2StorePath result </> "Demo" </> "B" </> "type.cbor")
    artifact <- either assertFailure pure (decodeTypeArtifact bytes)
    let termNames = mapMaybe (tcTermKeyIdentifier . fst) (tcInterfaceTerms (typeArtifactInterface artifact))
    assertBool "re-exported signature" ("fn" `elem` termNames)

test_installV2LocalDependencies :: Assertion
test_installV2LocalDependencies =
  withTempDir "aihc-install-v2-local-dependencies" $ \root -> do
    let sourceRoot = root </> "demo"
        dependencyRoot = root </> "dep"
        storeRoot = root </> "store"
        options = InstallV2Options sourceRoot (Just storeRoot) False False False AppleArm64
    createDirectoryIfMissing True (sourceRoot </> "src")
    createDirectoryIfMissing True (dependencyRoot </> "src")
    writeFile
      (dependencyRoot </> "dep.cabal")
      ( unlines
          [ "cabal-version: 3.0",
            "name: dep",
            "version: 1.0.0",
            "library",
            "  exposed-modules: Dep",
            "  hs-source-dirs: src",
            "  default-language: Haskell2010"
          ]
      )
    writeFile (dependencyRoot </> "src" </> "Dep.hs") "module Dep where\nidentity x = x\n"
    writeFile
      (sourceRoot </> "demo.cabal")
      ( unlines
          [ "cabal-version: 3.0",
            "name: demo",
            "version: 0.1.0.0",
            "library",
            "  exposed-modules: Demo",
            "  hs-source-dirs: src",
            "  build-depends: dep",
            "  default-language: Haskell2010"
          ]
      )
    writeFile (sourceRoot </> "src" </> "Demo.hs") "module Demo where\nimport Dep\nresult = identity\n"
    _ <- installV2 options
    let targetStoreRoot = storeRoot </> nativeTargetStoreDirectory AppleArm64
    storeEntries <- listDirectory targetStoreRoot
    let dependencyStores = filter ("dep-1.0.0-" `isPrefixOf`) storeEntries
    case dependencyStores of
      [dependencyStore] -> do
        assertFileExists (targetStoreRoot </> dependencyStore </> "Dep" </> "resolve.cbor")
        assertFileExists (targetStoreRoot </> dependencyStore </> "Dep" </> "type.cbor")
      _ -> assertFailure ("expected one installed dependency, got " <> show dependencyStores)

test_installV2StaleTypeArtifact :: Assertion
test_installV2StaleTypeArtifact =
  withTempDir "aihc-install-v2-stale-type" $ \root -> do
    let sourceRoot = root </> "source"
        sourceDir = sourceRoot </> "src"
        options = InstallV2Options sourceRoot (Just (root </> "store")) False False False AppleArm64
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
            "  default-language: Haskell2010"
          ]
      )
    writeFile (sourceDir </> "Demo.hs") "module Demo where\nvalue x = x\n"
    first <- installV2 options
    let artifactPath = installV2StorePath first </> "Demo" </> "type.cbor"
    artifact <- BS.readFile artifactPath
    BS.writeFile artifactPath (BS.take 11 artifact <> BS.singleton 1 <> BS.drop 12 artifact)
    rebuilt <- installV2 options
    assertEqual "rebuilt module" ["Demo"] (installV2WrittenModules rebuilt)

test_installV2ResolveDependencies :: Assertion
test_installV2ResolveDependencies =
  withTempDir "aihc-install-v2-dependencies" $ \root -> do
    let sourceRoot = root </> "source"
        sourceDir = sourceRoot </> "src" </> "Demo"
        options = InstallV2Options sourceRoot (Just (root </> "store")) False False False AppleArm64
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
    writeFile (sourceDir </> "A.hs") "module Demo.A where\nimport Demo.B\na = b\n"
    writeFile (sourceDir </> "B.hs") "module Demo.B where\nb x = x\n"
    _ <- installV2 options
    writeFile (sourceDir </> "B.hs") "module Demo.B where\nb x = (x)\n"
    sourceChanged <- installV2 options
    assertEqual "source-only change" ["Demo.B"] (sort (installV2WrittenModules sourceChanged))
    assertEqual "dependent with equal scope" ["Demo.A"] (sort (installV2ReusedModules sourceChanged))
    writeFile (sourceDir </> "B.hs") "module Demo.B where\nb x = x\nc x = x\n"
    scopeChanged <- installV2 options
    assertEqual "changed scope and dependent modules" ["Demo.A", "Demo.B"] (sort (installV2WrittenModules scopeChanged))
    assertEqual "no reused dependent after scope change" [] (installV2ReusedModules scopeChanged)

test_installV2StopsAtEqualScope :: Assertion
test_installV2StopsAtEqualScope =
  withTempDir "aihc-install-v2-scope-boundary" $ \root -> do
    let sourceRoot = root </> "source"
        sourceDir = sourceRoot </> "src" </> "Demo"
        options = InstallV2Options sourceRoot (Just (root </> "store")) False False False AppleArm64
    createDirectoryIfMissing True sourceDir
    writeFile
      (sourceRoot </> "demo.cabal")
      ( unlines
          [ "cabal-version: 3.0",
            "name: demo",
            "version: 0.1.0.0",
            "library",
            "  exposed-modules: Demo.A, Demo.B, Demo.C",
            "  hs-source-dirs: src",
            "  default-language: Haskell2010"
          ]
      )
    writeFile (sourceDir </> "A.hs") "module Demo.A where\na x = x\n"
    writeFile (sourceDir </> "B.hs") "module Demo.B where\nimport Demo.A\nb x = x\n"
    writeFile (sourceDir </> "C.hs") "module Demo.C where\nimport Demo.B\nc x = x\n"
    _ <- installV2 options
    writeFile (sourceDir </> "A.hs") "module Demo.A where\na x = x\na2 x = x\n"
    changed <- installV2 options
    assertEqual "changed module and direct dependent" ["Demo.A", "Demo.B"] (sort (installV2WrittenModules changed))
    assertEqual "transitive dependent with equal direct scope" ["Demo.C"] (sort (installV2ReusedModules changed))

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
