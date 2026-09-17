{-# LANGUAGE OverloadedStrings #-}

module Test.Aihc.Spec (tests) where

import Aihc.Capi (parseDependencyFile)
import Aihc.Cli.Build (build)
import Aihc.Cli.BuildModule (LinkBundle (..), linkBundleManifestPath, runLinkExe)
import Aihc.Cli.Install (InstallResult (..), install, parsePackageTarget)
import Aihc.Cli.Options (BuildOptions (..), Command (..), InstallOptions (..), LinkExeOptions (..), parseCommandPure)
import Aihc.Cli.PackageManifest (PackageManifest (..), packageManifestPath, readPackageManifest, writePackageManifest)
import Aihc.Cli.ResolveArtifact (ResolveArtifact (..), decodeResolveArtifact, encodeResolveArtifact)
import Aihc.Cli.TypeArtifact (TypeArtifact (..), decodeTypeArtifact)
import Aihc.Fc qualified as Fc
import Aihc.Hackage.Cabal qualified as HackageCabal
import Aihc.Hackage.Release (BootLibrary (..), emulatedGhc, lookupBootLibrary)
import Aihc.Native (NativeTarget (..), OptimizationLevel (..), backendCompiler, hostNativeTarget, nativeTargetStoreDirectory)
import Aihc.PackagePlan (CoreProvider (..), coreProviderSourcePath, coreProviders)
import Aihc.PackagePlan.Source (moduleDepsDigest, parseInterfaceFile, parsedFileDeps)
import Aihc.Parser.Syntax qualified as Syntax
import Aihc.Resolve (PackageId (..), ResolvedName (..), Scope (..), emptyScope)
import Aihc.Tc (TyConInfo (..), tcInterfaceTyCons, tyConName)
import Aihc.Testing.EvalFixture (packageSourceRoot, posixWidthModuleDirectory)
import Control.Exception (IOException, bracket, try)
import Control.Monad (forM_, void)
import Data.Aeson (FromJSON (..), withObject, (.!=), (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Char (isSpace)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, sort, stripPrefix)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Yaml qualified as Y
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
    withCurrentDirectory,
  )
import System.Environment (lookupEnv)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath (takeDirectory, takeExtension, (</>))
import System.IO (hClose, openTempFile)
import System.IO.Error (ioeGetErrorString)
import System.Process (readProcess, readProcessWithExitCode)
import Test.Aihc.SeedStore
  ( Sandbox (..),
    SeedStore,
    acquireCoreStore,
    acquireLtoStore,
    acquirePrimStore,
    buildHostTarget,
    installTestTargets,
    releaseSeedStore,
    seededPackagePath,
    withSandbox,
  )
import Test.Tasty (DependencyType (AllFinish), TestTree, dependentTestGroup, testGroup, withResource)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)

-- | The core libraries are seeded once for the whole group rather than by each
-- test; see "Test.Aihc.SeedStore". aihc-base is a separate, nested resource so
-- that running only the @install@ tests never installs it.
tests :: TestTree
tests =
  withResource acquirePrimStore releaseSeedStore $ \primStore ->
    testGroup
      "aihc"
      [ withResource (acquireCoreStore primStore) releaseSeedStore $ \coreStore ->
          -- These run one at a time: build resolves its build directory
          -- against the working directory, and the process has only one.
          dependentTestGroup
            "build"
            AllFinish
            [ testCase "builds imported source modules and runs the executable" (test_buildModuleSourceDirectories coreStore),
              testCase "reports the ambiguous installed module" (test_buildModuleAmbiguousModule coreStore),
              testCase "reports the generated entry collision" (test_buildModuleEntryCollision coreStore),
              testCase "writes a link bundle that link-exe turns into the executable" (test_buildModuleLinkBundle coreStore),
              testCase "parses the optimization level" test_buildModuleOptimizationOption,
              testCase "parses --check-prim-bounds" test_checkPrimBoundsOption,
              testCase "builds every executable of a Cabal package" (test_buildExecutables coreStore),
              testCase "keeps the intermediate output of the executable modules" (test_buildModuleKeepIntermediates coreStore),
              -- The --lto builds need core libraries built with the flag,
              -- which the other stores do not hold.
              withResource acquireLtoStore releaseSeedStore $ \ltoStore ->
                dependentTestGroup
                  "lto"
                  AllFinish
                  [ testCase "compiles the merged program of the executable once" (test_buildLto ltoStore),
                    testCase "compiles the merged program of each executable of a package" (test_buildPackageLto ltoStore),
                    testCase "installs a package as System FC only" (test_installLto ltoStore)
                  ]
            ],
        testGroup
          "install"
          [ testCase "code-quality install fixtures" (testInstallFixtures primStore),
            testCase "compiles and archives capi wrappers" (test_installCapi primStore),
            testCase "installs the runtime as the aihc-rts package" (test_installRuntimePackage primStore),
            testCase "resolves an include of an RTS header" (test_installRtsHeaderInclude primStore),
            testCase "defines MIN_VERSION macros from the installed dependency versions" (test_installMinVersionMacros primStore),
            testCase "core-libs versions match the emulated GHC release" test_coreLibsMatchRelease,
            testCase "selects Cabal source dirs by target architecture" (test_installArchSourceDirs primStore),
            -- This one installs aihc-prim into an empty store on purpose: it is
            -- the test that covers the install the seed store performs.
            testCase "parses Hackage package targets" test_parsePackageTarget
          ],
        testGroup
          "artifacts"
          [ testCase "resolve artifacts keep each kind of resolved name" test_resolveArtifactRoundTrip
          ],
        testGroup
          "sources"
          [ testCase "the POSIX type widths match the platform headers" test_posixTypeWidths,
            testCase "the sigset_t size matches the platform headers" test_sigsetSize,
            testCase "an included header is part of the module digest" test_moduleDepsIncludedHeader,
            testCase "reads the headers out of a compiler dependency file" test_parseDependencyFile
          ]
      ]

-- | The suffix the backend adds beside the object file it emits.
nativeArtifactExtension :: NativeTarget -> FilePath
nativeArtifactExtension target =
  case target of
    AppleArm64 -> ".lir"
    LinuxAmd64 -> ".lir"
    Llvm -> ".ll"
    Wasm32Wasip3 -> ".s"

-- | The target an install test names.
--
-- The backend of the host, always. An install test used to be free to name a
-- foreign target, because compiling Haskell for one needs nothing of it, and
-- the seed store carried aihc-prim for every target they named. They no
-- longer do: an install that reaches @aihc-base@ can need a C toolchain for
-- its target, and the host has one only for itself.
hostBackendTarget :: IO NativeTarget
hostBackendTarget = maybe (assertFailure "the host has a native aihc backend") pure hostNativeTarget

-- | The POSIX widths @aihc-base@ assumes are the widths the platform's own
-- headers give.
--
-- @System.Posix.Types.Repr@ states, per platform, how wide each POSIX type is
-- and whether it is signed. A Haskell module cannot ask the C headers, so
-- those numbers are written by hand, and a wrong one is silent: the FFI
-- passes the wrong number of bytes and a program misreads a @stat@ buffer
-- rather than failing to build.
--
-- This test turns the module into C and lets the C compiler settle it. Each
-- alias becomes a pair of static assertions about the type it stands for, and
-- the file is compiled against the real headers of the platform the test runs
-- on. Nothing here states a width of its own; the only numbers are the ones
-- read out of the module, so the test cannot agree with a mistake.
--
-- It checks the platform it runs on, which is the one whose headers are at
-- hand: @src-darwin@ on a Mac, @src-linux@ on Linux. The wasm32 widths go
-- unchecked, and four of them are a choice rather than a fact, as that
-- module's own comment says.
test_posixTypeWidths :: Assertion
test_posixTypeWidths = do
  target <- case hostNativeTarget of
    Just hostTarget -> pure hostTarget
    Nothing -> assertFailure "the POSIX widths are stated for a host aihc has a target for"
  -- The sources of aihc-base, not of the compiler: the width modules belong
  -- to the library, and a nix build hands each its own store path.
  baseRoot <- packageSourceRoot "AIHC_BASE_SRC" "aihc-base"
  let platformDirectory = posixWidthModuleDirectory
  widths <- readPosixTypeWidths (baseRoot </> platformDirectory </> "System" </> "Posix" </> "Types" </> "Repr.hs")
  assertEqual "every POSIX alias has a width" (sort (map fst posixTypeCNames)) (sort (map fst widths))
  (compiler, targetArguments) <- backendCompiler target
  withTempDir "aihc-posix-type-widths" $ \directory -> do
    let source = directory </> "widths.c"
    writeFile source (renderPosixWidthAssertions widths)
    (status, out, err) <-
      readProcessWithExitCode compiler (targetArguments <> ["-std=c11", "-fsyntax-only", source]) ""
    assertEqual ("the platform headers agree with " <> platformDirectory <> "\n" <> out <> err) ExitSuccess status

-- | The @sigset_t@ size @aihc-base@ assumes is the size the platform's own
-- headers give.
--
-- @System.Posix.Internals.Repr@ states it per platform for the same reason
-- the widths above are stated per platform, and a wrong number is just as
-- silent: a caller allocates a buffer of it and hands the buffer to
-- @sigprocmask@, which writes the size the platform really uses.
--
-- The check is the same one: the number becomes a static assertion, compiled
-- against the headers of the platform the test runs on. WASI has no
-- @sigset_t@ at all, so nothing there is checked and nothing there is used.
test_sigsetSize :: Assertion
test_sigsetSize = do
  target <- case hostNativeTarget of
    Just hostTarget -> pure hostTarget
    Nothing -> assertFailure "the sigset_t size is stated for a host aihc has a target for"
  baseRoot <- packageSourceRoot "AIHC_BASE_SRC" "aihc-base"
  let platformDirectory = posixWidthModuleDirectory
  size <- readSigsetSize (baseRoot </> platformDirectory </> "System" </> "Posix" </> "Internals" </> "Repr.hs")
  (compiler, targetArguments) <- backendCompiler target
  withTempDir "aihc-sigset-size" $ \directory -> do
    let source = directory </> "sigset.c"
    writeFile
      source
      ( unlines
          [ -- sigset_t is POSIX rather than ISO C, and -std=c11 defines
            -- __STRICT_ANSI__, which turns glibc's default feature-test
            -- macros off and hides it. _GNU_SOURCE turns them back on, and
            -- has to come before any header. Darwin declares it either way.
            "#define _GNU_SOURCE 1",
            "#include <signal.h>",
            "_Static_assert(sizeof(sigset_t) == " <> show size <> ", \"sigset_t size\");"
          ]
      )
    (status, out, err) <-
      readProcessWithExitCode compiler (targetArguments <> ["-std=c11", "-fsyntax-only", source]) ""
    assertEqual ("the platform headers agree with " <> platformDirectory <> "\n" <> out <> err) ExitSuccess status

-- | Read the @sizeofSigsetT = 4@ line of a platform's size module.
readSigsetSize :: FilePath -> IO Int
readSigsetSize path = do
  contents <- readFile path
  case [size | ["sizeofSigsetT", "=", size] <- map words (lines contents)] of
    [size] -> pure (read size)
    _ -> assertFailure ("cannot read the sigset_t size out of " <> path)

-- | The C type each alias of @System.Posix.Types.Repr@ stands for.
posixTypeCNames :: [(String, String)]
posixTypeCNames =
  [ ("CBlkCntRep", "blkcnt_t"),
    ("CBlkSizeRep", "blksize_t"),
    ("CCcRep", "cc_t"),
    ("CDevRep", "dev_t"),
    ("CFsBlkCntRep", "fsblkcnt_t"),
    ("CFsFilCntRep", "fsfilcnt_t"),
    ("CGidRep", "gid_t"),
    ("CIdRep", "id_t"),
    ("CInoRep", "ino_t"),
    ("CKeyRep", "key_t"),
    ("CModeRep", "mode_t"),
    ("CNfdsRep", "nfds_t"),
    ("CNlinkRep", "nlink_t"),
    ("COffRep", "off_t"),
    ("CPidRep", "pid_t"),
    ("CRLimRep", "rlim_t"),
    ("CSocklenRep", "socklen_t"),
    ("CSpeedRep", "speed_t"),
    ("CTcflagRep", "tcflag_t"),
    ("CUidRep", "uid_t")
  ]

-- | Read the @type CFooRep = Int32@ lines of a platform's width module as the
-- number of bytes and whether the type is signed.
readPosixTypeWidths :: FilePath -> IO [(String, (Int, Bool))]
readPosixTypeWidths path = do
  contents <- readFile path
  mapM parseLine [line | line <- lines contents, "type " `isPrefixOf` line]
  where
    parseLine line =
      case words line of
        ["type", name, "=", haskellType] -> (,) name <$> widthOf haskellType
        _ -> assertFailure ("cannot read a width out of " <> path <> ": " <> line)
    widthOf haskellType =
      case haskellType of
        'I' : 'n' : 't' : bits -> pure (read bits `div` 8, True)
        'W' : 'o' : 'r' : 'd' : bits -> pure (read bits `div` 8, False)
        _ -> assertFailure ("not a sized integer type in " <> path <> ": " <> haskellType)

-- | A C file that holds the widths against the platform's own headers.
renderPosixWidthAssertions :: [(String, (Int, Bool))] -> String
renderPosixWidthAssertions widths =
  unlines
    ( [ -- glibc hides blksize_t and key_t behind its feature-test macros, and
        -- -std=c11 defines __STRICT_ANSI__, which turns the default set off.
        -- _GNU_SOURCE turns all of them back on. It has to come before any
        -- header, and it changes what the headers declare, never how wide a
        -- type is. Darwin declares these types either way.
        "#define _GNU_SOURCE 1",
        "",
        "#include <poll.h>",
        "#include <sys/resource.h>",
        "#include <sys/socket.h>",
        "#include <sys/types.h>",
        "#include <termios.h>",
        ""
      ]
        <> concatMap assertions posixTypeCNames
    )
  where
    assertions (name, cName) =
      case lookup name widths of
        Nothing -> []
        Just (bytes, signed) ->
          [ "_Static_assert(sizeof(" <> cName <> ") == " <> show bytes <> ", \"" <> cName <> " width\");",
            "_Static_assert(((" <> cName <> ")-1 < (" <> cName <> ")0) == " <> (if signed then "1" else "0") <> ", \"" <> cName <> " signedness\");"
          ]

-- | The digest of a preprocessed module covers the headers it includes:
-- editing one changes the digest even though the module itself is untouched.
test_moduleDepsIncludedHeader :: Assertion
test_moduleDepsIncludedHeader =
  withTempDir "aihc-module-deps-header" $ \root -> do
    let sourceDir = root </> "src"
        includeDir = root </> "include"
        fileInfo =
          HackageCabal.FileInfo
            { HackageCabal.fileInfoPath = sourceDir </> "Demo.hs",
              HackageCabal.fileInfoExtensions = ["CPP"],
              HackageCabal.fileInfoCppOptions = [],
              HackageCabal.fileInfoIncludeDirs = [includeDir],
              HackageCabal.fileInfoLanguage = Just "Haskell2010",
              HackageCabal.fileInfoDependencies = [],
              HackageCabal.fileInfoPreprocessor = Nothing
            }
        -- The module includes a header of its own, so the compiler headers
        -- are not needed and their directory stays empty.
        digest = moduleDepsDigest . parsedFileDeps <$> parseInterfaceFile (root </> "headers") root mempty fileInfo
    createDirectoryIfMissing True sourceDir
    createDirectoryIfMissing True includeDir
    writeFile (sourceDir </> "Demo.hs") (unlines ["module Demo (demo) where", "#include \"demo.h\"", "demo = VALUE"])
    writeFile (includeDir </> "demo.h") (unlines ["#define VALUE ()"])
    original <- digest
    unchanged <- digest
    assertEqual "an unchanged module keeps its digest" original unchanged
    writeFile (includeDir </> "demo.h") (unlines ["/* the value the module reads */", "#define VALUE ()"])
    changed <- digest
    assertBool "an edited header changes the digest" (original /= changed)

-- | The scope encoder must keep each constructor of a resolved name.
--
-- The essential property is that the encoder and the decoder agree on all
-- four constructors of a resolved name. No fixture can test this property.
-- An exported scope holds only top-level names, thus source text cannot put
-- a local name or an error in a scope that the compiler writes to the
-- store. This test is a hand-written exception to the fixture rule.
test_resolveArtifactRoundTrip :: Assertion
test_resolveArtifactRoundTrip = do
  let qualified =
        emptyScope
          { scopeTypes = Map.singleton "Box" (ResolvedTopLevel (PackageId "demo") "Demo" (Syntax.mkName Nothing Syntax.NameConId "Box"))
          }
      scope =
        emptyScope
          { scopeTerms =
              Map.fromList
                [ ("here", ResolvedLocal 7 (Syntax.mkUnqualifiedName Syntax.NameVarId "here")),
                  ("broken", ResolvedError "unbound"),
                  ("syntax", ResolvedSyntax)
                ],
            scopeQualifiedModules = Map.singleton "D" qualified
          }
      artifact = ResolveArtifact "Demo" scope
      bytes = BL.toStrict (encodeResolveArtifact artifact)
  decoded <- either (assertFailure . ("invalid resolve artifact: " <>)) pure (decodeResolveArtifact bytes)
  let decodedScope = resolveArtifactScope decoded
  assertEqual "resolved terms" (scopeTerms scope) (scopeTerms decodedScope)
  assertEqual "qualified module types" (Map.map scopeTypes (scopeQualifiedModules scope)) (Map.map scopeTypes (scopeQualifiedModules decodedScope))
  assertBool "resolve artifact round trip" (artifact == decoded)

-- | Each package fixture specifies its expected error or stored constructors.
data InstallFixture = InstallFixture
  { installFixtureError :: Maybe String,
    installFixtureTyCons :: [(String, [String])]
  }

instance FromJSON InstallFixture where
  parseJSON = withObject "install fixture" $ \obj -> do
    status <- obj .: "status"
    if status == ("pass" :: String)
      then InstallFixture <$> obj .:? "expect-error" <*> obj .:? "expect-type-constructors" .!= []
      else fail "install fixtures require pass status"

testInstallFixtures :: IO SeedStore -> Assertion
testInstallFixtures getStore = do
  root <- findFixtureRoot "bin/aihc/test/Test/Fixtures/install/code-quality"
  names <- sort <$> listDirectory root
  forM_ names $ \name -> do
    let directory = root </> name
    fixture <- Y.decodeFileThrow (directory </> "fixture.yaml")
    assertBool (name <> ": empty expected diagnostic") (maybe True (not . null) (installFixtureError fixture))
    withSandbox getStore ("aihc-" <> name) $ \sandbox -> do
      store <- sandboxStore sandbox "store"
      outcome <- try (install (InstallOptions directory (Just store) (Just (sandboxRoot sandbox </> "build")) False False False False False False False O0 False True False False buildHostTarget))
      case outcome :: Either IOException InstallResult of
        Left err -> do
          assertBool (name <> ": unexpected error: " <> show err) (maybe False (`isInfixOf` show err) (installFixtureError fixture))
        Right result ->
          case installFixtureError fixture of
            Just _ -> assertFailure (name <> ": install accepted a package that requires an error")
            Nothing -> forM_ (installFixtureTyCons fixture) $ \(moduleName, expected) -> do
              bytes <- BL.readFile (installStorePath result </> moduleName </> "type.cbor")
              artifact <- either (assertFailure . ((name <> ": invalid type artifact: ") <>)) pure (decodeTypeArtifact bytes)
              let actual = map (T.unpack . tyConName . tciTyCon) (tcInterfaceTyCons (typeArtifactInterface artifact))
              forM_ expected $ \constructor ->
                assertBool (name <> ": missing type constructor " <> constructor <> " in " <> moduleName) (constructor `elem` actual)

test_parsePackageTarget :: Assertion
test_parsePackageTarget = do
  assertEqual "bare name" (Just ("nats", Nothing)) (parsePackageTarget "nats")
  assertEqual "hyphenated name" (Just ("aihc-base", Nothing)) (parsePackageTarget "aihc-base")
  assertEqual "name and version" (Just ("nats", Just "1.1.2")) (parsePackageTarget "nats-1.1.2")
  assertEqual "hyphenated name and version" (Just ("aihc-base", Just "4.21.2.0")) (parsePackageTarget "aihc-base-4.21.2.0")
  assertEqual "path" Nothing (parsePackageTarget "core-libs/aihc-base")
  assertEqual "spaces" Nothing (parsePackageTarget "not a package")

-- | Give a @build@ test a sandbox holding a seeded store, the fixture that
-- the default options compile, and those options. The options build at the
-- default level, which compiles each module to its own object; the seeded
-- core libraries are built at that level. @-O2@ compiles the whole program
-- at once and needs the @lto@ seed store.
withBuildModuleSandbox ::
  IO SeedStore ->
  String ->
  (Sandbox -> FilePath -> FilePath -> BuildOptions -> Assertion) ->
  Assertion
withBuildModuleSandbox getStore prefix action = do
  fixtureRoot <- findFixtureRoot "bin/aihc/test/Test/Fixtures/build/source-directories"
  withSandbox getStore prefix $ \sandbox -> do
    storeRoot <- sandboxStore sandbox "store"
    let options =
          BuildOptions
            { buildInput = fixtureRoot </> "Main.hs",
              buildSourceDirectories = [fixtureRoot],
              buildPackageConstraints = ["aihc-base == 4.21.2.0"],
              buildTarget = buildHostTarget,
              buildStoreRoot = Just storeRoot,
              buildBuildRoot = Nothing,
              buildWorkspace = Nothing,
              buildKeepCore = False,
              buildKeepGrin = False,
              buildKeepLir = False,
              buildKeepNative = False,
              buildLint = False,
              buildCheckPrimBounds = False,
              buildLto = False,
              buildOptimization = O0,
              buildNoLink = False,
              buildVerbose = False,
              buildOutput = Just (sandboxRoot sandbox </> "program")
            }
    action sandbox fixtureRoot storeRoot options

-- | Run @build@ and return the error it reports, failing the test when it
-- succeeds instead.
buildModuleError :: FilePath -> BuildOptions -> String -> IO String
buildModuleError workingDirectory options expectation = do
  result <-
    try (withCurrentDirectory workingDirectory (build options)) ::
      IO (Either IOException [FilePath])
  case result of
    Left err -> pure (ioeGetErrorString err)
    Right _ -> assertFailure expectation

test_buildModuleSourceDirectories :: IO SeedStore -> Assertion
test_buildModuleSourceDirectories getStore =
  withBuildModuleSandbox getStore "aihc-build" $ \sandbox fixtureRoot storeRoot options -> do
    let root = sandboxRoot sandbox
        output = sandboxRoot sandbox </> "program"
        target = buildTarget options
    basePackage <- seededPackagePath storeRoot target "aihc-base"
    manifestResult <- readPackageManifest (packageManifestPath basePackage)
    manifest <- either assertFailure pure manifestResult
    assertBool "package manifest contains Prelude" ("Prelude" `elem` packageManifestModules manifest)
    let unusedResolve = basePackage </> "Data" </> "Bool" </> "resolve.cbor"
        unusedType = basePackage </> "Data" </> "Bool" </> "type.cbor"
    resolveBytes <- BS.readFile unusedResolve
    BS.writeFile unusedResolve "invalid unused resolve interface"
    -- The store is never listed, so a stray package directory beside the
    -- real ones cannot take part in the build.
    let strayPackage = storeRoot </> nativeTargetStoreDirectory target </> "aihc-base-9999-0123456789abcdef"
    createDirectoryIfMissing True strayPackage
    writePackageManifest (packageManifestPath strayPackage) manifest {packageManifestVersion = "9999"}
    void (withCurrentDirectory root (build options))
    let mainObject = root </> ".aihc-target" </> nativeTargetStoreDirectory target </> "Main" </> "Main.o"
    assertFileExists mainObject
    assertFileDoesNotExist (root </> ".aihc-target" </> nativeTargetStoreDirectory target </> "GHC" </> "Base" </> "GHC.Base.o")
    -- The second build finds every module of the executable unchanged.
    mainTime <- getModificationTime mainObject
    void (withCurrentDirectory root (build options))
    rebuiltTime <- getModificationTime mainObject
    assertEqual "unchanged executable modules are reused" mainTime rebuiltTime
    let customBuildRoot = root </> "custom-build-root"
    void (withCurrentDirectory fixtureRoot (build options {buildBuildRoot = Just customBuildRoot}))
    assertFileExists (customBuildRoot </> nativeTargetStoreDirectory target </> "Main" </> "Main.o")
    BS.writeFile unusedResolve resolveBytes
    typeBytes <- BS.readFile unusedType
    BS.writeFile unusedType "invalid unused type interface"
    void (withCurrentDirectory root (build options))
    BS.writeFile unusedType typeBytes
    void (withCurrentDirectory root (build options {buildLint = True}))
    -- The entry unit is generated beside the module objects of the
    -- executable, and the runtime is an installed package like any other.
    assertFileExists (root </> ".aihc-target" </> nativeTargetStoreDirectory target </> "entry.o")
    rtsPackage <- seededPackagePath storeRoot target "aihc-rts"
    assertFileExists (rtsPackage </> "cbits" </> "native_aihc_runtime.o")
    (status, stdout, stderr) <- readProcessWithExitCode output [] ""
    assertEqual "executable exit status" ExitSuccess status
    assertEqual "executable stdout" "build works\n" stdout
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

-- | The @-O@ option of @build@ and @install@ takes level 0, 1, 2 or s,
-- and the default is 0. The help text names the option as @-O LEVEL@, which is the
-- form the benchmark runner detects.
--
-- The essential property is the command line, which no Haskell source text
-- can trigger. This test is a hand-written exception to the fixture rule.
test_buildModuleOptimizationOption :: Assertion
test_buildModuleOptimizationOption = do
  let arguments extra = ["build", "Main.hs", "--target", "apple-arm64"] <> extra
      levelOf parsed =
        case parsed of
          Right (CmdBuild options) -> pure (buildOptimization options)
          Right other -> assertFailure ("unexpected command: " <> show other)
          Left err -> assertFailure ("parse error: " <> err)
  assertEqual "default level" O0 =<< levelOf (parseCommandPure (arguments []))
  assertEqual "-O0" O0 =<< levelOf (parseCommandPure (arguments ["-O0"]))
  assertEqual "-O2" O2 =<< levelOf (parseCommandPure (arguments ["-O2"]))
  assertEqual "-O 0" O0 =<< levelOf (parseCommandPure (arguments ["-O", "0"]))
  assertEqual "-O1" O1 =<< levelOf (parseCommandPure (arguments ["-O1"]))
  assertEqual "-Os" Os =<< levelOf (parseCommandPure (arguments ["-Os"]))
  case parseCommandPure (arguments ["-O3"]) of
    Left err -> assertBool ("rejects -O3: " <> err) ("expected 0, 1, 2 or s" `isInfixOf` err)
    Right command -> assertFailure ("accepted -O3: " <> show command)
  installLevel <-
    case parseCommandPure ["install", "demo", "--target", "apple-arm64", "-O0"] of
      Right (CmdInstall options) -> pure (installOptimization options)
      other -> assertFailure ("install parse: " <> show other)
  assertEqual "install -O0" O0 installLevel
  help <- either pure (assertFailure . ("help is not a failure: " <>) . show) (parseCommandPure ["build", "--help"])
  assertBool ("help names -O LEVEL:\n" <> help) ("[-O LEVEL]" `isInfixOf` help)

-- | @--check-prim-bounds@ is off by default on @build@ and @install@, as
-- GHC leaves the array primitives unchecked, and the flag turns it on.
test_checkPrimBoundsOption :: Assertion
test_checkPrimBoundsOption = do
  let buildFlag extra =
        case parseCommandPure (["build", "Main.hs", "--target", "apple-arm64"] <> extra) of
          Right (CmdBuild options) -> pure (buildCheckPrimBounds options)
          other -> assertFailure ("build parse: " <> show other)
      installFlag extra =
        case parseCommandPure (["install", "demo", "--target", "apple-arm64"] <> extra) of
          Right (CmdInstall options) -> pure (installCheckPrimBounds options)
          other -> assertFailure ("install parse: " <> show other)
  assertEqual "build default" False =<< buildFlag []
  assertEqual "build flag" True =<< buildFlag ["--check-prim-bounds"]
  assertEqual "install default" False =<< installFlag []
  assertEqual "install flag" True =<< installFlag ["--check-prim-bounds"]

-- | @--no-link@ leaves no executable behind. The bundle it writes instead is
-- self-contained: linking it from another directory, with the store gone,
-- still produces the program.
test_buildModuleLinkBundle :: IO SeedStore -> Assertion
test_buildModuleLinkBundle getStore =
  withBuildModuleSandbox getStore "aihc-link-bundle" $ \sandbox _fixtureRoot storeRoot options -> do
    let root = sandboxRoot sandbox
        bundle = root </> "bundle"
        output = root </> "linked" </> "program"
    void (withCurrentDirectory root (build options {buildNoLink = True, buildOutput = Just bundle}))
    assertFileDoesNotExist (root </> "program")
    assertFileExists (linkBundleManifestPath bundle)
    decoded <- Aeson.eitherDecode <$> BL.readFile (linkBundleManifestPath bundle)
    manifest <- either assertFailure pure decoded
    assertEqual "bundle target" (buildTarget options) (linkBundleTarget manifest)
    assertBool "bundle lists the main object" (any ("Main.o" `isSuffixOf`) (linkBundleObjects manifest))
    assertBool "bundle lists the base archive" (any ("libaihc-base.a" `isSuffixOf`) (linkBundleArchives manifest))
    assertBool "bundle lists the entry object" (any ("entry.o" `isSuffixOf`) (linkBundleObjects manifest))
    assertBool "bundle lists the runtime objects" (any ("native_aihc_runtime.o" `isSuffixOf`) (linkBundleObjects manifest))
    forM_ (linkBundleObjects manifest <> linkBundleArchives manifest) $ \input -> do
      assertBool ("bundle input is relative: " <> input) ("inputs/" `isPrefixOf` input)
      assertFileExists (bundle </> input)
    removeDirectoryRecursive storeRoot
    withCurrentDirectory root $
      runLinkExe LinkExeOptions {linkExeBundle = bundle, linkExeOutputFile = output}
    (status, stdout, stderr) <- readProcessWithExitCode output [] ""
    assertEqual "linked executable exit status" ExitSuccess status
    assertEqual "linked executable stdout" "build works\n" stdout
    assertEqual "linked executable stderr" "" stderr

-- | @-O2@ implies @--lto@: every module stops at System FC and the merged
-- program of the executable is compiled once, without
-- the values the entry does not reach. The package archives hold no Haskell
-- object, the executable links the one program object, and an unchanged
-- program keeps that object.
test_buildLto :: IO SeedStore -> Assertion
test_buildLto getStore =
  withBuildModuleSandbox getStore "aihc-build-lto" $ \sandbox _fixtureRoot storeRoot options -> do
    ltoFlag <-
      case parseCommandPure ["build", "Main.hs", "--target", "apple-arm64", "--lto"] of
        Right (CmdBuild parsed) -> pure (buildLto parsed)
        other -> assertFailure ("build parse: " <> show other)
    assertBool "build parses --lto" ltoFlag
    installFlag <-
      case parseCommandPure ["install", "demo", "--target", "apple-arm64", "--lto"] of
        Right (CmdInstall parsed) -> pure (installLto parsed)
        other -> assertFailure ("install parse: " <> show other)
    assertBool "install parses --lto" installFlag
    let root = sandboxRoot sandbox
        output = root </> "program"
        target = buildTarget options
        targetRoot = root </> ".aihc-target" </> nativeTargetStoreDirectory target
        programObject = targetRoot </> "lto" </> "program" </> "program.o"
        programCore = targetRoot </> "lto" </> "program" </> "core"
        ltoOptions = options {buildOptimization = O2, buildKeepCore = True}
    void (withCurrentDirectory root (build ltoOptions))
    -- The modules of the executable stop at System FC.
    mainCore <- readCoreFile (targetRoot </> "Main" </> "core")
    assertFileDoesNotExist (targetRoot </> "Main" </> "Main.o")
    assertFileExists programObject
    -- @--keep-core@ keeps the merged program as well as the modules it was
    -- merged from, so the kept program holds more than the executable's own
    -- module does.
    programCoreProgram <- readCoreFile programCore
    assertBool
      "the kept program holds the merged declarations"
      (length (Fc.programDecls programCoreProgram) > length (Fc.programDecls mainCore))
    -- The program object holds the entry and no value the entry does not
    -- reach: the fixture never uses the Data.Complex instances. Nor does it
    -- hold a constructor that no value it keeps names, so the info tables of
    -- @:+@ go the same way as the instances.
    symbols <- readProcess "nm" [programObject] ""
    assertBool "program object defines the entry" ("Aihc__dEntry_entry" `isInfixOf` symbols)
    assertBool "program object drops unreached values" (not ("Data__dComplex___sfEqComplex" `isInfixOf` symbols))
    assertBool "program object drops unreached constructors" (not ("Data__dComplex___o__t" `isInfixOf` symbols))
    -- So do the modules of aihc-base, whose archive holds no module object.
    basePackage <- seededPackagePath storeRoot target "aihc-base"
    manifest <- either assertFailure pure =<< readPackageManifest (packageManifestPath basePackage)
    assertBool "manifest records the lto flag" ("lto" `elem` packageManifestFlags manifest)
    assertBool "manifest lists the compiled modules" ("GHC.Base" `elem` packageManifestCompiledModules manifest)
    assertCoreFile (basePackage </> "GHC" </> "Base" </> "core")
    assertFileDoesNotExist (basePackage </> "GHC" </> "Base" </> "GHC.Base.o")
    members <- filter (not . ("__.SYMDEF" `isPrefixOf`)) . lines <$> readProcess "ar" ["-t", basePackage </> "lib" </> "libaihc-base.a"] ""
    let moduleObjects = [T.unpack name <> ".o" | name <- packageManifestCompiledModules manifest]
    assertBool ("base archive holds no module object: " <> show members) (all (`notElem` moduleObjects) members)
    (status, stdout, stderr) <- readProcessWithExitCode output [] ""
    assertEqual "executable exit status" ExitSuccess status
    assertEqual "executable stdout" "build works\n" stdout
    assertEqual "executable stderr" "" stderr
    -- The second build finds the program object current.
    objectTime <- getModificationTime programObject
    void (withCurrentDirectory root (build ltoOptions))
    rebuiltTime <- getModificationTime programObject
    assertEqual "an unchanged program keeps its object" objectTime rebuiltTime
    -- A link bundle carries the program object in place of the module objects.
    let bundle = root </> "bundle"
    void (withCurrentDirectory root (build ltoOptions {buildNoLink = True, buildOutput = Just bundle}))
    decoded <- Aeson.eitherDecode <$> BL.readFile (linkBundleManifestPath bundle)
    linkManifest <- either assertFailure pure decoded
    assertBool "bundle lists the program object" (any ("program.o" `isSuffixOf`) (linkBundleObjects linkManifest))
    assertBool "bundle lists no module object" (not (any ("Main.o" `isSuffixOf`) (linkBundleObjects linkManifest)))

-- | An @install --lto@ writes the System FC of each module and nothing
-- below it, and an archive without Haskell code. The next install reuses
-- the unit. The seeded core libraries of this store are built at @-O2@,
-- which is the same build as @-O2 --lto@.
test_installLto :: IO SeedStore -> Assertion
test_installLto getStore = do
  fixtureRoot <- findFixtureRoot "bin/aihc/test/Test/Fixtures/install/keep-grin"
  withSandbox getStore "aihc-install-lto" $ \sandbox -> do
    storeRoot <- sandboxStore sandbox "store"
    let options = InstallOptions fixtureRoot (Just storeRoot) (Just (sandboxRoot sandbox </> "build")) False False False False False False True O2 False False False False buildHostTarget
    result <- install options
    let packageRoot = installStorePath result
    assertEqual "lto install writes the module" ["Demo"] (installWrittenModules result)
    assertCoreFile (packageRoot </> "Demo" </> "core")
    assertFileDoesNotExist (packageRoot </> "Demo" </> "grin")
    assertFileDoesNotExist (packageRoot </> "Demo" </> "Demo.o")
    manifest <- either assertFailure pure =<< readPackageManifest (packageManifestPath packageRoot)
    assertEqual "manifest flags" ["lto", "O2"] (packageManifestFlags manifest)
    assertEqual "manifest compiled modules" ["Demo"] (packageManifestCompiledModules manifest)
    let archivePath = packageRoot </> "lib" </> "libdemo.a"
    assertFileExists archivePath
    members <- filter (not . ("__.SYMDEF" `isPrefixOf`)) . lines <$> readProcess "ar" ["-t", archivePath] ""
    assertEqual "archive members" [] members
    reused <- install options
    assertEqual "lto install reuses the module" ["Demo"] (installReusedModules reused)

-- | A workspace package that exposes a module of aihc-base makes an import
-- of that module ambiguous.
test_buildModuleAmbiguousModule :: IO SeedStore -> Assertion
test_buildModuleAmbiguousModule getStore =
  withBuildModuleSandbox getStore "aihc-build-ambiguous-module" $ \sandbox _ _ options -> do
    workspace <- findFixtureRoot "bin/aihc/test/Test/Fixtures/build/workspace"
    err <-
      buildModuleError
        (sandboxRoot sandbox)
        options
          { buildPackageConstraints = buildPackageConstraints options <> ["duplicate == 1.0.0"],
            buildWorkspace = Just workspace
          }
        "expected the installed module import to be ambiguous"
    assertBool "reports the ambiguous installed module" ("Ambiguous installed module: System.IO" `isInfixOf` err)

test_buildModuleEntryCollision :: IO SeedStore -> Assertion
test_buildModuleEntryCollision getStore =
  withBuildModuleSandbox getStore "aihc-build-entry-collision" $ \sandbox _ _ options -> do
    entryCollisionRoot <- findFixtureRoot "bin/aihc/test/Test/Fixtures/build/generated-entry-collision"
    err <-
      buildModuleError
        (sandboxRoot sandbox)
        options
          { buildInput = entryCollisionRoot </> "Main.hs",
            buildSourceDirectories = [entryCollisionRoot]
          }
        "expected the generated entry module to conflict"
    assertBool
      "reports the generated entry collision"
      ("Source module conflicts with generated module Aihc.Entry" `isInfixOf` err)

-- | @build@ takes a Cabal package and builds each of its buildable
-- executables from the sources and dependencies of its own stanza. The
-- library of the package is a dependency like any other, built in place
-- under the build root.
-- | Give a package @build@ test a sandbox holding a seeded store, a build
-- root, and the options that build the executables fixture there.
withBuildPackageSandbox ::
  IO SeedStore ->
  String ->
  (Sandbox -> FilePath -> BuildOptions -> Assertion) ->
  Assertion
withBuildPackageSandbox getStore prefix action = do
  fixtureRoot <- findFixtureRoot "bin/aihc/test/Test/Fixtures/build/executables"
  withSandbox getStore prefix $ \sandbox -> do
    storeRoot <- sandboxStore sandbox "store"
    let buildRoot = sandboxRoot sandbox </> "build"
        options =
          BuildOptions
            { buildInput = fixtureRoot,
              buildSourceDirectories = [],
              buildPackageConstraints = [],
              buildTarget = buildHostTarget,
              buildStoreRoot = Just storeRoot,
              buildBuildRoot = Just buildRoot,
              buildWorkspace = Nothing,
              buildKeepCore = False,
              buildKeepGrin = False,
              buildKeepLir = False,
              buildKeepNative = False,
              buildLint = False,
              buildCheckPrimBounds = False,
              buildLto = False,
              buildOptimization = O0,
              buildNoLink = False,
              buildVerbose = False,
              buildOutput = Nothing
            }
    action sandbox buildRoot options

test_buildExecutables :: IO SeedStore -> Assertion
test_buildExecutables getStore =
  withBuildPackageSandbox getStore "aihc-build" $ \sandbox buildRoot options -> do
    let root = sandboxRoot sandbox
        targetDirectory = nativeTargetStoreDirectory (buildTarget options)
    let binDirectory = buildRoot </> targetDirectory </> "bin"
    outputs <- withCurrentDirectory root (build options)
    assertEqual "built executables" [binDirectory </> "greet", binDirectory </> "shout"] outputs
    forM_ [("greet", "hello, build\n"), ("shout", "build!\n")] $ \(name, expected) -> do
      (status, stdout, stderr) <- readProcessWithExitCode (binDirectory </> name) [] ""
      assertEqual (name <> " exit status") ExitSuccess status
      assertEqual (name <> " stdout") expected stdout
      assertEqual (name <> " stderr") "" stderr
    assertFileDoesNotExist (binDirectory </> "skipped")
    assertFileExists (buildRoot </> targetDirectory </> "executables-0.1.0.0" </> "Words" </> "Words.o")
    let greetObject = buildRoot </> targetDirectory </> "exe" </> "greet" </> "Main" </> "Main.o"
    assertFileExists greetObject
    -- The second build finds every module of the executables unchanged.
    builtTime <- getModificationTime greetObject
    _ <- withCurrentDirectory root (build options)
    rebuiltTime <- getModificationTime greetObject
    assertEqual "unchanged executable modules are reused" builtTime rebuiltTime
    -- Without the link, each executable becomes a bundle in the chosen directory.
    let bundles = root </> "bundles"
    bundleOutputs <- withCurrentDirectory root (build options {buildNoLink = True, buildOutput = Just bundles})
    assertEqual "written bundles" [bundles </> "greet", bundles </> "shout"] bundleOutputs
    forM_ ["greet", "shout"] $ \name -> do
      assertFileExists (linkBundleManifestPath (bundles </> name))
      decoded <- Aeson.eitherDecode <$> BL.readFile (linkBundleManifestPath (bundles </> name))
      manifest <- either assertFailure pure decoded
      assertBool (name <> " bundle lists the main object") (any ("Main.o" `isSuffixOf`) (linkBundleObjects manifest))
    bundle <- either assertFailure pure . Aeson.eitherDecode =<< BL.readFile (linkBundleManifestPath (bundles </> "greet"))
    assertBool "greet links the package library" (any ("libexecutables.a" `isSuffixOf`) (linkBundleArchives bundle))

-- | @-O2@ on a package build compiles the merged program of each
-- executable once, under the executable's own build directory, and links
-- no module object.
test_buildPackageLto :: IO SeedStore -> Assertion
test_buildPackageLto getStore =
  withBuildPackageSandbox getStore "aihc-build-package-lto" $ \sandbox buildRoot options -> do
    let root = sandboxRoot sandbox
        targetRoot = buildRoot </> nativeTargetStoreDirectory (buildTarget options)
        ltoOptions = options {buildOptimization = O2}
    outputs <- withCurrentDirectory root (build ltoOptions)
    assertEqual "built executables" [targetRoot </> "bin" </> "greet", targetRoot </> "bin" </> "shout"] outputs
    forM_ [("greet", "hello, build\n"), ("shout", "build!\n")] $ \(name, expected) -> do
      assertFileExists (targetRoot </> "exe" </> name </> "lto" </> "program" </> "program.o")
      assertCoreFile (targetRoot </> "exe" </> name </> "Main" </> "core")
      assertFileDoesNotExist (targetRoot </> "exe" </> name </> "Main" </> "Main.o")
      (status, stdout, stderr) <- readProcessWithExitCode (targetRoot </> "bin" </> name) [] ""
      assertEqual (name <> " exit status") ExitSuccess status
      assertEqual (name <> " stdout") expected stdout
      assertEqual (name <> " stderr") "" stderr
    -- The library of the package stops at System FC as well.
    assertCoreFile (targetRoot </> "executables-0.1.0.0" </> "Words" </> "core")
    assertFileDoesNotExist (targetRoot </> "executables-0.1.0.0" </> "Words" </> "Words.o")
    -- The bundle of an executable carries its program object.
    let bundles = root </> "bundles"
    _ <- withCurrentDirectory root (build ltoOptions {buildNoLink = True, buildOutput = Just bundles})
    manifest <- either assertFailure pure . Aeson.eitherDecode =<< BL.readFile (linkBundleManifestPath (bundles </> "greet"))
    assertBool "bundle lists the program object" (any ("program.o" `isSuffixOf`) (linkBundleObjects manifest))
    assertBool "bundle lists no module object" (not (any ("Main.o" `isSuffixOf`) (linkBundleObjects manifest)))

-- | The @--keep-*@ flags of @build@ keep the output of each phase beside
-- the object of the module. They name the modules of the executable alone:
-- the installed packages are built as @install@ builds them, so a store
-- entry that holds none of these outputs still serves the build.
test_buildModuleKeepIntermediates :: IO SeedStore -> Assertion
test_buildModuleKeepIntermediates getStore =
  withBuildModuleSandbox getStore "aihc-build-keep" $ \sandbox _fixtureRoot _storeRoot options -> do
    let root = sandboxRoot sandbox
        target = buildTarget options
        keepOptions =
          options
            { buildKeepCore = True,
              buildKeepGrin = True,
              buildKeepLir = True,
              buildKeepNative = True
            }
        moduleRoot = root </> ".aihc-target" </> nativeTargetStoreDirectory target </> "Main"
    void (withCurrentDirectory root (build keepOptions))
    assertCoreFile (moduleRoot </> "core")
    forM_ ["grin", "cps.grin", "gc.grin"] $ \name -> assertFileExists (moduleRoot </> name)
    -- The object backends of the host write the object themselves, so the
    -- Lir text is also the native source there.
    assertFileExists (moduleRoot </> "Main.o" <> ".lir")
    assertFileExists (moduleRoot </> "Main.o" <> nativeArtifactExtension target)
    -- A build without the flags leaves no kept output behind.
    let plainRoot = root </> "plain"
    void (withCurrentDirectory root (build options {buildBuildRoot = Just plainRoot}))
    let plainModuleRoot = plainRoot </> nativeTargetStoreDirectory target </> "Main"
    assertFileExists (plainModuleRoot </> "Main.o")
    forM_ ["core", "grin", "cps.grin", "gc.grin", "Main.o.lir"] $ \name ->
      assertFileDoesNotExist (plainModuleRoot </> name)

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

assertCoreFile :: FilePath -> Assertion
assertCoreFile = void . readCoreFile

-- | The System FC a kept @core@ file holds, which must parse.
readCoreFile :: FilePath -> IO Fc.Program
readCoreFile path = do
  assertFileExists path
  core <- TIO.readFile path
  case Fc.parseProgram core of
    Left parseError -> assertFailure ("invalid Core file " <> path <> ": " <> Fc.renderParseError parseError)
    Right program -> pure program

test_installArchSourceDirs :: IO SeedStore -> Assertion
test_installArchSourceDirs getStore = do
  fixtureRoot <- findFixtureRoot "bin/aihc/test/Test/Fixtures/install/arch-source-dirs"
  targets <- installTestTargets
  withSandbox getStore "aihc-install-arch-source-dirs" $ \sandbox -> do
    storeRoot <- sandboxStore sandbox "store"
    forM_ targets $ \target -> do
      result <- install (InstallOptions fixtureRoot (Just storeRoot) (Just (sandboxRoot sandbox </> "build")) False True False False False False False O0 False False False False target)
      core <- readFile (installStorePath result </> "Payload" </> "core")
      let expected = archSourceDirPayload target
          unexpected = if expected == "32#" then "64#" else "32#"
      assertBool
        ("Core for " <> show target <> " contains " <> expected)
        (expected `isInfixOf` core)
      assertBool
        ("Core for " <> show target <> " does not contain " <> unexpected)
        (not (unexpected `isInfixOf` core))

archSourceDirPayload :: NativeTarget -> String
archSourceDirPayload target =
  case target of
    Wasm32Wasip3 -> "32#"
    _ -> "64#"

-- A file that guards code on @MIN_VERSION_base@ must see the version of
-- the aihc-base it is compiled against, not an unconditional yes. The wrong
-- branch here is not Haskell, so the install only succeeds when the macro
-- compares honestly.
test_installMinVersionMacros :: IO SeedStore -> Assertion
test_installMinVersionMacros getStore =
  withSandbox getStore "aihc-install-min-version-macros" $ \sandbox -> do
    storeRoot <- sandboxStore sandbox "store"
    let sourceRoot = sandboxRoot sandbox </> "source"
        sourceDir = sourceRoot </> "src"
        baseVersion = maybe [] bootLibraryVersion (lookupBootLibrary "base" emulatedGhc)
        (major, minor) = case baseVersion of
          a : b : _ -> (a, b)
          _ -> error "the emulated release has no base version"
        guardOn a b = "MIN_VERSION_base(" <> show a <> "," <> show b <> ",0)"
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
            "  build-depends: base",
            "  default-language: Haskell2010",
            "  default-extensions: CPP"
          ]
      )
    writeFile
      (sourceDir </> "Demo.hs")
      ( unlines
          [ "module Demo (current) where",
            "#if " <> guardOn major minor <> " && !" <> guardOn major (minor + 1) <> " && !MIN_VERSION_GLASGOW_HASKELL(99,0,0,0)",
            "current = ()",
            "#else",
            "this is not haskell (",
            "#endif"
          ]
      )
    target <- hostBackendTarget
    result <- install (InstallOptions sourceRoot (Just storeRoot) (Just (sandboxRoot sandbox </> "build")) False False False False False False False O0 False False False False target)
    assertEqual "written modules" ["Demo"] (installWrittenModules result)

-- A module that includes an RTS header by its own name resolves it out of
-- the compiler's include directory. @unix@ does exactly this: the module
-- @System.Posix.Signals.hsc@ generates carries an @#include "rts/Signals.h"@
-- and then names the action codes that header defines, so the include has to
-- resolve through the subdirectory and the macros have to reach the Haskell
-- that follows.
test_installRtsHeaderInclude :: IO SeedStore -> Assertion
test_installRtsHeaderInclude getStore =
  withSandbox getStore "aihc-install-rts-header-include" $ \sandbox -> do
    storeRoot <- sandboxStore sandbox "store"
    let sourceRoot = sandboxRoot sandbox </> "source"
        sourceDir = sourceRoot </> "src"
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
            "  build-depends: base",
            "  default-language: Haskell2010",
            "  default-extensions: CPP"
          ]
      )
    writeFile
      (sourceDir </> "Demo.hs")
      ( unlines
          [ "module Demo (defaultAction) where",
            "#include \"rts/Signals.h\"",
            "defaultAction :: Int",
            "defaultAction = STG_SIG_DFL"
          ]
      )
    target <- hostBackendTarget
    result <- install (InstallOptions sourceRoot (Just storeRoot) (Just (sandboxRoot sandbox </> "build")) False False False False False False False O0 False False False False target)
    assertEqual "written modules" ["Demo"] (installWrittenModules result)

-- | The runtime is an installed package that aihc-prim depends on, so the
-- seed of aihc-prim brings it along for every target. Its C sources and
-- Lir units are compiled into its @cbits@ directory, which a link takes
-- object by object, and a Lir unit of constants alone produces no object.
test_installRuntimePackage :: IO SeedStore -> Assertion
test_installRuntimePackage getStore = do
  targets <- installTestTargets
  withSandbox getStore "aihc-rts-install" $ \sandbox -> do
    storeRoot <- sandboxStore sandbox "store"
    forM_ targets $ \target -> do
      rtsPackage <- seededPackagePath storeRoot target "aihc-rts"
      manifest <- either assertFailure pure =<< readPackageManifest (packageManifestPath rtsPackage)
      assertEqual "the runtime has no modules" [] (packageManifestModules manifest)
      assertFileExists (rtsPackage </> "lib" </> "libaihc-rts.a")
      let cbits = rtsPackage </> "cbits"
          hostObject = case target of
            Wasm32Wasip3 -> "native_aihc_host_wasip3.o"
            _ -> "native_aihc_host_posix.o"
      forM_ ["native_aihc_runtime.o", "native_aihc_gc_semispace.o", hostObject, "native_aihc_helpers.o", "native_aihc_enter.o", "native_aihc_array.o"] $ \object ->
        assertFileExists (cbits </> object)
      assertFileDoesNotExist (cbits </> "native_aihc_constants.o")
      case target of
        Wasm32Wasip3 -> do
          assertFileExists (cbits </> "wasm_aihc_wasip3.o")
          assertFileExists (cbits </> "wasm_generated_command.o")
        _ -> assertFileDoesNotExist (cbits </> "wasm_aihc_wasip3.o")

-- Every standin under core-libs claims the version of the boot library it
-- replaces, and the emulated release is the single source of that version.
test_coreLibsMatchRelease :: Assertion
test_coreLibsMatchRelease =
  forM_ coreProviders $ \provider -> do
    sourcePath <- coreProviderSourcePath provider
    cabalFiles <- filter ((== ".cabal") . takeExtension) <$> listDirectory sourcePath
    cabalFile <- case cabalFiles of
      [file] -> pure (sourcePath </> file)
      _ -> assertFailure ("expected one .cabal file under " <> sourcePath)
    contents <- readFile cabalFile
    let declared = [dropWhile isSpace rest | line <- lines contents, Just rest <- [stripPrefix "version:" line]]
    assertEqual (coreProviderName provider <> " version") [coreProviderVersion provider] declared

-- | A dependency file is one make rule, so a name in it may be split across
-- lines and may hold escaped spaces, colons and dollars.
test_parseDependencyFile :: Assertion
test_parseDependencyFile = do
  assertEqual
    "a rule over several lines"
    ["stub.c", "/usr/include/stdio.h", "/usr/include/stdlib.h"]
    ( parseDependencyFile
        (unlines ["stub.o: stub.c \\", "  /usr/include/stdio.h \\", "  /usr/include/stdlib.h"])
    )
  assertEqual
    "escaped separators inside names"
    ["a b.h", "c:d.h", "e$f.h"]
    (parseDependencyFile "stub.o: a\\ b.h c\\:d.h e$$f.h\n")
  assertEqual "a rule with no prerequisites" [] (parseDependencyFile "stub.o:\n")
  assertEqual "text that is no rule at all" [] (parseDependencyFile "")

-- | A @capi@ import reaches its entity through the C API of a header, so the
-- compiler writes a C wrapper for it, compiles that beside the module object
-- and archives the two together.  The entities here have no symbol of their
-- own at all: one is a macro and the other a @static inline@ function.
--
-- The headers a wrapper included are recorded, so editing one rebuilds the
-- module even though no Haskell source changed.
--
-- This installs for the LLVM target rather than a named one.  A wrapper
-- includes headers, and a host has only the headers of its own platform
-- unless it was given a cross SDK, which this check is not; the LLVM target
-- compiles C for whatever host runs the test.
test_installCapi :: IO SeedStore -> Assertion
test_installCapi getStore =
  withSandbox getStore "aihc-install-capi" $ \sandbox -> do
    storeRoot <- sandboxStore sandbox "store"
    let sourceRoot = sandboxRoot sandbox </> "source"
        sourceDir = sourceRoot </> "src"
        includeDir = sourceRoot </> "include"
        header = includeDir </> "demo_capi.h"
        options = InstallOptions sourceRoot (Just storeRoot) (Just (sandboxRoot sandbox </> "build")) False False False False False False False O0 False False False False Llvm
    createDirectoryIfMissing True sourceDir
    createDirectoryIfMissing True includeDir
    writeFile
      (sourceRoot </> "demo.cabal")
      ( unlines
          [ "cabal-version: 3.0",
            "name: demo",
            "version: 0.1.0.0",
            "library",
            "  exposed-modules: Demo",
            "  hs-source-dirs: src",
            "  include-dirs: include",
            "  default-language: Haskell2010",
            "  default-extensions: CApiFFI, MagicHash"
          ]
      )
    let headerText answer =
          unlines
            [ -- A package header can include the configuration headers of
              -- the compiler.  The compile of a wrapper thus searches the
              -- include directory of the runtime.  A C source of the package
              -- searches the same directory.
              "#include <ghcautoconf.h>",
              "#if !defined(SIZEOF_VOID_P)",
              "#error ghcautoconf.h must define the word size",
              "#endif",
              "#define DEMO_ANSWER " <> show (answer :: Int),
              "static inline int demo_double(int value) { return value * 2; }"
            ]
    writeFile header (headerText 42)
    writeFile
      (sourceDir </> "Demo.hs")
      ( unlines
          [ "module Demo where",
            "import GHC.Prim (Addr#, Int32#)",
            "data Int32 = I32# Int32#",
            "foreign import capi unsafe \"demo_capi.h value DEMO_ANSWER\" answer :: Int32",
            "foreign import capi unsafe \"demo_capi.h demo_double\" double :: Int32 -> Int32",
            -- An address import names a symbol the linker resolves, so it
            -- goes through no wrapper, which is what GHC does for capi \"&x\".
            "foreign import capi unsafe \"demo_capi.h &demo_data\" demoData :: Addr#"
          ]
      )
    first <- install options
    let packageRoot = installStorePath first
        stubSource = packageRoot </> "Demo" </> "Demo.capi.c"
    assertFileExists stubSource
    stub <- readFile stubSource
    assertBool "the wrapper includes the header of its entity" ("#include \"demo_capi.h\"" `isInfixOf` stub)
    assertBool "the value wrapper reads the macro" ("return DEMO_ANSWER;" `isInfixOf` stub)
    assertBool "the call wrapper calls the inline function" ("demo_double(a1)" `isInfixOf` stub)
    assertBool "the address import goes through no wrapper" (not ("demo_data" `isInfixOf` stub))
    assertEqual "one wrapper for each import that needs one" 2 (length (filter ("aihc_capi_" `isPrefixOf`) (words stub)))
    let archivePath = packageRoot </> "lib" </> "libdemo.a"
    members <- filter (not . ("__.SYMDEF" `isPrefixOf`)) . lines <$> readProcess "ar" ["-t", archivePath] ""
    assertEqual "archive members" ["Demo.capi.o", "Demo.o"] (sort members)
    symbols <- readProcess "nm" [archivePath] ""
    -- The wrapper name carries the package, the module and the Haskell name
    -- of the import, so that nothing else linked in can spell the same symbol.
    assertBool "the archive defines the value wrapper" ("aihc_capi_demo_m0_d1_d0_d0_Demo_answer" `isInfixOf` symbols)
    assertBool "the archive defines the call wrapper" ("aihc_capi_demo_m0_d1_d0_d0_Demo_double" `isInfixOf` symbols)
    unchanged <- install options
    assertEqual "an unchanged package rebuilds nothing" [] (installWrittenModules unchanged)
    -- The Haskell source is untouched, so only the recorded headers can tell
    -- the build that the wrapper is out of date.
    writeFile header (headerText 43)
    changed <- install options
    assertEqual "a changed header rebuilds the module" ["Demo"] (installWrittenModules changed)
    rebuilt <- readFile stubSource
    assertBool "the wrapper still reads the macro" ("return DEMO_ANSWER;" `isInfixOf` rebuilt)
    settled <- install options
    assertEqual "the rebuilt module is reusable again" [] (installWrittenModules settled)

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
