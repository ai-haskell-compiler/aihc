module Main (main) where

import Aihc.Hackage.Cabal qualified as HC
import Aihc.Hackage.Index (parseHackageIndex)
import Aihc.Hackage.Stackage (parseSnapshotConstraints)
import Aihc.Hackage.Types (PackageSpec (..))
import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Control.Exception (bracket)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as LBS
import Data.List (isInfixOf, isSuffixOf, sort)
import Data.Text qualified as T
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription)
import System.Directory (createDirectory, createDirectoryIfMissing, getTemporaryDirectory, removeDirectoryRecursive, removeFile)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck qualified as QC

main :: IO ()
main =
  defaultMain . testGroup "aihc-hackage" $
    [ testCase "maps selected installed packages to fixed versions" $ do
        parseSnapshotConstraints "constraints: binary installed, bytestring installed, Cabal installed, unix installed"
          @?= Right
            [ PackageSpec "binary" "0.8.9.3",
              PackageSpec "bytestring" "0.12.2.0",
              PackageSpec "Cabal" "3.14.2.0",
              PackageSpec "unix" "2.8.8.0"
            ],
      testCase "keeps installed for packages without a fixed override" $ do
        parseSnapshotConstraints "constraints: ghc-prim installed, custom-package installed"
          @?= Right
            [ PackageSpec "ghc-prim" "installed",
              PackageSpec "custom-package" "installed"
            ],
      testCase "parses latest package versions from Hackage index tarball" $ do
        parseHackageIndex testHackageIndex
          @?= Right
            [ PackageSpec "alpha" "1.2.0",
              PackageSpec "beta" "0.1"
            ],
      testCase "generates Cabal Paths module as a normal source file" test_generatesPathsModule,
      testCase "collects exposed modules from active conditional library branches" test_collectsConditionalExposedModules,
      testCase "extracts active build tool dependency names" test_extractsBuildToolDependencyNames,
      testCase "detects packages that default to Haskell98" test_detectsHaskell98DefaultLanguage,
      testCase "ignores inactive Haskell98 default-language branches" test_ignoresInactiveHaskell98DefaultLanguage,
      QC.testProperty "dummy quickcheck property" prop_dummy
    ]

-- | Dummy QuickCheck property that always passes.
-- Added so that --quickcheck-tests flag is accepted by the test suite.
prop_dummy :: Bool
prop_dummy = True

(@?=) :: (Eq a, Show a) => Either String a -> Either String a -> Assertion
actual @?= expected =
  if actual == expected
    then pure ()
    else assertFailure ("expected: " <> show expected <> "\n but got: " <> show actual)

testHackageIndex :: LBS.ByteString
testHackageIndex =
  GZip.compress $
    Tar.write
      [ cabalEntry "alpha/1.0.0/alpha.cabal",
        cabalEntry "alpha/1.2.0/alpha.cabal",
        cabalEntry "alpha/1.1.0/alpha.cabal",
        cabalEntry "beta/0.1/beta.cabal",
        cabalEntry "beta/0.2/not-beta.cabal",
        cabalEntry "preferred-versions"
      ]
  where
    cabalEntry path =
      case Tar.toTarPath False path of
        Left err -> error ("invalid test tar path: " <> show err)
        Right tarPath ->
          let contents = LBS.fromStrict (BSC.pack "name: ignored\n")
           in Tar.simpleEntry tarPath (Tar.NormalFile contents (LBS.length contents))

test_generatesPathsModule :: Assertion
test_generatesPathsModule =
  withTempDir "aihc-hackage-paths" $ \root -> do
    let cabalFile = root </> "paths-demo.cabal"
        srcDir = root </> "src"
        sourceFile = srcDir </> "PathsUser.hs"
    createDirectoryIfMissing True srcDir
    writeFile cabalFile pathsDemoCabal
    writeFile sourceFile pathsUserSource

    cabalBytes <- BS.readFile cabalFile
    gpd <-
      case snd (runParseResult (parseGenericPackageDescription cabalBytes)) of
        Right parsed -> pure parsed
        Left (_, errs) -> assertFailure ("failed to parse test cabal file: " <> show errs)

    files <- HC.collectComponentFiles gpd root
    let paths = map HC.fileInfoPath files
        generated = root </> ".aihc-autogen" </> "Paths_paths_demo.hs"
    assertBool "expected package source module to be selected" (any ("src/PathsUser.hs" `isSuffixOf`) paths)
    assertBool "expected generated Paths module to be selected" (generated `elem` paths)

    generatedSource <- readFile generated
    assertBool "expected Cabal module header" ("module Paths_paths_demo" `isInfixOf` generatedSource)
    assertBool "expected version export" ("version :: Version" `isInfixOf` generatedSource)
    assertBool "expected getDataDir export" ("getDataDir" `isInfixOf` generatedSource)
    assertBool "expected getDataFileName export" ("getDataFileName :: FilePath -> IO FilePath" `isInfixOf` generatedSource)

    case filter ((== generated) . HC.fileInfoPath) files of
      [info] -> assertEqual "expected generated module to depend on base" [T.pack "base"] (HC.fileInfoDependencies info)
      _ -> assertFailure "expected exactly one FileInfo for generated Paths module"

test_collectsConditionalExposedModules :: Assertion
test_collectsConditionalExposedModules =
  withTempDir "aihc-hackage-conditional-exposed" $ \root -> do
    let cabalFile = root </> "conditional-exposed.cabal"
        srcDir = root </> "src" </> "Control" </> "Category"
        sourceFile = srcDir </> "Unicode.hs"
    createDirectoryIfMissing True srcDir
    writeFile cabalFile conditionalExposedCabal
    writeFile sourceFile "module Control.Category.Unicode where\n"

    cabalBytes <- BS.readFile cabalFile
    gpd <-
      case snd (runParseResult (parseGenericPackageDescription cabalBytes)) of
        Right parsed -> pure parsed
        Left (_, errs) -> assertFailure ("failed to parse test cabal file: " <> show errs)

    files <- HC.collectComponentFiles gpd root
    let paths = map HC.fileInfoPath files
    assertBool "expected conditionally exposed module to be selected" (any ("src/Control/Category/Unicode.hs" `isSuffixOf`) paths)

test_extractsBuildToolDependencyNames :: Assertion
test_extractsBuildToolDependencyNames = do
  gpd <- parseTestCabal buildToolDependsCabal
  assertEqual
    "expected active modern and legacy build tools"
    (sort ["alex", "genprimopcode"])
    (sort (map T.unpack (HC.buildToolDependencyNames gpd)))

test_detectsHaskell98DefaultLanguage :: Assertion
test_detectsHaskell98DefaultLanguage = do
  explicit <- parseTestCabal haskell98DefaultLanguageCabal
  missing <- parseTestCabal missingDefaultLanguageCabal
  supported <- parseTestCabal haskell2010DefaultLanguageCabal
  assertBool "explicit Haskell98 default-language is unsupported" (HC.packageDefaultsToHaskell98 explicit)
  assertBool "missing default-language falls back to Haskell98" (HC.packageDefaultsToHaskell98 missing)
  assertBool "Haskell2010 default-language is supported" (not (HC.packageDefaultsToHaskell98 supported))

test_ignoresInactiveHaskell98DefaultLanguage :: Assertion
test_ignoresInactiveHaskell98DefaultLanguage = do
  gpd <- parseTestCabal inactiveHaskell98DefaultLanguageCabal
  assertBool "inactive Haskell98 branch should not filter the package" (not (HC.packageDefaultsToHaskell98 gpd))

parseTestCabal :: String -> IO GenericPackageDescription
parseTestCabal source =
  case snd (runParseResult (parseGenericPackageDescription (BSC.pack source))) of
    Right parsed -> pure parsed
    Left (_, errs) -> assertFailure ("failed to parse test cabal file: " <> show errs)

pathsDemoCabal :: String
pathsDemoCabal =
  unlines
    [ "cabal-version: 3.0",
      "name: paths-demo",
      "version: 0.1.0.0",
      "",
      "library",
      "  exposed-modules: PathsUser",
      "  autogen-modules: Paths_paths_demo",
      "  hs-source-dirs: src",
      "  default-language: Haskell2010"
    ]

pathsUserSource :: String
pathsUserSource =
  unlines
    [ "module PathsUser where",
      "import Paths_paths_demo (version, getDataDir, getDataFileName)",
      "pathsVersion = version",
      "pathsDataDir = getDataDir",
      "pathsDataFileName = getDataFileName"
    ]

conditionalExposedCabal :: String
conditionalExposedCabal =
  unlines
    [ "cabal-version: 3.0",
      "name: conditional-exposed",
      "version: 0.1.0.0",
      "",
      "flag old-base",
      "  default: False",
      "  manual: True",
      "",
      "library",
      "  hs-source-dirs: src",
      "  default-language: Haskell2010",
      "  if flag(old-base)",
      "    build-depends: base >= 3.0 && < 3.0.3.1",
      "  else",
      "    exposed-modules: Control.Category.Unicode",
      "    build-depends: base >= 3.0.3.1 && < 5"
    ]

buildToolDependsCabal :: String
buildToolDependsCabal =
  unlines
    [ "cabal-version: 2.4",
      "name: build-tool-demo",
      "version: 0.1.0.0",
      "",
      "flag generated",
      "  default: False",
      "  manual: True",
      "",
      "library",
      "  exposed-modules: BuildToolDemo",
      "  hs-source-dirs: src",
      "  build-tool-depends: genprimopcode:genprimopcode >= 0",
      "  build-tools: alex >= 3",
      "  default-language: Haskell2010",
      "  if flag(generated)",
      "    build-tool-depends: inactive-tool:inactive-tool >= 0"
    ]

haskell98DefaultLanguageCabal :: String
haskell98DefaultLanguageCabal =
  unlines
    [ "cabal-version: 2.4",
      "name: haskell98-language",
      "version: 0.1.0.0",
      "",
      "library",
      "  exposed-modules: Haskell98Language",
      "  hs-source-dirs: src",
      "  default-language: Haskell98"
    ]

missingDefaultLanguageCabal :: String
missingDefaultLanguageCabal =
  unlines
    [ "cabal-version: 1.10",
      "name: missing-language",
      "version: 0.1.0.0",
      "",
      "library",
      "  exposed-modules: MissingLanguage",
      "  hs-source-dirs: src"
    ]

haskell2010DefaultLanguageCabal :: String
haskell2010DefaultLanguageCabal =
  unlines
    [ "cabal-version: 2.4",
      "name: haskell2010-language",
      "version: 0.1.0.0",
      "",
      "library",
      "  exposed-modules: Haskell2010Language",
      "  hs-source-dirs: src",
      "  default-language: Haskell2010"
    ]

inactiveHaskell98DefaultLanguageCabal :: String
inactiveHaskell98DefaultLanguageCabal =
  unlines
    [ "cabal-version: 2.4",
      "name: inactive-haskell98-language",
      "version: 0.1.0.0",
      "",
      "flag legacy",
      "  default: False",
      "  manual: True",
      "",
      "library",
      "  exposed-modules: InactiveHaskell98Language",
      "  hs-source-dirs: src",
      "  default-language: Haskell2010",
      "  if flag(legacy)",
      "    default-language: Haskell98"
    ]

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
