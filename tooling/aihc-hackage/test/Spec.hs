module Main (main) where

import Aihc.Hackage.Cabal qualified as HC
import Aihc.Hackage.Stackage (parseSnapshotConstraints)
import Aihc.Hackage.Types (PackageSpec (..))
import Control.Exception (bracket)
import Data.ByteString qualified as BS
import Data.List (isInfixOf, isSuffixOf)
import Data.Text qualified as T
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
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
      testCase "generates Cabal Paths module as a normal source file" test_generatesPathsModule,
      testCase "collects exposed modules from active conditional library branches" test_collectsConditionalExposedModules,
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
