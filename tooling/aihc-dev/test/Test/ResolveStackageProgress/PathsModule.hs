{-# LANGUAGE OverloadedStrings #-}

module Test.ResolveStackageProgress.PathsModule
  ( resolveStackagePathsModuleTests,
  )
where

import Aihc.Hackage.Cabal qualified as HC
import Aihc.Hackage.Util (chooseBestCabalFile, findCabalFiles)
import Aihc.Name (Namespace (..), defaultPackageId, globalName, moduleId)
import Aihc.Name qualified as CompilerName
import Aihc.Parser.Syntax qualified as Syntax
import Aihc.Resolve (ModuleExports, ResolveResult (..), ResolvedName (..), Scope (..), extractInterface, resolveWithDeps)
import Control.Exception (bracket)
import Data.Aeson (Value, encode, object, (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
import ResolveStackageProgress qualified as Progress
import System.Directory (createDirectory, createDirectoryIfMissing, getTemporaryDirectory, removeDirectoryRecursive, removeFile)
import System.FilePath (takeDirectory, (</>))
import System.IO (hClose, openTempFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)

resolveStackagePathsModuleTests :: TestTree
resolveStackagePathsModuleTests =
  testGroup
    "resolve stackage generated Paths modules"
    [ testCase "extracts an interface for a no-dependency package using Paths_pkg" test_extractsInterfaceForGeneratedPathsPackage
    ]

test_extractsInterfaceForGeneratedPathsPackage :: Assertion
test_extractsInterfaceForGeneratedPathsPackage =
  withTempDir "aihc-dev-paths" $ \root -> do
    let cabalFile = root </> "paths-demo.cabal"
        srcDir = root </> "src"
        sourceFile = srcDir </> "PathsUser.hs"
        ifaceFile = root </> "interface.json"
    createDirectoryIfMissing True srcDir
    writeFile cabalFile pathsDemoCabal
    writeFile sourceFile pathsUserSource

    files <- findTargetFiles root
    modules <- mapM (parseFileInfo root) files
    let result = resolveWithDeps baseExports modules
    assertEqual "expected generated Paths package to resolve cleanly" [] (resolveErrors result)

    let iface = extractInterface result
    BL.writeFile ifaceFile (encode (interfaceJson iface))
    written <- BL.readFile ifaceFile
    assertBool "expected interface JSON to be written" (not (BL.null written))

    assertBool "expected user module in interface" (Map.member (moduleId defaultPackageId "PathsUser") iface)
    case Map.lookup (moduleId defaultPackageId "Paths_paths_demo") iface of
      Nothing -> assertFailure "expected generated Paths module in interface"
      Just pathsScope -> do
        assertBool "expected version export" (Map.member "version" (scopeTerms pathsScope))
        assertBool "expected getDataDir export" (Map.member "getDataDir" (scopeTerms pathsScope))
        assertBool "expected getDataFileName export" (Map.member "getDataFileName" (scopeTerms pathsScope))

findTargetFiles :: FilePath -> IO [HC.FileInfo]
findTargetFiles packageRoot = do
  cabalFiles <- findCabalFiles packageRoot
  cabalFile <-
    case cabalFiles of
      [] -> ioError (userError "expected a generated test Cabal file")
      [file] -> pure file
      files -> pure (chooseBestCabalFile packageRoot files)
  cabalBytes <- BS.readFile cabalFile
  case snd (runParseResult (parseGenericPackageDescription cabalBytes)) of
    Left (_, errors) -> ioError (userError (show errors))
    Right packageDescription -> HC.collectComponentFiles packageDescription (takeDirectory cabalFile)

parseFileInfo :: FilePath -> HC.FileInfo -> IO Syntax.Module
parseFileInfo packageRoot info = do
  result <- Progress.parseFileInfo packageRoot info
  case result of
    Left err -> assertFailure err
    Right (modu, _) -> pure modu

baseExports :: ModuleExports
baseExports =
  Map.fromList
    [ (moduleId defaultPackageId "GHC.Classes", mkScope "GHC.Classes" ["=="] []),
      (moduleId defaultPackageId "GHC.Num", mkScope "GHC.Num" ["fromInteger"] []),
      (moduleId defaultPackageId "Prelude", mkScope "Prelude" ["return", "++", "==", "otherwise", "fromInteger"] ["IO", "FilePath", "String", "Char", "Bool"]),
      (moduleId defaultPackageId "Control.Exception", mkScope "Control.Exception" ["catch"] ["IOException"]),
      (moduleId defaultPackageId "Data.List", mkScope "Data.List" ["last"] []),
      (moduleId defaultPackageId "Data.Version", mkScope "Data.Version" ["Version"] ["Version"]),
      (moduleId defaultPackageId "System.Environment", mkScope "System.Environment" ["getEnv"] [])
    ]

mkScope :: Text -> [Text] -> [Text] -> Scope
mkScope moduleName terms types =
  Scope
    { scopeTerms = Map.fromList [(name, resolve TermNamespace name) | name <- terms],
      scopeTypes = Map.fromList [(name, resolve TypeNamespace name) | name <- types],
      scopeConstructors = Map.empty,
      scopeRecordFields = Map.empty,
      scopeMethods = Map.empty,
      scopeFixities = Map.empty,
      scopeQualifiedModules = Map.empty
    }
  where
    resolve namespace name =
      ResolvedTopLevel (globalName (moduleId defaultPackageId moduleName) namespace name)

interfaceJson :: ModuleExports -> Value
interfaceJson iface =
  object
    [ "modules"
        .= [ object
               [ "module" .= moduleName,
                 "terms" .= Map.keys (scopeTerms scope),
                 "types" .= Map.keys (scopeTypes scope)
               ]
           | (owner, scope) <- Map.toList iface,
             let moduleName = CompilerName.unModuleName (CompilerName.moduleName owner)
           ]
    ]

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
