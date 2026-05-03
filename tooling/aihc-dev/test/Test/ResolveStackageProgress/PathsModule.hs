{-# LANGUAGE OverloadedStrings #-}

module Test.ResolveStackageProgress.PathsModule
  ( resolveStackagePathsModuleTests,
  )
where

import Aihc.Cpp (Result (..))
import Aihc.Parser (ParserConfig (..), defaultConfig, formatParseErrors, parseModule)
import Aihc.Parser.Syntax
  ( LanguageEdition (..),
    NameType (..),
    headerExtensionSettings,
    headerLanguageEdition,
    mkQualifiedName,
    mkUnqualifiedName,
  )
import Aihc.Parser.Syntax qualified as Syntax
import Aihc.Resolve (ModuleExports, ResolveResult (..), ResolvedName (..), Scope (..), extractInterface, resolveWithDeps)
import Control.Exception (bracket)
import CppSupport (moduleHeaderPragmas, preprocessForParserIfEnabled)
import Data.Aeson (Value, encode, object, (.=))
import Data.ByteString.Lazy qualified as BL
import Data.Char (isAlphaNum, isUpper)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import HackageSupport
  ( FileInfo (..),
    findTargetFilesFromCabal,
    readTextFileLenient,
    resolveIncludeBestEffort,
  )
import System.Directory (createDirectory, createDirectoryIfMissing, getTemporaryDirectory, removeDirectoryRecursive, removeFile)
import System.FilePath ((</>))
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

    files <- findTargetFilesFromCabal root
    modules <- mapM (parseFileInfo root) files
    let result = resolveWithDeps baseExports modules
    assertEqual "expected generated Paths package to resolve cleanly" [] (resolveErrors result)

    let iface = extractInterface result
    BL.writeFile ifaceFile (encode (interfaceJson iface))
    written <- BL.readFile ifaceFile
    assertBool "expected interface JSON to be written" (not (BL.null written))

    assertBool "expected user module in interface" (Map.member "PathsUser" iface)
    case Map.lookup "Paths_paths_demo" iface of
      Nothing -> assertFailure "expected generated Paths module in interface"
      Just pathsScope -> do
        assertBool "expected version export" (Map.member "version" (scopeTerms pathsScope))
        assertBool "expected getDataDir export" (Map.member "getDataDir" (scopeTerms pathsScope))
        assertBool "expected getDataFileName export" (Map.member "getDataFileName" (scopeTerms pathsScope))

parseFileInfo :: FilePath -> FileInfo -> IO Syntax.Module
parseFileInfo packageRoot info = do
  let file = fileInfoPath info
  source <- readTextFileLenient file
  Result {resultOutput = source'} <-
    preprocessForParserIfEnabled
      (fileInfoExtensions info)
      (fileInfoCppOptions info)
      file
      (fileInfoDependencies info)
      (resolveIncludeBestEffort packageRoot (fileInfoIncludeDirs info) file)
      source
  let headerPragmas = moduleHeaderPragmas source'
      defaultEdition = fromMaybe Haskell98Edition (fileInfoLanguage info)
      edition = fromMaybe defaultEdition (headerLanguageEdition headerPragmas)
      extensionSettings = fileInfoExtensions info ++ headerExtensionSettings headerPragmas
      effectiveExts = Syntax.effectiveExtensions edition extensionSettings
      config = defaultConfig {parserSourceName = file, parserExtensions = effectiveExts}
      (errs, modu) = parseModule config source'
  if null errs
    then pure modu
    else assertFailure (formatParseErrors file (Just source') errs)

baseExports :: ModuleExports
baseExports =
  Map.fromList
    [ ("Prelude", mkScope "Prelude" ["return", "++", "==", "otherwise"] ["IO", "FilePath", "String", "Char", "Bool"]),
      ("Control.Exception", mkScope "Control.Exception" ["catch"] ["IOException"]),
      ("Data.List", mkScope "Data.List" ["last"] []),
      ("Data.Version", mkScope "Data.Version" ["Version"] ["Version"]),
      ("System.Environment", mkScope "System.Environment" ["getEnv"] [])
    ]

mkScope :: Text -> [Text] -> [Text] -> Scope
mkScope moduleName terms types =
  Scope
    { scopeTerms = Map.fromList [(name, resolve name) | name <- terms],
      scopeTypes = Map.fromList [(name, resolve name) | name <- types],
      scopeQualifiedModules = Map.empty
    }
  where
    resolve name =
      ResolvedTopLevel (mkQualifiedName (mkUnqualifiedName (inferNameType name) name) (Just moduleName))

inferNameType :: Text -> NameType
inferNameType name =
  case T.uncons name of
    Nothing -> NameVarId
    Just (c, _)
      | c == ':' -> NameConSym
      | not (isAlphaNum c) && c /= '_' && c /= '\'' -> NameVarSym
      | isUpper c -> NameConId
      | otherwise -> NameVarId

interfaceJson :: ModuleExports -> Value
interfaceJson iface =
  object
    [ "modules"
        .= [ object
               [ "module" .= moduleName,
                 "terms" .= Map.keys (scopeTerms scope),
                 "types" .= Map.keys (scopeTypes scope)
               ]
           | (moduleName, scope) <- Map.toList iface
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
