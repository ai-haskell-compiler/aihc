{-# LANGUAGE OverloadedStrings #-}

-- | Golden tests for System FC desugaring.
module FcGolden
  ( ExpectedStatus (..),
    Outcome (..),
    FcCase (..),
    fixtureRoot,
    loadFcCases,
    evaluateFcCase,
  )
where

import Aihc.Fc (DesugarConfig (..), FcDesugarResult (..), desugarModuleFc, desugarPrepared, lintProgram, parseProgram, prepareDesugar, renderParseError, renderProgram)
import Aihc.Parser (ParserConfig (..), defaultConfig, parseModule)
import Aihc.Parser.Syntax
  ( Extension (ImplicitPrelude),
    LanguageEdition (Haskell98Edition),
    Module,
    effectiveExtensions,
    headerExtensionSettings,
    headerLanguageEdition,
    moduleName,
    parseExtensionName,
    parseLanguageEdition,
  )
import Aihc.Parser.Token (readModuleHeaderPragmas)
import Aihc.Resolve (ModuleExports, ModuleKey (..), Package (..), PackageId (..), ResolveResult (..), Scope, collectModuleExportsWithDeps, emptyScope, extractInterface, lookupImportedModule, modulesInPackage, resolveWithDeps, unionScope)
import Aihc.Tc
  ( TcInterface,
    emptyTcInterface,
    restrictTcInterfaceToModules,
    tcConfig,
    tcModuleBindings,
    tcModuleDiagnostics,
    tcModuleSuccess,
    typecheckModuleSccWithInterface,
    typecheckModulesWithInterface,
  )
import Data.Aeson ((.!=), (.:), (.:?))
import Data.Aeson.Types (parseEither, withArray, withObject)
import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd, sort)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Yaml qualified as Y
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, listDirectory)
import System.FilePath (takeDirectory, takeExtension, (</>))
import System.IO.Unsafe (unsafePerformIO)

data ExpectedStatus
  = StatusPass
  | StatusFail
  | StatusXPass
  | StatusXFail
  deriving (Eq, Show)

data Outcome
  = OutcomePass
  | OutcomeXFail
  | OutcomeXPass
  | OutcomeFail
  deriving (Eq, Show)

data FcCase = FcCase
  { caseId :: !String,
    casePath :: !FilePath,
    caseExtensions :: ![Extension],
    caseModules :: ![Text],
    caseExpected :: !String,
    caseStatus :: !ExpectedStatus,
    caseReason :: !String
  }
  deriving (Eq, Show)

data PrimitiveSupport = PrimitiveSupport
  { supportScopes :: !ModuleExports,
    supportTcInterface :: !TcInterface
  }

fixtureRoot :: FilePath
fixtureRoot = "compiler/fc/test/Test/Fixtures/golden"

loadFcCases :: IO [FcCase]
loadFcCases = do
  exists <- doesDirectoryExist fixtureRoot
  if not exists
    then pure []
    else do
      primitiveSupport `seq` pure ()
      paths <- listFixtureFiles fixtureRoot
      mapM loadFcCase paths

primitiveSupport :: PrimitiveSupport
primitiveSupport = unsafePerformIO $ do
  primitiveModules <- loadPrimitiveModules
  case preparePrimitiveSupport primitiveModules of
    Left errMsg -> fail errMsg
    Right support -> pure support
{-# NOINLINE primitiveSupport #-}

loadPrimitiveModules :: IO [(FilePath, Text)]
loadPrimitiveModules = do
  sourceRoot <- findPrimitiveSourceRoot
  mapM
    (loadOne sourceRoot)
    [ "GHC/Base.hs",
      "GHC/Classes.hs",
      "GHC/IO.hs",
      "GHC/Internal/Integer.hs",
      "GHC/Num.hs",
      "GHC/Prim.hs",
      "GHC/Tuple.hs",
      "GHC/Types.hs"
    ]
  where
    loadOne sourceRoot relativePath = do
      let path = sourceRoot </> relativePath
      source <- TIO.readFile path
      pure (path, source)

findPrimitiveSourceRoot :: IO FilePath
findPrimitiveSourceRoot = getCurrentDirectory >>= findUp
  where
    findUp directory = do
      let candidate = directory </> "core-libs/aihc-prim/src"
          files =
            [ candidate </> "GHC/Base.hs",
              candidate </> "GHC/Classes.hs",
              candidate </> "GHC/IO.hs",
              candidate </> "GHC/Internal/Integer.hs",
              candidate </> "GHC/Num.hs",
              candidate </> "GHC/Prim.hs",
              candidate </> "GHC/Tuple.hs",
              candidate </> "GHC/Types.hs"
            ]
      exists <- and <$> mapM doesFileExist files
      if exists
        then pure candidate
        else do
          let parent = takeDirectory directory
          if parent == directory
            then fail "Cannot find the aihc-prim source modules."
            else findUp parent

loadFcCase :: FilePath -> IO FcCase
loadFcCase path = do
  raw <- Y.decodeFileEither path
  case raw of
    Left err -> fail ("Invalid YAML fixture " <> path <> ": " <> Y.prettyPrintParseException err)
    Right value -> case parseFcFixture path value of
      Left e -> fail e
      Right c -> pure c

parseFcFixture :: FilePath -> Y.Value -> Either String FcCase
parseFcFixture path value = do
  (extNames, modules, expectedText, statusText, reasonText) <-
    parseEither
      ( withObject "fc fixture" $ \obj -> do
          exts <- obj .: "extensions"
          mods <- obj .: "modules" >>= parseModules
          expected <- (obj .:? "expected" >>= traverse parseExpectedValue) .!= ""
          status <- obj .: "status"
          reason <- obj .:? "reason" .!= ""
          pure (exts, mods, expected, status, reason)
      )
      value
  exts <- validateExtensions path extNames
  status <- parseStatus path statusText
  let relPath = dropRootPrefix path
      expected = trim (T.unpack expectedText)
      reason = trim (T.unpack reasonText)
  pure
    FcCase
      { caseId = relPath,
        casePath = relPath,
        caseExtensions = exts,
        caseModules = modules,
        caseExpected = expected,
        caseStatus = status,
        caseReason = reason
      }

parseModules :: Y.Value -> Y.Parser [Text]
parseModules = withArray "modules" $ \arr ->
  mapM parseModuleEntry (foldr (:) [] arr)
  where
    parseModuleEntry (Y.String t) = pure t
    parseModuleEntry _ = fail "each module must be a string"

parseExpectedValue :: Y.Value -> Y.Parser Text
parseExpectedValue (Y.String txt) = pure txt
parseExpectedValue (Y.Array arr) = T.intercalate "\n" <$> mapM parseLine (foldr (:) [] arr)
  where
    parseLine (Y.String t) = pure t
    parseLine _ = fail "each expected line must be a string"
parseExpectedValue _ = fail "expected must be a string or list"

evaluateFcCase :: FcCase -> (Outcome, String)
evaluateFcCase tc =
  case renderFcCase tc of
    Left details -> classifyFailure tc details
    Right actual -> classifySuccess tc actual

renderFcCase :: FcCase -> Either String String
renderFcCase tc =
  let parsedModules = map parseFixtureModule (caseModules tc)
   in case sequence parsedModules of
        Left errMsg -> Left ("parse error: " <> errMsg)
        Right modules ->
          case resolveWithDeps (fixtureBuiltinScope modules) (fixtureDependencyExports modules) (modulesInPackage fixturePackage modules) of
            ResolveResult {resolvedModules, resolveErrors = []} ->
              let fixtureAsts = map snd resolvedModules
                  primitiveInterface = fixturePrimitiveInterface modules
                  (fixtureTcResults, tcInterface) = typecheckModulesWithInterface (tcConfig (primPackageId desugarConfig)) primitiveInterface fixtureAsts
               in if all tcModuleSuccess fixtureTcResults
                    then do
                      env <- prepareDesugar desugarConfig (primitiveInterface <> tcInterface)
                      let fixtureResults = map (\checked -> desugarPrepared env (tcModuleBindings checked) checked) fixtureTcResults
                      if all dsSuccess fixtureResults
                        then lintAndRenderResults fixtureResults
                        else Left (unlines (concatMap dsErrors fixtureResults))
                    else Left ("typecheck error: " <> unlines [show d | r <- fixtureTcResults, d <- tcModuleDiagnostics r])
            ResolveResult {resolveErrors} ->
              Left ("resolve error: " <> show resolveErrors)
  where
    parseFixtureModule input =
      parseModuleText (T.unpack (T.takeWhile (/= '\n') input)) (caseExtensions tc) input
    lintAndRenderResults fixtureResults =
      case renderResults fixtureResults of
        Left renderError -> Left renderError
        Right rendered ->
          case concatMap (lintProgram . dsProgram) fixtureResults of
            [] -> Right rendered
            lintErrors ->
              Left
                ( unlines ["System FC lint error: " <> show lintError | lintError <- lintErrors]
                    <> "\nSystem FC output:\n"
                    <> rendered
                )
    renderResults results =
      unlines <$> traverse renderResult results
    renderResult result =
      let rendered = renderProgram (dsProgram result)
       in case parseProgram rendered of
            Left parseError -> Left ("System FC round-trip parse error:\n" <> renderParseError parseError <> "\n" <> T.unpack rendered)
            Right parsed ->
              let canonical = renderProgram parsed
               in if canonical == rendered
                    then Right (T.unpack rendered)
                    else Left ("System FC round trip changed canonical syntax:\n" <> T.unpack canonical <> "\noriginal:\n" <> T.unpack rendered)

preparePrimitiveSupport :: [(FilePath, Text)] -> Either String PrimitiveSupport
preparePrimitiveSupport primitiveModules =
  case mapM (uncurry parsePrimitiveModule) primitiveModules of
    Left errMsg -> Left ("parse error: " <> errMsg)
    Right modules ->
      case resolveWithDeps emptyScope mempty (modulesInPackage primitivePackage modules) of
        resolved@ResolveResult {resolvedModules, resolveErrors = []} ->
          let primitiveAsts = map snd resolvedModules
              (primitiveTcResults, tcInterface) = typecheckModuleSccWithInterface (tcConfig (primPackageId desugarConfig)) emptyTcInterface primitiveAsts
           in if all tcModuleSuccess primitiveTcResults
                then
                  let primitiveBindings = concatMap tcModuleBindings primitiveTcResults
                      primitiveResults = map (desugarModuleFc desugarConfig primitiveBindings tcInterface) primitiveTcResults
                   in if all dsSuccess primitiveResults
                        then
                          Right
                            PrimitiveSupport
                              { supportScopes = extractInterface resolved,
                                supportTcInterface = tcInterface
                              }
                        else Left (unlines (concatMap dsErrors primitiveResults))
                else Left ("typecheck error: " <> unlines [show (moduleName ast) <> ": " <> show diagnostic | (ast, result) <- zip primitiveAsts primitiveTcResults, diagnostic <- tcModuleDiagnostics result])
        ResolveResult {resolveErrors} -> Left ("resolve error: " <> show resolveErrors)

primitivePackage :: Package
primitivePackage = Package "aihc-prim" (PackageId "aihc-prim")

fixturePackage :: Package
fixturePackage = Package "" (PackageId "")

fixtureDependencyExports :: [Module] -> ModuleExports
fixtureDependencyExports modules =
  Map.filterWithKey
    (\(ModuleKey _ name) _ -> name `elem` fixturePrimitiveModuleNames modules && Just name `notElem` map moduleName modules)
    (supportScopes primitiveSupport)

fixturePrimitiveInterface :: [Module] -> TcInterface
fixturePrimitiveInterface modules =
  restrictTcInterfaceToModules
    (PackageId "aihc-prim")
    (fixturePrimitiveModuleNames modules)
    (supportTcInterface primitiveSupport)

fixturePrimitiveModuleNames :: [Module] -> [Text]
fixturePrimitiveModuleNames modules = selectedNames
  where
    localNames = mapMaybe moduleName modules
    legacyNames = ["GHC.Classes", "GHC.Prim", "GHC.Tuple", "GHC.Types"]
    allNames = ["GHC.Base", "GHC.Classes", "GHC.IO", "GHC.Internal.Integer", "GHC.Num", "GHC.Prim", "GHC.Tuple", "GHC.Types"]
    selectedNames
      | any (`elem` allNames) localNames = legacyNames
      | otherwise = allNames

fixtureBuiltinScope :: [Module] -> Scope
fixtureBuiltinScope modules =
  foldr (unionScope . lookupBuiltin) emptyScope builtinFunctionModules
  where
    dependencyExports = fixtureDependencyExports modules
    packageModules = modulesInPackage fixturePackage modules
    allExports = collectModuleExportsWithDeps dependencyExports packageModules <> dependencyExports
    lookupBuiltin name = lookupImportedModule fixturePackage Nothing name allExports
    builtinFunctionModules = ["GHC.Base", "GHC.Classes", "GHC.Num"]

desugarConfig :: DesugarConfig
desugarConfig = DesugarConfig {primPackageId = PackageId "aihc-prim"}

parsePrimitiveModule :: FilePath -> Text -> Either String Module
parsePrimitiveModule sourceName input =
  parseModuleText sourceName (primitiveExtensions input) input

parseModuleText :: FilePath -> [Extension] -> Text -> Either String Module
parseModuleText sourceName extensions input =
  let config =
        defaultConfig
          { parserSourceName = sourceName,
            parserExtensions = extensions
          }
      (errs, ast) = parseModule config input
   in if null errs
        then Right ast
        else Left (show errs)

primitiveExtensions :: Text -> [Extension]
primitiveExtensions source =
  filter (/= ImplicitPrelude) (effectiveExtensions language (headerExtensionSettings header))
  where
    header = readModuleHeaderPragmas source
    defaultLanguage = fromMaybe Haskell98Edition (parseLanguageEdition "GHC2021")
    language = fromMaybe defaultLanguage (headerLanguageEdition header)

classifySuccess :: FcCase -> String -> (Outcome, String)
classifySuccess tc actual =
  case caseStatus tc of
    StatusPass
      | trim actual == trim (caseExpected tc) -> (OutcomePass, "")
      | otherwise ->
          ( OutcomeFail,
            "output mismatch\nexpected:\n" <> caseExpected tc <> "\nactual:\n" <> trim actual
          )
    StatusFail -> (OutcomeFail, "expected failure but desugaring succeeded")
    StatusXFail
      | trim actual == trim (caseExpected tc) -> (OutcomeXPass, "")
      | otherwise -> (OutcomeXFail, "")
    StatusXPass
      | trim actual == trim (caseExpected tc) -> (OutcomeXPass, "known bug still passes")
      | otherwise -> (OutcomeFail, "expected xpass output match but got: " <> trim actual)

classifyFailure :: FcCase -> String -> (Outcome, String)
classifyFailure tc errDetails =
  case caseStatus tc of
    StatusPass -> (OutcomeFail, "expected success, got error: " <> errDetails)
    StatusFail -> (OutcomePass, "")
    StatusXFail -> (OutcomeXFail, "")
    StatusXPass -> (OutcomeFail, "expected xpass, got error: " <> errDetails)

listFixtureFiles :: FilePath -> IO [FilePath]
listFixtureFiles dir = do
  entries <- sort <$> listDirectory dir
  concat
    <$> mapM
      ( \entry -> do
          let path = dir </> entry
          isDir <- doesDirectoryExist path
          if isDir
            then listFixtureFiles path
            else
              if takeExtension path `elem` [".yaml", ".yml"]
                then pure [path]
                else pure []
      )
      entries

validateExtensions :: FilePath -> [Text] -> Either String [Extension]
validateExtensions path = traverse parseOne
  where
    parseOne raw =
      case parseExtensionName raw of
        Just ext -> Right ext
        Nothing -> Left ("Unknown extension " <> show raw <> " in " <> path)

parseStatus :: FilePath -> Text -> Either String ExpectedStatus
parseStatus path raw =
  case map toLower (trim (T.unpack raw)) of
    "pass" -> Right StatusPass
    "fail" -> Right StatusFail
    "xpass" -> Right StatusXPass
    "xfail" -> Right StatusXFail
    _ -> Left ("Invalid status in " <> path <> ": " <> T.unpack raw)

dropRootPrefix :: FilePath -> FilePath
dropRootPrefix path =
  maybe path T.unpack (T.stripPrefix (T.pack (fixtureRoot <> "/")) (T.pack path))

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
