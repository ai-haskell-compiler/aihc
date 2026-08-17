{-# LANGUAGE OverloadedStrings #-}

-- | Golden tests for System FC 2 desugaring.
module Fc2Golden
  ( ExpectedStatus (..),
    Outcome (..),
    Fc2Case (..),
    fixtureRoot,
    loadFc2Cases,
    evaluateFc2Case,
  )
where

import Aihc.Fc.Desugar (DesugarConfig (..))
import Aihc.Fc2 (Fc2DesugarResult (..), desugarModuleFc2, parseProgram, renderParseError, renderProgram)
import Aihc.Parser (ParserConfig (..), defaultConfig, parseModule)
import Aihc.Parser.Syntax (Extension, moduleName, parseExtensionName)
import Aihc.Resolve (Package (..), PackageId (..), ResolveResult (..), resolveWithDeps)
import Aihc.Tc (emptyTcInterface, tcModuleBindings, tcModuleDiagnostics, tcModuleSuccess, typecheckModulesWithInterface)
import Data.Aeson ((.!=), (.:), (.:?))
import Data.Aeson.Types (parseEither, withArray, withObject)
import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd, sort)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml qualified as Y
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, (</>))

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

data Fc2Case = Fc2Case
  { caseId :: !String,
    casePath :: !FilePath,
    caseExtensions :: ![Extension],
    caseSupportModules :: ![Text],
    caseModules :: ![Text],
    caseExpected :: !String,
    caseStatus :: !ExpectedStatus,
    caseReason :: !String
  }
  deriving (Eq, Show)

fixtureRoot :: FilePath
fixtureRoot = "test/Test/Fixtures/golden-v2"

loadFc2Cases :: IO [Fc2Case]
loadFc2Cases = do
  exists <- doesDirectoryExist fixtureRoot
  if not exists
    then pure []
    else do
      paths <- listFixtureFiles fixtureRoot
      mapM (loadFc2Case [listSupportModule]) paths

listSupportModule :: Text
listSupportModule =
  T.unlines
    [ "module GHC.Types (Bool(..), Levity(..), List(..), RuntimeRep(..), Type, TYPE) where",
      "data Bool = False | True",
      "data Levity = Lifted | Unlifted",
      "data List a = [] | a : [a]",
      "data RuntimeRep = BoxedRep Levity",
      "data Type",
      "data TYPE rep",
      "infixr 5 :"
    ]

loadFc2Case :: [Text] -> FilePath -> IO Fc2Case
loadFc2Case supportModules path = do
  raw <- Y.decodeFileEither path
  case raw of
    Left err -> fail ("Invalid YAML fixture " <> path <> ": " <> Y.prettyPrintParseException err)
    Right value -> case parseFc2Fixture supportModules path value of
      Left e -> fail e
      Right c -> pure c

parseFc2Fixture :: [Text] -> FilePath -> Y.Value -> Either String Fc2Case
parseFc2Fixture supportModules path value = do
  (extNames, modules, expectedText, statusText, reasonText) <-
    parseEither
      ( withObject "fc2 fixture" $ \obj -> do
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
    Fc2Case
      { caseId = relPath,
        casePath = relPath,
        caseExtensions = exts,
        caseSupportModules = supportModules,
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

evaluateFc2Case :: Fc2Case -> (Outcome, String)
evaluateFc2Case tc =
  case renderFc2Case tc of
    Left details -> classifyFailure tc details
    Right actual -> classifySuccess tc actual

renderFc2Case :: Fc2Case -> Either String String
renderFc2Case tc =
  let supportModuleCount = length supportModules
      parsedModules = map parseOne (supportModules <> caseModules tc)
   in case sequence parsedModules of
        Left errMsg -> Left ("parse error: " <> errMsg)
        Right modules ->
          case resolveWithDeps mempty (zipWith modulePackage [0 :: Int ..] modules) of
            ResolveResult {resolvedModules, resolveErrors = []} ->
              let moduleAsts = map snd resolvedModules
                  (tcResults, tcInterface) = typecheckModulesWithInterface emptyTcInterface moduleAsts
               in if all tcModuleSuccess tcResults
                    then
                      let allBindings = concatMap tcModuleBindings tcResults
                          results =
                            map
                              (desugarModuleFc2 (DesugarConfig {primPackageId = PackageId "aihc-prim"}) allBindings tcInterface)
                              tcResults
                          fixtureResults = drop supportModuleCount results
                       in if all ds2Success results
                            then renderResults fixtureResults
                            else Left (unlines (concatMap ds2Errors results))
                    else Left ("typecheck error: " <> unlines [show d | r <- tcResults, d <- tcModuleDiagnostics r])
            ResolveResult {resolveErrors} ->
              Left ("resolve error: " <> show resolveErrors)
  where
    hasFixtureGhcTypes = any (T.isPrefixOf "module GHC.Types" . T.stripStart) (caseModules tc)
    supportModules = if hasFixtureGhcTypes then [] else caseSupportModules tc
    modulePackage _ modu
      | moduleName modu `elem` [Just "GHC.Classes", Just "GHC.Prim", Just "GHC.Types"] =
          (Package "aihc-prim" (PackageId "aihc-prim"), modu)
      | otherwise = (Package "" (PackageId ""), modu)
    parseOne input =
      let config =
            defaultConfig
              { parserSourceName = T.unpack (T.takeWhile (/= '\n') input),
                parserExtensions = caseExtensions tc
              }
          (errs, ast) = parseModule config input
       in if null errs
            then Right ast
            else Left (show errs)
    renderResults results =
      unlines <$> traverse renderResult results
    renderResult result =
      let rendered = renderProgram (ds2Program result)
       in case parseProgram (T.pack rendered) of
            Left parseError -> Left ("System FC 2 round-trip parse error:\n" <> renderParseError parseError <> "\n" <> rendered)
            Right parsed ->
              let canonical = renderProgram parsed
               in if canonical == rendered
                    then Right rendered
                    else Left ("System FC 2 round trip changed canonical syntax:\n" <> canonical <> "\noriginal:\n" <> rendered)

classifySuccess :: Fc2Case -> String -> (Outcome, String)
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

classifyFailure :: Fc2Case -> String -> (Outcome, String)
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
