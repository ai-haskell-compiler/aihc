{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | File-level validation checks for Haskell source files.
module StackageProgress.FileChecker
  ( -- * File checking
    FileResult (..),
    PackageFileSummary (..),
    checkFile,
    emptyFileSummary,
    checkAndAccumulateFile,
    foldFilesForPackage,
    firstFailureMessage,
    getPackageFileErrors,

    -- * Check predicates
    needsFullPackageScan,
    needsParsedModule,
    shouldStopAfterFailure,
  )
where

import Aihc.Cpp (Severity (..), diagSeverity, resultDiagnostics, resultOutput)
import Aihc.Parser (ParseResult (..))
import qualified Aihc.Parser
import qualified Aihc.Parser.Syntax as Syntax
import CppSupport (moduleHeaderPragmas, preprocessForParserIfEnabled)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GhcOracle
import HackageSupport
  ( FileInfo (..),
    diagToText,
    prefixCppErrors,
    readTextFileLenient,
    resolveIncludeBestEffort,
  )
import HseExtensions (fromParserExtensions)
import qualified Language.Haskell.Exts as HSE
import ParserValidation (ValidationError (..), ValidationErrorKind (..), validateParserDetailedWithParserExtensions)
import StackageProgress.CLI (Check (..))
import StackageProgress.Summary (forceString)

-- | Result of checking a single file.
data FileResult = FileResult
  { fileOursOk :: Bool,
    fileHseOk :: Bool,
    fileGhcOk :: Bool,
    fileError :: Maybe String,
    fileGhcError :: Maybe String
  }

-- | Accumulated summary for a package's files.
data PackageFileSummary = PackageFileSummary
  { packageFileOursOk :: !Bool,
    packageFileHseOk :: !Bool,
    packageFileGhcOk :: !Bool,
    packageFileFirstFailure :: Maybe String,
    packageFileGhcError :: Maybe String,
    packageFileErrorsList :: [(String, String)] -- [(filePath, errorMessage)]
  }

-- | Empty file summary (all checks pass).
emptyFileSummary :: PackageFileSummary
emptyFileSummary =
  PackageFileSummary
    { packageFileOursOk = True,
      packageFileHseOk = True,
      packageFileGhcOk = True,
      packageFileFirstFailure = Nothing,
      packageFileGhcError = Nothing,
      packageFileErrorsList = []
    }

-- | Get the file errors from a summary.
getPackageFileErrors :: PackageFileSummary -> [(String, String)]
getPackageFileErrors = packageFileErrorsList

-- | Check a file and accumulate results.
checkAndAccumulateFile :: [Check] -> FilePath -> PackageFileSummary -> FileInfo -> IO PackageFileSummary
checkAndAccumulateFile checks packageRoot summary info = do
  result <- checkFile checks packageRoot info
  let !oursOk = packageFileOursOk summary && fileOursOk result
      !hseOk = packageFileHseOk summary && fileHseOk result
      !ghcOk = packageFileGhcOk summary && fileGhcOk result
      firstFailure =
        case packageFileFirstFailure summary of
          Just err -> Just err
          Nothing ->
            case fileError result of
              Just err -> Just (forceString err)
              Nothing ->
                if fileOursOk result
                  then Nothing
                  else Just "ours failed"
      ghcError =
        case packageFileGhcError summary of
          Just err -> Just err
          Nothing -> fmap forceString (fileGhcError result)
      errorsList =
        case fileError result of
          Just err -> packageFileErrorsList summary ++ [(fileInfoPath info, forceString err)]
          Nothing -> packageFileErrorsList summary
  pure
    PackageFileSummary
      { packageFileOursOk = oursOk,
        packageFileHseOk = hseOk,
        packageFileGhcOk = ghcOk,
        packageFileFirstFailure = firstFailure,
        packageFileGhcError = ghcError,
        packageFileErrorsList = errorsList
      }

-- | Get the first failure message from a summary.
firstFailureMessage :: PackageFileSummary -> String
firstFailureMessage summary =
  fromMaybe "unknown failure" (packageFileFirstFailure summary)

-- | Fold over files, checking each and accumulating results.
foldFilesForPackage :: [Check] -> FilePath -> PackageFileSummary -> [FileInfo] -> IO PackageFileSummary
foldFilesForPackage _ _ summary [] = pure summary
foldFilesForPackage checks packageRoot summary (info : rest)
  | shouldStopAfterFailure checks summary = pure summary
  | otherwise = do
      summary' <- checkAndAccumulateFile checks packageRoot summary info
      foldFilesForPackage checks packageRoot summary' rest

-- | Whether to stop checking after a failure.
shouldStopAfterFailure :: [Check] -> PackageFileSummary -> Bool
shouldStopAfterFailure checks summary =
  not (packageFileOursOk summary) && not (needsFullPackageScan checks)

-- | Whether we need to check all files in the package.
needsFullPackageScan :: [Check] -> Bool
needsFullPackageScan checks =
  CheckHse `elem` checks || CheckGhc `elem` checks

-- | Check a single file.
-- This uses unified extension handling: the default edition comes from the cabal file,
-- each file may override the language edition via pragmas, and we use our own mapping
-- to compute the final set of enabled extensions. This set is used by all parsers.
checkFile :: [Check] -> FilePath -> FileInfo -> IO FileResult
checkFile checks packageRoot info = do
  let file = fileInfoPath info
      -- Parse default language edition from cabal file
      defaultEdition = fileInfoLanguage info >>= Syntax.parseLanguageEdition . T.pack
  source <- readTextFileLenient file
  preprocessed <- preprocessForParserIfEnabled (fileInfoExtensions info) (fileInfoCppOptions info) file (resolveIncludeBestEffort packageRoot file) source
  let source' = resultOutput preprocessed
      cppErrors = [diagToText diag | diag <- resultDiagnostics preprocessed, diagSeverity diag == Error]
      cppErrorMsg =
        if null cppErrors
          then Nothing
          else Just (T.intercalate "\n" cppErrors)
      -- Read module header pragmas to get any LANGUAGE pragma overrides
      headerPragmas = moduleHeaderPragmas source'
      -- Compute the effective extensions using unified extension handling
      effectiveExts = GhcOracle.computeEffectiveExtensions defaultEdition (fileInfoExtensions info) headerPragmas
      -- Configure parser with computed extensions
      parserConfig =
        Aihc.Parser.defaultConfig
          { Aihc.Parser.parserSourceName = file,
            Aihc.Parser.parserExtensions = effectiveExts
          }
      oursResult = Aihc.Parser.parseModule parserConfig source'

  oursStatus <- case oursResult of
    ParseErr err ->
      if CheckParse `elem` checks || needsParsedModule checks
        then do
          let errorDetails = T.pack (Aihc.Parser.errorBundlePretty (Just source') err)
              errorMsg = prefixCppErrors cppErrorMsg errorDetails
          pure (Left (T.unpack errorMsg))
        else pure (Right ())
    ParseOk _parsed ->
      if CheckRoundtripGhc `elem` checks
        then pure (checkRoundtrip effectiveExts file cppErrorMsg source')
        else pure (Right ())

  hseOk <-
    if CheckHse `elem` checks
      then pure $ checkHse effectiveExts source'
      else pure True

  ghcOkResult <-
    if CheckGhc `elem` checks
      then pure $ GhcOracle.oracleDetailedParsesWithParserExtensionsAt file effectiveExts source'
      else pure (Right ())
  let ghcOk = case ghcOkResult of Right () -> True; Left _ -> False
      ghcErrMsg = case ghcOkResult of Left err -> Just (T.unpack err); Right () -> Nothing

  pure
    FileResult
      { fileOursOk = case oursStatus of Right () -> True; Left _ -> False,
        fileHseOk = hseOk,
        fileGhcOk = ghcOk,
        fileError = case oursStatus of Left err -> Just err; Right () -> Nothing,
        fileGhcError = ghcErrMsg
      }

-- | Check if parsing with HSE succeeds.
checkHse :: [Syntax.Extension] -> Text -> Bool
checkHse exts source =
  let mode = hseParseMode {HSE.extensions = fromParserExtensions exts}
   in case HSE.parseFileContentsWithMode mode (T.unpack source) of
        HSE.ParseOk _ -> True
        HSE.ParseFailed _ _ -> False

hseParseMode :: HSE.ParseMode
hseParseMode =
  HSE.defaultParseMode
    { HSE.parseFilename = "<stackage-progress>",
      HSE.extensions = []
    }

-- | Whether we need a parsed module for the checks.
needsParsedModule :: [Check] -> Bool
needsParsedModule checks =
  CheckRoundtripGhc `elem` checks

-- | Check roundtrip via GHC oracle.
checkRoundtrip :: [Syntax.Extension] -> FilePath -> Maybe Text -> Text -> Either String ()
checkRoundtrip exts file cppErrorMsg source' =
  case validateParserDetailedWithParserExtensions exts source' of
    Nothing -> Right ()
    Just err ->
      case validationErrorKind err of
        ValidationParseError ->
          Left (T.unpack (prefixCppErrors cppErrorMsg ("parse failed in " <> T.pack file <> ": " <> T.pack (validationErrorMessage err))))
        ValidationRoundtripError ->
          Left (T.unpack (prefixCppErrors cppErrorMsg ("roundtrip mismatch in " <> T.pack file <> ": " <> T.pack (validationErrorMessage err))))
