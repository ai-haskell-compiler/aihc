{-# LANGUAGE OverloadedStrings #-}

-- | Shared package file processing utilities.
--
-- This module provides common file processing functionality used by both
-- stackage-progress and hackage-tester for validating Haskell packages.
module PackageProcessor
  ( -- * File processing
    FileProcessResult (..),
    processFileWithOptions,

    -- * Processing options
    ProcessOptions (..),
    defaultProcessOptions,

    -- * Outcome types
    Outcome (..),
    outcomeIsSuccess,
    outcomeLabel,
  )
where

import Aihc.Cpp (Severity (..), diagSeverity, resultDiagnostics, resultOutput)
import CppSupport (preprocessForParserIfEnabled)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GhcOracle
import HackageSupport
  ( FileInfo (..),
    diagToText,
    readTextFileLenient,
    resolveIncludeBestEffort,
  )
import ParserValidation
  ( ValidationError (..),
    ValidationErrorKind (..),
    validateParserDetailedWithExtensionNames,
  )

-- | Outcome of processing a file.
data Outcome
  = -- | File processed successfully
    OutcomeSuccess
  | -- | GHC oracle rejected the file
    OutcomeGhcError
  | -- | Our parser failed to parse
    OutcomeParseError
  | -- | Roundtrip check failed
    OutcomeRoundtripFail
  deriving (Eq, Show)

-- | Check if an outcome represents success.
outcomeIsSuccess :: Outcome -> Bool
outcomeIsSuccess OutcomeSuccess = True
outcomeIsSuccess _ = False

-- | Get the label for an outcome (for failure reporting).
outcomeLabel :: Outcome -> Maybe String
outcomeLabel out =
  case out of
    OutcomeSuccess -> Nothing
    OutcomeGhcError -> Just "GHC_ERROR"
    OutcomeParseError -> Just "PARSE_ERROR"
    OutcomeRoundtripFail -> Just "ROUNDTRIP_FAIL"

-- | Result of processing a single file.
data FileProcessResult = FileProcessResult
  { fileProcessPath :: FilePath,
    fileProcessOutcome :: Outcome,
    fileProcessCppDiagnostics :: [Text],
    fileProcessDetail :: Maybe Text
  }
  deriving (Eq, Show)

-- | Options for file processing.
newtype ProcessOptions = ProcessOptions
  { -- | Whether to skip parser validation and only check GHC errors
    processOnlyGhcErrors :: Bool
  }
  deriving (Eq, Show)

-- | Default processing options.
defaultProcessOptions :: ProcessOptions
defaultProcessOptions =
  ProcessOptions
    { processOnlyGhcErrors = False
    }

-- | Process a file with the given options.
processFileWithOptions :: ProcessOptions -> FilePath -> FileInfo -> IO FileProcessResult
processFileWithOptions opts packageRoot info = do
  let file = fileInfoPath info
  source <- readTextFileLenient file
  preprocessed <- preprocessForParserIfEnabled (fileInfoExtensions info) (fileInfoCppOptions info) file (resolveIncludeBestEffort packageRoot file) source
  let source' = resultOutput preprocessed
      cppErrs = [diagToText diag | diag <- resultDiagnostics preprocessed, diagSeverity diag == Error]
      ghcResult = GhcOracle.oracleDetailedParsesModuleWithNamesAt file (fileInfoExtensions info) (fileInfoLanguage info) source'

  case ghcResult of
    Left err ->
      pure
        FileProcessResult
          { fileProcessPath = file,
            fileProcessOutcome = OutcomeGhcError,
            fileProcessCppDiagnostics = cppErrs,
            fileProcessDetail = Just err
          }
    Right () ->
      if processOnlyGhcErrors opts
        then
          pure
            FileProcessResult
              { fileProcessPath = file,
                fileProcessOutcome = OutcomeSuccess,
                fileProcessCppDiagnostics = cppErrs,
                fileProcessDetail = Nothing
              }
        else case validateParserDetailedWithExtensionNames (fileInfoExtensions info) (fileInfoLanguage info) source' of
          Nothing ->
            pure
              FileProcessResult
                { fileProcessPath = file,
                  fileProcessOutcome = OutcomeSuccess,
                  fileProcessCppDiagnostics = cppErrs,
                  fileProcessDetail = Nothing
                }
          Just err ->
            case validationErrorKind err of
              ValidationParseError ->
                pure
                  FileProcessResult
                    { fileProcessPath = file,
                      fileProcessOutcome = OutcomeParseError,
                      fileProcessCppDiagnostics = cppErrs,
                      fileProcessDetail = Just (T.pack (validationErrorMessage err))
                    }
              ValidationRoundtripError ->
                pure
                  FileProcessResult
                    { fileProcessPath = file,
                      fileProcessOutcome = OutcomeRoundtripFail,
                      fileProcessCppDiagnostics = cppErrs,
                      fileProcessDetail = Just (T.pack (validationErrorMessage err))
                    }
