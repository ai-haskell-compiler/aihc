{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

-- | File-level validation checks for Haskell source files.
module StackageProgress.FileChecker
  ( -- * File checking
    FileCheckOptions (..),
    FileResult (..),
    PackageFileSummary (..),
    checkFile,
    emptyFileSummary,
    checkAndAccumulateFile,
    foldFilesForPackage,
    firstFailureMessage,
    getPackageFileErrors,
  )
where

import Aihc.Cpp (Severity (..), diagSeverity, resultDiagnostics, resultOutput)
import Aihc.Parser qualified
import Aihc.Parser.Syntax qualified as Syntax
import Control.DeepSeq (deepseq)
import Control.Exception (evaluate)
import Control.Monad (when)
import CppSupport (moduleHeaderPragmas, preprocessForParserIfEnabled)
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import GhcOracle qualified
import HackageSupport
  ( FileInfo (..),
    diagToText,
    prefixCppErrors,
    readTextFileLenient,
    resolveIncludeBestEffort,
  )
import HseExtensions (fromParserExtensions)
import Language.Haskell.Exts qualified as HSE
import StackageProgress.CLI (Parser (..))
import StackageProgress.FileCheckerTiming (maybeVerboseTimingParts)
import StackageProgress.Summary (forceString)
import System.IO (hPutStrLn, stderr)
import System.Timeout (timeout)

-- | Run an @IO@ action and return its result with elapsed monotonic time in nanoseconds.
withElapsedNanos :: IO a -> IO (a, Word64)
withElapsedNanos action = do
  t0 <- getMonotonicTimeNSec
  x <- action
  t1 <- getMonotonicTimeNSec
  pure (x, t1 - t0)

aihcParseTimeoutMicros :: Int
aihcParseTimeoutMicros = 5 * 60 * 1_000_000

-- | Result of checking a single file.
data FileResult = FileResult
  { fileOursOk :: !Bool,
    fileHseOk :: !Bool,
    fileGhcOk :: !Bool,
    fileError :: !(Maybe String),
    fileGhcError :: !(Maybe String)
  }

data FileCheckOptions = FileCheckOptions
  { fileCheckKeepFirstFailure :: !Bool,
    fileCheckKeepFileErrors :: !Bool,
    fileCheckKeepGhcError :: !Bool
  }

-- | Accumulated summary for a package's files.
data PackageFileSummary = PackageFileSummary
  { packageFileOursOk :: !Bool,
    packageFileHseOk :: !Bool,
    packageFileGhcOk :: !Bool,
    packageFileFirstFailure :: !(Maybe String),
    packageFileGhcError :: !(Maybe String),
    packageFileErrorsList :: ![(String, String)] -- [(filePath, errorMessage)]
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
getPackageFileErrors = List.reverse . packageFileErrorsList

-- | Check a file and accumulate results.
checkAndAccumulateFile :: FileCheckOptions -> [Parser] -> Bool -> FilePath -> PackageFileSummary -> FileInfo -> IO PackageFileSummary
checkAndAccumulateFile checkOpts parsers verbose packageRoot summary info = do
  result <- checkFile checkOpts parsers verbose packageRoot info
  let !oursOk = packageFileOursOk summary && fileOursOk result
      !hseOk = packageFileHseOk summary && fileHseOk result
      !ghcOk = packageFileGhcOk summary && fileGhcOk result
      !firstFailure
        | not (fileCheckKeepFirstFailure checkOpts) = Nothing
        | otherwise =
            case packageFileFirstFailure summary of
              Just err -> Just err
              Nothing ->
                case fileError result of
                  Just err -> Just (forceString err)
                  Nothing ->
                    if fileOursOk result
                      then Nothing
                      else Just "ours failed"
      !ghcError
        | not (fileCheckKeepGhcError checkOpts) = Nothing
        | otherwise =
            case packageFileGhcError summary of
              Just err -> Just err
              Nothing -> fmap forceString (fileGhcError result)
      !errorsList =
        case fileError result of
          Just err | fileCheckKeepFileErrors checkOpts -> (fileInfoPath info, forceString err) : packageFileErrorsList summary
          Nothing -> packageFileErrorsList summary
          _ -> packageFileErrorsList summary
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
foldFilesForPackage :: FileCheckOptions -> [Parser] -> Bool -> FilePath -> PackageFileSummary -> [FileInfo] -> IO PackageFileSummary
foldFilesForPackage _ _ _ _ summary [] = pure summary
foldFilesForPackage checkOpts parsers verbose packageRoot summary (info : rest) =
  do
    summary' <- checkAndAccumulateFile checkOpts parsers verbose packageRoot summary info
    foldFilesForPackage checkOpts parsers verbose packageRoot summary' rest

-- | Check a single file.
-- This uses unified extension handling: the default edition comes from the cabal file,
-- each file may override the language edition via pragmas, and we use our own mapping
-- to compute the final set of enabled extensions. This set is used by all parsers.
checkFile :: FileCheckOptions -> [Parser] -> Bool -> FilePath -> FileInfo -> IO FileResult
checkFile checkOpts parsers verbose packageRoot info = do
  let file = fileInfoPath info
  source <- readTextFileLenient file
  (preprocessed, preprocessNanos) <-
    withElapsedNanos $
      preprocessForParserIfEnabled (fileInfoExtensions info) (fileInfoCppOptions info) file (fileInfoDependencies info) (resolveIncludeBestEffort packageRoot file) source
  let source' = resultOutput preprocessed
      keepAihcErrorDetail = fileCheckKeepFirstFailure checkOpts || fileCheckKeepFileErrors checkOpts
      cppErrorMsg
        | keepAihcErrorDetail =
            case [diagToText diag | diag <- resultDiagnostics preprocessed, diagSeverity diag == Error] of
              [] -> Nothing
              cppErrors -> Just (T.intercalate "\n" cppErrors)
        | otherwise = Nothing
      -- Read module header pragmas to get any LANGUAGE pragma overrides
      headerPragmas = moduleHeaderPragmas source'
      defaultEdition = fromMaybe Syntax.Haskell98Edition (fileInfoLanguage info)
      edition = fromMaybe defaultEdition (Syntax.headerLanguageEdition headerPragmas)
      -- Compute the effective extensions using unified extension handling
      extensionSettings = fileInfoExtensions info ++ Syntax.headerExtensionSettings headerPragmas
      effectiveExts = Syntax.effectiveExtensions edition extensionSettings
      -- Configure parser with computed extensions
      parserConfig =
        Aihc.Parser.defaultConfig
          { Aihc.Parser.parserSourceName = file,
            Aihc.Parser.parserExtensions = effectiveExts
          }

  (aihcOk, aihcErrMsg, aihcNanos) <-
    if ParserAihc `elem` parsers
      then do
        (parseOutcome, parseNanos) <-
          withElapsedNanos $
            timeout aihcParseTimeoutMicros $
              evaluate (let r = Aihc.Parser.parseModule parserConfig source' in r `deepseq` r)
        let (!aihcParseOk, aihcErr) =
              case parseOutcome of
                Nothing ->
                  ( False,
                    if keepAihcErrorDetail
                      then
                        let errorDetails = "AIHC parser timed out after 5 minutes"
                            errorMsg = prefixCppErrors cppErrorMsg errorDetails
                         in Just (T.unpack errorMsg)
                      else Nothing
                  )
                Just (parseErrs, _parsed) ->
                  case parseErrs of
                    [] -> (True, Nothing)
                    _
                      | keepAihcErrorDetail ->
                          let errorDetails = T.pack (Aihc.Parser.formatParseErrors file (Just source') parseErrs)
                              errorMsg = prefixCppErrors cppErrorMsg errorDetails
                           in (False, Just (T.unpack errorMsg))
                    _ -> (False, Nothing)
        pure (aihcParseOk, aihcErr, parseNanos)
      else pure (True, Nothing, 0)

  hseOk <-
    if ParserHse `elem` parsers
      then pure $ checkHse effectiveExts source'
      else pure True

  (ghcOk, ghcErrMsg, ghcNanos) <-
    if ParserGhc `elem` parsers
      then do
        let ghcRes = GhcOracle.oracleModuleAstFingerprintNoCPP file edition extensionSettings source'
        ((ghcParseOk, ghcErr), ns) <-
          withElapsedNanos $
            evaluate $
              case ghcRes of
                Right {} -> (True, Nothing)
                Left err
                  | fileCheckKeepGhcError checkOpts -> (False, Just (T.unpack err))
                  | otherwise -> (False, Nothing)
        pure (ghcParseOk, ghcErr, ns)
      else pure (True, Nothing, 0)

  let processedBytes = T.length source'
  when verbose $
    case maybeVerboseTimingParts parsers processedBytes preprocessNanos aihcNanos ghcNanos of
      Just timingParts ->
        hPutStrLn stderr ("stackage-progress: " ++ reverse (take 60 (reverse file)) ++ " " ++ unwords timingParts)
      Nothing ->
        pure ()

  pure
    FileResult
      { fileOursOk = aihcOk,
        fileHseOk = hseOk,
        fileGhcOk = ghcOk,
        fileError = aihcErrMsg,
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
