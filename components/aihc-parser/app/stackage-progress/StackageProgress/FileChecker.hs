{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}
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
  )
where

import Aihc.Cpp (Severity (..), diagSeverity, resultDiagnostics, resultOutput)
import qualified Aihc.Parser
import qualified Aihc.Parser.Syntax as Syntax
import Control.DeepSeq (deepseq)
import Control.Exception (evaluate)
import Control.Monad (when)
import CppSupport (moduleHeaderPragmas, preprocessForParserIfEnabled)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
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
import StackageProgress.CLI (Parser (..))
import StackageProgress.Summary (forceString)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

-- | Run an @IO@ action and return its result with elapsed monotonic time in nanoseconds.
withElapsedNanos :: IO a -> IO (a, Word64)
withElapsedNanos action = do
  t0 <- getMonotonicTimeNSec
  x <- action
  t1 <- getMonotonicTimeNSec
  pure (x, t1 - t0)

formatNanosMs :: Word64 -> String
formatNanosMs n = printf "%.3fms" (fromIntegral n / 1e6 :: Double)

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
checkAndAccumulateFile :: [Parser] -> Bool -> FilePath -> PackageFileSummary -> FileInfo -> IO PackageFileSummary
checkAndAccumulateFile parsers verbose packageRoot summary info = do
  result <- checkFile parsers verbose packageRoot info
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
foldFilesForPackage :: [Parser] -> Bool -> FilePath -> PackageFileSummary -> [FileInfo] -> IO PackageFileSummary
foldFilesForPackage _ _ _ summary [] = pure summary
foldFilesForPackage parsers verbose packageRoot summary (info : rest) =
  do
    summary' <- checkAndAccumulateFile parsers verbose packageRoot summary info
    foldFilesForPackage parsers verbose packageRoot summary' rest

-- | Check a single file.
-- This uses unified extension handling: the default edition comes from the cabal file,
-- each file may override the language edition via pragmas, and we use our own mapping
-- to compute the final set of enabled extensions. This set is used by all parsers.
checkFile :: [Parser] -> Bool -> FilePath -> FileInfo -> IO FileResult
checkFile parsers verbose packageRoot info = do
  let file = fileInfoPath info
  source <- readTextFileLenient file
  (preprocessed, preprocessNanos) <-
    withElapsedNanos $
      preprocessForParserIfEnabled (fileInfoExtensions info) (fileInfoCppOptions info) file (resolveIncludeBestEffort packageRoot file) source
  let source' = resultOutput preprocessed
      cppErrors = [diagToText diag | diag <- resultDiagnostics preprocessed, diagSeverity diag == Error]
      cppErrorMsg =
        if null cppErrors
          then Nothing
          else Just (T.intercalate "\n" cppErrors)
      -- Read module header pragmas to get any LANGUAGE pragma overrides
      headerPragmas = moduleHeaderPragmas source'
      defaultEdition = fromMaybe Syntax.Haskell2010Edition (fileInfoLanguage info)
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

  (aihcErrMsg, aihcNanos) <-
    if ParserAihc `elem` parsers
      then do
        ((parseErrs, _parsed), parseNanos) <-
          withElapsedNanos $
            evaluate (let r = Aihc.Parser.parseModule parserConfig source' in r `deepseq` r)
        let aihcErr = case parseErrs of
              [] -> Nothing
              _ ->
                let errorDetails = T.pack (Aihc.Parser.formatParseErrors file (Just source') parseErrs)
                    errorMsg = prefixCppErrors cppErrorMsg errorDetails
                 in Just (T.unpack errorMsg)
        pure (aihcErr, parseNanos)
      else pure (Nothing, 0)

  hseOk <-
    if ParserHse `elem` parsers
      then pure $ checkHse effectiveExts source'
      else pure True

  (ghcErrMsg, ghcNanos) <-
    if ParserGhc `elem` parsers
      then do
        let ghcRes = GhcOracle.oracleModuleAstFingerprintNoCPP file edition extensionSettings source'
        (ghcRes', ns) <- withElapsedNanos (evaluate (ghcRes `deepseq` ghcRes))
        pure
          ( case ghcRes' of
              Right {} -> Nothing
              Left err -> Just (T.unpack err),
            ns
          )
      else pure (Nothing, 0)

  let timingParts =
        ["cpp=" ++ formatNanosMs preprocessNanos]
          ++ ["aihc=" ++ formatNanosMs aihcNanos | ParserAihc `elem` parsers]
          ++ ["ghc=" ++ formatNanosMs ghcNanos | ParserGhc `elem` parsers]
  let timelimit = 1_000_000_000 -- 1s
  when (verbose && (preprocessNanos > timelimit || aihcNanos > timelimit || ghcNanos > timelimit)) $ do
    hPutStrLn stderr ("stackage-progress: " ++ reverse (take 60 (reverse file)) ++ " " ++ unwords timingParts)

  pure
    FileResult
      { fileOursOk = isNothing aihcErrMsg,
        fileHseOk = hseOk,
        fileGhcOk = isNothing ghcErrMsg,
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
