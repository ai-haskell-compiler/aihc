{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Aihc.Cpp (Severity (..), diagSeverity, resultDiagnostics, resultOutput)
import Aihc.Parser.Lex (readModuleHeaderPragmas)
import Aihc.Parser.Syntax qualified as Syntax
import ConcurrentProgress (mapConcurrentlyBounded)
import Control.Exception (SomeException, displayException, try)
import Control.Monad (unless, when)
import CppSupport (preprocessForParserIfEnabled)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Distribution.Package (packageId, pkgVersion)
import Distribution.PackageDescription (GenericPackageDescription (..))
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
import Distribution.Pretty (prettyShow)
import GHC.Conc (getNumProcessors)
import GhcOracle qualified
import HackageSupport
  ( FileInfo (..),
    diagToText,
    downloadPackage,
    findTargetFilesFromCabal,
    readTextFileLenient,
    resolveIncludeBestEffort,
  )
import HackageTester.CLI (Options (..), parseOptionsIO)
import HackageTester.Model (FileResult (..), Outcome (..), Summary (..), failureLabel, shouldFailSummary, summarizeResults)
import Network.HTTP.Client (HttpException, Manager, Request (responseTimeout), httpLbs, newManager, parseRequest, responseBody, responseTimeoutMicro)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import ParserValidation (ValidationError (..), ValidationErrorKind (..), validateParser)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (takeFileName)
import System.IO (hPutStrLn, stderr)

data RunInfo = RunInfo
  { runPackageName :: String,
    runVersion :: String,
    runSummary :: Summary
  }

main :: IO ()
main = do
  opts <- parseOptionsIO
  runResult <- try (runTester opts) :: IO (Either SomeException (Either Text Bool))
  case runResult of
    Left err -> do
      hPutStrLn stderr ("hackage-tester failed: " ++ displayException (err :: SomeException))
      exitFailure
    Right (Left err) -> do
      hPutStrLn stderr (T.unpack err)
      exitFailure
    Right (Right ok) -> if ok then exitSuccess else exitFailure

runTester :: Options -> IO (Either Text Bool)
runTester opts = do
  putStrLn ("Testing package: " ++ optPackage opts)

  versionResult <-
    case optVersion opts of
      Just forced -> pure (Right forced)
      Nothing -> getLatestVersion (optPackage opts)

  case versionResult of
    Left err -> pure (Left err)
    Right resolved -> runTesterWithVersion opts resolved

runTesterWithVersion :: Options -> String -> IO (Either Text Bool)
runTesterWithVersion opts version = do
  srcDir <- downloadPackage (optPackage opts) version
  files <- findTargetFilesFromCabal srcDir

  when (null files) $ do
    hPutStrLn stderr "No target source files found in package components"
    let summary = summarizeResults []
    emitSummary opts (RunInfo (optPackage opts) version summary)
  if null files
    then pure (Left "No target source files found in package components")
    else do
      putStrLn ("Found " ++ show (length files) ++ " Haskell source files")

      jobs <- maybe getNumProcessors pure (optJobs opts)
      results <- processFiles opts jobs srcDir files

      unless (optJson opts) (printFailureDetails results)

      let summary = summarizeResults results
      emitSummary opts (RunInfo (optPackage opts) version summary)
      pure (Right (not (shouldFailSummary summary)))

getLatestVersion :: String -> IO (Either Text String)
getLatestVersion packageName = do
  manager <- newManager tlsManagerSettings
  let url = "https://hackage.haskell.org/package/" ++ packageName ++ "/" ++ packageName ++ ".cabal"
  requestResult <- try (parseRequest url)
  case requestResult of
    Left err -> pure (Left ("Failed to build Hackage request: " <> T.pack (displayException (err :: HttpException))))
    Right request -> do
      fetchResult <- try (fetchCabalFile manager request)
      case fetchResult of
        Left err -> pure (Left ("Failed to fetch package metadata from Hackage: " <> T.pack (displayException (err :: HttpException))))
        Right cabalBytes ->
          case runParseResult (parseGenericPackageDescription (LBS.toStrict cabalBytes :: BS.ByteString)) of
            (_, Left (_, errs)) -> pure (Left ("Failed to parse Hackage cabal file: " <> T.pack (show errs)))
            (_, Right gpd) ->
              let ver = pkgVersion (packageId (packageDescription gpd))
               in pure (Right (prettyShow ver))

fetchCabalFile :: Manager -> Request -> IO LBS.ByteString
fetchCabalFile manager request = do
  let request' = request {responseTimeout = responseTimeoutMicro (30 * 1000 * 1000)}
  response <- httpLbs request' manager
  pure (responseBody response)

processFiles :: Options -> Int -> FilePath -> [FileInfo] -> IO [FileResult]
processFiles opts jobs packageRoot =
  mapConcurrentlyBounded jobs (processFile opts packageRoot)

processFile :: Options -> FilePath -> FileInfo -> IO FileResult
processFile opts packageRoot info = do
  let file = fileInfoPath info
  source <- readTextFileLenient file
  preprocessed <- preprocessForParserIfEnabled (fileInfoExtensions info) (fileInfoCppOptions info) file (fileInfoDependencies info) (resolveIncludeBestEffort packageRoot file) source
  let source' = resultOutput preprocessed
      cppErrs = [diagToText diag | diag <- resultDiagnostics preprocessed, diagSeverity diag == Error]
      headerPragmas = readModuleHeaderPragmas source'
      defaultEdition = fromMaybe Syntax.Haskell2010Edition (fileInfoLanguage info)
      edition = fromMaybe defaultEdition (Syntax.headerLanguageEdition headerPragmas)
      extensionSettings = fileInfoExtensions info ++ Syntax.headerExtensionSettings headerPragmas
      ghcResult = GhcOracle.oracleModuleAstFingerprint file edition extensionSettings source'

  case ghcResult of
    Left err ->
      pure
        FileResult
          { filePath = file,
            outcome = OutcomeGhcError,
            cppDiagnostics = cppErrs,
            outcomeDetail = Just err
          }
    Right {} ->
      if optOnlyGhcErrors opts
        then
          pure
            FileResult
              { filePath = file,
                outcome = OutcomeSuccess,
                cppDiagnostics = cppErrs,
                outcomeDetail = Nothing
              }
        else case validateParser (takeFileName file) edition extensionSettings source' of
          Nothing ->
            pure
              FileResult
                { filePath = file,
                  outcome = OutcomeSuccess,
                  cppDiagnostics = cppErrs,
                  outcomeDetail = Nothing
                }
          Just err ->
            case validationErrorKind err of
              ValidationParseError ->
                pure
                  FileResult
                    { filePath = file,
                      outcome = OutcomeParseError,
                      cppDiagnostics = cppErrs,
                      outcomeDetail = Just (T.pack (validationErrorMessage err))
                    }
              ValidationRoundtripError ->
                pure
                  FileResult
                    { filePath = file,
                      outcome = OutcomeRoundtripFail,
                      cppDiagnostics = cppErrs,
                      outcomeDetail = Just (T.pack (validationErrorMessage err))
                    }

printFailureDetails :: [FileResult] -> IO ()
printFailureDetails results = do
  mapM_
    ( \result ->
        case failureLabel (outcome result) of
          Nothing -> pure ()
          Just label -> do
            putStrLn (label ++ ": " ++ filePath result)
            unless (null (cppDiagnostics result)) $ do
              putStrLn "  cpp diagnostics:"
              mapM_ (TIO.putStrLn . ("    " <>)) (cppDiagnostics result)
            case outcomeDetail result of
              Nothing -> pure ()
              Just fullDetail -> do
                mapM_ (TIO.putStrLn . ("  " <>)) (T.lines fullDetail)
    )
    results

emitSummary :: Options -> RunInfo -> IO ()
emitSummary opts info =
  if optJson opts
    then printJsonSummary info
    else printHumanSummary (runSummary info)

printHumanSummary :: Summary -> IO ()
printHumanSummary summary = do
  putStrLn ""
  putStrLn "Summary:"
  putStrLn ("  Total files:     " ++ show (totalFiles summary))
  putStrLn ("  GHC errors:      " ++ show (ghcErrors summary))
  putStrLn ("  Parse errors:    " ++ show (parseErrors summary))
  putStrLn ("  Roundtrip fails: " ++ show (roundtripFails summary))
  putStrLn ("  Success rate:    " ++ show (round (successRate summary) :: Int) ++ "%")

printJsonSummary :: RunInfo -> IO ()
printJsonSummary info = do
  let summary = runSummary info
      status :: String
      status = if shouldFailSummary summary then "fail" else "pass"
      payload =
        Aeson.object
          [ "package" Aeson..= runPackageName info,
            "version" Aeson..= runVersion info,
            "status" Aeson..= status,
            "total_files" Aeson..= totalFiles summary,
            "ghc_errors" Aeson..= ghcErrors summary,
            "parse_errors" Aeson..= parseErrors summary,
            "roundtrip_fails" Aeson..= roundtripFails summary,
            "success_count" Aeson..= successCount summary,
            "failure_count" Aeson..= failureCount summary,
            "success_rate" Aeson..= successRate summary
          ]
  LBS8.putStrLn (Aeson.encode payload)
