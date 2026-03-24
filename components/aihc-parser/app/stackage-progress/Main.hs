{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Aihc.Cpp (Severity (..), diagSeverity, resultDiagnostics, resultOutput)
import Aihc.Parser (ParseResult (..))
import qualified Aihc.Parser
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (IOException, SomeException, displayException, try)
import Control.Monad (when)
import CppSupport (preprocessForParserIfEnabled)
import Data.Char (isAlphaNum, isSpace)
import Data.List (isPrefixOf, nub, sortBy)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Conc (getNumProcessors)
import qualified GhcOracle
import HackageSupport
  ( FileInfo (..),
    diagToText,
    downloadPackageQuietWithNetwork,
    findTargetFilesFromCabal,
    prefixCppErrors,
    readTextFileLenient,
    resolveIncludeBestEffort,
  )
import HseExtensions (fromExtensionNames)
import qualified Language.Haskell.Exts as HSE
import ParserValidation (ValidationError (..), ValidationErrorKind (..), validateParserDetailedWithExtensionNames)
import StackageProgress.Summary
  ( FailedPackage (..),
    PackageResult (..),
    PackageSpec (..),
    PromptCandidate,
    RunSummary,
    SummaryOptions (..),
    addPackageResults,
    emptySummary,
    finalizeSummary,
    forceString,
    promptCandidateFromResult,
    renderPrompt,
    selectPromptCandidate,
    summaryFailedPackages,
    summaryGhcErrors,
    summarySucceededPackages,
    summarySuccessGhcN,
    summarySuccessHseN,
    summarySuccessOursN,
  )
import System.Directory (XdgDirectory (XdgCache), createDirectoryIfMissing, doesFileExist, getCurrentDirectory, getFileSize, getHomeDirectory, getXdgDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (takeDirectory, (</>))
import System.IO (hFlush, hIsTerminalDevice, hPutStrLn, stderr, stdout)
import System.Process (readProcess)

data Check
  = CheckParse
  | CheckRoundtripGhc
  | CheckHse
  | CheckGhc
  deriving (Eq, Show)

data Options = Options
  { optSnapshot :: String,
    optChecks :: [Check],
    optJobs :: Maybe Int,
    optOffline :: Bool,
    optPrompt :: Bool,
    optPromptSeed :: Maybe Int,
    optPrintSucceeded :: Bool,
    optPrintFailedTable :: Bool,
    optSanityCheck :: Bool,
    optGhcErrorsFile :: Maybe FilePath,
    optGhcErrorsLimit :: Int
  }

main :: IO ()
main = do
  args <- getArgs
  opts <-
    case parseOptions args of
      Left err -> do
        hPutStrLn stderr err
        hPutStrLn stderr usage
        exitFailure
      Right parsed -> pure parsed

  snapshotResult <- loadStackageSnapshotWithMode (optSnapshot opts) (optOffline opts)
  packages <-
    case snapshotResult of
      Left err -> do
        hPutStrLn stderr ("Failed to load snapshot: " ++ err)
        exitFailure
      Right specs -> pure specs

  let total = length packages
  jobs <- maybe getNumProcessors pure (optJobs opts)
  isStdoutTerminal <- hIsTerminalDevice stdout
  let showProgress = isStdoutTerminal && not (optPrompt opts)
  when showProgress (putProgressLine (ProgressState 0 0 total))
  promptTemplate <-
    if optPrompt opts
      then loadPromptTemplate
      else pure ""
  (summary, promptCandidates) <-
    foldConcurrentlyChunksWithProgress
      jobs
      (runPackage opts)
      packages
      total
      showProgress
      (summaryOptions opts)
      (optPrompt opts)
  let successOursN = summarySuccessOursN summary
      successHseN = summarySuccessHseN summary
      successGhcN = summarySuccessGhcN summary
  when showProgress (putStrLn "")

  when (optPrompt opts) $ do
    candidate <- pickPromptCandidate (optPromptSeed opts) promptCandidates
    case candidate of
      Nothing -> do
        hPutStrLn stderr "No parser failures found in this snapshot; no prompt generated."
        exitFailure
      Just selected -> do
        putStr (renderPrompt promptTemplate selected)
        exitSuccess

  when (optPrintSucceeded opts) $ do
    mapM_ putStrLn (summarySucceededPackages summary)
    putStrLn ""

  putStrLn "Parsing success rates:"
  putStrLn $ "  AIHC: " ++ show successOursN ++ " / " ++ show total ++ " (" ++ show (pct successOursN total) ++ "%)"
  when (optSanityCheck opts) $ do
    putStrLn $ "  HSE:  " ++ show successHseN ++ " / " ++ show total ++ " (" ++ show (pct successHseN total) ++ "%)"
  putStrLn $ "  GHC:  " ++ show successGhcN ++ " / " ++ show total ++ " (" ++ show (pct successGhcN total) ++ "%)"

  case optGhcErrorsFile opts of
    Nothing -> pure ()
    Just path -> do
      home <- getHomeDirectory
      let sanitize = T.unpack . T.replace (T.pack home) "$HOME" . T.pack
          ghcErrors =
            take
              (optGhcErrorsLimit opts)
              [(pkg, sanitize err) | (pkg, err) <- summaryGhcErrors summary]
      writeFile path $ unlines ["=== " ++ pkg ++ " ===\n" ++ err ++ "\n" | (pkg, err) <- ghcErrors]
      putStrLn $ "GHC errors written to " ++ path

  when (optPrintFailedTable opts) $ do
    let failed =
          sortBy
            (\a b -> compare (failedPackageSourceSize a, failedPackageName a) (failedPackageSourceSize b, failedPackageName b))
            (summaryFailedPackages summary)
        col1 = "Package"
        col2 = "Size (bytes)"
        pkgWidth = max (length col1) $ case failed of
          [] -> 0
          rs -> maximum (map (length . failedPackageName) rs)
        sizeWidth = max (length col2) $ case failed of
          [] -> 0
          rs -> maximum (map (length . show . failedPackageSourceSize) rs)
        padL n s = s ++ replicate (n - length s) ' '
        padR n s = replicate (n - length s) ' ' ++ s
    putStrLn (padL pkgWidth col1 ++ "  " ++ padR sizeWidth col2)
    putStrLn (replicate (pkgWidth + 2 + sizeWidth) '-')
    mapM_ (\r -> putStrLn (padL pkgWidth (failedPackageName r) ++ "  " ++ padR sizeWidth (show (failedPackageSourceSize r)))) failed

  if successOursN == total then exitSuccess else exitFailure

usage :: String
usage =
  unlines
    [ "Usage: cabal run stackage-progress -- [--snapshot lts-24.33] [--checks parse,roundtrip-ghc] [--jobs N] [--offline] [--prompt] [--prompt-seed N] [--print-succeeded] [--print-failed-table] [--sanity-check]",
      "",
      "Defaults:",
      "  --snapshot lts-24.33",
      "  --checks parse",
      "  --jobs <num processors>",
      "  --offline false",
      "  --prompt false",
      "  --prompt-seed <monotonic clock>",
      "  --print-succeeded false",
      "  --print-failed-table false",
      "  --sanity-check false",
      "  --ghc-errors-file <path>",
      "  --ghc-errors-limit 100"
    ]

parseOptions :: [String] -> Either String Options
parseOptions = go (Options "lts-24.33" [CheckParse] Nothing False False Nothing False False False Nothing 100)
  where
    go opts [] =
      let opts' =
            if optSanityCheck opts
              then opts {optChecks = nub (optChecks opts ++ [CheckHse, CheckGhc])}
              else opts
       in Right opts'
    go opts ("--snapshot" : value : rest)
      | null value = Left "--snapshot requires a value"
      | otherwise = go opts {optSnapshot = value} rest
    go opts ("--checks" : value : rest) = do
      checks <- parseChecks value
      go opts {optChecks = checks} rest
    go opts ("--jobs" : value : rest) =
      case reads value of
        [(n, "")] | n > 0 -> go opts {optJobs = Just n} rest
        _ -> Left "--jobs must be a positive integer"
    go opts ("--offline" : rest) =
      go opts {optOffline = True} rest
    go opts ("--prompt" : rest) =
      go opts {optPrompt = True} rest
    go opts ("--prompt-seed" : value : rest) =
      case reads value of
        [(n, "")] -> go opts {optPrompt = True, optPromptSeed = Just n} rest
        _ -> Left "--prompt-seed must be an integer"
    go opts ("--print-succeeded" : rest) =
      go opts {optPrintSucceeded = True} rest
    go opts ("--print-failed-table" : rest) =
      go opts {optPrintFailedTable = True} rest
    go opts ("--sanity-check" : rest) =
      go opts {optSanityCheck = True} rest
    go opts ("--ghc-errors-file" : path : rest) =
      go opts {optGhcErrorsFile = Just path} rest
    go opts ("--ghc-errors-limit" : value : rest) =
      case reads value of
        [(n, "")] | n >= 0 -> go opts {optGhcErrorsLimit = n} rest
        _ -> Left "--ghc-errors-limit must be a non-negative integer"
    go _ ("--help" : _) = Left ""
    go _ (arg : _) = Left ("Unknown argument: " ++ arg)

totalSourceSize :: [FileInfo] -> IO Integer
totalSourceSize infos = sum <$> mapM (safeFileSize . fileInfoPath) infos
  where
    safeFileSize path = do
      r <- try (getFileSize path) :: IO (Either IOException Integer)
      pure $ case r of
        Left _ -> 0
        Right n -> n

parseChecks :: String -> Either String [Check]
parseChecks raw = do
  checks <- mapM parseCheck (splitComma raw)
  let uniq = nub checks
  if null uniq
    then Left "--checks cannot be empty"
    else Right uniq

parseCheck :: String -> Either String Check
parseCheck raw =
  case trim raw of
    "parse" -> Right CheckParse
    "roundtrip-ghc" -> Right CheckRoundtripGhc
    other -> Left ("Unknown check: " ++ other)

splitComma :: String -> [String]
splitComma s =
  case break (== ',') s of
    (chunk, []) -> [chunk]
    (chunk, _ : rest) -> chunk : splitComma rest

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = reverse . dropWhile p . reverse

loadStackageSnapshotWithMode :: String -> Bool -> IO (Either String [PackageSpec])
loadStackageSnapshotWithMode snapshot offline = do
  cacheFile <- snapshotCacheFile snapshot
  hasCache <- doesFileExist cacheFile
  if hasCache
    then do
      cachedBody <- readFile cacheFile
      pure (parseSnapshotConstraints cachedBody)
    else
      if offline
        then pure (Left ("Snapshot missing from cache in offline mode: " ++ snapshot))
        else do
          let url = "https://www.stackage.org/" ++ snapshot ++ "/cabal.config"
          fetched <- try (readProcess "curl" ["-s", "-f", url] "")
          case fetched of
            Left err -> pure (Left (displayException (err :: SomeException)))
            Right body ->
              case parseSnapshotConstraints body of
                Left parseErr -> pure (Left parseErr)
                Right specs -> do
                  writeFile cacheFile body
                  pure (Right specs)

snapshotCacheFile :: String -> IO FilePath
snapshotCacheFile snapshot = do
  base <- getXdgDirectory XdgCache "aihc"
  let dir = base </> "stackage"
      file = sanitizeSnapshotName snapshot ++ "-cabal.config"
  createDirectoryIfMissing True dir
  pure (dir </> file)

sanitizeSnapshotName :: String -> String
sanitizeSnapshotName = map sanitizeChar
  where
    sanitizeChar c
      | isAlphaNum c || c == '-' || c == '_' = c
      | otherwise = '_'

parseSnapshotConstraints :: String -> Either String [PackageSpec]
parseSnapshotConstraints content = do
  let section = constraintLines (lines content)
      entries = map trim (splitComma (concat section))
      specs = mapMaybe parseConstraint entries
  if null specs
    then Left "No package constraints found"
    else Right specs

constraintLines :: [String] -> [String]
constraintLines ls =
  case break (isPrefixOf "constraints:" . trimLeft) ls of
    (_, []) -> []
    (_, firstRaw : restRaw) ->
      let firstLine = trimLeft firstRaw
          start = [drop 12 firstLine]
          cont = [trimLeft line | line <- takeWhile isConstraintContinuation restRaw]
       in start <> cont

isConstraintContinuation :: String -> Bool
isConstraintContinuation line =
  case line of
    c : _ -> isSpace c
    [] -> False

trimLeft :: String -> String
trimLeft = dropWhile isSpace

parseConstraint :: String -> Maybe PackageSpec
parseConstraint entry
  | null entry = Nothing
  | "--" `isPrefixOf` trim entry = Nothing
  | otherwise =
      case breakOn "==" entry of
        Just (name, ver) -> Just (PackageSpec (trim name) (trim ver))
        Nothing ->
          let ws = words entry
           in case ws of
                -- Snapshot constraints like "base installed" refer to compiler-provided
                -- packages and do not map to downloadable Hackage tarballs.
                [_, "installed"] -> Nothing
                _ -> Nothing

breakOn :: String -> String -> Maybe (String, String)
breakOn needle haystack =
  case findNeedle needle haystack of
    Nothing -> Nothing
    Just i ->
      let (left, right) = splitAt i haystack
       in Just (left, drop (length needle) right)

findNeedle :: String -> String -> Maybe Int
findNeedle needle = go 0
  where
    go _ [] = Nothing
    go i xs
      | needle `isPrefixOf` xs = Just i
      | otherwise = go (i + 1) (drop 1 xs)

runPackage :: Options -> PackageSpec -> IO PackageResult
runPackage opts spec = do
  result <- try (runPackageOrThrow opts spec)
  pure $ case result of
    Left err ->
      PackageResult
        { package = spec,
          packageOursOk = False,
          packageHseOk = False,
          packageGhcOk = False,
          packageReason = displayException (err :: SomeException),
          packageGhcError = Nothing,
          packageSourceSize = 0
        }
    Right pkgResult -> pkgResult

runPackageOrThrow :: Options -> PackageSpec -> IO PackageResult
runPackageOrThrow opts spec = do
  if pkgVersion spec == "installed"
    then
      pure
        PackageResult
          { package = spec,
            packageOursOk = False,
            packageHseOk = False,
            packageGhcOk = False,
            packageReason = "installed package has no downloadable snapshot version",
            packageGhcError = Nothing,
            packageSourceSize = 0
          }
    else do
      srcDir <- downloadPackageQuietWithNetwork (not (optOffline opts)) (pkgName spec) (pkgVersion spec)
      files <- findTargetFilesFromCabal srcDir
      totalSize <- if optPrintFailedTable opts then totalSourceSize files else pure 0
      if null files
        then
          pure
            PackageResult
              { package = spec,
                packageOursOk = True,
                packageHseOk = True,
                packageGhcOk = True,
                packageReason = "",
                packageGhcError = Nothing,
                packageSourceSize = totalSize
              }
        else do
          fileSummary <- foldFilesForPackage opts srcDir emptyFileSummary files
          let hseOk = packageFileHseOk fileSummary
              ghcOk = packageFileGhcOk fileSummary
              ghcError = packageFileGhcError fileSummary
              oursOk = packageFileOursOk fileSummary
          if oursOk
            then
              pure
                PackageResult
                  { package = spec,
                    packageOursOk = True,
                    packageHseOk = hseOk,
                    packageGhcOk = ghcOk,
                    packageReason = "",
                    packageGhcError = ghcError,
                    packageSourceSize = totalSize
                  }
            else
              pure
                PackageResult
                  { package = spec,
                    packageOursOk = False,
                    packageHseOk = hseOk,
                    packageGhcOk = ghcOk,
                    packageReason = firstFailureMessage fileSummary,
                    packageGhcError = ghcError,
                    packageSourceSize = totalSize
                  }

data FileResult = FileResult
  { fileOursOk :: Bool,
    fileHseOk :: Bool,
    fileGhcOk :: Bool,
    fileError :: Maybe String,
    fileGhcError :: Maybe String
  }

data PackageFileSummary = PackageFileSummary
  { packageFileOursOk :: !Bool,
    packageFileHseOk :: !Bool,
    packageFileGhcOk :: !Bool,
    packageFileFirstFailure :: Maybe String,
    packageFileGhcError :: Maybe String
  }

emptyFileSummary :: PackageFileSummary
emptyFileSummary =
  PackageFileSummary
    { packageFileOursOk = True,
      packageFileHseOk = True,
      packageFileGhcOk = True,
      packageFileFirstFailure = Nothing,
      packageFileGhcError = Nothing
    }

checkAndAccumulateFile :: Options -> FilePath -> PackageFileSummary -> FileInfo -> IO PackageFileSummary
checkAndAccumulateFile opts packageRoot summary info = do
  result <- checkFile opts packageRoot info
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
  pure
    PackageFileSummary
      { packageFileOursOk = oursOk,
        packageFileHseOk = hseOk,
        packageFileGhcOk = ghcOk,
        packageFileFirstFailure = firstFailure,
        packageFileGhcError = ghcError
      }

firstFailureMessage :: PackageFileSummary -> String
firstFailureMessage summary =
  fromMaybe "unknown failure" (packageFileFirstFailure summary)

foldFilesForPackage :: Options -> FilePath -> PackageFileSummary -> [FileInfo] -> IO PackageFileSummary
foldFilesForPackage _ _ summary [] = pure summary
foldFilesForPackage opts packageRoot summary (info : rest)
  | shouldStopAfterFailure opts summary = pure summary
  | otherwise = do
      summary' <- checkAndAccumulateFile opts packageRoot summary info
      foldFilesForPackage opts packageRoot summary' rest

shouldStopAfterFailure :: Options -> PackageFileSummary -> Bool
shouldStopAfterFailure opts summary =
  not (packageFileOursOk summary) && not (needsFullPackageScan opts)

needsFullPackageScan :: Options -> Bool
needsFullPackageScan opts =
  CheckHse `elem` optChecks opts || CheckGhc `elem` optChecks opts

checkFile :: Options -> FilePath -> FileInfo -> IO FileResult
checkFile opts packageRoot info = do
  let file = fileInfoPath info
      parserExts = GhcOracle.extensionNamesToParserExtensions (fileInfoExtensions info)
      parserConfig = Aihc.Parser.defaultConfig {Aihc.Parser.parserExtensions = parserExts}
  source <- readTextFileLenient file
  preprocessed <- preprocessForParserIfEnabled (fileInfoExtensions info) (fileInfoCppOptions info) file (resolveIncludeBestEffort packageRoot file) source
  let source' = resultOutput preprocessed
      cppErrors = [diagToText diag | diag <- resultDiagnostics preprocessed, diagSeverity diag == Error]
      cppErrorMsg =
        if null cppErrors
          then Nothing
          else Just (T.intercalate "\n" cppErrors)
      oursResult = Aihc.Parser.parseModule parserConfig source'

  oursStatus <- case oursResult of
    ParseErr err ->
      if CheckParse `elem` optChecks opts || needsParsedModule (optChecks opts)
        then pure (Left (T.unpack (prefixCppErrors cppErrorMsg ("parse failed in " <> T.pack file <> ":\n" <> T.pack (Aihc.Parser.errorBundlePretty err)))))
        else pure (Right ())
    ParseOk _parsed -> do
      roundtripRes <-
        if CheckRoundtripGhc `elem` optChecks opts
          then pure (checkRoundtrip (fileInfoExtensions info) (fileInfoLanguage info) file cppErrorMsg source')
          else pure (Right ())
      case roundtripRes of
        Left err -> pure (Left err)
        Right () -> pure (Right ())

  hseOk <-
    if CheckHse `elem` optChecks opts
      then pure $ checkHse (fileInfoExtensions info) (fileInfoLanguage info) source'
      else pure True

  ghcOkResult <-
    if CheckGhc `elem` optChecks opts
      then pure $ GhcOracle.oracleDetailedParsesModuleWithNamesAt file (fileInfoExtensions info) (fileInfoLanguage info) source'
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

checkHse :: [String] -> Maybe String -> Text -> Bool
checkHse extNames _langName source =
  let mode = hseParseMode {HSE.extensions = fromExtensionNames extNames}
   in case HSE.parseFileContentsWithMode mode (T.unpack source) of
        HSE.ParseOk _ -> True
        HSE.ParseFailed _ _ -> False

hseParseMode :: HSE.ParseMode
hseParseMode =
  HSE.defaultParseMode
    { HSE.parseFilename = "<stackage-progress>",
      HSE.extensions = []
    }

needsParsedModule :: [Check] -> Bool
needsParsedModule checks =
  CheckRoundtripGhc `elem` checks

checkRoundtrip :: [String] -> Maybe String -> FilePath -> Maybe Text -> Text -> Either String ()
checkRoundtrip extNames langName file cppErrorMsg source' =
  case validateParserDetailedWithExtensionNames extNames langName source' of
    Nothing -> Right ()
    Just err ->
      case validationErrorKind err of
        ValidationParseError ->
          Left (T.unpack (prefixCppErrors cppErrorMsg ("parse failed in " <> T.pack file <> ": " <> T.pack (validationErrorMessage err))))
        ValidationRoundtripError ->
          Left (T.unpack (prefixCppErrors cppErrorMsg ("roundtrip mismatch in " <> T.pack file <> ": " <> T.pack (validationErrorMessage err))))

foldConcurrentlyChunksWithProgress :: Int -> (a -> IO PackageResult) -> [a] -> Int -> Bool -> SummaryOptions -> Bool -> IO (RunSummary, [PromptCandidate])
foldConcurrentlyChunksWithProgress n action items total showProgress opts collectPromptCandidates =
  go 0 0 emptySummary [] (chunksOf chunkSize items)
  where
    chunkSize = if n <= 0 then 1 else n
    go _ _ summary promptCandidatesRev [] = pure (finalizeSummary summary, reverse promptCandidatesRev)
    go done success summary promptCandidatesRev (chunk : rest) = do
      batch <- mapConcurrently action chunk
      let done' = done + length batch
          success' = success + length [() | result <- batch, packageOursOk result]
          !summary' = addPackageResults opts batch summary
          !promptCandidatesRev' =
            if collectPromptCandidates
              then reverse (mapMaybe promptCandidateFromResult batch) <> promptCandidatesRev
              else promptCandidatesRev
      when showProgress (putProgressLine (ProgressState done' success' total))
      go done' success' summary' promptCandidatesRev' rest

pickPromptCandidate :: Maybe Int -> [PromptCandidate] -> IO (Maybe PromptCandidate)
pickPromptCandidate maybeSeed candidates = do
  picker <-
    case maybeSeed of
      Just seed -> pure (toInteger seed)
      Nothing -> toInteger <$> getMonotonicTimeNSec
  pure (selectPromptCandidate picker candidates)

loadPromptTemplate :: IO String
loadPromptTemplate = do
  cwd <- getCurrentDirectory
  path <- findPromptTemplatePath cwd
  case path of
    Just promptPath -> readFile promptPath
    Nothing -> do
      hPutStrLn stderr ("Could not find prompt template docs/PKG_FIX_PROMPT.md (cwd: " ++ cwd ++ ")")
      exitFailure

findPromptTemplatePath :: FilePath -> IO (Maybe FilePath)
findPromptTemplatePath = go
  where
    go dir = do
      let candidate = dir </> "docs" </> "PKG_FIX_PROMPT.md"
      exists <- doesFileExist candidate
      if exists
        then pure (Just candidate)
        else
          let parent = takeDirectory dir
           in if parent == dir
                then pure Nothing
                else go parent

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
  let (chunk, rest) = splitAt n xs
   in chunk : chunksOf n rest

data ProgressState = ProgressState
  { progressDone :: Int,
    progressSuccess :: Int,
    progressTotal :: Int
  }

putProgressLine :: ProgressState -> IO ()
putProgressLine p =
  do
    putStr
      ( "\r"
          ++ show (progressSuccess p)
          ++ "/"
          ++ show (progressTotal p)
          ++ " ("
          ++ show (progressDone p)
          ++ "/"
          ++ show (progressTotal p)
          ++ " processed)"
      )
    hFlush stdout

pct :: Int -> Int -> Int
pct _ 0 = 100
pct n total = (n * 100) `div` total

summaryOptions :: Options -> SummaryOptions
summaryOptions opts =
  SummaryOptions
    { summaryKeepSucceeded = optPrintSucceeded opts,
      summaryKeepFailedPackages = optPrintFailedTable opts,
      summaryGhcErrorLimit =
        case optGhcErrorsFile opts of
          Just _ -> optGhcErrorsLimit opts
          Nothing -> 0
    }
