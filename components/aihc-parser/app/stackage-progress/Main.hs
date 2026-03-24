{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (when)
import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Conc (getNumProcessors)
import StackageProgress.CLI
  ( Options (..),
    parseOptionsIO,
    summaryOptionsFromOptions,
  )
import StackageProgress.PackageRunner (runPackage)
import StackageProgress.Snapshot (loadStackageSnapshotWithMode)
import StackageProgress.Summary
  ( FailedPackage (..),
    PackageResult (..),
    PromptCandidate,
    RunSummary,
    SummaryOptions,
    addPackageResults,
    emptySummary,
    finalizeSummary,
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
import System.Directory (doesFileExist, getCurrentDirectory, getHomeDirectory)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (takeDirectory, (</>))
import System.IO (hFlush, hIsTerminalDevice, hPutStrLn, stderr, stdout)

main :: IO ()
main = do
  opts <- parseOptionsIO

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
      (summaryOptionsFromOptions opts)
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

foldConcurrentlyChunksWithProgress ::
  Int ->
  (a -> IO PackageResult) ->
  [a] ->
  Int ->
  Bool ->
  SummaryOptions ->
  Bool ->
  IO (RunSummary, [PromptCandidate])
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
