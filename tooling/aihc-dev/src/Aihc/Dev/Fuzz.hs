{-# LANGUAGE NamedFieldPuns #-}

-- | Parallel, round-robin QuickCheck fuzz runner with a terminal dashboard.
module Aihc.Dev.Fuzz
  ( batchSize,
    dashboardRefreshMicroseconds,
    runCommand,
    selectProperties,
  )
where

import Aihc.Dev.Fuzz.CLI (Command (..), Options (..), Selection (..))
import Aihc.Dev.Fuzz.Registry (FuzzProperty (..), fuzzProperties, fuzzPropertyId)
import Aihc.Dev.Fuzz.TUI (Dashboard (..), renderDashboard, renderFrameUpdate)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel, race, waitAnyCatch)
import Control.Concurrent.STM
import Control.Exception (SomeException, bracket_, displayException, finally, onException)
import Control.Monad (forM, forM_, unless, when)
import Data.Char (toLower)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List (groupBy, intercalate, isInfixOf, sortOn)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import GHC.Conc (getNumCapabilities)
import System.Console.Terminal.Size qualified as Terminal
import System.Exit (exitFailure)
import System.IO (BufferMode (NoBuffering), hFlush, hGetBuffering, hIsTerminalDevice, hPutStr, hPutStrLn, hSetBuffering, stderr)
import Test.QuickCheck qualified as QC
import Test.QuickCheck.Property qualified as QCP

-- | Number of successful cases a worker runs before rotating the property.
batchSize :: Int
batchSize = 10000

-- | Keep dashboard redraws deliberately infrequent to avoid terminal flicker.
dashboardRefreshMicroseconds :: Int
dashboardRefreshMicroseconds = 5 * 1000000

data ActiveProperty = ActiveProperty
  { activePropertyId :: String,
    activeSuccessfulCases :: Int
  }

data Stats = Stats
  { statsActive :: TVar (IntMap ActiveProperty),
    statsCompletedBatches :: TVar Int,
    statsSuccessfulCases :: TVar Int
  }

data WorkerFailure = WorkerFailure
  { workerFailureProperty :: FuzzProperty,
    workerFailureResult :: QC.Result
  }

runCommand :: Command -> IO ()
runCommand command =
  case command of
    List selection -> listProperties (selectProperties selection fuzzProperties)
    Run options -> runFuzzer options

-- | Apply case-insensitive substring inclusion and exclusion rules.
selectProperties :: Selection -> [FuzzProperty] -> [FuzzProperty]
selectProperties Selection {selectionFilters, selectionExclusions} =
  filter selected
  where
    selected fuzzProperty =
      let propertyId = lowercase (fuzzPropertyId fuzzProperty)
          matches term = lowercase term `isInfixOf` propertyId
       in (null selectionFilters || any matches selectionFilters)
            && not (any matches selectionExclusions)
    lowercase = map toLower

listProperties :: [FuzzProperty] -> IO ()
listProperties properties =
  forM_ grouped printComponent
  where
    grouped =
      groupBy sameComponent . sortOn fuzzPropertyComponent $ properties
    sameComponent left right =
      fuzzPropertyComponent left == fuzzPropertyComponent right
    printComponent [] = pure ()
    printComponent componentProperties@(firstProperty : _) = do
      putStrLn (fuzzPropertyComponent firstProperty)
      forM_ componentProperties $ \fuzzProperty ->
        putStrLn ("  " <> fuzzPropertyName fuzzProperty)

runFuzzer :: Options -> IO ()
runFuzzer Options {optionsSelection, optionsJobs, optionsTimeLimit} = do
  let selected = selectProperties optionsSelection fuzzProperties
  when (null selected) $ do
    hPutStrLn stderr "aihc-dev fuzz: no properties matched the selection"
    exitFailure
  capabilities <- getNumCapabilities
  let requestedJobs = fromMaybe capabilities optionsJobs
      jobs = min requestedJobs (length selected)
  queue <- newTQueueIO
  atomically (forM_ selected (writeTQueue queue))
  stats <- Stats <$> newTVarIO IntMap.empty <*> newTVarIO 0 <*> newTVarIO 0
  startedAt <- getCurrentTime
  workers <- forM [1 .. jobs] $ \workerId -> async (worker queue stats workerId)
  interactive <- hIsTerminalDevice stderr
  let run = awaitOutcome optionsTimeLimit workers
  outcome <-
    ( if interactive
        then withDashboard startedAt (length selected) jobs stats run
        else do
          hPutStrLn stderr ("Fuzzing " <> show (length selected) <> " properties with " <> show jobs <> " jobs (10,000 cases per batch)")
          run
    )
      `onException` mapM_ cancel workers
  mapM_ cancel workers
  successfulCases <- readTVarIO (statsSuccessfulCases stats)
  case outcome of
    TimeLimitReached ->
      putStrLn ("Time limit reached after " <> formatInteger successfulCases <> " successful QuickCheck cases.")
    PropertyFailed WorkerFailure {workerFailureProperty, workerFailureResult} -> do
      hPutStrLn stderr ("Fuzz failure in " <> fuzzPropertyId workerFailureProperty)
      hPutStr stderr (QC.output workerFailureResult)
      exitFailure
    WorkerCrashed exception -> do
      hPutStrLn stderr ("Fuzz worker crashed: " <> displayException exception)
      exitFailure

data Outcome
  = TimeLimitReached
  | PropertyFailed WorkerFailure
  | WorkerCrashed SomeException

awaitOutcome :: Maybe NominalDiffTime -> [Async WorkerFailure] -> IO Outcome
awaitOutcome maybeTimeLimit workers =
  case maybeTimeLimit of
    Nothing -> waitForWorker
    Just timeLimit -> do
      result <- race (threadDelay (durationMicroseconds timeLimit)) waitForWorker
      pure $ case result of
        Left () -> TimeLimitReached
        Right outcome -> outcome
  where
    waitForWorker = do
      (_, result) <- waitAnyCatch workers
      pure $ case result of
        Left exception -> WorkerCrashed exception
        Right failure -> PropertyFailed failure

durationMicroseconds :: NominalDiffTime -> Int
durationMicroseconds duration =
  max 1 (floor (realToFrac duration * (1000000 :: Double)))

worker :: TQueue FuzzProperty -> Stats -> Int -> IO WorkerFailure
worker queue stats workerId = do
  fuzzProperty <- atomically (readTQueue queue)
  let propertyId = fuzzPropertyId fuzzProperty
      clearActive = atomically (modifyTVar' (statsActive stats) (IntMap.delete workerId))
  atomically $ modifyTVar' (statsActive stats) (IntMap.insert workerId (ActiveProperty propertyId 0))
  result <- runBatch stats workerId fuzzProperty `finally` clearActive
  case result of
    QC.Success {} -> do
      atomically $ do
        modifyTVar' (statsCompletedBatches stats) (+ 1)
        writeTQueue queue fuzzProperty
      worker queue stats workerId
    _ -> pure (WorkerFailure fuzzProperty result)

runBatch :: Stats -> Int -> FuzzProperty -> IO QC.Result
runBatch stats workerId FuzzProperty {fuzzPropertyValue} =
  QC.quickCheckWithResult
    QC.stdArgs
      { QC.chatty = False,
        QC.maxSuccess = batchSize
      }
    (QCP.callback progressCallback fuzzPropertyValue)
  where
    progressCallback =
      QCP.PostTest QCP.NotCounterexample $ \_ result ->
        when (QCP.ok result == Just True) $ atomically $ do
          modifyTVar' (statsSuccessfulCases stats) (+ 1)
          modifyTVar' (statsActive stats) $ IntMap.adjust increment workerId
    increment active =
      active {activeSuccessfulCases = activeSuccessfulCases active + 1}

withDashboard :: UTCTime -> Int -> Int -> Stats -> IO Outcome -> IO Outcome
withDashboard startedAt propertyCount jobs stats action = do
  originalBuffering <- hGetBuffering stderr
  bracket_
    enterDashboard
    (leaveDashboard originalBuffering)
    ( do
        renderer <- async (renderLoop startedAt propertyCount jobs stats)
        action `finally` cancel renderer
    )

enterDashboard :: IO ()
enterDashboard = do
  hSetBuffering stderr NoBuffering
  hPutStr stderr "\ESC[?1049h\ESC[2J\ESC[H\ESC[?25l"

leaveDashboard :: BufferMode -> IO ()
leaveDashboard originalBuffering = do
  hPutStr stderr "\ESC[?25h\ESC[?1049l"
  hSetBuffering stderr originalBuffering

renderLoop :: UTCTime -> Int -> Int -> Stats -> IO ()
renderLoop startedAt propertyCount jobs stats = go []
  where
    go previousFrame = do
      now <- getCurrentTime
      active <- readTVarIO (statsActive stats)
      successfulCases <- readTVarIO (statsSuccessfulCases stats)
      completedBatches <- readTVarIO (statsCompletedBatches stats)
      terminalSize <- Terminal.size
      let (rows, columns) =
            case terminalSize of
              Just window -> (Terminal.height window, Terminal.width window)
              Nothing -> (24, 80)
          currentFrame =
            renderDashboard
              rows
              columns
              Dashboard
                { dashboardActive =
                    [ (workerId, activePropertyId, activeSuccessfulCases)
                    | (workerId, ActiveProperty {activePropertyId, activeSuccessfulCases}) <- IntMap.toAscList active
                    ],
                  dashboardBatches = completedBatches,
                  dashboardCases = successfulCases,
                  dashboardElapsed = diffUTCTime now startedAt,
                  dashboardJobs = jobs,
                  dashboardProperties = propertyCount
                }
          update = renderFrameUpdate rows previousFrame currentFrame
      unless (null update) $ hPutStr stderr update >> hFlush stderr
      threadDelay dashboardRefreshMicroseconds
      go currentFrame

formatInteger :: Int -> String
formatInteger value =
  reverse . intercalate "," . chunksOfThree . reverse $ show value
  where
    chunksOfThree [] = []
    chunksOfThree input = take 3 input : chunksOfThree (drop 3 input)
