{-# LANGUAGE NamedFieldPuns #-}

-- | Parallel, weighted Hedgehog fuzz runner with a bug store and a dashboard.
module Aihc.Dev.Fuzz
  ( runCommand,
  )
where

import Aihc.Dev.Fuzz.Bugs
import Aihc.Dev.Fuzz.CLI (BugsOptions (..), Command (..), Options (..), ReplayOptions (..))
import Aihc.Dev.Fuzz.Follow (Relaunch, followLoop, relaunch, startFollower)
import Aihc.Dev.Fuzz.Model
import Aihc.Dev.Fuzz.Registry (FuzzProperty (..), fuzzProperties, fuzzPropertyId)
import Aihc.Dev.Fuzz.Report (reporterLoop)
import Aihc.Dev.Fuzz.Schedule (batchSize, cycleProperties, pickProperty, resolveJobCount, restSeconds, selectProperties)
import Aihc.Dev.Fuzz.TUI (Model (..), runDashboard)
import Aihc.Testing.RuntimeArchive (releaseCachedRuntimeArchives)
import Brick.BChan (newBChan, writeBChan, writeBChanNonBlocking)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel, poll, race, wait, waitAnyCatch)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.STM
import Control.Exception (ErrorCall (..), SomeException, displayException, finally, toException)
import Control.Monad (forM, forM_, forever, unless, void, when)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List (find, groupBy, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import GHC.Conc (getNumCapabilities)
import Hedgehog (withTests)
import Hedgehog.Internal.Config (UseColor (DisableColor))
import Hedgehog.Internal.Property (Property (..), PropertyName (..), Skip (..), TestCount (..), skipCompress, withSkip)
import Hedgehog.Internal.Report (FailedAnnotation (..), FailureReport (..), Progress (Running), Report (..), Result (..), renderResult)
import Hedgehog.Internal.Runner (checkReport)
import Hedgehog.Internal.Seed qualified as Seed
import Hedgehog.Internal.Source (Span (..), unLineNo)
import System.Exit (exitFailure)
import System.IO (hIsTerminalDevice, hPutStrLn, stderr, stdin, stdout)

-- | Update counters and spinners ten times per second.
dashboardRefreshMicroseconds :: Int
dashboardRefreshMicroseconds = 100000

data Stats = Stats
  { statsActive :: TVar (IntMap String),
    statsCompletedBatches :: TVar Int,
    statsSuccessfulCases :: TVar Int,
    statsPropertyCases :: TVar (Map String Int),
    statsBugs :: TVar [Bug],
    statsNewBugs :: TVar (Set String),
    statsResting :: TVar (Map String UTCTime),
    statsReporter :: TVar ReporterStatus,
    statsFollow :: TVar FollowStatus,
    statsStoreLock :: MVar (),
    statsStorePath :: FilePath,
    statsAnnounce :: Announcement -> IO ()
  }

runCommand :: Command -> IO ()
runCommand command =
  case command of
    List selection -> listProperties (selectProperties selection fuzzProperties)
    Run options -> do
      -- Release the archives before a relaunch, because the new process
      -- does not run the handlers of this process.
      next <- runFuzzer options `finally` releaseCachedRuntimeArchives
      mapM_ relaunch next
    Bugs options -> runBugs options
    Replay options -> runReplay options `finally` releaseCachedRuntimeArchives

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
        putStrLn ("  " <> fuzzPropertyName fuzzProperty <> "  [weight " <> show (fromRational (fuzzPropertyWeight fuzzProperty) :: Double) <> "]")

resolveStorePath :: Maybe FilePath -> IO FilePath
resolveStorePath = maybe bugStorePath pure

loadStore :: FilePath -> IO [Bug]
loadStore path = do
  loaded <- loadBugs path
  case loaded of
    Right bugs -> pure bugs
    Left reason -> do
      hPutStrLn stderr ("aihc-dev fuzz: cannot read the bug store " <> path <> ": " <> reason)
      exitFailure

runBugs :: BugsOptions -> IO ()
runBugs BugsOptions {bugsForget, bugsClear, bugsBugFile} = do
  path <- resolveStorePath bugsBugFile
  bugs <- loadStore path
  let kept
        | bugsClear = []
        | otherwise = [bug | bug <- bugs, not (any (`matchesBug` bug) bugsForget)]
  when (length kept /= length bugs) (saveBugs path kept)
  printBugs ReporterIdle Set.empty kept
  putStrLn ("Bug store: " <> path)
  where
    matchesBug prefix bug = not (null prefix) && take (length prefix) (bugId bug) == prefix

printBugs :: ReporterStatus -> Set String -> [Bug] -> IO ()
printBugs reporter newBugs bugs
  | null bugs = putStrLn "No known bugs."
  | otherwise = do
      putStrLn ("Known bugs: " <> show (length bugs) <> " (" <> show (Set.size newBugs) <> " new)")
      forM_ bugs $ \bug ->
        putStrLn $
          "  "
            <> (if bugId bug `Set.member` newBugs then "● " else "○ ")
            <> take 12 (bugId bug)
            <> "  "
            <> padRight 40 (bugProperty bug)
            <> "  "
            <> padLeft 5 (show (bugHits bug))
            <> " hits  "
            <> formatSeen (bugFirstSeen bug)
            <> "  "
            <> maybe (bugStatus reporter bug) issueUrl (bugIssue bug)
  where
    padRight width text = text <> replicate (width - length text) ' '
    padLeft width text = replicate (width - length text) ' ' <> text

runReplay :: ReplayOptions -> IO ()
runReplay ReplayOptions {replayProperty, replayArgument} = do
  fuzzProperty <- case find ((== replayProperty) . fuzzPropertyId) fuzzProperties of
    Just found -> pure found
    Nothing -> do
      hPutStrLn stderr ("aihc-dev fuzz: unknown property " <> show replayProperty)
      hPutStrLn stderr "Known properties:"
      forM_ fuzzProperties (hPutStrLn stderr . ("  " <>) . fuzzPropertyId)
      exitFailure
  (skip, seed) <- case parseReplay replayArgument of
    Right parsed -> pure parsed
    Left reason -> hPutStrLn stderr ("aihc-dev fuzz: " <> reason) >> exitFailure
  withFuzzProperty fuzzProperty $ \propertyValue -> do
    let configured = withSkip skip (withTests (fromIntegral batchSize) propertyValue)
    report <- checkReport (propertyConfig configured) 0 seed (propertyTest configured) (const (pure ()))
    rendered <- renderResult DisableColor (Just (PropertyName (fuzzPropertyId fuzzProperty))) report
    putStrLn rendered
    case reportStatus report of
      OK -> pure ()
      _ -> exitFailure

-- | Fuzz until the run stops. Give the new executable when a build of a
-- newer commit stopped the run.
runFuzzer :: Options -> IO (Maybe Relaunch)
runFuzzer Options {optionsSelection, optionsJobs, optionsTimeLimit, optionsRepository, optionsReport, optionsBugFile, optionsFollow} = do
  let selected = selectProperties optionsSelection fuzzProperties
  when (null selected) $ do
    hPutStrLn stderr "aihc-dev fuzz: no properties matched the selection"
    exitFailure
  capabilities <- getNumCapabilities
  let jobs = resolveJobCount capabilities optionsJobs
  storePath <- resolveStorePath optionsBugFile
  knownBugs <- loadStore storePath
  follower <-
    if optionsFollow
      then startFollower >>= either (\reason -> hPutStrLn stderr ("aihc-dev fuzz: " <> reason) >> exitFailure) (pure . Just)
      else pure Nothing
  interactive <- (&&) <$> hIsTerminalDevice stdin <*> hIsTerminalDevice stdout
  scheduler <- newTVarIO (cycleProperties [(fuzzPropertyWeight item, item) | item <- selected])
  stats <-
    Stats
      <$> newTVarIO IntMap.empty
      <*> newTVarIO 0
      <*> newTVarIO 0
      <*> newTVarIO (Map.fromList [(fuzzPropertyId item, 0) | item <- selected])
      <*> newTVarIO knownBugs
      <*> newTVarIO Set.empty
      <*> newTVarIO Map.empty
      <*> newTVarIO ReporterIdle
      <*> newTVarIO (maybe FollowOff (const FollowStarting) follower)
      <*> newMVar ()
      <*> pure storePath
      <*> (if interactive then pure (const (pure ())) else announceToLog <$> newMVar ())
  -- Write the header before a thread can write to the log.
  unless interactive $
    hPutStrLn stderr ("Fuzzing " <> show (length selected) <> " properties with " <> show jobs <> " jobs (" <> formatInteger batchSize <> " cases per batch)")
  startedAt <- getCurrentTime
  workers <- forM [1 .. jobs] $ \workerId -> async (worker scheduler stats (length selected) workerId)
  reporter <-
    if optionsReport
      then Just <$> async (reporterLoop optionsRepository (statsBugs stats) (statsReporter stats) (persistBugs stats) (statsAnnounce stats . ReportedBug) (statsAnnounce stats . ReporterChanged))
      else pure Nothing
  builder <- forM follower $ \follow ->
    async (followLoop follow (reportFollow stats))
  let stopAll = mapM_ cancel builder >> mapM_ cancel reporter >> mapM_ cancel workers
  outcome <-
    ( if interactive
        then runInteractive startedAt jobs optionsTimeLimit selected stats workers builder
        else runHeadless optionsTimeLimit workers builder
    )
      `finally` stopAll
  successfulCases <- readTVarIO (statsSuccessfulCases stats)
  bugs <- readTVarIO (statsBugs stats)
  newBugs <- readTVarIO (statsNewBugs stats)
  reporterStatus <- readTVarIO (statsReporter stats)
  case outcome of
    TimeLimitReached ->
      putStrLn ("Time limit reached after " <> formatInteger successfulCases <> " successful Hedgehog cases.")
    UserQuit ->
      putStrLn ("Stopped after " <> formatInteger successfulCases <> " successful Hedgehog cases.")
    WorkerCrashed exception ->
      putStrLn ("Fuzz worker crashed after " <> formatInteger successfulCases <> " successful Hedgehog cases: " <> displayException exception)
    NewBuildReady ->
      putStrLn ("A new build is ready after " <> formatInteger successfulCases <> " successful Hedgehog cases.")
  printBugs reporterStatus newBugs bugs
  putStrLn ("Bug store: " <> storePath)
  let crashed = case outcome of
        WorkerCrashed _ -> True
        _ -> False
  ready <- maybe (pure Nothing) poll builder
  case (outcome, ready) of
    (NewBuildReady, Just (Right next)) -> pure (Just next)
    _ -> do
      when (crashed || not (Set.null newBugs)) exitFailure
      pure Nothing

reportFollow :: Stats -> FollowStatus -> IO ()
reportFollow stats status = do
  previous <- atomically (swapTVar (statsFollow stats) status)
  when (previous /= status) (statsAnnounce stats (FollowChanged status))

runInteractive :: UTCTime -> Int -> Maybe NominalDiffTime -> [FuzzProperty] -> Stats -> [Async ()] -> Maybe (Async Relaunch) -> IO Outcome
runInteractive startedAt jobs timeLimit selected stats workers builder = do
  channel <- newBChan 16
  initial <- snapshot stats
  ticker <- async $ forever $ do
    current <- snapshot stats
    void (writeBChanNonBlocking channel (Tick current))
    threadDelay dashboardRefreshMicroseconds
  limiter <- forM timeLimit $ \limit -> async $ do
    threadDelay (durationMicroseconds limit)
    writeBChan channel TimeLimitEvent
  watcher <- async $ do
    exception <- awaitCrash workers
    writeBChan channel (Crashed exception)
  announcer <- forM builder $ \build -> async $ do
    _ <- wait build
    writeBChan channel BuildReady
  let helpers = [ticker, watcher] <> maybe [] pure limiter <> maybe [] pure announcer
  runDashboard
    channel
    Model
      { modelStartedAt = startedAt,
        modelJobs = jobs,
        modelTimeLimit = timeLimit,
        modelProperties = selected,
        modelStorePath = statsStorePath stats,
        modelSnapshot = initial,
        modelRates = Map.empty,
        modelOutcome = Nothing
      }
    `finally` mapM_ cancel helpers

runHeadless :: Maybe NominalDiffTime -> [Async ()] -> Maybe (Async Relaunch) -> IO Outcome
runHeadless timeLimit workers builder =
  foldr1 (\left right -> either id id <$> race left right) $
    [WorkerCrashed <$> awaitCrash workers]
      <> [TimeLimitReached <$ threadDelay (durationMicroseconds limit) | Just limit <- [timeLimit]]
      <> [NewBuildReady <$ wait build | Just build <- [builder]]

-- | Wait until a worker stops. A worker only stops with an exception.
awaitCrash :: [Async ()] -> IO SomeException
awaitCrash workers = do
  (_, result) <- waitAnyCatch workers
  pure $ case result of
    Left exception -> exception
    Right () -> toException (ErrorCall "a fuzz worker stopped without a failure")

snapshot :: Stats -> IO Snapshot
snapshot stats = do
  now <- getCurrentTime
  atomically $
    Snapshot now . IntMap.toAscList
      <$> readTVar (statsActive stats)
      <*> readTVar (statsPropertyCases stats)
      <*> readTVar (statsSuccessfulCases stats)
      <*> readTVar (statsCompletedBatches stats)
      <*> readTVar (statsBugs stats)
      <*> readTVar (statsNewBugs stats)
      <*> readTVar (statsReporter stats)
      <*> readTVar (statsFollow stats)

-- | Write one announcement at a time, so that workers do not mix their lines.
announceToLog :: MVar () -> Announcement -> IO ()
announceToLog lock announcement = withMVar lock $ \() ->
  case announcement of
    FoundBug bug True -> do
      hPutStrLn stderr ("✗ new bug " <> take 12 (bugId bug) <> " in " <> bugProperty bug)
      hPutStrLn stderr (failureReport (bugFailure bug))
      hPutStrLn stderr ("  replay: " <> replayCommand (bugFailure bug))
    FoundBug bug False ->
      hPutStrLn stderr ("· known bug " <> take 12 (bugId bug) <> " in " <> bugProperty bug <> " hit again (" <> show (bugHits bug) <> " hits)")
    ReportedBug bug ->
      hPutStrLn stderr ("↑ reported bug " <> take 12 (bugId bug) <> " as " <> maybe "?" issueUrl (bugIssue bug))
    ReporterChanged status ->
      hPutStrLn stderr ("  reporter: " <> describeReporter status)
    FollowChanged status ->
      forM_ (describeFollow status) (hPutStrLn stderr . ("  " <>))

durationMicroseconds :: NominalDiffTime -> Int
durationMicroseconds duration =
  max 1 (floor (realToFrac duration * (1000000 :: Double)))

-- | Take the next property that does not rest. If each property rests,
-- wait until the rest of the first one ends.
nextProperty :: TVar [FuzzProperty] -> Stats -> Int -> IO FuzzProperty
nextProperty scheduler stats lookahead = do
  now <- getCurrentTime
  (fuzzProperty, mustWait) <- atomically $ do
    resting <- readTVar (statsResting stats)
    scheduled <- readTVar scheduler
    let rests item = maybe False (> now) (Map.lookup (fuzzPropertyId item) resting)
    case pickProperty rests lookahead scheduled of
      Just (picked, mustWait, remaining) -> do
        writeTVar scheduler remaining
        pure (picked, mustWait)
      Nothing -> retry
  when mustWait $ do
    resting <- readTVarIO (statsResting stats)
    forM_ (Map.lookup (fuzzPropertyId fuzzProperty) resting) $ \restEnd ->
      threadDelay (durationMicroseconds (diffUTCTime restEnd now))
  pure fuzzProperty

-- | Run batches until the thread is cancelled. A failure becomes a known bug.
worker :: TVar [FuzzProperty] -> Stats -> Int -> Int -> IO ()
worker scheduler stats lookahead workerId = forever $ do
  fuzzProperty <- nextProperty scheduler stats lookahead
  let propertyId = fuzzPropertyId fuzzProperty
      clearActive = atomically (modifyTVar' (statsActive stats) (IntMap.delete workerId))
  atomically $ modifyTVar' (statsActive stats) (IntMap.insert workerId propertyId)
  result <- runBatch stats fuzzProperty `finally` clearActive
  case result of
    Nothing -> atomically $ modifyTVar' (statsCompletedBatches stats) (+ 1)
    Just failure -> do
      now <- getCurrentTime
      atomically $ modifyTVar' (statsResting stats) (Map.insert propertyId (addUTCTime (fromIntegral restSeconds) now))
      recordBug stats failure

recordBug :: Stats -> Failure -> IO ()
recordBug stats failure = do
  now <- getCurrentTime
  (bug, isNew) <- atomically $ do
    bugs <- readTVar (statsBugs stats)
    let (bug, isNew, updated) = recordFailure now failure bugs
    writeTVar (statsBugs stats) updated
    when isNew $ modifyTVar' (statsNewBugs stats) (Set.insert (bugId bug))
    pure (bug, isNew)
  persistBugs stats
  statsAnnounce stats (FoundBug bug isNew)

-- | Write the current bugs to the store. The lock keeps the writes in order.
persistBugs :: Stats -> IO ()
persistBugs stats =
  withMVar (statsStoreLock stats) $ \() -> do
    bugs <- readTVarIO (statsBugs stats)
    saveBugs (statsStorePath stats) bugs

runBatch :: Stats -> FuzzProperty -> IO (Maybe Failure)
runBatch stats fuzzProperty = withFuzzProperty fuzzProperty $ \fuzzPropertyValue -> do
  seed <- Seed.random
  previousCount <- newIORef 0
  let batchProperty = withTests (fromIntegral batchSize) fuzzPropertyValue
  report <-
    checkReport
      (propertyConfig batchProperty)
      0
      seed
      (propertyTest batchProperty)
      (progressCallback previousCount)
  recordCount previousCount report
  case reportStatus report of
    OK -> pure Nothing
    _ -> do
      rendered <- renderResult DisableColor (Just (PropertyName (fuzzPropertyId fuzzProperty))) report
      pure (Just (toFailure (fuzzPropertyId fuzzProperty) report rendered))
  where
    progressCallback previousCount report =
      case reportStatus report of
        Running -> recordCount previousCount report
        _ -> pure ()
    recordCount previousCount report = do
      let TestCount currentCount = reportTests report
      priorCount <- readIORef previousCount
      let completed = max 0 (currentCount - priorCount)
      writeIORef previousCount currentCount
      atomically $ do
        modifyTVar' (statsSuccessfulCases stats) (+ completed)
        modifyTVar' (statsPropertyCases stats) (Map.adjust (+ completed) (fuzzPropertyId fuzzProperty))

-- | Describe a failed or given-up report.
toFailure :: String -> Report Result -> String -> Failure
toFailure propertyId Report {reportTests, reportDiscards, reportSeed, reportStatus} rendered =
  Failure
    { failureProperty = propertyId,
      failureSeed = show reportSeed,
      failureReplay = replay,
      failureText = text,
      failureValues = values,
      failureSite = site,
      failureReport = rendered
    }
  where
    (replay, text, values, site) =
      case reportStatus of
        Failed failure ->
          ( skipCompress (SkipToShrink reportTests reportDiscards (failureShrinkPath failure)) <> " " <> show reportSeed,
            failureMessage failure,
            map failedValue (failureAnnotations failure),
            fmap renderSpan (failureLocation failure)
          )
        GaveUp -> (show reportSeed, "gave up: too many discards", [], Nothing)
        OK -> (show reportSeed, "", [], Nothing)
    renderSpan Span {spanFile, spanStartLine} = spanFile <> ":" <> show (unLineNo spanStartLine)
