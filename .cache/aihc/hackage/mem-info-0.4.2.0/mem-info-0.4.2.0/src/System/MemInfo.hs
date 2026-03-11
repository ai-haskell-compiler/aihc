{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : System.MemInfo
Copyright   : (c) 2023 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

Implements __printmem__, a command that computes the memory usage of some
processes
-}
module System.MemInfo (
  -- * Implement __printmem__
  getChoices,
  printProcs,

  -- * Read /MemUsage/
  readForOnePid,
  readForOnePid',
  readMemUsage',
  readMemUsage,
  NotRun (..),
  LostPid (..),

  -- * Stream /MemUsage/ periodically
  unfoldMemUsage,
  unfoldMemUsageAfter',
  unfoldMemUsageAfter,

  -- * Obtain the process/program name
  ProcNamer,
  nameFromExeOnly,
  nameFor,
  nameAsFullCmd,

  -- * Index by pid or name
  ProcName,
  Indexer,
  dropId,
  withPid,

  -- * Print /MemUsage/
  printUsage',
  printUsage,
  printUsages,

  -- * The current process ID
  selfId,
  selfId',

  -- * Convenient re-exports
  mkReportBud,
  defaultRoot,
  ProcessID,
  AsCmdName (..),
) where

import Control.Exception (Exception)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader (ask), asks, runReaderT)
import Data.Bifunctor (Bifunctor (..))
import Data.Functor ((<&>))
import Data.List (sortBy)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..), comparing)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Fmt (
  listF,
  (+|),
  (|+),
 )
import System.Exit (exitFailure)
import System.MemInfo.Choices (
  Choices (..),
  Mem (..),
  PrintOrder (..),
  Style (..),
  asFloat,
  defaultRoot,
  getChoices,
 )
import System.MemInfo.Prelude
import System.MemInfo.Print (
  AsCmdName (..),
  fmtMemUsage,
  styleOutput,
 )
import System.MemInfo.Proc (
  BadStatus (..),
  ExeInfo (..),
  MemUsage (..),
  ProcUsage (..),
  StatusInfo (..),
  amass,
  parseExeInfo,
  parseFromSmap,
  parseFromStatm,
  parseStatusInfo,
 )
import System.MemInfo.SysInfo (
  ReportBud (..),
  fmtRamFlaws,
  fmtSwapFlaws,
  mkReportBud,
 )


{- | Print a report to @stdout@ displaying the memory usage of the programs
specified by @Choices@
-}
printProcs :: Choices -> IO ()
printProcs cs@Choices {choiceByPid = byPid} = do
  bud <- verify cs
  if byPid
    then printProcs' withPid bud cs
    else printProcs' dropId bud cs


printProcs' :: (Ord a, AsCmdName a) => Indexer a -> ReportBud -> Choices -> IO ()
printProcs' indexer bud cs = do
  let Choices
        { choiceShowSwap = showSwap
        , choiceOnlyTotal = onlyTotal
        , choiceWatchSecs = watchSecsMb
        , choicePrintOrder = printOrder
        , choiceReversed = reversed
        , choiceStyle = style
        , choiceMinMemory = mem
        } = cs
      style' = fromMaybe Normal style
      toList = filterLT mem . sortBy (byPrintOrder' reversed printOrder) . Map.toList
      printEachCmd = printMemUsages bud style' showSwap onlyTotal . toList
      printTheTotal = onlyPrintTotal bud showSwap onlyTotal . Map.toList
      showTotal = if onlyTotal then printTheTotal else printEachCmd
      namer = if choiceSplitArgs cs then nameAsFullCmd else nameFor
  case watchSecsMb of
    Nothing -> readMemUsage' namer indexer bud >>= either haltLostPid showTotal
    (Just spanSecs) -> do
      let unfold = unfoldMemUsageAfter' namer indexer spanSecs
      loopPrintMemUsages unfold bud showTotal


printMemUsages ::
  (AsCmdName a) =>
  ReportBud ->
  Style ->
  Bool ->
  Bool ->
  [(a, MemUsage)] ->
  IO ()
printMemUsages bud style showSwap onlyTotal totals = do
  let overallIsAccurate = (showSwap && rbHasSwapPss bud) || rbHasPss bud
      output = styleOutput showSwap style overallIsAccurate totals
  mapM_ Text.putStrLn output
  reportFlaws bud showSwap onlyTotal


-- | Print the program name and memory usage, optionally hiding the swap size
printUsage' :: (AsCmdName a) => (a, MemUsage) -> Bool -> IO ()
printUsage' (name, mu) showSwap = Text.putStrLn $ fmtMemUsage showSwap name mu


-- | Like @'printUsage''@, but alway shows the swap size
printUsage :: (AsCmdName a) => (a, MemUsage) -> IO ()
printUsage = flip printUsage' True


-- | Like @'printUsage' for several @processIDs@
printUsages ::
  (AsCmdName a) =>
  Map a MemUsage ->
  IO ()
printUsages = mapM_ printUsage . Map.toAscList


onlyPrintTotal :: (AsCmdName k) => ReportBud -> Bool -> Bool -> [(k, MemUsage)] -> IO ()
onlyPrintTotal bud showSwap onlyTotal totals = do
  let (private, swap) = overallTotals $ map snd totals
      printRawTotal = Text.putStrLn . fmtMemBytes
  if showSwap
    then do
      when (rbHasSwapPss bud) $ printRawTotal swap
      reportFlaws bud showSwap onlyTotal
      when (isJust $ rbSwapFlaws bud) exitFailure
    else do
      when (rbHasPss bud) $ printRawTotal private
      reportFlaws bud showSwap onlyTotal
      when (isJust $ rbRamFlaws bud) exitFailure


loopPrintMemUsages ::
  (Ord c, AsCmdName c) =>
  (ReportBud -> IO (Either [ProcessID] ((Map c MemUsage, [ProcessID]), ReportBud))) ->
  ReportBud ->
  (Map c MemUsage -> IO ()) ->
  IO ()
loopPrintMemUsages unfold bud showTotal = do
  let clearScreen = putStrLn "\o033c"
      warnHalting = errStrLn False "halting: all monitored processes have stopped"
      handleNext (Left stopped) = do
        warnStopped stopped
        warnHalting
      handleNext (Right ((total, stopped), updated)) = do
        clearScreen
        warnStopped stopped
        showTotal total
        go updated
      go initial = unfold initial >>= handleNext
  go bud


warnStopped :: [ProcessID] -> IO ()
warnStopped pids = unless (null pids) $ do
  let errMsg = "some processes stopped:pids:" +| toInteger <$> pids |+ ""
  errStrLn False errMsg


-- | The name of a process or program in the memory report.
type ProcName = Text


-- | Like @'unfoldMemUsageAfter''@, but uses the default 'ProcNamer' and 'Indexer'
unfoldMemUsageAfter ::
  (Integral seconds) =>
  seconds ->
  ReportBud ->
  IO (Either [ProcessID] ((Map ProcName MemUsage, [ProcessID]), ReportBud))
unfoldMemUsageAfter = unfoldMemUsageAfter' nameFor dropId


-- | Like @'unfoldMemUsage'@ but computes the @'MemUsage's@ after a delay
unfoldMemUsageAfter' ::
  (Ord a, AsCmdName a, Integral seconds) =>
  ProcNamer ->
  Indexer a ->
  seconds ->
  ReportBud ->
  IO (Either [ProcessID] ((Map a MemUsage, [ProcessID]), ReportBud))
unfoldMemUsageAfter' namer mkCmd spanSecs bud = do
  let spanMicros = 1000000 * fromInteger (toInteger spanSecs)
  threadDelay spanMicros
  unfoldMemUsage namer mkCmd bud


{- | Unfold @'MemUsage's@ specified by a @'ReportBud'@

The @ProcessID@ of stopped processes are reported, both as part of intermediate
invocations (via the @[ProcessID]@ in the @Right@), and in the final one (as the
value of the @Left@).
-}
unfoldMemUsage ::
  (Ord a) =>
  ProcNamer ->
  Indexer a ->
  ReportBud ->
  IO (Either [ProcessID] ((Map a MemUsage, [ProcessID]), ReportBud))
unfoldMemUsage namer mkCmd bud = do
  let changePids rbPids = bud {rbPids}
      dropStopped t [] = Just t
      dropStopped ReportBud {rbPids = ps} stopped =
        changePids <$> nonEmpty (NE.filter (`notElem` stopped) ps)
      ReportBud {rbPids = pids, rbHasPss = hasPss} = bud
      nextState (stopped, []) = Left stopped
      nextState (stopped, xs) = case dropStopped bud stopped of
        Just updated -> Right ((amass hasPss (map mkCmd xs), stopped), updated)
        Nothing -> Left stopped
  nextState <$> foldlEitherM' (readNameAndStats namer bud) pids


{- | Like 'readForOnePid', but assumes the process file hierarchy
root is the linux default
-}
readForOnePid :: ProcessID -> IO (Either NotRun (ProcName, MemUsage))
readForOnePid = readForOnePid' defaultRoot


-- | Load the @'MemUsage'@ of a program specified by its @ProcessID@
readForOnePid' ::
  ProcRoot ->
  -- | the root of the process file system; usually @/proc@
  ProcessID ->
  -- | the ID of a valid process
  IO (Either NotRun (ProcName, MemUsage))
readForOnePid' root pid = do
  let mkBud' xs = mkReportBud root xs <&> maybe (Left OddKernel) Right
      noProc = NoProc pid
      fromMemUsage x = maybe (Left $ PidLost noProc) Right (Map.lookupMin x)
      andFromUsage = either (Left . PidLost) fromMemUsage
  nameFor root pid >>= \case
    Left err -> pure $ Left $ PidLost err
    Right _ ->
      mkBud' (pid :| []) >>= \case
        Left err -> pure $ Left err
        Right bud -> readMemUsage bud <&> andFromUsage


-- | Like @'readMemUsage''@ but uses the default 'ProcNamer' and 'Indexer'
readMemUsage :: ReportBud -> IO (Either LostPid (Map ProcName MemUsage))
readMemUsage = readMemUsage' nameFor dropId


{- | Loads the @'MemUsage'@ specified by a @'ReportBud'@

Fails if

- the system does not have the expected process filesystem memory records
- any of the processes specified by @'ReportBud'@ are missing or inaccessible
-}
readMemUsage' ::
  (Ord a) =>
  ProcNamer ->
  Indexer a ->
  ReportBud ->
  IO (Either LostPid (Map a MemUsage))
readMemUsage' namer mkCmd bud = do
  let amass' cmds = amass (rbHasPss bud) $ map mkCmd cmds
  fmap amass' <$> foldlEitherM (readNameAndStats namer bud) (rbPids bud)


readNameAndStats ::
  ProcNamer ->
  ReportBud ->
  ProcessID ->
  IO (Either LostPid (ProcessID, ProcName, ProcUsage))
readNameAndStats = readNameAndStats'


readNameAndStats' ::
  ProcNamer ->
  ReportBud ->
  ProcessID ->
  IO (Either LostPid (ProcessID, ProcName, ProcUsage))
readNameAndStats' namer bud pid = do
  let withProcRoot = flip runReaderT root
      root = rbProcRoot bud
  namer root pid >>= \case
    Left e -> pure $ Left e
    Right name ->
      withProcRoot $
        readMemStats bud pid >>= \case
          Left e -> pure $ Left e
          Right stats -> pure $ Right (pid, name, stats)


reportFlaws :: ReportBud -> Bool -> Bool -> IO ()
reportFlaws bud showSwap onlyTotal = do
  let reportSwap = errStrLn onlyTotal . fmtSwapFlaws
      reportRam = errStrLn onlyTotal . fmtRamFlaws
      (ram, swap) = (rbRamFlaws bud, rbSwapFlaws bud)
  -- when showSwap, report swap flaws
  -- unless (showSwap and onlyTotal), show ram flaws
  when showSwap $ maybe (pure ()) reportSwap swap
  unless (onlyTotal && showSwap) $ maybe (pure ()) reportRam ram


verify :: Choices -> IO ReportBud
verify cs = verify' (choiceProcRoot cs) (choicePidsToShow cs) >>= either (haltErr . fmtNotRun) pure


verify' :: FilePath -> Maybe (NonEmpty ProcessID) -> IO (Either NotRun ReportBud)
verify' root pidsMb = do
  let mkBud' xs = mkReportBud root xs <&> maybe (Left OddKernel) Right
      thenMkBud = either (pure . Left) mkBud'
  case pidsMb of
    Just pids -> checkAllExist root pids >>= thenMkBud
    Nothing -> allKnownProcs root >>= thenMkBud


-- | The root of the process file hierarchy
type ProcRoot = FilePath


pidPath :: (MonadReader ProcRoot m) => FilePath -> ProcessID -> m FilePath
pidPath base pid = ask >>= \root -> pure $ "" +| root |+ "/" +| toInteger pid |+ "/" +| base |+ ""


{- | pidExists returns false for any ProcessID that does not exist or cannot
be accessed
-}
pidExeExists :: FilePath -> ProcessID -> IO Bool
pidExeExists root pid = do
  result <- flip runReaderT root $ exeInfo pid
  pure $ either (const False) (const True) result


-- | Obtain the @ProcName@ as the full cmd path
nameAsFullCmd :: ProcNamer
nameAsFullCmd root pid = do
  let withProcRoot = flip runReaderT root
      err = NoCmdLine pid
      recombine = Text.intercalate " " . NE.toList
      orLostPid = maybe (Left err) (Right . recombine)
  withProcRoot (readCmdlinePath pid) >>= (pure . orLostPid) . parseCmdline


readCmdlinePath :: (MonadReader ProcRoot m, MonadIO m) => ProcessID -> m Text
readCmdlinePath pid = pidPath "cmdline" pid >>= liftIO . readUtf8Text


{- | Obtain the @ProcName@ by examining the path linked by
__{proc_root}\/pid\/exe__
-}
nameFromExeOnly :: ProcNamer
nameFromExeOnly root pid = do
  let withProcRoot = flip runReaderT root
      pickSuffix = \case
        Just (x :| _) -> do
          let addSuffix' b = x <> if b then " [updated]" else " [deleted]"
          Right . baseName . addSuffix' <$> exists x
        -- args should not be empty when {pid_root}/exe resolves to a
        -- path, it's an error if it is
        Nothing -> pure $ Left $ NoCmdLine pid

  withProcRoot (exeInfo pid) >>= \case
    Left e -> pure $ Left e
    Right i | not $ eiDeleted i -> pure $ Right $ baseName $ eiOriginal i
    -- when the exe bud ends with (deleted), the version of the exe used to
    -- invoke the process has been removed from the filesystem. Sometimes it has
    -- been updated; examining both the original bud and the version in
    -- cmdline help determine what occurred
    Right ExeInfo {eiOriginal = orig} ->
      exists orig >>= \wasUpdated ->
        if wasUpdated
          then pure $ Right $ baseName $ "" +| orig |+ " [updated]"
          else withProcRoot (readCmdlinePath pid) >>= pickSuffix . parseCmdline


-- | Functions that obtain a process name given its @pid@
type ProcNamer = ProcRoot -> ProcessID -> IO (Either LostPid ProcName)


{- | Obtain the @ProcName@ by examining the path linked by
__{proc_root}\/pid\/exe__ or its parent's name if that is a better match
-}
nameFor :: ProcNamer
nameFor root pid =
  nameFromExeOnly root pid
    >>= either (pure . Left) (parentNameIfMatched2 root pid)


parentNameIfMatched2 :: FilePath -> ProcessID -> Text -> IO (Either LostPid ProcName)
parentNameIfMatched2 root pid candidate = do
  let isMatch = flip Text.isPrefixOf candidate . siName
      withProcRoot = flip runReaderT root
  withProcRoot (statusInfo pid) >>= \case
    Left err -> pure $ Left err
    Right si | isMatch si -> pure $ Right candidate
    Right si ->
      nameFromExeOnly root (siParent si) >>= \case
        Right n | n == candidate -> pure $ Right n
        _anyLostPid -> pure $ Right $ siName si


-- | Represents errors that prevent a report from being generated
data NotRun
  = PidLost !LostPid
  | MissingPids !(NonEmpty ProcessID)
  | NeedsRoot
  | OddKernel
  | NoRecords
  | BadSelfId
  deriving (Eq, Show)


instance Exception NotRun


fmtNotRun :: NotRun -> Text
fmtNotRun NeedsRoot = "run as root when no pids are specified using -p"
fmtNotRun (PidLost x) = fmtLostPid x
fmtNotRun OddKernel = "unrecognized kernel version"
fmtNotRun (MissingPids pids) = "no records available for: " +| listF (toInteger <$> pids) |+ ""
fmtNotRun NoRecords = "could not find any process records"
fmtNotRun BadSelfId = "self value was not a valid process ID"


{- | Represents reasons a specified @pid@ may not have memory
records.
-}
data LostPid
  = NoExeFile !ProcessID
  | NoStatusCmd !ProcessID
  | NoStatusParent !ProcessID
  | NoCmdLine !ProcessID
  | BadStatm !ProcessID
  | NoProc !ProcessID
  deriving (Eq, Show)


fmtLostPid :: LostPid -> Text
fmtLostPid (NoStatusCmd pid) = "missing:no name in {proc_root}/" +| toInteger pid |+ "/status"
fmtLostPid (NoStatusParent pid) = "missing:no ppid in {proc_root}/" +| toInteger pid |+ "/status"
fmtLostPid (NoExeFile pid) = "missing:{proc_root}/" +| toInteger pid |+ "/exe"
fmtLostPid (NoCmdLine pid) = "missing:{proc_root}/" +| toInteger pid |+ "/cmdline"
fmtLostPid (NoProc pid) = "missing:memory records for pid:" +| toInteger pid |+ ""
fmtLostPid (BadStatm pid) = "missing:invalid memory record in {proc_root}/" +| toInteger pid |+ "/statm"


haltLostPid :: LostPid -> IO a
haltLostPid err = do
  Text.hPutStrLn stderr $ "halting due to " +| fmtLostPid err |+ ""
  exitFailure


exeInfo :: (MonadReader ProcRoot m, MonadIO m) => ProcessID -> m (Either LostPid ExeInfo)
exeInfo pid = do
  link <- pidPath "exe" pid
  linkText <- liftIO $ getSymbolicLinkText pid link
  pure $ fmap parseExeInfo linkText


getSymbolicLinkText :: ProcessID -> FilePath -> IO (Either LostPid Text)
getSymbolicLinkText pid link = do
  let handledErr e = isDoesNotExistError e || isPermissionError e
      onIOE e = if handledErr e then pure (Left $ NoExeFile pid) else throwIO e
  handle onIOE $ do
    Right . Text.pack <$> getSymbolicLinkTarget link


exists :: Text -> IO Bool
exists = doesFileExist . Text.unpack


statusInfo :: (MonadReader ProcRoot m, MonadIO m) => ProcessID -> m (Either LostPid StatusInfo)
statusInfo pid = do
  let fromBadStatus NoCmd = NoStatusCmd pid
      fromBadStatus NoParent = NoStatusParent pid
  statusPath <- pidPath "status" pid
  first fromBadStatus . parseStatusInfo <$> liftIO (readUtf8Text statusPath)


-- | the processId of the current running process
selfId :: (MonadReader ProcRoot m, MonadIO m) => m ProcessID
selfId = do
  let mkSelfPath root = "" +| root |+ "/self"
  selfText <- asks mkSelfPath >>= liftIO . getSymbolicLinkTarget
  case readMaybe selfText of
    Just procId -> pure procId
    Nothing -> liftIO $ throwIO BadSelfId


-- | Like 'selfId' using the default ProcRoot
selfId' :: IO ProcessID
selfId' = runReaderT selfId defaultRoot


parseCmdline :: Text -> Maybe (NonEmpty Text)
parseCmdline =
  let split' = Text.split isNullOrSpace . Text.strip . Text.dropWhileEnd isNull
   in nonEmpty . split'


nonExisting :: FilePath -> NonEmpty ProcessID -> IO [ProcessID]
nonExisting root = filterM (fmap not . pidExeExists root) . NE.toList


checkAllExist :: FilePath -> NonEmpty ProcessID -> IO (Either NotRun (NonEmpty ProcessID))
checkAllExist root pids =
  nonExisting root pids >>= \case
    [] -> pure $ Right pids
    x : xs -> pure $ Left $ MissingPids $ x :| xs


allKnownProcs :: FilePath -> IO (Either NotRun (NonEmpty ProcessID))
allKnownProcs root =
  let readIdsMaybe = fmap (mapMaybe readMaybe)
      readProcessIDs = readIdsMaybe (listDirectory root)
      orNoPids = pure . maybe (Left NoRecords) Right . nonEmpty
   in readProcessIDs >>= filterM (pidExeExists root) >>= orNoPids


baseName :: Text -> Text
baseName = Text.pack . takeBaseName . Text.unpack


readMemStats ::
  (MonadReader ProcRoot m, MonadIO m) =>
  ReportBud ->
  ProcessID ->
  m (Either LostPid ProcUsage)
readMemStats bud pid = do
  statmPath <- pidPath "statm" pid
  statmExists <- liftIO $ doesFileExist statmPath
  if
    | rbHasSmaps bud -> Right . parseFromSmap <$> readSmaps pid
    | statmExists -> do
        let readStatm' = liftIO $ readUtf8Text statmPath
            orLostPid = maybe (Left $ BadStatm pid) Right
        orLostPid . parseFromStatm (rbKernel bud) <$> readStatm'
    | otherwise -> pure $ Left $ NoProc pid


readSmaps :: (MonadReader ProcRoot m, MonadIO m) => ProcessID -> m Text
readSmaps pid = do
  smapPath <- pidPath "smaps" pid
  rollupPath <- pidPath "smaps_rollup" pid
  hasSmaps <- liftIO $ doesFileExist smapPath
  hasRollup <- liftIO $ doesFileExist rollupPath
  if
    | hasRollup -> liftIO $ readUtf8Text rollupPath
    | hasSmaps -> liftIO $ readUtf8Text smapPath
    | otherwise -> pure Text.empty


overallTotals :: [MemUsage] -> (Int, Int)
overallTotals cts =
  let step (private, swap) ct = (private + muPrivate ct, swap + muSwap ct)
   in foldl' step (0, 0) cts


fmtMemBytes :: Int -> Text
fmtMemBytes x = "" +| x * 1024 |+ ""


foldlEitherM ::
  (Foldable t, Monad m) =>
  (a -> m (Either b c)) ->
  t a ->
  m (Either b [c])
foldlEitherM f xs =
  let go (Left err) _ = pure $ Left err
      go (Right acc) a =
        f a >>= \case
          Left err -> pure $ Left err
          Right y -> pure $ Right (y : acc)
   in foldlM go (Right []) xs


foldlEitherM' ::
  (Foldable t, Monad m) =>
  (a -> m (Either b c)) ->
  t a ->
  m ([a], [c])
foldlEitherM' f xs =
  let
    go (as, cs) a =
      f a >>= \case
        Left _ -> pure (a : as, cs)
        Right c -> pure (as, c : cs)
   in
    foldlM go (mempty, mempty) xs


haltErr :: Text -> IO a
haltErr err = do
  errStrLn True err
  exitFailure


errStrLn :: Bool -> Text -> IO ()
errStrLn errOrWarn txt = do
  let prefix = if errOrWarn then "error: " else "warning: "
  Text.hPutStrLn stderr $ prefix <> txt


-- | Functions that generate the report index
type Indexer index = (ProcessID, ProcName, ProcUsage) -> (index, ProcUsage)


{- | Index a @'ProcUsage'@ using the program name and process ID.

Each @ProcUsage@ remains distinct when added to a @MemUsage@
-}
withPid :: Indexer (ProcessID, ProcName)
withPid (pid, name, pp) = ((pid, name), pp)


{- | Index a @'ProcUsage'@ using just the program name

@ProcUsage's@ with the same @ProcName@ will be merged when added to a @MemUsage@
-}
dropId :: Indexer ProcName
dropId (_pid, name, pp) = (name, pp)


byPrintOrder ::
  (Ord c) =>
  (((c, MemUsage) -> Int) -> (c, MemUsage) -> (c, MemUsage) -> Ordering) ->
  PrintOrder ->
  (c, MemUsage) ->
  (c, MemUsage) ->
  Ordering
byPrintOrder f Swap = f $ muSwap . snd
byPrintOrder f Shared = f $ muShared . snd
byPrintOrder f Private = f $ muPrivate . snd
byPrintOrder f Count = f $ muCount . snd


byPrintOrder' ::
  (Ord a) =>
  Bool ->
  Maybe PrintOrder ->
  (a, MemUsage) ->
  (a, MemUsage) ->
  Ordering
byPrintOrder' reversed mbOrder =
  let cmpUsage = if reversed then comparing else comparing'
      cmpName = if reversed then comparing else comparing'
      byName = cmpName fst
      byUsage = byPrintOrder cmpUsage
   in maybe byName byUsage mbOrder


comparing' :: (Ord a) => (b -> a) -> b -> b -> Ordering
comparing' f a b = compare (Down $ f a) (Down $ f b)


memLT :: Mem -> (a, MemUsage) -> Bool
memLT mem (_ignored, mu) = asFloat mem < fromIntegral (muPrivate mu)


filterLT :: Maybe Mem -> [(a, MemUsage)] -> [(a, MemUsage)]
filterLT Nothing xs = xs
filterLT (Just mem) xs = filter (memLT mem) xs
