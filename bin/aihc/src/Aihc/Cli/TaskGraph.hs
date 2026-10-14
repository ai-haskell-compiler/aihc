module Aihc.Cli.TaskGraph
  ( Task (..),
    TaskGraph,
    TaskId (..),
    TaskKind (..),
    TaskObserver (..),
    TaskTiming (..),
    addTasks,
    noTaskObserver,
    allocateTaskIds,
    renderDuration,
    renderTaskTimeline,
    runTaskGraph,
    runTaskGraphWith,
  )
where

import Aihc.Cli.Concurrent (mapConcurrently_)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newEmptyMVar, newMVar, takeMVar, tryPutMVar)
import Control.Exception (SomeException, mask_, throwIO, try)
import Control.Monad (unless, void, when)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Numeric (showFFloat)

-- | The identity of a task within one graph. The caller numbers its tasks,
-- so the graph stores and compares machine integers: naming the tasks made
-- every dependency edge a comparison of module names joined into a string.
newtype TaskId = TaskId Int
  deriving (Eq, Ord, Show)

data TaskKind
  = -- | A task of a package as a whole: prepare, partition, or finish.
    TaskPackage
  | TaskTypeCheck
  | TaskResolve
  | TaskParse
  | TaskBackend
  deriving (Eq, Show)

instance Ord TaskKind where
  compare left right = compare (taskKindPriority left) (taskKindPriority right)

taskKindPriority :: TaskKind -> Int
taskKindPriority kind =
  case kind of
    TaskPackage -> 0
    TaskTypeCheck -> 1
    TaskResolve -> 2
    TaskParse -> 3
    TaskBackend -> 4

data Task = Task
  { taskId :: !TaskId,
    taskKind :: !TaskKind,
    taskOrder :: !Int,
    taskDependencies :: !(Set TaskId),
    taskAction :: !(IO ())
  }

data TaskTiming = TaskTiming
  { timingWorker :: !Int,
    timingTask :: !TaskId,
    timingKind :: !TaskKind,
    timingStart :: !Word64,
    timingEnd :: !Word64
  }
  deriving (Eq, Show)

-- | What a graph tells about its run: how many workers it has, and when
-- a task of a kind starts and ends. The progress output counts them.
data TaskObserver = TaskObserver
  { observeWorkers :: Int -> IO (),
    observeTaskStart :: TaskKind -> IO (),
    observeTaskEnd :: TaskKind -> IO ()
  }

noTaskObserver :: TaskObserver
noTaskObserver = TaskObserver (const (pure ())) (const (pure ())) (const (pure ()))

data ReadyTask = ReadyTask !TaskKind !Int !TaskId
  deriving (Eq, Ord, Show)

data TaskState = TaskState
  { stateTasks :: !(Map TaskId Task),
    stateNextId :: !Int,
    stateWaiters :: ![MVar ()],
    stateReady :: !(Set ReadyTask),
    stateWaitCounts :: !(Map TaskId Int),
    stateDependents :: !(Map TaskId [TaskId]),
    -- | The tasks that ran, which a task added later can depend on.
    stateCompleted :: !(Set TaskId),
    -- | The tasks added and not yet run, the running ones among them.
    stateRemaining :: !Int
  }

-- | A graph that runs. A task that runs can add tasks to it, each with
-- dependencies on tasks already in the graph, run or not. The graph ends
-- when every task added has run, so a task that adds tasks does so before
-- it ends.
data TaskGraph = TaskGraph
  { graphState :: !(MVar TaskState),
    graphTimings :: !(IORef [TaskTiming])
  }

runTaskGraph :: Int -> [Task] -> IO [TaskTiming]
runTaskGraph requestedWorkers tasks = runTaskGraphWith noTaskObserver requestedWorkers (`addTasks` tasks)

-- | Run a graph that the seed action fills. The seed runs before the
-- workers start, and its tasks add the rest.
runTaskGraphWith :: TaskObserver -> Int -> (TaskGraph -> IO ()) -> IO [TaskTiming]
runTaskGraphWith observer requestedWorkers seed = do
  graph <-
    TaskGraph
      <$> newMVar (TaskState Map.empty 0 [] Set.empty Map.empty Map.empty Set.empty 0)
      <*> newIORef []
  seed graph
  let workers = max 1 requestedWorkers
  observeWorkers observer workers
  mapConcurrently_ (runWorker observer graph) [1 .. workers]
  sortOn timingStart <$> readIORef (graphTimings graph)

-- | A range of identifiers no other task of the graph has: the first of
-- @count@ consecutive ones.
allocateTaskIds :: TaskGraph -> Int -> IO Int
allocateTaskIds graph count =
  modifyMVar (graphState graph) $ \state ->
    pure (state {stateNextId = stateNextId state + count}, stateNextId state)

-- | Add tasks to the graph. Each dependency is a task of the graph, added
-- before or in this call. A cycle among the tasks is not checked: the
-- callers build their graphs from a dependency order that is already
-- acyclic, and a phase of a unit only ever waits on an earlier phase of
-- the same unit or on a unit before it.
addTasks :: TaskGraph -> [Task] -> IO ()
addTasks graph tasks = mask_ $
  modifyMVar_ (graphState graph) $ \state -> do
    let known = stateTasks state
        added = Map.fromList [(taskId task, task) | task <- tasks]
        duplicateCount = length tasks - Map.size added
        duplicates = Map.keysSet (Map.intersection known added)
        missingIds = Set.unions (map taskDependencies tasks) Set.\\ (Map.keysSet known <> Map.keysSet added)
    when (duplicateCount /= 0 || not (Set.null duplicates)) $
      ioError (userError "Task graph has duplicate task identifiers")
    unless (Set.null missingIds) $
      ioError (userError ("Task graph has missing dependencies: " <> show (Set.toAscList missingIds)))
    wakeWorkers (foldl' addTask state {stateTasks = Map.union added known} tasks)
  where
    addTask state task =
      let waiting = Set.size (taskDependencies task Set.\\ stateCompleted state)
       in state
            { stateReady = if waiting == 0 then Set.insert (readyTask task) (stateReady state) else stateReady state,
              stateWaitCounts = Map.insert (taskId task) waiting (stateWaitCounts state),
              stateDependents = addTaskDependents (stateDependents state) task,
              stateRemaining = stateRemaining state + 1
            }

addTaskDependents :: Map TaskId [TaskId] -> Task -> Map TaskId [TaskId]
addTaskDependents dependents task =
  foldl'
    (\result dependency -> Map.insertWith (<>) dependency [taskId task] result)
    dependents
    (Set.toList (taskDependencies task))

readyTask :: Task -> ReadyTask
readyTask task = ReadyTask (taskKind task) (taskOrder task) (taskId task)

runWorker :: TaskObserver -> TaskGraph -> Int -> IO ()
runWorker observer graph worker = do
  next <- claimTask graph
  case next of
    Nothing -> pure ()
    Just task -> do
      let kind = taskKind task
          identifier = taskId task
      observeTaskStart observer kind
      started <- getMonotonicTimeNSec
      result <- try (taskAction task)
      ended <- getMonotonicTimeNSec
      observeTaskEnd observer kind
      atomicModifyIORef' (graphTimings graph) (\items -> (TaskTiming worker identifier kind started ended : items, ()))
      case result of
        Left exception -> throwIO (exception :: SomeException)
        Right () -> do
          completeTask graph identifier
          runWorker observer graph worker

-- | Register a wake signal under the graph mutex before a worker waits.
claimTask :: TaskGraph -> IO (Maybe Task)
claimTask graph = do
  decision <- modifyMVar (graphState graph) $ \state ->
    case Set.minView (stateReady state) of
      Just (ReadyTask _ _ identifier, remainingReady) ->
        let task = fromMaybe (error "missing ready task") (Map.lookup identifier (stateTasks state))
         in pure (state {stateReady = remainingReady}, Right (Just task))
      Nothing
        | stateRemaining state == 0 -> pure (state, Right Nothing)
        | otherwise -> do
            signal <- newEmptyMVar
            pure (state {stateWaiters = signal : stateWaiters state}, Left signal)
  case decision of
    Right task -> pure task
    Left signal -> takeMVar signal >> claimTask graph

-- | Wake idle workers when work is ready or the graph has finished.
wakeWorkers :: TaskState -> IO TaskState
wakeWorkers state
  | not (Set.null (stateReady state)) || stateRemaining state == 0 = do
      mapM_ (\signal -> void (tryPutMVar signal ())) (stateWaiters state)
      pure state {stateWaiters = []}
  | otherwise = pure state

completeTask :: TaskGraph -> TaskId -> IO ()
completeTask graph identifier = mask_ $
  modifyMVar_ (graphState graph) $ \state -> do
    let taskMap = stateTasks state
        dependents = Map.findWithDefault [] identifier (stateDependents state)
        (waitCounts, newlyReady) = foldl' (unlock taskMap) (stateWaitCounts state, []) dependents
    wakeWorkers
      state
        { stateReady = stateReady state <> Set.fromList newlyReady,
          stateWaitCounts = waitCounts,
          stateDependents = Map.delete identifier (stateDependents state),
          stateCompleted = Set.insert identifier (stateCompleted state),
          stateRemaining = stateRemaining state - 1
        }
  where
    unlock taskMap (waitCounts, ready) dependent =
      let nextCount = Map.findWithDefault 0 dependent waitCounts - 1
          nextWaitCounts = Map.insert dependent nextCount waitCounts
       in if nextCount == 0
            then case Map.lookup dependent taskMap of
              Just task -> (nextWaitCounts, readyTask task : ready)
              Nothing -> error "missing dependent task"
            else (nextWaitCounts, ready)

-- | The timeline of a compile. The phases are the serial stretches that
-- run between the task graphs, as label and duration; they report no task,
-- so they are the idle stretches of the timeline.
renderTaskTimeline :: Bool -> [(String, Word64)] -> [TaskTiming] -> String
renderTaskTimeline _ phases [] = unlines (renderPhases phases <> ["Frontend time: 0.000 ns", "Compile time: 0.000 ns"])
renderTaskTimeline useColor phases timings =
  unlines
    ( map renderWorker workers
        <> [axis, renderLegend]
        <> renderPhases phases
        <> [ "Frontend time: " <> renderDuration frontend,
             "Compile time: " <> renderDuration total
           ]
        <> map renderKindTotal [TaskPackage, TaskParse, TaskResolve, TaskTypeCheck, TaskBackend]
    )
  where
    start = minimum (map timingStart timings)
    end = maximum (map timingEnd timings)
    total = max 1 (end - start)
    frontend =
      case [timingEnd timing | timing <- timings, timingKind timing == TaskTypeCheck] of
        [] -> 0
        typeCheckEnds -> max 1 (maximum typeCheckEnds - start)
    width = 60
    workers = [1 .. maximum (map timingWorker timings)]
    labelWidth = length (show (maximum workers))
    renderWorker worker =
      "Worker "
        <> padLeft labelWidth (show worker)
        <> " |"
        <> concat [symbolAt worker column | column <- [0 .. width - 1]]
        <> "|"
    symbolAt worker column =
      case [ timingKind timing
           | timing <- timings,
             timingWorker timing == worker,
             containsColumn column timing
           ] of
        kind : _ -> kindSymbol useColor kind
        [] -> "."
    containsColumn column timing =
      let columnStart = start + (fromIntegral column * total) `div` fromIntegral width
          columnEnd = start + (fromIntegral (column + 1) * total) `div` fromIntegral width
       in timingStart timing <= columnEnd && columnStart <= timingEnd timing
    axis = replicate (7 + labelWidth + 2) ' ' <> "0" <> replicate (width - 2) ' ' <> renderDuration total
    renderLegend =
      unwords
        [ kindSymbol useColor TaskPackage <> "=package",
          kindSymbol useColor TaskParse <> "=parse",
          kindSymbol useColor TaskResolve <> "=resolve",
          kindSymbol useColor TaskTypeCheck <> "=type-check",
          kindSymbol useColor TaskBackend <> "=backend",
          ".=idle"
        ]
    renderKindTotal kind =
      let matching = [timing | timing <- timings, timingKind timing == kind]
          totalNs = sum [timingEnd timing - timingStart timing | timing <- matching]
          spanNs =
            case matching of
              [] -> 0
              _ -> maximum (map timingEnd matching) - minimum (map timingStart matching)
       in kindSymbol useColor kind
            <> " total: "
            <> renderDuration totalNs
            <> ", spanning "
            <> renderSpanDuration spanNs

renderPhases :: [(String, Word64)] -> [String]
renderPhases phases = [name <> " time: " <> renderDuration duration | (name, duration) <- phases]

kindSymbol :: Bool -> TaskKind -> String
kindSymbol useColor kind = colorize useColor (kindColor kind) [kindGlyph kind]

kindGlyph :: TaskKind -> Char
kindGlyph kind =
  case kind of
    TaskPackage -> '▆'
    TaskParse -> '▁'
    TaskResolve -> '▂'
    TaskTypeCheck -> '▄'
    TaskBackend -> '█'

kindColor :: TaskKind -> String
kindColor kind =
  case kind of
    TaskPackage -> "33"
    TaskParse -> "37"
    TaskResolve -> "32"
    TaskTypeCheck -> "34"
    TaskBackend -> "35"

colorize :: Bool -> String -> String -> String
colorize useColor color value
  | useColor = "\ESC[" <> color <> "m" <> value <> "\ESC[0m"
  | otherwise = value

renderDuration :: Word64 -> String
renderDuration = renderScaledDuration durationDecimalPlaces

renderSpanDuration :: Word64 -> String
renderSpanDuration = renderScaledDuration spanDecimalPlaces

renderScaledDuration :: (Double -> Int) -> Word64 -> String
renderScaledDuration decimalPlaces nanoseconds
  | nanoseconds >= 60000000000 = renderUnit 60000000000 "min"
  | nanoseconds >= 1000000000 = renderUnit 1000000000 "s"
  | nanoseconds >= 1000000 = renderUnit 1000000 "ms"
  | nanoseconds >= 1000 = renderUnit 1000 "µs"
  | otherwise = renderUnit 1 "ns"
  where
    renderUnit divisor unit =
      let value = fromIntegral nanoseconds / divisor :: Double
       in showFFloat (Just (decimalPlaces value)) value "" <> " " <> unit

durationDecimalPlaces :: Double -> Int
durationDecimalPlaces value
  | value >= 100 = 1
  | value >= 10 = 2
  | otherwise = 3

spanDecimalPlaces :: Double -> Int
spanDecimalPlaces value
  | value >= 100 = 0
  | value >= 10 = 1
  | otherwise = 2

padLeft :: Int -> String -> String
padLeft width value = replicate (max 0 (width - length value)) ' ' <> value
