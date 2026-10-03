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

import Control.Concurrent.Async (mapConcurrently_)
import Control.Concurrent.STM
  ( STM,
    TVar,
    atomically,
    modifyTVar',
    newTVarIO,
    readTVar,
    readTVarIO,
    retry,
    writeTVar,
  )
import Control.Exception (SomeException, throwIO, try)
import Control.Monad (unless, when)
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
  { stateReady :: !(Set ReadyTask),
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
  { graphTasks :: !(TVar (Map TaskId Task)),
    graphState :: !(TVar TaskState),
    graphNextId :: !(TVar Int),
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
      <$> newTVarIO Map.empty
      <*> newTVarIO (TaskState Set.empty Map.empty Map.empty Set.empty 0)
      <*> newTVarIO 0
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
  atomically $ do
    next <- readTVar (graphNextId graph)
    writeTVar (graphNextId graph) (next + count)
    pure next

-- | Add tasks to the graph. Each dependency is a task of the graph, added
-- before or in this call. A cycle among the tasks is not checked: the
-- callers build their graphs from a dependency order that is already
-- acyclic, and a phase of a unit only ever waits on an earlier phase of
-- the same unit or on a unit before it.
addTasks :: TaskGraph -> [Task] -> IO ()
addTasks graph tasks = do
  known <- readTVarIO (graphTasks graph)
  let added = Map.fromList [(taskId task, task) | task <- tasks]
      duplicateCount = length tasks - Map.size added
      duplicates = Map.keysSet (Map.intersection known added)
      missingIds = Set.unions (map taskDependencies tasks) Set.\\ (Map.keysSet known <> Map.keysSet added)
  when (duplicateCount /= 0 || not (Set.null duplicates)) $
    ioError (userError "Task graph has duplicate task identifiers")
  unless (Set.null missingIds) $
    ioError (userError ("Task graph has missing dependencies: " <> show (Set.toAscList missingIds)))
  atomically $ do
    modifyTVar' (graphTasks graph) (Map.union added)
    modifyTVar' (graphState graph) (\state -> foldl' addTask state tasks)
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
  next <- atomically (takeReadyTask (graphState graph))
  case next of
    Nothing -> pure ()
    Just ready@(ReadyTask kind _ identifier) -> do
      taskMap <- readTVarIO (graphTasks graph)
      let task = fromMaybe (error "missing ready task") (Map.lookup identifier taskMap)
      observeTaskStart observer kind
      started <- getMonotonicTimeNSec
      result <- try (taskAction task)
      ended <- getMonotonicTimeNSec
      observeTaskEnd observer kind
      atomicModifyIORef' (graphTimings graph) (\items -> (TaskTiming worker identifier kind started ended : items, ()))
      case result of
        Left exception -> throwIO (exception :: SomeException)
        Right () -> do
          atomically (completeTask graph ready)
          runWorker observer graph worker

takeReadyTask :: TVar TaskState -> STM (Maybe ReadyTask)
takeReadyTask stateVar = do
  state <- readTVar stateVar
  case Set.minView (stateReady state) of
    Just (task, remainingReady) -> do
      writeTVar stateVar state {stateReady = remainingReady}
      pure (Just task)
    Nothing
      | stateRemaining state == 0 -> pure Nothing
      | otherwise -> retry

completeTask :: TaskGraph -> ReadyTask -> STM ()
completeTask graph (ReadyTask _ _ identifier) = do
  taskMap <- readTVar (graphTasks graph)
  modifyTVar' (graphState graph) (complete taskMap)
  where
    complete taskMap state =
      let dependents = Map.findWithDefault [] identifier (stateDependents state)
          (waitCounts, newlyReady) = foldl' (unlock taskMap) (stateWaitCounts state, []) dependents
       in state
            { stateReady = stateReady state <> Set.fromList newlyReady,
              stateWaitCounts = waitCounts,
              stateDependents = Map.delete identifier (stateDependents state),
              stateCompleted = Set.insert identifier (stateCompleted state),
              stateRemaining = stateRemaining state - 1
            }

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
