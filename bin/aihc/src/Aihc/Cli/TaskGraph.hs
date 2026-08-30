module Aihc.Cli.TaskGraph
  ( Task (..),
    TaskId (..),
    TaskKind (..),
    TaskTiming (..),
    renderTaskTimeline,
    runTaskGraph,
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
    retry,
    writeTVar,
  )
import Control.Exception (SomeException, throwIO, try)
import Data.Graph (SCC (..), stronglyConnComp)
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

newtype TaskId = TaskId String
  deriving (Eq, Ord, Show)

data TaskKind
  = TaskIO
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
    TaskIO -> 0
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

data ReadyTask = ReadyTask !TaskKind !Int !TaskId
  deriving (Eq, Ord, Show)

data TaskState = TaskState
  { stateReady :: !(Set ReadyTask),
    stateWaitCounts :: !(Map TaskId Int),
    stateDependents :: !(Map TaskId [TaskId]),
    stateRemaining :: !Int
  }

runTaskGraph :: Int -> [Task] -> IO [TaskTiming]
runTaskGraph requestedWorkers tasks = do
  taskMap <- validateTasks tasks
  state <- newTVarIO (initialTaskState tasks)
  timings <- newIORef []
  mapConcurrently_ (runWorker taskMap state timings) [1 .. max 1 requestedWorkers]
  sortOn timingStart <$> readIORef timings

validateTasks :: [Task] -> IO (Map TaskId Task)
validateTasks tasks = do
  let taskMap = Map.fromList [(taskId task, task) | task <- tasks]
      knownIds = Map.keysSet taskMap
      duplicateCount = length tasks - Map.size taskMap
      missingIds = Set.unions (map taskDependencies tasks) Set.\\ knownIds
      cycles =
        [ identifiers
        | CyclicSCC identifiers <-
            stronglyConnComp
              [ (identifier, identifier, Set.toList (taskDependencies task))
              | (identifier, task) <- Map.toList taskMap
              ]
        ]
  case () of
    _
      | duplicateCount /= 0 -> ioError (userError "Task graph has duplicate task identifiers")
      | not (Set.null missingIds) -> ioError (userError ("Task graph has missing dependencies: " <> show (Set.toAscList missingIds)))
      | not (null cycles) -> ioError (userError ("Task graph has dependency cycles: " <> show cycles))
      | otherwise -> pure taskMap

initialTaskState :: [Task] -> TaskState
initialTaskState tasks =
  TaskState
    { stateReady =
        Set.fromList
          [ readyTask task
          | task <- tasks,
            Set.null (taskDependencies task)
          ],
      stateWaitCounts = Map.fromList [(taskId task, Set.size (taskDependencies task)) | task <- tasks],
      stateDependents = foldl' addTaskDependents Map.empty tasks,
      stateRemaining = length tasks
    }

addTaskDependents :: Map TaskId [TaskId] -> Task -> Map TaskId [TaskId]
addTaskDependents dependents task =
  foldl'
    (\result dependency -> Map.insertWith (<>) dependency [taskId task] result)
    dependents
    (Set.toList (taskDependencies task))

readyTask :: Task -> ReadyTask
readyTask task = ReadyTask (taskKind task) (taskOrder task) (taskId task)

runWorker :: Map TaskId Task -> TVar TaskState -> IORef [TaskTiming] -> Int -> IO ()
runWorker taskMap state timings worker = do
  next <- atomically (takeReadyTask state)
  case next of
    Nothing -> pure ()
    Just ready@(ReadyTask kind _ identifier) -> do
      let task = fromMaybe (error "missing ready task") (Map.lookup identifier taskMap)
      started <- getMonotonicTimeNSec
      result <- try (taskAction task)
      ended <- getMonotonicTimeNSec
      atomicModifyIORef' timings (\items -> (TaskTiming worker identifier kind started ended : items, ()))
      case result of
        Left exception -> throwIO (exception :: SomeException)
        Right () -> do
          atomically (completeTask taskMap state ready)
          runWorker taskMap state timings worker

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

completeTask :: Map TaskId Task -> TVar TaskState -> ReadyTask -> STM ()
completeTask taskMap stateVar (ReadyTask _ _ identifier) =
  modifyTVar' stateVar complete
  where
    complete state =
      let dependents = Map.findWithDefault [] identifier (stateDependents state)
          (waitCounts, newlyReady) = foldl' unlock (stateWaitCounts state, []) dependents
       in state
            { stateReady = stateReady state <> Set.fromList newlyReady,
              stateWaitCounts = waitCounts,
              stateRemaining = stateRemaining state - 1
            }

    unlock (waitCounts, ready) dependent =
      let nextCount = Map.findWithDefault 0 dependent waitCounts - 1
          nextWaitCounts = Map.insert dependent nextCount waitCounts
       in if nextCount == 0
            then case Map.lookup dependent taskMap of
              Just task -> (nextWaitCounts, readyTask task : ready)
              Nothing -> error "missing dependent task"
            else (nextWaitCounts, ready)

renderTaskTimeline :: Bool -> [TaskTiming] -> String
renderTaskTimeline _ [] = unlines ["Frontend time: 0.000 ns", "Compile time: 0.000 ns"]
renderTaskTimeline useColor timings =
  unlines
    ( map renderWorker workers
        <> [ axis,
             renderLegend,
             "Frontend time: " <> renderDuration frontend,
             "Compile time: " <> renderDuration total
           ]
        <> map renderKindTotal [TaskIO, TaskParse, TaskResolve, TaskTypeCheck, TaskBackend]
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
        [ kindSymbol useColor TaskIO <> "=IO",
          kindSymbol useColor TaskParse <> "=parse",
          kindSymbol useColor TaskResolve <> "=resolve",
          kindSymbol useColor TaskTypeCheck <> "=type-check",
          kindSymbol useColor TaskBackend <> "=backend",
          ".=idle"
        ]
    renderKindTotal kind =
      kindSymbol useColor kind
        <> " total: "
        <> renderDuration (sum [timingEnd timing - timingStart timing | timing <- timings, timingKind timing == kind])

kindSymbol :: Bool -> TaskKind -> String
kindSymbol _ TaskIO = [kindGlyph TaskIO]
kindSymbol useColor kind = colorize useColor (kindColor kind) [kindGlyph kind]

kindGlyph :: TaskKind -> Char
kindGlyph kind =
  case kind of
    TaskIO -> '*'
    TaskParse -> '▁'
    TaskResolve -> '▂'
    TaskTypeCheck -> '▄'
    TaskBackend -> '█'

kindColor :: TaskKind -> String
kindColor kind =
  case kind of
    TaskIO -> ""
    TaskParse -> "37"
    TaskResolve -> "32"
    TaskTypeCheck -> "34"
    TaskBackend -> "35"

colorize :: Bool -> String -> String -> String
colorize useColor color value
  | useColor = "\ESC[" <> color <> "m" <> value <> "\ESC[0m"
  | otherwise = value

renderDuration :: Word64 -> String
renderDuration nanoseconds
  | nanoseconds >= 60000000000 = renderScaledDuration 60000000000 "min"
  | nanoseconds >= 1000000000 = renderScaledDuration 1000000000 "s"
  | nanoseconds >= 1000000 = renderScaledDuration 1000000 "ms"
  | nanoseconds >= 1000 = renderScaledDuration 1000 "µs"
  | otherwise = renderScaledDuration 1 "ns"
  where
    renderScaledDuration divisor unit =
      let value = fromIntegral nanoseconds / divisor :: Double
          decimalPlaces
            | value >= 100 = 1
            | value >= 10 = 2
            | otherwise = 3
       in showFFloat (Just decimalPlaces) value "" <> " " <> unit

padLeft :: Int -> String -> String
padLeft width value = replicate (max 0 (width - length value)) ' ' <> value
