-- | The progress that @install@ and @build@ show while they run.
--
-- The commands report events: the plan, and what happens to each package
-- and executable of it. A reporter turns the events into output. On a
-- terminal the output is a live frame that redraws in place; elsewhere it
-- is one line for each event that matters, so that a log stays readable.
module Aihc.Cli.Progress
  ( ProgressItem (..),
    ProgressEvent (..),
    ProgressReporter (..),
    quietProgress,
    withProgress,
    progressTaskObserver,
    renderProgressItem,
  )
where

import Aihc.Cli.TaskGraph (TaskKind (..), TaskObserver (..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar)
import Control.Exception (bracket)
import Control.Monad (forever, unless)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Numeric (showFFloat)
import System.Console.Terminal.Size qualified as Terminal
import System.Environment (lookupEnv)
import System.IO (BufferMode (..), Handle, hFlush, hGetBuffering, hIsTerminalDevice, hPutStr, hPutStrLn, hSetBuffering)

-- | What the progress is about: a package of the plan, named with its
-- version, or an executable, named as its Cabal file names it.
data ProgressItem
  = ItemPackage !Text
  | ItemExecutable !Text
  deriving (Eq, Ord, Show)

renderProgressItem :: ProgressItem -> String
renderProgressItem item =
  case item of
    ItemPackage name -> T.unpack name
    ItemExecutable name -> T.unpack name <> " (executable)"

-- | What a command reports while it runs.
data ProgressEvent
  = -- | The items of the plan, in the order they build.
    ProgressPlan ![ProgressItem]
  | -- | The item is read, configured, and preprocessed.
    ProgressPrepare !ProgressItem
  | -- | The store holds the item, so nothing builds.
    ProgressStore !ProgressItem
  | -- | The item builds, with its module count. Its modules parse from
    -- here on, and wait for the dependencies they import.
    ProgressBuild !ProgressItem !Int
  | -- | A unit of the item type checks. The time of the item counts from
    -- the first of these, because its parse tasks run long before its
    -- dependencies are complete.
    ProgressCompile !ProgressItem
  | -- | This many more modules of the item are compiled.
    ProgressModules !ProgressItem !Int
  | -- | The item links.
    ProgressLink !ProgressItem
  | -- | The item is complete.
    ProgressDone !ProgressItem
  | -- | A task graph runs with this many workers.
    ProgressWorkers !Int
  | ProgressTaskStart !TaskKind
  | ProgressTaskEnd !TaskKind
  | -- | A line that stays, for a verbose message or a timing report.
    ProgressLog !String
  deriving (Show)

data ProgressReporter = ProgressReporter
  { progressReport :: ProgressEvent -> IO (),
    -- | Whether the lines the reporter shows can carry colors.
    progressColor :: Bool
  }

-- | A reporter that writes the log lines to the handle and drops the rest.
-- A library caller, such as a test, uses it.
quietProgress :: Handle -> ProgressReporter
quietProgress handle = ProgressReporter {progressReport = report, progressColor = False}
  where
    report event =
      case event of
        ProgressLog message -> hPutStrLn handle message
        _ -> pure ()

-- | Run an action with a reporter that writes to the handle. A terminal
-- gets the live frame, unless @TERM@ is @dumb@. Everything else gets one
-- line for each event that matters. @NO_COLOR@ removes the colors.
withProgress :: Handle -> (ProgressReporter -> IO a) -> IO a
withProgress handle action = do
  isTerminal <- hIsTerminalDevice handle
  term <- lookupEnv "TERM"
  noColor <- lookupEnv "NO_COLOR"
  if isTerminal && term /= Just "dumb"
    then withLiveProgress handle (isNothing noColor) action
    else withPlainProgress handle action

-- | Count the workers and the running tasks of a graph for the reporter.
progressTaskObserver :: ProgressReporter -> TaskObserver
progressTaskObserver reporter =
  TaskObserver
    { observeWorkers = progressReport reporter . ProgressWorkers,
      observeTaskStart = progressReport reporter . ProgressTaskStart,
      observeTaskEnd = progressReport reporter . ProgressTaskEnd
    }

-- * State

data ItemState
  = ItemWaiting
  | ItemPreparing
  | ItemInStore
  | -- | Module count, modules compiled, the time the build started, and
    -- the time the first unit type checked.
    ItemBuilding !Int !Int !Word64 !(Maybe Word64)
  | -- | Module count and start time.
    ItemLinking !Int !Word64
  | -- | Module count and elapsed time.
    ItemDone !Int !Word64
  deriving (Eq, Show)

data ProgressState = ProgressState
  { stateItems :: !(Map ProgressItem ItemState),
    -- | The items in plan order. An item the plan did not name comes last.
    stateOrder :: ![ProgressItem],
    stateWorkers :: !Int,
    -- | The running tasks of each kind.
    stateActive :: !(Map TaskKind Int),
    stateStart :: !Word64
  }

initialState :: Word64 -> ProgressState
initialState = ProgressState Map.empty [] 0 Map.empty

-- | Apply an event. An item that is complete stays complete: a later
-- graph of the same command finds it in the store, or builds it again in
-- place, and neither changes what the user sees.
applyEvent :: Word64 -> ProgressEvent -> ProgressState -> ProgressState
applyEvent now event state =
  case event of
    ProgressPlan items -> foldl' (\acc item -> setItem item ItemWaiting acc) state items
    ProgressPrepare item -> transition item (const ItemPreparing)
    ProgressStore item -> transition item (const ItemInStore)
    ProgressBuild item total -> transition item (const (ItemBuilding total 0 now Nothing))
    ProgressCompile item -> transition item compiles
    ProgressModules item count -> transition item (compiled count)
    ProgressLink item -> transition item links
    ProgressDone item -> transition item done
    ProgressWorkers count -> state {stateWorkers = count}
    ProgressTaskStart kind -> state {stateActive = Map.insertWith (+) kind 1 (stateActive state)}
    ProgressTaskEnd kind -> state {stateActive = Map.adjust (subtract 1) kind (stateActive state)}
    ProgressLog _ -> state
  where
    compiles current =
      case current of
        ItemBuilding total count start Nothing -> ItemBuilding total count start (Just now)
        other -> other
    compiled count current =
      case current of
        ItemBuilding total before start compiling -> ItemBuilding total (min total (before + count)) start compiling
        other -> other
    links current =
      case current of
        ItemBuilding total _ start compiling -> ItemLinking total (fromMaybe start compiling)
        other -> other
    done current =
      case current of
        ItemBuilding total _ start compiling -> ItemDone total (now - fromMaybe start compiling)
        ItemLinking total start -> ItemDone total (now - start)
        _ -> ItemDone 0 0
    transition item step =
      case Map.lookup item (stateItems state) of
        Just (ItemDone _ _) -> state
        Just current -> setItem item (step current) state
        Nothing -> setItem item (step ItemWaiting) state
    setItem item value acc =
      acc
        { stateItems = Map.insert item value (stateItems acc),
          stateOrder = if item `elem` stateOrder acc then stateOrder acc else stateOrder acc <> [item]
        }

itemState :: ProgressState -> ProgressItem -> ItemState
itemState state item = fromMaybe ItemWaiting (Map.lookup item (stateItems state))

-- | Whether the event moved the item to a state it was not in before.
changedTo :: ProgressState -> ProgressState -> ProgressItem -> Bool
changedTo before after item = itemState before item /= itemState after item

countItems :: (ItemState -> Bool) -> ProgressState -> Int
countItems predicate state = length (filter predicate (Map.elems (stateItems state)))

isDone, isInStore, isActive, isWaiting :: ItemState -> Bool
isDone value = case value of ItemDone _ _ -> True; _ -> False
isInStore value = value == ItemInStore
isActive value = case value of ItemPreparing -> True; ItemBuilding {} -> True; ItemLinking {} -> True; _ -> False
isWaiting value = value == ItemWaiting

-- * Plain output

-- | One line for each event that changes what a reader of a log wants to
-- know: the plan, and each item that the store holds, builds, links, or
-- completes.
withPlainProgress :: Handle -> (ProgressReporter -> IO a) -> IO a
withPlainProgress handle action = do
  now <- getMonotonicTimeNSec
  stateVar <- newMVar (initialState now)
  let report event = do
        lines' <- modifyMVar stateVar $ \before -> do
          at <- getMonotonicTimeNSec
          let after = applyEvent at event before
          pure (after, plainLines before after event)
        unless (null lines') $ do
          mapM_ (hPutStrLn handle) lines'
          hFlush handle
  action ProgressReporter {progressReport = report, progressColor = False}

plainLines :: ProgressState -> ProgressState -> ProgressEvent -> [String]
plainLines before after event =
  case event of
    ProgressPlan items -> planHeading items : map (("  " <>) . renderProgressItem) items
    ProgressStore item | changed item -> ["store  " <> renderProgressItem item]
    ProgressBuild item total | changed item -> ["build  " <> renderProgressItem item <> " (" <> renderModules total <> ")"]
    ProgressLink item | changed item -> ["link   " <> renderProgressItem item]
    ProgressDone item
      | changed item,
        ItemDone _ elapsed <- itemState after item ->
          ["built  " <> renderProgressItem item <> " in " <> renderElapsed elapsed]
    ProgressLog message -> lines message
    _ -> []
  where
    changed = changedTo before after

planHeading :: [ProgressItem] -> String
planHeading items =
  "Plan: "
    <> intercalate
      ", "
      ( [renderCount packages "package" | packages > 0]
          <> [renderCount executables "executable" | executables > 0]
      )
  where
    packages = length [() | ItemPackage _ <- items]
    executables = length [() | ItemExecutable _ <- items]

renderCount :: Int -> String -> String
renderCount count noun = show count <> " " <> noun <> (if count == 1 then "" else "s")

renderModules :: Int -> String
renderModules count
  | count == 0 = "no modules"
  | otherwise = renderCount count "module"

-- | A duration with one decimal below one minute, and as minutes and
-- seconds above it.
renderElapsed :: Word64 -> String
renderElapsed nanoseconds
  | seconds < 60 = showFFloat (Just 1) seconds " s"
  | otherwise = show minutes <> " min " <> show remainder <> " s"
  where
    seconds = fromIntegral nanoseconds / 1e9 :: Double
    whole = nanoseconds `div` 1000000000
    (minutes, remainder) = whole `divMod` 60

-- * Live output

-- | What the frame thread and the event handlers share.
data LiveState = LiveState
  { liveProgress :: !ProgressState,
    -- | The lines that stay, newest first, written before the next frame.
    livePending :: ![String],
    -- | How many lines the frame on the screen has.
    liveFrameLines :: !Int,
    liveTick :: !Int
  }

-- | A frame at the bottom of the terminal that redraws ten times a second,
-- below the lines that stay. The frame shows the items that build, with
-- a bar each, how many wait, and how busy the workers are.
withLiveProgress :: Handle -> Bool -> (ProgressReporter -> IO a) -> IO a
withLiveProgress handle color action = do
  now <- getMonotonicTimeNSec
  liveVar <- newMVar (LiveState (initialState now) [] 0 0)
  let report event =
        modifyMVar_ liveVar $ \live -> do
          at <- getMonotonicTimeNSec
          let before = liveProgress live
              after = applyEvent at event before
          width <- terminalWidth handle
          pure
            live
              { liveProgress = after,
                livePending = reverse (liveLines color width before after event) <> livePending live
              }
      reporter = ProgressReporter {progressReport = report, progressColor = color}
      setup = do
        buffering <- hGetBuffering handle
        hSetBuffering handle (BlockBuffering Nothing)
        hPutStr handle hideCursor
        hFlush handle
        pure buffering
      teardown buffering = do
        modifyMVar_ liveVar $ \live -> do
          let pending = reverse (livePending live)
          hPutStr handle (eraseFrame (liveFrameLines live) <> concatMap (<> "\n") pending <> showCursor)
          hFlush handle
          pure live {livePending = [], liveFrameLines = 0}
        hSetBuffering handle buffering
  bracket setup teardown $ \_ ->
    withAsync (forever (threadDelay 100000 >> redrawLive handle color liveVar)) $ \_ ->
      action reporter

redrawLive :: Handle -> Bool -> MVar LiveState -> IO ()
redrawLive handle color liveVar =
  modifyMVar_ liveVar $ \live -> do
    now <- getMonotonicTimeNSec
    (width, height) <- terminalSize handle
    let pending = reverse (livePending live)
        frame = renderFrame color width height now (liveTick live) (liveProgress live)
    hPutStr handle (eraseFrame (liveFrameLines live) <> concatMap (<> "\n") (pending <> frame))
    hFlush handle
    pure live {livePending = [], liveFrameLines = length frame, liveTick = liveTick live + 1}

-- | The columns and rows of the terminal. A terminal that reports no size,
-- such as a pseudo-terminal without a window, counts as 80 by 24.
terminalSize :: Handle -> IO (Int, Int)
terminalSize handle = do
  window <- Terminal.hSize handle
  let known fallback value = if value > 0 then value else fallback
  pure (maybe 80 (known 80 . Terminal.width) window, maybe 24 (known 24 . Terminal.height) window)

terminalWidth :: Handle -> IO Int
terminalWidth handle = fst <$> terminalSize handle

-- | Move up over the frame and clear from there to the end of the screen.
eraseFrame :: Int -> String
eraseFrame count
  | count <= 0 = ""
  | otherwise = "\ESC[" <> show count <> "A\r\ESC[J"

hideCursor, showCursor :: String
hideCursor = "\ESC[?25l"
showCursor = "\ESC[?25h"

-- | The lines of the live output that stay: the plan, and each item that
-- completes. An item the store holds is counted in the frame and not
-- listed, so that a plan with many installed packages stays short.
liveLines :: Bool -> Int -> ProgressState -> ProgressState -> ProgressEvent -> [String]
liveLines color width before after event =
  case event of
    ProgressPlan items -> planHeading items : wrapItems width (map renderProgressItem items)
    ProgressDone item
      | changedTo before after item,
        ItemDone total elapsed <- itemState after item ->
          [ sgr color green "✔ "
              <> renderProgressItem item
              <> sgr color dim ("  " <> intercalate "  " ([renderModules total | total > 0] <> [renderElapsed elapsed]))
          ]
    ProgressLog message -> lines message
    _ -> []

-- | Lay the names out in rows of the width, indented by two columns.
wrapItems :: Int -> [String] -> [String]
wrapItems width = go [] 0
  where
    limit = max 20 width - 2
    go row used names =
      case names of
        [] -> [flush row | not (null row)]
        name : rest
          | null row -> go [name] (length name) rest
          | used + 2 + length name <= limit -> go (name : row) (used + 2 + length name) rest
          | otherwise -> flush row : go [name] (length name) rest
    flush row = "  " <> intercalate "  " (reverse row)

-- | The frame: one line for each item that builds, a line for the items
-- that wait, and a summary line. The frame fits the terminal: the lines
-- are cut at its width, and the items past its height are counted.
renderFrame :: Bool -> Int -> Int -> Word64 -> Int -> ProgressState -> [String]
renderFrame color width height now tick state
  | Map.null (stateItems state) = [sgr color cyan (spinner tick) <> " Solving the plan · " <> renderClock (now - stateStart state)]
  | otherwise = map (truncateLine width) (itemLines <> hiddenLine <> waitingLine <> [summaryLine])
  where
    active = [(item, value) | item <- stateOrder state, let value = itemState state item, isActive value]
    room = max 1 (height - 4)
    shown = take room active
    hidden = length active - length shown
    nameWidth = min 40 (maximum (0 : map (length . renderProgressItem . fst) shown))
    itemLines = map renderActive shown
    hiddenLine = ["  " <> sgr color dim ("… " <> show hidden <> " more") | hidden > 0]
    waiting = countItems isWaiting state
    waitingLine = ["  " <> sgr color dim (show waiting <> " waiting") | waiting > 0]
    renderActive (item, value) =
      sgr color cyan (spinner tick <> " ")
        <> padRight nameWidth (renderProgressItem item)
        <> "  "
        <> case value of
          ItemPreparing -> sgr color dim "configuring"
          ItemBuilding 0 _ _ _ -> sgr color dim "no modules"
          ItemBuilding modules done _ _ -> renderBar color modules done <> "  " <> padLeft (length (show modules)) (show done) <> "/" <> show modules <> " modules"
          ItemLinking _ _ -> sgr color dim "linking"
          _ -> ""
    total = Map.size (stateItems state)
    built = countItems isDone state
    inStore = countItems isInStore state
    busy = sum (Map.elems (stateActive state))
    kinds = [show count <> " " <> kindName kind | (kind, count) <- Map.toList (stateActive state), count > 0]
    summaryLine =
      sgr color bold (show built <> "/" <> show total <> " built")
        <> (if inStore > 0 then " · " <> show inStore <> " in store" else "")
        <> " · "
        <> show busy
        <> "/"
        <> show (stateWorkers state)
        <> " threads busy"
        <> (if null kinds then "" else sgr color dim (" (" <> intercalate ", " kinds <> ")"))
        <> " · "
        <> renderClock (now - stateStart state)

renderBar :: Bool -> Int -> Int -> String
renderBar color total done =
  sgr color green (replicate filled '█') <> sgr color dim (replicate (cells - filled) '░')
  where
    cells = 20
    filled
      | total <= 0 = 0
      | otherwise = min cells ((done * cells) `div` total)

kindName :: TaskKind -> String
kindName kind =
  case kind of
    TaskPackage -> "package"
    TaskParse -> "parse"
    TaskResolve -> "resolve"
    TaskTypeCheck -> "type-check"
    TaskBackend -> "backend"

spinner :: Int -> String
spinner tick = [frames !! (tick `mod` length frames)]
  where
    frames = "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏"

-- | Minutes and seconds, as a clock shows them.
renderClock :: Word64 -> String
renderClock nanoseconds = show minutes <> ":" <> padLeftWith '0' 2 (show seconds)
  where
    (minutes, seconds) = (nanoseconds `div` 1000000000) `divMod` 60

-- | Cut a line at the width, so that it does not wrap and move the frame.
-- The escape sequences take no columns and are kept whole.
truncateLine :: Int -> String -> String
truncateLine width = go (max 1 width - 1)
  where
    go remaining text =
      case text of
        [] -> []
        '\ESC' : rest ->
          let (sequence', after) = break (== 'm') rest
           in '\ESC' : sequence' <> take 1 after <> go remaining (drop 1 after)
        character : rest
          | remaining <= 0 -> []
          | otherwise -> character : go (remaining - 1) rest

sgr :: Bool -> String -> String -> String
sgr color code text
  | color && not (null text) = "\ESC[" <> code <> "m" <> text <> "\ESC[0m"
  | otherwise = text

bold, dim, green, cyan :: String
bold = "1"
dim = "2"
green = "32"
cyan = "36"

padRight :: Int -> String -> String
padRight width text = text <> replicate (width - length text) ' '

padLeft :: Int -> String -> String
padLeft = padLeftWith ' '

padLeftWith :: Char -> Int -> String -> String
padLeftWith fill width text = replicate (width - length text) fill <> text
