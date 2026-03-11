{-# LANGUAGE BangPatterns, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -O2 #-}
{- This module does a lot of calculation that can be expensive, so optimise
 - it well -}

-- | 
-- Copyright: 2015 Joey Hess <id@joeyh.name>
-- License: BSD-2-clause
-- 
-- Console regions are displayed near the bottom of the console, and can be
-- updated concurrently by threads. 
--
-- Any other output lines displayed using
-- `outputConcurrent` and `createProcessConcurrent`
-- will scroll up above the open console regions.
--
-- For example, this program:
--
-- > import Control.Concurrent.Async
-- > import Control.Concurrent
-- > import System.Console.Concurrent
-- > import System.Console.Regions
-- > import System.Process
-- > 
-- > main = displayConsoleRegions $ do
-- > 	mapConcurrently download [1..5]
-- >		`concurrently` mapM_ message [1..10]
-- >		`concurrently` createProcessConcurrent (proc "echo" ["hello world"])
-- > 
-- > message :: Int -> IO ()
-- > message n = do
-- > 	threadDelay 500000
-- > 	outputConcurrent ("Message " ++ show n ++ "\n")
-- > 
-- > download :: Int -> IO ()
-- > download n = withConsoleRegion Linear $ \r -> do
-- > 	setConsoleRegion r basemsg
-- > 	go n r
-- >   where
-- > 	basemsg = "Download " ++ show n
-- >	go c r
-- >		| c < 1 = finishConsoleRegion r (basemsg ++ " done!")
-- > 		| otherwise = do
-- > 			threadDelay 1000000
-- > 			appendConsoleRegion r " ... "
-- > 			go (c-1) r
--
-- Will display like this:
--
-- > Message 1
-- > hello world
-- > Message 2
-- > Download 1 ...
-- > Download 2 ...
-- > Download 3 ...
--
-- Once the 1st download has finished, and another message has displayed,
-- the console will update like this:
--
-- > Message 1
-- > hello world
-- > Message 2
-- > Download 1 done!
-- > Message 3
-- > Download 2 ... ...
-- > Download 3 ... ...

module System.Console.Regions (
	-- * Types
	ConsoleRegion,
	RegionLayout(..),
	ToRegionContent(..),
	RegionContent(..),
	LiftRegion(..),
	-- * Initialization
	displayConsoleRegions,
	withConsoleRegion,
	openConsoleRegion,
	newConsoleRegion,
	closeConsoleRegion,
	-- * Region content and display
	setConsoleRegion,
	appendConsoleRegion,
	finishConsoleRegion,
	getConsoleRegion,
	tuneDisplay,
	-- * STM region contents
	--
	-- | The `ToRegionContent` instance for `STM` `Text` can be used to
	-- make regions that automatically update whenever there's
	-- a change to any of the STM values that they use.
	--
	-- For example, a region that displays the screen size,
	-- and automatically refreshes it:
	--
	-- > import qualified Data.Text as T
	--
	-- > r <- openConsoleRegion Linear s
	-- > setConsoleRegion r $ do
	-- > 	w <- readTVar consoleWidth
	-- > 	h <- readTVar consoleHeight
	-- > 	return $ T.pack $ unwords
	-- > 		[ "size:"
	-- >		, show w
	-- > 		, "x"
	-- >		, show h
	-- > 		]
	-- >
	consoleWidth,
	consoleHeight,
	regionList,
	waitDisplayChange,
) where

import Data.Monoid
import Data.String
import Data.Char
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.IO as T
import Data.Text (Text)
import Control.Monad
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Control.Concurrent.Async
import System.Console.ANSI
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import Text.Read
import Data.List (intercalate, nubBy)
import Control.Applicative
import Prelude
#ifdef VERSION_terminal_size
import qualified System.Console.Terminal.Size as Console
#ifndef mingw32_HOST_OS
import System.Posix.Signals
import System.Posix.Signals.Exts
#endif
#endif

import System.Console.Concurrent
import Utility.Monad
import Utility.Exception

-- | Controls how a region is laid out in the console.
--
-- Here's an annotated example of how the console layout works.
-- Each sequence of the same letter represents a distinct region.
--
-- > scrolling......
-- > scrolling......
-- > scrolling......
-- > aaaaaa......... -- Linear
-- > bbbbbbbbbbbbbbb -- Linear
-- > bbb............       (expanded to multiple lines)
-- > ccccccccc...... -- Linear
-- > ddddeeeefffffff -- [InLine]
-- > fffffggggg.....       (expanded to multiple lines)
-- > 
data RegionLayout = Linear | InLine ConsoleRegion
	deriving (Eq)

-- | A handle allowing access to a region of the console.
newtype ConsoleRegion = ConsoleRegion (TVar R)
	deriving (Eq)

data R = R
	{ regionContent :: RegionContent
	, regionRender :: (Text -> STM Text)
	, regionLayout :: RegionLayout
	, regionChildren :: TVar [ConsoleRegion]
	}

newtype RegionContent = RegionContent (STM Text)

-- | All the regions that are currently displayed on the screen.
--
-- The list is ordered from the bottom of the screen up. Reordering
-- it will change the order in which regions are displayed.
-- It's also fine to remove, duplicate, or add new regions to the list.
{-# NOINLINE regionList #-}
regionList :: TMVar [ConsoleRegion]
regionList = unsafePerformIO newEmptyTMVarIO

data ConsoleSize = ConsoleSize
	{ _consoleHeight :: Int
	, _consoleWidth :: Int
	}

{-# NOINLINE consoleSize #-}
consoleSize :: TVar ConsoleSize
consoleSize = unsafePerformIO $ newTVarIO $
	ConsoleSize { _consoleWidth = 80, _consoleHeight = 25}

type Width = Int

-- | Gets the width of the console.
--
-- On Unix, this is automatically updated when the terminal is resized.
-- On Windows, it is determined at start. On WASM,
-- the console width is hard coded to 80 since WASI does not provide a way
-- to determine it.
consoleWidth :: STM Int
consoleWidth = munge . _consoleWidth <$> readTVar consoleSize
  where
#ifndef mingw32_HOST_OS
	munge = id
#else
	-- On Windows, writing to the right-most column caused some
	-- problimatic wrap, so avoid it.
	munge = pred
#endif

-- | Get the height of the console.
--
-- On Unix, this is automatically updated when the terminal is resized.
-- On Windows, it is determined at start. On WASM,
-- the console heigth is hard coded to 25 since WASI does not provide a way
-- to determine it.
consoleHeight :: STM Int
consoleHeight = _consoleHeight <$> readTVar consoleSize

-- | Check if `displayConsoleRegions` is running.
regionDisplayEnabled :: IO Bool
regionDisplayEnabled = atomically $ not <$> isEmptyTMVar regionList

-- | Many actions in this module can be run in either the IO monad
-- or the STM monad. Using STM allows making several changes to the
-- displayed regions atomically, with the display updated a single time.
class LiftRegion m where
	liftRegion :: STM a -> m a

instance LiftRegion STM where
	liftRegion = id

instance LiftRegion IO where
	liftRegion = atomically

-- | Values that can be displayed in a region.
class ToRegionContent v where
	toRegionContent :: v -> RegionContent

instance ToRegionContent String where
	toRegionContent = fromOutput

instance ToRegionContent Text where
	toRegionContent = fromOutput

-- | Note that using a lazy Text in a region will buffer it all in memory.
instance ToRegionContent L.Text where
	toRegionContent = fromOutput

fromOutput :: Outputable v => v -> RegionContent
fromOutput = RegionContent . pure . toOutput

-- | Makes a STM action be run to get the content of a region.
--
-- Any change to the values that action reads will result in an immediate
-- refresh of the display.
instance ToRegionContent (STM Text) where
	toRegionContent = RegionContent

-- | Sets the value of a console region. This will cause the
-- console to be updated to display the new value.
--
-- It's fine for the value to be longer than the terminal is wide,
-- or to include newlines ('\n'). Regions expand to multiple lines as
-- necessary.
--
-- The value can include ANSI SGR escape sequences for changing
-- the colors of all or part of a region. For this to display properly,
-- a reset escape sequence must be included to get the color back
-- to default. System.Console.ANSI makes it easy to construct such
-- values. For example:
--
-- > import System.Console.ANSI
-- > 
-- > setConsoleRegion region 
-- > 	( "hello "
-- > 	<> setSGRCode [SetColor Foreground Vivid Red] 
-- >	<> "Mars" 
-- >	<> setSGRCode [Reset]
-- > 	<> "!"
-- >	)
-- 
-- Other ANSI escape sequences, especially those doing cursor
-- movement, will mess up the layouts of regions. Caveat emptor.
--
-- ANSI SGR escape sequences that span multiple lines do not currently
-- display as you might hope. (Patches would be accepted.)
setConsoleRegion :: (ToRegionContent v, LiftRegion m) => ConsoleRegion -> v -> m ()
setConsoleRegion r v = liftRegion $
	modifyRegion r $ const $ pure $ toRegionContent v

-- | Appends a value to the current value of a console region.
--
-- > appendConsoleRegion progress "." -- add another dot to progress display
appendConsoleRegion :: (Outputable v, LiftRegion m) => ConsoleRegion -> v -> m ()
appendConsoleRegion r v = liftRegion $
	modifyRegion r $ \(RegionContent a) ->
		return $ RegionContent $ do
			t <- a
			return (t <> toOutput v)

modifyRegion :: ConsoleRegion -> (RegionContent -> STM RegionContent) -> STM ()
modifyRegion (ConsoleRegion tv) f = do
	r <- readTVar tv
	rc <- f (regionContent r)
	let r' = r { regionContent = rc }
	writeTVar tv r'

readRegionContent :: RegionContent -> STM Text
readRegionContent (RegionContent a) = a

resizeRegion :: Width -> ConsoleRegion -> STM [Text]
resizeRegion width (ConsoleRegion tv) = do
	r <- readTVar tv
	ls <- calcRegionLines r width
	return ls

-- | Runs the action with a new console region, closing the region when
-- the action finishes or on exception.
withConsoleRegion :: (MonadIO m, MonadMask m) => RegionLayout -> (ConsoleRegion -> m a) -> m a
withConsoleRegion ly = bracketIO
	(openConsoleRegion ly)
	(uninterruptibleMask_ . closeConsoleRegion)

-- | Opens a new console region.
openConsoleRegion :: LiftRegion m => RegionLayout -> m ConsoleRegion
openConsoleRegion ly = liftRegion $ do
	h <- newConsoleRegion ly T.empty
	case ly of
		Linear -> do
			ml <- tryTakeTMVar regionList
			case ml of
				Just l -> putTMVar regionList (h:l)
				-- displayConsoleRegions is not active, so
				-- it's not put on any list, and won't display
				Nothing -> return ()
		InLine parent -> addChild h parent
	return h

-- | Makes a new region, but does not add it to the display.
newConsoleRegion :: (LiftRegion m) => ToRegionContent v => RegionLayout -> v -> m ConsoleRegion
newConsoleRegion ly v = liftRegion $ do
	cs <- newTVar mempty
	let r = R
		{ regionContent = RegionContent $ return mempty
		, regionRender = pure
		, regionLayout = ly
		, regionChildren = cs
		}
	h <- ConsoleRegion <$> newTVar r
	displayChildren h
	setConsoleRegion h v
	return h

displayChildren :: ConsoleRegion -> STM ()
displayChildren p@(ConsoleRegion tv) = tuneDisplay p $ \t -> do
	children <- readTVar . regionChildren =<< readTVar tv
	ct <- T.concat <$> mapM getc children
	return $ t <> ct
  where
	getc (ConsoleRegion cv) = do
		c <- readTVar cv
		regionRender c =<< readRegionContent (regionContent c)

-- | Closes a console region. Once closed, the region is removed from the
-- display.
closeConsoleRegion :: LiftRegion m => ConsoleRegion -> m ()
closeConsoleRegion h@(ConsoleRegion tv) = liftRegion $ do
	v <- tryTakeTMVar regionList
	case v of
		Just l ->
			let !l' = filter (/= h) l
			in putTMVar regionList l'
		_ -> return ()
	ly <- regionLayout <$> readTVar tv
	case ly of
		Linear -> return ()
		InLine parent -> removeChild h parent

-- | Closes the console region, and displays the passed value in the
-- scrolling area above the active console regions. When Nothing is passed,
-- displays the current value of the console region.
finishConsoleRegion :: (Outputable v, LiftRegion m) => ConsoleRegion -> v -> m ()
finishConsoleRegion h v = liftRegion $ do
	closeConsoleRegion h
	bufferOutputSTM StdOut (toOutput v <> fromString "\n")

-- | Gets the current content of a console region.
getConsoleRegion :: LiftRegion m => ConsoleRegion -> m Text
getConsoleRegion (ConsoleRegion tv) = liftRegion $
	readRegionContent . regionContent =<< readTVar tv

-- | Changes how a console region displays.
--
-- Each time the region's value changes, the STM action is provided
-- with the current value of the region, and returns the value to display.
--
-- For example, this will prevent a region from ever displaying more
-- than 10 characters wide, and will make it display text reversed:
-- 
-- > tuneDisplay myregion $ pure . T.take 10
-- > tuneDisplay myregion $ pure . T.reverse
--
-- Note that repeated calls to tuneDisplay are cumulative.
--
-- Normally, the STM action should avoid retrying, as that would
-- block all display updates.
tuneDisplay :: LiftRegion m => ConsoleRegion -> (Text -> STM Text) -> m ()
tuneDisplay (ConsoleRegion tv) renderer = liftRegion $ do
	r <- readTVar tv
	let rr = \t -> renderer =<< regionRender r t
	let r' = r { regionRender = rr }
	writeTVar tv r'

addChild :: ConsoleRegion -> ConsoleRegion -> STM ()
addChild child _parent@(ConsoleRegion pv) = do
	cv <- regionChildren <$> readTVar pv
	children <- readTVar cv
	let !children' = filter (/= child) children ++ [child]
	writeTVar cv children'

removeChild :: ConsoleRegion -> ConsoleRegion -> STM ()
removeChild child _parent@(ConsoleRegion pv) = do
	cv <- regionChildren <$> readTVar pv
	modifyTVar' cv (filter (/= child))

-- | Handles all display for the other functions in this module.
--
-- Note that this uses `lockOutput`, so it takes over all output to the
-- console while the passed IO action is running. As well as displaying
-- the console regions, this handles display of anything buffered by
-- `outputConcurrent` and `createProcessConcurrent`. So,
-- `withConcurrentOutput` and `flushConcurrentOutput` should not be run
-- while this is in use, and will block.
--
-- When standard output is not an ANSI capable terminal,
-- console regions are not displayed.
displayConsoleRegions :: (MonadIO m, MonadMask m) => m a -> m a
displayConsoleRegions a = ifM (liftIO regionDisplayEnabled)
	( a -- displayConsoleRegions is already running
	, lockOutput $ bracket setup cleanup (const a)
	)
  where
	setup = liftIO $ uninterruptibleMask $ \unmask -> do
		atomically $ putTMVar regionList []
		endsignal <- atomically $ do
			s <- newTSem 1
			waitTSem s
			return s
		isterm <- liftIO $ hSupportsANSI stdout
		when isterm (unmask trackConsoleWidth)
		da <- async $ unmask $ displayThread isterm endsignal
		return (isterm, da, endsignal)
	cleanup (isterm, da, endsignal) = liftIO $ uninterruptibleMask_ $ do
		atomically $ signalTSem endsignal
		void $ wait da
		void $ atomically $ takeTMVar regionList
		when isterm $
			installResizeHandler Nothing

trackConsoleWidth :: IO ()
#ifdef VERSION_terminal_size
trackConsoleWidth = do
	let getsz = maybe noop (atomically . writeTVar consoleSize . conv)
		=<< Console.size
	getsz
	installResizeHandler (Just getsz)
  where
	conv wsz = ConsoleSize
		{ _consoleWidth = Console.width wsz
		, _consoleHeight = Console.height wsz
		}
#else
trackConsoleWidth = return ()
#endif

data DisplayChange
	= BufferChange BufferSnapshot 
	| RegionChange RegionSnapshot
	| RegionListChange RegionSnapshot
	| TerminalResize Width
	| Shutdown
	| DisplayChangeBarrier Barrier

type BufferSnapshot = (StdHandle, OutputBuffer)
type RegionSnapshot = ([ConsoleRegion], [R], [[Text]])
type Barrier = Integer

-- | This is a broadcast TChan, which gets a DisplayChange written to it
-- after the display has been updated. It can be used to wait for something
-- to be displayed.
{-# NOINLINE displayUpdateNotifier #-}
displayUpdateNotifier :: TChan DisplayChange
displayUpdateNotifier = unsafePerformIO $ newBroadcastTChanIO

{-# NOINLINE displayChangeBarrier #-}
displayChangeBarrier :: TVar Barrier
displayChangeBarrier = unsafePerformIO $ newTVarIO 0

-- | Runs a STM action, and waits for the display to be fully updated
-- with any changes that action makes to the displayed regions.
waitDisplayChange :: STM a -> IO a
waitDisplayChange a = do
	c <- atomically $ dupTChan displayUpdateNotifier
	bv <- newEmptyTMVarIO
	_ <- setbarrier bv `concurrently` waitchange c bv
	snd <$> atomically (readTMVar bv)
  where
	setbarrier bv = atomically $ do
		!b <- succ <$> readTVar displayChangeBarrier
		r <- a
		writeTVar displayChangeBarrier b
		putTMVar bv (b, r)
	waitchange c bv = do
		change <- atomically $ readTChan c
		-- this blocks until the STM action has run, and the
		-- barrier is set.
		b <- fst <$> atomically (readTMVar bv)
		case change of
			DisplayChangeBarrier b' | b' >= b -> return ()
			_ -> waitchange c bv

displayThread :: Bool -> TSem -> IO ()
displayThread isterm endsignal = do
	origwidth <- atomically consoleWidth
	origbarrier <- atomically (readTVar displayChangeBarrier)
	go ([], [], []) origwidth origbarrier
  where
	go origsnapshot@(orighandles, origregions, origlines) origwidth origbarrier = do
		let waitwidthchange = do
			w <- consoleWidth
			if w == origwidth then retry else return w
		let waitbarrierchange = do
			b <- readTVar displayChangeBarrier
			if b /= origbarrier
				then return b
				else retry
		let waitanychange =
			(RegionChange <$> regionWaiter origsnapshot origwidth)
				`orElse`
			(RegionListChange <$> regionListWaiter origsnapshot)
				`orElse`
			(BufferChange <$> outputBufferWaiterSTM waitCompleteLines)
				`orElse`
			(TerminalResize <$> waitwidthchange)
				`orElse`
			(waitTSem endsignal >> pure Shutdown)
				`orElse`
			-- Must come last, so the changes above are
			-- processed before barriers.
			(DisplayChangeBarrier <$> waitbarrierchange)
		(change, height) <- atomically $ (,)
			<$> waitanychange
			<*> consoleHeight
		let onscreen = take (height - 1) . concat
		let update snapshot@(_, _, newlines) = do
			when isterm $
				changedLines (onscreen origlines) (onscreen newlines)
			return $ go snapshot origwidth origbarrier
		next <- case change of
			RegionChange snapshot -> update snapshot
			RegionListChange snapshot -> update snapshot
			BufferChange (h, buf) -> do
				-- Note that even when every available line
				-- is dedicated to visible regions, the
				-- buffer is still displayed. It would be
				-- more efficient to not display it, but
				-- this makes it available in scroll back.
				let origlines' = onscreen origlines
				inAreaAbove isterm (length origlines') origlines' $
					emitOutputBuffer h buf
				return $ go origsnapshot origwidth origbarrier
			TerminalResize newwidth -> do
				newlines <- atomically (mapM (resizeRegion newwidth) orighandles)
				when isterm $ do
					resizeRecovery (onscreen newlines)
				return $ go (orighandles, origregions, newlines) newwidth origbarrier
			Shutdown ->
				return $ return ()
			DisplayChangeBarrier b ->
				return $ go origsnapshot origwidth b
		hFlush stdout
		atomically $ writeTChan displayUpdateNotifier change
		next

readRegions :: [ConsoleRegion] -> STM [R]
readRegions = mapM (\(ConsoleRegion h) -> readTVar h)

-- | Wait for any changes to the region list, eg adding or removing a region.
regionListWaiter :: RegionSnapshot -> STM RegionSnapshot
regionListWaiter (orighandles, _origregions, origlines) = do
	handles <- readTMVar regionList
	if handles == orighandles
		then retry
		else do
			rs <- readRegions handles
			return (handles, rs, origlines)

-- Wait for any changes to any of the contents of regions currently in the
-- region list.
regionWaiter :: RegionSnapshot -> Width -> STM RegionSnapshot
regionWaiter (orighandles, _origregions, origlines) width = do
	rs <- readRegions orighandles
	newlines <- mapM getr rs
	unless (newlines /= origlines)
		retry
	return (orighandles, rs, newlines)
  where
	getr r = calcRegionLines r width

-- This is not an optimal screen update like curses can do, but it's
-- pretty efficient, most of the time!
changedLines :: [Text] -> [Text] -> IO ()
changedLines origlines newlines
	| delta == 0 = do
		-- The total number of lines is unchanged, so update
		-- whichever ones have changed, and leave the rest as-is.
		diffUpdate origlines newlines
	| delta > 0 = do
		-- Added more lines, so output each, with a
		-- newline, thus scrolling the old lines up
		-- the screen. (We can do this, because the cursor
		-- is left below the first line.)
		let addedlines = reverse (take delta newlines)
		displayLines addedlines
		-- Some existing lines may have also changed..
		let scrolledlines = addedlines ++ origlines
		diffUpdate scrolledlines newlines
	| otherwise = do
		-- Some lines were removed. Move up that many lines,
		-- clearing each line, and update any changed lines.
		replicateM_ (abs delta) $ do
			setCursorColumn 0
			cursorUp 1
			clearLine
		diffUpdate (drop (abs delta) origlines) newlines
  where
	delta = length newlines - length origlines

diffUpdate :: [Text] -> [Text] -> IO ()
diffUpdate old new = updateLines (zip (zip new changed) old)
  where
	changed = map (uncurry (/=)) (zip new old) ++ repeat True

changeOffsets :: [((r, Bool), r)] -> Int -> [((r, Int), r)] -> [((r, Int), r)]
changeOffsets [] _ c = reverse c
changeOffsets (((new, changed), old):rs) n c
	| changed = changeOffsets rs 1 (((new, n), old):c)
	| otherwise = changeOffsets rs (succ n) c

-- Displays lines that are paired with True, and skips over the rest.
-- Cursor is assumed to be just below the first line at the
-- beginning, and is put back there at the end.
updateLines :: [((Text, Bool), Text)] -> IO ()
updateLines l
	| null l' = noop
	| otherwise = do
		forM_ l' $ \((newt, offset), oldt) -> do
			setCursorColumn 0
			cursorUp offset
#ifndef mingw32_HOST_OS
			T.hPutStr stdout $
				genLineUpdate $ calcLineUpdate oldt newt
#else
			-- Windows does not support ansi characters
			-- emitted in a string, so do a full line
			-- redraw.
			T.hPutStr stdout newt
			clearFromCursorToLineEnd
#endif
		cursorDown (sum (map (snd . fst) l'))
		setCursorColumn 0
  where
	l' = changeOffsets l 1 []

-- Recover from a resize by redrawing all region lines.
--
-- The resize can change the position of the cursor, which would garble
-- the display going forward. To fix, the cursor is moved to the top of
-- the screen, which is cleared, and all regions are redrawn from there.
resizeRecovery :: [Text] -> IO ()
resizeRecovery newlines = do
	setCursorPosition 0 0
	inAreaAbove True 0 newlines $
		return ()

-- Move cursor up before the lines, performs some output there,
-- which will scroll down and overwrite the lines, so 
-- redraws all the lines below.
inAreaAbove :: Bool -> Int -> [Text] -> IO () -> IO ()
inAreaAbove isterm numlines ls outputter = do
	when isterm $ do
		unless (numlines < 1) $ do
			setCursorColumn 0
			cursorUp $ numlines
		clearFromCursorToScreenEnd
	-- Flush stdout now, because the outputter may write to stderr, so
	-- the cursor needs to be moved first.
	hFlush stdout
	outputter
	when isterm $ do
		setCursorColumn 0 -- just in case the output lacked a newline
		displayLines (reverse ls)

displayLines :: [Text] -> IO ()
displayLines = mapM_ $ \l -> do
	T.hPutStr stdout l
	putChar '\n'

installResizeHandler :: Maybe (IO ()) -> IO ()
#ifndef mingw32_HOST_OS
#ifdef VERSION_terminal_size
installResizeHandler h = void $
	installHandler windowChange (maybe Default Catch h) Nothing
#else
installResizeHandler _ = return ()
#endif
#else
installResizeHandler _ = return ()
#endif

calcRegionLines :: R -> Width -> STM [Text]
calcRegionLines r width = do
	t <- regionRender r =<< readRegionContent (regionContent r)
	return $ reverse $ calcLines t width

-- | Splits a Text into the lines it would display using when output onto
-- a console with a given width, starting from the first column.
--
-- ANSI SGR sequences are handled specially, so that color, etc settings
-- work despite the lines being split up, and the lines can be output
-- indepedently. For example, "foooREDbar bazRESET" when split into lines
-- becomes ["fooREDbarRESET", "RED bazRESET"]
calcLines :: Text -> Width -> [Text]
calcLines t width
	| width < 1 || T.null t = [t] -- even an empty text is 1 line high
	| otherwise = calcLines' width [] [] 0 1 (T.length t) t

calcLines' :: Int -> [Text] -> [Text] -> Int -> Int -> Int -> Text -> [Text]
calcLines' width collectedlines collectedSGR i displaysize len t
	| i >= len = if i > 0
		then reverse (finishline t)
		else reverse collectedlines
	| t1 == '\n' = calcLines' width (finishline $ T.init currline)
		[] 0 1 (T.length rest) (contSGR rest)
	-- ANSI escape sequences do not take up space on screen.
	| t1 == '\ESC' && i+1 < len = case T.index t (i+1) of
		'[' -> skipansi endCSI True
		']' -> skipansi endOSC False
		_ -> calcLines' width collectedlines collectedSGR (i+1) displaysize len t
	-- Control characters do not take up space on screen.
	| isControl t1 = calcLines' width collectedlines collectedSGR (i+1) displaysize len t
	| displaysize >= width = calcLines' width (finishline currline)
		[] 0 1 (T.length rest) (contSGR rest)
	| otherwise = calcLines' width collectedlines collectedSGR (i+1) (displaysize+1) len t
  where
	t1 = T.index t i
	(currline, rest) = T.splitAt (i+1) t

	skipansi toend isCSI = case T.findIndex toend (T.drop (i+2) t) of
		Just csiend -> calcLines' width collectedlines 
			(addSGR (csiend+2)) (i+2+csiend) (displaysize-1) len t
		Nothing -> reverse (finishline t)
	  where
		addSGR csiend
			| not isCSI = collectedSGR
			| ansicode == resetSGR = []
			| not (T.null ansicode) && T.last ansicode == endSGR =
				ansicode : collectedSGR
			| otherwise = collectedSGR
		  where
			ansicode = T.take (csiend + 1) (T.drop i t)
	finishline l = closeSGR l : collectedlines
	-- Close any open SGR codes at end of line
	closeSGR l
		| null collectedSGR = l
		| otherwise = l <> resetSGR
	-- Continue any open SGR codes from previous line
	contSGR l = mconcat (reverse collectedSGR) <> l

resetSGR :: Text
resetSGR = T.pack (setSGRCode [Reset])

endCSI :: Char -> Bool
endCSI c = let o = ord c in o >= 64 && o < 127

endOSC :: Char -> Bool
endOSC c = c == '\BEL'

endSGR :: Char
endSGR = 'm'

#ifndef mingw32_HOST_OS

-- | Finds the least expensive output to make a console that was displaying
-- the old line display the new line. Cursor starts at far left.
--
-- Basically, loop through and find spans where the old and new line are
-- the same. Generate cursorForwardCode ANSI sequences to skip over those
-- spans, unless such a sequence would be longer than the span it's skipping.
--
-- Since ANSI sequences can be present in the line, need to take them
-- into account. Generally, each of the sequences in new has to be included,
-- even if old contained the same sequence:
--
-- > old: GREENfoofoofooREDbarbarbarRESETbaz
-- > new: GREENfoofoofooREDxarbarbaxRESETbaz
-- > ret: GREEN-------->REDx------>yRESET
--
-- (The first GREEN does not effect any output text, so it can be elided.)
-- 
-- Also, despite old having the same second span as new, in the same
-- location, that span has to be re-emitted because its color changed:
-- 
-- > old: GREENfoofooREDbarbarbarbarbar
-- > new: GREENfoofoofooTANbarbarbar
-- > ret: GREEN----->fooTANbarbarbarCLEARREST
--
-- Also note above that the sequence has to clear the rest of the line,
-- since the new line is shorter than the old.
calcLineUpdate :: Text -> Text -> [LineUpdate]
calcLineUpdate old new = 
	reverse $ go
		(advanceLine old [] [])
		(advanceLine new [] [])
  where
	go (Just _, _, _, _) (Nothing, _, past, _) = ClearToEnd : past
	go (Nothing, _, _, _) (Nothing, _, past, _) = past
	go (Nothing, _, _, _) (Just n, ns, past, _) =
		Display ns : Display (T.singleton n) : past
	go (Just o, os, _, oinvis) (Just n, ns, past, ninvis)
		| o == n && oinvis == ninvis = go
			(advanceLine os [] oinvis)
			(advanceLine ns (Skip [o] : past) ninvis)
		| otherwise = go
			(advanceLine os [] oinvis)
			(advanceLine ns (Display (T.singleton n) : past) ninvis)

type Past = [LineUpdate]
type Invis = [LineUpdate]

-- Find next character of t that is not a ANSI escape sequence
-- or control char. Any such passed on the way to the character
-- are prepended to past, and added to invis.
--
-- resetSGR is handled specially; it causes all SGRs to be removed from
-- invis, It's still prepended to past.
advanceLine :: Text -> Past -> Invis -> (Maybe Char, Text, Past, Invis)
advanceLine t past invis
	| T.null t = (Nothing, T.empty, past, invis)
	| otherwise = case T.head t of
		'\ESC' -> case T.drop 1 t of
			t' | T.null t' -> advanceLine (T.drop 1 t)
				(Skip "\ESC":past) (Skip "\ESC":invis)
			   | otherwise -> case T.head t' of
			   	'[' -> skipansi endCSI
				']' -> skipansi endOSC
				c -> (Just c, T.drop 2 t, Skip "\ESC":past, Skip "\ESC":invis)
		c | isControl c -> advanceLine (T.drop 1 t) (Skip [c]:past) (Skip [c]:invis)
		  | otherwise -> (Just c, T.drop 1 t, past, invis)
  where
	skipansi toend = case T.findIndex toend (T.drop 2 t) of
		Just csiend -> 
			let sgr = SGR (T.take (csiend+3) t)
			in advanceLine (T.drop (csiend+3) t)
				(sgr:past) (addsgr sgr invis)
		Nothing -> (Nothing, T.empty, past, invis)
	addsgr (SGR sgrt) l
		| sgrt == resetSGR = filter (not . isSGR) l
	addsgr s l = s:l

data LineUpdate = Display Text | Skip [Char] | SGR Text | ClearToEnd
	deriving (Eq, Show)

isSGR :: LineUpdate -> Bool
isSGR (SGR _) = True
isSGR _ = False

genLineUpdate :: [LineUpdate] -> Text
genLineUpdate l = T.concat $ map tot (optimiseLineUpdate l)
  where
	tot (Display t) = t
	tot (Skip s)
		-- length (cursorForwardCode 1) == 4 so there's no point
		-- generating that for a skip of less than 5.
		| len < 5 = T.pack s
		| otherwise = T.pack (cursorForwardCode len)
	  where
		len = length s
	tot (SGR t) = t
	tot ClearToEnd = T.pack clearFromCursorToLineEndCode

optimiseLineUpdate :: [LineUpdate] -> [LineUpdate]
optimiseLineUpdate = go []
  where
	-- elide trailing Skips
	go (Skip _:rest) [] = go rest []
	-- elide SGRs at the end of the line, except for the reset SGR
	go (SGR t:rest) [] | t /= resetSGR = go rest []
	go c [] = reverse c
	-- combine adjacent SGRs and Skips
	go c (SGR t1:Skip s:SGR t2:rest) = tryharder c (SGR (combineSGR t1 t2):Skip s:rest)
	go c (Skip s:Skip s':rest) = tryharder c (Skip (s++s'):rest)
	go c (SGR t1:SGR t2:rest) = tryharder c (SGR (combineSGR t1 t2):rest)
	go c (v:rest) = go (v:c) rest
	tryharder c l = go [] (reverse c ++ l)

-- Parse and combine 2 ANSI SGR sequences into one.
combineSGR :: Text -> Text -> Text
combineSGR a b = case combineSGRCodes (codes a) (codes b) of
	Nothing -> a <> b
	Just cs -> T.pack $ "\ESC[" ++ intercalate ";" (map show cs) ++ "m"
  where
	codes = map (readMaybe . T.unpack) .
		T.split (== ';') . T.drop 2 . T.init

-- Prefers values from the second sequence when there's a conflict with
-- values from the first sequence.
combineSGRCodes :: [Maybe Int] -> [Maybe Int] -> Maybe [Int]
combineSGRCodes as bs =
	map snd . nubBy (\a b -> fst a == fst b) <$> mapM range (reverse bs ++ reverse as)
  where
	range Nothing = Nothing
	range (Just x)
		| x >= 30 && x <= 37 = Just (Foreground, x)
		| x >= 40 && x <= 47 = Just (Background, x)
		| x >= 90 && x <= 97 = Just (Foreground, x)
		| x >= 100 && x <= 107 = Just (Background, x)
		| otherwise = Nothing

#endif
