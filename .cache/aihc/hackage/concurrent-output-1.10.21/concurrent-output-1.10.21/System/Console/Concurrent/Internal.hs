{-# LANGUAGE BangPatterns, TypeSynonymInstances, FlexibleInstances, TupleSections #-}
{-# OPTIONS_GHC -O2 #-}
{- Building this module with -O0 causes streams not to fuse and too much
 - memory to be used. -}

-- | 
-- Copyright: 2015 Joey Hess <id@joeyh.name>
-- License: BSD-2-clause
-- 
-- Concurrent output handling, internals.
--
-- May change at any time.

module System.Console.Concurrent.Internal where

import System.IO
import System.Directory
import System.Exit
import Control.Monad
import Control.Monad.IO.Class (liftIO, MonadIO)
import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import Data.Maybe
import Data.List
import Data.Monoid
import qualified System.Process as P
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as L
import Control.Applicative
import Prelude

import Utility.Monad
import Utility.Exception

data OutputHandle = OutputHandle
	{ outputLock :: TMVar Lock
	, outputBuffer :: TMVar OutputBuffer
	, errorBuffer :: TMVar OutputBuffer
	, outputThreads :: TMVar Integer
	}

data Lock = Locked

-- | A shared global variable for the OutputHandle.
{-# NOINLINE globalOutputHandle #-}
globalOutputHandle :: OutputHandle
globalOutputHandle = unsafePerformIO $ OutputHandle
	<$> newEmptyTMVarIO
	<*> newTMVarIO (OutputBuffer [])
	<*> newTMVarIO (OutputBuffer [])
	<*> newTMVarIO 0

-- | Holds a lock while performing an action. This allows the action to
-- perform its own output to the console, without using functions from this
-- module.
--
-- While this is running, other threads that try to lockOutput will block.
-- Any calls to `outputConcurrent` and `createProcessConcurrent` will not
-- block, but the output will be buffered and displayed only once the
-- action is done.
lockOutput :: (MonadIO m, MonadMask m) => m a -> m a
lockOutput = bracket_ (liftIO takeOutputLock) (liftIO dropOutputLock)

-- | Blocks until we have the output lock.
takeOutputLock :: IO ()
takeOutputLock = void $ takeOutputLock' True

-- | Tries to take the output lock, without blocking.
tryTakeOutputLock :: IO Bool
tryTakeOutputLock = takeOutputLock' False

withLock :: (TMVar Lock -> STM a) -> IO a
withLock a = atomically $ a (outputLock globalOutputHandle)

takeOutputLock' :: Bool -> IO Bool
takeOutputLock' block = do
	locked <- withLock $ \l -> do
		v <- tryTakeTMVar l
		case v of
			Just Locked
				| block -> retry
				| otherwise -> do
					-- Restore value we took.
					putTMVar l Locked
					return False
			Nothing -> do
				putTMVar l Locked
				return True
	when locked $ do
		(outbuf, errbuf) <- atomically $ (,)
			<$> swapTMVar (outputBuffer globalOutputHandle) (OutputBuffer [])
			<*> swapTMVar (errorBuffer globalOutputHandle) (OutputBuffer [])
		emitOutputBuffer StdOut outbuf
		emitOutputBuffer StdErr errbuf
	return locked

-- | Only safe to call after taking the output lock.
dropOutputLock :: IO ()
dropOutputLock = withLock $ void . takeTMVar

-- | Use this around any actions that use `outputConcurrent`
-- or `createProcessConcurrent`, unless 
-- `System.Console.Regions.displayConsoleRegions` is being used.
--
-- This is necessary to ensure that buffered concurrent output actually
-- gets displayed before the program exits.
withConcurrentOutput :: (MonadIO m, MonadMask m) => m a -> m a
withConcurrentOutput a = a `finally` liftIO flushConcurrentOutput

-- | Blocks until any processes started by `createProcessConcurrent` have
-- finished, and any buffered output is displayed. Also blocks while
-- `lockOutput` is is use.
--
-- `withConcurrentOutput` calls this at the end, so you do not normally
-- need to use this.
flushConcurrentOutput :: IO ()
flushConcurrentOutput = do
	atomically $ do
		r <- takeTMVar (outputThreads globalOutputHandle)
		if r <= 0
			then putTMVar (outputThreads globalOutputHandle) r
			else retry
	-- Take output lock to wait for anything else that might be
	-- currently generating output.
	lockOutput $ return ()

-- | Values that can be output.
class Outputable v where
	toOutput :: v -> T.Text

instance Outputable T.Text where
	toOutput = id

-- | Note that using a lazy Text as an Outputable value 
-- will buffer it all in memory.
instance Outputable L.Text where
	toOutput = toOutput . L.toStrict

instance Outputable String where
	toOutput = toOutput . T.pack

-- | Displays a value to stdout.
--
-- Uses locking to ensure that the whole output occurs atomically
-- even when other threads are concurrently generating output.
--
-- No newline is appended to the value, so if you want a newline, be sure
-- to include it yourself.
--
-- When something else is writing to the console at the same time, this does
-- not block. It buffers the value, so it will be displayed once the other
-- writer is done.
--
-- When outputConcurrent is used within a call to
-- `System.Console.Regions.displayConsoleRegions`, the output is displayed
-- above the currently open console regions. Only lines ending in a newline
-- are displayed in this case (it uses `waitCompleteLines`).
outputConcurrent :: Outputable v => v -> IO ()
outputConcurrent = outputConcurrent' StdOut

-- | Like `outputConcurrent`, but displays to stderr.
--
-- (Does not throw an exception.)
errorConcurrent :: Outputable v => v -> IO ()
errorConcurrent = outputConcurrent' StdErr

outputConcurrent' :: Outputable v => StdHandle -> v -> IO ()
outputConcurrent' stdh v = do
	-- Use a worker thread. This is so any async exception that
	-- is thrown to the current thread does not affect
	-- tryTakeOutputLock, which is not async exception safe.
	worker <- async $ bracket setup cleanup go
	wait worker
  where
	setup = tryTakeOutputLock
	cleanup False = return ()
	cleanup True = dropOutputLock
	go True = do
		T.hPutStr h (toOutput v)
		hFlush h
	go False = do
		oldbuf <- atomically $ takeTMVar bv
		newbuf <- addOutputBuffer (Output (toOutput v)) oldbuf
		atomically $ putTMVar bv newbuf
	h = toHandle stdh
	bv = bufferFor stdh

-- | This alias is provided to avoid breaking backwards compatibility.
type ConcurrentProcessHandle = P.ProcessHandle

-- | Same as `P.waitForProcess`; provided to avoid breaking backwards
-- compatibility.
waitForProcessConcurrent :: ConcurrentProcessHandle -> IO ExitCode
waitForProcessConcurrent = P.waitForProcess

-- | Wrapper around `System.Process.createProcess` that prevents 
-- multiple processes that are running concurrently from writing
-- to stdout/stderr at the same time.
--
-- If the process does not output to stdout or stderr, it's run
-- by createProcess entirely as usual. Only processes that can generate
-- output are handled specially:
--
-- A process is allowed to write to stdout and stderr in the usual
-- way, assuming it can successfully take the output lock.
--
-- When the output lock is held (ie, by another concurrent process,
-- or because `outputConcurrent` is being called at the same time),
-- the process is instead run with its stdout and stderr
-- redirected to a buffer. The buffered output will be displayed as soon
-- as the output lock becomes free.
--
-- Note that the the process is waited for by a background thread,
-- so unlike createProcess, neglecting to call waitForProcess will not
-- result in zombie processess.
createProcessConcurrent :: P.CreateProcess -> IO (Maybe Handle, Maybe Handle, Maybe Handle, P.ProcessHandle) 
createProcessConcurrent p
	| willOutput (P.std_out p) || willOutput (P.std_err p) =
		ifM tryTakeOutputLock
			( fgProcess p
			, bgProcess p
			)
	| otherwise = do
		r@(_, _, _, h) <- P.createProcess p
		_ <- async $ void $ tryIO $ P.waitForProcess h
		return r

-- | Wrapper around `System.Process.createProcess` that makes sure a process
-- is run in the foreground, with direct access to stdout and stderr.
-- Useful when eg, running an interactive process.
--
-- Note that the the process is waited for by a background thread,
-- so unlike createProcess, neglecting to call waitForProcess will not
-- result in zombie processess.
createProcessForeground :: P.CreateProcess -> IO (Maybe Handle, Maybe Handle, Maybe Handle, P.ProcessHandle)
createProcessForeground p = do
	takeOutputLock
	fgProcess p

fgProcess :: P.CreateProcess -> IO (Maybe Handle, Maybe Handle, Maybe Handle, P.ProcessHandle)
fgProcess p = do
	r@(_, _, _, h) <- P.createProcess p
		`onException` dropOutputLock
	registerOutputThread
	-- Wait for the process to exit and drop the lock.
	_ <- async $ do
		void $ tryIO $ P.waitForProcess h
		unregisterOutputThread
		dropOutputLock
	return r

bgProcess :: P.CreateProcess -> IO (Maybe Handle, Maybe Handle, Maybe Handle, P.ProcessHandle)
bgProcess p = do
	let p' = p
		{ P.std_out = rediroutput (P.std_out p)
		, P.std_err = rediroutput (P.std_err p)
		}
	registerOutputThread
	(stdin_h, stdout_h, stderr_h, h) <- P.createProcess p'
		`onException` unregisterOutputThread
	let r =
		( stdin_h
		, mungeret (P.std_out p) stdout_h
		, mungeret (P.std_err p) stderr_h
		, h
		)
	-- Wait for the process for symmetry with fgProcess,
	-- which does the same.
	_ <- async $ void $ tryIO $ P.waitForProcess h
	outbuf <- setupOutputBuffer StdOut (mungebuf (P.std_out p) stdout_h)
	errbuf <- setupOutputBuffer StdErr (mungebuf (P.std_err p) stderr_h)
	void $ async $ bufferWriter [outbuf, errbuf]
	return r
  where
	rediroutput ss
		| willOutput ss = P.CreatePipe
		| otherwise = ss
	mungebuf ss mh
		| willOutput ss = mh
		| otherwise = Nothing
	mungeret ss mh
		| willOutput ss = Nothing
		| otherwise = mh

willOutput :: P.StdStream -> Bool
willOutput P.Inherit = True
willOutput _ = False

-- | Buffered output.
data OutputBuffer = OutputBuffer [OutputBufferedActivity]
	deriving (Eq)

data StdHandle = StdOut | StdErr

toHandle :: StdHandle -> Handle
toHandle StdOut = stdout
toHandle StdErr = stderr

bufferFor :: StdHandle -> TMVar OutputBuffer
bufferFor StdOut = outputBuffer globalOutputHandle
bufferFor StdErr = errorBuffer globalOutputHandle

data OutputBufferedActivity
	= Output T.Text
	| InTempFile
		{ tempFile :: FilePath
		, endsInNewLine :: Bool
		}
	deriving (Eq)

data AtEnd = AtEnd
	deriving Eq

data BufSig = BufSig

setupOutputBuffer :: StdHandle -> Maybe Handle -> IO (StdHandle, MVar OutputBuffer, TMVar BufSig, TMVar AtEnd)
setupOutputBuffer h fromh = do
	buf <- newMVar (OutputBuffer [])
	bufsig <- atomically newEmptyTMVar
	bufend <- atomically newEmptyTMVar
	void $ async $ outputDrainer fromh buf bufsig bufend
	return (h, buf, bufsig, bufend)

-- Drain output from the handle, and buffer it.
outputDrainer :: Maybe Handle -> MVar OutputBuffer -> TMVar BufSig -> TMVar AtEnd -> IO ()
outputDrainer mfromh buf bufsig bufend = case mfromh of
	Nothing -> atend
	Just fromh -> go fromh
  where
	go fromh = do
		t <- T.hGetChunk fromh
		if T.null t
			then do
				atend
				hClose fromh
			else do
				modifyMVar_ buf $ addOutputBuffer (Output t)
				changed
				go fromh
	atend = atomically $ putTMVar bufend AtEnd
	changed = atomically $ do
		void $ tryTakeTMVar bufsig
		putTMVar bufsig BufSig

registerOutputThread :: IO ()
registerOutputThread = do
	let v = outputThreads globalOutputHandle
	atomically $ putTMVar v . succ =<< takeTMVar v
	
unregisterOutputThread :: IO ()
unregisterOutputThread = do
	let v = outputThreads globalOutputHandle
	atomically $ putTMVar v . pred =<< takeTMVar v

-- Wait to lock output, and once we can, display everything 
-- that's put into the buffers, until the end.
--
-- If end is reached before lock is taken, instead add the command's
-- buffers to the global outputBuffer and errorBuffer.
bufferWriter :: [(StdHandle, MVar OutputBuffer, TMVar BufSig, TMVar AtEnd)] -> IO ()
bufferWriter ts = do
	activitysig <- atomically newEmptyTMVar
	worker1 <- async $ lockOutput $
		ifM (atomically $ tryPutTMVar activitysig ())
			( void $ mapConcurrently displaybuf ts
			, noop -- buffers already moved to global
			)
	worker2 <- async $ void $ globalbuf activitysig worker1
	void $ async $ do
		void $ waitCatch worker1
		void $ waitCatch worker2
		unregisterOutputThread
  where
	displaybuf v@(outh, buf, bufsig, bufend) = do
		change <- atomically $
			(Right <$> takeTMVar bufsig)
				`orElse`
			(Left <$> takeTMVar bufend)
		l <- takeMVar buf
		putMVar buf (OutputBuffer [])
		emitOutputBuffer outh l
		case change of
			Right BufSig -> displaybuf v
			Left AtEnd -> return ()
	globalbuf activitysig worker1 = do
		ok <- atomically $ do
			-- signal we're going to handle it
			-- (returns false if the displaybuf already did)
			ok <- tryPutTMVar activitysig ()
			-- wait for end of all buffers
			when ok $
				mapM_ (\(_outh, _buf, _bufsig, bufend) -> takeTMVar bufend) ts
			return ok
		when ok $ do
			-- add all of the command's buffered output to the
			-- global output buffer, atomically
			bs <- forM ts $ \(outh, buf, _bufsig, _bufend) ->
				(outh,) <$> takeMVar buf
			atomically $
				forM_ bs $ \(outh, b) -> 
					bufferOutputSTM' outh b
			-- worker1 might be blocked waiting for the output
			-- lock, and we've already done its job, so cancel it
			cancel worker1

-- Adds a value to the OutputBuffer. When adding Output to a Handle,
-- it's cheaper to combine it with any already buffered Output to that
-- same Handle.
--
-- When the total buffered Output exceeds 1 mb in size, it's moved out of
-- memory, to a temp file. This should only happen rarely, but is done to
-- avoid some verbose process unexpectedly causing excessive memory use.
addOutputBuffer :: OutputBufferedActivity -> OutputBuffer -> IO OutputBuffer
addOutputBuffer (Output t) (OutputBuffer buf)
	| T.length t' <= 1048576 = return $ OutputBuffer (Output t' : other)
	| otherwise = do
		tmpdir <- getTemporaryDirectory
		(tmp, h) <- openTempFile tmpdir "output.tmp"
		let !endnl = endsNewLine t'
		let i = InTempFile
			{ tempFile = tmp
			, endsInNewLine = endnl
			}
		T.hPutStr h t'
		hClose h
		return $ OutputBuffer (i : other)
  where
	!t' = T.concat (mapMaybe getOutput this) <> t
	!(this, other) = partition isOutput buf
	isOutput v = case v of
		Output _ -> True
		_ -> False
	getOutput v = case v of
		Output t'' -> Just t''
		_ -> Nothing
addOutputBuffer v (OutputBuffer buf) = return $ OutputBuffer (v:buf)

-- | Adds a value to the output buffer for later display.
--
-- Note that buffering large quantities of data this way will keep it
-- resident in memory until it can be displayed. While `outputConcurrent`
-- uses temp files if the buffer gets too big, this STM function cannot do
-- so.
bufferOutputSTM :: Outputable v => StdHandle -> v -> STM ()
bufferOutputSTM h v = bufferOutputSTM' h (OutputBuffer [Output (toOutput v)])

bufferOutputSTM' :: StdHandle -> OutputBuffer -> STM ()
bufferOutputSTM' h (OutputBuffer newbuf) = do
	(OutputBuffer buf) <- takeTMVar bv
	putTMVar bv (OutputBuffer (newbuf ++ buf))
  where
	bv = bufferFor h

-- | A STM action that waits for some buffered output to become
-- available, and returns it.
--
-- The function can select a subset of output when only some is desired;
-- the fst part is returned and the snd is left in the buffer.
--
-- This will prevent it from being displayed in the usual way, so you'll
-- need to use `emitOutputBuffer` to display it yourself.
outputBufferWaiterSTM :: (OutputBuffer -> (OutputBuffer, OutputBuffer)) -> STM (StdHandle, OutputBuffer)
outputBufferWaiterSTM selector = waitgetbuf StdOut `orElse` waitgetbuf StdErr
  where
	waitgetbuf h = do
		let bv = bufferFor h
		(selected, rest) <- selector <$> takeTMVar bv
		when (selected == OutputBuffer [])
			retry
		putTMVar bv rest
		return (h, selected)

waitAnyBuffer :: OutputBuffer -> (OutputBuffer, OutputBuffer)
waitAnyBuffer b = (b, OutputBuffer [])

-- | Use with `outputBufferWaiterSTM` to make it only return buffered
-- output that ends with a newline. Anything buffered without a newline
-- is left in the buffer.
waitCompleteLines :: OutputBuffer -> (OutputBuffer, OutputBuffer)
waitCompleteLines (OutputBuffer l) = 
	let (selected, rest) = span completeline l
	in (OutputBuffer selected, OutputBuffer rest)
  where
	completeline (v@(InTempFile {})) = endsInNewLine v
	completeline (Output b) = endsNewLine b

endsNewLine :: T.Text -> Bool
endsNewLine t = not (T.null t) && T.last t == '\n'

-- | Emits the content of the OutputBuffer to the Handle
--
-- If you use this, you should use `lockOutput` to ensure you're the only
-- thread writing to the console.
emitOutputBuffer :: StdHandle -> OutputBuffer -> IO ()
emitOutputBuffer stdh (OutputBuffer l) = 
	forM_ (reverse l) $ \ba -> case ba of
		Output t -> emit t
		InTempFile tmp _ -> do
			emit =<< T.readFile tmp
			void $ tryWhenExists $ removeFile tmp
  where
	outh = toHandle stdh
	emit t = void $ tryIO $ do
		T.hPutStr outh t
		hFlush outh
