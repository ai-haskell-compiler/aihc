{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnboxedTuples #-}

module Control.Concurrent
  ( module Control.Concurrent.MVar,
    module Control.Concurrent.Chan,
    module Control.Concurrent.QSem,
    module Control.Concurrent.QSemN,
    ThreadId,
    forkFinally,
    forkIO,
    forkIOWithUnmask,
    forkOn,
    forkOnWithUnmask,
    forkOS,
    getNumCapabilities,
    setNumCapabilities,
    threadCapability,
    killThread,
    myThreadId,
    threadDelay,
    threadWaitRead,
    threadWaitWrite,
    throwTo,
    yield,
    rtsSupportsBoundThreads,
    isCurrentThreadBound,
    runInBoundThread,
    runInUnboundThread,
    mkWeakThreadId,
  )
where

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent.QSem
import Control.Concurrent.QSemN
import Control.Exception.Base (SomeException, mask, try)
import GHC.Conc.IO (threadDelay)
import GHC.Conc.Sync (ThreadId (..), forkIO, getNumCapabilities, killThread, myThreadId, throwTo, yield)
import GHC.Exception (ErrorCall (..))
import GHC.IO (IO (..), throwIO, unsafeUnmask)
import GHC.Prim (mkWeakNoFinalizer#)
import GHC.Weak (Weak (..))
import System.Posix.Types (Fd)
import Prelude (Bool (..), Either, Int, fail, return, ($), (>>=))

-- | Run the action in a child thread. Give its result to the callback.
forkFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkFinally action callback =
  mask $ \restore -> forkIO (try (restore action) >>= callback)

-- | Run the action in a child thread. The action gets a function that
-- unmasks asynchronous exceptions.
forkIOWithUnmask :: ((forall a. IO a -> IO a) -> IO ()) -> IO ThreadId
forkIOWithUnmask action = forkIO (action unsafeUnmask)

-- | Run the action in a child thread on the given capability. The runtime
-- has one capability, so this function ignores the number.
forkOn :: Int -> IO () -> IO ThreadId
forkOn _ = forkIO

-- | Run the action in a child thread on the given capability. See 'forkOn'
-- and 'forkIOWithUnmask'.
forkOnWithUnmask :: Int -> ((forall a. IO a -> IO a) -> IO ()) -> IO ThreadId
forkOnWithUnmask _ = forkIOWithUnmask

-- | Bound threads are not supported. The stub does not run the action.
forkOS :: IO () -> IO ThreadId
forkOS _ = throwIO (ErrorCallWithLocation "forkOS: bound threads are not supported" "")

-- | Set the number of capabilities. The runtime has one capability, and it
-- cannot add more. Thus this function does nothing, as in the
-- non-threaded GHC runtime.
setNumCapabilities :: Int -> IO ()
setNumCapabilities _ = return ()

-- | Give the capability of a thread, and whether the thread stays on that
-- capability. The runtime has only capability 0.
threadCapability :: ThreadId -> IO (Int, Bool)
threadCapability _ = return (0, False)

-- | Wait until a file descriptor has data to read. The runtime has no IO
-- manager, so this function returns immediately. The next read waits for
-- the data and stops all green threads during the wait.
threadWaitRead :: Fd -> IO ()
threadWaitRead _ = return ()

-- | Wait until a file descriptor can accept data. See 'threadWaitRead'.
threadWaitWrite :: Fd -> IO ()
threadWaitWrite _ = return ()

-- | The runtime does not support bound threads.
rtsSupportsBoundThreads :: Bool
rtsSupportsBoundThreads = False

-- | No thread is bound, because the runtime does not support bound threads.
isCurrentThreadBound :: IO Bool
isCurrentThreadBound = return False

-- | Run the action in a bound thread. The runtime does not support bound
-- threads, so this function fails as in the non-threaded GHC runtime.
runInBoundThread :: IO a -> IO a
runInBoundThread _ =
  fail "RTS doesn't support multiple OS threads (use ghc -threaded when linking)"

-- | Run the action in an unbound thread. No thread is bound, so this
-- function runs the action in the current thread.
runInUnboundThread :: IO a -> IO a
runInUnboundThread action = action

-- | Make a weak pointer to a thread.
mkWeakThreadId :: ThreadId -> IO (Weak ThreadId)
mkWeakThreadId thread@(ThreadId rawThread) =
  IO
    ( \state ->
        case mkWeakNoFinalizer# rawThread thread state of
          (# nextState, weak #) -> (# nextState, Weak weak #)
    )
