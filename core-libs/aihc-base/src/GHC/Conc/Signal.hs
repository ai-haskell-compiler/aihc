-- | POSIX signal handlers.
--
-- The handler table below mirrors the one in GHC's @GHC.Internal.Conc.Signal@:
-- 'setHandler' records a handler for a signal number and 'runHandlers' looks
-- one up and runs it. In GHC the RTS is what calls 'runHandlers', from the
-- signal handler it installs on the C side; aihc's runtime has no such
-- callback yet, so nothing ever invokes 'runHandlers' on its own. Until it
-- does, installing a handler through @System.Posix.Signals@ records it here
-- and it simply never fires.
module GHC.Conc.Signal
  ( Signal,
    HandlerFun,
    setHandler,
    runHandlers,
    runHandlersPtr,
  )
where

import Control.Monad (void, when)
import Data.Dynamic (Dynamic)
import Foreign.C.Types (CInt)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr)
import Foreign.Marshal.Alloc (finalizerFree)
import GHC.Conc.Sync (forkIO)
import GHC.IOArray (IOArray, newIOArray, unsafeReadIOArray, unsafeWriteIOArray)
import GHC.Ptr (Ptr)
import GHC.Word (Word8)
import System.IO.Unsafe (unsafePerformIO)
import Prelude (Bool, IO, Int, Maybe (..), Ord (..), errorWithoutStackTrace, fromIntegral, return, ($), (&&))

type Signal = CInt

-- | The highest signal number the handler table has room for.
maxSig :: Int
maxSig = 64

-- | A handler is passed the @siginfo_t@ the signal was delivered with.
type HandlerFun = ForeignPtr Word8 -> IO ()

-- | The installed handler for every signal number in @[0, 'maxSig']@.
--
-- GHC guards this array with an 'Control.Concurrent.MVar.MVar' and shares it
-- across copies of @base@ via the RTS. Neither is needed here: aihc has no
-- signal delivery, so there is no handler to race with, and there is only
-- ever one copy of this module.
signalHandlers :: IOArray Int (Maybe (HandlerFun, Dynamic))
signalHandlers = unsafePerformIO (newIOArray (0, maxSig) Nothing)
{-# NOINLINE signalHandlers #-}

-- | Whether a signal number has a slot in 'signalHandlers'.
inTable :: Int -> Bool
inTable index = index >= 0 && index <= maxSig

-- | Install @handler@ for @sig@, returning the handler it replaced.
setHandler :: Signal -> Maybe (HandlerFun, Dynamic) -> IO (Maybe (HandlerFun, Dynamic))
setHandler sig handler =
  let index = fromIntegral sig
   in if inTable index
        then do
          old <- unsafeReadIOArray signalHandlers index
          unsafeWriteIOArray signalHandlers index handler
          return old
        else errorWithoutStackTrace "GHC.Conc.setHandler: signal out of range"

-- | Run the handler installed for @sig@, if any, in a new thread.
--
-- Nothing in aihc's runtime calls this yet; see the module header.
runHandlers :: ForeignPtr Word8 -> Signal -> IO ()
runHandlers info sig =
  let index = fromIntegral sig
   in when (inTable index) $ do
        handler <- unsafeReadIOArray signalHandlers index
        case handler of
          Nothing -> return ()
          Just (run, _) -> void (forkIO (run info))

-- | 'runHandlers' for a @siginfo_t@ buffer we are handed ownership of.
--
-- The finalizer that frees the buffer is 'finalizerFree', which aihc does not
-- have yet, so attaching it throws. Like 'runHandlers', nothing calls this
-- until the runtime delivers signals.
runHandlersPtr :: Ptr Word8 -> Signal -> IO ()
runHandlersPtr info sig = do
  managed <- newForeignPtr finalizerFree info
  runHandlers managed sig
