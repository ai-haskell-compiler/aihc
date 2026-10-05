{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.Conc.IO
  ( ensureIOManagerIsRunning,
    registerDelay,
    threadDelay,
    threadWaitRead,
    threadWaitWrite,
    threadWaitReadSTM,
    threadWaitWriteSTM,
    closeFdWith,
  )
where

import Data.Bits ((.&.))
import Foreign.C.Error (eBADF, eINTR, errnoToIOError, getErrno, throwErrno)
import Foreign.C.Types (CInt (..), CShort (..), CUInt (..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekByteOff, pokeByteOff)
import GHC.Conc.Sync (STM, TVar (..), atomically, readTVar, retry, unsafeIOToSTM, yield)
import GHC.IO (IO (..))
import GHC.Internal.IO.Types (ioError)
import GHC.Prim (newDelayTVar#)
import GHC.Types (Bool (..), Int (..))
import System.Posix.Types (Fd (..))
import Prelude (Eq (..), Maybe (..), Ord (..), Ordering (..), String, otherwise, return, (>>), (>>=))

registerDelay :: Int -> IO (TVar Bool)
registerDelay (I# delay) =
  IO
    ( \state ->
        case newDelayTVar# delay False True state of
          (# next, variable #) -> (# next, TVar variable #)
    )

-- | Suspend the current green thread for at least the given number of
-- microseconds.
--
-- GHC has a @delay#@ primitive for this. aihc builds the wait out of the
-- runtime's transaction timer instead: the thread blocks in @atomically@
-- until the timer fires, and the scheduler runs every other runnable thread
-- meanwhile. A delay of zero or less only yields, as it does in GHC.
threadDelay :: Int -> IO ()
threadDelay microseconds
  | microseconds <= 0 = yield
  | otherwise = do
      expired <- registerDelay microseconds
      atomically
        ( do
            fired <- readTVar expired
            case fired of
              True -> return ()
              False -> retry
        )

-- | Start the IO manager if it is not running already.
--
-- aihc has no IO manager: timers and file descriptor waits are not multiplexed
-- through one, so there is nothing to start. Callers such as
-- @System.Posix.Signals.installHandler@ invoke this before installing a
-- handler, so it has to exist and succeed; doing nothing is the honest
-- implementation until an IO manager does.
ensureIOManagerIsRunning :: IO ()
ensureIOManagerIsRunning = return ()

-- | Block the current green thread until the descriptor is ready for
-- reading.
--
-- GHC asks its IO manager to wake the thread. aihc has no IO manager, so
-- the thread asks @poll(2)@ without a timeout and sleeps with 'threadDelay'
-- between the questions. The other green threads run meanwhile. A closed
-- descriptor raises an @EBADF@ error, as in GHC.
threadWaitRead :: Fd -> IO ()
threadWaitRead = waitForDescriptor "threadWaitRead" pollIn

-- | Block the current green thread until the descriptor is ready for
-- writing. See 'threadWaitRead'.
threadWaitWrite :: Fd -> IO ()
threadWaitWrite = waitForDescriptor "threadWaitWrite" pollOut

-- | Get a transaction that waits until the descriptor is ready for reading,
-- and an action that stops the wait.
--
-- The transaction asks @poll(2)@ itself. When the descriptor is not ready,
-- it starts a 'registerDelay' timer and retries. The runtime runs the
-- transaction again when a timer expires. The transaction needs no helper
-- thread, so the action that stops the wait has nothing to do.
threadWaitReadSTM :: Fd -> IO (STM (), IO ())
threadWaitReadSTM descriptor =
  return (waitForDescriptorSTM "threadWaitReadSTM" pollIn descriptor, return ())

-- | Get a transaction that waits until the descriptor is ready for writing,
-- and an action that stops the wait. See 'threadWaitReadSTM'.
threadWaitWriteSTM :: Fd -> IO (STM (), IO ())
threadWaitWriteSTM descriptor =
  return (waitForDescriptorSTM "threadWaitWriteSTM" pollOut descriptor, return ())

-- | Close a descriptor with the given action.
--
-- GHC first removes the descriptor from its IO manager. aihc has no IO
-- manager, so the action runs directly. A thread that waits on the
-- descriptor sees @POLLNVAL@ at its next @poll(2)@ and gets an error.
closeFdWith :: (Fd -> IO ()) -> Fd -> IO ()
closeFdWith close = close

-- | The time in microseconds between two questions to @poll(2)@.
pollInterval :: Int
pollInterval = 1000

-- | The size of @struct pollfd@. POSIX gives the fields as an @int@ and two
-- @short@ values in this sequence, and each supported platform lays them out
-- without padding.
pollEntrySize :: Int
pollEntrySize = 8

waitForDescriptor :: String -> CShort -> Fd -> IO ()
waitForDescriptor location events descriptor = wait
  where
    wait = do
      ready <- descriptorReady location events descriptor
      case ready of
        True -> return ()
        False -> threadDelay pollInterval >> wait

waitForDescriptorSTM :: String -> CShort -> Fd -> STM ()
waitForDescriptorSTM location events descriptor = do
  ready <- unsafeIOToSTM (descriptorReady location events descriptor)
  case ready of
    True -> return ()
    False -> unsafeIOToSTM (registerDelay pollInterval) >> retry

-- | Ask @poll(2)@ without a timeout whether the descriptor has one of the
-- events. An error or a hang-up also makes the descriptor ready, as in GHC.
descriptorReady :: String -> CShort -> Fd -> IO Bool
descriptorReady location events (Fd descriptor) =
  allocaBytes pollEntrySize ask
  where
    ask :: Ptr () -> IO Bool
    ask entry = do
      pokeByteOff entry 0 descriptor
      pokeByteOff entry 4 events
      pokeByteOff entry 6 (0 :: CShort)
      ready <- c_poll entry 1 0
      case compare ready 0 of
        LT -> do
          errno <- getErrno
          case errno == eINTR of
            True -> ask entry
            False -> throwErrno location
        EQ -> return False
        GT -> do
          returned <- peekByteOff entry 6 :: IO CShort
          case returned .&. pollNval == 0 of
            True -> return True
            False -> ioError (errnoToIOError location eBADF Nothing Nothing)

foreign import capi unsafe "poll.h poll"
  c_poll :: Ptr () -> CUInt -> CInt -> IO CInt

foreign import capi unsafe "poll.h value POLLIN" pollIn :: CShort

foreign import capi unsafe "poll.h value POLLOUT" pollOut :: CShort

foreign import capi unsafe "poll.h value POLLNVAL" pollNval :: CShort
