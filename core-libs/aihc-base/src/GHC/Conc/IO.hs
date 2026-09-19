{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.Conc.IO (ensureIOManagerIsRunning, registerDelay, threadDelay) where

import GHC.Conc.Sync (TVar (..), atomically, readTVar, retry, yield)
import GHC.IO (IO (..))
import GHC.Prim (newDelayTVar#)
import GHC.Types (Bool (..), Int (..))
import Prelude (Ord (..), otherwise, return)

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
