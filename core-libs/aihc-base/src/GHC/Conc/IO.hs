{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.Conc.IO (ensureIOManagerIsRunning, registerDelay) where

import GHC.Conc.Sync (TVar (..))
import GHC.IO (IO (..))
import GHC.Prim (newDelayTVar#)
import GHC.Types (Bool (..), Int (..))
import Prelude (return)

registerDelay :: Int -> IO (TVar Bool)
registerDelay (I# delay) =
  IO
    ( \state ->
        case newDelayTVar# delay False True state of
          (# next, variable #) -> (# next, TVar variable #)
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
