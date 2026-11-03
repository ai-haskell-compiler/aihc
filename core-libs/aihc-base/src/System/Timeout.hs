-- | A limit on the time that an action can take.
--
-- As in GHC, 'timeout' forks a watchdog thread. When the limit passes, the
-- watchdog raises 'Timeout' in the thread that runs the action. The action
-- must reach a safepoint to take the exception: a heap check, an
-- evaluation, or a wait. A loop that does not allocate does not stop.
module System.Timeout
  ( Timeout,
    timeout,
  )
where

import Control.Concurrent (forkIOWithUnmask, killThread, myThreadId, threadDelay, throwTo)
import Control.Exception (Exception, bracket, handleJust, uninterruptibleMask_)
import Data.Unique (Unique, newUnique)
import GHC.IO (IO)
import Prelude (Eq (..), Int, Maybe (..), Ord (..), Show (..), fmap, otherwise, pure, showString, (>>), ($), (.))

-- | The exception that the watchdog of 'timeout' raises. Each call of
-- 'timeout' has its own value, so nested calls do not catch the exception
-- of another call.
newtype Timeout = Timeout Unique
  deriving (Eq)

instance Exception Timeout

instance Show Timeout where
  showsPrec _ _ = showString "<<timeout>>"

-- | Run an action under a time limit that is given in microseconds. The
-- result is 'Nothing' when the limit passes before the action ends.
--
-- A negative limit means no limit. A zero limit gives 'Nothing' and does not
-- run the action.
timeout :: Int -> IO a -> IO (Maybe a)
timeout microseconds action
  | microseconds < 0 = fmap Just action
  | microseconds == 0 = pure Nothing
  | otherwise = do
      current <- myThreadId
      exception <- fmap Timeout newUnique
      handleJust
        (\raised -> if raised == exception then Just () else Nothing)
        (\_ -> pure Nothing)
        ( bracket
            (forkIOWithUnmask $ \unmask -> unmask $ threadDelay microseconds >> throwTo current exception)
            (uninterruptibleMask_ . killThread)
            (\_ -> fmap Just action)
        )
