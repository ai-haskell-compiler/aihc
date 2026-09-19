-- | Attaching a deadline to an 'IO' action.
--
-- GHC implements 'timeout' by forking a watchdog thread that delivers a
-- 'Timeout' exception to the waiting thread once the deadline passes. aihc
-- does not have asynchronous exceptions yet: there is no @throwTo@ and no
-- interruptible wait, so an action that has already begun cannot be stopped.
-- See @docs/exceptions.md@ for the design that will make it possible.
--
-- Rather than run an action that no deadline can bound, a positive timeout
-- raises an unsupported-operation 'IOError'. The two cases that need no
-- watchdog behave as they do in GHC: a negative timeout runs the action to
-- completion, and a zero timeout runs nothing.
module System.Timeout
  ( Timeout,
    timeout,
  )
where

import Control.Exception (Exception)
import GHC.Internal.IO.Types (ioe_unsupportedOperation)
import Prelude

-- | The exception GHC delivers to an action that has run out of time. aihc
-- never throws it, because it cannot interrupt the action it would belong to.
data Timeout = Timeout

instance Eq Timeout where
  Timeout == Timeout = True

instance Ord Timeout where
  compare Timeout Timeout = EQ

instance Show Timeout where
  show Timeout = "<<timeout>>"

instance Exception Timeout

-- | Run an action, giving up after the given number of microseconds.
--
-- The result is 'Nothing' when the action did not finish in time, and
-- @'Just' value@ when it did.
timeout :: Int -> IO a -> IO (Maybe a)
timeout microseconds action
  | microseconds < 0 = Just <$> action
  | microseconds == 0 = return Nothing
  | otherwise = ioe_unsupportedOperation
