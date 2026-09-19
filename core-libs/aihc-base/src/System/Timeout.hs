-- | A limit on the time that an action can take.
--
-- GHC stops a slow action with an asynchronous exception. aihc has no
-- asynchronous exceptions yet, thus it cannot stop an action that runs.
-- Therefore 'timeout' obeys only the two limit conditions that need no
-- interruption. A negative limit means no limit. A zero limit lets no action
-- start. For a positive limit, 'timeout' runs the action to its end and gives
-- its result. Code that uses 'timeout' only as a safety limit is correct. Code
-- that depends on the interruption of a slow action is not correct.
module System.Timeout
  ( Timeout,
    timeout,
  )
where

import Control.Exception (Exception)
import GHC.IO (IO)
import Prelude (Eq (..), Int, Maybe (..), Ord (..), Show (..), fmap, otherwise, pure, showString)

-- | The exception that GHC throws into an action that is too slow.
--
-- aihc keeps the type, because other packages name it. aihc never throws it.
data Timeout = Timeout

instance Exception Timeout

instance Show Timeout where
  showsPrec _ Timeout = showString "<<timeout>>"

-- | Run an action under a time limit that is given in microseconds.
--
-- A negative limit means no limit. A zero limit gives 'Nothing' and does not
-- run the action. For a positive limit, see the limits of this module.
timeout :: Int -> IO a -> IO (Maybe a)
timeout microseconds action
  | microseconds < 0 = fmap Just action
  | microseconds == 0 = pure Nothing
  | otherwise = fmap Just action
