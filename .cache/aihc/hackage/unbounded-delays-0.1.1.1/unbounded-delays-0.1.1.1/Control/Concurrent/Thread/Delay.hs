{-# LANGUAGE CPP, NoImplicitPrelude #-}

#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Safe #-}
#endif

-- | Arbitrarily long thread delays.
module Control.Concurrent.Thread.Delay ( delay ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Control.Concurrent ( threadDelay )
import Control.Monad      ( when, return )
import Data.Eq            ( (/=) )
import Data.Function      ( ($) )
import Data.Int           ( Int )
import Data.Ord           ( min, (<=) )
import Prelude            ( Integer, toInteger, fromInteger, maxBound, (-) )
import System.IO          ( IO )

#if __GLASGOW_HASKELL__ < 700
import Control.Monad      ( (>>) )
#endif


-------------------------------------------------------------------------------
-- Delay
-------------------------------------------------------------------------------

{-|
Like @Control.Concurrent.'threadDelay'@, but not bounded by an 'Int'.

Suspends the current thread for a given number of microseconds (GHC only).

There is no guarantee that the thread will be rescheduled promptly when the
delay has expired, but the thread will never continue to run earlier than
specified.
-}
delay :: Integer -> IO ()
delay time | time <= 0 =
  -- When time is a big negative integer, casting it to Int may overflow.
  -- So we handle it as a special case here.
  return ()
delay time = do
  let maxWait = min time $ toInteger (maxBound :: Int)
  threadDelay $ fromInteger maxWait
  when (maxWait /= time) $ delay (time - maxWait)
