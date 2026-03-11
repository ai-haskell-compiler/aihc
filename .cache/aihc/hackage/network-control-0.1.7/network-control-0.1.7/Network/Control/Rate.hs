module Network.Control.Rate (
    -- * Rate control
    Rate,
    newRate,
    getRate,
    addRate,
) where

import Data.IORef
import Data.UnixTime

-- | Type for rating.
newtype Rate = Rate (IORef Counter)

data Counter = Counter Int UnixTime

-- | Creating a new 'Rate'.
newRate :: IO Rate
newRate = do
    cntr <- Counter 0 <$> getUnixTime
    Rate <$> newIORef cntr

-- | Getting the current rate.
-- If one or more seconds have passed since the previous call, the
-- counter is re-initialized with 1 and it is returned.  Otherwise,
-- incremented counter number is returned.
getRate :: Rate -> IO Int
getRate r = addRate r 1

-- | Getting the current rate.
-- If one or more seconds have passed since the previous call, the
-- counter is re-initialized with the second argument and it is
-- returned.  Otherwise, increased counter number is returned.
addRate :: Rate -> Int -> IO Int
addRate (Rate ref) x = do
    Counter n beg <- readIORef ref
    cur <- getUnixTime
    if (cur `diffUnixTime` beg) > 1
        then do
            let n' = x
            writeIORef ref $ Counter n' cur
            return n'
        else do
            let n' = n + x
            writeIORef ref $ Counter n' beg
            return n'
