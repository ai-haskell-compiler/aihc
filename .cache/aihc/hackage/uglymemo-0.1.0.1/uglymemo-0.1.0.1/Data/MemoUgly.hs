module Data.MemoUgly(memoIO, memo) where
import Control.Concurrent.MVar
import qualified Data.Map as M
import System.IO.Unsafe(unsafePerformIO)

-- | Memoize the given function by allocating a memo table,
-- and then updating the memo table on each function call.
memoIO :: (Ord a)
       => (a -> b)           -- ^Function to memoize
       -> IO (a -> IO b)
memoIO f = do
    v <- newMVar M.empty
    let f' x = do
            m <- readMVar v
            case M.lookup x m of
                Nothing -> do let { r = f x }; modifyMVar_ v (return . M.insert x r); return r
                Just r  -> return r
    return f'

-- | The pure version of 'memoIO'.
memo :: (Ord a)
     => (a -> b)           -- ^Function to memoize
     -> (a -> b)
memo f = let f' = unsafePerformIO (memoIO f) in \ x -> unsafePerformIO (f' x)
