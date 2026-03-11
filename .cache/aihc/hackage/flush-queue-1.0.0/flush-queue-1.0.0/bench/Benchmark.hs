{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Concurrent.Async
import           Control.Concurrent.BFQueue
import           Control.Concurrent.MVar
import           Control.Concurrent.STM          (STM, atomically, check,
                                                  orElse)
import           Control.Concurrent.STM.TBFQueue
import           Control.Concurrent.STM.TBQueue
import           Control.DeepSeq
import           Control.Monad
import           Data.Foldable
import           Data.IORef
import           System.CPUTime
import           System.IO                       (BufferMode (LineBuffering),
                                                  hSetBuffering, stdout)
import           System.Time


writeQueueUsing :: (Int -> IO ()) -> Int -> IO ()
writeQueueUsing f = go
  where go n | n > 0 = f n >> go (n-1)
             | otherwise = return ()

fillFlushQueue :: Int -- ^ Queue bound
               -> Int -- ^ Number of threads filling the queue
               -> (Int -> IO ()) -- ^ Queue writer
               -> IO [Int] -- ^ Queue flusher
               -> IO (Time, Time)
fillFlushQueue bound n write flush = do
  ((), fillTime) <- time $ replicateConcurrently_ n (writeQueueUsing write x)
  ((), flushTime) <- time $ do
    res <- flush
    res `deepseq` return ()
  return (fillTime, flushTime)
  where
    x = bound `div` n


runBench :: [Char] -> IO (Time, Time) -> IO ()
runBench name runCycle = do
  let cycles = 30 :: Int
  putStrLn $ replicate 80 '-'
  putStrLn $ name ++ " (cycles " ++ show cycles ++ ")"
  (tFill, tFlush) <- unzip <$> mapM (const runCycle) [1..cycles]
  putStrLn "Average Fill:"
  putStrLn $ prettyTime $ avg tFill
  putStrLn "Average Flush:"
  putStrLn $ prettyTime $ avg tFlush


-- | A rundown of a benchmark:
-- * Fill out the queue (`bound` is the limit) without making it block (1. benchmark)
-- * Writing to the queue is done concurrently by number of `threads`
-- * Flush the queue (2. benchmark)
main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  let bound = 100000
      threads = 16
      runFlushBFQueueMVar = do
        q <- newBFQueueMVar bound
        fillFlushQueue bound threads (void . writeBFQueueMVar q) (flushBFQueueMVar q)
      runFlushSQueue = do
        q <- newSQueue bound
        fillFlushQueue bound threads (writeSQueue q) (flushSQueue q)
      runFlushTBQueue = do
        q <- newTBQueueIO $ fromIntegral bound
        fillFlushQueue bound threads (atomically . writeTBQueue q) (atomically $ flushTBQueue q)
      runFlushTBFQueue = do
        q <- atomically $ newTBFQueue $ fromIntegral bound
        fillFlushQueue bound threads (atomically . writeTBFQueue q) (atomically $ flushTBFQueue q)
      runFlushBFQueue = do
        q <- newBFQueue $ fromIntegral bound
        fillFlushQueue bound threads (writeBFQueue q) (flushBFQueue q)
  putStrLn "==== Fill and Flush all ===="
  runBench "BFQueueMVar (MVar + no blocking)" runFlushBFQueueMVar
  runBench "SQueue (IORef + MVar for blocking)" runFlushSQueue
  runBench "STM TBQueue" runFlushTBQueue
  runBench "STM TBFQueue" runFlushTBFQueue
  runBench "BFQueue (IORef + MVar)" runFlushBFQueue
  let runTakeTBQueue = do
        q <- newTBQueueIO $ fromIntegral bound
        fillFlushQueue
          bound
          threads
          (atomically . void . tryWriteTBQueue q)
          (atomically $ takeTBQueue (bound `div` 2) q)
      runTakeTBFQueue = do
        q <- atomically $ newTBFQueue $ fromIntegral bound
        fillFlushQueue
          bound
          threads
          (atomically . void . tryWriteTBFQueue q)
          (atomically $ takeTBFQueue (fromIntegral bound `div` 2) q)
      runTakeBFQueue = do
        q <- newBFQueue $ fromIntegral bound
        fillFlushQueue
          bound
          threads
          (void . tryWriteBFQueue q)
          (takeBFQueue (fromIntegral bound `div` 2) q)
  putStrLn "==== Try Fill and Take half ===="
  runBench "STM TBQueue" runTakeTBQueue
  runBench "STM TBFQueue" runTakeTBFQueue
  runBench "BFQueue (IORef + MVar)" runTakeBFQueue


-----------------------------------
-- Missing STM TBQueue functions --
-----------------------------------

tryWriteTBQueue :: TBQueue a -> a -> STM Bool
tryWriteTBQueue tbQueue x =
  orElse (writeTBQueue tbQueue x >> return True) (isFullTBQueue tbQueue >>= check >> return False)


takeTBQueue :: (Ord a1, Num a1) => a1 -> TBQueue a2 -> STM [a2]
takeTBQueue i tbQueue
  | i <= 0 = return []
  | otherwise = do
    let tryReadN n acc =
          tryReadTBQueue tbQueue >>= \case
            Just v
              | n < i -> tryReadN (n + 1) (v : acc)
            Just v -> return $ reverse (v : acc)
            _ -> return $ reverse acc
    tryReadN 1 []

---------------------------------
-- Alternative implementations --
---------------------------------

type SQueue' a = IORef (SQueue a)

-- | Simple Queue
data SQueue a = SQueue
  { sqCount    :: !Int
  , sqStack    :: ![a]
  , sqMaxCount :: !Int
  , sqLock     :: !(MVar ())
  }

newSQueue :: Int -> IO (SQueue' a)
newSQueue bound = newEmptyMVar >>= newIORef . SQueue 0 [] bound

writeSQueue :: SQueue' a -> a -> IO ()
writeSQueue queue x = inner
  where
    inner = join $ atomicModifyIORef' queue $ \foo0@(SQueue cnt list bound baton) ->
      if cnt < bound
        then (SQueue (cnt + 1) (x:list) bound baton, pure ())
        else (foo0, readMVar baton >> inner)

flushSQueue :: SQueue' a -> IO [a]
flushSQueue queue = do
  newBaton <- newEmptyMVar
  join $ atomicModifyIORef' queue $ \(SQueue _ list bound oldBaton) ->
    (SQueue 0 [] bound newBaton, reverse list <$ putMVar oldBaton ())



-- | Bounded Flush queue based on MVar, that does not support blocking
newtype BFQueueMVar a = BFQueueMVar (MVar (BList a))

-- | Simple Queue
data BList a = BList
  { bqCount    :: !Int
  , bqStack    :: ![a]
  , bqMaxCount :: !Int
  }

newBFQueueMVar :: Int -> IO (BFQueueMVar a)
newBFQueueMVar bound = BFQueueMVar <$> newMVar (BList 0 [] bound)

writeBFQueueMVar :: BFQueueMVar a -> a -> IO Bool
writeBFQueueMVar (BFQueueMVar bListMVar) x =
  modifyMVar bListMVar $ \ blist@(BList cnt list bound) ->
      if cnt < bound
        then return (BList (cnt + 1) (x:list) bound, True)
        else return (blist, False)

flushBFQueueMVar :: BFQueueMVar a -> IO [a]
flushBFQueueMVar (BFQueueMVar bListMVar) = do
  modifyMVar bListMVar $ \ (BList _ list bound) ->
    return (BList 0 [] bound, reverse list)




--------------------
-- Time functions --
--------------------
-- Temporarely borrowed from:
-- https://github.com/haskell-repa/repa/blob/master/repa-io/Data/Array/Repa/IO/Timing.hs

-- Time -----------------------------------------------------------------------
-- | Abstract representation of process time.
data Time
        = Time
        { cpu_time  :: Integer
        , wall_time :: Integer
        }

zipT :: (Integer -> Integer -> Integer) -> Time -> Time -> Time
zipT f (Time cpu1 wall1) (Time cpu2 wall2)
        = Time (f cpu1 cpu2) (f wall1 wall2)

-- | Subtract second time from the first.
minus :: Time -> Time -> Time
minus = zipT (-)


-- | Add two times.
plus :: Time -> Time -> Time
plus  = zipT (+)

avg :: [Time] -> Time
avg ts = zipT div (foldl' plus (Time 0 0) ts) (Time len len)
  where len = fromIntegral $ length ts

-- TimeUnit -------------------------------------------------------------------
-- | Conversion
type TimeUnit = Integer -> Integer

microseconds :: TimeUnit
microseconds n = n `div` 1000000

milliseconds :: TimeUnit
milliseconds n = n `div` 1000000000

cpuTime :: TimeUnit -> Time -> Integer
cpuTime f = f . cpu_time

wallTime :: TimeUnit -> Time -> Integer
wallTime f = f . wall_time


-- | Get the current time.
getTime :: IO Time
getTime =
  do
    cpu          <- getCPUTime
    TOD sec pico <- getClockTime
    return $ Time cpu (pico + sec * 1000000000000)


-- | Pretty print the times, in milliseconds.
prettyTime :: Time -> String
prettyTime t
        = "elapsedTimeMS   = " ++ (show $ wallTime milliseconds t) ++
          "\ncpuTimeMS       = " ++ (show $ cpuTime  milliseconds t)

-- Timing benchmarks ----------------------------------------------------------

-- | Time some IO action.
--   Make sure to deepseq the result before returning it from the action. If you
--   don't do this then there's a good chance that you'll just pass a suspension
--   out of the action, and the computation time will be zero.
time :: IO a -> IO (a, Time)
{-# NOINLINE time #-}
time p = do
           start <- getTime
           x     <- p
           ()    <- x `seq` return ()
           end   <- getTime
           return (x, end `minus` start)

