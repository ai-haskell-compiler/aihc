
{-# LANGUAGE BangPatterns #-}

module Main where

import qualified Data.TLS.GHC as GHC
import qualified Data.TLS.PThread as PT

-- Retired:
-- import qualified Data.TLS.GCC as GCC

import Criterion
import Criterion.Main
import Data.Atomics.Counter

import Control.Monad
import Control.Concurrent (forkOS)
import Control.Concurrent.MVar
import Data.IORef
import GHC.Conc
import System.Environment
--------------------------------------------------------------------------------

main :: IO ()
main = do
  numProc <- getNumProcessors
  n       <- getNumCapabilities
  when (n == 1) $ do putStrLn "HACK: using setNumCapabilities to bump it up... should set this in the .cabal"
                     setNumCapabilities numProc
  numCap  <- getNumCapabilities
  putStrLn $ "Benchmarking platform with "++show numProc++
             " processors, while currently using "++show numCap++" threads."

  -- Substitute in default command line args:
  args <- getArgs
  let args' = if null args
              then words $ " --regress=allocated:iters --regress=bytesCopied:iters --regress=cycles:iters "++
                           " --regress=numGcs:iters --regress=mutatorWallSeconds:iters --regress=gcWallSeconds:iters "++
                           " --regress=cpuTime:iters " ++
                           " -o tls_report.html "
--                         ++ " --raw tls_report.criterion "
              else args

      threadify fn =
          bgroup ""
          [ fn (threads, suff)
          | threads <- [1..numCap*4],
            let suff = "_" ++ show threads ++"io_"++ show numCap++"os" ]

      mkTests name mkTLS getTLS freeAllTLS = bgroup name
         [
           -- bench ("counter/getTLS/incrCntr"++suff) $
           --       benchPar0 threads (GHC.mkTLS (newCounter 0))
           --                     (\t -> incrCounter_ 1 =<< GHC.getTLS t)
           threadify $ \ (threads,suff) ->
            bench ("counter/getTLS/readIORef"++suff) $
                  benchPar0 threads (mkTLS (newIORef ()))
                                (\t -> readIORef =<< getTLS t)
                                freeAllTLS
         ]

  withArgs args' $ defaultMain $
   [ mkTests "PThread" PT.mkTLS  PT.getTLS  PT.freeAllTLS
   , mkTests "GHC"     GHC.mkTLS GHC.getTLS GHC.freeAllTLS
   ]
{-     bgroup "infrastructure"
      [ bench ("benchPar1"++suff) $ benchPar1 threads (return ())
      , bench ("benchPar0"++suff) $ benchPar0 threads (return ()) (\_ -> return ())
--      , bench ("benchPar2"++suff) $ benchPar2 threads (return ())
      ], -}

   -- | ]

 where



----------------------------------------------------------------------------------------------------

benchPar0 :: Int -> IO a -> (a -> IO ()) -> (a -> IO ()) -> Benchmarkable
benchPar0 numT new fn shutd = toBenchmarkable $ \ iters -> do
  x <- new
  numCap  <- getNumCapabilities
  -- We compute the number of iterations such that the time would be
  -- flat IFF parallelism works perfectly up to numCapabilities, and
  -- then load balancing works perfectly when # threads exceeds
  -- numCapabilities.
  let totalIters = (fromIntegral iters) * (max numCap numT)
      perThread  = totalIters `quot` numT
  mvs <- replicateM numT $ do
    v <- newEmptyMVar
    _ <- forkOS $ do rep perThread (fn x)
                     putMVar v ()
    return v
  forM_ mvs takeMVar
  -- Shut down only when all threads are finished with it:
  shutd x
{-# INLINE benchPar0 #-}


-- | Benchmarking the same action on ALL of N threads.
--   This version uses MVar synchronization.
benchPar1 :: Int -> IO () -> Benchmarkable
benchPar1 num act = toBenchmarkable $ \ iters -> do
  mvs <- replicateM num $ do
    v <- newEmptyMVar
    _ <- forkOS $ do rep (fromIntegral iters) act
                     putMVar v ()
    return v
  forM_ mvs takeMVar
{-# INLINE benchPar1 #-}

-- | This version never blocks on an MVar.
benchPar2 :: Int -> IO () -> Benchmarkable
benchPar2 num act = toBenchmarkable $ \ iters -> do
  done <- newCounter 0
  let waitCounter = do x <- readCounter done
                       unless (num == x) waitCounter
      go = do rep (fromIntegral iters) act
              incrCounter_ 1 done
              waitCounter
  replicateM_ (num-1) $ forkOS go
  go

{-# INLINE benchPar2 #-}

-- | My own forM for inclusive numeric ranges (not requiring deforestation optimizations).
for_ :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
for_ start end _fn | start > end = error "for_: start is greater than end"
for_ start end fn = loop start
  where
   loop !i | i > end   = return ()
           | otherwise = do fn i; loop (i+1)
{-# INLINE for_ #-}

rep :: Monad m => Int -> (m ()) -> m ()
rep n m = for_ 1 n (\_ -> m)
{-# INLINE rep #-}

