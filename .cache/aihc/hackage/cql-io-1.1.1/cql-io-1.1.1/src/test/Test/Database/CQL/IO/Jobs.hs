{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Test.Database.CQL.IO.Jobs (tests) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Data.IORef
import Data.Either
import Data.Maybe
import Data.Numbers.Primes
import Test.Tasty
import Test.Tasty.HUnit

import Database.CQL.IO.Jobs

tests :: TestTree
tests = testGroup "Database.CQL.IO.Jobs"
    [ testCase "run-sequential"     testRunJobSequential
    , testCase "run-concurrent"     testRunJobConcurrent
    , testCase "try-run-sequential" testTryRunJobSequential
    , testCase "try-run-concurrent" testTryRunJobConcurrent
    , testCase "try-run-replace"    testTryRunJobReplace
    ]

-----------------------------------------------------------------------------
-- Tests

-- 1. Sequentially run jobs with the same key, each waiting on a latch.
-- 2. Release all latches.
-- 3. Expect only the last job to complete and all others to be replaced.
testRunJobSequential :: IO ()
testRunJobSequential = do
    jobs <- newJobs
    iref <- newIORef (1 :: Int)
    let prepare = prepareRun jobs . mulJob iref
    (fails, succs) <- execute mapM prepare (take 100 primes)
    val <- readIORef iref
    map fromException fails @?= replicate 99 (Just JobReplaced)
    succs                   @?= [()]
    val                     @?= 541 -- 100th prime => 100th job

-- 1. Concurrently run jobs with the same key, each waiting on a latch.
-- 2. Release all latches.
-- 3. Expect only one (non-deterministic) job to complete and all others to
--    be replaced.
testRunJobConcurrent :: IO ()
testRunJobConcurrent = do
    jobs <- newJobs
    iref <- newIORef (0 :: Int)
    let prepare = prepareRun jobs . incJob iref
    (fails, succs) <- execute mapConcurrently prepare [1..100 :: Int]
    val <- readIORef iref
    map fromException fails @?= replicate 99 (Just JobReplaced)
    succs                   @?= [()]
    val                     @?= 1

-- 1. Try to run jobs for the same key, each waiting on a latch.
-- 2. Release all latches.
-- 3. Expect only the first job to run.
testTryRunJobSequential :: IO ()
testTryRunJobSequential = do
    jobs <- newJobs
    iref <- newIORef (1 :: Int)
    let prepare = prepareTryRun jobs . mulJob iref
    (fails, succs) <- execute mapM prepare (take 100 primes)
    val <- readIORef iref
    length fails @?= 0
    succs        @?= [()]
    val          @?= 2

-- 1. Concurrently try to run jobs for the same key, each waiting on a latch.
-- 2. Release all latches.
-- 3. Expect only one job to run.
testTryRunJobConcurrent :: IO ()
testTryRunJobConcurrent = do
    jobs <- newJobs
    iref <- newIORef (0 :: Int)
    let prepare = prepareTryRun jobs . incJob iref
    (fails, succs) <- execute mapConcurrently prepare [1..100]
    val <- readIORef iref
    length fails @?= 0
    succs        @?= [()]
    val          @?= 1

-- 1. Concurrently try to run jobs for the same key, each waiting on
--    a latch. One of the jobs is run unconditionally and should
--    thus always take precedence.
-- 2. Release all latches.
-- 3. Expect only the unconditionally run job to complete,
--    replacing one job. All others are never run.
testTryRunJobReplace :: IO ()
testTryRunJobReplace = do
    jobs <- newJobs
    iref <- newIORef (1 :: Int)
    let prepare = choose jobs (mulJob iref)
    (Just a1, l1)  <- prepare 2 -- there should always be a job to replace
    (fails, succs) <- execute mapConcurrently prepare (take 99 (drop 1 primes))
    putMVar l1 ()
    r1  <- either Just (const Nothing) <$> waitCatch a1
    val <- readIORef iref
    (fromException =<< r1) @?= Just JobReplaced
    length fails           @?= 0
    succs                  @?= [()]
    val                    @?= 229 -- 50th's prime is unconditionally run
  where
    choose jobs run i
        | i == 229  = prepareRun    jobs (run i)
        | otherwise = prepareTryRun jobs (run i)

-----------------------------------------------------------------------------
-- Test Jobs

type PrepareJob a = a -> IO (Maybe (Async ()), MVar ())
type ExecuteJob a = IORef a -> a -> IO ()

jobKey :: Int
jobKey = 1

prepareRun :: Jobs Int -> IO () -> IO (Maybe (Async ()), MVar ())
prepareRun jobs run = do
    l <- newEmptyMVar
    a <- runJob jobs jobKey $ takeMVar l >> run
    return (Just a, l)

prepareTryRun :: Jobs Int -> IO () -> IO (Maybe (Async ()), MVar ())
prepareTryRun jobs run = do
    l <- newEmptyMVar
    a <- tryRunJob jobs jobKey $ takeMVar l >> run
    return (a, l)

execute
    :: (forall a b. (a -> IO b) -> [a] -> IO [b])
    -> PrepareJob i
    -> [i]
    -> IO ([SomeException], [()])
execute runIO prepJob input = do
    js <- runIO prepJob input
    as <- runIO (\(a, l) -> putMVar l () *> pure a) js
    partitionEithers <$> mapM waitCatch (catMaybes as)

incJob :: ExecuteJob Int
incJob iref _ = atomicModifyIORef' iref (\x -> (x + 1, ()))

mulJob :: ExecuteJob Int
mulJob iref i = atomicModifyIORef' iref (\x -> (x * i, ()))

