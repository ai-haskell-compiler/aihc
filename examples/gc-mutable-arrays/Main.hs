module Main (main) where

import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import GHC.Arr (STArray, newSTArray, readSTArray, writeSTArray)

-- A GC workload: long-lived mutable cells that receive fresh objects. Each
-- round writes a new boxed value into every cell, so an old cell points at
-- a young object. A generational collector sees these stores through its
-- write barrier and its remembered set.
cellCount :: Int
cellCount = 4096

rounds :: Int
rounds = 100

arraySum :: Int
arraySum = runST $ do
  cells <- newSTArray (0, cellCount - 1) (0 :: Int)
  forM_ [1 .. rounds] $ \round' ->
    forM_ [0 .. cellCount - 1] $ \index -> do
      value <- readSTArray cells index
      writeSTArray cells index $! value + round' * index
  foldST cells 0 0
  where
    foldST :: STArray s Int Int -> Int -> Int -> ST s Int
    foldST cells index acc
      | index == cellCount = pure acc
      | otherwise = do
          value <- readSTArray cells index
          foldST cells (index + 1) (acc + value)

referenceSum :: IO Int
referenceSum = do
  references <- mapM newIORef (replicate cellCount (0 :: Int))
  forM_ [1 .. rounds] $ \round' ->
    forM_ (zip [0 ..] references) $ \(index, reference) ->
      modifyIORef' reference (+ round' * index)
  sum <$> mapM readIORef references

main :: IO ()
main = do
  print arraySum
  referenceSum >>= print
