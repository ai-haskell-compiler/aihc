-- | Concurrent processing utilities with progress reporting.
--
-- This module provides bounded concurrent processing primitives that are
-- useful for processing large numbers of items (like packages or files)
-- while showing progress to the user.
module ConcurrentProgress
  ( -- * Bounded concurrent mapping
    mapConcurrentlyBounded,
    mapConcurrentlyBoundedWithProgress,

    -- * Chunked concurrent folding
    foldConcurrentlyChunks,
    foldConcurrentlyChunksWithProgress,

    -- * Progress display
    ProgressState (..),
    putProgressLine,
    clearProgressLine,

    -- * Utilities
    chunksOf,
  )
where

import Control.Concurrent (newChan, readChan, writeChan)
import Control.Concurrent.Async (async, mapConcurrently, wait)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Monad (when)
import Data.List (sortOn)
import System.IO (hFlush, hIsTerminalDevice, stdout)

-- | Progress state for display purposes.
data ProgressState = ProgressState
  { progressDone :: !Int,
    progressSuccess :: !Int,
    progressTotal :: !Int
  }
  deriving (Eq, Show)

-- | Display a progress line on the terminal.
--
-- Overwrites the current line using carriage return.
putProgressLine :: ProgressState -> IO ()
putProgressLine p = do
  putStr
    ( "\r"
        ++ show (progressSuccess p)
        ++ "/"
        ++ show (progressTotal p)
        ++ " ("
        ++ show (progressDone p)
        ++ "/"
        ++ show (progressTotal p)
        ++ " processed)"
    )
  hFlush stdout

-- | Clear the progress line.
clearProgressLine :: IO ()
clearProgressLine = putStrLn ""

-- | Map a function over a list with bounded concurrency.
--
-- Unlike 'mapConcurrently', this limits the number of concurrent operations
-- to avoid overwhelming system resources.
mapConcurrentlyBounded :: Int -> (a -> IO b) -> [a] -> IO [b]
mapConcurrentlyBounded jobs action items = do
  let indexedItems = zip [0 :: Int ..] items
      workerCount = max 1 (min jobs (length indexedItems))
  queue <- newChan
  results <- newMVar []
  mapM_ (writeChan queue . Just) indexedItems
  mapM_ (const (writeChan queue Nothing)) [1 .. workerCount]
  workers <- mapM (\_ -> async (workerLoop queue results)) [1 .. workerCount]
  mapM_ wait workers
  ordered <- readMVar results
  pure (map snd (sortOn fst ordered))
  where
    workerLoop queue results = do
      next <- readChan queue
      case next of
        Nothing -> pure ()
        Just (idx, item) -> do
          value <- action item
          modifyMVar_ results (\acc -> pure ((idx, value) : acc))
          workerLoop queue results

-- | Map a function over a list with bounded concurrency and progress reporting.
mapConcurrentlyBoundedWithProgress ::
  Int ->
  (a -> IO b) ->
  [a] ->
  (b -> Bool) ->
  IO [b]
mapConcurrentlyBoundedWithProgress jobs action items isSuccess = do
  showProgress <- hIsTerminalDevice stdout
  let total = length items
  counter <- newMVar (0, 0)
  let worker item = do
        result <- action item
        when showProgress $ do
          (done', success') <- modifyMVar counter $ \(d, s) ->
            let d' = d + 1
                s' = if isSuccess result then s + 1 else s
             in pure ((d', s'), (d', s'))
          putProgressLine (ProgressState done' success' total)
        pure result
  results <- mapConcurrentlyBounded jobs worker items
  when showProgress clearProgressLine
  pure results

-- | Split a list into chunks of the given size.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
  let (chunk, rest) = splitAt n xs
   in chunk : chunksOf n rest

-- | Fold over items in concurrent chunks.
--
-- Items are processed in chunks where each chunk is processed concurrently.
-- Results from each chunk are folded into an accumulator.
foldConcurrentlyChunks ::
  -- | Number of concurrent workers per chunk
  Int ->
  -- | Processing function
  (a -> IO b) ->
  -- | Items to process
  [a] ->
  -- | Initial accumulator
  acc ->
  -- | Fold function
  ([b] -> acc -> acc) ->
  IO acc
foldConcurrentlyChunks n action items acc0 folder =
  go acc0 (chunksOf chunkSize items)
  where
    chunkSize = if n <= 0 then 1 else n
    go acc [] = pure acc
    go acc (chunk : rest) = do
      batch <- mapConcurrently action chunk
      let acc' = folder batch acc
      go acc' rest

-- | Fold over items in concurrent chunks with progress reporting.
--
-- Like 'foldConcurrentlyChunks' but displays progress on the terminal.
foldConcurrentlyChunksWithProgress ::
  -- | Number of concurrent workers per chunk
  Int ->
  -- | Processing function
  (a -> IO b) ->
  -- | Items to process
  [a] ->
  -- | Total item count
  Int ->
  -- | Whether to show progress
  Bool ->
  -- | Initial accumulator
  acc ->
  -- | Fold function that also returns success count increment
  ([b] -> acc -> (acc, Int)) ->
  IO acc
foldConcurrentlyChunksWithProgress n action items total showProgress acc0 folder = do
  when showProgress (putProgressLine (ProgressState 0 0 total))
  go 0 0 acc0 (chunksOf chunkSize items)
  where
    chunkSize = if n <= 0 then 1 else n
    go _ _ acc [] = pure acc
    go done success acc (chunk : rest) = do
      batch <- mapConcurrently action chunk
      let done' = done + length batch
          (acc', successInc) = folder batch acc
          success' = success + successInc
      when showProgress (putProgressLine (ProgressState done' success' total))
      go done' success' acc' rest

-- | Increment progress counter and optionally display.
printProgress :: MVar Int -> Int -> IO ()
printProgress counter total = do
  done <- modifyMVar counter $ \n ->
    let n' = n + 1
     in pure (n', n')
  putStr ("\rProcessed " ++ show done ++ "/" ++ show total)
  hFlush stdout
