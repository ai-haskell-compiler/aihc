{-# LANGUAGE NamedFieldPuns #-}
module Control.Concurrent.BQueue
  ( BQueue
  , newBQueue
  , pushBQueue
  , popBQueue
  , takeBQueue
  , flushBQueue
  , lengthBQueue
  ) where

-- | FIFO Bounded Queue with O(1) amortized popping.
data BQueue a = BQueue
  { bqRead      :: ![a]
  , bqReadSize  :: {-# UNPACK #-}!Int
  , bqWrite     :: ![a]
  , bqWriteSize :: {-# UNPACK #-}!Int
  , bqMaxSize   :: {-# UNPACK #-}!Int
  }

-- | Create new Bounded Queue
newBQueue :: Int -- ^ Upper bound on the numer of elements the queue can hold.
          -> BQueue a
newBQueue bqMaxSize =
  BQueue {bqRead = [], bqReadSize = 0, bqWrite = [], bqWriteSize = 0, bqMaxSize}

-- | Push an element onto the queue. Returns the new queue with the element placed onto the right
-- side of the source queue, but only if the maximum bound hasn't been reached, otherwise it will
-- return `Nothing`
pushBQueue :: a -> BQueue a -> Maybe (BQueue a)
pushBQueue x bq@BQueue {bqWrite, bqWriteSize}
  | bqReadSize bq + bqWriteSize < bqMaxSize bq =
    Just bq {bqWrite = x : bqWrite, bqWriteSize = bqWriteSize + 1}
  | otherwise = Nothing


-- | Pop an element from the queue. Returns the leftmost element from the queue together with new
-- queue, lacking that element, `Nothing` if the queue was empty.
popBQueue :: BQueue a -> Maybe (a, BQueue a)
popBQueue bq@BQueue {bqRead, bqReadSize, bqWrite, bqWriteSize} =
  case bqRead of
    (x:xs) -> Just (x, bq {bqRead = xs, bqReadSize = bqReadSize - 1})
    [] ->
      case reverse bqWrite of
        (y:ys) ->
          Just (y, bq {bqRead = ys, bqReadSize = bqWriteSize - 1, bqWrite = [], bqWriteSize = 0})
        [] -> Nothing

-- | /O(n) - Get all the elements from the Bounded Queue.
flushBQueue :: BQueue a -> ([a], BQueue a)
flushBQueue bq = (bqRead bq ++ reverse (bqWrite bq), newBQueue (bqMaxSize bq))

-- | /O(i)/ - Take @i@ elements from the Bounded Queue. This function doesn't fail - it returns empty
-- list on negative @i@ and all elements there is if requested more than available.
takeBQueue :: Int -> BQueue a -> ([a], BQueue a)
takeBQueue i bq@BQueue {bqRead, bqReadSize, bqWrite, bqWriteSize}
  | i < bqReadSize =
    let (taken, leftover) = splitAt i bqRead
     in (taken, bq {bqRead = leftover, bqReadSize = bqReadSize - max 0 i})
  | i < totalSize =
    let (taken, leftover) = splitAt i (bqRead ++ reverse bqWrite)
     in (taken, bq {bqRead = leftover, bqReadSize = totalSize - i, bqWrite = [], bqWriteSize = 0})
  | otherwise = flushBQueue bq
  where
    totalSize = bqReadSize + bqWriteSize


-- | /O(1)/ - Get the current length of a queue
lengthBQueue :: BQueue a -> Int
lengthBQueue bq = bqReadSize bq + bqWriteSize bq
