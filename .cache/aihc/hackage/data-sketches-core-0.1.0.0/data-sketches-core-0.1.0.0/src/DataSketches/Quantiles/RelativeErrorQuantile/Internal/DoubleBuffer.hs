{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module DataSketches.Quantiles.RelativeErrorQuantile.Internal.DoubleBuffer
  ( DoubleBuffer
  , Capacity
  , GrowthIncrement
  , SpaceAtBottom
  , DoubleIsNonFiniteException(..)
  , mkBuffer
  , copyBuffer
  , append
  , ensureCapacity
  , getCountWithCriterion
  , getEvensOrOdds
  , (!) -- getItem
  , growthIncrement
  , spaceAtBottom
  , getCapacity
  , getCount
  , getSpace
  , getVector
  , isEmpty
  , isSorted
  , sort
  , mergeSortIn
  , trimCount
  ) where

import DataSketches.Quantiles.RelativeErrorQuantile.Types
    ( Criterion )
import Control.Monad ( unless, when )
import Control.Monad.Primitive ( PrimMonad(PrimState) )
import Data.Primitive.MutVar
    ( newMutVar, readMutVar, writeMutVar, MutVar )
import qualified Data.Vector.Unboxed as UVector
import qualified Data.Vector.Unboxed.Mutable as MUVector
import DataSketches.Core.Internal.URef
    ( URef, newURef, readURef, writeURef, modifyURef )
import Data.Vector.Algorithms.Intro (sortByBounds)
import GHC.Stack ( HasCallStack )
import System.IO.Unsafe ()
import qualified DataSketches.Quantiles.RelativeErrorQuantile.Internal.InequalitySearch as IS
import Control.Exception ( Exception, throw )
import DataSketches.Core.Snapshot ( TakeSnapshot(..) )

-- | A special buffer of floats specifically designed to support the ReqCompactor class.
data DoubleBuffer s = DoubleBuffer
  { vec :: {-# UNPACK #-} !(MutVar s (MUVector.MVector s Double))
  , count :: {-# UNPACK #-} !(URef s Int)
  , sorted :: {-# UNPACK #-} !(URef s Bool)
  , growthIncrement :: {-# UNPACK #-} !Int
  , spaceAtBottom :: !Bool
  }

data DoubleBufferSnapshot = DoubleBufferSnapshot
    { dbSnapshotVec :: UVector.Vector Double
    , dbSnapshotCount :: !Int
    , dbSnapshotSorted :: !Bool
    , dbSnapshotGrowthIncrement :: !Int
    , dbSnapshotSpaceAtBottom :: !Bool
    } deriving (Show)

instance TakeSnapshot DoubleBuffer where
  type Snapshot DoubleBuffer = DoubleBufferSnapshot

  takeSnapshot DoubleBuffer{..} = DoubleBufferSnapshot
    <$> (readMutVar vec >>= UVector.freeze)
    <*> readURef count
    <*> readURef sorted
    <*> pure growthIncrement
    <*> pure spaceAtBottom

type Capacity = Int
type GrowthIncrement = Int
type SpaceAtBottom = Bool

-- | Constructs an new empty FloatBuffer with an initial capacity specified by
-- the <code>capacity</code> argument.
mkBuffer :: PrimMonad m => Capacity -> GrowthIncrement -> SpaceAtBottom -> m (DoubleBuffer (PrimState m))
mkBuffer capacity_ growthIncrement spaceAtBottom = do
  vec <- newMutVar =<< MUVector.new capacity_
  count <- newURef 0
  sorted <- newURef True
  pure $ DoubleBuffer{..}

copyBuffer :: PrimMonad m => DoubleBuffer (PrimState m) -> m (DoubleBuffer (PrimState m))
copyBuffer buf@DoubleBuffer{..} = do
  vec <- newMutVar =<< MUVector.clone =<< getVector buf
  count <- newURef =<< getCount buf
  sorted <- newURef =<< readURef sorted
  pure $ DoubleBuffer {..}

-- | Appends the given item to the active array and increments the active count.
-- This will expand the array if necessary.
append :: PrimMonad m => DoubleBuffer (PrimState m) -> Double -> m ()
append buf@DoubleBuffer{..} x = do
  ensureSpace buf 1
  index <- if spaceAtBottom
    then
      (\capacity_ count_ -> capacity_ - count_ - 1)
        <$> getCapacity buf
        <*> getCount buf
    else readURef count
  modifyURef count (+ 1)
  getVector buf >>= \vec -> MUVector.unsafeWrite vec index x
  writeURef sorted False
{-# SCC append #-}

-- | Ensures that the capacity of this FloatBuffer is at least newCapacity.
-- If newCapacity &lt; capacity(), no action is taken.
ensureSpace :: PrimMonad m => DoubleBuffer (PrimState m) -> Int -> m ()
ensureSpace buf@DoubleBuffer{..} space = do
  count_ <- readURef count
  capacity_ <- getCapacity buf
  let notEnoughSpace = count_ + space > capacity_
  when notEnoughSpace $ do
    let newCap = count_ + space + growthIncrement
    ensureCapacity buf newCap

getVector :: (PrimMonad m, PrimState m ~ s) => DoubleBuffer s -> m (MUVector.MVector s Double)
getVector = readMutVar . vec
{-# INLINE getVector #-}

getCapacity :: PrimMonad m => DoubleBuffer (PrimState m) -> m Int
getCapacity = fmap MUVector.length . getVector
{-# INLINE getCapacity #-}

ensureCapacity :: PrimMonad m => DoubleBuffer (PrimState m) -> Int -> m ()
ensureCapacity buf@DoubleBuffer{..} newCapacity = do
  capacity_ <- getCapacity buf
  when (newCapacity > capacity_) $ do
    count_ <- getCount buf
    (srcPos, destPos) <- if spaceAtBottom
      then do
        pure (capacity_ - count_, newCapacity - count_)
      else pure (0, 0)
    oldVec <- getVector buf
    newVec <- MUVector.new newCapacity
    MUVector.unsafeCopy
      (MUVector.slice destPos count_ newVec)
      (MUVector.slice srcPos count_ oldVec)
    writeMutVar vec newVec
{-# SCC ensureCapacity #-}

newtype DoubleIsNonFiniteException = DoubleIsNonFiniteException Double
  deriving (Show, Eq)

instance Exception DoubleIsNonFiniteException

getCountWithCriterion :: PrimMonad m => DoubleBuffer (PrimState m) -> Double -> Criterion -> m Int
getCountWithCriterion buf@DoubleBuffer{..} value criterion = do
  when (isNaN value || isInfinite value) $ throw $ DoubleIsNonFiniteException value
  sort buf
  count_ <- getCount buf
  vec <- getVector buf
  (low, high) <- if spaceAtBottom
    then do
      capacity_ <- getCapacity buf
      pure (capacity_ - count_, capacity_ - 1)
    else pure (0, count_)

  ix <- IS.find criterion vec low high value
  pure $! if ix == MUVector.length vec
    then 0
    else ix - low + 1

-- data EvensOrOdds = Evens | Odds

getEvensOrOdds :: PrimMonad m => DoubleBuffer (PrimState m) -> Int -> Int -> Bool -> m (DoubleBuffer (PrimState m))
getEvensOrOdds buf@DoubleBuffer{..} startOffset endOffset odds = do
  (start, end) <- if spaceAtBottom
    then do
      basis <- (-) <$> getCapacity buf <*> getCount buf
      pure (basis + startOffset, basis + endOffset)
    else pure (startOffset, endOffset)
  sort buf
  let range = endOffset - startOffset
  vec <- getVector buf
  out <- MUVector.new (range `div` 2)
  go vec out start 0
  where
    odd = if odds then 1 else 0
    go vec !out !i !j = if j < MUVector.length out
      then do
        MUVector.unsafeWrite out j =<< MUVector.unsafeRead vec (i + odd)
        go vec out (i + 2) (j + 1)
      else do
        count <- newURef (MUVector.length out)
        sorted <- newURef True
        vec <- newMutVar out
        pure DoubleBuffer
          { vec = vec
          , count = count
          , sorted = sorted
          , growthIncrement = 0
          , spaceAtBottom = spaceAtBottom
          }
{-# SCC getEvensOrOdds #-}


(!) :: PrimMonad m => DoubleBuffer (PrimState m) -> Int -> m Double
(!) buf offset = do
  index <- if spaceAtBottom buf
    then do
      capacity_ <- getCapacity buf
      count_ <- getCount buf
      pure $! capacity_ - count_ + offset
    else pure offset
  vec <- getVector buf
  MUVector.read vec index

getCount :: PrimMonad m => DoubleBuffer (PrimState m) -> m Int
getCount = readURef . count

getSpace :: PrimMonad m => DoubleBuffer (PrimState m) -> m Int
getSpace buf@DoubleBuffer{..} = (-) <$> getCapacity buf <*> getCount buf

isEmpty :: PrimMonad m => DoubleBuffer (PrimState m) -> m Bool
isEmpty buf = (== 0) <$> getCount buf

isSorted :: PrimMonad m => DoubleBuffer (PrimState m) -> m Bool
isSorted = readURef . sorted

-- | Sorts the active region
sort :: PrimMonad m => DoubleBuffer (PrimState m) -> m ()
sort buf@DoubleBuffer{..} = do
  sorted_ <- isSorted buf
  unless sorted_ $ do
    capacity_ <- getCapacity buf
    count_ <- getCount buf
    let (start, end) = if spaceAtBottom
          then (capacity_ - count_, capacity_)
          else (0, count_)
    vec <- getVector buf
    sortByBounds compare vec start end
    writeURef sorted True
{-# SCC sort #-}

-- | Merges the incoming sorted buffer into this sorted buffer.
mergeSortIn :: (PrimMonad m, HasCallStack) => DoubleBuffer (PrimState m) -> DoubleBuffer (PrimState m) -> m ()
mergeSortIn this bufIn = do
  sort this
  sort bufIn

  thatBuf <- getVector bufIn
  bufInLen <- getCount bufIn

  ensureSpace this bufInLen
  count_ <- getCount this
  let totalLength = count_ + bufInLen

  thisBuf <- getVector this

  if spaceAtBottom this
    then do -- scan up, insert at bottom
      capacity_ <- getCapacity this
      bufInCapacity_ <- getCapacity bufIn
      inSs <- takeSnapshot bufIn
      let i = capacity_ - count_
      let j = bufInCapacity_ - bufInLen
      let targetStart = capacity_ - totalLength
      let k = targetStart
      mergeUpwards thisBuf thatBuf capacity_ bufInCapacity_ i j k
    else do -- scan down, insert at top
      let i = count_ - 1
      let j = bufInLen - 1
      let k = totalLength
      mergeDownwards thisBuf thatBuf i j (k - 1)

  modifyURef (count this) (+ bufInLen)
  writeURef (sorted this) True
  pure ()
  where
    mergeUpwards thisBuf thatBuf capacity_ bufInCapacity_ = go
      where
        go !i !j !k
          -- for loop ended
          | k >= capacity_ = pure ()
          -- both valid
          | i < capacity_ && j < bufInCapacity_ = do
            iVal <- MUVector.read thisBuf i
            jVal <- MUVector.read thatBuf j
            if iVal <= jVal
              then MUVector.unsafeWrite thisBuf k iVal >> go (i + 1) j (k + 1)
              else MUVector.unsafeWrite thisBuf k jVal >> go i (j + 1) (k + 1)
          -- i is valid
          | i < capacity_ = do
            MUVector.unsafeWrite thisBuf k =<< MUVector.read thisBuf i
            go (i + 1) j (k + 1)
          -- j is valid
          | j < bufInCapacity_ = do
            MUVector.unsafeWrite thisBuf k =<< MUVector.read thatBuf j
            go i (j + 1) (k + 1)
          -- neither is valid, break;
          | otherwise = pure ()
    mergeDownwards thisBuf thatBuf !i !j !k
      -- for loop ended
      | k < 0 = pure ()
      -- both valid
      | i >= 0 && j >= 0 = do
        iVal <- MUVector.read thisBuf i
        jVal <- MUVector.read thatBuf j
        if iVal >= jVal
          then do
            MUVector.unsafeWrite thisBuf k iVal >> continue (i - 1) j (k - 1)
          else do
            MUVector.unsafeWrite thisBuf k jVal >> continue i (j - 1) (k - 1)
      | i >= 0 = do
        MUVector.unsafeWrite thisBuf k =<< MUVector.read thisBuf i
        continue (i - 1) j (k - 1)
      | j >= 0 = do
        MUVector.unsafeWrite thisBuf k =<< MUVector.read thatBuf j
        continue i (j - 1) (k - 1)
      -- neither is valid, break;
      | otherwise = pure ()
      where
        continue = mergeDownwards thisBuf thatBuf
{-# SCC mergeSortIn #-}

trimCount :: PrimMonad m => DoubleBuffer (PrimState m) -> Int -> m ()
trimCount DoubleBuffer{..} newCount = modifyURef count (\oldCount -> if newCount < oldCount then newCount else oldCount)
