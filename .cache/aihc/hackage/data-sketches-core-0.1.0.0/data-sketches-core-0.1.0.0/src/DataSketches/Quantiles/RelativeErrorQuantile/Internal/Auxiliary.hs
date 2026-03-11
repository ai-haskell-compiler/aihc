module DataSketches.Quantiles.RelativeErrorQuantile.Internal.Auxiliary
  ( ReqAuxiliary(..)
  , MReqAuxiliary (..)
  , mkAuxiliary
  , getQuantile
  -- | Really extra private, just needed for tests
  , mergeSortIn
  ) where

import GHC.TypeLits
import Control.Monad (when)
import Control.Monad.Primitive
import Data.Bits (shiftL)
import Data.Word
import Data.Primitive.MutVar
import Data.Vector.Algorithms.Search
import qualified Data.Vector as Vector
import qualified Data.Vector.Unboxed.Mutable as MUVector
import DataSketches.Quantiles.RelativeErrorQuantile.Types
import DataSketches.Quantiles.RelativeErrorQuantile.Internal.Compactor (ReqCompactor)
import qualified DataSketches.Quantiles.RelativeErrorQuantile.Internal.Compactor as Compactor
import DataSketches.Quantiles.RelativeErrorQuantile.Internal.DoubleBuffer (DoubleBuffer)
import qualified DataSketches.Quantiles.RelativeErrorQuantile.Internal.DoubleBuffer as DoubleBuffer
import qualified Data.Vector.Unboxed as U
import Control.Monad.ST
import DataSketches.Quantiles.RelativeErrorQuantile.Internal.InequalitySearch (find)
import qualified DataSketches.Quantiles.RelativeErrorQuantile.Internal.InequalitySearch as IS
import Debug.Trace
import qualified Data.Vector.Generic.Mutable as MG

data ReqAuxiliary = ReqAuxiliary
  { raWeightedItems :: {-# UNPACK #-} !(U.Vector (Double, Word64))
  , raHighRankAccuracy :: !RankAccuracy
  , raSize :: {-# UNPACK #-} !Word64
  }
  deriving (Show, Eq)

data MReqAuxiliary s = MReqAuxiliary
  { mraWeightedItems :: {-# UNPACK #-} !(MutVar s (MUVector.MVector s (Double, Word64)))
  , mraHighRankAccuracy :: !RankAccuracy
  , mraSize :: {-# UNPACK #-} !Word64
  }

mkAuxiliary :: (PrimMonad m, s ~ PrimState m) => RankAccuracy -> Word64 -> Int -> Vector.Vector (ReqCompactor s) -> m ReqAuxiliary
mkAuxiliary rankAccuracy totalN retainedItems compactors = do
  items <- newMutVar =<< MUVector.replicate retainedItems (0, 0)
  let this = MReqAuxiliary
        { mraWeightedItems = items
        , mraHighRankAccuracy = rankAccuracy
        , mraSize = totalN
        }
  Vector.foldM_ (mergeBuffers this) 0 compactors
  createCumulativeWeights this
  dedup this
  items' <- U.unsafeFreeze =<< readMutVar items
  pure ReqAuxiliary
    { raWeightedItems = items'
    , raHighRankAccuracy = rankAccuracy
    , raSize = totalN
    }
  where
    mergeBuffers this auxCount compactor = do
      buff <- Compactor.getBuffer compactor
      buffSize <-  DoubleBuffer.getCount buff
      let lgWeight = Compactor.getLgWeight compactor
          weight = 1 `shiftL` fromIntegral lgWeight
      mergeSortIn this buff weight auxCount
      pure $ auxCount + buffSize

getWeightedItems :: PrimMonad m => MReqAuxiliary (PrimState m) -> m (MUVector.MVector (PrimState m) (Double, Word64))
getWeightedItems = readMutVar . mraWeightedItems

getItems :: PrimMonad m => MReqAuxiliary (PrimState m) -> m (MUVector.MVector (PrimState m) Double)
getItems = fmap (fst . MUVector.unzip) . getWeightedItems

getWeights :: PrimMonad m => MReqAuxiliary (PrimState m) -> m (MUVector.MVector (PrimState m) Word64)
getWeights = fmap (snd . MUVector.unzip) . getWeightedItems

getQuantile :: ReqAuxiliary -> Double -> Criterion  -> Double
getQuantile this normalRank ltEq = fst (weightedItems U.! ix)
  where
    ix = if searchResult == U.length weightedItems
      then searchResult - 1
      else searchResult
    searchResult = runST $ do
      v <- U.unsafeThaw $ snd $ U.unzip weightedItems
      let search = case ltEq of
            (:<) -> find (IS.:>)
            (:<=) -> find (IS.:>=)
      search v 0 (weightsSize - 1) rank
    weightedItems = raWeightedItems this
    weightsSize = U.length weightedItems
    rank = floor (normalRank * fromIntegral (raSize this))

createCumulativeWeights :: PrimMonad m => MReqAuxiliary (PrimState m) -> m ()
createCumulativeWeights this = do
  weights <- getWeights this
  let size = MUVector.length weights
  let accumulateM i weight = do
        when (i > 0) $ do
          prevWeight <- MUVector.read weights (i - 1)
          MUVector.unsafeWrite weights i (weight + prevWeight)
  forI_ weights (\i -> MUVector.read weights i >>= \x -> accumulateM i x)
  lastWeight <- MUVector.read weights (size - 1)
  when (lastWeight /= mraSize this) $ do
    error "invariant violated: lastWeight does not equal raSize"
  where
    forI_ :: (Monad m, MG.MVector v a) => v (PrimState m) a -> (Int -> m b) -> m ()
    {-# INLINE forI_ #-}
    forI_ v f = loop 0
      where
        loop i 
          | i >= n    = return ()
          | otherwise = f i >> loop (i + 1)
        n = MG.length v


dedup :: PrimMonad m => MReqAuxiliary (PrimState m) -> m ()
dedup this = do
  weightedItems <- getWeightedItems this
  let size = MUVector.length weightedItems
  weightedItemsB <- MUVector.replicate size (0, 0)
  bi <- doDedup weightedItems size weightedItemsB 0 0
  writeMutVar (mraWeightedItems this) $ MUVector.slice 0 bi weightedItemsB
  where
    doDedup weightedItems itemsSize weightedItemsB = go
      where 
        go !i !bi
          | i >= itemsSize = pure bi
          | otherwise = do
            let j = i + 1
                hidup = j
                countDups !j !hidup = if j < itemsSize 
                  then do
                    (itemI, _) <- MUVector.read weightedItems i
                    (itemJ, _) <- MUVector.read weightedItems j
                    if itemI == itemJ
                      then countDups (j + 1) j
                      else pure (j, hidup)
                  else pure (j, hidup)
            (j', hidup') <- countDups j hidup
            if j' - i == 1 -- no dups
              then do
                (item, weight) <- MUVector.read weightedItems i
                MUVector.unsafeWrite weightedItemsB bi (item, weight)
                go (i + 1) (bi + 1)
              else do
                (item, weight) <- MUVector.read weightedItems hidup'
                MUVector.unsafeWrite weightedItemsB bi (item, weight)
                go j' (bi + 1)

mergeSortIn :: PrimMonad m => MReqAuxiliary (PrimState m) -> DoubleBuffer (PrimState m) -> Word64 -> Int -> m ()
mergeSortIn this bufIn defaultWeight auxCount = do
  DoubleBuffer.sort bufIn
  weightedItems <- getWeightedItems this
  otherItems <- DoubleBuffer.getVector bufIn
  otherBuffSize <- DoubleBuffer.getCount bufIn
  otherBuffCapacity <- DoubleBuffer.getCapacity bufIn
  let totalSize = otherBuffSize + auxCount - 1
      height = case mraHighRankAccuracy this of
        HighRanksAreAccurate -> otherBuffCapacity - 1
        LowRanksAreAccurate -> otherBuffSize - 1
  merge totalSize weightedItems otherItems (auxCount - 1) (otherBuffSize - 1) height 
  where
    merge totalSize weightedItems otherItems = go totalSize
      where
        go !k !i !j !h 
          | k < 0 = pure ()
          | i >= 0 && j >= 0 = do
            (item, weight) <- MUVector.read weightedItems i
            otherItem <- MUVector.read otherItems h
            if item >= otherItem
               then do
                 MUVector.unsafeWrite weightedItems k (item, weight)
                 continue (i - 1) j h
               else do
                 MUVector.unsafeWrite weightedItems k (otherItem, defaultWeight)
                 continue i (j - 1) (h - 1)
          | i >= 0 = do
            MUVector.read weightedItems i >>= MUVector.write weightedItems k
            continue (i - 1) j h
          | j >= 0 = do
            otherItem <- MUVector.read otherItems h
            MUVector.unsafeWrite weightedItems k (otherItem, defaultWeight)
            continue i (j - 1) (h - 1)
          | otherwise = pure ()
          where
            continue = go (k - 1)

