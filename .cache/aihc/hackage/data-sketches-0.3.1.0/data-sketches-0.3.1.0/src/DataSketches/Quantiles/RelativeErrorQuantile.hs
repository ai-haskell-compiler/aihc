-- | The Relative Error Quantile (REQ) sketch provides extremely high accuracy at a chosen end of the rank domain. 
-- This is best illustrated with some rank domain accuracy plots that compare the KLL quantiles sketch to the REQ sketch.
--
-- This first plot illustrates the typical error behavior of the KLL sketch (also the quantiles/DoublesSketch). 
-- The error is flat for all ranks (0, 1). The green and yellow lines correspond to +/- one RSE at 68% confidence; 
-- the blue and red lines, +/- two RSE at 95% confidence; and, the purple and brown lines +/- 3 RSE at 99% confidence. 
-- The reason all the curves pinch at 0 and 1.0, is because the sketch knows with certainty that a request for a quantile at 
-- rank = 0 is the minimum value of the stream; and a request for a quantiles at rank = 1.0, is the maximum value of the stream. 
-- Both of which the sketch tracks.
--
-- ![KLL Gaussian Error Quantiles](docs/images/KllErrorK100SL11.png)
--
-- The next plot is the exact same data and queries fed to the REQ sketch set for High Rank Accuracy (HRA) mode. 
-- In this plot, starting at a rank of about 0.3, the contour lines start converging and actually reach zero error at 
-- rank 1.0. Therefore the error (the inverse of accuracy) is relative to the requested rank, thus the name of the sketch. 
-- This means that the user can perform getQuantile(rank) queries, where rank = .99999 and get accurate results.
--
-- ![ReqSketch Gaussian Error Quantiles - HighRankAccuracy](docs/images/ReqErrorHraK12SL11_LT.png)
--
-- This next plot is also the same data and queries, except the REQ sketch was configured for Low Rank Accuracy (LRA). In this case the user can perform getQuantiles(rank) queries, where rank = .00001 and get accurate results.
--
-- ![ReqSketch Gaussian Error Quantiles - LowRankAccuracy](docs/images/ReqErrorLraK12SL11_LE.png)

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
module DataSketches.Quantiles.RelativeErrorQuantile (
  -- * Construction
    ReqSketch (criterion)
  , mkReqSketch
  -- ** Configuration settings
  , RankAccuracy(..)
  , Criterion(..)
  -- * Sketch summaries
  , count
  , null
  , sum
  , maximum
  , minimum
  , retainedItemCount
  , relativeStandardError
  , countWithCriterion
  , probabilityMassFunction
  , quantile
  , quantiles
  , rank
  , rankLowerBound
  , ranks
  , rankUpperBound
  , cumulativeDistributionFunction
  , getK
  -- * Updating the sketch
  , merge
  , insert
  , rankAccuracy
  , isEstimationMode
  , isLessThanOrEqual
  -- | If you see this error, please file an issue in the GitHub repository.
  , CumulativeDistributionInvariants(..)
  ) where

import Control.Monad (when, unless, foldM, foldM_)
import Control.Monad.Primitive ( PrimMonad(PrimState) )
import Data.Bits (shiftL)
import Data.Vector ((!), imapM_)
import qualified Data.Vector as Vector
import Data.Primitive.MutVar
    ( modifyMutVar', newMutVar, readMutVar, writeMutVar )
import Data.Word ( Word32, Word64 )
import DataSketches.Quantiles.RelativeErrorQuantile.Internal.Constants
    ( fixRseFactor, initNumberOfSections, relRseFactor )
import DataSketches.Quantiles.RelativeErrorQuantile.Types
    ( Criterion(..), RankAccuracy(..) )
import DataSketches.Quantiles.RelativeErrorQuantile.Internal.Compactor (ReqCompactor)
import DataSketches.Quantiles.RelativeErrorQuantile.Internal.Auxiliary (ReqAuxiliary)
import DataSketches.Quantiles.RelativeErrorQuantile.Internal
    ( count,
      retainedItemCount,
      CumulativeDistributionInvariants(..),
      ReqSketch(..),
      computeTotalRetainedItems,
      getCompactors )
import qualified DataSketches.Quantiles.RelativeErrorQuantile.Internal.Auxiliary as Auxiliary
import qualified DataSketches.Quantiles.RelativeErrorQuantile.Internal.Compactor as Compactor
import qualified DataSketches.Quantiles.RelativeErrorQuantile.Internal.DoubleBuffer as DoubleBuffer
import DataSketches.Core.Internal.URef
    ( modifyURef, newURef, readURef, writeURef )
import Data.Maybe (isNothing)
import qualified Data.Foldable
import qualified Data.List
import GHC.Exception.Type (Exception)
import Control.Exception (throw, assert)
import System.Random.MWC (Gen, create)
import qualified Data.Vector.Generic.Mutable as MG
import Prelude hiding (sum, minimum, maximum, null)

-- | The K parameter can be increased to trade increased space efficiency for higher accuracy in rank and quantile
-- calculations. Due to the way the compaction algorithm works, it must be an even number between 4 and 1024.
--
-- A good starting number when in doubt is 6.
mkReqSketch :: forall m. (PrimMonad m)
  => Word32 -- ^ K
  -> RankAccuracy
  -> m (ReqSketch (PrimState m))
mkReqSketch k rank = do
  unless (even k && k >= 4 && k <= 1024) $ error "k must be divisible by 2, and satisfy 4 <= k <= 1024"
  r <- ReqSketch k rank (:<)
    <$> create
    <*> newURef 0
    <*> newURef (0 / 0)
    <*> newURef (0 / 0)
    <*> newURef 0
    <*> newURef 0
    <*> newURef 0
    <*> newMutVar Nothing
    <*> newMutVar Vector.empty
  grow r
  pure r


getAux :: PrimMonad m => ReqSketch (PrimState m) -> m (Maybe ReqAuxiliary)
getAux = readMutVar . aux

getNumLevels :: PrimMonad m => ReqSketch (PrimState m) -> m Int
getNumLevels = fmap Vector.length . getCompactors

getIsEmpty :: PrimMonad m => ReqSketch (PrimState m) -> m Bool
getIsEmpty = fmap (== 0) . readURef . totalN

getK :: ReqSketch s -> Word32
getK = k

getMaxNominalCapacity :: PrimMonad m => ReqSketch (PrimState m) -> m Int
getMaxNominalCapacity = readURef . maxNominalCapacitiesSize

validateSplits :: Monad m => [Double] -> m ()
validateSplits splits = do
  when (Data.Foldable.null splits) $ do
    throw CumulativeDistributionInvariantsSplitsAreEmpty
  when (any isInfinite splits || any isNaN splits) $ do
    throw CumulativeDistributionInvariantsSplitsAreNotFinite
  when (Data.List.nub (Data.List.sort splits) /= splits) $ do
    throw CumulativeDistributionInvariantsSplitsAreNotUniqueAndMontonicallyIncreasing

getCounts :: (PrimMonad m) => ReqSketch (PrimState m) -> [Double] -> m [Word64]
getCounts this values = do
  compactors <- getCompactors this
  let numValues = length values
      numCompactors = Vector.length compactors
      ans = replicate numValues 0
  isEmpty <- getIsEmpty this
  if isEmpty
    then pure []
    else Vector.ifoldM doCount ans compactors
  where
    doCount acc index compactor = do
      let wt = (1 `shiftL` fromIntegral (Compactor.getLgWeight compactor)) :: Word64
      buff <- Compactor.getBuffer compactor
      let updateCounts buff value = do
            count_ <- DoubleBuffer.getCountWithCriterion buff (values !! index) (criterion this)
            pure $ fromIntegral value + fromIntegral count_ * wt
      mapM (updateCounts buff) acc

getPMForCDF :: (PrimMonad m) => ReqSketch (PrimState m) -> [Double] -> m [Word64]
getPMForCDF this splits = do
  () <- validateSplits splits
  let numSplits = length splits
      numBuckets = numSplits -- + 1
  splitCounts <- getCounts this splits
  n <- count this
  pure $ (++ [n]) $ take numBuckets splitCounts

-- | Returns an approximation to the Cumulative Distribution Function (CDF), which is the cumulative analog of the PMF, 
-- of the input stream given a set of splitPoint (values).
cumulativeDistributionFunction
  :: (PrimMonad m)
  => ReqSketch (PrimState m)
  -> [Double]
  -- ^ Returns an approximation to the Cumulative Distribution Function (CDF), 
  -- which is the cumulative analog of the PMF, of the input stream given a set of 
  -- splitPoint (values).
  --
  -- The resulting approximations have a probabilistic guarantee that be obtained, 
  -- a priori, from the getRSE(int, double, boolean, long) function.
  --
  -- If the sketch is empty this returns 'Nothing'.
  -> m (Maybe [Double])
cumulativeDistributionFunction this splitPoints = do
  buckets <- getPMForCDF this splitPoints
  isEmpty <- getIsEmpty this
  if isEmpty
    then pure Nothing
    else do
      let numBuckets = length splitPoints + 1
      n <- count this
      pure $ Just $ (/ fromIntegral n) . fromIntegral <$> buckets

rankAccuracy :: ReqSketch s -> RankAccuracy
rankAccuracy = rankAccuracySetting

-- | Returns an a priori estimate of relative standard error (RSE, expressed as a number in [0,1]). Derived from Lemma 12 in https://arxiv.org/abs/2004.01668v2, but the constant factors were modified based on empirical measurements.
relativeStandardError
  :: Int
  -- ^ k - the given value of k
  -> Double
  -- ^ rank - the given normalized rank, a number in [0,1].
  -> RankAccuracy
  -> Word64
  -- ^ totalN - an estimate of the total number of items submitted to the sketch.
  -> Double
  -- ^ an a priori estimate of relative standard error (RSE, expressed as a number in [0,1]).
relativeStandardError k rank_ hra = getRankUB k 2 rank_ 1 isHra
  where
    isHra = case hra of
      HighRanksAreAccurate -> True
      _ -> False

-- | Gets the smallest value seen by this sketch
minimum :: PrimMonad m => ReqSketch (PrimState m) -> m Double
minimum = readURef . minValue

-- | Gets the largest value seen by this sketch
maximum :: PrimMonad m => ReqSketch (PrimState m) -> m Double
maximum = readURef . maxValue

-- | Returns the approximate count of items satisfying the criterion set in the ReqSketch 'criterion' field.
countWithCriterion :: (PrimMonad m, s ~ PrimState m) => ReqSketch s -> Double -> m Word64
countWithCriterion s value = fromIntegral <$> do
  empty <- null s
  if empty
    then pure 0
    else do
      compactors <- getCompactors s
      let go !accum compactor = do
            let wt = (1 `shiftL` fromIntegral (Compactor.getLgWeight compactor)) :: Word64
            buf <- Compactor.getBuffer compactor
            count_ <- DoubleBuffer.getCountWithCriterion buf value (criterion s)
            pure (accum + (fromIntegral count_ * wt))
      Vector.foldM go 0 compactors

sum :: (PrimMonad m) => ReqSketch (PrimState m) -> m Double
sum = readURef . sumValue

-- | Returns an approximation to the Probability Mass Function (PMF) of the input stream given a set of splitPoints (values).
-- The resulting approximations have a probabilistic guarantee that be obtained, a priori, from the getRSE(int, double, boolean, long) function.
--
-- If the sketch is empty this returns an empty list.
probabilityMassFunction
  :: (PrimMonad m)
  => ReqSketch (PrimState m)
  -> [Double]
  -- ^ splitPoints - an array of m unique, monotonically increasing double values that divide 
  -- the real number line into m+1 consecutive disjoint intervals. The definition of an "interval" 
  -- is inclusive of the left splitPoint (or minimum value) and exclusive of the right splitPoint, 
  -- with the exception that the last interval will include the maximum value. It is not necessary 
  -- to include either the min or max values in these splitpoints.
  -> m [Double]
  -- ^ An array of m+1 doubles each of which is an approximation to the fraction of 
  -- the input stream values (the mass) that fall into one of those intervals. 
  -- The definition of an "interval" is inclusive of the left splitPoint and exclusive 
  -- of the right splitPoint, with the exception that the last interval will 
  -- include maximum value.
probabilityMassFunction this splitPoints = do
  isEmpty <- getIsEmpty this
  if isEmpty
     then pure []
     else do
       let numBuckets = length splitPoints + 1
       buckets <- fmap fromIntegral <$> getPMForCDF this splitPoints
       total <- fromIntegral <$> count this
       let computeProb (0, bucket) = bucket / total
           computeProb (i, bucket) = (prevBucket + bucket) / total
             where prevBucket = buckets !! i - 1
           probs = computeProb <$> zip [0..] buckets
       pure probs

-- | Gets the approximate quantile of the given normalized rank based on the lteq criterion.
quantile
  :: (PrimMonad m)
  => ReqSketch (PrimState m)
  -> Double
  -- ^ normRank - the given normalized rank
  -> m Double
  -- ^ the approximate quantile given the normalized rank.
quantile this normRank = do
  isEmpty <- getIsEmpty this
  if isEmpty
     then pure (0/0)
     else do
       when (normRank < 0 || normRank > 1.0) $
         error $ "Normalized rank must be in the range [0.0, 1.0]: " ++ show normRank
       currAuxiliary <- getAux this
       when (isNothing currAuxiliary) $ do
         total <- count this
         retainedItems <- retainedItemCount this
         compactors <- getCompactors this
         newAuxiliary <- Auxiliary.mkAuxiliary (rankAccuracySetting this) total retainedItems compactors
         writeMutVar (aux this) (Just newAuxiliary)
       mAuxiliary <- getAux this
       case mAuxiliary of
         Just auxiliary -> pure $! Auxiliary.getQuantile auxiliary normRank $ criterion this
         Nothing -> error "invariant violated: aux is not set"

-- | Gets an array of quantiles that correspond to the given array of normalized ranks.
quantiles
  :: (PrimMonad m)
  => ReqSketch (PrimState m)
  -> [Double]
  -- ^ normRanks - the given array of normalized ranks.
  -> m [Double]
  -- ^ the array of quantiles that correspond to the given array of normalized ranks.
quantiles this normRanks = do
  isEmpty <- getIsEmpty this
  if isEmpty
     then pure []
     else mapM (quantile this) normRanks

-- | Computes the normalized rank of the given value in the stream. The normalized rank is the fraction of values less than the given value; or if lteq is true, the fraction of values less than or equal to the given value.
rank :: (PrimMonad m)
  => ReqSketch (PrimState m)
  -> Double
  -- ^ value - the given value
  -> m Double
  -- ^ the normalized rank of the given value in the stream.
rank s value = do
  isEmpty <- null s
  if isEmpty
    then pure (0 / 0) -- NaN
    else do
      nnCount <- countWithCriterion s value
      total <- readURef $ totalN s
      pure (fromIntegral nnCount / fromIntegral total)


-- getRankLB k levels rank numStdDev hra totalN = if exactRank k levels rank hra totalN

-- | Returns an approximate lower bound rank of the given normalized rank.
rankLowerBound
  :: (PrimMonad m)
  => ReqSketch (PrimState m)
  -> Double
  -- ^ rank - the given rank, a value between 0 and 1.0.
  -> Int
  -- ^ numStdDev - the number of standard deviations. Must be 1, 2, or 3.
  -> m Double
  -- ^ an approximate lower bound rank.
rankLowerBound this rank numStdDev = do
  numLevels <- getNumLevels this
  let k = fromIntegral $ getK this
  total <- count this
  pure $ getRankLB k numLevels rank numStdDev (rankAccuracySetting this == HighRanksAreAccurate) total

-- | Gets an array of normalized ranks that correspond to the given array of values.
-- TODO, make it ifaster
ranks :: (PrimMonad m, s ~ PrimState m) => ReqSketch s -> [Double] -> m [Double]
ranks s values = mapM (rank s) values

-- | Returns an approximate upper bound rank of the given rank.
rankUpperBound
  :: (PrimMonad m)
  => ReqSketch (PrimState m)
  -> Double
  -- ^ rank - the given rank, a value between 0 and 1.0.
  -> Int
  -- ^ numStdDev - the number of standard deviations. Must be 1, 2, or 3.
  -> m Double
  -- ^ an approximate upper bound rank.
rankUpperBound this rank numStdDev= do
  numLevels <- getNumLevels this
  let k = fromIntegral $ getK this
  total <- count this
  pure $ getRankUB k numLevels rank numStdDev (rankAccuracySetting this == HighRanksAreAccurate) total

-- | Returns true if this sketch is empty.
null :: (PrimMonad m) => ReqSketch (PrimState m) -> m Bool
null = fmap (== 0) . readURef . totalN

-- | Returns true if this sketch is in estimation mode.
isEstimationMode :: PrimMonad m => ReqSketch (PrimState m) -> m Bool
isEstimationMode = fmap (> 1) . getNumLevels

-- | Returns the current comparison criterion.
isLessThanOrEqual :: ReqSketch s -> Bool
isLessThanOrEqual s = case criterion s of
  (:<) -> False
  (:<=) -> True

computeMaxNominalSize :: PrimMonad m => ReqSketch (PrimState m) -> m Int
computeMaxNominalSize this = do
  compactors <- getCompactors this
  Vector.foldM countNominalCapacity 0 compactors
  where
    countNominalCapacity acc compactor = do
      nominalCapacity <- Compactor.getNominalCapacity compactor
      pure $ nominalCapacity + acc

grow :: (PrimMonad m) => ReqSketch (PrimState m) -> m ()
grow this = do
  lgWeight <- fromIntegral <$> getNumLevels this
  let rankAccuracy = rankAccuracySetting this
      sectionSize = getK this
  newCompactor <- Compactor.mkReqCompactor (sketchRng this) lgWeight rankAccuracy sectionSize
  modifyMutVar' (compactors this) (`Vector.snoc` newCompactor)
  maxNominalCapacity <- computeMaxNominalSize this
  writeURef (maxNominalCapacitiesSize this) maxNominalCapacity

compress :: (PrimMonad m) => ReqSketch (PrimState m) -> m ()
compress this = do
  compactors <- getCompactors this
  let compressionStep height compactor = do
        buffSize <- DoubleBuffer.getCount =<< Compactor.getBuffer compactor
        nominalCapacity <- Compactor.getNominalCapacity compactor
        when (buffSize >= nominalCapacity) $ do
          numLevels <- getNumLevels this
          when (height + 1 >= numLevels) $ do
            grow this
          compactors' <- getCompactors this
          cReturn <- Compactor.compact compactor
          let topCompactor = compactors' ! (height + 1)
          buff <- Compactor.getBuffer topCompactor
          DoubleBuffer.mergeSortIn buff $ Compactor.crDoubleBuffer cReturn
          modifyURef (retainedItems this) (+ Compactor.crDeltaRetItems cReturn)
          modifyURef (maxNominalCapacitiesSize this) (+ Compactor.crDeltaNominalSize cReturn)
  imapM_ compressionStep compactors
  writeMutVar (aux this) Nothing

-- | Merge other sketch into this one.
merge
  :: (PrimMonad m, s ~ PrimState m)
  => ReqSketch s
  -> ReqSketch s
  -> m (ReqSketch s)
merge this other = do
  otherIsEmpty <- getIsEmpty other
  unless otherIsEmpty $ do
    let rankAccuracy = rankAccuracySetting this
        otherRankAccuracy = rankAccuracySetting other
    when (rankAccuracy /= otherRankAccuracy) $
      error "Both sketches must have the same HighRankAccuracy setting."
    -- update total
    otherN <- count other
    modifyURef (totalN this) (+ otherN)
    -- update the min and max values
    thisMin <- minimum this
    thisMax <- maximum this
    otherMin <- minimum other
    otherMax <- maximum other
    when (isNaN thisMin || otherMin < thisMin) $ do
      writeURef (minValue this) otherMin
    when (isNaN thisMax || otherMax < thisMax) $ do
      writeURef (maxValue this) otherMax
    -- grow until this has at least as many compactors as other
    numRequiredCompactors <- getNumLevels other
    growUntil numRequiredCompactors
    -- merge the items in all height compactors
    thisCompactors <- getCompactors this
    otherCompactors <- getCompactors other
    Vector.zipWithM_ Compactor.merge thisCompactors otherCompactors
    -- update state
    maxNominalCapacity <- computeMaxNominalSize this
    totalRetainedItems <- computeTotalRetainedItems this
    writeURef (maxNominalCapacitiesSize this) maxNominalCapacity
    writeURef (retainedItems this) totalRetainedItems
    -- compress and check invariants
    when (totalRetainedItems >= maxNominalCapacity) $ do
      compress this
    maxNominalCapacity' <- readURef $ maxNominalCapacitiesSize this
    totalRetainedItems' <- readURef $ retainedItems this
    assert (totalRetainedItems' < maxNominalCapacity') $
      writeMutVar (aux this) Nothing
  pure this
  where
    growUntil target = do
      numCompactors <- getNumLevels this
      when (numCompactors < target) $
        grow this

-- | Updates this sketch with the given item.
insert :: (PrimMonad m) => ReqSketch (PrimState m) -> Double -> m ()
insert this item = do
  unless (isNaN item) $ do
    isEmpty <- getIsEmpty this
    if isEmpty
       then do
         writeURef (minValue this) item
         writeURef (maxValue this) item
       else do
         min_ <- minimum this
         max_ <- maximum this
         when (item < min_) $ writeURef (minValue this) item
         when (item > max_) $ writeURef (maxValue this) item
    compactor <- Vector.head <$> getCompactors this
    buff <- Compactor.getBuffer compactor
    DoubleBuffer.append buff item
    modifyURef (retainedItems this) (+1)
    modifyURef (totalN this) (+1)
    modifyURef (sumValue this) (+ item)
    retItems <- retainedItemCount this
    maxNominalCapacity <- getMaxNominalCapacity this
    when (retItems >= maxNominalCapacity) $ do
      DoubleBuffer.sort buff
      compress this
    writeMutVar (aux this) Nothing

-- Private pure bits

getRankLB :: Int -> Int -> Double -> Int -> Bool -> Word64 -> Double
getRankLB k levels rank numStdDev hra totalN = if exactRank k levels rank hra totalN
  then rank
  else max lbRel lbFix
  where
    relative = relRseFactor / fromIntegral k * (if hra then 1.0 - rank else rank)
    fixed = fixRseFactor / fromIntegral k
    lbRel = rank - fromIntegral numStdDev * relative
    lbFix = rank - fromIntegral numStdDev * fixed

getRankUB :: Int -> Int -> Double -> Int -> Bool -> Word64 -> Double
getRankUB k levels rank numStdDev hra totalN = if exactRank k levels rank hra totalN
  then rank
  else min ubRel ubFix
  where
    relative = relRseFactor / fromIntegral k * (if hra then 1.0 - rank else rank)
    fixed = fixRseFactor / fromIntegral k
    ubRel = rank + fromIntegral numStdDev * relative
    ubFix = rank + fromIntegral numStdDev * fixed

exactRank :: Int -> Int -> Double -> Bool -> Word64 -> Bool
exactRank k levels rank hra totalN = (levels == 1 || fromIntegral totalN <= baseCap) || (hra && rank >= 1.0 - exactRankThresh || not hra && rank <= exactRankThresh)
  where
    baseCap = k * initNumberOfSections
    exactRankThresh :: Double
    exactRankThresh = fromIntegral baseCap / fromIntegral totalN
