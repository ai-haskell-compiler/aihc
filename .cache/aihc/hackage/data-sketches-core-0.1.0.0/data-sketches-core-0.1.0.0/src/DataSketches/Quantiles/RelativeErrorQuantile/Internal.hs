{-# LANGUAGE DeriveGeneric #-}
module DataSketches.Quantiles.RelativeErrorQuantile.Internal where

import Data.Primitive (MutVar, readMutVar)
import Data.Word
import qualified Data.Vector as Vector
import GHC.Generics
import System.Random.MWC (Gen)

import DataSketches.Core.Snapshot
import DataSketches.Quantiles.RelativeErrorQuantile.Types
import DataSketches.Core.Internal.URef (URef, readURef)
import DataSketches.Quantiles.RelativeErrorQuantile.Internal.Auxiliary (ReqAuxiliary)
import DataSketches.Quantiles.RelativeErrorQuantile.Internal.Compactor (ReqCompactor)
import Control.DeepSeq (NFData, rnf)
import Control.Monad.Primitive (PrimMonad (PrimState))
import qualified DataSketches.Quantiles.RelativeErrorQuantile.Internal.Compactor as Compactor
import qualified DataSketches.Quantiles.RelativeErrorQuantile.Internal.DoubleBuffer as DoubleBuffer
import qualified DataSketches.Quantiles.RelativeErrorQuantile.Internal.Auxiliary as Auxiliary
import Control.Exception (Exception)


{- |
This Relative Error Quantiles Sketch is the Haskell implementation based on the paper
"Relative Error Streaming Quantiles", https://arxiv.org/abs/2004.01668, and loosely derived from
a Python prototype written by Pavel Vesely, ported from the Java equivalent.

This implementation differs from the algorithm described in the paper in the following:

The algorithm requires no upper bound on the stream length.
Instead, each relative-compactor counts the number of compaction operations performed
so far (via variable state). Initially, the relative-compactor starts with INIT_NUMBER_OF_SECTIONS.
Each time the number of compactions (variable state) exceeds 2^{numSections - 1}, we double
numSections. Note that after merging the sketch with another one variable state may not correspond
to the number of compactions performed at a particular level, however, since the state variable
never exceeds the number of compactions, the guarantees of the sketch remain valid.

The size of each section (variable k and sectionSize in the code and parameter k in
the paper) is initialized with a value set by the user via variable k.
When the number of sections doubles, we decrease sectionSize by a factor of sqrt(2).
This is applied at each level separately. Thus, when we double the number of sections, the
nominal compactor size increases by a factor of approx. sqrt(2) (+/- rounding).

The merge operation here does not perform "special compactions", which are used in the paper
to allow for a tight mathematical analysis of the sketch.

This implementation provides a number of capabilities not discussed in the paper or provided
in the Python prototype.

The Python prototype only implemented high accuracy for low ranks. This implementation
provides the user with the ability to choose either high rank accuracy or low rank accuracy at
the time of sketch construction.

- The Python prototype only implemented a comparison criterion of "<". This implementation
allows the user to switch back and forth between the "<=" criterion and the "<=" criterion.
-}
data ReqSketch s = ReqSketch
  { k :: !Word32
  , rankAccuracySetting :: !RankAccuracy
  , criterion :: !Criterion
  , sketchRng :: {-# UNPACK #-} !(Gen s)
  , totalN :: {-# UNPACK #-} !(URef s Word64)
  , minValue :: {-# UNPACK #-} !(URef s Double)
  , maxValue :: {-# UNPACK #-} !(URef s Double)
  , sumValue :: {-# UNPACK #-} !(URef s Double)
  , retainedItems :: {-# UNPACK #-} !(URef s Int)
  , maxNominalCapacitiesSize :: {-# UNPACK #-} !(URef s Int)
  , aux :: {-# UNPACK #-} !(MutVar s (Maybe ReqAuxiliary))
  , compactors :: {-# UNPACK #-} !(MutVar s (Vector.Vector (ReqCompactor s)))
  } deriving (Generic)

instance NFData (ReqSketch s) where
  rnf !rs = ()

data ReqSketchSnapshot = ReqSketchSnapshot
    { snapshotRankAccuracySetting :: !RankAccuracy
    , snapshotCriterion :: !Criterion
    , snapshotTotalN :: !Word64
    , snapshotMinValue :: !Double
    , snapshotMaxValue :: !Double
    , snapshotRetainedItems :: !Int
    , snapshotMaxNominalCapacitiesSize :: !Int
    -- , aux :: !(MutVar s (Maybe ()))
    , snapshotCompactors :: !(Vector.Vector (Snapshot ReqCompactor))
    } deriving Show

instance TakeSnapshot ReqSketch where
  type Snapshot ReqSketch = ReqSketchSnapshot 
  takeSnapshot ReqSketch{..} = ReqSketchSnapshot rankAccuracySetting criterion
    <$> readURef totalN
    <*> readURef minValue
    <*> readURef maxValue
    <*> readURef retainedItems
    <*> readURef maxNominalCapacitiesSize
    <*> (readMutVar compactors >>= mapM takeSnapshot)

getCompactors :: PrimMonad m => ReqSketch (PrimState m) -> m (Vector.Vector (ReqCompactor (PrimState m)))
getCompactors = readMutVar . compactors

computeTotalRetainedItems :: PrimMonad m => ReqSketch (PrimState m) -> m Int
computeTotalRetainedItems this = do
  compactors <- getCompactors this
  Vector.foldM countBuffer 0 compactors
  where
    countBuffer acc compactor = do
      buff <- Compactor.getBuffer compactor
      buffSize <- DoubleBuffer.getCount buff
      pure $ buffSize + acc

retainedItemCount :: PrimMonad m => ReqSketch (PrimState m) -> m Int
retainedItemCount = readURef . retainedItems

-- | Get the total number of items inserted into the sketch
count :: PrimMonad m => ReqSketch (PrimState m) -> m Word64
count = readURef . totalN

mkAuxiliaryFromReqSketch :: PrimMonad m => ReqSketch (PrimState m) -> m ReqAuxiliary
mkAuxiliaryFromReqSketch this = do
  total <- count this
  retainedItems <- retainedItemCount this
  compactors <- getCompactors this
  Auxiliary.mkAuxiliary (rankAccuracySetting this) total retainedItems compactors

data CumulativeDistributionInvariants
  = CumulativeDistributionInvariantsSplitsAreEmpty
  | CumulativeDistributionInvariantsSplitsAreNotFinite
  | CumulativeDistributionInvariantsSplitsAreNotUniqueAndMontonicallyIncreasing
  deriving (Show, Eq)

instance Exception CumulativeDistributionInvariants
