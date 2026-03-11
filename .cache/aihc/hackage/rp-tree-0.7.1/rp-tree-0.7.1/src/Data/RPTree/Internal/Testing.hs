{-# options_ghc -Wno-unused-imports #-}
module Data.RPTree.Internal.Testing where

import Control.Monad.IO.Class (MonadIO(..))
import GHC.Word (Word8, Word64)

-- conduit
import qualified Data.Conduit as C (ConduitT, runConduit, yield, await, transPipe)
import Data.Conduit ((.|))
import qualified Data.Conduit.Combinators as C (map, mapM, scanl, scanlM, last, print)
import qualified Data.Conduit.List as C (chunksOf, unfold, unfoldM)
-- splitmix
import System.Random.SplitMix (initSMGen, unseedSMGen)
-- splitmix-distributions
import System.Random.SplitMix.Distributions (GenT)

import Data.RPTree.Internal (SVector, fromListSv, DVector, fromListDv)
import Data.RPTree.Gen (dense, sparse, normal2, normalSparse2, normalDense2)
import Data.RPTree.Conduit (dataSource)

data BenchConfig = BenchConfig {
  bcDescription :: String
  , bcMaxTreeDepth :: Int
  , bcMinLeafSize :: Int
  , bcNumTrees :: Int
  , bcChunkSize :: Int
  , bcNZDensity :: Double
  , bcVectorDim :: Int
  , bcDataSize :: Int
  , bcNumQueryPoints :: Int
                               } deriving (Show)

randSeed :: MonadIO m => m Word64
randSeed = liftIO (fst . unseedSMGen <$> initSMGen)


-- | binary mixture of isotropic Gaussian rvs
datD :: Monad m =>
        Int -- ^ number of data points
     -> Int -- ^ vector dimension
     -> C.ConduitT i (DVector Double) (GenT m) ()
datD n d = dataSource n $ normalDense2 d

-- | binary mixture of isotropic Gaussian rvs with sparse components
datS :: Monad m =>
        Int -- ^ number of data points
     -> Int -- ^ vector dimension
     -> Double -- ^ nonzero density
     -> C.ConduitT i (SVector Double) (GenT m) ()
datS n d pnz = dataSource n $ normalSparse2 pnz d
