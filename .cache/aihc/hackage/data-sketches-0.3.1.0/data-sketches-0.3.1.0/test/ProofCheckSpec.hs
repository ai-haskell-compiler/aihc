{-# LANGUAGE TypeApplications #-}
module ProofCheckSpec where

import Statistics.Quantile
import Test.Hspec
import qualified Data.Vector as V
import DataSketches.Quantiles.RelativeErrorQuantile (mkReqSketch, RankAccuracy (HighRanksAreAccurate), insert)
import qualified DataSketches.Quantiles.RelativeErrorQuantile as SK
import Test.QuickCheck
import Control.Monad.ST
import Control.Monad.Primitive
import Control.Monad
import Debug.Trace

sampleData :: V.Vector Double
sampleData = V.fromList [1..200]

{-
compareRealToApproximate :: IO ()
compareRealToApproximate = do
  let realQuantiles = quantiles spss [50, 90, 95, 99] 100 sampleData
  -- print realQuantiles
  sk <- mkReqSketch 6 HighRanksAreAccurate
  mapM_ (update sk) sampleData
  let ranks = [0.01, 0.02 .. 0.99]
      rankInts = map (floor . (* 100)) ranks
  -- print =<< SK.quantiles sk ranks
  upperLowerBounds <- mapM (upperAndLowerBound sk) ranks
  forM_ upperAndLowerBound $ \(actualRank, l, u) -> do
    let actual = quantile spss actualRank 100 sampleData
    assert (actual >= l && actual <= u)
-}

upperAndLowerBound :: (PrimMonad m) => SK.ReqSketch (Control.Monad.Primitive.PrimState m)
  -> Double -> m (Double, Double, Double)
upperAndLowerBound sk r = do
  l <- SK.rankLowerBound sk r 3
  u <- SK.rankUpperBound sk r 3
  pure (r, l, u)

spec :: Spec
spec = do
  let sampleInputGen = arbitrary `suchThat` (all (\x -> not (isNaN x) && not (isInfinite x)) . getNonEmpty)

  specify "quantile ranks should fall within advertised upper and lower bounds" $
    property $ forAll sampleInputGen $ \(NonEmpty doubles) -> runST $ do
      let sampleData = V.fromList doubles
      sk <- mkReqSketch 6 HighRanksAreAccurate
      mapM_ (insert sk) sampleData
      let ranks = [0.01, 0.02 .. 0.99]
          rankInts = map (floor . (* 100)) ranks
      upperLowerBounds <- mapM (upperAndLowerBound sk) ranks
      ranksInAdvertisedRanges <- forM upperLowerBounds $ \(actualRank, l, u) -> do
        pure (actualRank >= l && actualRank <= u)
      pure $! and ranksInAdvertisedRanges

  {- TODO
  specify "values at quantiles should be close to real quantile" $
    property $ forAll sampleInputGen $ \(NonEmpty doubles) -> runST $ do
      let sampleData = V.fromList doubles
      sk <- mkReqSketch 6 HighRanksAreAccurate
      mapM_ (update sk) sampleData
      let ranks = [0.01, 0.02 .. 0.99]
          rankInts = map (floor . (* 100)) ranks
      upperLowerBounds <- mapM (upperAndLowerBound sk) ranks
      valuesInAdvertisedRanges <- forM upperLowerBounds $ \(actualRank, l, u) -> do
        let actual = quantile spss (floor (actualRank * 100)) 100 sampleData
        estimatedL <- SK.quantile sk l
        estimatedH <- SK.quantile sk u
        traceShow (estimatedL, actual, estimatedH) $ pure
          ((actual > estimatedL || actual `approximatelyEqual` estimatedL) && (actual < estimatedH || actual `approximatelyEqual` estimatedH))
      pure $! and valuesInAdvertisedRanges
  -}

approximatelyEqual :: Double -> Double -> Bool
approximatelyEqual x y = (abs (x - y) / max (abs x) (abs y)) < eEqD

eEqD :: Double
eEqD = 1e-5