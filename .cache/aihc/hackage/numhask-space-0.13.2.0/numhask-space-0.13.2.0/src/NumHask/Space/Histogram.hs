{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- | A histogram, if you squint, is a series of contiguous 'Range's, annotated with values.
module NumHask.Space.Histogram
  ( Histogram (..),
    emptyHistogram,
    DealOvers (..),
    fill,
    cutI,
    regular,
    makeRects,
    regularQuantiles,
    quantileFold,
    freq,
    average,
    quantiles,
    quantile,
  )
where

import Data.Map qualified as Map
import Data.TDigest qualified as TD
import Data.Vector qualified as V
import NumHask.Prelude
import NumHask.Space.Range
import NumHask.Space.Rect
import NumHask.Space.Types

-- $setup
-- >>> :m -Prelude
-- >>> :set -XRebindableSyntax
-- >>> import NumHask.Prelude
-- >>> import NumHask.Space

-- | This Histogram is a list of contiguous boundaries (a boundary being the lower edge of one bucket and the upper edge of another), and a value (usually a count) for each bucket, represented here as a map
--
-- Overs and Unders are contained in key = 0 and key = length cuts
-- Intervals are defined as (l,u]
data Histogram = Histogram
  { cuts :: V.Vector Double, -- bucket boundaries
    values :: Map.Map Int Double -- bucket counts
  }
  deriving (Show, Eq)

-- | A histogram with no cuts nor data.
emptyHistogram :: Histogram
emptyHistogram = Histogram V.empty Map.empty

-- | Whether or not to ignore unders and overs.  If overs and unders are dealt with, IncludeOvers supplies an assumed width for the outer buckets.
data DealOvers = IgnoreOvers | IncludeOvers Double

-- | Fill a Histogram using pre-specified cuts
--
-- >>> fill [0,50,100] [0..99]
-- Histogram {cuts = [0.0,50.0,100.0], values = fromList [(1,50.0),(2,50.0)]}
fill :: (Foldable f) => [Double] -> f Double -> Histogram
fill cs xs =
  Histogram
    (V.fromList cs)
    (foldl' (\x a -> Map.insertWith (+) (cutI (V.fromList cs) a) 1 x) Map.empty xs)

-- | find the index of the bucket the value is contained in.
cutI :: (Ord a) => V.Vector a -> a -> Int
cutI cs a = go (Range zero (V.length cs))
  where
    go (Range l u) =
      let k = (u + l) `div` 2
       in case compare a (cs V.! k) of
            EQ -> k + 1
            LT -> bool (go (Range l k)) k (l == k)
            GT ->
              bool
                ( case compare a (cs V.! (k + one)) of
                    EQ -> k + 2
                    LT -> k + 1
                    GT -> go (Range k u)
                )
                (k + 1)
                (k >= u - one)

-- | Make a histogram using n equally spaced cuts over the entire range of the data
--
-- >>> regular 4 [0..100]
-- Histogram {cuts = [0.0,25.0,50.0,75.0,100.0], values = fromList [(1,25.0),(2,25.0),(3,25.0),(4,25.0),(5,1.0)]}
regular :: Int -> [Double] -> Histogram
regular _ [] = emptyHistogram
regular n xs = fill cs xs
  where
    cs = grid OuterPos (unsafeSpace1 xs :: Range Double) n

-- | Transform a Histogram to Rects
--
-- >>> makeRects IgnoreOvers (regular 4 [0..100])
-- [Rect 0.0 25.0 0.0 0.25,Rect 25.0 50.0 0.0 0.25,Rect 50.0 75.0 0.0 0.25,Rect 75.0 100.0 0.0 0.25]
makeRects :: DealOvers -> Histogram -> [Rect Double]
makeRects o (Histogram cs counts) = V.toList $ V.zipWith3 (\x z w' -> Rect x z zero w') x z w'
  where
    w =
      V.zipWith
        (/)
        ((\x' -> Map.findWithDefault 0 x' counts) <$> V.enumFromN f (l - f + one))
        (V.zipWith (-) z x)
    f = case o of
      IgnoreOvers -> one
      IncludeOvers _ -> zero
    l = case o of
      IgnoreOvers -> length cs - one
      IncludeOvers _ -> length cs
    w' = (/ sum w) <$> w
    x = case o of
      IgnoreOvers -> cs
      IncludeOvers outw ->
        V.singleton (V.head cs - outw)
          <> cs
          <> V.singleton (V.last cs + outw)
    z = V.drop one x

-- | approx regular n-quantiles
--
-- >>> regularQuantiles 4 [0..100]
-- [0.0,24.75,50.0,75.25,100.0]
regularQuantiles :: Double -> [Double] -> [Double]
regularQuantiles n xs = quantileFold qs xs
  where
    qs = ((1 / n) *) <$> [0 .. n]

-- | one-pass approximate quantiles fold
quantileFold :: [Double] -> [Double] -> [Double]
quantileFold qs xs = done $ foldl' step begin xs
  where
    step x a = TD.insert a x
    begin = TD.tdigest ([] :: [Double]) :: TD.TDigest 25
    done x = fromMaybe (0 / 0) . (`TD.quantile` TD.compress x) <$> qs

-- | normalize a histogram
--
-- > \h -> sum (values $ freq h) == one
--
-- >>> freq $ fill [0,50,100] [0..99]
-- Histogram {cuts = [0.0,50.0,100.0], values = fromList [(1,0.5),(2,0.5)]}
freq :: Histogram -> Histogram
freq (Histogram cs vs) = Histogram cs $ Map.map (* recip (sum vs)) vs

-- | average
--
-- >>> average [0..1000]
-- 500.0
average :: (Foldable f) => f Double -> Double
average xs = sum xs / fromIntegral (length xs)

-- | Regularly spaced (approx) quantiles
--
-- >>> quantiles 5 [1..1000]
-- [1.0,200.5,400.5,600.5000000000001,800.5,1000.0]
quantiles :: (Foldable f) => Int -> f Double -> [Double]
quantiles n xs =
  ( \x ->
      fromMaybe 0 $
        TD.quantile x (TD.tdigest xs :: TD.TDigest 25)
  )
    . (/ fromIntegral n)
    . fromIntegral
    <$> [0 .. n]

-- | single (approx) quantile
--
-- >>> quantile 0.1 [1..1000]
-- 100.5
quantile :: (Foldable f) => Double -> f Double -> Double
quantile p xs = fromMaybe 0 $ TD.quantile p (TD.tdigest xs :: TD.TDigest 25)
