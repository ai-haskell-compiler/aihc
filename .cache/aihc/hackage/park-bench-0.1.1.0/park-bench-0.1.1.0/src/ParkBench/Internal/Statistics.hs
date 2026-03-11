-- | Simple statistics code.
module ParkBench.Internal.Statistics
  ( Timed (..),
    Estimate (..),
    initialEstimate,
    updateEstimate,
    elapsed,
    stdev,
    variance,
    goodness,
    Roll (..),
  )
where

import ParkBench.Internal.Prelude

-- | A value that took a certan time to compute.
data Timed a = Timed
  { nanoseconds :: {-# UNPACK #-} !Rational,
    value :: !a
  }
  deriving stock (Functor, Show)

instance Monoid a => Monoid (Timed a) where
  mempty = Timed 0 mempty
  mappend = (<>)

instance Semigroup a => Semigroup (Timed a) where
  Timed n0 x0 <> Timed n1 x1 =
    Timed (n0 + n1) (x0 <> x1)

data Estimate a = Estimate
  { kvariance :: {-# UNPACK #-} !Rational,
    mean :: {-# UNPACK #-} !(Timed a),
    samples :: {-# UNPACK #-} !Word64
  }
  deriving stock (Functor, Show)

-- | The total elapsed time of an estimate, in nanoseconds.
elapsed :: Estimate a -> Rational
elapsed Estimate {mean, samples} =
  w2r samples * nanoseconds mean

-- | The standard deviation of an estimate.
stdev :: Estimate a -> Double
stdev =
  sqrt . (fromRational @Double) . variance

-- | The variance of an estimate.
variance :: Estimate a -> Rational
variance (Estimate kvariance _ samples) =
  if samples == 1
    then 0
    else kvariance / w2r (samples - 1)

-- | The "goodness" of an estimate, which is just how large its standard deviation is, relative to its mean.
--
-- Smaller is better, and the smallest possible value is 0.
goodness :: Estimate a -> Double
goodness e =
  stdev e / r2d (nanoseconds (mean e))

-- | @initialEstimate v@ creates an estimate per thing-that-took-time @v@ that was a run of 1 iteration.
initialEstimate :: Timed a -> Estimate a
initialEstimate mean =
  Estimate
    { kvariance = 0,
      mean,
      samples = 1
    }

-- | @updateEstimate n v e@ updates estimate @e@ per thing-that-took-time @v@ that was a run of @n@ iterations.
updateEstimate :: Roll a => Word64 -> Timed a -> Estimate a -> Estimate a
updateEstimate n (Timed tn1 value1) (Estimate kvariance0 (Timed mean0 value0) samples0) =
  Estimate
    { kvariance = kvariance0 + nr * (t1 - mean0) * (t1 - mean1),
      mean = Timed mean1 value',
      samples = samples1
    }
  where
    mean1 = rollmean mean0 tn1
    samples1 = samples0 + n
    t1 = tn1 / nr
    value' = roll rollmean value0 value1
    rollmean u0 u1 = u0 + ((u1 - nr * u0) / w2r samples1)
    nr = w2r n

class Roll a where
  roll :: (Rational -> Rational -> Rational) -> a -> a -> a
