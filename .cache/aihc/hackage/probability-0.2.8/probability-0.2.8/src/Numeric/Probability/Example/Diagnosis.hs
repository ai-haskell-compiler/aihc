{- |
You take part in a screening test for a disease
that you have with a probability 'pDisease'.
The test can fail in two ways:
If you are ill,
the test says with probability 'pFalseNegative' that you are healthy.
If you are healthy,
it says with probability 'pFalsePositive' that you are ill.

Now consider the test is positive -
what is the probability that you are indeed ill?
-}
module Numeric.Probability.Example.Diagnosis where

import qualified Numeric.Probability.Distribution as Dist
import Numeric.Probability.Distribution ((??), (?=<<), )


type Probability = Rational
type Dist a = Dist.T Probability a


data State = Healthy | Ill
   deriving (Eq, Ord, Show, Enum)

data Finding = Negative | Positive
   deriving (Eq, Ord, Show, Enum)


pDisease, pFalseNegative, pFalsePositive :: Probability
pDisease = 0.001
pFalseNegative = 0.01
pFalsePositive = 0.01


dist :: Dist (State, Finding)
dist =
   do s <- Dist.choose pDisease Ill Healthy
      f <- case s of
              Ill     -> Dist.choose pFalseNegative Negative Positive
              Healthy -> Dist.choose pFalsePositive Positive Negative
      return (s,f)


{- |
Alternative way for computing the distribution.
It is usually more efficient because we do not need to switch on the healthy state.
-}
distAlt :: Dist (State, Finding)
distAlt =
   do (s,fr) <-
          Dist.choose pDisease
             (Ill,     Dist.choose pFalseNegative Negative Positive)
             (Healthy, Dist.choose pFalsePositive Positive Negative)
      f <- fr
      return (s,f)


p :: Probability
p = (Dist.just Ill . fst) ?? (Dist.just Positive . snd) ?=<< dist
