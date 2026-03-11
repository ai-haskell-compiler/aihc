module Test.Utility where

import qualified Numeric.NonNegative.Wrapper as NonNeg


type TimeDiff = NonNeg.Int

timeToDouble :: TimeDiff -> NonNeg.Double
timeToDouble = fromIntegral

makeFracTime :: (TimeDiff, TimeDiff) -> NonNeg.Double
makeFracTime (n,d) =
   timeToDouble n / (timeToDouble d + 1)
