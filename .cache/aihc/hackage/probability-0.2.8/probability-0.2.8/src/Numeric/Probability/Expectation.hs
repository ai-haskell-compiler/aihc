module Numeric.Probability.Expectation where

import qualified Numeric.Probability.Distribution as Dist
import qualified Numeric.Probability.Percentage as Probability

-- TO DO: generalize Float to arbitrary Num type
--
class ToFloat a where
  toFloat :: a -> Float

instance ToFloat Float   where toFloat = id
instance ToFloat Int     where toFloat = fromIntegral
instance ToFloat Integer where toFloat = fromIntegral
instance ToFloat Probability.T
                         where toFloat (Probability.Cons x) = x

class FromFloat a where
  fromFloat :: Float -> a

instance FromFloat Float   where fromFloat = id
instance FromFloat Int     where fromFloat = round
instance FromFloat Integer where fromFloat = round

-- expected :: ToFloat a => Prob.Dist a -> Float
-- expected = sum . map (\(x,p)->toFloat x*p) . Dist.decons

class Expected a where
  expected :: a -> Float

-- instance ToFloat a => Expected a where
--   expected = toFloat
instance Expected Float   where expected = id
instance Expected Int     where expected = toFloat
instance Expected Integer where expected = toFloat

instance Expected a => Expected [a] where
  expected = Dist.expected . Dist.uniform . map expected
--  expected xs = sum (map expected xs) / toFloat (length xs)

floatDist :: (ToFloat prob, Expected a) =>
   Dist.T prob a -> Dist.T Float Float
floatDist =
   Dist.Cons .
   map (\(x,p) -> (expected x, toFloat p)) .
   Dist.decons

instance (ToFloat prob, Expected a) => Expected (Dist.T prob a) where
  expected = Dist.expected . floatDist
--  expected = Dist.expected . fmap expected


-- | statistical analyses
variance :: Expected a => Probability.Dist a -> Float
variance = Dist.variance . floatDist

stdDev :: Expected a => Probability.Dist a -> Float
stdDev = sqrt . variance
