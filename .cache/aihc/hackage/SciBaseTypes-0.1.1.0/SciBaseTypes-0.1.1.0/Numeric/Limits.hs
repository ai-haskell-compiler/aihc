
-- | Approximate and exact limits around 0 and towards transfinite numbers.

module Numeric.Limits where



-- | The class of limits into the transfinite.

class NumericLimits x where
  -- "A" minimal finite number that can still be worked with. (And should not
  -- trip the CPU transfinite number handling)
  minFinite ∷ x
  -- "A" maximal finite number.
  maxFinite ∷ x

-- | The smallest value @/= 0@ for numeric values.

class NumericEpsilon x where
  -- | Numeric epsilon.
  epsilon ∷ x



instance NumericLimits Word where
  minFinite = minBound `div` 100000
  maxFinite = maxBound `div` 100000
  {-# Inline minFinite #-}
  {-# Inline maxFinite #-}

instance NumericLimits Int where
  minFinite = minBound `div` 100000
  maxFinite = maxBound `div` 100000
  {-# Inline minFinite #-}
  {-# Inline maxFinite #-}

instance NumericLimits Double where
  minFinite = -1.79e308
  maxFinite =  1.79e308
  {-# Inline minFinite #-}
  {-# Inline maxFinite #-}

instance NumericEpsilon Double where
  epsilon = 2.2e-16
  {-# Inline epsilon #-}

