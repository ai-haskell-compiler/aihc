-- | Various floating point limit related numerical constants.
module Numeric.Limits(
    -- * Significant digits
    digits10,
    -- * Minimum difference
    epsilon,
    -- * Extreme finite values
    maxValue,
    minValue,
    -- * Abnormal values
    infinity,
    nan,
    ) where

-- | The number of decimal digits that the type can represent without loss of precision.
{-# SPECIALIZE digits10 :: Double -> Int #-}
{-# SPECIALIZE digits10 :: Float -> Int #-}
digits10 :: (RealFloat a) => a -> Int
digits10 x = count 0 (floatRadix x ^ floatDigits x)
  where count n v = if v < 10 then n else count (n+1) (v `quot` 10)
    

-- | The difference between 1 and the smallest value greater than 1 that is representable for the type.
{-# SPECIALIZE epsilon :: Double #-}
{-# SPECIALIZE epsilon :: Float #-}
epsilon :: (RealFloat a) => a
epsilon = r
  where r = 1 - encodeFloat (m-1) e
        (m, e) = decodeFloat (1 `asTypeOf` r)

-- | Infinity value if the type supports it.
{-# SPECIALIZE infinity :: Double #-}
{-# SPECIALIZE infinity :: Float #-}
infinity :: (RealFloat a) => a
infinity = 1/0

-- | Not-a-number value if the type supports it.
{-# SPECIALIZE nan :: Double #-}
{-# SPECIALIZE nan :: Float #-}
nan :: (RealFloat a) => a
nan = 0/0

-- | The maximum finite value for the type.
{-# SPECIALIZE maxValue :: Double #-}
{-# SPECIALIZE maxValue :: Float #-}
maxValue :: (RealFloat a) => a
maxValue = x
  where n = floatDigits x
        b = floatRadix x
        (_, u) = floatRange x
        x = encodeFloat (b^n - 1) (u - n)

-- | The minimum (positive) normalized value for the type.
{-# SPECIALIZE minValue :: Double #-}
{-# SPECIALIZE minValue :: Float #-}
minValue :: (RealFloat a) => a
minValue = x
  where n = floatDigits x
        b = floatRadix x
        (l, _) = floatRange x
        x = encodeFloat (b^n - 1) (l - n - 1)
