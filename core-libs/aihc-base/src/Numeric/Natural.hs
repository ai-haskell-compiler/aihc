module Numeric.Natural
  ( Natural,
    minusNaturalMaybe,
  )
where

import GHC.Num.Natural (Natural)
import Prelude

-- | Subtraction that reports an underflow instead of throwing one.
minusNaturalMaybe :: Natural -> Natural -> Maybe Natural
minusNaturalMaybe left right =
  case left < right of
    True -> Nothing
    False -> Just (left - right)
