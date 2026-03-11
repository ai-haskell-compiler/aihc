{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Orphans () where

import Numeric.Natural (Natural)
import Test.QuickCheck (Arbitrary (..))

#if !MIN_VERSION_QuickCheck(2,17,0)
-- | The QuickCheck-2.10 doesn't define the Arbitrary Natural instance
-- We define own instance (and not use quickcheck-instance) to break
-- the cycle in tests.
instance Arbitrary Natural where
    arbitrary = fmap (fromInteger . abs) arbitrary
    shrink = map (fromInteger . abs) . shrink . fromIntegral
#endif
