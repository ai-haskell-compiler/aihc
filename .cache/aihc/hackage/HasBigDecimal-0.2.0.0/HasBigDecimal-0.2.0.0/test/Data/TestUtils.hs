module Data.TestUtils where

import           Test.Hspec            hiding (it)
import qualified Test.Hspec as HS      (it)
import           Test.Hspec.QuickCheck (modifyMaxSize, modifyMaxSuccess)
import           Test.QuickCheck       hiding (shrink)
import           Data.BigDecimal
import GHC.Natural

-- redefine it to use a sample with 1000 elements
it :: (HasCallStack, Example a) => String -> a -> SpecWith (Arg a)
it label action = modifyMaxSuccess (const 1000) $ HS.it label action

instance Arbitrary Natural where
  arbitrary = do
    NonNegative n <- arbitrary
    pure $ fromInteger n

-- arbitrary BigDecimals can be constructed using any Integer as unscaled value
-- and any non-negative Integer as scale
instance Arbitrary BigDecimal where
    arbitrary = do
      unscaledValue     <- arbitrary
      NonNegative scale <- arbitrary
      return $ BigDecimal unscaledValue scale
