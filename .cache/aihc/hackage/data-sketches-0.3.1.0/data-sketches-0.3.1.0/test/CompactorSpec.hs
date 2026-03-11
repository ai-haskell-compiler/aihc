module CompactorSpec where

import DataSketches.Quantiles.RelativeErrorQuantile.Internal.Compactor
import Test.Hspec

spec :: Spec
spec = do
  specify "nearestEven behaves as expected" $ do
    nearestEven (-0.9) `shouldBe` 0