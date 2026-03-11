module Scientist.ExperimentSpec
  ( spec
  ) where

import Scientist.Test

import Control.Monad (replicateM)
import Scientist.Experiment

spec :: Spec
spec = do
  describe "experimentEnabledPercent" $ do
    it "returns True the given percent of the time" $ do
      results <- replicateM 10000 $ experimentEnabledPercent 23

      length (filter id results) `shouldSatisfy` isWithinOf 500 2300
