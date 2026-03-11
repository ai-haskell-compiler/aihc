{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.Mergeful.Timed where

import Data.GenValidity
import Data.Mergeful.Timed
import Test.QuickCheck

instance GenValid a => GenValid (Timed a) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ServerTime where
  genValid = ServerTime <$> arbitrary

  -- Use the quickcheck generator to produce Word64s
  -- This will hide the failures around maxBound for Word64
  -- but that's fine in this case.
  -- See also the comment for the 'ServerTime' constructor.
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
