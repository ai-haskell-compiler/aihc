{-# OPTIONS_GHC -Wno-orphans #-}

module Util.ExtraInstances where

import qualified Data.Text.Lazy.Builder as TextLazyBuilder
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Prelude

instance Arbitrary TextLazyBuilder.Builder where
  arbitrary =
    TextLazyBuilder.fromLazyText <$> arbitrary
