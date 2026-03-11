{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Text.Arbitrary(
  Text()
) where

import           Data.Text

import           Test.QuickCheck
import           Test.QuickCheck.Utf8

instance Arbitrary Text where
  arbitrary = genValidUtf8
  shrink = shrinkValidUtf8
