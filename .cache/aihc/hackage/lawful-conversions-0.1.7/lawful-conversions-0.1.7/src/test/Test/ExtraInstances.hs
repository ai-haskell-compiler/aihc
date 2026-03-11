{-# OPTIONS_GHC -Wno-orphans #-}

module Test.ExtraInstances () where

import qualified Data.ByteString.Builder as ByteStringBuilder
import qualified Data.Text.Lazy.Builder as TextLazyBuilder
import Rebase.Prelude
import Test.QuickCheck
import Test.QuickCheck.Instances ()

instance Arbitrary TextLazyBuilder.Builder where
  arbitrary = TextLazyBuilder.fromText <$> arbitrary
  shrink = shrinkMap TextLazyBuilder.fromLazyText TextLazyBuilder.toLazyText

instance Eq ByteStringBuilder.Builder where
  (==) = on (==) ByteStringBuilder.toLazyByteString

instance Arbitrary ByteStringBuilder.Builder where
  arbitrary = ByteStringBuilder.byteString <$> arbitrary
  shrink = shrinkMap ByteStringBuilder.lazyByteString ByteStringBuilder.toLazyByteString
