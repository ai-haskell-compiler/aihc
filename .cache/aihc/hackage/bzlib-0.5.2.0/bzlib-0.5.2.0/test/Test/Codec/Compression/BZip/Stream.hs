{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Test code and properties for "Codec.Compression.BZip.Stream"
--
module Test.Codec.Compression.BZip.Stream where

import Codec.Compression.BZip.Internal
import Test.QuickCheck


instance Arbitrary BlockSize where
    arbitrary = elements $ DefaultBlockSize : map BlockSize [1 .. 9]


instance Arbitrary MemoryLevel where
    arbitrary = elements [DefaultMemoryLevel, MinMemoryLevel]


instance Arbitrary WorkFactor where
    arbitrary = elements $ DefaultWorkFactor : map WorkFactor [1 .. 250]
