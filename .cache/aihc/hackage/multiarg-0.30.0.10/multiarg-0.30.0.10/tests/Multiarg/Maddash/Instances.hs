{-# OPTIONS_GHC -fno-warn-orphans #-}
module Multiarg.Maddash.Instances where

import Control.Applicative
import Test.QuickCheck
import Multiarg.Maddash
import Multiarg.Types.Instances ()

instance Arbitrary OptionError where
  arbitrary = oneof
    [ BadOption <$> arbitrary
    , LongArgumentForZeroArgumentOption <$> arbitrary <*> arbitrary
    ]

instance Arbitrary a => Arbitrary (Output a) where
  arbitrary = oneof
    [ Good <$> arbitrary
    , OptionError <$> arbitrary
    ]

instance Arbitrary a => Arbitrary (Pallet a) where
  arbitrary = oneof
    [ return NotAnOption
    , Full <$> arbitrary
    ]

instance Arbitrary a => Arbitrary (State a) where
  arbitrary = oneof
    [ return Ready
    , Pending <$> arbitrary <*> arbitrary
    ]
