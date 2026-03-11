{-# OPTIONS_GHC -fno-warn-orphans #-}
module Multiarg.Types.Instances where

import Control.Applicative
import Test.QuickCheck
import Multiarg.Types

instance Arbitrary a => Arbitrary (ArgSpec a) where
  arbitrary = oneof
    [ ZeroArg <$> arbitrary
    , OneArg <$> arbitrary
    , TwoArg <$> arbitrary
    , ThreeArg <$> arbitrary
    ]

instance Arbitrary ShortName where
  arbitrary = do
    c <- arbitrary
    case shortName c of
      Nothing -> arbitrary
      Just n -> return n

instance Arbitrary LongName where
  arbitrary = do
    c1 <- arbitrary `suchThat` (\c -> c /= '-' && c /= '=')
    cs <- listOf (arbitrary `suchThat` (/= '='))
    case longName (c1 : cs) of
      Nothing -> error $ "could not generate long name: " ++ (c1:cs)
      Just n -> return n

instance Arbitrary OptName where
  arbitrary = fmap OptName arbitrary

instance Arbitrary Multiarg.Types.Word where
  arbitrary = Multiarg.Types.Word <$> arbitrary

instance CoArbitrary Multiarg.Types.Word where
  coarbitrary (Multiarg.Types.Word s) = coarbitrary s

instance Arbitrary OptArg where
  arbitrary = OptArg <$> arbitrary

