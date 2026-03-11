{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Features.StrictTextBuilder (tests) where

#if MIN_VERSION_text(2,1,2)

import Data.Function
import Data.Proxy
import qualified Data.Text.Encoding as TextEncoding
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.QuickCheck
import TextBuilderDev
import Util.TestTrees
import Prelude

tests :: [TestTree]
tests =
  [ isomorphic $
      Proxy @TextEncoding.StrictTextBuilder,
    testGroup "to" $
      [ mapsToMonoid (to @TextEncoding.StrictTextBuilder),
        mapsToMonoid (from @TextEncoding.StrictTextBuilder)
      ]
  ]

instance Eq TextEncoding.StrictTextBuilder where
  a == b =
    on (==) TextEncoding.strictBuilderToText a b

instance Show TextEncoding.StrictTextBuilder where
  showsPrec d =
    showsPrec d . TextEncoding.strictBuilderToText

instance Arbitrary TextEncoding.StrictTextBuilder where
  arbitrary =
    TextEncoding.textToStrictBuilder <$> arbitrary

#else

import Test.Tasty

tests :: [TestTree]
tests = []

#endif
