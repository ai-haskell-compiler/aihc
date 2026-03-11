{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Features.StrictBuilder (tests) where

#if MIN_VERSION_text(2,0,2) && !MIN_VERSION_text(2,1,2)

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
      Proxy @TextEncoding.StrictBuilder,
    testGroup "to" $
      [ mapsToMonoid (to @TextEncoding.StrictBuilder),
        mapsToMonoid (from @TextEncoding.StrictBuilder)
      ]
  ]

instance Eq TextEncoding.StrictBuilder where
  a == b =
    on (==) TextEncoding.strictBuilderToText a b

instance Show TextEncoding.StrictBuilder where
  showsPrec d =
    showsPrec d . TextEncoding.strictBuilderToText

instance Arbitrary TextEncoding.StrictBuilder where
  arbitrary =
    TextEncoding.textToStrictBuilder <$> arbitrary

#else

import Test.Tasty

tests :: [TestTree]
tests = []

#endif
