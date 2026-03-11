module Main where

import Data.Proxy
import Data.Text (Text)
import qualified Data.Text.Lazy as TextLazy
import qualified Data.Text.Lazy.Builder as TextLazyBuilder
import qualified Features
import Test.QuickCheck.Classes
import Test.QuickCheck.Instances ()
import Test.Tasty
import qualified TextBuilderDev as B
import Util.ExtraInstances ()
import Util.TestTrees
import Prelude

main :: IO ()
main = (defaultMain . testGroup "All") tests

tests :: [TestTree]
tests =
  [ testGroup "Features" Features.tests,
    testGroup "Isomorphic instances" $
      [ testGroup "Text" $
          [ isomorphic $ Proxy @Text
          ],
        testGroup "Lazy Text" $
          [ isomorphic $ Proxy @TextLazy.Text
          ],
        testGroup "Lazy Text Builder" $
          [ isomorphic $ Proxy @TextLazyBuilder.Builder
          ]
      ],
    followsLaws $ showLaws (Proxy @B.TextBuilder),
    followsLaws $ eqLaws (Proxy @B.TextBuilder),
    followsLaws $ semigroupLaws (Proxy @B.TextBuilder),
    followsLaws $ monoidLaws (Proxy @B.TextBuilder)
  ]
