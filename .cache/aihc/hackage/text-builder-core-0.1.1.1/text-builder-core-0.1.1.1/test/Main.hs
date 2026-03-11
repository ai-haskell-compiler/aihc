module Main where

import Data.Proxy
import qualified Data.Text as Text
import qualified Features
import Test.QuickCheck.Classes
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.QuickCheck hiding ((.&.))
import qualified TextBuilderCore as B
import Util.ExtraInstances ()
import Util.TestTrees
import Prelude

main :: IO ()
main = (defaultMain . testGroup "All") tests

tests :: [TestTree]
tests =
  [ testGroup "Legacy" $
      [ testProperty "Packing a list of chars is isomorphic to appending a list of builders" $
          \chars ->
            Text.pack chars
              === B.toText (foldMap B.char chars),
        testProperty "Concatting a list of texts is isomorphic to fold-mapping with builders" $
          \texts ->
            mconcat texts
              === B.toText (foldMap B.text texts),
        testProperty "Concatting a list of texts is isomorphic to concatting a list of builders" $
          \texts ->
            mconcat texts
              === B.toText (mconcat (map B.text texts)),
        testProperty "Concatting a list of trimmed texts is isomorphic to concatting a list of builders" $
          \texts ->
            let trimmedTexts = fmap (Text.drop 3) texts
             in mconcat trimmedTexts
                  === B.toText (mconcat (map B.text trimmedTexts))
      ],
    testGroup "Laws" $
      [ followsLaws $ showLaws (Proxy @B.TextBuilder),
        followsLaws $ eqLaws (Proxy @B.TextBuilder),
        followsLaws $ semigroupLaws (Proxy @B.TextBuilder),
        followsLaws $ monoidLaws (Proxy @B.TextBuilder)
      ],
    testGroup "Features" Features.tests
  ]
