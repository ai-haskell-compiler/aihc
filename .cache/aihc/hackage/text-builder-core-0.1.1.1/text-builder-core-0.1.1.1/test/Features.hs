module Features (tests) where

import Control.Monad
import Data.Char
import Data.String
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.QuickCheck
import TextBuilderCore
import Util.TestTrees
import Prelude

tests :: [TestTree]
tests =
  [ testGroup "isEmpty" $
      [ testProperty "Is True for empty string" $
          isEmpty (fromString ""),
        testProperty "Is True for mempty" $
          isEmpty (fromString mempty),
        testProperty "Is True for (mempty <> mempty)" $
          isEmpty (fromString (mempty <> mempty)),
        testProperty "Is isomorphic to Text.null" $ \a ->
          isEmpty (text a) === Text.null a
      ],
    testGroup "toText" $
      [ mapsToMonoid toText,
        testProperty "Roundtrips" \textValue ->
          toText (text textValue) === textValue
      ],
    testGroup "string" $
      [ mapsToMonoid string,
        testProperty "Roundtrips" \builder ->
          string (Text.unpack (toText builder)) === builder
      ],
    testGroup "text" $
      [ mapsToMonoid text,
        testProperty "Roundtrips" \builder ->
          text (toText builder) === builder
      ],
    testGroup "lazyText" $
      [ mapsToMonoid lazyText,
        testProperty "Roundtrips" \builder ->
          lazyText (Text.Lazy.fromStrict (toText builder)) === builder
      ],
    testGroup "char" $
      [ mapsToMonoid char,
        testProperty "Is isomorphic to Text.singleton" \a ->
          toText (char a) === Text.singleton a
      ],
    testGroup "unicodeCodepoint" $
      [ mapsToMonoid unicodeCodepoint,
        testProperty "Is isomorphic to Text.singleton" \a ->
          toText (unicodeCodepoint (ord a)) === Text.singleton a
      ],
    testGroup "unsafeSeptets" $
      [ isMonoidWithCustomGen do
          maxGenSize <- getSize
          maxSize <- chooseInt (0, maxGenSize)
          actualSize <- chooseInt (0, maxSize)
          septets <-
            replicateM actualSize $
              fromIntegral <$> chooseInt (0, 127)
          pure (unsafeSeptets maxSize septets)
      ],
    testGroup "unsafeReverseSeptets" $
      [ isMonoidWithCustomGen do
          maxGenSize <- getSize
          maxSize <- chooseInt (0, maxGenSize)
          septets <-
            replicateM maxSize $
              fromIntegral <$> chooseInt (0, 127)
          pure (unsafeReverseSeptets maxSize septets)
      ]
  ]
