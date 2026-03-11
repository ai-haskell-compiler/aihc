module Features (tests) where

import Data.Int
import Data.String
import Data.Time
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Word
import qualified Features.StrictBuilder as StrictBuilder
import qualified Features.StrictTextBuilder as StrictTextBuilder
import Numeric.Natural (Natural)
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.QuickCheck
import TextBuilderDev
import Util.TestTrees
import Prelude

tests :: [TestTree]
tests =
  [ testGroup "StrictBuilder" StrictBuilder.tests,
    testGroup "StrictTextBuilder" StrictTextBuilder.tests,
    testGroup "doubleFixedPoint" $
      [ mapsToMonoid (doubleFixedPoint 3)
      ],
    testGroup "doubleFixedPointPercent" $
      [ mapsToMonoid (doubleFixedPointPercent 3)
      ],
    testGroup "utcTimeIso8601Timestamp" $
      [ mapsToMonoid utcTimeIso8601Timestamp,
        testProperty "Same as iso8601Show" $ \x ->
          let roundedToSecondsTime =
                x {utctDayTime = (fromIntegral @Int . round . utctDayTime) x}
           in (fromString . flip mappend "Z" . take 19 . iso8601Show) roundedToSecondsTime
                === toText (utcTimeIso8601Timestamp roundedToSecondsTime)
      ],
    testGroup "approximateDataSize" $
      [ testGroup "Word" $
          [ mapsToMonoid @Word approximateDataSize
          ],
        testGroup "Natural" $
          [ mapsToMonoid @Natural approximateDataSize
          ]
      ]
  ]
