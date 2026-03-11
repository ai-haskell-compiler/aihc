import           Test.Hspec

import Data.Time.ISO8601


main :: IO ()
main = hspec $ do
  let readTest str date = parseISO8601 str `shouldBe` (Just date)

  describe "parseISO8601" $ do

    it "time zone formats" $ do
      readTest "2014-03-28T10:26:00Z"      (read "2014-03-28 10:26:00 UTC")
      readTest "2014-03-28T10:26:00-0700"  (read "2014-03-28 17:26:00 UTC")
      readTest "2014-03-28T10:26:00+0700"  (read "2014-03-28 03:26:00 UTC")
      readTest "2014-03-28T10:26:00-07:00" (read "2014-03-28 17:26:00 UTC")
      readTest "2014-03-28T10:26:00+07:00" (read "2014-03-28 03:26:00 UTC")
