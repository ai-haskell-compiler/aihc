{-# OPTIONS_GHC -fno-warn-orphans #-}
import           Data.Aeson                     ( encode
                                                , decode
                                                )
import qualified Data.CAProvinceCodes          as CAProvinces
import qualified Data.Text                     as T
import           Test.Hspec
import           Test.QuickCheck

-- brittany-disable-next-binding
main :: IO ()
main = hspec $ do
    describe "all" $
        it "contains all enumerations" $
            CAProvinces.all `shouldBe` [minBound .. maxBound]
    describe "toName" $
        it "is idempotent with fromName" $ property $
            \c -> (CAProvinces.fromName . CAProvinces.toName) c == Just c
    describe "fromName" $
        it "is case-insensitive" $ property $
            \c -> (CAProvinces.fromName . T.toUpper . CAProvinces.toName) c == Just c
    describe "Aeson instance" $
        it "is idempotent" $ property $
            \c -> (decode . encode) c == Just (c :: CAProvinces.Code)


instance Arbitrary CAProvinces.Code where
    arbitrary = elements CAProvinces.all
