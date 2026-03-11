{-# OPTIONS_GHC -fno-warn-orphans #-}
import           Data.Aeson                     ( encode
                                                , decode
                                                )
import qualified Data.MXStateCodes             as MXCodes
import qualified Data.Text                     as T
import           Test.Hspec
import           Test.QuickCheck

-- brittany-disable-next-binding
main :: IO ()
main = hspec $ do
    describe "all" $
        it "contains all enumerations" $
            MXCodes.all `shouldBe` [minBound .. maxBound]
    describe "toName" $
        it "is idempotent with fromName" $ property $
            \c -> (MXCodes.fromName . MXCodes.toName) c == Just c
    describe "fromName" $
        it "is case-insensitive" $ property $
            \c -> (MXCodes.fromName . T.toUpper . MXCodes.toName) c == Just c
    describe "Aeson instance" $
        it "is idempotent" $ property $
            \c -> (decode . encode) c == Just (c :: MXCodes.Code)


instance Arbitrary MXCodes.Code where
    arbitrary = elements MXCodes.all
