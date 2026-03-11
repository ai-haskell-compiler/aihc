module SlashSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding ((.&.))

import Control.Monad
import Data.Ratio
import Data.Ratio.Slash

spec :: Spec
spec = do
  describe "rational" $ do

    prop "read" $
      \n d -> d /= 0 && n /= 0 && d > 1 && denominator ( n % d ) > 1 ==>
        ( (read $ join [show n,"/",show d] ) :: Slash Integer ) `shouldBe` ( Slash $ n % d )

    prop "both ways" $
      \n d -> d /= 0 && n /= 0 && d > 1 && denominator ( n % d ) > 1 ==>
        ( read . show $ Slash ( n % d ) :: Slash Integer ) `shouldBe` ( Slash $ n % d )

  describe "integer" $ do
    prop "read" $
      \n -> True ==>
        ( (read $ show n ) :: Slash Integer ) `shouldBe` ( Slash $ n % 1 )

    prop "both ways" $
      \n -> True ==>
        ( read . show $ Slash ( n % 1 ) :: Slash Integer ) `shouldBe` ( Slash $ n % 1 )
