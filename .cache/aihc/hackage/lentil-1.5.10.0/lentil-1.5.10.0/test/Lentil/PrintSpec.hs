module Lentil.PrintSpec where

import Test.Hspec

-- import Lentil.Types
import Lentil.Print

-- Parsing tests

main :: IO ()
main = hspec spec


spec :: Spec
spec = do


  describe "alignNumber" $ do
    it "right aligns an integer" $
      alignNumber  4 30 `shouldBe` "  30"
