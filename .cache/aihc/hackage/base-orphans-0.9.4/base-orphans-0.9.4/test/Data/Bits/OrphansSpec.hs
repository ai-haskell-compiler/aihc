module Data.Bits.OrphansSpec (main, spec) where

import Test.Hspec

import Data.Bits
import Data.Orphans ()

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Bits Bool instance" $
    it "allows bitwise operations on Bools" $ do
      True  .&. True  `shouldBe` True
      True  .&. False `shouldBe` False
      False .&. True  `shouldBe` False
      False .&. False `shouldBe` False
