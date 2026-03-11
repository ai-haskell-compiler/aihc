module LengthSpec (spec) where

import Data.Agreement
import Test.Hspec


doTheseHaveTheSameLength :: [String] -> String
doTheseHaveTheSameLength l = case foldMap (Somebody . length) l of
  Somebody n -> "All have length " <> show n
  Nobody     -> "The lengths differ"
  Anybody    -> "No strings provided"


spec :: Spec
spec = do
  describe "Testing whether strings have same length" $ do
    it "works on strings of same length" $
      doTheseHaveTheSameLength ["cat", "dog", "rat", "cow"] `shouldBe` "All have length 3"
    it "works on single string" $
      doTheseHaveTheSameLength ["crocodile"] `shouldBe` "All have length 9"
    it "works on empty list" $
      doTheseHaveTheSameLength [] `shouldBe` "No strings provided"
    it "works on disparate strings" $
      doTheseHaveTheSameLength ["sheep", "sheep", "sheep", "Archangel Gabriel", "sheep"] `shouldBe` "The lengths differ"

