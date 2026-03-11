module Main (main) where

import Test.Hspec

import Data.DoList
import Data.DoMonoid

import Data.Monoid (Sum (..))
import GHC.Exts (fromString)


main :: IO ()
main = hspec $ do
  describe "DoMonoid" $ do
    it "(>>)" $
      runDoM (DoM (Sum 1) >> DoM (Sum 1)) `shouldBe` Sum (2 :: Int)
    it "fromString" $
      fromString "abc" `shouldBe` DoM ("abc" :: String)
  describe "DoList" $ do
    it "(>>)" $
      toList (item 1 >> item 2) `shouldBe` ([1, 2] :: [Int])
    it "fromString" $
      fromString "abc" `shouldBe` DoList (DoM ["abc" :: String])