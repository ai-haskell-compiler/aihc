-- Copyright (c) 2018 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
{-# LANGUAGE CPP #-}
import Test.Hspec
import Test.Hspec.LeanCheck as LC

import Data.List (sort)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "sort" $ do
    it "is idempotent" $
      LC.property $ \xs -> sort (sort xs :: [Int]) == sort xs
    it "preserves length" $
      LC.property $ \xs -> length (sort xs :: [Int]) == length xs
    it "preserves membership" $
      LC.property $ \x xs -> (x `elem` (sort xs :: [Int])) == (x `elem` xs)

  describe "sort (tested by propertyFor)" $ do
    it "is idempotent" $
      LC.propertyFor 2000 $ \xs -> sort (sort xs :: [Int]) == sort xs
    it "preserves length" $
      LC.propertyFor 2000 $ \xs -> length (sort xs :: [Int]) == length xs
    it "preserves membership" $
      LC.propertyFor 2000 $ \x xs -> (x `elem` (sort xs :: [Int])) == (x `elem` xs)

  describe "sort (tested by prop)" $ do
    prop "idempotent" $ \xs   ->       sort (sort xs :: [Int]) == sort xs
    prop "== length"  $ \xs   ->     length (sort xs :: [Int]) == length xs
    prop "== `elem`"  $ \x xs -> (x `elem` (sort xs :: [Int])) == (x `elem` xs)
