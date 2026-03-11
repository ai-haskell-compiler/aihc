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
      LC.property $ \xs -> sort (sort xs :: [Int]) `shouldBe` sort xs
    it "is identity" $ -- not really
      LC.property $ \xs -> sort (xs :: [Int]) `shouldBe` xs
    it "head is minimum" $ -- not really, error
      LC.property $ \xs -> head (sort xs :: [Int]) `shouldBe` minimum xs
