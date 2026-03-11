-- Copyright (c) 2018 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
{-# LANGUAGE CPP #-}
import Test.Framework
import Test.Framework.Providers.LeanCheck as LC

import Data.List (sort)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ testProperty "sort . sort == sort"
      $ \xs -> sort (sort xs :: [Int]) == sort xs
  , testProperty "x + x == 2 * x"
      $ \x -> x + x == 2 * (x :: Int)
  ]
