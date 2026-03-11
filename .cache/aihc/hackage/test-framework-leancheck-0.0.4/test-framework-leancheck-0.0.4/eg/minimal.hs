-- Copyright (c) 2018 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
--
-- Minimal example of using LeanCheck with test-framework.
import Test.Framework
import Test.Framework.Providers.LeanCheck as LC
import Data.List

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ LC.testProperty "sort . sort == sort"
      $ \xs -> sort (sort xs :: [Int]) == sort xs
  , LC.testProperty "sort == id" -- not really, should fail
      $ \xs -> sort (xs :: [Int]) == xs
  , LC.testProperty "head . sort == minimum h" -- not really, error
      $ \xs -> head (sort xs :: [Int]) == minimum xs
  ]
