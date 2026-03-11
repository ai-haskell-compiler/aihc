-- Copyright (c) 2018 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
--
-- Minimal example of using LeanCheck with Tasty.
import Test.Tasty
import Test.Tasty.LeanCheck as LC
import Data.List

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Test properties checked by LeanCheck"
  [ LC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
  , LC.testProperty "Fermat's little theorem" $
      \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
  -- the following two properties do not hold
  , LC.testProperty "Fermat's last theorem" $
      \x y z n ->
        (n :: Integer) >= 3 LC.==> x^n + y^n /= (z^n :: Integer)
  , LC.testProperty "divMod" $
      \x y -> y * (x `div` y) + x `mod` y == (x :: Int)
  ]
