-- Copyright (c) 2018 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
{-# LANGUAGE CPP #-}
import Test.Tasty
import Test.Tasty.LeanCheck as LC

import Data.List (sort)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests (checked by LeanCheck)" [passing]
-- tests = testGroup "Tests (checked by LeanCheck)" [passing,failing,erroring]

-- tests that pass
passing :: TestTree
passing = testGroup "Passing tests"
  [ LC.testProperty "sort . sort == sort" $
      \xs -> sort (sort xs :: [Int]) == sort xs
  , LC.testProperty "Fermat's little theorem" $
      \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
  , LC.testProperty "divMod" $
      \x y -> y /= 0 ==> y * (x `div` y) + x `mod` y == (x :: Int)
  ]

-- tests that fail (and should)
failing :: TestTree
failing = testGroup "Failing tests"
  [ LC.testProperty "Fermat's last theorem" $
      \x y z n -> (n :: Integer) >= 3 ==> x^n + y^n /= (z^n :: Integer)
  ]

-- tests that err (and should)
erroring :: TestTree
erroring = testGroup "Erroring tests"
  [ LC.testProperty "divMod (unguarded)" $
      \x y -> y * (x `div` y) + x `mod` y == (x :: Int)
  ]
