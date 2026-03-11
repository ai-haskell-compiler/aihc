module Combinatorics.Permutation.WithoutSomeFixpoints where

import Combinatorics (permute)

{- $setup
>>> import qualified Combinatorics.Permutation.WithoutSomeFixpoints as PermWOFP
>>> import qualified Combinatorics as Comb
>>> import qualified Test.QuickCheck as QC
>>> import Control.Applicative ((<$>))
>>> import Data.List (nub)
>>>
>>> genPermutationWOFP :: QC.Gen (Int, String)
>>> genPermutationWOFP = do
>>>    xs <- take 6 . nub <$> QC.arbitrary
>>>    k <- QC.choose (0, length xs)
>>>    return (k,xs)
-}


{- |
@enumerate n xs@ list all permutations of @xs@
where the first @n@ elements do not keep their position
(i.e. are no fixpoints).

This is a generalization of derangement.

Naive but comprehensible implementation.
-}
enumerate :: (Eq a) => Int -> [a] -> [[a]]
enumerate k xs = filter (and . zipWith (/=) xs . take k) $ permute xs

{- | <http://oeis.org/A047920>

prop> QC.forAll genPermutationWOFP $ \(k,xs) -> PermWOFP.numbers !! length xs !! k == length (PermWOFP.enumerate k xs)
prop> QC.forAll (QC.choose (0,100)) $ \k -> Comb.factorial (toInteger k) == PermWOFP.numbers !! k !! 0
prop> QC.forAll (QC.choose (0,100)) $ \k -> Comb.derangementNumber (toInteger k) == PermWOFP.numbers !! k !! k
-}
numbers :: (Num a) => [[a]]
numbers =
   tail $ scanl (\row fac -> scanl (-) fac row) [] $
   scanl (*) 1 $ iterate (1+) 1
