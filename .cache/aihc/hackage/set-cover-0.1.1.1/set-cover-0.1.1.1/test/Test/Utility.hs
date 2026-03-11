module Test.Utility where

import qualified Math.SetCover.Exact as ESC

import Control.Applicative ((<$>))

import qualified Data.Set as Set
import qualified Data.List as List
import Data.Set(Set)
import Data.Eq.HT (equating)

import qualified Test.QuickCheck as QC


setAssigns :: (Ord a) => [[a]] -> [ESC.Assign [a] (Set a)]
setAssigns = map (\x -> ESC.assign x (Set.fromList x))

normalizeSolution :: Ord a => [[a]] -> [[a]]
normalizeSolution = List.sort . map List.sort

equivalentSolutions :: Ord a => [[a]] -> [[a]] -> Bool
equivalentSolutions = equating normalizeSolution


genWord :: QC.Gen String
genWord = take 5 <$> QC.listOf (QC.choose ('a','e'))

genWords :: QC.Gen [String]
genWords = take 10 <$> QC.listOf genWord

type InflatedString = [(Char,Int)]

inflate :: String -> InflatedString
inflate = concatMap (\c -> map ((,) c) [0..49])

{- |
Make sure that bitset-based implementations
are forced to use more than one Word64 or one 128 bit vector.
-}
genInflatedWords :: QC.Gen [InflatedString]
genInflatedWords = fmap inflate <$> genWords


forAllShrinkSmall ::
   (QC.Testable prop, Show a, QC.Arbitrary a) =>
   QC.Gen a -> (a -> prop) -> (Int, QC.Property)
forAllShrinkSmall gen = (,) 200 . QC.forAllShrink gen QC.shrink
