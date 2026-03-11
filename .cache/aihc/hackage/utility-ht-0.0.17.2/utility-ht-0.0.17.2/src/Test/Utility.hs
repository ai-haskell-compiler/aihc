-- cf. Test.NumericPrelude.Utility
module Test.Utility where

import qualified Test.QuickCheck as QC

import Data.List.HT (mapAdjacent, )
import qualified Data.List as List


-- compare the lists simultaneously
equalLists :: Eq a => [[a]] -> Bool
equalLists xs =
   let equalElems ys =
          and (mapAdjacent (==) ys)  &&  length xs == length ys
   in  all equalElems (List.transpose xs)

equalInfLists :: Eq a => Int -> [[a]] -> Bool
equalInfLists n xs = equalLists (map (take n) xs)


forAllPredicates ::
   (QC.Testable test) => ((Char -> Bool) -> test) -> QC.Property
forAllPredicates prop = QC.property $ \x -> prop (x<=)

defined :: (Eq a) => a -> Bool
defined a = a==a
