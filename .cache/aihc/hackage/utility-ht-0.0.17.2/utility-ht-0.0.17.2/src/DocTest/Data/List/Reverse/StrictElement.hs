-- Do not edit! Automatically created with doctest-extract from src/Data/List/Reverse/StrictElement.hs
{-# LINE 15 "src/Data/List/Reverse/StrictElement.hs" #-}

module DocTest.Data.List.Reverse.StrictElement where

import Data.List.Reverse.StrictElement
import qualified Test.DocTest.Driver as DocTest

{-# LINE 16 "src/Data/List/Reverse/StrictElement.hs" #-}
import     Test.Utility (forAllPredicates, defined)
import     qualified Data.List.Reverse.StrictElement as Rev
import     qualified Data.List.Match as Match
import     qualified Data.List as List
import     Data.Tuple.HT (mapPair, swap)

_suppressUnusedImportWarning     :: (a -> Bool) -> [a] -> [a]
_suppressUnusedImportWarning     = Data.List.Reverse.StrictElement.dropWhile

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Data.List.Reverse.StrictElement:31: "
{-# LINE 31 "src/Data/List/Reverse/StrictElement.hs" #-}
 DocTest.property(
{-# LINE 31 "src/Data/List/Reverse/StrictElement.hs" #-}
      forAllPredicates $ \p xs -> Rev.dropWhile p xs == reverse (List.dropWhile p (reverse xs))
  )
 DocTest.printPrefix "Data.List.Reverse.StrictElement:32: "
{-# LINE 32 "src/Data/List/Reverse/StrictElement.hs" #-}
 DocTest.property(
{-# LINE 32 "src/Data/List/Reverse/StrictElement.hs" #-}
      \x xs pad -> defined $ Match.take (pad::[()]) $ Rev.dropWhile ((x::Char)/=) $ cycle $ x:xs
  )
 DocTest.printPrefix "Data.List.Reverse.StrictElement:41: "
{-# LINE 41 "src/Data/List/Reverse/StrictElement.hs" #-}
 DocTest.property(
{-# LINE 41 "src/Data/List/Reverse/StrictElement.hs" #-}
      forAllPredicates $ \p xs -> Rev.takeWhile p xs == reverse (List.takeWhile p (reverse xs))
  )
 DocTest.printPrefix "Data.List.Reverse.StrictElement:52: "
{-# LINE 52 "src/Data/List/Reverse/StrictElement.hs" #-}
 DocTest.property(
{-# LINE 52 "src/Data/List/Reverse/StrictElement.hs" #-}
      forAllPredicates $ \p xs -> Rev.span p xs == swap (mapPair (reverse, reverse) (List.span p (reverse xs)))
  )
 DocTest.printPrefix "Data.List.Reverse.StrictElement:53: "
{-# LINE 53 "src/Data/List/Reverse/StrictElement.hs" #-}
 DocTest.property(
{-# LINE 53 "src/Data/List/Reverse/StrictElement.hs" #-}
      forAllPredicates $ \p xs -> Rev.span p xs == (Rev.dropWhile p xs, Rev.takeWhile p xs)
  )
 DocTest.printPrefix "Data.List.Reverse.StrictElement:54: "
{-# LINE 54 "src/Data/List/Reverse/StrictElement.hs" #-}
 DocTest.property(
{-# LINE 54 "src/Data/List/Reverse/StrictElement.hs" #-}
      \x xs pad -> defined $ Match.take (pad::[()]) $ fst $ Rev.span ((x::Char)/=) $ cycle $ x:xs
  )
