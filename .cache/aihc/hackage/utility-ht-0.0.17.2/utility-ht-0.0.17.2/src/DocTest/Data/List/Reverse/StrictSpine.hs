-- Do not edit! Automatically created with doctest-extract from src/Data/List/Reverse/StrictSpine.hs
{-# LINE 14 "src/Data/List/Reverse/StrictSpine.hs" #-}

module DocTest.Data.List.Reverse.StrictSpine where

import Data.List.Reverse.StrictSpine
import qualified Test.DocTest.Driver as DocTest

{-# LINE 15 "src/Data/List/Reverse/StrictSpine.hs" #-}
import     Test.Utility (forAllPredicates, defined)
import     qualified Data.List.Reverse.StrictSpine as Rev
import     qualified Data.List.Match as Match
import     qualified Data.List as List
import     Data.Tuple.HT (mapFst, mapPair, swap)

_suppressUnusedImportWarning     :: (a -> Bool) -> [a] -> [a]
_suppressUnusedImportWarning     = Data.List.Reverse.StrictSpine.dropWhile

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Data.List.Reverse.StrictSpine:26: "
{-# LINE 26 "src/Data/List/Reverse/StrictSpine.hs" #-}
 DocTest.property(
{-# LINE 26 "src/Data/List/Reverse/StrictSpine.hs" #-}
      forAllPredicates $ \p xs -> Rev.dropWhile p xs == reverse (List.dropWhile p (reverse xs))
  )
 DocTest.printPrefix "Data.List.Reverse.StrictSpine:27: "
{-# LINE 27 "src/Data/List/Reverse/StrictSpine.hs" #-}
 DocTest.property(
{-# LINE 27 "src/Data/List/Reverse/StrictSpine.hs" #-}
      \x xs pad -> defined $ length $ Rev.dropWhile ((x::Char)/=) $ Match.replicate (pad::[()]) undefined ++ x:xs
  )
 DocTest.printPrefix "Data.List.Reverse.StrictSpine:34: "
{-# LINE 34 "src/Data/List/Reverse/StrictSpine.hs" #-}
 DocTest.property(
{-# LINE 34 "src/Data/List/Reverse/StrictSpine.hs" #-}
      forAllPredicates $ \p xs -> Rev.takeWhile p xs == reverse (List.takeWhile p (reverse xs))
  )
 DocTest.printPrefix "Data.List.Reverse.StrictSpine:35: "
{-# LINE 35 "src/Data/List/Reverse/StrictSpine.hs" #-}
 DocTest.property(
{-# LINE 35 "src/Data/List/Reverse/StrictSpine.hs" #-}
      \x xs pad -> defined $ Rev.takeWhile ((x::Char)/=) $ Match.replicate (pad::[()]) undefined ++ x:xs
  )
 DocTest.printPrefix "Data.List.Reverse.StrictSpine:46: "
{-# LINE 46 "src/Data/List/Reverse/StrictSpine.hs" #-}
 DocTest.property(
{-# LINE 46 "src/Data/List/Reverse/StrictSpine.hs" #-}
      forAllPredicates $ \p xs -> Rev.span p xs == swap (mapPair (reverse, reverse) (List.span p (reverse xs)))
  )
 DocTest.printPrefix "Data.List.Reverse.StrictSpine:47: "
{-# LINE 47 "src/Data/List/Reverse/StrictSpine.hs" #-}
 DocTest.property(
{-# LINE 47 "src/Data/List/Reverse/StrictSpine.hs" #-}
      forAllPredicates $ \p xs -> Rev.span p xs == (Rev.dropWhile p xs, Rev.takeWhile p xs)
  )
 DocTest.printPrefix "Data.List.Reverse.StrictSpine:48: "
{-# LINE 48 "src/Data/List/Reverse/StrictSpine.hs" #-}
 DocTest.property(
{-# LINE 48 "src/Data/List/Reverse/StrictSpine.hs" #-}
      \x xs pad -> defined $ mapFst length $ Rev.span ((x::Char)/=) $ Match.replicate (pad::[()]) undefined ++ x:xs
  )
