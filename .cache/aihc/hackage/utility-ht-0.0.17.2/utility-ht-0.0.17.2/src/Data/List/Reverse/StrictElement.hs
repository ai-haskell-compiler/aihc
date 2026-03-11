{- |
The functions in this module process the list formally from the end.
Actually they traverse the list from the start and check every element.
This way they are strict in the elements and lazy in the list spline.
Thus you can apply them to infinite lists.
Use these functions if the list is long or the test is cheap.
-}
module Data.List.Reverse.StrictElement where

import Data.Tuple.HT (mapFst, mapSnd, forcePair, )

import Prelude hiding (dropWhile, takeWhile, span, )


-- $setup
-- >>> import Test.Utility (forAllPredicates, defined)
-- >>> import qualified Data.List.Reverse.StrictElement as Rev
-- >>> import qualified Data.List.Match as Match
-- >>> import qualified Data.List as List
-- >>> import Data.Tuple.HT (mapPair, swap)
-- >>>
-- >>> _suppressUnusedImportWarning :: (a -> Bool) -> [a] -> [a]
-- >>> _suppressUnusedImportWarning = Data.List.Reverse.StrictElement.dropWhile


{- |
Remove the longest suffix of elements satisfying p.
In contrast to @reverse . dropWhile p . reverse@
this works for infinite lists, too.

prop> forAllPredicates $ \p xs -> Rev.dropWhile p xs == reverse (List.dropWhile p (reverse xs))
prop> \x xs pad -> defined $ Match.take (pad::[()]) $ Rev.dropWhile ((x::Char)/=) $ cycle $ x:xs
-}
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p =
   foldr (\x xs -> if p x && null xs then [] else x:xs) []

{- |
Alternative version of @reverse . takeWhile p . reverse@.

prop> forAllPredicates $ \p xs -> Rev.takeWhile p xs == reverse (List.takeWhile p (reverse xs))
-}
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p =
   snd .
   foldr
      (\x xys ->
         (if p x && fst xys then mapSnd (x:) else mapFst (const False)) xys)
      (True, [])

{- |
prop> forAllPredicates $ \p xs -> Rev.span p xs == swap (mapPair (reverse, reverse) (List.span p (reverse xs)))
prop> forAllPredicates $ \p xs -> Rev.span p xs == (Rev.dropWhile p xs, Rev.takeWhile p xs)
prop> \x xs pad -> defined $ Match.take (pad::[()]) $ fst $ Rev.span ((x::Char)/=) $ cycle $ x:xs
-}
span :: (a -> Bool) -> [a] -> ([a], [a])
span p =
   forcePair .
   foldr
      (\x xys ->
         (if p x && null (fst xys) then mapSnd else mapFst) (x:) xys)
      ([], [])
