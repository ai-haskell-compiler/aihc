{- |
The functions in this module process the list from the end.
They do not access elements at the beginning if not necessary.
You can apply the function only to finite lists.
Use these functions if the list is short and the test is expensive.
-}
module Data.List.Reverse.StrictSpine where

import Data.Tuple.HT (mapFst, mapSnd, forcePair, )

import Prelude hiding (dropWhile, takeWhile, span, )


-- $setup
-- >>> import Test.Utility (forAllPredicates, defined)
-- >>> import qualified Data.List.Reverse.StrictSpine as Rev
-- >>> import qualified Data.List.Match as Match
-- >>> import qualified Data.List as List
-- >>> import Data.Tuple.HT (mapFst, mapPair, swap)
-- >>>
-- >>> _suppressUnusedImportWarning :: (a -> Bool) -> [a] -> [a]
-- >>> _suppressUnusedImportWarning = Data.List.Reverse.StrictSpine.dropWhile


{- |
prop> forAllPredicates $ \p xs -> Rev.dropWhile p xs == reverse (List.dropWhile p (reverse xs))
prop> \x xs pad -> defined $ length $ Rev.dropWhile ((x::Char)/=) $ Match.replicate (pad::[()]) undefined ++ x:xs
-}
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p =
   foldr (\x xs -> if null xs && p x then [] else x:xs) []

{- |
prop> forAllPredicates $ \p xs -> Rev.takeWhile p xs == reverse (List.takeWhile p (reverse xs))
prop> \x xs pad -> defined $ Rev.takeWhile ((x::Char)/=) $ Match.replicate (pad::[()]) undefined ++ x:xs
-}
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p =
   snd .
   foldr
      (\x xys ->
         (if fst xys && p x then mapSnd (x:) else mapFst (const False)) xys)
      (True, [])

{- |
prop> forAllPredicates $ \p xs -> Rev.span p xs == swap (mapPair (reverse, reverse) (List.span p (reverse xs)))
prop> forAllPredicates $ \p xs -> Rev.span p xs == (Rev.dropWhile p xs, Rev.takeWhile p xs)
prop> \x xs pad -> defined $ mapFst length $ Rev.span ((x::Char)/=) $ Match.replicate (pad::[()]) undefined ++ x:xs
-}
span :: (a -> Bool) -> [a] -> ([a], [a])
span p =
   forcePair .
   foldr
      (\x xys ->
         (if null (fst xys) && p x then mapSnd else mapFst) (x:) xys)
      ([], [])
