module Data.List.Reverse.Private where

import qualified Data.List.Key.Private as Key
import Data.List.HT (segmentAfter, viewR, groupBy)

import Prelude hiding (dropWhile, takeWhile)


-- $setup
-- >>> import Test.Utility (forAllPredicates)
-- >>> import qualified Data.List.Reverse.StrictElement as Rev
-- >>> import Prelude hiding (dropWhile, takeWhile)


{- |
prop> forAllPredicates $ \p xs -> dropWhile p xs == Rev.dropWhile p xs
-}
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p =
   concat . init . segmentAfter (not . p)

{- |
prop> forAllPredicates $ \p xs -> takeWhile0 p xs == Rev.takeWhile p xs
-}
takeWhile0 :: (a -> Bool) -> [a] -> [a]
takeWhile0 p =
   last . segmentAfter (not . p)

{- |
Doesn't seem to be superior to the naive implementation.

prop> forAllPredicates $ \p xs -> takeWhile1 p xs == Rev.takeWhile p xs
-}
takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 p =
   (\mx ->
      case mx of
         Just (_, xs@((True,_):_)) -> map snd xs
         _ -> []) .
   viewR . Key.aux groupBy (==) p

{- |
However it is more inefficient,
because of repeatedly appending single elements. :-(

prop> forAllPredicates $ \p xs -> takeWhile2 p xs == Rev.takeWhile p xs
-}
takeWhile2 :: (a -> Bool) -> [a] -> [a]
takeWhile2 p =
   foldl (\xs x -> if p x then xs++[x] else []) []
