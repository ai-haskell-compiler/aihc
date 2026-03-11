module Data.Record.HT.Private where

import Data.Monoid (mconcat, )
import Data.List.HT (switchL, )

{- |
Lexicographically compare a list of attributes of two records.

Example:

> compare [comparing fst, comparing snd]
-}
{-# INLINE compare #-}
compare :: [a -> a -> Ordering] -> a -> a -> Ordering
compare cs x y =
   mconcat $ map (\c -> c x y) cs

{-# INLINE compare1 #-}
compare1 :: [a -> a -> Ordering] -> a -> a -> Ordering
compare1 cs x y =
   switchL EQ const $ dropWhile (EQ==) $ map (\c -> c x y) cs

{-# INLINE compare2 #-}
compare2 :: [a -> a -> Ordering] -> a -> a -> Ordering
compare2 cs x y =
   head $ dropWhile (EQ==) (map (\c -> c x y) cs) ++ [EQ]

{- |
Check whether a selected set of fields of two records is equal.

Example:

> equal [equating fst, equating snd]
-}
{-# INLINE equal #-}
equal :: [a -> a -> Bool] -> a -> a -> Bool
equal cs x y = all (\c -> c x y) cs
