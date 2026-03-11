{-# LANGUAGE RebindableSyntax #-}
{- |
Functions that are counterparts of the @generic@ functions in "Data.List"
using NumericPrelude.Numeric type classes.
For input arguments we use the restrictive @ToInteger@ constraint,
although in principle @RealRing@ would be enough.
However we think that @take 0.5 xs@ is rather a bug than a feature,
thus we forbid fractional types.
On the other hand fractional types as result can be quite handy,
e.g. in @average xs = sum xs / length xs@.
-}
module NumericPrelude.List.Generic
   ((!!), lengthLeft, lengthRight, replicate,
    take, drop, splitAt,
    findIndex, elemIndex, findIndices, elemIndices,
   ) where

import NumericPrelude.List.Checked ((!!), )

import qualified Algebra.ToInteger  as ToInteger
import qualified Algebra.Ring       as Ring
import Algebra.Ring (one, )
import Algebra.Additive (zero, (+), (-), )

import qualified Data.Maybe         as Maybe
import Data.Tuple.HT (mapFst, )

import NumericPrelude.Base as List
   hiding (take, drop, splitAt, length, replicate, (!!), )


replicate :: (ToInteger.C n) => n -> a -> [a]
replicate n x = take n (List.repeat x)

take :: (ToInteger.C n) => n -> [a] -> [a]
take _ [] = []
take n (x:xs) =
   if n<=zero
     then []
     else x : take (n-one) xs

drop :: (ToInteger.C n) => n -> [a] -> [a]
drop _ [] = []
drop n xt@(_:xs) =
   if n<=zero
     then xt
     else drop (n-one) xs

splitAt :: (ToInteger.C n) => n -> [a] -> ([a], [a])
splitAt _ [] = ([], [])
splitAt n xt@(x:xs) =
   if n<=zero
     then ([], xt)
     else mapFst (x:) $ splitAt (n-one) xs


{- |
Left associative length computation
that is appropriate for types like @Integer@.
-}
lengthLeft :: (Ring.C n) => [a] -> n
lengthLeft = List.foldl (\n _ -> n+one) zero

{- |
Right associative length computation
that is appropriate for types like @Peano@ number.
-}
lengthRight :: (Ring.C n) => [a] -> n
lengthRight = List.foldr (\_ n -> one+n) zero

elemIndex :: (Ring.C n, Eq a) => a -> [a] -> Maybe n
elemIndex e = findIndex (e==)

elemIndices :: (Ring.C n, Eq a) => a -> [a] -> [n]
elemIndices e = findIndices (e==)

findIndex :: Ring.C n => (a -> Bool) -> [a] -> Maybe n
findIndex p = Maybe.listToMaybe . findIndices p

findIndices :: Ring.C n => (a -> Bool) -> [a] -> [n]
findIndices p =
   map fst .
   filter (p . snd) .
   zip (iterate (one+) zero)
