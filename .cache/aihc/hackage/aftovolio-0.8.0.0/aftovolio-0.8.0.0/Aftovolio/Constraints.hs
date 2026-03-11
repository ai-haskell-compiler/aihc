{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{- |
Module      :  Aftovolio.Constraints
Copyright   :  (c) OleksandrZhabenko 2020-2024
License     :  MIT
Stability   :  Experimental
Maintainer  :  oleksandr.zhabenko@yahoo.com

Provides several most important variants of constraints for the
permutations. All the 'Array'
here must consists of unique 'Int' starting from 0 to n-1 and the 'Int'
arguments must be in the range [0..n-1] though these inner constraints are
not checked. It is up to user to check them.
Uses arrays instead of vectors.
The word \"unsafe\" in the names means that there is no checking whether the arguments are
within the elements of the arrays, this checking is intended to be done elsewhere before applying
the functions here. Without such a checking the result are meaningless.
-}
module Aftovolio.Constraints (
    -- * Predicates
    unsafeOrderIJ,
    unsafeSignDistanceIJ,
    unsafeUnsignDistanceIJ,
    isSignDistIJK3,
    isUnsignDistIJK3,
    isMixedDistIJK3,
    isTripleOrdered,
    isQuadrupleOrdered,
    isQuintupleOrdered,
    isSeveralAOrdered,
    isSeveralBOrdered,
    isFixedPointTup,
    isFixedPoint,
    notSignDistIJK3,
    notUnsignDistIJK3,
    notMixedDistIJK3,
    notTripleOrdered,
    notQuadrupleOrdered,
    notQuintupleOrdered,
    notSeveralAOrdered,
    notSeveralBOrdered,
    notFixedPointTup,
    notFixedPoint,
    isGrouppedTogether,
    notGrouppedTogether,

    -- * Functions to work with permutations with basic constraints ('Array'-based)
    filterOrderIJ,
    unsafeTriples,
    unsafeQuadruples,
    unsafeQuintuples,
    filterGrouppedTogether,

    -- ** With multiple elements specified
    unsafeSeveralA,
    unsafeSeveralB,

    -- ** With fixed points
    fixedPointsG,
    fixedPointsS,

    -- * Distances between elements
    filterSignDistanceIJ,
    filterUnsignDistanceIJ,
    filterSignDistanceIJK3,
    filterUnsignDistanceIJK3,
    filterMixedDistanceIJK3,
) where

import Data.Bits (clearBit, setBit, shiftR, testBit, (.&.))
import Data.Foldable (Foldable, all, any, foldr)
import Data.InsertLeft (InsertLeft (..), filterG)
import GHC.Arr
import GHC.Base hiding (foldr)
import GHC.Int (Int8 (..))
import GHC.Num (Num, (+), (-))
import Data.Tuple (snd)

f2 :: (Foldable t, Eq p) => p -> p -> t p -> Int8
f2 i j = foldr g (0 :: Int8) -- (== 3)
  where
    g x y
        | y == 3 = 3
        | x == j = bitChange (shiftR y 1 == 0) y 0
        | x == i = bitChange (testBit y 0) y 1
        | otherwise = y
{-# INLINE f2 #-}
{-# SPECIALIZE f2 :: Int -> Int -> Array Int Int -> Int8 #-}

f3 :: (Foldable t, Eq p) => p -> p -> p -> t p -> Int8
f3 i j k = foldr g (0 :: Int8) -- (== 7)
  where
    g x y
        | y == 7 = 7
        | x == k = bitChange (shiftR y 1 == 0) y 0 -- u, not (t && u))
        | x == j = bitChange (not (testBit y 2) && testBit y 0) y 1 -- (t, w && not t, w)
        | x == i = bitChange (clearBit y 2 == 3) y 2 -- (u && w, u, w)
        | otherwise = y
{-# INLINE f3 #-}
{-# SPECIALIZE f3 :: Int -> Int -> Int -> Array Int Int -> Int8 #-}

f4 :: (Foldable t, Eq p) => p -> p -> p -> p -> t p -> Int8
f4 i j k l = foldr g (0 :: Int8) -- (== 15)
  where
    g x y
        | y == 15 = 15
        | x == l = bitChange (shiftR y 1 == 0) y 0
        | x == k = bitChange (shiftR y 2 == 0 && testBit y 0) y 1
        | x == j = bitChange (not (testBit y 3) && y .&. 3 == 3) y 2
        | x == i = bitChange (clearBit y 3 == 7) y 3
        | otherwise = y
{-# INLINE f4 #-}
{-# SPECIALIZE f4 :: Int -> Int -> Int -> Int -> Array Int Int -> Int8 #-}

f5 :: (Foldable t, Eq p) => p -> p -> p -> p -> p -> t p -> Int8
f5 i j k l m = foldr g (0 :: Int8) -- (== 31)
  where
    g x y
        | y == 31 = 31
        | x == m = bitChange (shiftR y 1 == 0) y 0
        | x == l = bitChange (testBit y 0 && shiftR y 2 == 0) y 1
        | x == k = bitChange (shiftR y 3 == 0 && y .&. 3 == 3) y 2
        | x == j = bitChange (not (testBit y 4) && y .&. 7 == 7) y 3
        | x == i = bitChange (clearBit y 4 == 15) y 4
        | otherwise = y
{-# INLINE f5 #-}
{-# SPECIALIZE f5 ::
    Int -> Int -> Int -> Int -> Int -> Array Int Int -> Int8
    #-}

-- | @n@ must be in the range [0..7] though it is not checked here.
bitChange ::
    Bool ->
    Int8 ->
    Int ->
    Int8
bitChange bool = (if bool then setBit else clearBit)
{-# INLINE bitChange #-}

-- | Being given the data satisfying the constraints in the module header checks whether in the 'Array' the first argument stands before the second one.
unsafeOrderIJ :: Int -> Int -> Array Int Int -> Bool
unsafeOrderIJ i j = (== 3) . f2 i j
{-# INLINE unsafeOrderIJ #-}

{- | Being given the data satisfying the constraints in the module header checks whether in the
'Array' the distance between positions of the first two arguments values is equal to the signed
third argument.
-}
unsafeSignDistanceIJ ::
    Int ->
    Int ->
    -- | Can be of both signs, but not equal to 0. The positive value gives 'True' for the first argument being find earlier in the 'Array' than the second and the distance between their positions are equal to 'abs' @d@ (this argument). The negative value gives 'True' for the second argument  being earlier in the 'Array' than the first one and the distance between their positions are equal to 'abs' @d@ (this argument).
    Int ->
    Array Int Int ->
    Bool
unsafeSignDistanceIJ i j d =
    (\(_, _, r) -> if r > 100 then (100 - r) == d else r == d)
        . foldr helpG2 (j, i, -1)
{-# INLINE unsafeSignDistanceIJ #-}

helpG2 :: (Ord a1, Ord a2, Num a1, Num a2) => a2 -> (a2, a2, a1) -> (a2, a2, a1)
helpG2 z (t, u, n)
    | n < 0 =
        if (z /= t && z /= u) then (t, u, n) else (t, u, if z == t then 1 else 101)
    | z /= u && z /= t && t >= 0 = (t, u, n + 1)
    | otherwise = (-1, u, n)
{-# INLINE helpG2 #-}
{-# SPECIALIZE helpG2 :: Int -> (Int, Int, Int) -> (Int, Int, Int) #-}

{- | Being given the data satisfying the constraints in the module header checks whether in the
'Array' the distance between positions of the first two arguments values is equal to the unsigned
third argument. The following is true: if 'unsafeSignDistanceIJ' @i@ @j@ @d@ @array@ == 'True' then
'unsafeUnsignDistanceIJ' @i@ @j@ @d@ @array@ == 'True', but not necessarily vice versa.
-}
unsafeUnsignDistanceIJ ::
    Int ->
    Int ->
    -- | Only for positive values can give 'True', if the distance between the positions of the elements equal to the first two arguments are equal to this argument. Otherwise, 'False'.
    Int ->
    Array Int Int ->
    Bool
unsafeUnsignDistanceIJ i j d = (\(_, _, r) -> r == d) . foldr helpG3 (j, i, -1)
{-# INLINE unsafeUnsignDistanceIJ #-}

helpG3 :: (Ord a1, Ord a2, Num a1, Num a2) => a2 -> (a2, a2, a1) -> (a2, a2, a1)
helpG3 z (t, u, n)
    | n < 0 = if (z /= t && z /= u) then (t, u, n) else (t, u, 1)
    | z /= u && z /= t && t >= 0 = (t, u, n + 1)
    | otherwise = (-1, u, n)
{-# INLINE helpG3 #-}
{-# SPECIALIZE helpG3 :: Int -> (Int, Int, Int) -> (Int, Int, Int) #-}

-- | Being given the data satisfying the constraints in the module header returns the elements that satisfy 'unsafeOrderIJ' as a predicate.
filterOrderIJ ::
    (InsertLeft t (Array Int Int), Monoid (t (Array Int Int))) =>
    Int ->
    Int ->
    t (Array Int Int) ->
    t (Array Int Int)
filterOrderIJ i j = filterG (unsafeOrderIJ i j)
{-# INLINE filterOrderIJ #-}
{-# SPECIALIZE filterOrderIJ ::
    Int -> Int -> [Array Int Int] -> [Array Int Int]
    #-}

-- | Being given the data satisfying the constraints in the module header returns the elements that satisfy 'unsafeSignDistanceIJ' as a predicate.
filterSignDistanceIJ ::
    (InsertLeft t (Array Int Int), Monoid (t (Array Int Int))) =>
    Int ->
    Int ->
    Int ->
    t (Array Int Int) ->
    t (Array Int Int)
filterSignDistanceIJ i j d = filterG (unsafeSignDistanceIJ i j d)
{-# INLINE filterSignDistanceIJ #-}
{-# SPECIALIZE filterSignDistanceIJ ::
    Int -> Int -> Int -> [Array Int Int] -> [Array Int Int]
    #-}

-- | Being given the data satisfying the constraints in the module header returns the elements that satisfy 'unsafeUnsignDistanceIJ' as a predicate.
filterUnsignDistanceIJ ::
    (InsertLeft t (Array Int Int), Monoid (t (Array Int Int))) =>
    Int ->
    Int ->
    Int ->
    t (Array Int Int) ->
    t (Array Int Int)
filterUnsignDistanceIJ i j d = filterG (unsafeUnsignDistanceIJ i j d)
{-# INLINE filterUnsignDistanceIJ #-}
{-# SPECIALIZE filterUnsignDistanceIJ ::
    Int -> Int -> Int -> [Array Int Int] -> [Array Int Int]
    #-}

-- | Being given the data satisfying the constraints in the module header returns the elements that pairwisely (1 and 2, 2 and 3) satisfy 'unsafeSignDistanceIJ' as a predicate.
filterSignDistanceIJK3 ::
    (InsertLeft t (Array Int Int), Monoid (t (Array Int Int))) =>
    Int ->
    Int ->
    Int ->
    Int ->
    Int ->
    t (Array Int Int) ->
    t (Array Int Int)
filterSignDistanceIJK3 i j k d1 d2 = filterG (isSignDistIJK3 i j k d1 d2)
{-# INLINE filterSignDistanceIJK3 #-}
{-# SPECIALIZE filterSignDistanceIJK3 ::
    Int -> Int -> Int -> Int -> Int -> [Array Int Int] -> [Array Int Int]
    #-}

-- | Being given the data satisfying the constraints in the module header returns the elements that pairwisely (1 and 2, 2 and 3) satisfy 'unsafeUnsignDistanceIJ' as a predicate.
filterUnsignDistanceIJK3 ::
    (InsertLeft t (Array Int Int), Monoid (t (Array Int Int))) =>
    Int ->
    Int ->
    Int ->
    Int ->
    Int ->
    t (Array Int Int) ->
    t (Array Int Int)
filterUnsignDistanceIJK3 i j k d1 d2 = filterG (isUnsignDistIJK3 i j k d1 d2)
{-# INLINE filterUnsignDistanceIJK3 #-}
{-# SPECIALIZE filterUnsignDistanceIJK3 ::
    Int -> Int -> Int -> Int -> Int -> [Array Int Int] -> [Array Int Int]
    #-}

-- | Being given the data satisfying the constraints in the module header returns the elements that satisfy both 'unsafeSignDistanceIJ' with the 1st, 2nd and 4th arguments and 'unsafeUnsignDistanceIJ' with the 2nd, 3rd and 5th arguments as predicates.
filterMixedDistanceIJK3 ::
    (InsertLeft t (Array Int Int), Monoid (t (Array Int Int))) =>
    Int ->
    Int ->
    Int ->
    Int ->
    Int ->
    t (Array Int Int) ->
    t (Array Int Int)
filterMixedDistanceIJK3 i j k d1 d2 = filterG (isMixedDistIJK3 i j k d1 d2)
{-# INLINE filterMixedDistanceIJK3 #-}
{-# SPECIALIZE filterMixedDistanceIJK3 ::
    Int -> Int -> Int -> Int -> Int -> [Array Int Int] -> [Array Int Int]
    #-}

{- | Being given the data satisfying the constraints in the module header reduces the number of further computations in the foldable structure of
the permutations each one being represented as 'Array' 'Int' 'Int' where the elements are all the numbers in the range [0..n-1] without duplication if the
arguments are the indices of the duplicated words or their concatenated combinations in the corresponding line.
The first three arguments
can be the indices of the the triple duplicated elements (words or their concatenated combinations in the @phonetic-languages@ series of packages).
-}
unsafeTriples ::
    (InsertLeft t (Array Int Int), Monoid (t (Array Int Int))) =>
    Int ->
    Int ->
    Int ->
    t (Array Int Int) ->
    t (Array Int Int)
unsafeTriples i j k = filterG (isTripleOrdered i j k)
{-# INLINE unsafeTriples #-}
{-# SPECIALIZE unsafeTriples ::
    Int -> Int -> Int -> [Array Int Int] -> [Array Int Int]
    #-}

{- | Being given the data satisfying the constraints in the module header reduces the number of further computations in the foldable structure of
the permutations each one being represented as 'Array' 'Int' 'Int' where the elements are all the numbers in the range [0..n-1] without duplication if the
arguments are the indices of the duplicated words or their concatenated combinations in the corresponding line.
The first four arguments
can the indices of the the quadruple duplicated elements (words or their concatenated combinations in the @phonetic-languages@ series of packages).
-}
unsafeQuadruples ::
    (InsertLeft t (Array Int Int), Monoid (t (Array Int Int))) =>
    Int ->
    Int ->
    Int ->
    Int ->
    t (Array Int Int) ->
    t (Array Int Int)
unsafeQuadruples i j k l = filterG (isQuadrupleOrdered i j k l)
{-# INLINE unsafeQuadruples #-}
{-# SPECIALIZE unsafeQuadruples ::
    Int -> Int -> Int -> Int -> [Array Int Int] -> [Array Int Int]
    #-}

{- | Being given the data satisfying the constraints in the module header reduces the number of further computations in the foldable structure of
the permutations each one being represented as 'Array' 'Int' 'Int' where the elements are all the numbers in the range [0..n-1] without duplication if the
arguments are the indices of the duplicated words or their concatenated combinations in the corresponding line.
The first five arguments
can be the indices of the the quintuple duplicated elements (words or their concatenated combinations in the @phonetic-languages@ series of packages).
-}
unsafeQuintuples ::
    (InsertLeft t (Array Int Int), Monoid (t (Array Int Int))) =>
    Int ->
    Int ->
    Int ->
    Int ->
    Int ->
    t (Array Int Int) ->
    t (Array Int Int)
unsafeQuintuples i j k l m = filterG (isQuintupleOrdered i j k l m)
{-# INLINE unsafeQuintuples #-}
{-# SPECIALIZE unsafeQuintuples ::
    Int -> Int -> Int -> Int -> Int -> [Array Int Int] -> [Array Int Int]
    #-}

{- | Being given the data satisfying the constraints in the module header reduces the number of further computations in the foldable structure of
the permutations each one being represented as 'Array' 'Int' 'Int' where the elements are all the numbers in the range [0..n-1] without duplication.
The first argument
is the index of the the element (a word or their concatenated combination in the @phonetic-languages@ series of packages), the second argument
is 'Array' 'Int' of indices that are in the range [0..n-1]. Filters (and reduces further complex computations) the permutations so that only the
variants with the indices in the second argument all stand AFTER the element with the index equal to the first argument.
-}
unsafeSeveralA ::
    (InsertLeft t (Array Int Int), Monoid (t (Array Int Int))) =>
    Int ->
    Array Int Int ->
    t (Array Int Int) ->
    t (Array Int Int)
unsafeSeveralA !i0 arr = filterG (isSeveralAOrdered i0 arr)
{-# INLINE unsafeSeveralA #-}
{-# SPECIALIZE unsafeSeveralA ::
    Int -> Array Int Int -> [Array Int Int] -> [Array Int Int]
    #-}

{- | Being given the data satisfying the constraints in the module header reduces the number of further computations in the foldable structure of
the permutations each one being represented as 'Array' 'Int' 'Int' where the elements are all the numbers in the range [0..n-1] without duplication.
The first argument
is the index of the the element (a word or their concatenated combination in the @phonetic-languages@ series of packages), the second argument
is 'Array' of indices that are in the range [0..n-1]. Filters (and reduces further complex computations) the permutations so that only the
variants with the indices in the second argument all stand BEFORE the element with the index equal to the first argument.
-}
unsafeSeveralB ::
    (InsertLeft t (Array Int Int), Monoid (t (Array Int Int))) =>
    Int ->
    Array Int Int ->
    t (Array Int Int) ->
    t (Array Int Int)
unsafeSeveralB !i0 arr = filterG (isSeveralBOrdered i0 arr)
{-# INLINE unsafeSeveralB #-}
{-# SPECIALIZE unsafeSeveralB ::
    Int -> Array Int Int -> [Array Int Int] -> [Array Int Int]
    #-}

--------------------------------------------------------------------------------

{- | Reduces the number of permutations using filtering leaving just those ones permutations where elements on the
first elements in the tuples in the first argument 'Array' places are moved to the places indexed with the second
elements in the tuples respectively.
-}
fixedPointsG ::
    (InsertLeft t (Array Int Int), Monoid (t (Array Int Int))) =>
    Array Int (Int, Int) ->
    t (Array Int Int) ->
    t (Array Int Int)
fixedPointsG arr = filterG (isFixedPointTup arr)
{-# INLINE fixedPointsG #-}
{-# SPECIALIZE fixedPointsG ::
    Array Int (Int, Int) -> [Array Int Int] -> [Array Int Int]
    #-}

{- | A simplified variant of the 'fixedPointsG' function where the specified elements stay on their place and that is
why are 'fixed points' in the permutation specified.
-}
fixedPointsS ::
    (InsertLeft t (Array Int Int), Monoid (t (Array Int Int))) =>
    Array Int Int ->
    t (Array Int Int) ->
    t (Array Int Int)
fixedPointsS arr = filterG (isFixedPoint arr)
{-# INLINE fixedPointsS #-}
{-# SPECIALIZE fixedPointsS ::
    Array Int Int -> [Array Int Int] -> [Array Int Int]
    #-}

filterGrouppedTogether :: 
    (InsertLeft t (Array Int Int), Monoid (t (Array Int Int))) =>
    Int -> 
    Int -> 
    t (Array Int Int) -> 
    t (Array Int Int)
filterGrouppedTogether i j = filterG (isGrouppedTogether i j) 
{-# INLINE filterGrouppedTogether #-}
{-# SPECIALIZE filterGrouppedTogether ::
    Int -> Int -> [Array Int Int] -> [Array Int Int]
    #-}

------------------------------------------

isSignDistIJK3 :: Int -> Int -> Int -> Int -> Int -> Array Int Int -> Bool
isSignDistIJK3 i j k d1 d2 arr = unsafeSignDistanceIJ i j d1 arr && unsafeSignDistanceIJ j k d2 arr
{-# INLINE isSignDistIJK3 #-}

isUnsignDistIJK3 :: Int -> Int -> Int -> Int -> Int -> Array Int Int -> Bool
isUnsignDistIJK3 i j k d1 d2 arr = unsafeUnsignDistanceIJ i j d1 arr && unsafeUnsignDistanceIJ j k d2 arr
{-# INLINE isUnsignDistIJK3 #-}

isMixedDistIJK3 :: Int -> Int -> Int -> Int -> Int -> Array Int Int -> Bool
isMixedDistIJK3 i j k d1 d2 arr = unsafeSignDistanceIJ i j d1 arr && unsafeUnsignDistanceIJ j k d2 arr
{-# INLINE isMixedDistIJK3 #-}

isTripleOrdered :: Int -> Int -> Int -> Array Int Int -> Bool
isTripleOrdered i j k = (== 7) . f3 i j k
{-# INLINE isTripleOrdered #-}

isQuadrupleOrdered :: Int -> Int -> Int -> Int -> Array Int Int -> Bool
isQuadrupleOrdered i j k l = (== 15) . f4 i j k l
{-# INLINE isQuadrupleOrdered #-}

isQuintupleOrdered :: Int -> Int -> Int -> Int -> Int -> Array Int Int -> Bool
isQuintupleOrdered i j k l m = (== 31) . f5 i j k l m
{-# INLINE isQuintupleOrdered #-}

isSeveralAOrdered :: Int -> Array Int Int -> Array Int Int -> Bool
isSeveralAOrdered !i0 !arr1 arr2 = all (\k -> unsafeOrderIJ i0 k arr2) arr1
{-# INLINE isSeveralAOrdered #-}

isSeveralBOrdered :: Int -> Array Int Int -> Array Int Int -> Bool
isSeveralBOrdered !i0 !arr1 arr2 = all (\k -> unsafeOrderIJ k i0 arr2) arr1
{-# INLINE isSeveralBOrdered #-}

isFixedPointTup :: Array Int (Int, Int) -> Array Int Int -> Bool
isFixedPointTup arr1 arr2 = all (\(k, j) -> unsafeAt arr2 k == j) arr1
{-# INLINE isFixedPointTup #-}

isFixedPoint :: Array Int Int -> Array Int Int -> Bool
isFixedPoint arr1 arr2 = all (\k -> unsafeAt arr2 k == k) arr1
{-# INLINE isFixedPoint #-}

isGrouppedTogether :: Int -> Int -> Array Int Int -> Bool
isGrouppedTogether i j = (> -1) . snd . foldr f6 v
    where v = (0,0)
          f6 x (k,l)
              | x < i || x > j = 
                     case k of 
                         0 -> (0, l) 
                         b -> if b < j - i + 1 then (b, -1) else (b, l)
              | otherwise = 
                     case l of
                         -1 -> (0, -1)
                         c  -> (k + 1, c + 1)
{-# INLINE isGrouppedTogether #-}


notSignDistIJK3 :: Int -> Int -> Int -> Int -> Int -> Array Int Int -> Bool
notSignDistIJK3 i j k d1 d2 arr = unsafeSignDistanceIJ j i d1 arr || unsafeSignDistanceIJ k j d2 arr
{-# INLINE notSignDistIJK3 #-}

notUnsignDistIJK3 :: Int -> Int -> Int -> Int -> Int -> Array Int Int -> Bool
notUnsignDistIJK3 i j k d1 d2 = not . isUnsignDistIJK3 i j k d1 d2
{-# INLINE notUnsignDistIJK3 #-}

notMixedDistIJK3 :: Int -> Int -> Int -> Int -> Int -> Array Int Int -> Bool
notMixedDistIJK3 i j k d1 d2 arr = unsafeSignDistanceIJ j i d1 arr || not (unsafeUnsignDistanceIJ j k d2 arr)
{-# INLINE notMixedDistIJK3 #-}

notTripleOrdered :: Int -> Int -> Int -> Array Int Int -> Bool
notTripleOrdered i j k = (/= 7) . f3 i j k
{-# INLINE notTripleOrdered #-}

notQuadrupleOrdered :: Int -> Int -> Int -> Int -> Array Int Int -> Bool
notQuadrupleOrdered i j k l = (/= 15) . f4 i j k l
{-# INLINE notQuadrupleOrdered #-}

notQuintupleOrdered :: Int -> Int -> Int -> Int -> Int -> Array Int Int -> Bool
notQuintupleOrdered i j k l m = (/= 31) . f5 i j k l m
{-# INLINE notQuintupleOrdered #-}

notSeveralAOrdered :: Int -> Array Int Int -> Array Int Int -> Bool
notSeveralAOrdered !i0 !arr1 = not . isSeveralAOrdered i0 arr1
{-# INLINE notSeveralAOrdered #-}

notSeveralBOrdered :: Int -> Array Int Int -> Array Int Int -> Bool
notSeveralBOrdered !i0 !arr1 = not . isSeveralBOrdered i0 arr1
{-# INLINE notSeveralBOrdered #-}

notFixedPointTup :: Array Int (Int, Int) -> Array Int Int -> Bool
notFixedPointTup arr1 arr2 = any (\(k, j) -> unsafeAt arr2 k /= j) arr1
{-# INLINE notFixedPointTup #-}

notFixedPoint :: Array Int Int -> Array Int Int -> Bool
notFixedPoint arr1 arr2 = any (\k -> unsafeAt arr2 k /= k) arr1
{-# INLINE notFixedPoint #-}

notGrouppedTogether :: Int -> Int -> Array Int Int -> Bool
notGrouppedTogether i j = not . isGrouppedTogether i j
{-# INLINE notGrouppedTogether #-}
