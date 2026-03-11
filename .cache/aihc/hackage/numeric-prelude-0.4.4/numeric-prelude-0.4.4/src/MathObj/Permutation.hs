{-# LANGUAGE RebindableSyntax #-}
{- |
Copyright    :   (c) Henning Thielemann 2006
Maintainer   :   numericprelude@henning-thielemann.de
Stability    :   provisional
Portability  :

Routines and abstractions for permutations of Integers.

***
Seems to be a candidate for Algebra directory.
Algebra.PermutationGroup ?
-}

module MathObj.Permutation where

import Data.Array(Ix)



{- |
There are quite a few way we could represent elements of permutation
groups: the images in a row, a list of the cycles, et.c. All of these
differ highly in how complex various operations end up being.
-}

class C p where
   domain  :: (Ix i) => p i -> (i, i)
   apply   :: (Ix i) => p i -> i -> i
   inverse :: (Ix i) => p i -> p i
