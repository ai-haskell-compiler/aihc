module Synthesizer.Utility where

import qualified Algebra.Module    as Module
import qualified Algebra.RealField as RealField
import qualified Algebra.Ring      as Ring
import qualified Algebra.Field     as Field

import System.Random (Random, RandomGen, randomRs, )

import NumericPrelude.Numeric
import NumericPrelude.Base
import Prelude ()


{-|
If two values are equal, then return one of them,
otherwise raise an error.
-}
{-# INLINE common #-}
common :: (Eq a) => String -> a -> a -> a
common errorMsg x y =
   if x == y
     then x
     else error errorMsg


-- * arithmetic


{-# INLINE fwrap #-}
fwrap :: RealField.C a => (a,a) -> a -> a
fwrap (lo,hi) x = lo + fmod (x-lo) (hi-lo)

{-# INLINE fmod #-}
fmod :: RealField.C a => a -> a -> a
fmod x y = fraction (x/y) * y

{-# INLINE fmodAlt #-}
fmodAlt :: RealField.C a => a -> a -> a
fmodAlt x y = x - fromInteger (floor (x/y)) * y

propFMod :: RealField.C a => a -> a -> Bool
propFMod x y =
--   y /= 0 ==>
   fmod x y == fmodAlt x y

{- |
This one should be more precise than 'affineCombAlt' in floating computations
whenever @x1@ is small and @x0@ is big.
-}
{-# INLINE affineComb #-}
affineComb :: (Module.C t y) => t -> (y,y) -> y
affineComb phase (x0,x1) = (Ring.one-phase) *> x0 + phase *> x1

affineCombAlt :: (Module.C t y) => t -> (y,y) -> y
affineCombAlt phase (x0,x1) = x0 + phase *> (x1-x0)


{-# INLINE balanceLevel #-}
balanceLevel :: (Field.C y) =>
   y -> [y] -> [y]
balanceLevel center xs =
   let d = center - sum xs / fromIntegral (length xs)
   in  map (d+) xs

{-# INLINE randomRsBalanced #-}
randomRsBalanced :: (RandomGen g, Random y, Field.C y) =>
   g -> Int -> y -> y -> [y]
randomRsBalanced g n center width =
   balanceLevel center (take n $ randomRs (zero,width) g)
