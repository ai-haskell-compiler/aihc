{-# LANGUAGE NoImplicitPrelude #-}
module Synthesizer.Plain.Filter.Delay.List (modulated, modulatedRev) where

import qualified Synthesizer.Plain.Interpolation as Interpolation

import Data.List(tails)

import qualified Algebra.RealField as RealField
import qualified Algebra.Additive  as Additive

import NumericPrelude.Numeric
import NumericPrelude.Base


{- |
This function uses suffixes of the reversed signal.
This way small delays perform well
but the big drawback is that the garbage collector
can not deallocate old samples.
-}
modulatedRevCore :: (RealField.C a, Additive.C v) =>
   Interpolation.T a v -> Int -> [a] -> [v] -> [v]
modulatedRevCore ip size ts xs =
   zipWith
      (\t x ->
          let (ti,tf) = splitFraction t
          in  Interpolation.func ip tf (drop ti x))
      ts (drop size (scanl (flip (:)) [] xs))

modulatedRev :: (RealField.C a, Additive.C v) =>
   Interpolation.T a v -> Int -> [a] -> [v] -> [v]
modulatedRev ip maxDelay ts xs =
   let size = maxDelay + Interpolation.number ip
   in  modulatedRevCore ip
          (size + 1 + Interpolation.offset ip)
          ts
          (replicate size zero ++ xs)



modulatedCore :: (RealField.C a, Additive.C v) =>
   Interpolation.T a v -> Int -> [a] -> [v] -> [v]
modulatedCore ip size ts xs =
   zipWith
      (\t x ->
          let (ti,tf) = splitFraction (-t)
          in  Interpolation.func ip tf (drop (size+ti) x))
      ts (tails xs)

{- |
This is essentially different for constant interpolation,
because this function "looks forward"
whereas the other two variants "look backward".
For the symmetric interpolation functions
of linear and cubic interpolation, this does not really matter.
-}
modulated :: (RealField.C a, Additive.C v) =>
   Interpolation.T a v -> Int -> [a] -> [v] -> [v]
modulated ip maxDelay ts xs =
   let size = maxDelay + Interpolation.number ip
   in  modulatedCore ip
          (size - Interpolation.offset ip)
          ts
          (replicate size zero ++ xs)
