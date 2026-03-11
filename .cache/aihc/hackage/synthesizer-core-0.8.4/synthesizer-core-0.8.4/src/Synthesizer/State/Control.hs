{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{- |
Copyright   :  (c) Henning Thielemann 2008
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes
-}
module Synthesizer.State.Control (
   constant,
   line,
   linear, linearMultiscale, linearMultiscaleNeutral,
   exponential, exponentialMultiscale, exponentialMultiscaleNeutral,
   exponential2, exponential2Multiscale, exponential2MultiscaleNeutral,
   exponentialFromTo, exponentialFromToMultiscale,
   vectorExponential,
   vectorExponential2,
   cosine,
   cubicHermite,

   -- used in Analysis
   curveMultiscale,
   curveMultiscaleNeutral,
   ) where

import qualified Synthesizer.Plain.Control as Ctrl

import qualified Synthesizer.State.Signal as Sig

import qualified Algebra.Module                as Module
import qualified Algebra.Transcendental        as Trans
import qualified Algebra.Field                 as Field
import qualified Algebra.Ring                  as Ring
import qualified Algebra.Additive              as Additive

import NumericPrelude.Numeric
import NumericPrelude.Base


{- * Control curve generation -}

{-# INLINE constant #-}
constant :: a -> Sig.T a
constant = Sig.repeat

{-# INLINE linear #-}
linear :: Additive.C a =>
      a   {-^ steepness -}
   -> a   {-^ initial value -}
   -> Sig.T a
          {-^ linear progression -}
linear d y0 = Sig.iterate (d+) y0

{- |
As stable as the addition of time values.
-}
{-# INLINE linearMultiscale #-}
linearMultiscale :: Additive.C y =>
      y
   -> y
   -> Sig.T y
linearMultiscale = curveMultiscale (+)

{- |
Linear curve starting at zero.
-}
{-# INLINE linearMultiscaleNeutral #-}
linearMultiscaleNeutral :: Additive.C y =>
      y
   -> Sig.T y
linearMultiscaleNeutral slope =
   curveMultiscaleNeutral (+) slope zero

{- |
Linear curve of a fixed length.
The final value is not actually reached,
instead we stop one step before.
This way we can concatenate several lines
without duplicate adjacent values.
-}
{-# INLINE line #-}
line :: Field.C y =>
      Int     {-^ length -}
   -> (y,y)   {-^ initial and final value -}
   -> Sig.T y {-^ linear progression -}
line n (y0,y1) =
   Sig.take n $ linear ((y1-y0) / fromIntegral n) y0


{-# INLINE exponential #-}
{-# INLINE exponentialMultiscale #-}
exponential, exponentialMultiscale :: Trans.C a =>
      a   {-^ time where the function reaches 1\/e of the initial value -}
   -> a   {-^ initial value -}
   -> Sig.T a
          {-^ exponential decay -}
exponential time =
   Sig.iterate (exp (- recip time) *)

exponentialMultiscale time = curveMultiscale (*) (exp (- recip time))

{-# INLINE exponentialMultiscaleNeutral #-}
exponentialMultiscaleNeutral :: Trans.C y =>
      y   {-^ time where the function reaches 1\/e of the initial value -}
   -> Sig.T y {-^ exponential decay -}
exponentialMultiscaleNeutral time =
   curveMultiscaleNeutral (*) (exp (- recip time)) one


{-# INLINE exponential2 #-}
{-# INLINE exponential2Multiscale #-}
exponential2, exponential2Multiscale :: Trans.C a =>
      a   {-^ half life -}
   -> a   {-^ initial value -}
   -> Sig.T a
          {-^ exponential decay -}
exponential2 halfLife =
   Sig.iterate (((Ring.one+Ring.one) ** (- recip halfLife)) *)
--   Sig.iterate (((Ring.one/(Ring.one+Ring.one)) ** recip halfLife) *)

exponential2Multiscale halfLife = curveMultiscale (*) (0.5 ** recip halfLife)

{- the 0.5 constant seems to block fusion
   Sig.iterate ((0.5 ** recip halfLife) *)
-}
{- dito fromInteger
   Sig.iterate ((fromInteger 2 ** (- recip halfLife)) *)
-}

{-# INLINE exponential2MultiscaleNeutral #-}
exponential2MultiscaleNeutral :: Trans.C y =>
      y   {-^ half life -}
   -> Sig.T y {-^ exponential decay -}
exponential2MultiscaleNeutral halfLife =
   curveMultiscaleNeutral (*) (0.5 ** recip halfLife) one


{-# INLINE exponentialFromTo #-}
{-# INLINE exponentialFromToMultiscale #-}
exponentialFromTo, exponentialFromToMultiscale :: Trans.C y =>
      y   {-^ time where the function reaches 1\/e of the initial value -}
   -> y   {-^ initial value -}
   -> y   {-^ value after given time -}
   -> Sig.T y {-^ exponential decay -}
exponentialFromTo time y0 y1 =
   Sig.iterate (*  (y1/y0) ** recip time) y0
exponentialFromToMultiscale time y0 y1 =
   curveMultiscale (*) ((y1/y0) ** recip time) y0




{-| This is an extension of 'exponential' to vectors
    which is straight-forward but requires more explicit signatures.
    But since it is needed rarely I setup a separate function. -}
{-# INLINE vectorExponential #-}
vectorExponential :: (Trans.C a, Module.C a v) =>
       a  {-^ time where the function reaches 1\/e of the initial value -}
   ->  v  {-^ initial value -}
   -> Sig.T v
          {-^ exponential decay -}
vectorExponential time y0 =
   Sig.iterate (exp (-1/time) *>) y0

{-# INLINE vectorExponential2 #-}
vectorExponential2 :: (Trans.C a, Module.C a v) =>
       a  {-^ half life -}
   ->  v  {-^ initial value -}
   -> Sig.T v
          {-^ exponential decay -}
vectorExponential2 halfLife y0 =
   Sig.iterate (0.5**(1/halfLife) *>) y0



{-# INLINE cosine #-}
cosine :: Trans.C a =>
       a  {-^ time t0 where  1 is approached -}
   ->  a  {-^ time t1 where -1 is approached -}
   -> Sig.T a
          {-^ a cosine wave where one half wave is between t0 and t1 -}
cosine = Ctrl.cosineWithSlope $
   \d x -> Sig.map cos (linear d x)



{-# INLINE cubicHermite #-}
cubicHermite :: Field.C a => (a, (a,a)) -> (a, (a,a)) -> Sig.T a
cubicHermite node0 node1 =
   Sig.map (Ctrl.cubicFunc node0 node1) (linear 1 0)


-- * auxiliary functions

{-# INLINE curveMultiscale #-}
curveMultiscale :: (y -> y -> y) -> y -> y -> Sig.T y
curveMultiscale op d y0 =
   Sig.cons y0 (Sig.map (op y0) (Sig.iterateAssociative op d))

{-# INLINE curveMultiscaleNeutral #-}
curveMultiscaleNeutral :: (y -> y -> y) -> y -> y -> Sig.T y
curveMultiscaleNeutral op d neutral =
   Sig.cons neutral (Sig.iterateAssociative op d)
